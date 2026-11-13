# Simplex frames: compositions closed to a constant total. Scale space holds
# the part fractions on its axes (the last part is implied by closure), and
# the frame is the affine barycentric map onto a triangle or a regular
# tetrahedron, so visibility stays exact without subdivision.

simplex_part_family_3d <- function(part, coef, offset = 0) {
  list(scale = "x", coef = coef, offset = offset, element = "x", part = part)
}

FrameSimplex <- ggplot2::ggproto("FrameSimplex", Frame3D,
  name = "simplex",
  linear = TRUE,
  parts = character(),
  vertices = NULL,
  origin = NULL,
  bounds = function(self) self$map(unique(do.call(rbind, lapply(self$panes(), `[[`, "xyz")))),
  map = function(self, xyz) {
    out <- sweep(xyz %*% self$linear_map, 2L, self$origin, "+")
    dimnames(out) <- NULL
    out
  },
  setup_aesthetics = function(self, data) {
    present <- intersect(self$parts, names(data))
    if (!length(present)) return(data)
    if (!all(self$parts %in% names(data))) stop("A simplex frame needs all of ", paste(self$parts, collapse = ", "), ".", call. = FALSE)
    values <- as.matrix(data[self$parts])
    if (!is.numeric(values)) stop("Composition parts must be numeric.", call. = FALSE)
    if (any(values < 0, na.rm = TRUE)) stop("Composition parts must be nonnegative.", call. = FALSE)
    total <- rowSums(values)
    shares <- values / ifelse(total > 0, total, NA_real_)
    axes <- c("x", "y", "z")[seq_len(length(self$parts) - 1L)]
    for (k in seq_along(axes)) data[[axes[k]]] <- shares[, k]
    data
  },
  back_candidates = function(self) setdiff(vapply(self$panes(), `[[`, character(1), "name"), self$floor_panes()),
  back_panes = function(self, partitions) {
    rear <- names(partitions)[partitions == 0L]
    rear[!(rear %in% self$floor_panes())]
  },
  origin_axis = function(self, axis, origin) {
    stop("Origin axes need a frame whose scale space is the unit cube; a simplex frame has none.", call. = FALSE)
  },
  # Only the height of a ternary prism has cross-sections of the full frame.
  section = function(self, axis, value) {
    floor <- Find(function(pane) identical(pane$name, "floor"), self$panes())
    if (axis != 3L || is.null(floor)) return(NULL)
    floor$xyz[, 3L] <- value
    floor$name <- "zzero"; floor$families <- list(); floor$outward <- c(0, 0, 1)
    floor
  },
  axis_candidates = function(self, guide) {
    part <- guide$part %or3d% guide$axis
    spec <- self$axes[[part]]
    if (is.null(spec)) return(Frame3D$axis_candidates(guide))
    candidates <- lapply(spec, function(s) list(start = s$start, end = s$end,
      title = if (is.null(s$vertex)) NULL else list(type = "vertex", at = s$vertex)))
    attr(candidates, "auto") <- if (identical(part, "z")) "outer" else "lower"
    candidates
  },
  guides = function(self, guides, panel_params) {
    titles <- panel_params$part_titles %or3d% list()
    axes <- c("x", "y", "z")
    out <- list()
    for (k in seq_along(self$parts)) {
      part <- self$parts[k]
      source <- guides[[if (k <= length(self$parts) - 1L) axes[k] else "x"]] %or3d% guides[["x"]]
      if (is.null(source)) next
      source$part <- part
      source$axis <- if (k <= length(self$parts) - 1L) axes[k] else "x"
      if (k > length(self$parts) - 1L) {
        source$position <- guides[["x"]]$position; source$labels <- guides[["x"]]$labels; source$minor <- guides[["x"]]$minor
      }
      title <- titles[[part]]
      if (inherits(source$params$title, "waiver") && !is.null(title)) source$title <- title
      out[[part]] <- source
    }
    if (length(self$parts) == 3L && !is.null(guides[["z"]])) out[["z"]] <- guides[["z"]]
    out
  }
)

ternary_vertices_3d <- function() {
  h <- sqrt(3) / 2
  y0 <- (1 - h) / 2
  rbind(a = c(0, y0), b = c(1, y0), c = c(0.5, y0 + h))
}

frame_ternary_3d <- function() {
  v <- ternary_vertices_3d()
  # Scale x = a, y = b, z = height; c = 1 - a - b.
  linear_map <- rbind(c(v["a", ] - v["c", ], 0), c(v["b", ] - v["c", ], 0), c(0, 0, 1))
  A <- c(1, 0); B <- c(0, 1); C <- c(0, 0)
  tri <- function(z) rbind(c(A, z), c(B, z), c(C, z))
  wall <- function(p, q) rbind(c(p, 0), c(q, 0), c(q, 1), c(p, 1))
  a <- simplex_part_family_3d("a", c(1, 0, 0)); b <- simplex_part_family_3d("b", c(0, 1, 0))
  c <- simplex_part_family_3d("c", c(-1, -1, 0), 1)
  height <- list(scale = "z", coef = c(0, 0, 1), offset = 0, element = "z")
  panes <- list(
    list(name = "floor", xyz = tri(0), outward = c(0, 0, -1), families = list(a, b, c), element = "z"),
    list(name = "top", xyz = tri(1), outward = c(0, 0, 1), families = list(a, b, c), element = "z"),
    list(name = "ab", xyz = wall(A, B), outward = c(1, 1, 0), families = list(a, height), element = "x"),
    list(name = "bc", xyz = wall(B, C), outward = c(-1, 0, 0), families = list(b, height), element = "y"),
    list(name = "ca", xyz = wall(C, A), outward = c(0, -1, 0), families = list(c, height), element = "x"))
  edge <- function(p, q, incident) list(xyz = rbind(p, q, deparse.level = 0), incident = incident)
  edges <- list(
    edge(c(A, 0), c(B, 0), c("floor", "ab")), edge(c(B, 0), c(C, 0), c("floor", "bc")), edge(c(C, 0), c(A, 0), c("floor", "ca")),
    edge(c(A, 1), c(B, 1), c("top", "ab")), edge(c(B, 1), c(C, 1), c("top", "bc")), edge(c(C, 1), c(A, 1), c("top", "ca")),
    edge(c(A, 0), c(A, 1), c("ab", "ca")), edge(c(B, 0), c(B, 1), c("ab", "bc")), edge(c(C, 0), c(C, 1), c("bc", "ca")))
  axes <- list(
    a = list(list(start = c(B, 0), end = c(A, 0), vertex = c(A, 0))),
    b = list(list(start = c(C, 0), end = c(B, 0), vertex = c(B, 0))),
    c = list(list(start = c(A, 0), end = c(C, 0), vertex = c(C, 0))),
    z = list(list(start = c(A, 0), end = c(A, 1)), list(start = c(B, 0), end = c(B, 1)), list(start = c(C, 0), end = c(C, 1))))
  ggplot2::ggproto(NULL, FrameSimplex, name = "ternary", parts = c("a", "b", "c"),
    linear_map = linear_map, origin = c(v["c", ], 0), axes = axes,
    panes = function(self) panes, edges = function(self) edges,
    floor_panes = function(self) "floor")
}

tetrahedron_vertices_3d <- function() {
  h <- sqrt(3) / 2; height <- sqrt(2 / 3)
  y0 <- (1 - h) / 2; z0 <- (1 - height) / 2
  rbind(a = c(0, y0, z0), b = c(1, y0, z0), c = c(0.5, y0 + h, z0), d = c(0.5, y0 + h / 3, z0 + height))
}

frame_tetrahedron_3d <- function() {
  v <- tetrahedron_vertices_3d()
  # Scale x = a, y = b, z = c; d = 1 - a - b - c.
  linear_map <- rbind(v["a", ] - v["d", ], v["b", ] - v["d", ], v["c", ] - v["d", ])
  P <- rbind(a = c(1, 0, 0), b = c(0, 1, 0), c = c(0, 0, 1), d = c(0, 0, 0))
  family <- list(a = simplex_part_family_3d("a", c(1, 0, 0)), b = simplex_part_family_3d("b", c(0, 1, 0)),
    c = simplex_part_family_3d("c", c(0, 0, 1)), d = simplex_part_family_3d("d", c(-1, -1, -1), 1))
  outward <- list(d = c(1, 1, 1), a = c(-1, 0, 0), b = c(0, -1, 0), c = c(0, 0, -1))
  panes <- lapply(c("d", "a", "b", "c"), function(missing) {
    present <- setdiff(c("a", "b", "c", "d"), missing)
    list(name = paste(present, collapse = ""), xyz = unname(P[present, , drop = FALSE]), outward = outward[[missing]],
      families = unname(family[present]), element = "x")
  })
  pairs <- utils::combn(c("a", "b", "c", "d"), 2L)
  pane_names <- vapply(panes, `[[`, character(1), "name")
  edges <- lapply(seq_len(ncol(pairs)), function(k) {
    ends <- pairs[, k]
    list(xyz = unname(P[ends, , drop = FALSE]), incident = pane_names[vapply(panes, function(p) all(ends %in% strsplit(p$name, "")[[1L]]), logical(1))])
  })
  axis <- function(from, to) list(list(start = unname(P[from, ]), end = unname(P[to, ]), vertex = unname(P[to, ])))
  axes <- list(a = axis("b", "a"), b = axis("c", "b"), c = axis("a", "c"), d = axis("a", "d"))
  ggplot2::ggproto(NULL, FrameSimplex, name = "tetrahedron", parts = c("a", "b", "c", "d"),
    linear_map = linear_map, origin = unname(v["d", ]), axes = axes,
    panes = function(self) panes, edges = function(self) edges,
    floor_panes = function(self) "abc")
}

coord_simplex_frame_3d <- function(coord, frame, panes, part_axes) {
  check_frame_panes_3d(frame, panes)
  if (identical(coord$model_aspect, "data")) stop("aspect = \"data\" applies to Cartesian frames only.", call. = FALSE)
  coord$frame <- frame
  coord$config$panes <- unique(panes)
  for (axis in part_axes) {
    coord$limits[[axis]] <- c(0, 1)
    coord$expand_axes[[axis]] <- FALSE
  }
  coord
}

simplex_axes_3d <- function(axes, allowed) {
  if (!all(axes %in% allowed)) stop("axes must be a subset of ", paste(allowed, collapse = ", "), ".", call. = FALSE)
  unique(axes)
}

#' Ternary prism coordinates
#'
#' Three nonnegative parts `a`, `b` and `c` are closed to proportions and placed
#' by barycentric position on an equilateral triangle; `z` stands on it as a
#' height, so a three-part composition carries a response. Parts are mapped
#' with the aesthetics `a`, `b` and `c` in place of `x` and `y`. The map is
#' affine, so visibility stays exact.
#' @param axes Guides to draw, any of `"a"`, `"b"`, `"c"` and `"z"`.
#' @param panes Panes to draw: `"back"` (the rear walls), `"floor"`, `"front"`
#'   (the other panes facing the viewer), or names among `"floor"`, `"top"`,
#'   `"ab"`, `"bc"`, `"ca"`.
#' @param ... Other [coord_3d()] arguments: camera, z limits, box, light,
#'   backend.
#' @details The part guides share the break set of the `x` scale, and their
#'   titles come from the `a`, `b` and `c` mappings. Each part's ticks run along
#'   the triangle edge where the next part is zero, and its title sits at the
#'   vertex where the part is one.
#' @return A Coord3D ggproto object.
#' @examples
#' soil <- data.frame(sand = c(60, 20, 30, 10), silt = c(30, 50, 20, 30),
#'   clay = c(10, 30, 50, 60), yield = c(2, 5, 4, 3))
#' ggplot2::ggplot(soil, ggplot2::aes(a = sand, b = silt, c = clay, z = yield)) +
#'   geom_point_3d(size = 3) + coord_ternary_3d()
#' @export
coord_ternary_3d <- function(axes = c("a", "b", "c", "z"), panes = c("back", "floor"), ...) {
  coord <- coord_3d(...)
  coord$config$axes <- simplex_axes_3d(axes, c("a", "b", "c", "z"))
  coord_simplex_frame_3d(coord, frame_ternary_3d(), panes, c("x", "y"))
}

#' Quaternary simplex coordinates
#'
#' Four nonnegative parts `a`, `b`, `c` and `d` are closed to proportions and
#' placed by barycentric position in a regular tetrahedron. Each face carries
#' isolines of its three parts; each vertex carries the title of its part.
#' @param axes Guides to draw, any of `"a"`, `"b"`, `"c"` and `"d"`.
#' @param panes Panes to draw: `"back"` (rear faces other than the base),
#'   `"floor"` (the base `"abc"`), `"front"` (the other faces facing the
#'   viewer), or face names `"abc"`, `"bcd"`, `"acd"`, `"abd"`.
#' @param ... Other [coord_3d()] arguments: camera, box, light, backend.
#' @return A Coord3D ggproto object.
#' @examples
#' set.seed(1)
#' w <- matrix(stats::rexp(80), ncol = 4)
#' d <- data.frame(a = w[, 1], b = w[, 2], c = w[, 3], d = w[, 4])
#' ggplot2::ggplot(d, ggplot2::aes(a = a, b = b, c = c, d = d)) +
#'   geom_point_3d() + coord_simplex_3d()
#' @export
coord_simplex_3d <- function(axes = c("a", "b", "c", "d"), panes = c("back", "floor"), ...) {
  coord <- coord_3d(...)
  coord$config$axes <- simplex_axes_3d(axes, c("a", "b", "c", "d"))
  coord_simplex_frame_3d(coord, frame_tetrahedron_3d(), panes, c("x", "y", "z"))
}

# Isometric log-ratio coordinates with the pivot basis of Egozcue et al. (2003):
# column i contrasts part i with the geometric mean of the parts after it.
ilr_basis_3d <- function(parts) {
  V <- matrix(0, parts, parts - 1L)
  for (i in seq_len(parts - 1L)) {
    rest <- parts - i
    V[i, i] <- sqrt(rest / (rest + 1))
    V[(i + 1L):parts, i] <- -sqrt(1 / (rest * (rest + 1)))
  }
  V
}

ilr_3d <- function(x) {
  logs <- log(x)
  (logs - rowMeans(logs)) %*% ilr_basis_3d(ncol(x))
}

ilr_inverse_3d <- function(z, parts) {
  clr <- z %*% t(ilr_basis_3d(parts))
  e <- exp(clr - apply(clr, 1L, max))
  e / rowSums(e)
}

# Composition coordinates for statistics: four parts give three log-ratio
# coordinates; three parts give two plus the height z. back() returns x, y, z
# positions (part shares and height) for points in those coordinates.
composition_space_3d <- function(data) {
  four <- all(c("a", "b", "c", "d") %in% names(data))
  if (!four && !all(c("a", "b", "c") %in% names(data))) stop("space = \"ilr\" needs the composition parts a, b and c (and d) mapped.", call. = FALSE)
  parts <- if (four) c("a", "b", "c", "d") else c("a", "b", "c")
  values <- as.matrix(data[parts])
  rows <- which(rowSums(!is.finite(values) | values <= 0) == 0L)
  if (length(rows) < nrow(data)) warning("Removed ", nrow(data) - length(rows), " rows with zero or missing parts; log-ratios need positive parts.", call. = FALSE)
  shares <- values[rows, , drop = FALSE] / rowSums(values[rows, , drop = FALSE])
  coords <- ilr_3d(shares)
  if (!four) coords <- cbind(coords, data$z[rows])
  back <- function(z) {
    if (four) return(ilr_inverse_3d(z, 4L)[, 1:3, drop = FALSE])
    cbind(ilr_inverse_3d(z[, 1:2, drop = FALSE], 3L)[, 1:2, drop = FALSE], z[, 3L])
  }
  list(coords = unname(coords), rows = rows, back = back)
}
