#' Triangulated surfaces in three dimensions
#'
#' Grid surfaces estimate vertex normals from height differences along each
#' axis. Lighting uses the normalised mean of each face's vertex normals.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard ggplot2 layer arguments.
#' @param method Grid connectivity or Delaunay triangulation.
#' @param max_edge Maximum triangle edge length in transformed x/y units.
#' @param shade Whether to apply lighting.
#' @param wireframe Whether to draw triangle edges.
#' @param hidden Hidden stroke policy.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 6),
#'                  y = seq(-1, 1, length.out = 6))
#' d$z <- with(d, x^2 + y^2)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_surface_3d(fill = "steelblue") + coord_3d()
#' p + stat_surface_3d() + coord_3d()
#' @export
geom_surface_3d <- function(mapping = NULL, data = NULL, stat = StatSurface3D,
                             position = "identity", ..., method = "grid", max_edge = Inf,
                             shade = TRUE, wireframe = FALSE, hidden = "hide",
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSurface3D, mapping, data, stat, position,
    c(list(method = method, max_edge = max_edge, shade = shade, wireframe = wireframe,
           hidden = hidden, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_surface_3d
#' @param geom Geom prototype used to draw the computed triangles.
#' @export
stat_surface_3d <- function(mapping = NULL, data = NULL, geom = GeomSurface3D,
                             position = "identity", ..., method = "grid", max_edge = Inf,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSurface3D, position,
    c(list(method = method, max_edge = max_edge, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

surface_triangles_3d <- function(data, method, max_edge) {
  x_id <- match(data$x, unique(data$x)); y_id <- match(data$y, unique(data$y))
  index <- order(x_id, y_id, method = "radix")
  a <- head(index, -1L); b <- tail(index, -1L)
  if (any(x_id[a] == x_id[b] & y_id[a] == y_id[b])) stop("Surface x/y coordinates must be unique within each group.", call. = FALSE)
  if (method == "grid") {
    xs <- sort(unique(data$x)); ys <- sort(unique(data$y))
    grid <- matrix(NA_integer_, length(xs), length(ys))
    grid[cbind(match(data$x, xs), match(data$y, ys))] <- seq_len(nrow(data))
    triangles <- matrix(integer(), ncol = 3L)
    if (length(xs) >= 2L && length(ys) >= 2L) {
      i <- rep(seq_len(length(xs) - 1L), each = length(ys) - 1L)
      j <- rep.int(seq_len(length(ys) - 1L), length(xs) - 1L)
      cells <- cbind(grid[cbind(i, j)], grid[cbind(i + 1L, j)],
                     grid[cbind(i + 1L, j + 1L)], grid[cbind(i, j + 1L)])
      good <- rowSums(matrix(is.finite(data$z[cells]), ncol = 4L)) == 4L
      cells <- cells[good, , drop = FALSE]
      triangles <- matrix(as.integer(t(cbind(cells[, c(1L, 2L, 3L), drop = FALSE],
                                             cells[, c(1L, 3L, 4L), drop = FALSE]))), ncol = 3L, byrow = TRUE)
    }
  } else {
    good <- which(is.finite(data$z))
    triangles <- matrix(good[unlist(delaunay_triangles_3d(as.matrix(data[good, c("x", "y")])), use.names = FALSE)], ncol = 3L, byrow = TRUE)
  }
  if (is.finite(max_edge) && nrow(triangles)) {
    next_corner <- triangles[, c(2L, 3L, 1L), drop = FALSE]
    lengths <- sqrt((data$x[triangles] - data$x[next_corner])^2 +
                      (data$y[triangles] - data$y[next_corner])^2)
    triangles <- triangles[rowSums(matrix(lengths <= max_edge, ncol = 3L)) == 3L, , drop = FALSE]
  }
  triangles
}

StatSurface3D <- ggplot2::ggproto("StatSurface3D", ggplot2::Stat,
  required_aes = c("x", "y"),
  optional_aes = c("z", "fill"),
  extra_params = c("na.rm", "method", "max_edge"),
  setup_params = function(data, params) {
    if (is.null(data$z)) stop("A surface requires a z aesthetic.", call. = FALSE)
    params$method <- match.arg(params$method %or3d% "grid", c("grid", "delaunay"))
    params$max_edge <- params$max_edge %or3d% Inf
    if (length(params$max_edge) != 1L || is.na(params$max_edge) || params$max_edge <= 0) stop("max_edge must be positive.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, method = "grid", max_edge = Inf) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Surfaces require continuous x/y/z scales.", call. = FALSE)
    triangles <- surface_triangles_3d(data, method, max_edge)
    if (!length(triangles)) return(data.frame())
    if (method == "grid") data <- surface_normals_3d(data)
    rows <- as.vector(t(triangles))
    out <- lapply(data, `[`, rows)
    out <- structure(out, class = "data.frame", row.names = .set_row_names(length(rows)))
    out$.face <- rep(seq_len(nrow(triangles)), each = 3L)
    out$.corner <- rep.int(1:3, nrow(triangles))
    out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
    if ("fill" %in% names(data)) {
      if (is.numeric(data$fill)) out$fill <- rep(rowMeans(matrix(data$fill[triangles], ncol = 3L)), each = 3L)
      else if (!constant_corners_3d(data$fill, triangles)) stop("Non-numeric surface fill must be constant on each triangle.", call. = FALSE)
    }
    out
  }
)

GeomSurface3D <- ggplot2::ggproto("GeomSurface3D", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "z", ".face", ".corner"),
  optional_aes = c("nx", "ny", "nz"),
  default_aes = ggplot2::aes(colour = NA, fill = "grey70", linewidth = 0.3, linetype = 1, alpha = NA)
)

emit_surface_3d <- function(data, params, panel_params) {
  rows <- order(data$group, data$.face, data$.corner)
  first <- c(TRUE, data$group[head(rows, -1L)] != data$group[tail(rows, -1L)] |
                    data$.face[head(rows, -1L)] != data$.face[tail(rows, -1L)])[seq_along(rows)]
  counts <- diff(c(which(first), length(rows) + 1L))
  valid <- valid_rows_3d(data, params)
  bad <- c(0L, cumsum(!valid[rows]))
  keep <- diff(bad[c(which(first), length(rows) + 1L)]) == 0L
  rows <- rows[rep.int(keep, counts)]
  counts <- counts[keep]
  if (any(counts != 3L)) stop("Surface faces require three keyed corners.", call. = FALSE)
  faces <- matrix(rows, ncol = 3L, byrow = TRUE)
  corner <- matrix(data$.corner[faces], ncol = 3L)
  if (any(corner[, 1L] == corner[, 2L] | corner[, 2L] == corner[, 3L] | corner[, 1L] == corner[, 3L])) stop("Surface faces require three keyed corners.", call. = FALSE)
  for (name in intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(data))) {
    if (!constant_corners_3d(data[[name]], faces)) stop("Surface materials must be constant per face; use fill = after_stat(z_mean).", call. = FALSE)
  }
  edges <- matrix(integer(), ncol = 2L)
  if (isTRUE(params$wireframe) || any(!is.na(data$colour))) {
    edges <- matrix(as.vector(t(faces[, c(1L, 2L, 2L, 3L, 3L, 1L), drop = FALSE])), ncol = 2L, byrow = TRUE)
    edges <- unique_edges_3d(edges, data$.source, data$group)
  }
  packet_from_faces_edges_3d(data, faces, edges, params, panel_params)
}

constant_corners_3d <- function(value, corners) {
  ids <- match(value, unique(value))
  all(ids[corners[, 1L]] == ids[corners[, 2L]] & ids[corners[, 1L]] == ids[corners[, 3L]])
}

# Bowyer-Watson insertion removes triangles whose circumcircles contain the new vertex.
# Boundary edges of that cavity form the replacement triangles in counterclockwise order.
delaunay_triangles_3d <- function(xy) {
  n <- nrow(xy)
  if (n < 3L) return(list())
  index <- order(xy[, 1L], xy[, 2L])
  xy <- xy[index, , drop = FALSE]
  span <- max(apply(xy, 2L, function(x) diff(range(x))))
  if (!is.finite(span) || span == 0) stop("Delaunay surface coordinates are collinear.", call. = FALSE)
  xy <- sweep(xy, 2L, colMeans(xy)) / span
  xy <- rbind(xy, c(-32, -16), c(32, -16), c(0, 32))
  triangles <- list(as.integer(n + 1:3))
  for (i in seq_len(n)) {
    inside <- vapply(triangles, function(t) {
      p <- sweep(xy[t, , drop = FALSE], 2L, xy[i, ])
      s <- rowSums(p^2)
      determinant <- s[1] * cross2_3d(p[2, ], p[3, ]) - s[2] * cross2_3d(p[1, ], p[3, ]) + s[3] * cross2_3d(p[1, ], p[2, ])
      determinant > 64 * .Machine$double.eps * max(s)^2
    }, logical(1))
    edges <- do.call(rbind, lapply(triangles[inside], function(t) rbind(t[c(1, 2)], t[c(2, 3)], t[c(3, 1)])))
    triangles <- triangles[!inside]
    if (is.null(edges)) stop("Delaunay insertion produced no containing cavity.", call. = FALSE)
    keys <- apply(edges, 1L, function(e) paste(sort(e), collapse = ":"))
    edges <- edges[!(duplicated(keys) | duplicated(keys, fromLast = TRUE)), , drop = FALSE]
    for (j in seq_len(nrow(edges))) {
      t <- c(edges[j, ], i)
      orient <- cross2_3d(xy[t[2], ] - xy[t[1], ], xy[t[3], ] - xy[t[1], ])
      if (abs(orient) <= 64 * .Machine$double.eps) next
      if (orient < 0) t <- t[c(2, 1, 3)]
      triangles[[length(triangles) + 1L]] <- as.integer(t)
    }
  }
  triangles <- Filter(function(t) all(t <= n), triangles)
  if (!length(triangles)) stop("Delaunay surface coordinates are collinear.", call. = FALSE)
  lapply(triangles, function(t) as.integer(index[t]))
}

register_geom_3d(GeomSurface3D, emit_surface_3d,
  position_check = function(position, scales) check_position_3d(position, scales))
