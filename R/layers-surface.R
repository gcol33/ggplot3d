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
#' @param outline Feature lines from [outline_3d()], or NULL for none.
#' @param alpha_mode How a translucent surface composites in vector output.
#'   `"face"` blends every face over what lies behind it, so a surface seen
#'   through itself darkens where it folds over. `"surface"` paints each sheet
#'   (a connected set of faces sharing vertices, such as one isosurface level)
#'   as one translucent film: other sheets show through it, its own pieces
#'   behind its nearer pieces do not. A sheet of several fill colours needs a
#'   device with Porter-Duff groups and alpha masks (ragg, cairo_pdf, svg);
#'   other devices, pdf() among them, paint it as `"face"`. The layers that
#'   draw surfaces ([stat_isosurface_3d()], [stat_parametric_3d()],
#'   [geom_mesh_3d()] and others) accept it too. Raster backends composite per
#'   face.
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
                             shade = TRUE, wireframe = FALSE, hidden = "hide", outline = NULL,
                             alpha_mode = "face", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSurface3D, mapping, data, stat, position,
    c(list(method = method, max_edge = max_edge, shade = shade, wireframe = wireframe,
           hidden = hidden, outline = outline, alpha_mode = alpha_mode_3d(alpha_mode), na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
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

surface_lattice_3d <- function(data) {
  list(i = match(data$x, sort(unique(data$x))), j = match(data$y, sort(unique(data$y))))
}

lattice_triangles_3d <- function(lattice, z) {
  grid <- matrix(NA_integer_, max(0L, lattice$i), max(0L, lattice$j))
  grid[cbind(lattice$i, lattice$j)] <- seq_along(lattice$i)
  if (nrow(grid) < 2L || ncol(grid) < 2L) return(matrix(integer(), ncol = 3L))
  i <- rep(seq_len(nrow(grid) - 1L), each = ncol(grid) - 1L)
  j <- rep.int(seq_len(ncol(grid) - 1L), nrow(grid) - 1L)
  cells <- cbind(grid[cbind(i, j)], grid[cbind(i + 1L, j)],
                 grid[cbind(i + 1L, j + 1L)], grid[cbind(i, j + 1L)])
  good <- rowSums(matrix(is.finite(z[cells]), ncol = 4L)) == 4L
  cells <- cells[good, , drop = FALSE]
  matrix(as.integer(t(cbind(cells[, c(1L, 2L, 3L), drop = FALSE],
                            cells[, c(1L, 3L, 4L), drop = FALSE]))), ncol = 3L, byrow = TRUE)
}

surface_triangles_3d <- function(data, method, max_edge, lattice = surface_lattice_3d(data)) {
  index <- order(lattice$i, lattice$j, method = "radix")
  a <- head(index, -1L); b <- tail(index, -1L)
  if (any(lattice$i[a] == lattice$i[b] & lattice$j[a] == lattice$j[b])) stop("Surface x/y coordinates must be unique within each group.", call. = FALSE)
  if (method == "grid") {
    triangles <- lattice_triangles_3d(lattice, data$z)
  } else {
    good <- which(is.finite(data$z))
    triangles <- matrix(good[delaunay_triangles_3d(as.matrix(data[good, c("x", "y")]))], ncol = 3L)
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
  outline_merge_3d(packet_from_faces_edges_3d(data, faces, edges, params, panel_params), data, faces, params, panel_params)
}

constant_corners_3d <- function(value, corners) {
  ids <- match(value, unique(value))
  all(ids[corners[, 1L]] == ids[corners[, 2L]] & ids[corners[, 1L]] == ids[corners[, 3L]])
}

# Delaunay triangles of the x/y positions as a three-column matrix of row
# indices, counterclockwise (native Bowyer-Watson with exact predicates).
delaunay_triangles_3d <- function(xy) {
  if (nrow(xy) < 3L) return(matrix(integer(), ncol = 3L))
  triangles <- geometry_delaunay_cpp(xy)
  if (!nrow(triangles)) stop("Delaunay surface coordinates are collinear.", call. = FALSE)
  triangles
}

register_geom_3d(GeomSurface3D, emit_surface_3d, parameters = surface_parameters_3d)
