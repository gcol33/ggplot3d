utils::globalVariables("level")

contour_levels_3d <- function(range, breaks, bins, binwidth) {
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || any(!is.finite(breaks))) stop("Contour breaks must be finite numeric values.", call. = FALSE)
    return(sort(unique(breaks)))
  }
  if (!is.null(bins) && !is.null(binwidth)) stop("Supply bins or binwidth.", call. = FALSE)
  if (!is.null(binwidth)) {
    if (length(binwidth) != 1L || !is.finite(binwidth) || binwidth <= 0) stop("binwidth must be positive.", call. = FALSE)
    low <- ceiling(range[1L] / binwidth); high <- floor(range[2L] / binwidth)
    return(if (low > high) numeric() else seq(low, high) * binwidth)
  }
  if (is.null(bins)) {
    levels <- pretty(range, n = 10L)
    return(levels[levels > range[1L] & levels < range[2L]])
  }
  if (length(bins) != 1L || !is.finite(bins) || bins < 1 || bins != trunc(bins)) stop("bins must be a positive integer.", call. = FALSE)
  seq(range[1L], range[2L], length.out = bins + 2L)[-c(1L, bins + 2L)]
}

check_plane_3d <- function(plane) {
  if (is.numeric(plane) && length(plane) == 1L && is.finite(plane)) return(plane)
  if (is.character(plane) && length(plane) >= 1L) return(match.arg(plane, c("surface", "floor")))
  stop("plane must be 'surface', 'floor' or a finite numeric z.", call. = FALSE)
}

StatContour3D <- ggplot2::ggproto("StatContour3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  optional_aes = "value",
  dropped_aes = "value",
  extra_params = c("na.rm", "breaks", "bins", "binwidth", "plane", "filled", "method", "max_edge"),
  compute_panel = function(self, data, scales, ...) {
    output <- list(); offset <- 0L
    for (rows in split(seq_len(nrow(data)), data$group)) {
      result <- self$compute_group(data[rows, , drop = FALSE], scales, ...)
      if (!nrow(result)) next
      result$group <- result$group + offset
      offset <- max(result$group)
      output[[length(output) + 1L]] <- result
    }
    if (!length(output)) data.frame() else do.call(rbind, output)
  },
  compute_group = function(data, scales, breaks = NULL, bins = NULL, binwidth = NULL,
                            plane = "surface", filled = FALSE, method = "grid", max_edge = Inf) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Contours require continuous position scales.", call. = FALSE)
    method <- match.arg(method, c("grid", "delaunay"))
    if (filled && !is.null(data$value)) stop("Filled contours follow z; the value aesthetic applies to contour lines.", call. = FALSE)
    triangles <- contour_triangles_3d(data, method, max_edge)
    if (!nrow(triangles)) return(data.frame())
    range <- range(data$z, finite = TRUE)
    values <- data$value %or3d% data$z
    levels <- contour_levels_3d(range(values, finite = TRUE), breaks, bins, binwidth)
    if (filled) levels <- sort(unique(c(range, levels)))
    if (!length(levels) || (filled && length(levels) < 2L)) return(data.frame())
    if (!filled) return(contour_lines_3d(data, triangles, values, levels, range, plane, scales))
    require_geometry_package_3d("isoband")
    results <- list(); group <- 0L
    for (i in seq_len(nrow(triangles))) {
      rows <- triangles[i, ]; tri <- data[rows, c("x", "y", "z")]
      values <- matrix(as.double(c(tri$z[1L], tri$z[3L], tri$z[2L], tri$z[3L])), 2L)
      # Collapsing the upper square edge maps one isoband cell onto a triangle.
      iso <- isoband::isobands(c(0, 1), c(0, 1), values, head(levels, -1L), tail(levels, -1L))
      for (j in seq_along(iso)) {
        curve <- iso[[j]]
        if (!length(curve$x)) next
        for (id in unique(curve$id)) {
          k <- which(curve$id == id); u <- curve$x[k]; v <- curve$y[k]
          weights <- cbind((1 - u) * (1 - v), u * (1 - v), v)
          xyz <- weights %*% as.matrix(tri)
          keep <- !duplicated(as.data.frame(xyz))
          xyz <- xyz[keep, , drop = FALSE]
          if (nrow(xyz) < 3L) next
          group <- group + 1L
          out <- data[rep(rows[1L], nrow(xyz)), , drop = FALSE]
          out[c("x", "y", "z")] <- as.data.frame(xyz)
          out$.origin <- rep(as.integer(data$.source[rows[1L]] %or3d% rows[1L]), nrow(out))
          out$level <- mean(levels[j + 0:1])
          out$level_low <- levels[j]
          out$level_high <- levels[j + 1L]
          out$piece <- group; out$group <- group
          out$zmin <- range[1L]; out$zmax <- range[2L]
          if (is.numeric(plane)) out$zintercept <- scales$z$get_transformation()$transform(plane)
          results[[length(results) + 1L]] <- out
        }
      }
    }
    if (!length(results)) return(data.frame())
    do.call(rbind, results)
  }
)

# Level lines of values (one per row of data) on the triangles of data, by
# the marching triangles of geometry_contour_cpp(): crossings are keyed by the
# edge's two vertex rows, so triangles sharing an edge share the crossing
# exactly, and segments are chained into open lines (ending on the mesh
# boundary) or closed loops. Each line keeps the attributes of the first row
# of the triangle it starts in. Contours of z lie exactly at their level.
contour_lines_3d <- function(data, triangles, values, levels, range, plane, scales) {
  lines <- geometry_contour_cpp(as.matrix(data[c("x", "y", "z")]), matrix(as.integer(triangles), ncol = 3L),
    as.double(values), as.double(levels))
  if (!nrow(lines)) return(data.frame())
  source <- triangles[lines$triangle, 1L]
  out <- data[source, , drop = FALSE]
  out[c("x", "y", "z")] <- lines[c("x", "y", "z")]
  if (identical(values, data$z)) out$z <- lines$level
  out$.origin <- as.integer(data$.source[source] %or3d% source)
  out$level <- out$level_low <- out$level_high <- lines$level
  out$piece <- out$group <- lines$piece
  out$zmin <- range[1L]; out$zmax <- range[2L]
  if (is.numeric(plane)) out$zintercept <- scales$z$get_transformation()$transform(plane)
  rownames(out) <- NULL
  out
}

# Triangles of a contour layer's rows: the faces of mesh input (keyed by
# .face and .corner, polygons fanned, corners welded by .vertex), otherwise
# the grid or Delaunay triangulation of x and y used by geom_surface_3d().
contour_triangles_3d <- function(data, method, max_edge) {
  if (!all(c(".face", ".corner") %in% names(data))) return(surface_triangles_3d(data, method, max_edge))
  rows <- order(data$.mesh %or3d% 1L, data$.face, data$.corner)
  face <- interaction(data$.mesh %or3d% 1L, data$.face, drop = TRUE)[rows]
  pieces <- lapply(split(rows, face), function(r) if (length(r) >= 3L) cbind(r[1L], r[-c(1L, length(r))], r[-(1:2)]))
  triangles <- do.call(rbind, pieces) %or3d% matrix(integer(), ncol = 3L)
  if (!is.null(data$.vertex)) {
    key <- paste(data$.mesh %or3d% 1L, data$.vertex)
    triangles[] <- match(key, key)[triangles]
  }
  triangles
}

GeomContour3D <- ggplot2::ggproto("GeomContour3D", ggplot2::GeomPath,
  required_aes = c("x", "y", "z"), optional_aes = c(".origin", "level", "level_low", "level_high")
)

GeomContourFilled3D <- ggplot2::ggproto("GeomContourFilled3D", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "z"), optional_aes = c(".origin", "level", "level_low", "level_high"),
  default_aes = solid_defaults_3d()
)

StatContourFilled3D <- ggplot2::ggproto("StatContourFilled3D", StatContour3D,
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(level))
)

#' Contours following a triangulated surface or a horizontal plane
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param bins,binwidth,breaks Level controls in transformed z units. Explicit breaks take precedence.
#' @param plane Either surface, floor, or a numeric z in original units. A
#'   flat plane is a shorthand for `position = position_project_3d("zmin")` or
#'   `position_project_3d(list(axis = "z", at = plane))`; use
#'   [position_project_3d()] directly to draw contours on a side wall.
#' @param method `"grid"` triangulates a complete x/y grid as
#'   geom_surface_3d() does; `"delaunay"` triangulates scattered x/y
#'   positions. Mesh input ([mesh_3d()]) uses its own faces.
#' @param max_edge Longest x/y edge kept in a Delaunay triangulation.
#' @details Contour lines are traced by marching triangles on the triangles
#'   of the surface: the field is linear on each triangle, crossings on a
#'   shared edge are shared, and segments are chained into open lines and
#'   closed loops. The field is z, or the `value` aesthetic when mapped, so
#'   any scalar on any triangulated surface can be contoured: a variable
#'   measured on a terrain, or `value = n . p` for sections of the surface by
#'   parallel planes. Mesh input (a `mesh3d` or [mesh_3d()] data with `.face`,
#'   `.corner` and `.vertex` keys) is contoured on its own faces, polygons
#'   fanned from their first corner.
#'   Filled contours follow z only; isoband operates on the same grid
#'   triangles used by geom_surface_3d().
#'   Default levels are interior pretty(range, n = 10) values.
#'   Surface bands are lifted by barycentric interpolation within each triangle.
#'   Floor contours use the lower expanded panel boundary and are painted with
#'   the floor pane. The source height
#'   range trains the z scale for either projection.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
#' d$z <- exp(-d$x^2 - d$y^2)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d() + geom_contour_3d(bins = 4, colour = "navy") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d() +
#'   geom_contour_3d(ggplot2::aes(value = x + y), bins = 6, colour = "navy") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_contour_filled_3d(bins = 4, plane = "floor") + coord_3d()
#' @export
geom_contour_3d <- function(mapping = NULL, data = NULL, stat = StatContour3D, position = "identity", ...,
                            bins = NULL, binwidth = NULL, breaks = NULL, plane = c("surface", "floor"),
                            method = c("grid", "delaunay"), max_edge = Inf,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  plane <- check_plane_3d(plane)
  make_layer_3d(GeomContour3D, mapping, data, stat, plane_position_3d(plane, position),
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, plane = plane, filled = FALSE,
      method = match.arg(method), max_edge = max_edge, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_contour_3d
#' @export
geom_contour_filled_3d <- function(mapping = NULL, data = NULL, stat = StatContourFilled3D, position = "identity", ...,
                                   bins = NULL, binwidth = NULL, breaks = NULL, plane = c("surface", "floor"),
                                   na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  plane <- check_plane_3d(plane)
  make_layer_3d(GeomContourFilled3D, mapping, data, stat, plane_position_3d(plane, position),
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, plane = plane, filled = TRUE, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Mesh input keeps its face keys for the contour stat.
prepare_contour_data_3d <- function(data) {
  if (inherits(data, "mesh3d")) prepare_mesh_data_3d(data) else data
}
prepare_contour_aesthetics_3d <- function(out, data, plot, params) {
  if (is.data.frame(data) && nrow(data) == nrow(out))
    for (key in intersect(c(".mesh", ".face", ".corner", ".vertex"), names(data))) out[[key]] <- data[[key]]
  out
}

register_geom_3d(GeomContour3D, emit_paths_3d,
  prepare_data = prepare_contour_data_3d, prepare_aesthetics = prepare_contour_aesthetics_3d)
register_geom_3d(GeomContourFilled3D, function(data, params, panel_params) emit_polygons_3d(data, params, panel_params))
