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
  bins <- bins %or3d% 10L
  if (length(bins) != 1L || !is.finite(bins) || bins < 1 || bins != trunc(bins)) stop("bins must be a positive integer.", call. = FALSE)
  seq(range[1L], range[2L], length.out = bins + 2L)[-c(1L, bins + 2L)]
}

check_contour_plane_3d <- function(plane) {
  if (is.numeric(plane) && length(plane) == 1L && is.finite(plane)) return(plane)
  if (is.character(plane) && length(plane) >= 1L) return(match.arg(plane, c("surface", "floor")))
  stop("plane must be 'surface', 'floor' or a finite numeric z.", call. = FALSE)
}

StatContour3D <- ggplot2::ggproto("StatContour3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  extra_params = c("na.rm", "breaks", "bins", "binwidth", "plane", "filled"),
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
                            plane = "surface", filled = FALSE) {
    require_geometry_package_3d("isoband")
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Contours require continuous position scales.", call. = FALSE)
    triangles <- surface_triangles_3d(data, "grid", Inf)
    if (!nrow(triangles)) return(data.frame())
    range <- range(data$z, finite = TRUE)
    levels <- contour_levels_3d(range, breaks, bins, binwidth)
    if (filled) levels <- sort(unique(c(range, levels)))
    if (!length(levels) || (filled && length(levels) < 2L)) return(data.frame())
    results <- list(); group <- 0L
    for (i in seq_len(nrow(triangles))) {
      rows <- triangles[i, ]; tri <- data[rows, c("x", "y", "z")]
      values <- matrix(as.double(c(tri$z[1L], tri$z[3L], tri$z[2L], tri$z[3L])), 2L)
      # Collapsing the upper square edge maps one isoband cell onto a triangle.
      iso <- if (filled) isoband::isobands(c(0, 1), c(0, 1), values, head(levels, -1L), tail(levels, -1L))
      else isoband::isolines(c(0, 1), c(0, 1), values, levels)
      for (j in seq_along(iso)) {
        curve <- iso[[j]]
        if (!length(curve$x)) next
        for (id in unique(curve$id)) {
          k <- which(curve$id == id); u <- curve$x[k]; v <- curve$y[k]
          weights <- cbind((1 - u) * (1 - v), u * (1 - v), v)
          xyz <- weights %*% as.matrix(tri)
          keep <- !duplicated(as.data.frame(xyz))
          xyz <- xyz[keep, , drop = FALSE]
          if (nrow(xyz) < if (filled) 3L else 2L) next
          group <- group + 1L
          out <- data[rep(rows[1L], nrow(xyz)), , drop = FALSE]
          out[c("x", "y", "z")] <- as.data.frame(xyz)
          out$.origin <- rep(as.integer(data$.source[rows[1L]] %or3d% rows[1L]), nrow(out))
          out$level <- if (filled) mean(levels[j + 0:1]) else levels[j]
          out$level_low <- if (filled) levels[j] else levels[j]
          out$level_high <- if (filled) levels[j + 1L] else levels[j]
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

contour_draw_data_3d <- function(data, params, panel_params) {
  plane <- params$plane
  if (identical(plane, "floor")) data$z <- 0
  else if (is.numeric(plane)) {
    value <- panel_params$z$get_transformation()$transform(plane)
    data$z <- (value - panel_params$ranges$z[1L]) / diff(panel_params$ranges$z)
  }
  data
}

#' Contours following a triangulated surface or a horizontal plane
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param bins,binwidth,breaks Level controls in transformed z units. Explicit breaks take precedence.
#' @param plane Either surface, floor, or a numeric z in original units.
#' @details Isoband operates on the same grid triangles used by geom_surface_3d().
#'   Surface bands are lifted by barycentric interpolation within each triangle.
#'   Floor contours use the lower expanded panel boundary. The source height
#'   range trains the z scale for either projection.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
#' d$z <- exp(-d$x^2 - d$y^2)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d() + geom_contour_3d(bins = 4, colour = "navy") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_contour_filled_3d(bins = 4, plane = "floor") + coord_3d()
#' @export
geom_contour_3d <- function(mapping = NULL, data = NULL, stat = StatContour3D, position = "identity", ...,
                            bins = NULL, binwidth = NULL, breaks = NULL, plane = c("surface", "floor"),
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  plane <- check_contour_plane_3d(plane)
  make_layer_3d(GeomContour3D, mapping, data, stat, position,
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, plane = plane, filled = FALSE, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_contour_3d
#' @export
geom_contour_filled_3d <- function(mapping = NULL, data = NULL, stat = StatContourFilled3D, position = "identity", ...,
                                   bins = NULL, binwidth = NULL, breaks = NULL, plane = c("surface", "floor"),
                                   na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  plane <- check_contour_plane_3d(plane)
  make_layer_3d(GeomContourFilled3D, mapping, data, stat, position,
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, plane = plane, filled = TRUE, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

register_geom_3d(GeomContour3D, function(data, params, panel_params) {
  emit_paths_3d(contour_draw_data_3d(data, params, panel_params), params, panel_params)
}, position_check = function(position, scales) check_position_3d(position, scales))
register_geom_3d(GeomContourFilled3D, function(data, params, panel_params) {
  emit_polygons_3d(contour_draw_data_3d(data, params, panel_params), params, panel_params)
}, position_check = function(position, scales) check_position_3d(position, scales))
