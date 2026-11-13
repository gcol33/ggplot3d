# Section lines as a list of x/y matrices in data units, from an sf/sfc
# line geometry, a data frame with x, y and optional group, or a matrix.
profile_lines_3d <- function(line) {
  if (inherits(line, "sf") || inherits(line, "sfc")) {
    rows <- spatial_rows_3d(line)
    if (any(rows$.kind != "line")) stop("Profile lines must be LINESTRING or MULTILINESTRING geometries.", call. = FALSE)
    return(unname(lapply(split(rows[c(".x3d", ".y3d")], list(rows$.feature, rows$.part), drop = TRUE), as.matrix)))
  }
  if (is.matrix(line) && is.numeric(line) && ncol(line) == 2L) return(list(unname(line)))
  if (is.data.frame(line) && all(c("x", "y") %in% names(line))) {
    groups <- line$group %or3d% rep(1L, nrow(line))
    return(unname(lapply(split(line[c("x", "y")], factor(groups, unique(groups))), as.matrix)))
  }
  stop("line must be an sf line, a data frame with x and y, or a two-column matrix.", call. = FALSE)
}

# Positions every step along a polyline, keeping its vertices so the section
# follows each bend.
profile_samples_3d <- function(xy, n) {
  if (nrow(xy) < 2L || any(!is.finite(xy))) stop("Each profile line needs at least two finite vertices.", call. = FALSE)
  length <- c(0, cumsum(sqrt(rowSums(diff(xy)^2))))
  if (length[nrow(xy)] <= 0) stop("Profile lines must have positive length.", call. = FALSE)
  at <- sort(unique(c(seq(0, length[nrow(xy)], length.out = n), length)))
  data.frame(x = stats::approx(length, xy[, 1L], at, ties = "ordered")$y,
             y = stats::approx(length, xy[, 2L], at, ties = "ordered")$y, distance = at)
}

# Linear interpolation on the triangles drawn by geom_surface_3d(): grid cells
# split along the diagonal from their lower-left corner, or Delaunay triangles.
surface_interpolate_3d <- function(data, x, y, method) {
  if (method == "grid") {
    xs <- sort(unique(data$x)); ys <- sort(unique(data$y))
    z <- matrix(NA_real_, length(xs), length(ys))
    z[cbind(match(data$x, xs), match(data$y, ys))] <- data$z
    i <- findInterval(x, xs, rightmost.closed = TRUE); j <- findInterval(y, ys, rightmost.closed = TRUE)
    inside <- i >= 1L & i < length(xs) & j >= 1L & j < length(ys)
    result <- rep(NA_real_, length(x))
    i <- i[inside]; j <- j[inside]
    u <- (x[inside] - xs[i]) / (xs[i + 1L] - xs[i]); v <- (y[inside] - ys[j]) / (ys[j + 1L] - ys[j])
    z1 <- z[cbind(i, j)]; z2 <- z[cbind(i + 1L, j)]; z3 <- z[cbind(i + 1L, j + 1L)]; z4 <- z[cbind(i, j + 1L)]
    result[inside] <- ifelse(u >= v, z1 + u * (z2 - z1) + v * (z3 - z2), z1 + v * (z4 - z1) + u * (z3 - z4))
    return(result)
  }
  triangles <- surface_triangles_3d(data, "delaunay", Inf)
  ax <- data$x[triangles[, 1L]]; ay <- data$y[triangles[, 1L]]
  bx <- data$x[triangles[, 2L]] - ax; by <- data$y[triangles[, 2L]] - ay
  cx <- data$x[triangles[, 3L]] - ax; cy <- data$y[triangles[, 3L]] - ay
  det <- bx * cy - by * cx
  tolerance <- 1e-12
  vapply(seq_along(x), function(k) {
    px <- x[k] - ax; py <- y[k] - ay
    s <- (px * cy - py * cx) / det; t <- (bx * py - by * px) / det
    hit <- which(s >= -tolerance & t >= -tolerance & s + t <= 1 + tolerance)[1L]
    if (is.na(hit)) return(NA_real_)
    tri <- triangles[hit, ]
    data$z[tri[1L]] + s[hit] * (data$z[tri[2L]] - data$z[tri[1L]]) + t[hit] * (data$z[tri[3L]] - data$z[tri[1L]])
  }, numeric(1))
}

StatProfile3D <- ggplot2::ggproto("StatProfile3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  extra_params = c("na.rm", "line", "n", "base", "method"),
  setup_params = function(data, params) {
    params$method <- match.arg(params$method %or3d% "grid", c("grid", "delaunay"))
    n <- params$n %or3d% 100L
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2 || n != trunc(n)) stop("n must be an integer of at least two.", call. = FALSE)
    if (!is.numeric(params$base) || length(params$base) != 1L || is.na(params$base)) stop("base must be a number or -Inf.", call. = FALSE)
    params$lines <- profile_lines_3d(params$line)
    params
  },
  compute_panel = function(data, scales, line = NULL, lines = list(), n = 100L, base = -Inf, method = "grid") {
    z_scale <- stats_scale_3d(scales, "z")
    floor <- if (is.finite(base)) z_scale$get_transformation()$transform(base) else base
    transform <- function(axis, value) stats_scale_3d(scales, axis)$get_transformation()$transform(value)
    out <- list(); id <- 0L
    for (surface in split(data, data$group)) {
      for (line_id in seq_along(lines)) {
        xy <- lines[[line_id]]
        xy <- cbind(transform("x", xy[, 1L]), transform("y", xy[, 2L]))
        sample <- profile_samples_3d(xy, n)
        sample$z <- surface_interpolate_3d(surface, sample$x, sample$y, method)
        sample$value <- sample$z
        sample$xend <- sample$x; sample$yend <- sample$y; sample$zend <- floor
        sample$line <- line_id
        id <- id + 1L
        sample$group <- id
        sample$PANEL <- surface$PANEL[1L]
        for (column in setdiff(names(surface), c(names(sample), "z", "x", "y", ".source")))
          if (length(unique(surface[[column]])) == 1L) sample[[column]] <- surface[[column]][1L]
        out[[id]] <- sample
      }
    }
    do.call(rbind, out)
  }
)

#' Cross-sections of a surface along a line
#'
#' Samples a surface given as x/y/z grid nodes along one or more section
#' lines and draws the vertical wall between the surface and `base`: a
#' cross-section, or with several lines a fence diagram. The surface is
#' interpolated linearly on the same triangles [geom_surface_3d()] draws, so the
#' top of the wall lies on the drawn surface. Samples are spaced evenly along
#' each line and include its vertices; samples outside the surface are
#' missing and break the wall.
#'
#' The computed variables are `distance` along the line in transformed x/y
#' units, `value` (the surface height in transformed z units) and `line`, the
#' index of the section line. With `geom = ggplot2::GeomPath` the stat draws the profile
#' line on the surface. Lines and data share one coordinate system.
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param geom Registered geom prototype for the sampled profile; the default
#'   draws [geom_band_3d()] walls.
#' @param line Section lines: an sf object or sfc of LINESTRING or
#'   MULTILINESTRING features, a data frame with x, y and an optional group
#'   column, or a two-column matrix.
#' @param n Number of evenly spaced samples per line, besides its vertices.
#' @param base Bottom of the wall in original z units; `-Inf` is the floor of the box.
#' @param method Surface triangulation, `"grid"` or `"delaunay"`, as in [geom_surface_3d()].
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = seq(0, 10, length.out = 21), y = seq(0, 10, length.out = 21))
#' d$z <- with(d, sin(x / 2) + cos(y / 3))
#' fence <- data.frame(x = c(1, 9, 1, 9), y = c(2, 2, 8, 8), group = c(1, 1, 2, 2))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey85", alpha = 0.5) +
#'   stat_profile_3d(line = fence, fill = "tan", colour = "grey30") +
#'   coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey85") +
#'   stat_profile_3d(line = fence, geom = ggplot2::GeomPath, colour = "red", linewidth = 1) +
#'   coord_3d()
#' @export
stat_profile_3d <- function(mapping = NULL, data = NULL, geom = GeomBand3D, position = "identity", ...,
                            line, n = 100L, base = -Inf, method = "grid",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatProfile3D, position,
    c(list(line = line, n = n, base = base, method = method, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
