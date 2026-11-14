# Reference lines through the panel box: lines parallel to an axis at fixed
# intercepts, and lines through a point in any direction. Both are cut to the
# box when the panel is drawn.

# The part of each line point + t * direction inside the unit cube, both in box
# units, by the parametric clip of Liang and Barsky (1984): each axis bounds t
# to the interval where that coordinate lies in [0, 1]. Returns the start and
# end points and which lines cross the box over a positive length.
clip_lines_to_box_3d <- function(point, direction) {
  n <- nrow(point)
  lower <- rep(-Inf, n); upper <- rep(Inf, n)
  tolerance <- 1e-9
  for (k in seq_len(3L)) {
    p <- point[, k]; d <- direction[, k]
    moving <- d != 0
    enter <- ifelse(moving, pmin(-p / d, (1 - p) / d), -Inf)
    leave <- ifelse(moving, pmax(-p / d, (1 - p) / d), Inf)
    outside <- !moving & (p < -tolerance | p > 1 + tolerance)
    lower <- pmax(lower, enter)
    upper <- pmin(upper, leave)
    upper[outside] <- -Inf
  }
  span <- (upper - lower) * sqrt(rowSums(direction^2))
  inside <- is.finite(span) & span > tolerance
  lower[!inside] <- 0; upper[!inside] <- 0
  list(start = point + lower * direction, end = point + upper * direction, inside = inside)
}

# One polyline per row of data from the clipped lines. Lines that miss the box
# are dropped with a warning naming the layer function.
emit_box_lines_3d <- function(data, point, direction, params, panel_params, what) {
  valid <- rowSums(!is.finite(cbind(point, direction))) == 0L & rowSums(direction != 0) > 0L
  if (any(!valid) && !isTRUE(params$na.rm)) {
    warning("Removed ", sum(!valid), " ", what, " with missing positions or directions.", call. = FALSE)
  }
  clipped <- clip_lines_to_box_3d(point[valid, , drop = FALSE], direction[valid, , drop = FALSE])
  if (any(!clipped$inside)) warning("Skipped ", sum(!clipped$inside), " ", what, " outside the panel box.", call. = FALSE)
  rows <- which(valid)[clipped$inside]
  n <- length(rows)
  if (!n) return(new_scene_packet(panel_id = panel_params$panel_id))
  ends <- data[rep(rows, each = 2L), , drop = FALSE]
  start <- clipped$start[clipped$inside, , drop = FALSE]
  end <- clipped$end[clipped$inside, , drop = FALSE]
  for (k in seq_len(3L)) ends[[c("x", "y", "z")[k]]] <- as.vector(rbind(start[, k], end[, k]))
  packet_from_primitives_3d(ends, seq_len(2L * n), rep("polylines", n), params,
    panel_params$panel_id, rep.int(2L, n))
}

# Lines parallel to the one axis not named in `fixed`, a named vector mapping
# each held axis to the column holding its position in box units.
emit_axis_lines_3d <- function(data, fixed, params, panel_params) {
  axes <- c("x", "y", "z")
  along <- setdiff(axes, names(fixed))
  if (length(along) != 1L) stop("A reference line holds exactly two of x, y and z fixed.", call. = FALSE)
  n <- nrow(data)
  point <- matrix(0, n, 3L, dimnames = list(NULL, axes))
  for (axis in names(fixed)) point[, axis] <- as.numeric(data[[fixed[[axis]]]])
  direction <- matrix(0, n, 3L, dimnames = list(NULL, axes))
  direction[, along] <- 1
  emit_box_lines_3d(data, point, direction, params, panel_params, "reference lines")
}

GeomRefline3D <- ggplot2::ggproto("GeomRefline3D", ggplot2::Geom,
  optional_aes = c("xintercept", "yintercept", "zintercept"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path
)

GeomAbline3D <- ggplot2::ggproto("GeomAbline3D", ggplot2::Geom,
  optional_aes = c("dx", "dy", "dz"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path
)

reference_intercepts_3d <- c(x = "xintercept", y = "yintercept", z = "zintercept")

#' Reference lines through the panel box
#'
#' `geom_refline_3d()` draws lines parallel to an axis: fix two of `xintercept`,
#' `yintercept` and `zintercept` and the line runs through the whole box along
#' the third axis, the three-dimensional counterpart of
#' [ggplot2::geom_hline()] and [ggplot2::geom_vline()]. `geom_abline_3d()`
#' draws lines in any direction through a point, the counterpart of
#' [ggplot2::geom_abline()].
#'
#' Intercepts given as arguments draw one line per value and stay out of the
#' legend; mapped intercepts draw one line per row. As in ggplot2, intercepts
#' are positions and expand the axis ranges to include them.
#'
#' A line from `geom_abline_3d()` passes through `point` along `direction`.
#' Both are in the plot's transformed scale space, so a direction of
#' `c(1, 0, 2)` rises two z units per x unit. The point and direction do not
#' expand the axis ranges: each line is cut to the panel box when the panel is
#' drawn, and a line that misses the box is skipped with a warning.
#' Directions are given as arguments (one line per row of a three-column
#' matrix) or mapped to `dx`, `dy` and `dz`; mapped lines share one point.
#'
#' Both lines are depth-sorted with the rest of the scene. In a curvilinear
#' frame such as [coord_cylindrical_3d()] they bend with the frame, as a
#' horizontal line becomes a circle in [ggplot2::coord_radial()].
#' @param mapping,data,...,na.rm,show.legend,inherit.aes Standard layer
#'   arguments. `...` takes fixed aesthetics such as `colour`, `linewidth`,
#'   `linetype` and `alpha`, and the 3D drawing parameters such as `hidden`
#'   and `occlusion`.
#' @param xintercept,yintercept,zintercept Positions of the held axes. Give
#'   exactly two; vectors of length one or a common length draw several lines.
#' @param point A point each line passes through: a numeric vector `c(x, y, z)`,
#'   or a three-column matrix with one row per line.
#' @param direction Directions of the lines: a numeric vector `c(dx, dy, dz)`
#'   or a three-column matrix with one row per line. When `NULL`, the
#'   directions come from the `dx`, `dy` and `dz` aesthetics.
#' @return A ggplot2 layer.
#' @section Aesthetics:
#' `geom_refline_3d()` understands `xintercept`, `yintercept`, `zintercept`,
#' `colour`, `linewidth`, `linetype` and `alpha`; `geom_abline_3d()`
#' understands `dx`, `dy`, `dz`, `colour`, `linewidth`, `linetype` and `alpha`.
#' @examples
#' d <- data.frame(x = rnorm(60), y = rnorm(60), z = rnorm(60))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
#'
#' # A vertical line through the origin and a line along x at y = 1, z = 0
#' p + geom_refline_3d(xintercept = 0, yintercept = 0, colour = "red") +
#'   geom_refline_3d(yintercept = 1, zintercept = 0, linetype = 2) +
#'   coord_3d()
#'
#' # The diagonal through the centre of the data, and the first principal axis
#' axis1 <- prcomp(d)$rotation[, 1]
#' p + geom_abline_3d(point = colMeans(d), direction = rbind(c(1, 1, 1), axis1),
#'   colour = c("grey50", "red")) +
#'   coord_3d()
#' @export
geom_refline_3d <- function(mapping = NULL, data = NULL, ..., xintercept, yintercept, zintercept,
                            na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  given <- list(xintercept = if (!missing(xintercept)) xintercept,
                yintercept = if (!missing(yintercept)) yintercept,
                zintercept = if (!missing(zintercept)) zintercept)
  given <- Filter(Negate(is.null), given)
  if (length(given)) {
    if (length(given) != 2L) stop("Give exactly two of xintercept, yintercept and zintercept.", call. = FALSE)
    data <- recycle_columns_3d(given)
    mapping <- ggplot2::aes_all(names(given))
    show.legend <- FALSE
  }
  make_layer_3d(GeomRefline3D, mapping, data, "identity", "identity",
    c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_refline_3d
#' @export
geom_abline_3d <- function(mapping = NULL, data = NULL, ..., point = c(0, 0, 0), direction = NULL,
                           na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  point <- line_rows_3d(point, "point")
  if (!is.null(direction)) {
    direction <- line_rows_3d(direction, "direction")
    if (any(rowSums(direction != 0) == 0L)) stop("Every direction needs a nonzero component.", call. = FALSE)
    lines <- max(nrow(point), nrow(direction))
    if (!all(c(nrow(point), nrow(direction)) %in% c(1L, lines))) {
      stop("point and direction need one row or a common number of rows.", call. = FALSE)
    }
    data <- recycle_columns_3d(list(dx = direction[, 1L], dy = direction[, 2L], dz = direction[, 3L]), lines)
    mapping <- ggplot2::aes_all(c("dx", "dy", "dz"))
    show.legend <- FALSE
  } else if (nrow(point) != 1L) {
    stop("Mapped directions share one point.", call. = FALSE)
  }
  make_layer_3d(GeomAbline3D, mapping, data, "identity", "identity",
    c(list(point = point, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# A vector of three coordinates or a three-column matrix, as rows.
line_rows_3d <- function(value, name) {
  if (is.data.frame(value)) value <- as.matrix(value)
  if (is.null(dim(value))) value <- matrix(value, nrow = 1L)
  if (!is.numeric(value) || ncol(value) != 3L || !nrow(value) || any(!is.finite(value))) {
    stop(name, " must be finite numbers c(x, y, z) or a three-column matrix.", call. = FALSE)
  }
  unname(value)
}

# The common length of vectors of length one or n; an error names the others.
common_length_3d <- function(sizes, what = "lengths") {
  n <- max(c(1L, sizes))
  if (any(!sizes %in% c(1L, n))) {
    stop("Unequal ", what, ": ", paste0(names(sizes), " (", sizes, ")", collapse = ", "), ".", call. = FALSE)
  }
  n
}

recycle_columns_3d <- function(columns, n = common_length_3d(lengths(columns))) {
  as.data.frame(lapply(columns, rep, length.out = n))
}

emit_reflines_3d <- function(data, params, panel_params) {
  fixed <- reference_intercepts_3d[reference_intercepts_3d %in% names(data)]
  if (length(fixed) != 2L) stop("geom_refline_3d() needs exactly two of xintercept, yintercept and zintercept.", call. = FALSE)
  emit_axis_lines_3d(data, fixed, params, panel_params)
}

# The point is normalised to box units like any position; a direction is a
# difference of positions, so it is only divided by each axis range.
emit_ablines_3d <- function(data, params, panel_params) {
  n <- nrow(data)
  point <- params$point
  point <- point[if (nrow(point) == 1L) rep.int(1L, n) else data$.source, , drop = FALSE]
  normal <- normalise_positions_3d(data.frame(x = point[, 1L], y = point[, 2L], z = point[, 3L]), panel_params)
  spans <- vapply(c("x", "y", "z"), function(axis) diff(panel_params$ranges[[axis]]), numeric(1))
  direction <- cbind(data$dx %or3d% rep(0, n), data$dy %or3d% rep(0, n), data$dz %or3d% rep(0, n))
  direction <- sweep(direction, 2L, spans, "/")
  emit_box_lines_3d(data, as.matrix(normal), direction, params, panel_params, "lines")
}

register_geom_3d(GeomRefline3D, emit_reflines_3d, default_z = -Inf, tween = "row")
register_geom_3d(GeomAbline3D, emit_ablines_3d, parameters = list(point = c(0, 0, 0)),
  default_z = -Inf, positions = character(), tween = "row")
register_geom_3d(ggplot2::GeomHline, function(data, params, panel_params) {
  emit_axis_lines_3d(data, c(y = "yintercept", z = "z"), params, panel_params)
}, default_z = -Inf, tween = "row")
register_geom_3d(ggplot2::GeomVline, function(data, params, panel_params) {
  emit_axis_lines_3d(data, c(x = "xintercept", z = "z"), params, panel_params)
}, default_z = -Inf, tween = "row")

emit_floor_ablines_3d <- function(data, params, panel_params) {
  x <- panel_params$ranges$x; y <- panel_params$ranges$y
  n <- nrow(data)
  point <- cbind(rep(0, n), (data$intercept + data$slope * x[1L] - y[1L]) / diff(y), data$z)
  direction <- cbind(rep(1, n), data$slope * diff(x) / diff(y), rep(0, n))
  emit_box_lines_3d(data, point, direction, params, panel_params, "reference lines")
}

register_geom_3d(ggplot2::GeomAbline, emit_floor_ablines_3d, default_z = -Inf, tween = "row")
