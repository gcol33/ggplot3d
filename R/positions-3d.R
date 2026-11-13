#' Stack column heights along z
#' @param vjust Fraction of each stacked interval used for z.
#' @param reverse Reverse group order.
#' @return A ggplot2 position adjustment.
#' @examples
#' d <- data.frame(x = 1, y = 1, z = c(2, 3), part = c("a", "b"))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
#'   geom_col_3d(position = position_stack_3d()) + coord_3d()
#' @export
position_stack_3d <- function(vjust = 1, reverse = FALSE) {
  stopifnot(length(vjust) == 1L, is.finite(vjust), length(reverse) == 1L, !is.na(reverse))
  ggplot2::ggproto(NULL, PositionStack3D, vjust = vjust, reverse = reverse)
}

PositionStack3D <- ggplot2::ggproto("PositionStack3D", ggplot2::Position,
  required_aes = c("x", "y", "z"),
  setup_params = function(self, data) list(vjust = self$vjust, reverse = self$reverse),
  compute_panel = function(data, params, scales) {
    check_stack_scale_3d(scales)
    if (any(data$zmin != 0, na.rm = TRUE)) stop("Z stacking requires a shared zero baseline.", call. = FALSE)
    data$zmin <- data$zmin %or3d% rep(0, nrow(data))
    data$zmax <- data$z
    order <- order(data$x, data$y, if (params$reverse) data$group else -data$group)
    cells <- interaction(data$x, data$y, drop = TRUE)
    for (rows in split(order, cells[order])) {
      for (part in list(rows[which(data$z[rows] >= 0)], rows[which(data$z[rows] < 0)])) {
        top <- cumsum(data$z[part])
        data$zmin[part] <- top - data$z[part]
        data$zmax[part] <- top
        data$z[part] <- data$zmin[part] + params$vjust * (top - data$zmin[part])
      }
    }
    data
  }
)

check_stack_scale_3d <- function(scales) {
  if (scales$z$is_discrete() || !identical(scales$z$get_transformation()$name, "identity")) {
    stop("Z stacking requires a linear continuous z scale.", call. = FALSE)
  }
}

check_position_3d <- function(position, scales, stack = FALSE) {
  if (inherits(position, "PositionStack3D")) {
    if (!stack) stop("Z stacking requires columns.", call. = FALSE)
    check_stack_scale_3d(scales)
  }
  else if (!inherits(position, "PositionIdentity") && !inherits(position, "PositionNudge3D")) {
    stop("Unsupported 3D position: ", class(position)[1L], ".", call. = FALSE)
  }
}

#' Nudge all three position families
#' @param x,y,z Displacements in transformed coordinate units.
#' @param nudge_z Optional spelling of the z displacement.
#' @return A ggplot2 position adjustment.
#' @examples
#' ggplot2::ggplot(data.frame(x = 1, y = 1, z = 1), ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(position = position_nudge_3d(nudge_z = 0.5)) + coord_3d()
#' @export
position_nudge_3d <- function(x = 0, y = 0, z = 0, nudge_z = NULL) {
  if (!is.null(nudge_z)) {
    if (!missing(z)) stop("Supply only one of z and nudge_z.", call. = FALSE)
    z <- nudge_z
  }
  stopifnot(all(vapply(list(x, y, z), function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1))))
  ggplot2::ggproto(NULL, PositionNudge3D, x = x, y = y, z = z)
}

PositionNudge3D <- ggplot2::ggproto("PositionNudge3D", ggplot2::PositionNudge,
  setup_params = function(self, data) list(x = self$x, y = self$y, z = self$z),
  compute_layer = function(data, params, layout) {
    families <- list(x = c("x", "xmin", "xmax", "xend", "xintercept"),
                     y = c("y", "ymin", "ymax", "yend", "yintercept"), z = z_aesthetics)
    for (axis in names(families)) for (column in intersect(families[[axis]], names(data))) {
      data[[column]] <- data[[column]] + params[[axis]]
    }
    data
  }
)
