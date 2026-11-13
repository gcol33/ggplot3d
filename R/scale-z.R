z_aesthetics <- c("z", "zmin", "zmax", "zend", "zintercept")

as_z_scale <- function(scale, position) {
  choices <- c("auto", "xmin-ymin", "xmin-ymax", "xmax-ymin", "xmax-ymax")
  scale$position <- match.arg(position, choices)
  scale$aesthetics <- z_aesthetics
  scale
}

#' Continuous z position scales
#'
#' These scales use ggplot2 position scale transformations, limits and breaks.
#' @param ... Arguments passed to the corresponding ggplot2 x scale constructor.
#' @param position Cube edge used for the z guide.
#' @return A ggplot2 position scale for the z aesthetic family.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 2, 1), z = c(1, 10, 100))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d() + coord_3d()
#' p + scale_z_continuous(name = "Height")
#' p + scale_z_log10()
#' p + scale_z_sqrt()
#' p + scale_z_reverse()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = factor(z))) +
#'   geom_point_3d() + scale_z_discrete() + coord_3d()
#' d$day <- as.Date("2026-01-01") + 0:2
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = day)) +
#'   geom_point_3d() + scale_z_date() + coord_3d()
#' d$time <- as.POSIXct("2026-01-01", tz = "UTC") + (0:2) * 3600
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = time)) +
#'   geom_point_3d() + scale_z_datetime() + coord_3d()
#' d$elapsed <- as.difftime(1:3, units = "hours")
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = elapsed)) +
#'   geom_point_3d() + scale_z_time() + coord_3d()
#' @export
scale_z_continuous <- function(..., position = "auto") {
  as_z_scale(ggplot2::scale_x_continuous(...), position)
}

#' @rdname scale_z_continuous
#' @export
scale_z_discrete <- function(..., position = "auto") {
  as_z_scale(ggplot2::scale_x_discrete(...), position)
}

#' @rdname scale_z_continuous
#' @export
scale_z_log10 <- function(...) scale_z_continuous(..., transform = "log10")

#' @rdname scale_z_continuous
#' @export
scale_z_sqrt <- function(...) scale_z_continuous(..., transform = "sqrt")

#' @rdname scale_z_continuous
#' @export
scale_z_reverse <- function(...) scale_z_continuous(..., transform = "reverse")

#' @rdname scale_z_continuous
#' @export
scale_z_date <- function(..., position = "auto") {
  as_z_scale(ggplot2::scale_x_date(...), position)
}

#' @rdname scale_z_continuous
#' @export
scale_z_datetime <- function(..., position = "auto") {
  as_z_scale(ggplot2::scale_x_datetime(...), position)
}

#' @rdname scale_z_continuous
#' @export
scale_z_time <- function(..., position = "auto") {
  as_z_scale(ggplot2::scale_x_time(...), position)
}

infer_z_scale <- function(data) {
  columns <- data[intersect(names(data), z_aesthetics)]
  columns <- Filter(function(x) any(!is.na(x) & (!is.numeric(x) | is.finite(x))), columns)
  if (!length(columns)) return(NULL)
  types <- vapply(columns, function(x) ggplot2::scale_type(x)[1L], character(1))
  if (length(unique(types)) > 1L) stop("Incompatible types in the z position family.", call. = FALSE)
  constructors <- list(continuous = scale_z_continuous, discrete = scale_z_discrete,
                       date = scale_z_date, datetime = scale_z_datetime,
                       time = scale_z_time)
  constructor <- constructors[[types[1L]]]
  if (is.null(constructor)) stop("Unsupported z scale type: ", types[1L], call. = FALSE)
  constructor()
}

#' Set z scale limits
#' @param ... Two limits or one vector of limits, in original data units.
#' @return A z position scale.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 2, 1), z = 1:3)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d() + zlim(0, 4) + coord_3d()
#' @export
zlim <- function(...) {
  as_z_scale(ggplot2::xlim(...), "auto")
}
