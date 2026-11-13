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
#' @export
zlim <- function(...) {
  as_z_scale(ggplot2::xlim(...), "auto")
}
