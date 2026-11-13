#' Draw an intrinsic ball, tube, or half-space
#' @param shape `"ball"`, `"cylinder"`, or `"halfspace"`.
#' @param center Three coordinates of the centre or a point on the boundary.
#' @param radius Positive intrinsic radius for balls and cylinders.
#' @param direction Unit normal for a half-space at its centre. Nil and Sol
#'   use chart half-spaces; product spaces support vertical or horizontal ones.
#' @param ... Material parameters including fill.
#' @details This layer uses `backend_3d("raymarch")`. Balls use intrinsic
#'   distance in E3, S3, H3, the product spaces, Nil and SL2R. Sol balls use
#'   the explicitly approximate level set
#'   `sqrt(exp(-2*z)*x^2 + exp(2*z)*y^2 + z^2) = radius` in coordinates
#'   left-translated to the centre, as in Coulon et al. section 11.5.
#'   Cylinders surround the vertical geodesic or fibre through the centre;
#'   Sol cylinders surround the left translate of the x-axis subgroup.
#'   S3 cylinder radii must be below pi/2 and S3 ball radii below pi.
#'   General chart-defined CSG surfaces use [geom_isofunction_3d()] with
#'   `render = "trace"`; their SDF algebra is compiled by the shared registry.
#' @return A three-dimensional ggplot layer.
#' @examples
#' ggplot2::ggplot() + geom_thurston_3d(center = c(0, 0, 2), fill = "steelblue") +
#'   coord_3d(backend = backend_3d("raymarch", geometry = "nil", dpi = 40))
#' @export
geom_thurston_3d <- function(shape = "ball", center = c(0, 0, 2), radius = 0.5, direction = c(0, 0, 1), ...) {
  shape <- match.arg(shape, c("ball", "cylinder", "halfspace"))
  center <- thurston_point_3d(center, "e3", "center")
  direction <- thurston_point_3d(direction, "e3", "direction")
  if (sum(direction^2) == 0) stop("direction must be nonzero.", call. = FALSE)
  spec <- list(kind = "thurston", shape = match(shape, c("ball", "cylinder", "halfspace")) - 1L,
    center = center, radius = check_positive_3d(radius, "radius"), direction = direction / sqrt(sum(direction^2)))
  make_layer_3d(GeomRaymarch3D, NULL, stats_seed_data_3d(NULL), "identity", "identity",
    stats_params_3d(list(ray_spec = spec), list(...)), FALSE, FALSE)
}

