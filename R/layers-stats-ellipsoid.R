#' Covariance and confidence ellipsoids
#'
#' Fits one ellipsoid to each group of points and draws it as a solid with
#' [geom_ellipsoid_3d()]: the three-dimensional counterpart of
#' [ggplot2::stat_ellipse()], whose `type` and `level` it takes over. The
#' ellipsoid is the unit sphere carried through the square root of a shape
#' matrix S onto the set of points p with (p - centre)' S^-1 (p - centre) <= 1,
#' computed in the scale-transformed units of the position axes.
#'
#' `type` chooses the centre and the scatter matrix V:
#' * `"t"` (the default) the location and scatter of a multivariate t
#'   distribution with five degrees of freedom, fitted by iterative
#'   reweighting as `MASS::cov.trob()` does, which keeps outlying points from
#'   inflating the ellipsoid;
#' * `"norm"` the mean and the sample covariance;
#' * `"euclid"` the mean and a sphere of radius `level`, in data units.
#'
#' `kind` chooses what the ellipsoid covers, with p = 3 dimensions and n
#' points in the group:
#' * `"data"` (the default) the spread of the observations, S = r^2 V with
#'   r^2 = p F(level; p, n - 1), the scaling of [ggplot2::stat_ellipse()];
#' * `"se"` the confidence region of the centre, S = r^2 V / n with
#'   r^2 = p (n - 1) / (n - p) F(level; p, n - p), Hotelling's T^2 region
#'   for the mean of a normal sample. `vegan3d::orglellipse(kind = "se")`
#'   draws the same shape V / n scaled by a chi-square quantile, the
#'   large-sample limit of r^2;
#' * `"ehull"` the smallest ellipsoid that contains every point of the group,
#'   as `vegan3d::orglellipse(kind = "ehull")`, computed by Khachiyan's
#'   algorithm; `type` and `level` are not used.
#'
#' A group needs at least four points that do not lie on one plane; other
#' groups are dropped with a message.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x, y, z}{Centre of the ellipsoid.}
#'   \item{sigma}{Shape matrix S, a list-column of 3 x 3 matrices.}
#'   \item{level}{Confidence or coverage level; 1 for `kind = "ehull"`.}
#'   \item{volume}{Volume of the ellipsoid, 4/3 pi sqrt(det S), in cubed
#'     scale-transformed units.}
#'   \item{n}{Number of points in the group.}
#' }
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_ellipsoid_3d
#' @param geom The geometry drawing the ellipsoids, by default the one of
#'   [geom_ellipsoid_3d()].
#' @param type Centre and scatter estimate: `"t"`, `"norm"` or `"euclid"`.
#' @param level Confidence or coverage level, between zero and one; for
#'   `type = "euclid"` the radius of the sphere.
#' @param kind What the ellipsoid covers: `"data"`, `"se"` or `"ehull"`.
#' @references Kent, Tyler and Vardi (1994). A curious likelihood identity
#'   for the multivariate t-distribution. Communications in Statistics -
#'   Simulation and Computation 23, 441-453. \doi{10.1080/03610919408813180}
#'
#'   Hotelling (1931). The generalization of Student's ratio. Annals of
#'   Mathematical Statistics 2, 360-378. \doi{10.1214/aoms/1177732979}
#'
#'   Todd and Yildirim (2007). On Khachiyan's algorithm for the computation of
#'   minimum-volume enclosing ellipsoids. Discrete Applied Mathematics 155,
#'   1731-1744. \doi{10.1016/j.dam.2007.02.013}
#' @seealso [geom_ellipsoid_3d()] for ellipsoids given by their axes or
#'   shape matrices.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' p <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width,
#'   z = Petal.Length, fill = Species))
#' p + geom_point_3d(ggplot2::aes(colour = Species)) +
#'   stat_ellipsoid_3d(alpha = 0.3) + coord_3d()
#'
#' # Confidence regions of the three species means, with their principal axes.
#' p + geom_point_3d(ggplot2::aes(colour = Species), size = 0.6) +
#'   stat_ellipsoid_3d(kind = "se", type = "norm", alpha = 0.4, axes = TRUE) +
#'   coord_3d()
#'
#' # The smallest ellipsoid around each species.
#' p + geom_point_3d(ggplot2::aes(colour = Species)) +
#'   stat_ellipsoid_3d(kind = "ehull", alpha = 0.2, detail = 3) + coord_3d()
stat_ellipsoid_3d <- function(mapping = NULL, data = NULL, geom = GeomEllipsoid3D,
                              position = "identity", ..., type = "t", level = 0.95,
                              kind = "data", detail = 2L, axes = FALSE, shade = TRUE,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatEllipsoid3D, position,
    stats_params_3d(c(check_ellipsoid_params_3d(type, level, kind), list(
      detail = check_detail_3d(detail), axes = check_flag_3d(axes, "axes"), shade = shade, na.rm = na.rm)),
      list(...)), show.legend, inherit.aes)
}
