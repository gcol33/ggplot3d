#' Sample a bivariate function in three dimensions
#'
#' The function receives transformed x and y coordinates and returns transformed
#' z coordinates. Thus, on a log10 z scale, returning x + y draws z = 10^(x+y)
#' in original units. Existing geometric coordinates pass through scale mapping
#' once. Newly mapped after_stat variables follow ggplot2 scale transformation.
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param geom Geom prototype for keyed triangle corners.
#' @param fun Vectorised function of x and y.
#' @param n Grid resolution, a scalar or two integers.
#' @param args Additional named arguments passed to fun.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   stat_function_3d(fun = function(x, y) sin(x) * cos(y), n = 15) +
#'   ggplot2::xlim(-3, 3) + ggplot2::ylim(-3, 3) + coord_3d()
#' ggplot2::ggplot() +
#'   geom_function_3d(fun = function(x, y) x^2 + y^2, n = 8) +
#'   ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + coord_3d()
stat_function_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                              position = "identity", ..., fun, n = 40L, args = list(),
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatFunction3D, position,
    stats_params_3d(list(fun = fun, n = n, args = args, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_function_3d
#' @export
geom_function_3d <- function(...) stat_function_3d(...)

#' Fit a bivariate response surface
#'
#' Fits and predictions use transformed x, y and z coordinates. The default lm
#' formula is z ~ x + y, loess uses a quadratic local surface, and gam uses
#' z ~ s(x, y). Bounds are pointwise confidence surfaces for the conditional mean.
#' The computed surface variable identifies fit, lower and upper surfaces.
#' The computed z_mean is the transformed mean height of each triangle.
#' @inheritParams stat_function_3d
#' @param method One of loess, lm or gam. gam requires mgcv.
#' @param formula Model formula using x, y and z.
#' @param se Whether to include lower and upper standard-error surfaces.
#' @param level Pointwise confidence level.
#' @param span Loess span.
#' @param method.args Named arguments passed to the model fitter.
#' @param se.alpha Opacity multiplier for confidence surfaces.
#' @export
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
#' d$z <- with(d, x + y + sin(3*x*y) / 5)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_smooth_3d(method = "lm", n = 12) + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()
stat_smooth_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                            position = "identity", ..., method = "loess", formula = NULL,
                            n = 30L, se = TRUE, level = 0.95, span = 0.75,
                            method.args = list(), se.alpha = 0.2,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSmooth3D, position,
    stats_params_3d(list(method = method, formula = formula, n = n, se = se, level = level, span = span,
      method.args = method.args, se.alpha = se.alpha, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_smooth_3d
#' @export
geom_smooth_3d <- function(...) stat_smooth_3d(...)

#' Bivariate kernel density as a height surface
#'
#' Uses an axis-aligned Gaussian product kernel on transformed x/y coordinates.
#' Bandwidth h follows the MASS kde2d convention used by ggplot2: the Gaussian
#' standard deviation is h / 4. Automatic bandwidths are 4 * bw.nrd per axis,
#' with bw.nrd0 for a zero interquartile range. adjust applies to automatic h.
#' Height is density in original z units and receives the z transformation once.
#' Computed density, ndensity and count are averaged per triangle for fill mapping.
#' @inheritParams stat_function_3d
#' @param h Positive bandwidth, scalar or one per axis; NULL estimates it.
#' @param adjust Positive automatic bandwidth multiplier, scalar or one per axis.
#' @export
#' @examples
#' ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
#'   stat_density_2d_3d(n = 20) + coord_3d()
stat_density_2d_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                position = "identity", ..., h = NULL, adjust = 1, n = 40L,
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatDensity2D3D, position,
    stats_params_3d(list(h = h, adjust = adjust, n = n, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' Grouped one-dimensional density ridges
#'
#' Each group has a single position on the perpendicular x/y axis. Densities
#' are fitted along the chosen transformed axis. base and height specify the
#' baseline and density multiplier in transformed z units. Each ridge is a
#' filled, triangulated vertical sheet. Use group for multiple ridges.
#' Density tails within floating-point roundoff of zero are set to zero;
#' collapsed triangles are omitted.
#' @inheritParams stat_density_2d_3d
#' @param along Axis along which observations vary, x or y.
#' @param bw Bandwidth passed to stats::density.
#' @param base Baseline in transformed z units.
#' @param height Density height multiplier in transformed z units.
#' @export
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, as.numeric(Species),
#'   group = Species, fill = Species)) + geom_ridgeline_3d(n = 32) + coord_3d()
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, as.numeric(Species),
#'   group = Species, fill = Species)) + stat_ridgeline_3d(n = 24) + coord_3d()
stat_ridgeline_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                               position = "identity", ..., along = "x", n = 128L,
                               bw = "nrd0", adjust = 1, base = 0, height = 1,
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatRidgeline3D, position,
    stats_params_3d(list(along = along, n = n, bw = bw, adjust = adjust, base = base, height = height,
      na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_ridgeline_3d
#' @export
geom_ridgeline_3d <- function(...) stat_ridgeline_3d(...)
