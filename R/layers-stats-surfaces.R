#' Sample a bivariate function in three dimensions
#'
#' The sampling grid is evenly spaced in transformed x and y coordinates.
#' The function receives x and y in original data units and returns z in data
#' units. The z scale transforms the returned heights once.
#' Newly mapped after_stat variables follow ggplot2 scale transformation.
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
#' Fits a model of z on x and y in each group and draws its predictions on an
#' x/y grid. As in [ggplot2::stat_smooth()], `method` is a fitting function or
#' its name, called as `method(formula, data = data, weights = weight, ...)`
#' with `method.args` in `...`, and the surface is `predict(fit, newdata,
#' se.fit = )`. Any model with such a `predict()` method works: thin-plate or
#' tensor-product smooths from mgcv, robust or generalised linear models,
#' random forests. Fitting functions see transformed x, y and z. The default
#' formula is `z ~ x + y`; `"gam"` uses `z ~ s(x, y)` from mgcv, and loess
#' takes `span`.
#'
#' `method` may instead be a model fitted beforehand. It is evaluated in data
#' units: the grid's x and y are back-transformed and named by `view`, the
#' predictions are transformed by the z scale. When `view` is `NULL` it takes
#' the variable names of the layer's x and y mappings, if these are bare column
#' names, else `c("x", "y")`. Other predictors of the model take the values in
#' `cond`, or by default the observed value closest to the median and the most
#' frequent level, as in [mgcv::vis.gam()].
#'
#' Bounds are pointwise confidence intervals for the conditional mean, with
#' Student t quantiles on the residual degrees of freedom, and normal quantiles
#' on the link scale mapped through the inverse link for glm fits, as in
#' ggplot2.
#'
#' @section Computed variables:
#' \describe{
#'   \item{surface}{`"fit"`, `"lower"`, `"upper"`, and `"side"` for the walls of a band.}
#'   \item{se}{Standard error of the fitted mean on the model's scale (link
#'     scale for glm fits), averaged over each triangle.}
#'   \item{z_mean}{Transformed mean height of each triangle.}
#' }
#' @inheritParams stat_function_3d
#' @param method A fitting function, the name of one (`"loess"`, `"lm"`,
#'   `"gam"`, `"glm"`, ...), or a fitted model with a `predict()` method.
#'   `"gam"` requires mgcv.
#' @param formula Model formula in x, y and z, for a fitting function.
#' @param se Confidence display: `"surfaces"` (or `TRUE`) draws translucent
#'   lower and upper surfaces; `"band"` closes them with walls along the edge
#'   of the domain into one translucent volume around the fit; `"fill"` draws
#'   the fit alone, filled by the standard error unless fill is mapped or set;
#'   `"none"` (or `FALSE`) draws the fit alone.
#' @param level Pointwise confidence level.
#' @param span Loess span.
#' @param method.args Named arguments passed to the fitting function.
#' @param se.alpha Opacity multiplier for confidence surfaces and walls.
#' @param domain `"box"` fills the x/y range of the panel; `"hull"` clips the
#'   surface to the convex hull of the group's observations, so the model is
#'   not drawn where it extrapolates. Grid cells crossed by the hull edge are
#'   cut along it and the model is evaluated on the cut.
#' @param view For a fitted model, the names of its predictors drawn on x and y.
#' @param cond For a fitted model, a named list holding a value for each
#'   other predictor.
#' @export
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
#' d$z <- with(d, x + y + sin(3*x*y) / 5)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_smooth_3d(method = "lm", n = 12) + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()
#'
#' # A fitting function with its own formula: a quadratic response surface
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_smooth_3d(method = lm, formula = z ~ poly(x, y, degree = 2),
#'     se = "band", n = 12) + coord_3d()
#'
#' # A model fitted beforehand, drawn over the hull of the data and filled by
#' # its standard error
#' fit <- lm(mpg ~ poly(wt, 2) + hp, data = mtcars)
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
#'   geom_smooth_3d(method = fit, domain = "hull", se = "fill", n = 20) +
#'   geom_point_3d() + coord_3d()
stat_smooth_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                            position = "identity", ..., method = "loess", formula = NULL,
                            n = 30L, se = TRUE, level = 0.95, span = 0.75,
                            method.args = list(), se.alpha = 0.2, domain = c("box", "hull"),
                            view = NULL, cond = list(),
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  domain <- match.arg(domain)
  extra <- list(...)
  if (identical(smooth_se_mode_3d(se), "fill"))
    mapping <- stats_default_mapping_3d(mapping, extra, ggplot2::aes(fill = ggplot2::after_stat(se)))
  model_view_layer_3d(make_layer_3d(geom, mapping, data, StatSmooth3D, position,
    stats_params_3d(list(method = method, formula = formula, n = n, se = se, level = level, span = span,
      method.args = method.args, se.alpha = se.alpha, domain = domain, view = view, cond = cond,
      na.rm = na.rm, shade = TRUE), extra), show.legend, inherit.aes), method, view)
}

#' @rdname stat_smooth_3d
#' @export
geom_smooth_3d <- function(...) stat_smooth_3d(...)

#' Residuals from a fitted surface
#'
#' Fits a model of z on x and y as [stat_smooth_3d()] does and draws a
#' vertical segment from each observation to the fitted surface, the 3D
#' counterpart of residual drop lines in regression diagnostics. `method`,
#' `formula`, `span`, `method.args`, `view` and `cond` work as in
#' [stat_smooth_3d()], so pairing both layers with the same arguments draws
#' the residuals of the surface that is shown.
#'
#' @section Computed variables:
#' \describe{
#'   \item{fitted}{Fitted height at the observation, in transformed z.}
#'   \item{resid}{Observed minus fitted height, in transformed z.}
#'   \item{sign}{Sign of the residual: -1, 0 or 1.}
#' }
#' @inheritParams stat_smooth_3d
#' @param geom Geom drawing the residuals; segments by default.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(x = runif(40), y = runif(40))
#' d$z <- with(d, x + 2 * y + rnorm(40, sd = 0.2))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_smooth_3d(method = "lm", se = FALSE, n = 6, alpha = 0.6) +
#'   geom_residual_3d(ggplot2::aes(colour = ggplot2::after_stat(factor(sign))), method = "lm") +
#'   geom_point_3d() + coord_3d()
stat_residual_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomSegment,
                              position = "identity", ..., method = "loess", formula = NULL,
                              span = 0.75, method.args = list(), view = NULL, cond = list(),
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  model_view_layer_3d(make_layer_3d(geom, mapping, data, StatResidual3D, position,
    stats_params_3d(list(method = method, formula = formula, span = span, method.args = method.args,
      view = view, cond = cond, na.rm = na.rm), list(...)), show.legend, inherit.aes), method, view)
}

#' @rdname stat_residual_3d
#' @export
geom_residual_3d <- function(...) stat_residual_3d(...)

# For a fitted model without an explicit view, names the predictors after the
# layer's x and y mappings when these are bare column names.
model_view_layer_3d <- function(layer, method, view) {
  if (is.function(method) || is.character(method) || !is.null(view)) return(layer)
  parent <- layer
  params <- layer$stat_params
  ggplot2::ggproto(NULL, layer,
    compute_statistic = function(self, data, layout) {
      self$stat_params <- params
      self$stat_params$view <- mapping_view_3d(self$computed_mapping)
      ggplot2::ggproto_parent(parent, self)$compute_statistic(data, layout)
    }
  )
}

mapping_view_3d <- function(mapping) {
  names <- vapply(c("x", "y"), function(aesthetic) {
    value <- mapping[[aesthetic]]
    expr <- if (is.null(value)) NULL else if (is.language(value) && inherits(value, "formula")) unclass(value)[[2L]] else value
    if (is.symbol(expr)) as.character(expr) else NA_character_
  }, character(1))
  if (anyNA(names)) NULL else unname(names)
}

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
#' @param plane `"surface"` draws density as height. `"floor"` or a numeric z
#'   in original units lays the density grid flat at that height, filled by
#'   density unless fill is mapped or set, and unshaded unless `shade = TRUE`.
#'   The density range still trains the z scale, as for contours. A flat
#'   plane is a shorthand for [position_project_3d()].
#' @export
#' @examples
#' ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
#'   stat_density_2d_3d(n = 20) + coord_3d()
#' ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
#'   stat_density_2d_3d(n = 20, alpha = 0.6) +
#'   stat_density_2d_3d(n = 40, plane = "floor") + coord_3d()
stat_density_2d_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                position = "identity", ..., h = NULL, adjust = 1, n = 40L,
                                plane = c("surface", "floor"),
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  plane <- check_plane_3d(plane)
  extra <- list(...)
  flat <- !identical(plane, "surface")
  if (flat) mapping <- stats_default_mapping_3d(mapping, extra, ggplot2::aes(fill = ggplot2::after_stat(density)))
  make_layer_3d(geom, mapping, data, StatDensity2D3D, plane_position_3d(plane, position),
    stats_params_3d(list(h = h, adjust = adjust, n = n, na.rm = na.rm, shade = !flat), extra),
    show.legend, inherit.aes)
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
