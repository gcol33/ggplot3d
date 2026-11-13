StatFunction3D <- ggplot2::ggproto("StatFunction3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, n = 40L, args = list()) {
    grid <- expand.grid(stats_grid_3d(scales, n))
    x <- stats_scale_3d(scales, "x")$get_transformation()$inverse(grid$x)
    y <- stats_scale_3d(scales, "y")$get_transformation()$inverse(grid$y)
    z <- do.call(fun, c(list(x, y), args))
    if (!is.numeric(z) || !(length(z) %in% c(1L, nrow(grid)))) stop("fun must return one numeric value per grid point.", call. = FALSE)
    grid$z <- stats_scale_3d(scales, "z")$get_transformation()$transform(rep_len(z, nrow(grid)))
    stats_triangles_3d(grid)
  }
)

StatSmooth3D <- ggplot2::ggproto("StatSmooth3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "weight",
  compute_group = function(data, scales, method = "loess", formula = NULL, n = 30L,
                           se = TRUE, level = 0.95, span = 0.75, method.args = list(), se.alpha = 0.2,
                           domain = "box", view = NULL, cond = list()) {
    se <- smooth_se_mode_3d(se)
    domain <- match.arg(domain, c("box", "hull"))
    if (length(se.alpha) != 1L || !is.finite(se.alpha) || se.alpha < 0 || se.alpha > 1) stop("se.alpha must lie between zero and one.", call. = FALSE)
    model <- smooth_model_3d(data, scales, method, formula, span, method.args, view, cond)
    mesh <- stats_mesh_3d(scales, n, domain, data)
    values <- model(mesh$points, se != "none", level)
    if (se != "none" && all(is.na(values$se))) {
      if (se == "fill") stop("se = \"fill\" requires a model whose predict() method returns se.fit.", call. = FALSE)
      warning("The model's predict() method returns no standard errors; drawing the fitted surface only.", call. = FALSE)
      se <- "none"
    }
    smooth_surfaces_3d(mesh, values, se, se.alpha)
  }
)

StatResidual3D <- ggplot2::ggproto("StatResidual3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "weight",
  compute_group = function(data, scales, method = "loess", formula = NULL, span = 0.75,
                           method.args = list(), view = NULL, cond = list()) {
    model <- smooth_model_3d(data, scales, method, formula, span, method.args, view, cond)
    fitted <- model(data[c("x", "y")], FALSE, 0.95)$fit
    data$xend <- data$x; data$yend <- data$y; data$zend <- fitted
    data$fitted <- fitted; data$resid <- data$z - fitted; data$sign <- sign(data$resid)
    data
  }
)

smooth_se_mode_3d <- function(se) {
  if (is.logical(se) && length(se) == 1L && !is.na(se)) return(if (se) "surfaces" else "none")
  if (!is.character(se) || length(se) != 1L || !se %in% c("surfaces", "band", "fill", "none"))
    stop("se must be TRUE, FALSE, \"surfaces\", \"band\", \"fill\" or \"none\".", call. = FALSE)
  se
}

# Returns function(xy, se, level) evaluating the fitted surface at transformed
# x/y positions, as transformed fit, lower and upper heights and the standard
# error on the model's scale (NA without standard errors). A fitting function
# is called as ggplot2::stat_smooth() calls it, on transformed x, y and z; a
# fitted model is evaluated in data units, with its predictors named by view
# and the remaining predictors held at cond.
smooth_model_3d <- function(data, scales, method, formula, span, method.args, view, cond) {
  if (stats_scale_3d(scales, "z")$is_discrete()) stop("Smoothing requires a continuous z scale.", call. = FALSE)
  if (!is.list(method.args)) stop("method.args must be a list.", call. = FALSE)
  if (is.function(method) || is.character(method)) {
    fitter <- smooth_fitter_3d(method)
    args <- c(list(formula = formula %or3d% fitter$formula, data = data),
      if (identical(fitter$fun, stats::loess)) list(span = span), method.args)
    if ("weight" %in% names(data)) args$weights <- data$weight
    model <- do.call(fitter$fun, args)
    return(function(xy, se, level) smooth_prediction_3d(model, xy, se, level))
  }
  if (!is.null(formula)) stop("formula applies only when method fits a model.", call. = FALSE)
  transform <- lapply(stats::setNames(nm = c("x", "y", "z")), function(axis) stats_scale_3d(scales, axis)$get_transformation())
  function(xy, se, level) {
    newdata <- model_newdata_3d(method, transform$x$inverse(xy$x), transform$y$inverse(xy$y), view, cond)
    out <- smooth_prediction_3d(method, newdata, se, level)
    out$fit <- transform$z$transform(out$fit)
    bounds <- cbind(transform$z$transform(out$lower), transform$z$transform(out$upper))
    out$lower <- pmin(bounds[, 1L], bounds[, 2L]); out$upper <- pmax(bounds[, 1L], bounds[, 2L])
    out
  }
}

smooth_fitter_3d <- function(method) {
  if (is.character(method) && (length(method) != 1L || is.na(method))) stop("method must be a single function name.", call. = FALSE)
  if (identical(method, "gam")) {
    if (!requireNamespace("mgcv", quietly = TRUE)) stop("Install mgcv to use method = 'gam'.", call. = FALSE)
    return(list(fun = mgcv::gam, formula = stats::as.formula("z ~ s(x, y)", env = asNamespace("mgcv"))))
  }
  list(fun = if (is.character(method)) match.fun(method) else method, formula = z ~ x + y)
}

# Pointwise confidence bounds as in ggplot2's predictdf(): Student t with the
# residual degrees of freedom, or normal quantiles on the link scale for glm
# fits, mapped back through the inverse link.
smooth_prediction_3d <- function(model, newdata, se, level) {
  if (se && (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)) stop("level must lie strictly between zero and one.", call. = FALSE)
  pred <- if (!se) stats::predict(model, newdata = newdata)
    else if (inherits(model, "loess")) stats::predict(model, newdata = newdata, se = TRUE)
    else stats::predict(model, newdata = newdata, se.fit = TRUE)
  listed <- is.list(pred) && !is.null(pred$fit)
  fit <- as.double(if (listed) pred$fit else pred)
  if (length(fit) != nrow(newdata)) stop("The model's predict() method must return one value per row of newdata.", call. = FALSE)
  out <- data.frame(fit = fit, lower = NA_real_, upper = NA_real_, se = NA_real_)
  glm <- inherits(model, "glm")
  if (se && listed && !is.null(pred$se.fit)) {
    out$se <- as.double(pred$se.fit)
    df <- if (glm) NULL else pred$df %or3d% tryCatch(stats::df.residual(model), error = function(e) NULL)
    critical <- if (is.numeric(df) && length(df) == 1L && is.finite(df) && df > 0) stats::qt((1 + level) / 2, df) else stats::qnorm((1 + level) / 2)
    out$lower <- fit - critical * out$se; out$upper <- fit + critical * out$se
  }
  if (glm) {
    linkinv <- stats::family(model)$linkinv
    out[c("fit", "lower", "upper")] <- lapply(out[c("fit", "lower", "upper")], linkinv)
  }
  out
}

model_newdata_3d <- function(model, x, y, view, cond) {
  view <- view %or3d% c("x", "y")
  if (!is.character(view) || length(view) != 2L || anyNA(view) || view[1L] == view[2L])
    stop("view must name two different predictors.", call. = FALSE)
  if (!is.list(cond) || (length(cond) && (is.null(names(cond)) || any(names(cond) == ""))))
    stop("cond must be a named list.", call. = FALSE)
  variables <- model_variables_3d(model)
  if (length(variables) && !all(view %in% variables))
    stop("The model has no predictor ", paste0("\"", setdiff(view, variables), "\"", collapse = " or "),
      "; name the predictors drawn on x and y with view = c(\"<x>\", \"<y>\").", call. = FALSE)
  newdata <- stats::setNames(data.frame(x, y), view)
  missing <- setdiff(variables, c(view, names(cond)))
  if (length(missing)) cond <- c(cond, typical_values_3d(model, missing))
  for (name in setdiff(names(cond), view)) {
    if (length(cond[[name]]) != 1L) stop("Each cond value must have length one.", call. = FALSE)
    newdata[[name]] <- rep(cond[[name]], nrow(newdata))
  }
  newdata
}

model_variables_3d <- function(model) {
  terms <- tryCatch(stats::terms(model), error = function(e) NULL)
  if (is.null(terms)) character() else all.vars(stats::delete.response(terms))
}

# Predictors outside the view take the observed value closest to the median,
# or the most frequent level, as in mgcv::vis.gam(). Values are read from the
# untransformed variables of the data the model was fitted to.
typical_values_3d <- function(model, variables) {
  frame <- tryCatch({
    formula <- stats::formula(model)
    stats::get_all_vars(formula, data = eval(stats::getCall(model)$data, environment(formula)))
  }, error = function(e)
    stop("Set ", paste(variables, collapse = ", "), " with cond; the model's data are unavailable: ", conditionMessage(e), call. = FALSE))
  lapply(stats::setNames(nm = variables), function(name) {
    value <- frame[[name]]
    if (is.null(value)) stop("Set ", name, " with cond.", call. = FALSE)
    if (is.numeric(value)) return(value[which.min(abs(value - stats::median(value, na.rm = TRUE)))])
    counts <- table(value)
    level <- names(counts)[which.max(counts)]
    if (is.factor(value)) factor(level, levels = levels(value)) else if (is.logical(value)) as.logical(level) else level
  })
}

smooth_surfaces_3d <- function(mesh, values, se, se.alpha) {
  if (se == "band") values[!(is.finite(values$fit) & is.finite(values$lower) & is.finite(values$upper)), c("fit", "lower", "upper")] <- NA_real_
  base <- mesh$points; base$se <- values$se
  surface <- function(name, z, opacity, layer) {
    d <- base; d$z <- z; d$surface <- name; d$.opacity <- opacity
    out <- stats_mesh_triangles_3d(mesh, d)
    if (nrow(out)) out$.vertex <- out$.vertex + layer * nrow(base)
    out
  }
  out <- list(surface("fit", values$fit, 1, 0L))
  if (se %in% c("surfaces", "band")) out <- c(out, list(surface("lower", values$lower, se.alpha, 1L), surface("upper", values$upper, se.alpha, 2L)))
  if (se == "band") out <- c(out, list(smooth_walls_3d(out[[1L]], base, values, se.alpha)))
  stats_bind_faces_3d(out)
}

# Vertical walls along the boundary edges of the fitted mesh, from the lower
# to the upper confidence surface, so that the band encloses the fit.
smooth_walls_3d <- function(fit, base, values, se.alpha) {
  if (!nrow(fit)) return(data.frame())
  faces <- matrix(fit$.vertex, ncol = 3L, byrow = TRUE)
  edges <- rbind(faces[, 1:2, drop = FALSE], faces[, 2:3, drop = FALSE], faces[, c(3L, 1L), drop = FALSE])
  key <- paste(pmin(edges[, 1L], edges[, 2L]), pmax(edges[, 1L], edges[, 2L]))
  edges <- edges[!key %in% key[duplicated(key)], , drop = FALSE]
  if (!nrow(edges)) return(data.frame())
  a <- edges[, 1L]; b <- edges[, 2L]
  out <- base[as.vector(rbind(a, b, b, a, b, a)), , drop = FALSE]
  out$z <- as.vector(rbind(values$lower[a], values$lower[b], values$upper[b],
    values$lower[a], values$upper[b], values$upper[a]))
  dx <- base$x[b] - base$x[a]; dy <- base$y[b] - base$y[a]; size <- sqrt(dx^2 + dy^2)
  out$nx <- rep(dy / size, each = 6L); out$ny <- rep(-dx / size, each = 6L); out$nz <- 0
  out$se <- rep((values$se[a] + values$se[b]) / 2, each = 6L)
  out$surface <- "side"; out$.opacity <- se.alpha
  faces <- 2L * length(a)
  out$.face <- rep(seq_len(faces), each = 3L)
  out$.corner <- rep.int(1:3, faces)
  out$.vertex <- nrow(base) * c(1L, 1L, 2L, 1L, 2L, 2L) + as.vector(rbind(a, b, b, a, b, a))
  out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
  out
}

StatDensity2D3D <- ggplot2::ggproto("StatDensity2D3D", StatGeometry3D,
  required_aes = c("x", "y"),
  optional_aes = "z",
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 40L) {
    axes <- stats_grid_3d(scales, n)
    xy <- data[c("x", "y")]
    bandwidth <- stats_bandwidth_3d(xy, h, adjust, mass = TRUE)
    out <- expand.grid(axes)
    out$density <- geometry_kde_cpp(as.matrix(xy), axes, bandwidth)
    out$ndensity <- out$density / max(out$density); out$count <- nrow(data) * out$density
    out$z <- scales$z$get_transformation()$transform(out$density)
    stats_triangles_3d(out)
  }
)

StatRidgeline3D <- ggplot2::ggproto("StatRidgeline3D", StatGeometry3D,
  required_aes = c("x", "y"),
  optional_aes = "z",
  compute_group = function(data, scales, along = "x", n = 128L, bw = "nrd0", adjust = 1, base = 0, height = 1) {
    along <- match.arg(along, c("x", "y")); across <- setdiff(c("x", "y"), along)
    location <- unique(data[[across]])
    if (length(location) != 1L) stop("Each ridge group must have one location on the perpendicular axis.", call. = FALSE)
    if (length(base) != 1L || !is.finite(base) || length(height) != 1L || !is.finite(height) || height <= 0)
      stop("base must be finite and height must be positive and finite.", call. = FALSE)
    axis <- stats_grid_3d(scales, n, along)[[1L]]
    den <- stats::density(data[[along]], bw = bw, adjust = adjust, n = length(axis), from = min(axis), to = max(axis))
    # FFT roundoff at vanishing density tails produces numerically collapsed faces.
    den$y[den$y <= 256 * .Machine$double.eps * max(den$y)] <- 0
    grid <- data.frame(x = den$x, y = location, z = base + height * den$y, density = den$y)
    names(grid)[1:2] <- c(along, across)
    bottom <- grid; bottom$z <- base
    vertices <- rbind(grid, bottom); m <- nrow(grid); i <- seq_len(m - 1L)
    tri <- rbind(cbind(i, i + 1L, i + m), cbind(i + 1L, i + m + 1L, i + m))
    tri <- tri[nondegenerate_triangles_3d(vertices, tri), , drop = FALSE]
    out <- vertices[as.vector(t(tri)), , drop = FALSE]
    out$.face <- rep(seq_len(nrow(tri)), each = 3L); out$.corner <- rep.int(1:3, nrow(tri))
    out$.vertex <- as.vector(t(tri)); out$z_mean <- rep(rowMeans(matrix(vertices$z[tri], ncol = 3L)), each = 3L)
    out$density <- rep(rowMeans(matrix(vertices$density[tri], ncol = 3L)), each = 3L)
    out
  }
)
