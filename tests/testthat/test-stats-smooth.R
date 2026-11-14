smooth_scales <- function(x = c(-1, 1), y = c(-1, 1)) {
  list(x = ggplot2::scale_x_continuous(limits = x), y = ggplot2::scale_y_continuous(limits = y),
    z = scale_z_continuous())
}

smooth_edge_counts <- function(b) {
  faces <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  edges <- rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  table(paste(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2])))
}

smooth_data <- function(seed = 4, n = 60) {
  set.seed(seed)
  d <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1))
  d$z <- 1 + d$x - 2 * d$y + 0.5 * d$x * d$y + rnorm(n, sd = 0.1)
  d
}

test_that("method takes fitting functions and their names as ggplot2 does", {
  d <- smooth_data()
  scales <- smooth_scales()
  by_name <- StatSmooth3D$compute_group(d, scales, method = "lm", formula = z ~ x * y, n = 5, se = FALSE)
  by_function <- StatSmooth3D$compute_group(d, scales, method = stats::lm, formula = z ~ x * y, n = 5, se = FALSE)
  expect_equal(by_function, by_name)
  model <- stats::lm(z ~ x * y, data = d)
  expect_equal(by_name$z, unname(stats::predict(model, by_name[c("x", "y")])))
  seen <- NULL
  recorder <- function(formula, data, weights = NULL, ...) {
    seen <<- list(formula = formula, args = list(...), weights = weights)
    stats::lm(formula, data = data)
  }
  d$weight <- seq_len(nrow(d))
  StatSmooth3D$compute_group(d, scales, method = recorder, n = 3, se = FALSE, method.args = list(tag = 1))
  expect_equal(seen$formula, z ~ x + y, ignore_formula_env = TRUE)
  expect_equal(seen$args, list(tag = 1))
  expect_equal(seen$weights, d$weight)
})

test_that("loess and lm bounds match their own predict() intervals", {
  d <- smooth_data()
  scales <- smooth_scales()
  b <- StatSmooth3D$compute_group(d, scales, method = "loess", n = 4, se = TRUE, level = 0.9)
  model <- stats::loess(z ~ x + y, data = d, span = 0.75)
  upper <- b[b$surface == "upper", ]
  p <- stats::predict(model, upper[c("x", "y")], se = TRUE)
  expect_equal(upper$z, unname(p$fit + stats::qt(0.95, p$df) * p$se.fit))
  b <- StatSmooth3D$compute_group(d, scales, method = "lm", n = 4, se = TRUE, level = 0.9)
  lower <- b[b$surface == "lower", ]
  p <- stats::predict(stats::lm(z ~ x + y, d), lower[c("x", "y")], interval = "confidence", level = 0.9)
  expect_equal(lower$z, unname(p[, "lwr"]))
})

test_that("glm bounds are built on the link scale and mapped back", {
  set.seed(9)
  d <- data.frame(x = runif(80, -1, 1), y = runif(80, -1, 1))
  d$z <- stats::rpois(80, exp(1 + d$x - d$y))
  b <- StatSmooth3D$compute_group(d, smooth_scales(), method = "glm", n = 4,
    method.args = list(family = stats::poisson()))
  model <- stats::glm(z ~ x + y, family = stats::poisson(), data = d)
  fit <- b[b$surface == "fit", ]
  expect_equal(fit$z, unname(stats::predict(model, fit[c("x", "y")], type = "response")))
  upper <- b[b$surface == "upper", ]
  link <- stats::predict(model, upper[c("x", "y")], se.fit = TRUE)
  expect_equal(upper$z, unname(exp(link$fit + stats::qnorm(0.975) * link$se.fit)))
  expect_true(all(b$z > 0))
})

test_that("fitted models are evaluated in data units with mapped predictor names", {
  model <- stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
    geom_smooth_3d(method = model, se = FALSE, n = 4) + scale_z_log10() + coord_3d()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  qsec <- mtcars$qsec[which.min(abs(mtcars$qsec - stats::median(mtcars$qsec)))]
  expected <- stats::predict(model, data.frame(wt = b$x, hp = b$y, qsec = qsec))
  expect_equal(b$z, unname(log10(expected)))
  explicit <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
    geom_smooth_3d(method = model, se = FALSE, n = 4, view = c("wt", "hp"), cond = list(qsec = qsec)) +
    scale_z_log10() + coord_3d()
  expect_equal(ggplot2::ggplot_build(explicit)$data[[1L]]$z, b$z)
  fixed <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
    geom_smooth_3d(method = model, se = FALSE, n = 4, cond = list(qsec = 20)) + coord_3d()
  b <- ggplot2::ggplot_build(fixed)$data[[1L]]
  expect_equal(b$z, unname(stats::predict(model, data.frame(wt = b$x, hp = b$y, qsec = 20))))
  logged <- ggplot2::ggplot(mtcars, ggplot2::aes(log(wt), hp, z = mpg)) +
    geom_smooth_3d(method = model, se = FALSE, n = 4) + coord_3d()
  expect_error(ggplot2::ggplot_build(logged), "view")
  factor_model <- stats::lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
  b <- ggplot2::ggplot_build(ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
    geom_smooth_3d(method = factor_model, se = FALSE, n = 3) + coord_3d())$data[[1L]]
  expect_equal(b$z, unname(stats::predict(factor_model, data.frame(wt = b$x, hp = b$y, cyl = 6))))
})

test_that("models without standard errors draw the fit and refuse se fill", {
  centroid <- structure(list(level = 3), class = "ggplot3d_test_model")
  registerS3method("predict", "ggplot3d_test_model", function(object, newdata, ...) rep(object$level, nrow(newdata)))
  d <- smooth_data()
  expect_warning(b <- StatSmooth3D$compute_group(d, smooth_scales(), method = centroid, n = 3, se = TRUE),
    "no standard errors")
  expect_equal(unique(b$surface), "fit")
  expect_equal(unique(b$z), 3)
  expect_error(StatSmooth3D$compute_group(d, smooth_scales(), method = centroid, n = 3, se = "fill"), "se.fit")
  expect_error(StatSmooth3D$compute_group(d, smooth_scales(), method = "lm", n = 3, se = "wide"), "se must")
  expect_error(StatSmooth3D$compute_group(d, smooth_scales(), method = centroid, formula = z ~ x, n = 3), "formula")
})

test_that("hull domains clip the grid exactly to the convex hull of the data", {
  d <- smooth_data(n = 40)
  scales <- smooth_scales()
  b <- StatSmooth3D$compute_group(d, scales, method = "lm", n = 9, se = FALSE, domain = "hull")
  hull <- as.matrix(d[rev(grDevices::chull(d$x, d$y)), c("x", "y")])
  edge <- hull[c(2:nrow(hull), 1L), ] - hull
  side <- outer(b$y, hull[, 2], "-") * rep(edge[, 1], each = nrow(b)) -
    outer(b$x, hull[, 1], "-") * rep(edge[, 2], each = nrow(b))
  expect_gte(min(side), -1e-9)
  xy <- as.matrix(b[c("x", "y")]); i <- seq(1, nrow(xy), 3)
  area <- ((xy[i + 1, 1] - xy[i, 1]) * (xy[i + 2, 2] - xy[i, 2]) - (xy[i + 1, 2] - xy[i, 2]) * (xy[i + 2, 1] - xy[i, 1])) / 2
  expect_true(all(area > 0))
  expect_equal(sum(area), polygon_area_3d(hull), tolerance = 1e-9)
  model <- stats::lm(z ~ x + y, data = d)
  expect_equal(b$z, unname(stats::predict(model, b[c("x", "y")])))
  expect_equal(as.matrix(b[c("nx", "ny", "nz")]),
    matrix(c(-stats::coef(model)[2:3], 1) / sqrt(sum(c(stats::coef(model)[2:3], 1)^2)), nrow(b), 3, byrow = TRUE),
    ignore_attr = TRUE)
  expect_error(StatSmooth3D$compute_group(data.frame(x = c(0, 0.5, 1), y = c(0, 0.5, 1), z = 1:3), scales,
    method = "lm", n = 4, se = FALSE, domain = "hull"), "two-dimensional")
})

test_that("se bands close lower and upper surfaces with walls", {
  d <- smooth_data(n = 40)
  for (domain in c("box", "hull")) {
    b <- StatSmooth3D$compute_group(d, smooth_scales(), method = "lm", n = 6, se = "band", domain = domain)
    expect_setequal(unique(b$surface), c("fit", "lower", "upper", "side"))
    shell <- b[b$surface != "fit", ]
    expect_true(all(smooth_edge_counts(shell) == 2))
    side <- shell[shell$surface == "side", ]
    expect_true(all(side$nz == 0))
    expect_equal(sqrt(side$nx^2 + side$ny^2), rep(1, nrow(side)))
    expect_true(all(diff(b$.face[seq(1, nrow(b), 3)]) == 1))
  }
})

test_that("se fill maps the standard error and residuals reach the surface", {
  d <- smooth_data(n = 40)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_smooth_3d(method = "lm", se = "fill", n = 5) + coord_3d()
  s <- stats_scene(p); b <- s$built$data[[1L]]
  expect_equal(unique(b$surface), "fit")
  expect_gt(length(unique(b$fill)), 4L)
  se <- matrix(b$se, ncol = 3L, byrow = TRUE)
  expect_true(all(se[, 1L] == se[, 2L] & se[, 1L] == se[, 3L]))
  expect_silent(validate_scene_packet(s$packet))
  set <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_smooth_3d(method = "lm", se = "fill", n = 5, fill = "red") + coord_3d()
  expect_equal(unique(ggplot2::ggplot_build(set)$data[[1L]]$fill), "red")

  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_residual_3d(ggplot2::aes(colour = ggplot2::after_stat(factor(sign))), method = "lm") + coord_3d()
  s <- stats_scene(p); b <- s$built$data[[1L]]
  model <- stats::lm(z ~ x + y, data = d)
  expect_equal(b$resid, unname(stats::residuals(model)))
  expect_equal(b$zend, unname(stats::fitted(model)))
  expect_equal(b$sign, sign(b$resid))
  expect_equal(c(b$x, b$y), c(b$xend, b$yend))
  expect_length(s$packet$polylines$primitive_id, nrow(d))
  expect_equal(length(unique(b$colour)), 2L)
  fitted_model <- stats::lm(z ~ x + y, data = d)
  b <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_residual_3d(method = fitted_model) + coord_3d())$data[[1L]]
  expect_equal(b$resid, unname(stats::residuals(fitted_model)))
})

test_that("extended smooth and residual layers render", {
  skip_if_not_installed("ragg")
  d <- smooth_data(n = 30)
  plots <- list(
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_smooth_3d(method = "lm", se = "band", domain = "hull", n = 8),
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_smooth_3d(method = "loess", se = "fill", n = 8),
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_residual_3d(method = "lm") + geom_point_3d())
  for (p in plots) {
    file <- tempfile(fileext = ".png")
    expect_silent(ggplot2::ggsave(file, p + coord_3d(), device = ragg::agg_png, width = 3, height = 3, dpi = 72))
    expect_gt(file.info(file)$size, 1000); unlink(file)
  }
})
