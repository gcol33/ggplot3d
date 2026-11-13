stats_scene <- function(plot) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  grobs <- lapply(seq_along(built$plot@layers), function(i) built$plot@layers[[i]]$draw_geom(built$data[[i]], built$layout)[[1L]])
  packets <- collect_scene_layers(grobs, built$layout$panel_params[[1L]], built$plot@theme)$packets
  list(packet = merge_scene_packets(packets, 1L), built = built)
}

test_that("function grids and new mappings respect transformed positions", {
  p <- ggplot2::ggplot() + geom_function_3d(fun = function(x, y) x + y, n = c(5, 4)) +
    ggplot2::scale_x_log10(limits = c(1, 100)) + ggplot2::ylim(0, 1) + scale_z_log10() + coord_3d()
  scene <- stats_scene(p); b <- scene$built$data[[1L]]
  expect_equal(range(b$x), c(0, 2)); expect_equal(b$z, b$x + b$y)
  expect_length(scene$packet$faces$primitive_id, 24L)
  expect_silent(validate_scene_packet(scene$packet))
  p <- ggplot2::ggplot() + stat_function_3d(ggplot2::aes(z = ggplot2::after_stat(10^z_mean)),
    fun = function(x, y) x + y, n = 4) + scale_z_log10() + coord_3d()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_equal(b$z, b$z_mean)
  expect_error(stats_grid_3d(list(), 1), "at least two")
})

test_that("smooth recovers a log-scale plane and emits confidence surfaces", {
  set.seed(31)
  d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
  d$z <- 10^(2 + 0.4 * d$x - 0.2 * d$y + rnorm(nrow(d), sd = 0.01))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_smooth_3d(method = "lm", n = 7) + scale_z_log10() + coord_3d()
  scene <- stats_scene(p); b <- scene$built$data[[1L]]
  fit <- b[b$surface == "fit", ]
  expect_equal(fit$z, 2 + 0.4 * fit$x - 0.2 * fit$y, tolerance = 0.005)
  expect_setequal(unique(b$surface), c("fit", "lower", "upper"))
  expect_length(scene$packet$faces$primitive_id, 216L)
  expect_setequal(unique(round(scene$packet$faces$opacity, 3)), c(1, 0.2))
  expect_silent(validate_scene_packet(scene$packet))
  for (method in c("loess", if (requireNamespace("mgcv", quietly = TRUE)) "gam")) {
    p$layers[[1L]] <- geom_smooth_3d(method = method, n = 5, se = FALSE)
    expect_gt(nrow(ggplot2::ggplot_build(p)$data[[1L]]), 0)
  }
})

test_that("pointwise lm confidence bounds cover known mean across seeds", {
  scales <- list(x = ggplot2::scale_x_continuous(limits = c(-1, 1)),
    y = ggplot2::scale_y_continuous(limits = c(-1, 1)), z = scale_z_continuous())
  cover <- vapply(1:30, function(seed) {
    set.seed(seed)
    d <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
    d$z <- 2 + d$x - 0.3 * d$y + rnorm(nrow(d), sd = 0.2)
    b <- StatSmooth3D$compute_group(d, scales, method = "lm", n = 3)
    lo <- b[b$surface == "lower" & b$x == 0 & b$y == 0, "z"][1L]
    hi <- b[b$surface == "upper" & b$x == 0 & b$y == 0, "z"][1L]
    lo <= 2 && hi >= 2
  }, logical(1))
  expect_gte(mean(cover), 0.85)
})

test_that("bivariate KDE agrees with product Gaussian and MASS bandwidth units", {
  set.seed(8); d <- data.frame(x = rnorm(30), y = rnorm(30))
  axes <- list(x = seq(-2, 2, length.out = 7), y = seq(-2, 2, length.out = 6))
  actual <- ggplot3d:::geometry_kde_cpp(as.matrix(d), axes, c(0.5, 0.8))
  grid <- expand.grid(axes)
  expected <- vapply(seq_len(nrow(grid)), function(i) mean(stats::dnorm((grid$x[i] - d$x) / 0.5) *
    stats::dnorm((grid$y[i] - d$y) / 0.8)) / (0.5 * 0.8), numeric(1))
  expect_equal(actual, expected, tolerance = 1e-13)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_density_2d_3d(h = c(2, 3.2), n = c(7, 6)) + coord_3d()
  s <- stats_scene(p)
  expect_length(s$packet$faces$primitive_id, 60L)
  expect_true(all(s$built$data[[1L]]$z >= 0))
  expect_equal(stats_bandwidth_3d(d, NULL, 2, TRUE), unname(vapply(d, stats::bw.nrd, numeric(1)) * 2))
  p <- p + scale_z_log10()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_true(all(is.finite(b$z)))
})

test_that("ridgelines retain grouping in both orientations", {
  d <- iris
  for (along in c("x", "y")) {
    mapping <- if (along == "x") ggplot2::aes(Sepal.Length, as.numeric(Species), group = Species) else
      ggplot2::aes(as.numeric(Species), Sepal.Length, group = Species)
    s <- stats_scene(ggplot2::ggplot(d, mapping) + geom_ridgeline_3d(along = along, n = 16) + coord_3d())
    b <- s$built$data[[1L]]
    expect_equal(length(unique(b$group)), 3L)
    expect_gt(length(s$packet$faces$primitive_id), 60L)
    expect_lte(length(s$packet$faces$primitive_id), 90L)
    expect_true(all(b$z >= 0)); expect_silent(validate_scene_packet(s$packet))
  }
})

test_that("all statistical surface layers render", {
  skip_if_not_installed("ragg")
  d <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6))
  d$z <- d$x + d$y + sin(d$x * d$y)
  plots <- list(ggplot2::ggplot() + geom_function_3d(fun = function(x, y) x*y, n = 6),
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_smooth_3d(method = "lm", n = 5),
    ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_density_2d_3d(n = 6),
    ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, as.numeric(Species), group = Species)) + geom_ridgeline_3d(n = 12))
  for (p in plots) {
    file <- tempfile(fileext = ".png")
    expect_silent(ggplot2::ggsave(file, p + coord_3d(backend = backend_3d("raster", dpi = 72)),
      device = ragg::agg_png, width = 3, height = 3, dpi = 72))
    expect_gt(file.info(file)$size, 1000); unlink(file)
  }
})
stats_edge_counts <- function(b) {
  faces <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  edges <- rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  table(paste(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2]), sep = ":"))
}

test_that("tetrahedral extraction is closed and gradient normals recover a sphere", {
  a <- seq(-1.2, 1.2, length.out = 13)
  d <- expand.grid(x = a, y = a, z = a); d$value <- with(d, x^2 + y^2 + z^2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
    geom_isosurface_3d(levels = 0.73) + coord_3d()
  s <- stats_scene(p); b <- s$built$data[[1L]]
  expect_true(all(stats_edge_counts(b) == 2))
  expect_equal(b$x^2 + b$y^2 + b$z^2, rep(0.73, nrow(b)), tolerance = 0.03)
  normal <- as.matrix(b[c("nx", "ny", "nz")]); xyz <- as.matrix(b[c("x", "y", "z")])
  expect_equal(normal, xyz / sqrt(rowSums(xyz^2)), tolerance = 1e-10, ignore_attr = TRUE)
  expect_length(s$packet$faces$primitive_id, nrow(b) / 3L)
  expect_silent(validate_scene_packet(s$packet))
  expect_equal(stats_levels_3d(d$value, NULL), pretty(range(d$value), n = 10)[pretty(range(d$value), n = 10) > min(d$value) & pretty(range(d$value), n = 10) < max(d$value)])
})

test_that("box caps close sublevel regions and missing cells remain masked", {
  d <- expand.grid(x = 0:3, y = 0:3, z = 0:3); d$value <- d$x + d$y + d$z
  b <- stats_field_3d(d, 3.25, TRUE)
  expect_true(all(stats_edge_counts(b) == 2))
  expect_true(any(b$cap == 1L))
  cap <- b[b$cap == 1L, ]
  expect_true(all(rowSums(abs(as.matrix(cap[c("nx", "ny", "nz")]))) == 1))
  d$value[22L] <- NA_real_
  masked <- stats_field_3d(d, 3.25, FALSE)
  expect_lt(nrow(masked), nrow(stats_field_3d(transform(d, value = x + y + z), 3.25, FALSE)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) + geom_isosurface_3d(levels = 3.25) + coord_3d()
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(stats_field_3d(d[-1L, ], 1, FALSE), "complete")
})

test_that("hulls contain points, orient outwards and support degenerate inputs", {
  set.seed(20); d <- as.data.frame(matrix(rnorm(600), ncol = 3)); names(d) <- c("x", "y", "z")
  t <- ggplot3d:::geometry_hull_cpp(as.matrix(d)); p <- as.matrix(d)
  signed <- vapply(seq_len(nrow(t)), function(i) {
    a <- p[t[i, 1], ]; ab <- p[t[i, 2], ] - a; ac <- p[t[i, 3], ] - a
    normal <- ab[c(2, 3, 1)] * ac[c(3, 1, 2)] - ab[c(3, 1, 2)] * ac[c(2, 3, 1)]
    max(sweep(p, 2, a) %*% normal)
  }, numeric(1))
  expect_lte(max(signed), 1e-10)
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  s <- stats_scene(ggplot2::ggplot(rbind(cube, cube), ggplot2::aes(x, y, z = z)) + geom_hull_3d() + coord_3d())
  expect_length(s$packet$faces$primitive_id, 12L)
  expect_true(all(stats_edge_counts(s$built$data[[1L]]) == 2))
  flat <- as.matrix(expand.grid(x = 0:3, y = 0:3, z = 0))
  expect_equal(nrow(ggplot3d:::geometry_hull_cpp(rbind(flat, flat))), 2L)
  expect_equal(nrow(ggplot3d:::geometry_hull_cpp(cbind(1:6, 1:6, 1:6))), 0L)
  tilted <- cbind(flat[, 1:2], 2 * flat[, 1] + 3 * flat[, 2] + 1e6)
  expect_equal(nrow(ggplot3d:::geometry_hull_cpp(tilted)), 2L)
  set.seed(108)
  sphere <- matrix(rnorm(6000), ncol = 3)
  sphere <- sphere / sqrt(rowSums(sphere^2))
  faces <- ggplot3d:::geometry_hull_cpp(sphere)
  expect_equal(nrow(faces), 3996L)
  expect_equal(length(unique(as.vector(faces))), 2000L)
})

test_that("probability-content levels recover spherical Gaussian mass", {
  a <- seq(-4, 4, length.out = 45)
  d <- expand.grid(x = a, y = a, z = a)
  density <- with(d, stats::dnorm(x) * stats::dnorm(y) * stats::dnorm(z))
  levels <- stats_probability_levels_3d(density, list(a, a, a), probs(0.5, 0.9))
  expected <- exp(-stats::qchisq(c(0.5, 0.9), df = 3) / 2) / (2*pi)^1.5
  expect_lt(max(abs(levels - expected)), 0.002)
  expect_error(probs(1), "strictly")
  set.seed(12); data <- data.frame(x = rnorm(50), y = rnorm(50), z = rnorm(50))
  s <- stats_scene(ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) +
    geom_isodensity_3d(h = 0.6, n = 10, levels = probs(0.5)) +
    ggplot2::xlim(-5, 5) + ggplot2::ylim(-5, 5) + scale_z_continuous(limits = c(-5, 5)) + coord_3d())
  expect_true(all(s$built$data[[1L]]$probability == 0.5))
  expect_gt(length(s$packet$faces$primitive_id), 0L)
})

test_that("native volume KDE agrees with a Gaussian product reference", {
  d <- matrix(c(0, 1, -1, 0.2, -0.5, 0.6), ncol = 3)
  axes <- rep(list(seq(-2, 2, length.out = 4)), 3)
  g <- expand.grid(axes)
  actual <- ggplot3d:::geometry_kde_cpp(d, axes, c(0.5, 0.7, 0.8))
  expected <- vapply(seq_len(nrow(g)), function(i) mean(apply(d, 1, function(p)
    prod(stats::dnorm((as.double(g[i, ]) - p) / c(0.5, 0.7, 0.8)) / c(0.5, 0.7, 0.8)))), numeric(1))
  expect_equal(actual, expected, tolerance = 1e-13)
})

test_that("volume geometry and hulls use transformed z without a second transform", {
  a <- seq(0, 2, length.out = 5)
  d <- expand.grid(x = a, y = a, z = 10^a)
  d$value <- with(d, x + y + log10(z))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
    geom_isosurface_3d(levels = 2.25) + scale_z_log10() + coord_3d()
  b <- stats_scene(p)$built$data[[1L]]
  expect_equal(b$x + b$y + b$z, rep(2.25, nrow(b)), tolerance = 1e-12)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_hull_3d() + scale_z_log10() + coord_3d()
  s <- stats_scene(p)
  expect_equal(range(s$built$data[[1L]]$z), c(0, 2))
  expect_length(s$packet$faces$primitive_id, 12L)
})

test_that("skinny tetrahedral triangles have stable face-normal anchors", {
  a <- seq(-1.4, 1.4, length.out = 26)
  d <- expand.grid(x = a, y = a, z = a)
  d$value <- with(d, (sqrt(x*x + y*y) - 0.85)^2 + z*z)
  b <- stats_field_3d(d, 0.13, FALSE)
  expect_true(all(stats_edge_counts(b) == 2))
  xyz <- as.matrix(b[c("x", "y", "z")]); i <- seq(1, nrow(b), 3)
  ab <- xyz[i + 1, ] - xyz[i, ]; ac <- xyz[i + 2, ] - xyz[i, ]
  normal <- ab[, c(2, 3, 1)] * ac[, c(3, 1, 2)] - ab[, c(3, 1, 2)] * ac[, c(2, 3, 1)]
  normal <- normal / sqrt(rowSums(normal^2))
  expect_lt(max(abs(rowSums(normal * ab))), 1e-14)
  expect_lt(max(abs(rowSums(normal * ac))), 1e-14)
})

test_that("numerically coincident level vertices preserve closed mesh topology", {
  a <- seq(-pi, pi, length.out = 13)
  d <- expand.grid(x = a, y = a, z = a)
  d$value <- with(d, cos(x) * sin(y) + cos(y) * sin(z) + cos(z) * sin(x))
  b <- stats_field_3d(d, 0, TRUE)
  expect_true(all(stats_edge_counts(b) == 2))
  tiny <- stats_field_3d(transform(d, value = value * 1e-20), 0, TRUE)
  tiny <- tiny[order(tiny$.face, tiny$.vertex), ]; b <- b[order(b$.face, b$.vertex), ]
  expect_equal(as.matrix(tiny[c("x", "y", "z", ".vertex")]),
    as.matrix(b[c("x", "y", "z", ".vertex")]), tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("volume layers render through the public face path", {
  skip_if_not_installed("ragg")
  d <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6), z = seq(-1, 1, length.out = 6))
  d$value <- with(d, x*x + y*y + z*z)
  plots <- list(
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) + geom_isosurface_3d(levels = 0.6),
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_hull_3d(),
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_isodensity_3d(h = 0.3, n = 6, levels = 0.07),
    ggplot2::ggplot() + geom_isofunction_3d(fun = function(x, y, z) x + y + z, n = 6, levels = 1.5))
  for (p in plots) {
    file <- tempfile(fileext = ".png")
    expect_silent(ggplot2::ggsave(file, p + coord_3d(backend = backend_3d("raster", dpi = 72)),
      device = ragg::agg_png, width = 3, height = 3, dpi = 72))
    expect_gt(file.info(file)$size, 1000); unlink(file)
  }
})

