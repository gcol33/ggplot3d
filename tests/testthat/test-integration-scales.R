integration_plot <- function(z = c(1, 10, 100)) {
  ggplot2::ggplot(data.frame(x = seq_along(z), y = seq_along(z), z = z), ggplot2::aes(x, y, z = z))
}

test_that("z transforms every endpoint once and fixed parameters before stats", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1, z = 10, zend = 100),
                       ggplot2::aes(x, y, z = z, zend = zend)) + geom_segment_3d() + coord_3d() + scale_z_log10()
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$z, 1)
  expect_equal(b$data[[1]]$zend, 2)
  expect_equal(b$data[[1]]$xend, b$data[[1]]$x)
  expect_equal(b$data[[1]]$yend, b$data[[1]]$y)
  p <- integration_plot() + geom_segment_3d(z = 10, zend = 100) + scale_z_log10() + coord_3d()
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$z, rep(1, 3))
  expect_equal(b$data[[1]]$zend, rep(2, 3))
})

test_that("discrete z is mapped through limits and agrees with ticks", {
  p <- integration_plot(factor(c("low", "high"), levels = c("low", "high"))) +
    geom_point_3d() + scale_z_discrete(limits = c("high", "low")) + coord_3d()
  b <- ggplot2::ggplot_build(p)
  expect_equal(as.numeric(b$data[[1]]$z), c(2, 1))
  view <- b$layout$panel_params[[1]]$z
  expect_equal(as.character(view$get_labels()), c("high", "low"))
  expect_equal(as.numeric(view$mapped_breaks()), c(1, 2))
})

test_that("stats receive transformed positions, z scale and pre-stat OOB", {
  probe <- ggplot2::ggproto("StatIntegrationProbe", ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales) {
      data$seen <- data$z
      data$has_z_scale <- !is.null(scales$z)
      data
    })
  p <- integration_plot() + geom_point_3d(stat = probe) + coord_3d() + scale_z_log10(limits = c(1, 10))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$seen, c(0, 1, NA))
  expect_true(all(b$data[[1]]$has_z_scale))
  p <- integration_plot() + geom_point_3d(stat = probe) + coord_3d() +
    scale_z_continuous(limits = c(1, 10), oob = function(x, range, ...) pmin(pmax(x, range[1]), range[2]))
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$seen, c(1, 10, 10))
  p <- integration_plot() + geom_point_3d(stat = probe) + coord_3d() + zlim(1, 10)
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$seen, c(1, 10, NA))
  p <- integration_plot() + geom_point_3d(stat = probe) + coord_3d(zlim = c(1, 10))
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$seen, c(1, 10, 100))
})

test_that("facets key independent z scales and duplicate missing facet variables", {
  data <- data.frame(x = rep(1:3, 2), y = rep(1:3, 2), z = c(1:3, 101:103), f = rep(c("a", "b"), each = 3))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    ggplot2::facet_wrap(~ f, scales = "free") + coord_3d(z_scales = "free")
  b <- ggplot2::ggplot_build(p)
  expect_length(b$layout$panel_scales_z, 2)
  expect_equal(b$layout$panel_scales_z[[1]]$range$range, c(1, 3))
  expect_equal(b$layout$panel_scales_z[[2]]$range$range, c(101, 103))
  expect_equal(length(unique(b$layout$layout$COORD)), 2)
  p <- p + geom_point_3d(data = data.frame(x = 1, y = 1, z = 50))
  b <- ggplot2::ggplot_build(p)
  expect_equal(sort(unique(as.integer(b$data[[2]]$PANEL))), 1:2)
  expect_equal(b$layout$get_scales(1)$z$range$range, c(1, 50))
  expect_equal(b$layout$get_scales(2)$z$range$range, c(50, 103))
})

test_that("addition order and repeated builds isolate user-owned objects", {
  p <- integration_plot() + geom_point_3d()
  a <- p + coord_3d() + scale_z_log10()
  b <- p + scale_z_log10() + coord_3d()
  original_range <- a@scales$get_scales("z")$range$range
  original_geom <- a@layers[[1]]$geom
  first <- ggplot2::ggplot_build(a)
  expect_equal(first$data, ggplot2::ggplot_build(b)$data)
  expect_equal(first$data, ggplot2::ggplot_build(a)$data)
  expect_equal(first$layout$panel_params[[1]]$ranges, ggplot2::ggplot_build(a)$layout$panel_params[[1]]$ranges)
  expect_identical(a@scales$get_scales("z")$range$range, original_range)
  expect_identical(a@layers[[1]]$geom, original_geom)
  expect_null(a@coordinates$panel_params)
  expect_equal(ggplot2::ggplot_build(a + geom_point_3d())$data[[2]]$z, c(0, 1, 2))
  ordinary <- integration_plot() + ggplot2::geom_point() + coord_3d()
  expect_equal(ggplot2::ggplot_build(ordinary)$data[[1]]$z, c(1, 10, 100))
  expect_s3_class(suppressMessages(ggplot2::ggplot_build(ordinary + ggplot2::coord_cartesian()))$layout$coord, "CoordCartesian")
})

test_that("date, datetime, after-stat and infinite boundaries retain scale semantics", {
  dates <- as.Date(c("2020-01-01", "2020-01-11"))
  b <- ggplot2::ggplot_build(integration_plot(dates) + geom_point_3d() + coord_3d())
  expect_s3_class(b$layout$get_scales(1)$z, "ScaleContinuousDate")
  expect_equal(b$data[[1]]$z, as.numeric(dates))
  times <- as.POSIXct(dates, tz = "UTC")
  b <- ggplot2::ggplot_build(integration_plot(times) + geom_point_3d() + coord_3d())
  expect_s3_class(b$layout$get_scales(1)$z, "ScaleContinuousDatetime")
  stat <- ggplot2::ggproto("StatIntegrationComputed", ggplot2::Stat,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(z = ggplot2::after_stat(value)),
    compute_group = function(data, scales) {data$value <- c(10, 100, 1000); data})
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + geom_point_3d(stat = stat) + scale_z_log10() + coord_3d()
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$z, c(1, 2, 3))
  p <- integration_plot(c(-Inf, 1, 100, Inf)) + geom_point_3d() + scale_z_reverse() + coord_3d()
  b <- ggplot2::ggplot_build(p)
  normal <- normalise_positions_3d(b$data[[1]], b$layout$panel_params[[1]])
  expect_equal(normal$z[c(1, 4)], c(0, 1))
  expect_equal(b$layout$get_scales(1)$z$range$range, c(-100, -1))
})

test_that("facet shrink, fixed z and margin panels follow keyed ranges", {
  data <- data.frame(x = rep(1:3, 2), y = 1, z = c(1, 2, 9, 101, 102, 109), f = rep(c("a", "b"), each = 3))
  mean_stat <- ggplot2::ggproto("StatIntegrationMean", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    compute_group = function(data, scales) {data[1, ]$z <- mean(data$z); data[1, ]})
  base <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d(stat = mean_stat) + coord_3d(z_scales = "free")
  shrink <- ggplot2::ggplot_build(base + ggplot2::facet_wrap(~ f, shrink = TRUE))
  full <- ggplot2::ggplot_build(base + ggplot2::facet_wrap(~ f, shrink = FALSE))
  expect_equal(shrink$layout$get_scales(1)$z$range$range, c(4, 4))
  expect_equal(full$layout$get_scales(1)$z$range$range, c(1, 9))
  fixed <- ggplot2::ggplot_build(ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
                                 ggplot2::facet_grid(~ f, margins = TRUE) + coord_3d())
  expect_length(fixed$layout$panel_scales_z, 1L)
  expect_equal(fixed$layout$get_scales(1)$z$range$range, c(1, 109))
  free <- ggplot2::ggplot_build(ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
                                ggplot2::facet_grid(~ f, margins = TRUE) + coord_3d(z_scales = "free"))
  expect_length(free$layout$panel_scales_z, 3L)
  expect_equal(free$layout$get_scales(3)$z$range$range, c(1, 109))
})

test_that("nested builds keep their own cloned scales", {
  inner <- integration_plot(c(10, 100)) + geom_point_3d() + coord_3d() + scale_z_log10()
  probe <- ggplot2::ggproto("StatIntegrationNested", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    compute_group = function(data, scales) {
      built <- ggplot2::ggplot_build(inner)
      data$inner <- built$data[[1]]$z[1L]
      data$outer_range <- diff(scales$z$range$range)
      data
    })
  outer <- integration_plot(c(1, 1000)) + geom_point_3d(stat = probe) + coord_3d() + scale_z_log10()
  data <- ggplot2::ggplot_build(outer)$data[[1]]
  expect_equal(data$inner, c(1, 1))
  expect_equal(data$outer_range, c(3, 3))
  expect_equal(data$z, c(0, 3))
})

test_that("all z-family bounds infer one scale and retain ordinary transforms", {
  data <- data.frame(x = 1:2, y = 1:2, z = c(10, 100), zmin = c(1, 10), zmax = c(100, 1000), zintercept = c(10, 100))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z, zmin = zmin, zmax = zmax, zintercept = zintercept)) +
    geom_point_3d() + coord_3d(zlim = c(10, 100)) + scale_z_log10()
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$zmin, c(0, 1))
  expect_equal(b$data[[1]]$zmax, c(2, 3))
  expect_equal(b$data[[1]]$zintercept, c(1, 2))
  expect_equal(b$layout$panel_params[[1]]$z.range, c(0.95, 2.05))
  expect_equal(zlim(10, 1)$transform(c(1, 10)), c(-1, -10))
  b <- ggplot2::ggplot_build(integration_plot(c(TRUE, FALSE)) + geom_point_3d() + coord_3d())
  expect_true(b$layout$get_scales(1)$z$is_discrete())
  expect_equal(as.numeric(b$data[[1]]$z), c(2, 1))
})

test_that("inference waits for generated discrete positions and empty axes stay finite", {
  stat <- ggplot2::ggproto("StatIntegrationDiscrete", ggplot2::Stat,
    required_aes = c("x", "y"), default_aes = ggplot2::aes(z = ggplot2::after_stat(level)),
    compute_group = function(data, scales) {data$level <- c("low", "high"); data})
  p <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) + geom_point_3d(stat = stat) + coord_3d()
  b <- ggplot2::ggplot_build(p)
  expect_true(b$layout$get_scales(1)$z$is_discrete())
  expect_equal(as.numeric(b$data[[1]]$z), c(2, 1))
  b <- ggplot2::ggplot_build(integration_plot(c(-Inf, Inf)) + geom_point_3d() + coord_3d(expand = FALSE))
  expect_equal(b$layout$panel_params[[1]]$z.range, c(0, 1))
  expect_true(all(is.finite(normalise_positions_3d(b$data[[1]], b$layout$panel_params[[1]])$z)))
})
