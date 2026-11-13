slice_scene <- function(plot) stats_scene(plot)

test_that("facet_slices_3d computes statistics per slice and draws one panel", {
  set.seed(3)
  d <- data.frame(g = rep(c("b", "a", "c"), each = 60), x = rnorm(180, rep(c(0, 2, 4), each = 60)))
  plot <- ggplot2::ggplot(d, ggplot2::aes(x)) + ggplot2::geom_density() + facet_slices_3d(ggplot2::vars(g)) + coord_3d()
  built <- ggplot2::ggplot_build(plot)
  expect_equal(nrow(built$layout$layout), 3L)
  expect_equal(built$layout$layout$.slice_label_3d, c("a", "b", "c"))
  densities <- built$data[[2L]]
  expect_true(all(as.integer(densities$PANEL) == 1L))
  expect_equal(sort(unique(densities$z)), 1:3)
  expect_equal(length(unique(densities$group)), 3L)
  peaks <- vapply(split(densities, densities$z), function(s) s$x[which.max(s$y)], numeric(1))
  expect_true(all(abs(peaks - c(2, 0, 4)) < 0.8))
  expect_equal(built$layout$panel_params[[1L]]$axis_titles$z, "g")
  expect_equal(nrow(built$data[[1L]]), 12L)
  expect_equal(nrow(built$data[[3L]]), 3L)
  expect_equal(built$data[[3L]]$label, c("a", "b", "c"))
})

test_that("a numeric slicing variable places slices at its values", {
  d <- data.frame(year = rep(c(2000, 2010, 2025), each = 4), x = rep(1:4, 3), y = 1:12)
  built <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_line() +
    facet_slices_3d(~year, planes = FALSE, labels = FALSE) + coord_3d())
  expect_length(built$data, 1L)
  expect_equal(sort(unique(built$data[[1L]]$z)), c(2000, 2010, 2025))
  expect_equal(length(unique(built$data[[1L]]$group)), 3L)
  scene <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_line() +
    facet_slices_3d("year", planes = FALSE, labels = FALSE) + coord_3d())
  expect_equal(length(scene$packet$polylines$offset), 3L)
})

test_that("layers without the slicing variables repeat in every slice", {
  d <- data.frame(g = rep(c("a", "b"), each = 3), x = 1:6, y = 1:6)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 3) + facet_slices_3d(~g, labels = FALSE) + coord_3d()
  built <- ggplot2::ggplot_build(plot)
  expect_equal(nrow(built$data[[3L]]), 2L)
  expect_equal(sort(built$data[[3L]]$z), 1:2)
  scene <- slice_scene(plot)
  expect_equal(length(scene$packet$markers$offset), 6L)
})

test_that("slices along y draw layers written in x and z", {
  d <- data.frame(g = rep(1:3, each = 10), x = rep(1:10, 3), v = rnorm(30))
  built <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, z = v)) + ggplot2::geom_line() +
    facet_slices_3d(~g, along = "y") + coord_3d())
  expect_equal(sort(unique(built$data[[2L]]$y)), c(1, 2, 3))
  planes <- built$data[[1L]]
  expect_true(all(is.infinite(planes$x)) && all(is.infinite(planes$z)))
  expect_equal(built$layout$panel_params[[1L]]$axis_titles$y, "g")
})

test_that("facet_slices_3d checks its use", {
  d <- data.frame(g = c("a", "b"), x = 1:2, y = 1:2, z = 1:2)
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + ggplot2::geom_point() +
    facet_slices_3d(~g) + coord_3d()), "places the slices on z")
  expect_error(print(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() + facet_slices_3d(~g)), "coord_3d")
  expect_error(facet_slices_3d(1), "facets must be")
  expect_error(facet_slices_3d(~g, along = "w"))
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    facet_slices_3d(~h) + coord_3d()), "No layer has the slicing variables")
})

test_that("an upright camera builds without a recursive frame", {
  d <- data.frame(g = rep(c("a", "b"), each = 5), x = 1:10, y = 1:10)
  built <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    facet_slices_3d(~g) + coord_3d(up = "y"))
  expect_equal(built$layout$panel_params[[1L]]$frame$up, 2L)
})
