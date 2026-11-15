function_rows_plot <- function(data = NULL, mapping = ggplot2::aes(), ...) {
  ggplot2::ggplot(data, mapping) +
    geom_function_3d(fun = function(x, y, t = 0, k = 1) x + k * y + t,
      n = c(4, 3), ...) + coord_3d()
}

test_that("function surfaces evaluate once per row and retain parameters", {
  calls <- numeric()
  p <- ggplot2::ggplot(data.frame(t = c(2, 4), k = c(3, 5))) +
    geom_function_3d(fun = function(x, y, t, k) {
      calls <<- c(calls, t)
      x + k * y + t
    }, n = c(4, 3)) + coord_3d()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_identical(calls, c(2, 4))
  expect_equal(b$z, b$x + b$k * b$y + b$t)
  expect_equal(as.integer(table(b$t)), rep(36L, 2))
  expect_length(unique(b$group), 2)
  expect_false(".function_args" %in% names(b))
  for (data in list(NULL, data.frame(), data.frame(t = numeric()))) {
    b <- ggplot2::ggplot_build(function_rows_plot(data))$data[[1L]]
    expect_equal(nrow(b), 36L)
    expect_equal(b$z, b$x + b$y)
  }
})

test_that("function parameters work with facets, aesthetics, groups and args", {
  d <- data.frame(t = 1:3)
  p <- function_rows_plot(d, ggplot2::aes(colour = factor(t)), args = list(k = 2))
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_equal(b$z, b$x + 2 * b$y + b$t)
  expect_length(unique(b$colour), 3)
  f <- ggplot2::ggplot_build(p + ggplot2::facet_wrap(~t))
  expect_equal(as.integer(table(f$data[[1L]]$PANEL)), rep(36L, 3))
  expect_identical(f$layout$layout$t, 1:3)
  g <- ggplot2::ggplot_build(function_rows_plot(d, ggplot2::aes(group = 0 * t)))$data[[1L]]
  expect_length(unique(g$group), 1)
  expect_equal(nrow(g), 108)
  expect_length(unique(g$.face), 36)
  expect_length(unique(g$.vertex), 36)
  expect_error(ggplot2::ggplot_build(function_rows_plot(d, args = list(t = 0))), "both data and args")
  layer <- geom_function_3d(data = function(d) d[d$t > 1, , drop = FALSE],
    fun = function(x, y, t) x + y + t, n = 3)
  b <- ggplot2::ggplot_build(ggplot2::ggplot(d) + layer + coord_3d())$data[[1L]]
  expect_identical(sort(unique(b$t)), 2:3)
})

test_that("single function grids retain their exact geometry", {
  p <- function_rows_plot(args = list(k = 2))
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  grid <- expand.grid(x = seq(0, 1, length.out = 4), y = seq(0, 1, length.out = 3))
  grid$z <- grid$x + 2 * grid$y
  expected <- stats_triangles_3d(grid)
  attr(expected, "out.attrs") <- NULL
  rownames(expected) <- NULL
  expect_identical(b[names(expected)], expected)
})

test_that("rows containing only position aesthetics each produce a surface", {
  p <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2, z = 1:2), ggplot2::aes(x, y, z = z)) +
    geom_function_3d(fun = function(x, y) x + y, n = 3) + coord_3d()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_length(unique(b$group), 2L)
  expect_equal(nrow(b), 48L)
  expect_equal(b$z, b$x + b$y)
})

test_that("isofunction rows share parameter evaluation through refinement", {
  p <- ggplot2::ggplot(data.frame(r = c(0.4, 0.6))) +
    geom_isofunction_3d(fun = function(x, y, z, r, k) x^2 + y^2 + z^2 - k * r^2,
      args = list(k = 1), levels = 0, n = 7, refine = 4) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
    scale_z_continuous(limits = c(-1, 1)) + coord_3d()
  b <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_equal(sort(unique(b$r)), c(0.4, 0.6))
  expect_length(unique(b$group), 2)
  expect_equal(sqrt(b$x^2 + b$y^2 + b$z^2), b$r, tolerance = 1e-5)
  expect_equal(nrow(ggplot2::ggplot_build(p + ggplot2::facet_wrap(~r))$layout$layout), 2)
})

test_that("function data supply one animation frame per time row", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  movie <- animate_3d(function_rows_plot(data.frame(t = 1:3)), time = "t",
    width = 240, height = 240, progress = FALSE)
  on.exit(unlink(movie$frames))
  expect_identical(movie$frame_count, 3L)
  expect_identical(movie$camera$value, 1:3)
  expect_true(all(file.exists(movie$frames)))
})
