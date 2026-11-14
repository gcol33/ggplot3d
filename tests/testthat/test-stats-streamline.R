flow_grid <- function(rotation = FALSE) {
  d <- expand.grid(x = c(-2, 0, 2), y = c(-2, 0, 2), z = c(-1, 1))
  d$u <- if (rotation) -d$y else 1
  d$v <- if (rotation) d$x else 0
  d$w <- 0
  d
}

flow_integrate <- function(data, method = "rk4", seeds = matrix(c(.5, 0, 0), 1L),
                           direction = "forward", steps = 20L, slice = -1L, spacing = 0) {
  field <- streamline_field_3d(data)
  ggplot3d:::streamlines_cpp(field$axes, field$values, seeds, .1, steps, method, direction,
    1e-9, spacing, spacing / 2, slice, 30L)
}

test_that("both integrators recover constant and rotating vector fields", {
  for (method in c("rk4", "rk45")) {
    straight <- flow_integrate(flow_grid(), method, direction = "both", steps = 4L)
    expect_equal(nrow(straight), 9L)
    expect_equal(straight$x, .5 + straight$time, tolerance = 1e-12)
    expect_equal(straight$time, seq(-.4, .4, by = .1), tolerance = 1e-12)
    expect_equal(straight$arclength, seq(0, .8, by = .1), tolerance = 1e-12)
    expect_equal(straight$norm, rep(1, 9))
    circle <- flow_integrate(flow_grid(TRUE), method)
    expect_equal(circle$x, .5 * cos(circle$time), tolerance = if (method == "rk4") 2e-6 else 1e-7)
    expect_equal(circle$y, .5 * sin(circle$time), tolerance = if (method == "rk4") 2e-6 else 1e-7)
    expect_equal(circle$z, rep(0, nrow(circle)))
    expect_equal(sqrt(circle$x^2 + circle$y^2), rep(.5, nrow(circle)), tolerance = 1e-6)
    d <- flow_grid(); d$u <- d$x; d$v <- 2 * d$y; d$w <- -d$z
    diagonal <- flow_integrate(d, method, seeds = matrix(c(.5, .2, .4), 1L), steps = 5L)
    expect_equal(diagonal$x, .5 * exp(diagonal$time), tolerance = 1e-5)
    expect_equal(diagonal$y, .2 * exp(2 * diagonal$time), tolerance = 2e-5)
    expect_equal(diagonal$z, .4 * exp(-diagonal$time), tolerance = 1e-6)
  }
})

test_that("field assembly joins coordinates and terminates at boundaries and missing cells", {
  d <- flow_grid(TRUE)
  expect_identical(streamline_field_3d(d), streamline_field_3d(d[nrow(d):1L, ]))
  expect_error(streamline_field_3d(d[-1L, ]), "exactly once")
  duplicate <- d;duplicate[1L, c("x", "y", "z")] <- duplicate[2L, c("x", "y", "z")]
  expect_error(streamline_field_3d(duplicate), "exactly once")
  d <- flow_grid();d$u <- 0
  expect_equal(nrow(flow_integrate(d)), 0L)
  d$u <- NA_real_
  expect_equal(nrow(flow_integrate(d)), 0L)
  out <- flow_integrate(flow_grid(), steps = 100L)
  expect_lte(max(out$x), 2)
  expect_gt(max(out$x), 1.99)
})

test_that("point plane and line seeds build paths and reuse tube sweeps", {
  d <- flow_grid(TRUE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w))
  seeds <- list(rbind(c(.5, 0, 0)), list(from = c(.3, 0, 0), to = c(.8, 0, 0), n = 3),
    list(plane = "xy", at = 0, n = 3))
  for (seed in seeds) {
    out <- stats_scene(p + stat_streamline_3d(seeds = seed, max_steps = 8,
      ggplot2::aes(colour = ggplot2::after_stat(norm))) + coord_3d())
    expect_gt(length(out$packet$polylines$offset), 0L)
    expect_true(all(c("norm", "time", "arclength", "step") %in% names(out$built$data[[1L]])))
  }
  tube <- stats_scene(p + stat_streamline_3d(seeds = seeds[[1L]], geom = "tube", radius = .04,
    sides = 6, max_steps = 6, cap = "flat") + coord_3d())
  expect_gt(length(tube$packet$quadrics$kind), 0L)
  expect_true(all(is.finite(unlist(tube$packet$quadrics[ggplot3d:::quadric_transform_columns]))))
})

test_that("Jobard-Lefer slice placement respects spacing and projected fields", {
  d <- flow_grid();d$w <- 1
  out <- flow_integrate(d, seeds = matrix(c(0, 0, 0), 1L), direction = "both", steps = 200L, slice = 2L, spacing = .5)
  expect_gte(length(unique(out$group)), 5L)
  expect_equal(out$z, rep(0, nrow(out)))
  centres <- vapply(split(out$y, out$group), function(x) x[1L], numeric(1))
  expect_true(all(diff(sort(centres)) >= .5 - 1e-8))
  for (rows in split(seq_len(nrow(out)), out$group)) {
    expect_true(all(diff(out$arclength[rows]) <= .125 + 1e-8))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w)) +
    stat_streamline_3d(seeds = list(type = "even", plane = "xy", at = c(-.5, .5), spacing = 1), max_steps = 20) + coord_3d()
  values <- stats_scene(p)$built$data[[1L]]
  expect_setequal(unique(values$z), c(-.5, .5))
  expect_error(stat_streamline_3d(step = 0), "positive")
})
