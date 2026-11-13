built_layer <- function(plot, i = 1L) ggplot2::ggplot_build(plot)$data[[i]]

position_plot <- function(data, layer, mapping = ggplot2::aes(x, y, z = z)) {
  ggplot2::ggplot(data, mapping) + layer + coord_3d(expand = FALSE)
}

test_that("fill rescales each stack to the unit interval, signs apart", {
  d <- data.frame(x = c(1, 1, 1, 2, 2), y = 1, z = c(1, 3, -2, 2, 2), part = c("a", "b", "c", "a", "b"))
  b <- built_layer(position_plot(d, geom_col_3d(position = position_fill_3d()),
                                 ggplot2::aes(x, y, z = z, fill = part)))
  b <- b[order(b$x, b$group), ]
  expect_equal(b$zmin, c(0.75, 0, 0, 0.5, 0))
  expect_equal(b$zmax, c(1, 0.75, -1, 1, 0.5))
  expect_equal(b$z, b$zmax)
})

test_that("vjust places the stacked anchor inside each interval and labels follow columns", {
  d <- data.frame(x = 1, y = 1, z = c(2, 4), part = c("a", "b"))
  p <- position_plot(d, list(geom_col_3d(position = position_stack_3d()),
                             ggplot2::geom_text(ggplot2::aes(label = part), position = position_stack_3d(vjust = 0.5))),
                     ggplot2::aes(x, y, z = z, fill = part, group = part))
  built <- ggplot2::ggplot_build(p)
  cols <- built$data[[1L]]; text <- built$data[[2L]]
  expect_equal(text$z[order(text$group)], ((cols$zmin + cols$zmax) / 2)[order(cols$group)])
  b <- built_layer(position_plot(d, geom_col_3d(position = position_stack_3d(reverse = TRUE)),
                                 ggplot2::aes(x, y, z = z, fill = part)))
  expect_equal(b$zmin[order(b$group)], c(0, 2))
})

test_that("columns stack along x from a zero baseline and need one", {
  d <- data.frame(x = c(2, 3), y = 1, z = 1, part = c("a", "b"))
  p <- position_plot(d, geom_col_3d(position = position_stack_3d(axis = "x")),
                     ggplot2::aes(x, y, z = z, xmin = 0, xmax = x, fill = part))
  b <- built_layer(p)
  expect_equal(b$xmin[order(b$group)], c(3, 0)); expect_equal(b$xmax[order(b$group)], c(5, 3))
  expect_equal(b$ymin, c(0.55, 0.55))
  expect_error(built_layer(position_plot(d, geom_col_3d(position = position_stack_3d(axis = "y")),
                                         ggplot2::aes(x, y, z = z, fill = part))), "zero baseline")
  expect_error(suppressWarnings(built_layer(p + ggplot2::scale_x_log10())), "linear continuous x")
})

test_that("walls stack along z vertex by vertex and bands stack along y", {
  w <- expand.grid(x = 1:3, y = 1, part = c("a", "b"))
  w$h <- c(1, 2, 3, 10, 20, 30)
  b <- built_layer(position_plot(w, geom_band_3d(position = position_stack_3d()),
                                 ggplot2::aes(x, y, zmin = 0, zmax = h, group = part, fill = part)))
  b <- b[order(b$group, b$x), ]
  expect_equal(b$z, c(10, 20, 30, 0, 0, 0)); expect_equal(b$zend, c(11, 22, 33, 10, 20, 30))
  s <- stats_scene(position_plot(w, geom_band_3d(position = position_stack_3d()),
                                 ggplot2::aes(x, y, zmin = 0, zmax = h, group = part, fill = part)))$packet
  expect_gt(length(s$faces$primitive_id), 0L)
  flat <- data.frame(x = c(0, 1, 0, 1), y = 0, yend = c(1, 1, 2, 2), z = 1, part = rep(c("a", "b"), each = 2L))
  b <- built_layer(position_plot(flat, geom_band_3d(zend = 1, position = position_fill_3d(axis = "y")),
                                 ggplot2::aes(x, y, z = z, xend = x, yend = yend, group = part)))
  expect_equal(b$y, c(2 / 3, 2 / 3, 0, 0)); expect_equal(b$yend, c(1, 1, 2 / 3, 2 / 3))
})

test_that("dodge splits columns within each x/y cell along either axis", {
  d <- expand.grid(x = 1:2, y = 1:2, part = c("a", "b")); d$z <- 1
  b <- built_layer(position_plot(d, geom_col_3d(width = 0.8, position = position_dodge_3d()),
                                 ggplot2::aes(x, y, z = z, fill = part)))
  a <- b[b$fill == b$fill[1L], ]
  expect_equal(b$xmax - b$xmin, rep(0.4, 8L))
  expect_equal(sort(unique(round(b$x - round(b$x), 10))), c(-0.2, 0.2))
  expect_equal(b$ymax - b$ymin, rep(0.9, 8L))
  b <- built_layer(position_plot(d, geom_col_3d(width = 0.8, depth = 0.6, position = position_dodge_3d(axis = "y")),
                                 ggplot2::aes(x, y, z = z, fill = part)))
  expect_equal(b$ymax - b$ymin, rep(0.3, 8L)); expect_equal(b$xmax - b$xmin, rep(0.8, 8L))
  expect_equal(sort(unique(round(b$y - round(b$y), 10))), c(-0.15, 0.15))
})

test_that("dodge preserve single keeps widths and centres partial cells", {
  d <- data.frame(x = c(1, 1, 1, 2), y = 1, z = 1, part = c("a", "b", "c", "a"))
  total <- built_layer(position_plot(d, geom_col_3d(width = 0.9, position = position_dodge_3d()),
                                     ggplot2::aes(x, y, z = z, fill = part)))
  expect_equal(total$xmax[total$x > 1.5] - total$xmin[total$x > 1.5], 0.9)
  single <- built_layer(position_plot(d, geom_col_3d(width = 0.9, position = position_dodge_3d(preserve = "single")),
                                      ggplot2::aes(x, y, z = z, fill = part)))
  expect_equal(single$xmax - single$xmin, rep(0.3, 4L))
  expect_equal(single$x[single$xmin > 1.5], 2)
  rev <- built_layer(position_plot(d, geom_col_3d(width = 0.9, position = position_dodge_3d(reverse = TRUE)),
                                   ggplot2::aes(x, y, z = z, fill = part)))
  expect_equal(rev$x[rev$x < 1.5], c(1.3, 1, 0.7))
})

test_that("dodge moves points and segment endpoints together", {
  d <- data.frame(x = 1, y = 1, z = 1:2, g = c("a", "b"))
  b <- built_layer(position_plot(d, geom_point_3d(position = position_dodge_3d(width = 0.5)),
                                 ggplot2::aes(x, y, z = z, colour = g)))
  expect_equal(b$x, c(0.875, 1.125)); expect_equal(b$y, c(1, 1))
  b <- built_layer(position_plot(d, geom_segment_3d(zend = 0, position = position_dodge_3d(axis = "y", width = 0.5)),
                                 ggplot2::aes(x, y, z = z, colour = g, xend = x, yend = y)))
  expect_equal(b$yend, b$y); expect_equal(b$y, c(0.875, 1.125))
})

test_that("jitter moves each family together, honours amounts and is reproducible", {
  d <- data.frame(x = rep(1:3, 20), y = rep(1:2, 30), z = 1)
  p <- position_plot(d, geom_segment_3d(zend = 0, position = position_jitter_3d(0.1, 0.2, 0.3, seed = 7)),
                     ggplot2::aes(x, y, z = z, xend = x, yend = y))
  b <- built_layer(p)
  expect_equal(b$xend, b$x); expect_equal(b$yend, b$y); expect_equal(b$z - b$zend, rep(1, 60L))
  expect_true(all(abs(b$x - d$x) <= 0.1)); expect_true(all(abs(b$y - d$y) <= 0.2))
  expect_true(all(abs(b$z - 1) <= 0.3)); expect_gt(stats::sd(b$z), 0)
  expect_equal(built_layer(p)$x, b$x)
  set.seed(1); before <- stats::runif(1)
  set.seed(1); built_layer(p); expect_equal(stats::runif(1), before)
  defaults <- built_layer(position_plot(d, geom_point_3d(position = position_jitter_3d(seed = 1))))
  expect_true(all(abs(defaults$x - d$x) <= 0.4)); expect_true(all(abs(defaults$z - 1) <= 0.4 + 1e-12))
  none <- built_layer(position_plot(d, geom_point_3d(position = position_jitter_3d(0, 0, 0))))
  expect_equal(none$x, d$x)
  expect_error(position_jitter_3d(width = -1), "non-negative")
})

test_that("each emitter declares the adjustments it accepts", {
  grid <- expand.grid(x = 0:1, y = 0:1); grid$z <- 1
  expect_error(built_layer(position_plot(grid, geom_surface_3d(position = position_jitter_3d()))),
               "Unsupported 3D position for GeomSurface3D")
  expect_error(built_layer(position_plot(grid, geom_surface_3d(position = position_stack_3d()))), "Unsupported")
  expect_error(built_layer(position_plot(grid, geom_col_3d(position = ggplot2::position_dodge()))), "Unsupported")
  b <- built_layer(position_plot(grid, geom_point_3d(position = ggplot2::position_nudge(x = 1))))
  expect_equal(b$x, grid$x + 1)
  expect_error(position_dodge_3d(axis = "z"), "axis")
  expect_error(position_stack_3d(axis = "w"), "axis")
  expect_error(register_geom_3d(ggplot2::GeomPoint, emit_points_3d, positions = "spin", replace = TRUE))
})

test_that("stacked and dodged columns render", {
  d <- expand.grid(x = 1:2, y = 1:2, part = c("a", "b")); d$z <- 1:8
  for (position in list(position_stack_3d(), position_dodge_3d(), position_fill_3d())) {
    s <- stats_scene(position_plot(d, geom_col_3d(position = position),
                                   ggplot2::aes(x, y, z = z, fill = part)))$packet
    expect_length(s$faces$primitive_id, 6L * nrow(d))
  }
})
