bins_data <- function(plot, i = 1L) suppressWarnings(ggplot2::ggplot_build(plot))$data[[i]]

test_that("rectangular bins count observations against an independent tabulation", {
  set.seed(11)
  d <- data.frame(x = runif(500, 0, 10), y = rnorm(500, 5, 2), w = rexp(500))
  b <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_2d_3d(binwidth = c(1, 0.5)) + coord_3d())
  expect_true(all(abs(b$xmin / 1 - round(b$xmin)) < 1e-9))
  expect_true(all(abs(b$ymin / 0.5 - round(b$ymin / 0.5)) < 1e-9))
  expected <- table(paste(floor(d$x), floor(d$y / 0.5)))
  observed <- stats::setNames(b$count, paste(round(b$xmin), round(b$ymin / 0.5)))
  expect_equal(sort(names(observed)), sort(names(expected)))
  expect_equal(unname(observed[names(expected)]), as.vector(expected))
  expect_equal(b$z, b$count)
  expect_equal(b$width, rep(1, nrow(b))); expect_equal(b$depth, rep(0.5, nrow(b)))
  expect_equal(sum(b$density * b$width * b$depth), 1)
  expect_equal(max(b$ncount), 1); expect_equal(max(b$ndensity), 1)
  expect_equal(b$zmin, rep(0, nrow(b)))

  weighted <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y, weight = w)) + stat_bin_2d_3d(bins = 4) + coord_3d())
  expect_equal(sum(weighted$count), sum(d$w))
  expect_equal(range(c(weighted$xmin, weighted$xmax)), range(d$x))
  full <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_2d_3d(bins = c(4, 3), drop = FALSE) + coord_3d())
  expect_equal(nrow(full), 12L)
  expect_equal(sum(full$count), 500)
  shifted <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_bin_2d_3d(binwidth = 2, boundary = 0.5) + coord_3d())
  expect_true(all(abs((shifted$xmin - 0.5) / 2 - round((shifted$xmin - 0.5) / 2)) < 1e-9))
  given <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_bin_2d_3d(breaks = list(c(0, 5, 10), c(0, 5, 10))) + coord_3d())
  expect_equal(sum(given$count), sum(d$y >= 0 & d$y <= 10))
  hist <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_histogram_3d(binwidth = c(1, 0.5)) + coord_3d())
  expect_equal(hist[c("x", "y", "count", "z")], b[c("x", "y", "count", "z")])
})

test_that("rectangular bins are shared by groups and draw as columns", {
  set.seed(2)
  d <- data.frame(x = runif(300), y = runif(300), g = rep(c("a", "b"), 150))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) +
    stat_bin_2d_3d(bins = 3, position = position_stack_3d()) + coord_3d()
  b <- bins_data(p)
  expect_equal(sum(b$count), 300)
  expect_equal(length(unique(b$xmin)), 3L)
  top <- tapply(b$zmax, paste(b$x, b$y), max)
  expect_equal(unname(top[order(names(top))]),
    unname(tapply(b$count, paste(b$x, b$y), sum)[order(names(top))]))
  s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_histogram_3d(bins = 2) + coord_3d())
  expect_silent(validate_scene_packet(s$packet))
  expect_length(unique(s$packet$faces$source_row), 4L)

  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = x)) + stat_bin_2d_3d() + coord_3d()), "summaries")
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(g, y)) + stat_bin_2d_3d() + coord_3d()), "continuous")
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_2d_3d(bins = 0) + coord_3d()), "bins")
})

test_that("2D summaries apply fun to the z values of each bin", {
  set.seed(4)
  d <- data.frame(x = runif(400), y = runif(400))
  d$z <- d$x + 2 * d$y + rnorm(400, sd = 0.05)
  b <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_summary_2d_3d(breaks = seq(0, 1, by = 0.25), fun = "median") + coord_3d())
  key <- paste(findInterval(d$x, seq(0, 1, 0.25), rightmost.closed = TRUE),
               findInterval(d$y, seq(0, 1, 0.25), rightmost.closed = TRUE))
  expected <- tapply(d$z, key, stats::median)
  observed <- stats::setNames(b$value, paste(round(b$xmax / 0.25), round(b$ymax / 0.25)))
  expect_equal(unname(observed[names(expected)]), as.vector(expected))
  expect_equal(b$z, b$value)
  expect_equal(sum(b$count), 400)
  q <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_summary_2d_3d(bins = 2, fun = stats::quantile, fun.args = list(probs = 0.9, names = FALSE)) + coord_3d())
  expect_equal(nrow(q), 4L)
  na <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_summary_2d_3d(bins = 2, fun = function(v) if (length(v) > 105) mean(v) else NA) + coord_3d())
  expect_false(anyNA(na$value))
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_summary_2d_3d(bins = 2, fun = range) + coord_3d()), "single number")
})

test_that("hexagonal bins assign every observation to the nearest centre", {
  set.seed(7)
  d <- data.frame(x = rnorm(600), y = rnorm(600, sd = 3))
  bw <- c(0.5, 1.2)
  b <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_hex_3d(binwidth = bw) + coord_3d())
  expect_equal(sum(b$count), 600)
  # Brute-force nearest centre among all occupied centres, in bin-width units.
  dist <- outer(d$x, b$x, "-")^2 / bw[1]^2 + outer(d$y, b$y, "-")^2 / bw[2]^2
  nearest <- tabulate(max.col(-dist, ties.method = "first"), nrow(b))
  expect_equal(nearest, b$count)
  rows <- sort(unique(b$y)) / (bw[2] * sqrt(3) / 2)
  expect_equal(rows - rows[1L], round(rows - rows[1L]))
  expect_equal(b$width, rep(bw[1], nrow(b)))
  expect_equal(sum(b$density * bw[1] * bw[2] * sqrt(3) / 2), 1)
  full <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_hex_3d(bins = 5, drop = FALSE) + coord_3d())
  expect_gt(nrow(full), nrow(bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_bin_hex_3d(bins = 5) + coord_3d())))
  expect_equal(sum(full$count), 600)

  d$z <- d$x^2
  s <- bins_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + stat_summary_hex_3d(binwidth = bw, fun = "max") + coord_3d())
  expect_equal(s[c("x", "y")], b[c("x", "y")], ignore_attr = TRUE)
  owner <- max.col(-dist, ties.method = "first")
  expect_equal(s$value, as.vector(tapply(d$z, factor(owner, seq_len(nrow(b))), max)))
})

test_that("hexagonal prisms are extruded hexagons that tile the plane", {
  centres <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, sqrt(3) / 2), z = c(1, 2, 3))
  p <- ggplot2::ggplot(centres, ggplot2::aes(x, y, z = z)) + geom_hex_3d(stat = "identity") + coord_3d()
  b <- bins_data(p)
  expect_equal(b$width, rep(1, 3)); expect_equal(b$depth, rep(1, 3))
  expect_equal(b$ymax - b$ymin, rep(2 / sqrt(3), 3))
  s <- stats_scene(p)
  expect_silent(validate_scene_packet(s$packet))
  expect_length(s$packet$faces$primitive_id, 3L * 8L)
  expect_setequal(unique(s$packet$faces$source_row), 1:3)
  sized <- bins_data(ggplot2::ggplot(centres, ggplot2::aes(x, y, z = z)) +
    geom_hex_3d(stat = "identity", width = 0.5, depth = 2) + coord_3d())
  expect_equal(sized$xmax - sized$xmin, rep(0.5, 3))
  hexes <- stats_scene(ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) + geom_hex_3d(bins = 6) + coord_3d())
  expect_silent(validate_scene_packet(hexes$packet))
  expect_error(geom_hex_3d(cap = "sides"))
})
