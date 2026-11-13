reference_base_3d <- function() {
  d <- data.frame(x = c(0, 4), y = c(0, 2), z = c(0, 10))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
}

polyline_ends_3d <- function(packet) {
  start <- packet$corners$vertex[packet$polylines$offset]
  end <- packet$corners$vertex[packet$polylines$offset + packet$polylines$count - 1L]
  v <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)
  list(start = v[start, , drop = FALSE], end = v[end, , drop = FALSE])
}

test_that("the box clip keeps the part of a line inside the unit cube", {
  point <- rbind(c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5), c(2, 0.5, 0.5), c(0, 0, 0.3))
  direction <- rbind(c(1, 0, 0), c(1, 1, 1), c(0, 1, 0), c(1, 1, 0))
  clipped <- clip_lines_to_box_3d(point, direction)
  expect_equal(clipped$inside, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(clipped$start[1, ], c(0, 0.5, 0.5))
  expect_equal(clipped$end[1, ], c(1, 0.5, 0.5))
  expect_equal(clipped$start[2, ], c(0, 0, 0))
  expect_equal(clipped$end[2, ], c(1, 1, 1))
  expect_equal(clipped$start[4, ], c(0, 0, 0.3))
  expect_equal(clipped$end[4, ], c(1, 1, 0.3))
})

test_that("geom_refline_3d draws a line along the axis it leaves free", {
  out <- integration_packets(reference_base_3d() +
    geom_refline_3d(xintercept = 2, yintercept = c(0.5, 1.5)) + coord_3d())
  ranges <- out$params$ranges
  ends <- polyline_ends_3d(out$packet)
  vertical <- abs(ends$end[, 3L] - ends$start[, 3L]) > 0.5
  expect_equal(sum(vertical), 2L)
  expect_equal(sort(c(ends$start[vertical, 3L], ends$end[vertical, 3L])), c(0, 0, 1, 1))
  expect_equal(unique(ends$start[vertical, 1L]), (2 - ranges$x[1L]) / diff(ranges$x))
  expect_equal(sort(ends$start[vertical, 2L]), (c(0.5, 1.5) - ranges$y[1L]) / diff(ranges$y))
  expect_length(out$packet$polylines$count, 2L)
})

test_that("mapped intercepts draw one line per row and train the scales", {
  lines <- data.frame(yi = c(-1, 3), zi = c(5, 20))
  out <- integration_packets(reference_base_3d() +
    geom_refline_3d(ggplot2::aes(yintercept = yi, zintercept = zi), data = lines) + coord_3d())
  expect_true(out$params$ranges$z[2L] >= 20)
  expect_true(out$params$ranges$y[1L] <= -1)
  ends <- polyline_ends_3d(out$packet)
  along_x <- abs(ends$end[, 1L] - ends$start[, 1L]) > 0.5
  expect_equal(sum(along_x), 2L)
  expect_error(geom_refline_3d(xintercept = 1), "exactly two")
  expect_error(geom_refline_3d(xintercept = 1, yintercept = 1, zintercept = 1), "exactly two")
  expect_error(geom_refline_3d(xintercept = 1:2, yintercept = 1:3), "Unequal lengths")
})

test_that("geom_hline and geom_vline share the axis line emitter", {
  out <- integration_packets(reference_base_3d() + ggplot2::geom_hline(yintercept = 1, colour = "red") + coord_3d())
  red <- out$packet$polylines$style_id[out$packet$styles$colour[out$packet$polylines$style_id] == "#FF0000"]
  expect_length(red, 1L)
})

test_that("geom_abline_3d cuts lines through a point to the box and ignores them for ranges", {
  base <- reference_base_3d() + coord_3d()
  plain <- integration_packets(base)$params$ranges
  out <- integration_packets(reference_base_3d() +
    geom_abline_3d(point = c(2, 1, 5), direction = rbind(c(1, 0, 0), c(4, 2, 10))) + coord_3d())
  expect_equal(out$params$ranges, plain)
  ranges <- out$params$ranges
  ends <- polyline_ends_3d(out$packet)
  centre <- c((2 - ranges$x[1L]) / diff(ranges$x), (1 - ranges$y[1L]) / diff(ranges$y), (5 - ranges$z[1L]) / diff(ranges$z))
  expect_length(out$packet$polylines$count, 2L)
  for (i in 1:2) {
    expect_true(all(ends$start[i, ] >= -1e-9 & ends$start[i, ] <= 1 + 1e-9))
    expect_true(any(abs(ends$start[i, ] - 0) < 1e-9 | abs(ends$start[i, ] - 1) < 1e-9))
    mid <- (ends$start[i, ] + ends$end[i, ]) / 2
    step <- ends$end[i, ] - ends$start[i, ]
    offset <- centre - ends$start[i, ]
    expect_lt(sqrt(sum((offset - sum(offset * step) / sum(step^2) * step)^2)), 1e-9)
  }
  diagonal <- ends$end[2L, ] - ends$start[2L, ]
  data_step <- diagonal * c(diff(ranges$x), diff(ranges$y), diff(ranges$z))
  expect_equal(data_step / sqrt(sum(data_step^2)), c(4, 2, 10) / sqrt(120))
})

test_that("mapped directions share one point, and lines missing the box are skipped", {
  dirs <- data.frame(a = c(1, 0), b = c(0, 1), c = c(0, 0))
  out <- integration_packets(reference_base_3d() +
    geom_abline_3d(ggplot2::aes(dx = a, dy = b, dz = c), data = dirs, point = c(2, 1, 5)) + coord_3d())
  expect_length(out$packet$polylines$count, 2L)
  expect_warning(integration_packets(reference_base_3d() +
    geom_abline_3d(point = c(100, 1, 5), direction = c(0, 1, 0)) + coord_3d()), "outside the panel box")
  expect_error(geom_abline_3d(direction = c(0, 0, 0)), "nonzero")
  expect_error(geom_abline_3d(point = rbind(c(0, 0, 0), c(1, 1, 1)), direction = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))), "common number")
  expect_error(geom_abline_3d(point = rbind(c(0, 0, 0), c(1, 1, 1))), "share one point")
  expect_error(geom_abline_3d(point = c(1, 2)), "three-column")
})

test_that("reference lines bend with a curvilinear frame", {
  out <- integration_packets(reference_base_3d() +
    geom_refline_3d(yintercept = 1, zintercept = 5) + coord_cylindrical_3d())
  expect_gt(max(out$packet$polylines$count), 2L)
})

test_that("brackets draw tips back toward both ends and a label past the middle", {
  pair <- data.frame(x = 1, xend = 3, y = 1, yend = 1, z = 6, label = "p < 0.05")
  out <- integration_packets(reference_base_3d() +
    geom_bracket_3d(ggplot2::aes(xend = xend, yend = yend, label = label), data = pair, offset = c(0, 0, 0.1)) +
    coord_3d())
  packet <- out$packet
  ranges <- out$params$ranges
  expect_equal(packet$polylines$count, 4L)
  path <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)[packet$corners$vertex[packet$polylines$offset + 0:3], ]
  z <- (6 - ranges$z[1L]) / diff(ranges$z)
  expect_equal(path[, 3L], z + c(0.08, 0.1, 0.1, 0.08))
  expect_equal(path[c(1, 4), 1L], (c(1, 3) - ranges$x[1L]) / diff(ranges$x))
  expect_length(packet$text$primitive_id, 1L)
  text_vertex <- packet$corners$vertex[packet$text$offset]
  expect_equal(packet$vertices$z[text_vertex], z + 0.1 + 0.02 + 0.03)
})

test_that("dimension lines carry arrowheads and plain extension lines", {
  span <- data.frame(x = 0, y = 0, z = 0, xend = 4, yend = 0, zend = 0)
  packet <- integration_packets(ggplot2::ggplot(span, ggplot2::aes(x, y, z = z, xend = xend, yend = yend, zend = zend)) +
    geom_bracket_3d(type = "dimension", offset = c(0, 0.2, 0)) + coord_3d())$packet
  expect_equal(packet$polylines$count, c(2L, 2L, 2L))
  expect_equal(packet$polylines$arrow_start, c(FALSE, FALSE, TRUE))
  expect_equal(packet$polylines$arrow_end, c(FALSE, FALSE, TRUE))
  expect_length(packet$text$primitive_id, 0L)
  expect_error(geom_bracket_3d(offset = c(0, 0, 0)), "not all zero")
  expect_error(geom_bracket_3d(tip_length = -1), "nonnegative")
})
