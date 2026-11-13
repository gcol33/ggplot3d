interval_segments <- function(packet) {
  v <- as.data.frame(packet$vertices)
  start <- packet$corners$vertex[packet$polylines$offset]
  end <- packet$corners$vertex[packet$polylines$offset + 1L]
  data.frame(x = v$x[start], y = v$y[start], z = v$z[start],
             xend = v$x[end], yend = v$y[end], zend = v$z[end])
}

segment_axes <- function(s) {
  moving <- cbind(x = s$x != s$xend, y = s$y != s$yend, z = s$z != s$zend)
  apply(moving, 1L, function(m) paste(c("x", "y", "z")[m], collapse = ""))
}

interval_data <- function() {
  d <- expand.grid(x = 1:3, y = 1:2)
  d$z <- c(2, 3, 5, 4, 6, 5)
  d$se <- c(0.4, 0.6, 0.5, 0.8, 0.3, 0.7)
  d
}

test_that("error bars draw a bar and two caps per interval, capped along x by default", {
  d <- interval_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se)) +
    geom_errorbar_3d(width = 0.2) + coord_3d()
  s <- interval_segments(stats_scene(p)$packet)
  expect_equal(nrow(s), 18L)
  expect_equal(as.vector(table(segment_axes(s))[c("x", "z")]), c(12L, 6L))
  caps <- s[segment_axes(s) == "x", ]
  expect_equal(abs(caps$xend - caps$x), rep(abs(caps$xend[1] - caps$x[1]), 12L))
})

test_that("caps train the scale they extend along", {
  d <- data.frame(x = 1:3, y = 1, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - 1, zmax = z + 1))
  wide <- stats_scene(p + geom_errorbar_3d(width = 2) + coord_3d())$built
  narrow <- stats_scene(p + geom_linerange_3d() + coord_3d())$built
  x_wide <- wide$layout$panel_params[[1L]]$ranges$x
  expect_lte(x_wide[1L], 0)
  expect_gte(x_wide[2L], 4)
  expect_lt(diff(narrow$layout$panel_params[[1L]]$ranges$x), diff(x_wide))
  expect_lte(wide$layout$panel_params[[1L]]$ranges$z[1L], 0)
})

test_that("rows use any subset of the three intervals", {
  e <- data.frame(x = c(1, 2), y = c(1, 3), z = c(2, 1),
                  xlo = c(0.7, NA), xhi = c(1.3, NA))
  p <- ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, xmin = xlo, xmax = xhi,
      ymin = y - 0.5, ymax = y + 0.5, zmin = z - 0.4, zmax = z + 0.4)) +
    geom_linerange_3d() + coord_3d()
  s <- interval_segments(stats_scene(p)$packet)
  expect_equal(sort(segment_axes(s)), c("x", "y", "y", "z", "z"))
  cross <- ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, ymin = y - 0.5, ymax = y + 0.5,
      zmin = z - 0.4, zmax = z + 0.4)) + geom_errorbar_3d(width = 0.1, cap.plane = "cross") + coord_3d()
  s <- interval_segments(stats_scene(cross)$packet)
  expect_equal(nrow(s), 2L * (2L + 2L * 2L * 2L))
  expect_true(all(segment_axes(s) %in% c("x", "y", "z")))
})

test_that("cap.plane chooses the axis of the caps", {
  d <- interval_data()
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se))
  caps <- function(plane) {
    s <- interval_segments(stats_scene(base + geom_errorbar_3d(cap.plane = plane) + coord_3d())$packet)
    sort(unique(segment_axes(s)))
  }
  expect_equal(caps("auto"), c("x", "z"))
  expect_equal(caps("yz"), c("y", "z"))
  expect_equal(caps("xy"), c("x", "y", "z"))
  expect_equal(cap_axes_3d("x", "auto"), "y")
  expect_equal(cap_axes_3d("y", "yz"), "z")
  expect_equal(cap_axes_3d("z", "cross"), c("x", "y"))
  expect_error(geom_errorbar_3d(cap.plane = "zz"), "cap.plane")
})

test_that("a missing centre is the middle of its interval and z-less layers lie on the floor", {
  e <- data.frame(x = 1:3, y = c(1, 3, 2))
  p <- ggplot2::ggplot(e, ggplot2::aes(x, y, ymin = y - 1, ymax = y + 1)) +
    geom_pointrange_3d() + coord_3d()
  out <- stats_scene(p)
  expect_true(all(out$packet$vertices$z == 0))
  marker <- out$packet$corners$vertex[out$packet$markers$offset]
  expect_equal(out$packet$vertices$y[marker], (c(1, 3, 2) - out$built$layout$panel_params[[1L]]$ranges$y[1L]) /
    diff(out$built$layout$panel_params[[1L]]$ranges$y))
  z_only <- ggplot2::ggplot(e, ggplot2::aes(x, y, zmin = 1, zmax = 3)) + geom_linerange_3d() + coord_3d()
  built <- stats_scene(z_only)$built
  expect_equal(built$data[[1L]]$z, rep(2, 3))
  expect_error(stats_scene(ggplot2::ggplot(e, ggplot2::aes(x, y)) + geom_linerange_3d() + coord_3d()),
    "need xmin/xmax")
})

test_that("point ranges draw a marker at the centre of every row", {
  d <- interval_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se))
  packet <- stats_scene(p + geom_pointrange_3d(size = 3) + coord_3d())$packet
  expect_length(packet$markers$offset, 6L)
  expect_length(packet$polylines$offset, 6L)
  expect_equal(unique(packet$styles$size[packet$markers$style_id]), 3)
  native <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, ymin = y - se, ymax = y + se)) +
    ggplot2::geom_pointrange(size = 0.5) + coord_3d())$packet
  expect_equal(unique(native$styles$size[native$markers$style_id]), 2)
})

test_that("crossbars draw a rectangle, or a cuboid with outward faces, and a middle line", {
  d <- interval_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se))
  flat <- stats_scene(p + geom_crossbar_3d(fill = "grey80", width = 0.4) + coord_3d())$packet
  expect_equal(flat$faces$count, rep(4L, 6L))
  expect_length(flat$polylines$offset, 6L * 4L + 6L)
  solid <- stats_scene(p + geom_crossbar_3d(fill = "grey80", width = 0.4, cap.plane = "cross",
    middle.colour = "red", fatten = 3) + coord_3d())$packet
  expect_equal(solid$faces$count, rep(4L, 36L))
  expect_length(solid$polylines$offset, 6L * 12L + 6L * 4L)
  middle <- solid$styles[solid$polylines$style_id, ]
  expect_equal(sum(middle$colour == "#FF0000"), 24L)
  expect_true(all(middle$linewidth[middle$colour == "#FF0000"] == 1.5))
  empty <- stats_scene(p + geom_crossbar_3d() + coord_3d())$packet
  expect_length(empty$faces$offset, 0L)
})

test_that("cuboid faces point away from their box", {
  rows <- data.frame(x = 0.5, y = 0.5, z = 0.5, zmin = 0.2, zmax = 0.8,
                     xend = 0.4, xintercept = 0.6, yend = 0.3, yintercept = 0.7)
  template <- cube_template_3d(3L)
  vertices <- do.call(rbind, lapply(seq_len(8L), function(v) cell_vertices_3d(rows, "z", c("x", "y"), template$bits[v, ])))
  faces <- orient_faces_3d(vertices, template$faces[, 4:1], rep(1L, 8L))
  p <- as.matrix(vertices[c("x", "y", "z")])
  d1 <- p[faces[, 3], ] - p[faces[, 1], ]; d2 <- p[faces[, 4], ] - p[faces[, 2], ]
  normal <- cbind(d1[, 2] * d2[, 3] - d1[, 3] * d2[, 2], d1[, 3] * d2[, 1] - d1[, 1] * d2[, 3],
                  d1[, 1] * d2[, 2] - d1[, 2] * d2[, 1])
  centre <- (p[faces[, 1], ] + p[faces[, 3], ]) / 2
  expect_true(all(rowSums(normal * sweep(centre, 2, colMeans(p))) > 0))
  expect_equal(range(p[, "x"]), c(0.4, 0.6))
  expect_equal(range(p[, "y"]), c(0.3, 0.7))
})

test_that("ggplot2 interval layers draw inside coord_3d()", {
  e <- data.frame(x = 1:3, y = c(1, 3, 2), lo = c(0, 2, 1), hi = c(2, 4, 3))
  base <- ggplot2::ggplot(e, ggplot2::aes(x, y, ymin = lo, ymax = hi))
  bars <- interval_segments(stats_scene(base + ggplot2::geom_errorbar(width = 0.2) + coord_3d())$packet)
  expect_equal(nrow(bars), 9L)
  expect_true(all(c(bars$z, bars$zend) == 0))
  expect_length(stats_scene(base + ggplot2::geom_linerange() + coord_3d())$packet$polylines$offset, 3L)
  cross <- stats_scene(base + ggplot2::geom_crossbar(fill = "white", width = 0.3) + coord_3d())$packet
  expect_equal(cross$faces$count, rep(4L, 3L))
  expect_length(stats_scene(base + ggplot2::geom_pointrange() + coord_3d())$packet$markers$offset, 3L)
})

test_that("point ranges dodge along x and y", {
  g <- expand.grid(x = 1:2, y = 1, g = c("a", "b"))
  g$z <- 1:4
  p <- ggplot2::ggplot(g, ggplot2::aes(x, y, z = z, zmin = z - 0.5, zmax = z + 0.5, colour = g)) +
    geom_pointrange_3d(position = position_dodge_3d(width = 0.4)) + coord_3d()
  built <- stats_scene(p)$built
  expect_equal(sort(built$data[[1L]]$x), c(0.9, 1.1, 1.9, 2.1))
  expect_error(stats_scene(ggplot2::ggplot(g, ggplot2::aes(x, y, z = z, zmin = z - 1, zmax = z + 1)) +
    geom_errorbar_3d(position = position_stack_3d()) + coord_3d()), "Unsupported 3D position")
})

test_that("stems run from the base value to each point along the chosen axis", {
  d <- data.frame(x = 1:3, y = 1, z = c(-1, 2, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_stem_3d() + coord_3d()
  out <- stats_scene(p)
  s <- interval_segments(out$packet)
  expect_equal(segment_axes(s), rep("z", 3L))
  range_z <- out$built$layout$panel_params[[1L]]$ranges$z
  expect_equal(unique(s$zend), (0 - range_z[1L]) / diff(range_z))
  expect_length(out$packet$markers$offset, 3L)
  sideways <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_stem_3d(axis = "x", base = 2, shape = NA) + coord_3d())
  s <- interval_segments(sideways$packet)
  expect_equal(segment_axes(s)[d$x != 2], c("x", "x"))
  expect_length(sideways$packet$markers$offset, 0L)
  expect_error(geom_stem_3d(axis = "w"), "axis")
})

test_that("stat_summary_3d() summarises one axis per position on the other two", {
  set.seed(3)
  d <- expand.grid(x = 1:2, y = 1:2, rep = 1:8)
  d$z <- d$x + 2 * d$y + stats::rnorm(nrow(d))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
  expect_message(built <- stats_scene(p + stat_summary_3d() + coord_3d())$built, "mean_se")
  out <- built$data[[1L]]
  expect_equal(nrow(out), 4L)
  cell <- d[d$x == 2 & d$y == 1, "z"]
  row <- out[out$x == 2 & out$y == 1, ]
  expect_equal(row$z, mean(cell))
  expect_equal(row$zmax - row$z, stats::sd(cell) / sqrt(length(cell)))
  quartiles <- stats_scene(p + stat_summary_3d(geom = "errorbar", fun = stats::median,
    fun.min = function(z) stats::quantile(z, 0.25), fun.max = "max") + coord_3d())$built$data[[1L]]
  row <- quartiles[quartiles$x == 2 & quartiles$y == 1, ]
  expect_equal(c(row$zmin, row$z, row$zmax), unname(c(stats::quantile(cell, 0.25), stats::median(cell), max(cell))))
  sideways <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(z, y, z = x)) +
    stat_summary_3d(axis = "x", fun.data = ggplot2::mean_se, fun.args = list(mult = 2)) + coord_3d())$built$data[[1L]]
  expect_true(all(c("xmin", "xmax") %in% names(sideways)))
  expect_equal(nrow(sideways), 4L)
  expect_error(stat_summary_3d(geom = "violin"), "geom must be")
})
