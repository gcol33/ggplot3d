distribution_data <- function() {
  data.frame(x = factor(rep(c("a", "b"), each = 9)), y = 0,
    z = c(1:8, 30, 2:10))
}

test_that("named packet collections retain the same indexed scene", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 2, z = 3), ggplot2::aes(x, y, z = z)) +
    geom_point_3d() + coord_3d()
  packet <- stats_scene(p)$packet
  expect_identical(merge_scene_packets(list(a = packet, b = packet), 1L),
    merge_scene_packets(list(packet, packet), 1L))
})

test_that("box summaries agree with ggplot2's statistic on z and preserve outliers", {
  data <- distribution_data()
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_boxplot_3d() + coord_3d()
  scene <- stats_scene(p)
  built <- scene$built$data[[1L]]
  summaries <- built[built$.kind == "box", ]
  for (i in seq_len(2L)) {
    reference <- ggplot2::StatBoxplot$compute_group(data.frame(x = i, y = data$z[data$x == levels(data$x)[i]]), NULL)
    expect_equal(unlist(summaries[i, c("zintercept", "zmin", "z", "zmax", "zend")], use.names = FALSE),
      unlist(reference[c("ymin", "lower", "middle", "upper", "ymax")], use.names = FALSE))
  }
  expect_equal(built$z[built$.kind == "outlier"], 30)
  expect_equal(length(scene$packet$markers$offset), 1L)
  expect_equal(length(scene$packet$faces$offset), 14L)
  expect_equal(length(scene$packet$polylines$offset), 40L)
  expect_gte(scene$built$layout$panel_params[[1]]$ranges$z[2L], 30)
  quiet <- stats_scene(p + geom_boxplot_3d(outliers = FALSE))
  expect_equal(length(quiet$packet$markers$offset), 1L)
  expect_error(geom_boxplot_3d(coef = -1), "nonnegative")
})

test_that("distribution cells have explicit groups and work on transformed scales", {
  p <- ggplot2::ggplot(distribution_data(), ggplot2::aes(x, y, z = z)) + geom_boxplot_3d() +
    scale_z_log10() + coord_3d()
  out <- stats_scene(p)
  values <- out$built$data[[1L]]
  expect_equal(values$z[values$.kind == "box"], c(stats::median(log10(c(1:8, 30))), stats::median(log10(2:10))))
  expect_true(all(is.finite(out$packet$vertices$z)))
  expect_error(distribution_cell_3d(data.frame(x = 1:2, y = 0)), "one x/y cell")
  for (position in list(position_nudge_3d(z = 1), position_dodge_3d())) {
    expect_no_error(stats_scene(ggplot2::ggplot(distribution_data(), ggplot2::aes(x, y, z = z)) +
      geom_boxplot_3d(position = position) + coord_3d()))
  }
})

test_that("violin walls and revolved solids retain density and cell widths", {
  d <- distribution_data()
  for (shape in c("wall", "revolve")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
      geom_violin_3d(shape = shape, width = 0.8, depth = 0.6, n = 12, sides = 8) + coord_3d()
    out <- stats_scene(p)
    expect_gt(length(out$packet$faces$offset), 0L)
    expect_true(all(is.finite(out$packet$vertices$x)))
    values <- out$built$data[[1L]]
    expect_equal(values$xmax - values$xmin, rep(0.8, nrow(values)))
    expect_equal(as.numeric(tapply(values$scaled, values$group, max)), c(1, 1))
    expect_true(all(values$density >= 0))
    if (shape == "revolve") expect_equal(length(out$packet$faces$offset), 2L * (11L * 8L + 2L))
  }
  expect_error(geom_violin_3d(n = 2), "at least three")
})

test_that("areas stack signed walls per lattice vertex and waterfall retains rows", {
  d <- expand.grid(x = 0:3, y = 0, part = c("a", "b"))
  d$z <- rep(c(1, 2, -1, -2), 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = part, fill = part))
  out <- stats_scene(p + geom_area_3d() + coord_3d())
  values <- out$built$data[[1L]]
  expect_equal(values$z[values$group == 2], rep(0, 4))
  expect_equal(values$zend[values$group == 2], c(1, 2, -1, -2))
  expect_equal(values$z[values$group == 1], c(1, 2, -1, -2))
  expect_equal(values$zend[values$group == 1], c(2, 4, -2, -4))
  expect_equal(length(out$packet$faces$offset), 12L)
  wall <- stats_scene(p + geom_waterfall_3d(base = -3) + coord_3d())
  expect_equal(wall$built$data[[1L]]$z, rep(-3, 8))
  expect_equal(wall$built$data[[1L]]$zend, d$z)
  expect_no_error(stats_scene(p + geom_area_3d(position = position_fill_3d()) + coord_3d()))
  d$y <- rep(1:2, each = 4)
  expect_no_error(stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = y)) +
    geom_waterfall_3d() + coord_3d()))
})
