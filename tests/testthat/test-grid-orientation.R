orientation_plot <- function(...) {
  ggplot2::ggplot(data.frame(x = c(1, 1000), y = c(0, 1000), z = c(0, 100)),
    ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d(...)
}

orientation_panels <- function(plot) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = 800, height = 600, res = 100)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(plot)))
}

test_that("orientation guides follow each panel camera with physical size and corner", {
  for (projection in c("orthographic", "perspective", "oblique")) {
    p <- orientation_plot(axes = character(), projection = projection, roll = 15, up = "y") +
      ggplot2::guides(orientation = guide_triad_3d(position = "topright", size = grid::unit(20, "mm")))
    panel <- orientation_panels(p)[[1L]]
    triad <- panel$furniture$children[[1L]]
    basis <- frame_basis_3d(panel$panel_params$camera, panel$inner_viewport)
    expect_equal(triad$directions, basis$basis)
    expect_equal(triad$size_mm, 20)
    expect_equal(as.numeric(triad$vp$x), panel$outer_viewport$width_mm - 22)
    expect_equal(as.numeric(triad$vp$y), panel$outer_viewport$height_mm - 22)
    expect_length(triad$children, 6L)
  }
  p <- orientation_plot(axes = character()) + facet_views(c("top", "front")) +
    ggplot2::guides(orientation = guide_triad_3d())
  panels <- orientation_panels(p)
  expect_length(panels, 2L)
  expect_false(isTRUE(all.equal(panels[[1L]]$furniture$children[[1L]]$directions,
    panels[[2L]]$furniture$children[[1L]]$directions)))
})

test_that("north uses a model direction and works alongside the triad", {
  p <- orientation_plot(axes = character(), roll = 30) +
    ggplot2::guides(orientation = guide_triad_3d()) + annotation_north_3d(north = c(1, 1, 0))
  panel <- orientation_panels(p)[[1L]]
  expect_length(panel$furniture$children, 2L)
  expect_equal(anyDuplicated(names(panel$furniture$children)), 0L)
  basis <- frame_basis_3d(panel$panel_params$camera, panel$inner_viewport)
  north <- Filter(function(g) nrow(g$directions) == 1L, panel$furniture$children)[[1L]]
  expect_equal(unname(north$directions), matrix(c(1, 1, 0) / sqrt(2), 1L) %*% basis$basis)
  inside <- orientation_plot(axes = character(), projection = "perspective", distance = 0.3, frame = "sphere") +
    ggplot2::guides(orientation = guide_triad_3d())
  expect_length(orientation_panels(inside)[[1L]]$furniture$children, 1L)
  expect_error(annotation_north_3d(north = c(0, 0, 0)), "nonzero")
  expect_error(guide_triad_3d(labels = "x"), "labels")
  expect_error(guide_triad_3d(size = grid::unit(1, "npc")), "physical")
  expect_error(guide_triad_3d(linewidth = 0), "linewidth")
})

test_that("scale bars measure data units and leave scale training unchanged", {
  base <- orientation_plot(projection = "perspective")
  p <- base + annotation_scale_3d(length = 100, unit = "m", at = c(x = 200, y = 200))
  out <- stats_scene(p)
  expected <- ggplot2::ggplot_build(base)$layout$panel_params[[1L]]$ranges
  expect_equal(out$built$layout$panel_params[[1L]]$ranges, expected)
  packet <- out$packet
  rows <- which(packet$polylines$layer_id == 2L)
  expect_length(rows, 3L)
  ids <- packet$corners$vertex[packet$polylines$offset[rows[1L]] + 0:1]
  raw <- expected$x[1L] + packet$vertices$x[ids] * diff(expected$x)
  expect_equal(raw, c(200, 300))
  expect_equal(packet$vertices$z[ids], c(0, 0))
  expect_true(all(packet$polylines$partition[rows] == 1L))
  expect_true("100 m" %in% packet$styles$label)
  log <- stats_scene(base + ggplot2::scale_x_log10() +
    annotation_scale_3d(length = 100, at = c(x = 100), plane = list(axis = "z", at = 25)))
  packet <- log$packet
  i <- which(packet$polylines$layer_id == 2L)[1L]
  ids <- packet$corners$vertex[packet$polylines$offset[i] + 0:1]
  range <- log$built$layout$panel_params[[1L]]$ranges$x
  expect_equal(10^(range[1L] + packet$vertices$x[ids] * diff(range)), c(100, 200))
  expect_error(stats_scene(base + annotation_scale_3d(axis = "z", plane = "floor")), "containing")
  expect_error(stats_scene(base + annotation_scale_3d(length = 10000)), "fit inside")
  expect_error(annotation_scale_3d(length = -1), "positive")
  expect_error(annotation_scale_3d(at = 1:3), "named")
})

test_that("orientation guides and plane scale bars draw in each backend", {
  for (type in c("vector", "raster", "hybrid")) {
    expect_no_error(orientation_panels(orientation_plot(backend = backend_3d(type), axes = character()) +
      ggplot2::guides(orientation = guide_triad_3d()) + annotation_scale_3d(length = 100) + annotation_north_3d()))
  }
})
