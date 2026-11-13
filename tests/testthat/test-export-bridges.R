bridge_plot <- function() {
  export_surface_plot(fill = "#336699", colour = NA) +
    geom_path_3d(data = data.frame(x = c(-1, 0, 1), y = c(-1, 1, -1), z = 0), colour = "red", linewidth = 1) +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0.3), size = 3) +
    geom_text_3d(data = data.frame(x = 0, y = 0, z = 0.4, label = "peak"), ggplot2::aes(label = label))
}

test_that("as.mesh3d() returns one rgl mesh with vertex colours", {
  skip_if_not_installed("rgl")
  p <- bridge_plot()
  mesh <- rgl::as.mesh3d(p)
  expect_s3_class(mesh, "mesh3d")
  expect_identical(mesh$meshColor, "vertices")
  expect_identical(length(mesh$material$color), ncol(mesh$vb))
  expect_true("#336699" %in% mesh$material$color)
  expect_null(mesh$is)
  lines <- rgl::as.mesh3d(p, lines = "line", points = "point")
  expect_identical(dim(lines$is), c(2L, 2L))
  expect_identical(length(lines$ip), 1L)
  expect_identical(ncol(lines$it), 50L)
  surface <- rgl::as.mesh3d(export_surface_plot(fill = "grey50", colour = NA, coord = coord_3d(aspect = c(1, 1, 2))), size = 4)
  expect_equal(range(surface$vb[3, ] / surface$vb[4, ]), c(0, 4), tolerance = 0.1)
})

test_that("plot3d() draws triangles, segments, points and text with the plot's view", {
  skip_if_not_installed("rgl")
  old <- options(rgl.useNULL = TRUE)
  on.exit(options(old), add = TRUE)
  p <- export_surface_plot(fill = "#336699", colour = NA, coord = coord_3d(azimuth = 30, elevation = 20)) +
    geom_path_3d(data = data.frame(x = c(-1, 1), y = 0, z = 0), colour = "red", linewidth = 1) +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0.3), size = 3) +
    geom_text_3d(data = data.frame(x = 0, y = 0, z = 0.4, label = "peak"), ggplot2::aes(label = label))
  ids <- rgl::plot3d(p)
  on.exit(rgl::close3d(), add = TRUE)
  expect_setequal(rgl::ids3d()$type, c("triangles", "lines", "points", "text"))
  expect_identical(sort(ids), sort(rgl::ids3d()$id))
  view <- rgl::par3d("userMatrix")
  scene <- export_scene_3d(p, 1L, export_options_3d(FALSE, "line", "point", 1), 640L, 640L, 96)
  expect_equal(view[3, 1:3], -scene$camera$forward, tolerance = 1e-6)
  expect_identical(rgl::par3d("FOV"), 0)
})

test_that("as_rayrender_3d() builds one object per layer with triangles", {
  skip_if_not_installed("rayrender")
  scene <- as_rayrender_3d(bridge_plot())
  expect_s3_class(scene, "ray_scene")
  expect_identical(nrow(scene), 3L)
  expect_identical(unique(scene$shape), "mesh3d")
  expect_error(as_rayrender_3d(bridge_plot(), render = NA), "render must be TRUE or FALSE")
})
