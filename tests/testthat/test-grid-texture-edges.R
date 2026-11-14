test_that("nearest texture samples agree with texel-clipped volcano geometry", {
  packet <- stats_scene(texture_volcano_plot())$packet
  geometry <- geometric_texture_packet(packet, checker_colours())
  for (dpi in c(110, 300)) {
    viewport <- new_scene_viewport(6.5 * 25.4, 4.5 * 25.4, dpi)
    actual <- texture_channels_3d(raster_scene_3d(packet, new_camera_3d(), viewport, 2L)$image)
    reference <- texture_channels_3d(raster_scene_3d(geometry, new_camera_3d(), viewport, 2L)$image)
    inside <- actual[4L, ] == 255L & reference[4L, ] == 255L
    expect_true(identical(actual[, inside], reference[, inside]), info = paste("dpi", dpi))
  }
})

test_that("checker boundaries stay within half a pixel beside shared triangle edges", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("svglite")
  skip_if_not_installed("pdftools")
  skip_if_not_installed("chromote")
  browser <- viewer_browser_session()
  on.exit(browser$close(), add = TRUE)
  packet <- stats_scene(texture_volcano_plot())$packet
  geometry <- geometric_texture_packet(packet, checker_colours())
  for (dpi in c(110, 300)) {
    viewport <- new_scene_viewport(6.5 * 25.4, 4.5 * 25.4, dpi)
    scene <- scene_compile(packet, new_camera_3d(), viewport)
    segments <- texture_edge_segments(scene, round(c(6.5, 4.5) * dpi))
    sampling <- viewport; sampling$dpi <- texture_image_dpi_3d(viewport)
    geometric <- raster_scene_3d(geometry, new_camera_3d(), sampling, 8L, blend = "srgb")
    for (device in c("ragg", "svglite", "pdf", "cairo")) {
      reference <- texture_device_image(scene, viewport, device, browser, reference = geometric)
      for (lit in c(FALSE, TRUE)) {
        painted <- scene; painted$styles$shade <- lit
        actual <- texture_device_image(painted, viewport, device, browser,
          light = if (lit) light_3d(ambient = 1, diffuse = 0) else NULL)
        error <- texture_edge_displacements(actual, reference, segments)
        expect_gt(length(error), 100L)
        expect_lte(max(abs(error)), .5, label = paste(device, dpi, lit))
      }
    }
  }
})

test_that("the raster and hybrid drape use the same checker sampling and placement", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  plot <- texture_volcano_plot()
  for (dpi in c(110, 300)) {
    paths <- vapply(c("raster", "hybrid"), function(backend) {
      path <- tempfile(fileext = ".png")
      ggplot2::ggsave(path, plot_backend_3d(plot, backend_3d(backend, dpi = dpi, rasterise = "surfaces")),
        device = ragg::agg_png, width = 6.5, height = 4.5, dpi = dpi)
      path
    }, character(1))
    on.exit(unlink(paths), add = TRUE)
    expect_identical(png::readPNG(paths[1L]), png::readPNG(paths[2L]))
    unlink(paths)
  }
})
