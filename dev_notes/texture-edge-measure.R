devtools::load_all()
source("tests/testthat/helper-stats-scene.R")
source("tests/testthat/helper-textures.R")
source("tests/testthat/helper-texture-edges.R")
source("tests/testthat/helper-viewer.R")
out <- Sys.getenv("TEXTURE_EDGE_OUT", "dev_notes/texture-edge-current")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
browser <- viewer_browser_session()
packet <- stats_scene(texture_volcano_plot())$packet
geometry <- geometric_texture_packet(packet, checker_colours())
camera <- new_camera_3d()
metrics <- list()
for (dpi in as.numeric(strsplit(Sys.getenv("TEXTURE_EDGE_DPI", "110,300"), ",")[[1L]])) {
  viewport <- new_scene_viewport(6.5 * 25.4, 4.5 * 25.4, dpi)
  pixels <- round(c(6.5, 4.5) * dpi)
  scene <- scene_compile(packet, camera, viewport)
  segments <- texture_edge_segments(scene, pixels)
  native_reference <- texture_gray_image(raster_scene_3d(geometry, camera, viewport, 8L, blend = "srgb")$image)
  sampling <- viewport; sampling$dpi <- texture_image_dpi_3d(viewport)
  geometric <- raster_scene_3d(geometry, camera, sampling, 8L, blend = "srgb")
  cat("REFERENCE", dpi, dim(native_reference), pixels, "\n")
  for (device in c("raster", "ragg", "svglite", "pdf", "cairo")) for (lit in c(FALSE, TRUE)) {
    reference <- if (device == "raster") native_reference else texture_device_image(scene, viewport, device, browser, reference = geometric)
    image <- if (device == "raster") {
      painted <- packet; painted$styles$shade <- lit
      texture_gray_image(raster_scene_3d(painted, camera, viewport, 2L,
        light = if (lit) light_3d(ambient = 1, diffuse = 0) else NULL, blend = "srgb")$image)
    } else {
      test_scene <- scene
      test_scene$styles$shade <- lit
      texture_device_image(test_scene, viewport, device, browser,
        if (lit) light_3d(ambient = 1, diffuse = 0) else NULL)
    }
    error <- texture_edge_displacements(image, reference, segments)
    saveRDS(list(error = error, image = image, reference = reference, segments = segments),
      file.path(out, paste0(device, "-", lit, "-", dpi, ".rds")))
    row <- data.frame(dpi, device, lit, samples = length(error), max = max(abs(error)),
      p99 = unname(quantile(abs(error), .99)), missing = sum(!is.finite(error)))
    print(row)
    metrics[[length(metrics) + 1L]] <- row
  }
}
browser$close()
write.csv(do.call(rbind, metrics), file.path(out, "edge-metrics.csv"), row.names = FALSE)
source("tests/fixtures/oracle/reference.R")
source("tests/fixtures/scenes/builders.R")
fixtures <- list()
for (name in names(ref_fixture_builders())) for (view in names(ref_cameras())) {
  f <- ref_fixture(name, view)
  scene <- scene_compile(f$packet, f$camera, f$viewport)
  fixtures[[paste(name, view)]] <- list(fragments = scene$fragments, vertices = scene$vertices,
    order = scene$draw_order, image = raster_scene_3d(f$packet, f$camera, f$viewport, 1L)$image)
}
saveRDS(fixtures, file.path(out, "untextured.rds"))
