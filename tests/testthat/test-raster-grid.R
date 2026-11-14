test_that("raster panels preserve vector furniture and bypass data ordering", {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = 500, height = 500, res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  compile <- ggplot3d:::scene_compile
  testthat::local_mocked_bindings(scene_compile = function(packet, ...) {
    expect_true(all(packet$faces$layer_id == 0L))
    expect_true(all(packet$markers$layer_id == 0L))
    compile(packet, ...)
  })
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3, z = 1:3), ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_point(size = 3) + coord_3d(backend = backend_3d("raster", dpi = 100))
  grob <- grid::grid.force(ggplot2::ggplotGrob(p))
  find <- function(g) {
    if (identical(g$name, "scene-raster-3d")) return(list(g))
    children <- if (length(g$children)) as.list(g$children) else g$grobs
    unlist(lapply(children, find), recursive = FALSE)
  }
  panel <- find(grob)[[1L]]
  expect_s3_class(panel$children[[2L]], "rastergrob")
  expect_true(panel$children[[2L]]$interpolate)
  expect_identical(panel$raster_result$passes, 1L)
})

test_that("native rasters preserve rectangular device pixel orientation", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = c(.1, .8, .1), y = c(.1, .1, .7), z = rep(.5, 3)),
    corners = list(vertex = 1:3), faces = list(offset = 1L),
    styles = data.frame(style_id = 1L, fill = "red"))
  viewport <- ggplot3d:::new_scene_viewport(120 * 25.4 / 96, 80 * 25.4 / 96, 96)
  result <- ggplot3d:::raster_scene_3d(packet,
    ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90), viewport, 1L)
  capture <- ragg::agg_capture(width = result$width, height = result$height, res = 96, background = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.raster(result$image, interpolate = FALSE)
  pixels <- capture()
  red <- matrix(grDevices::col2rgb(pixels)[2L, ] == 0L, nrow(pixels), ncol(pixels))
  expect_identical(red, result$id != 0L)
})
