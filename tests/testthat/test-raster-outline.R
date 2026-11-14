outline_cube_scene <- function() {
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5),
                 c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  p <- ggplot2::ggplot() + geom_mesh_3d(vertices = cube, faces = quads, fill = "grey95",
    outline = outline_3d(hidden = "dashed", linewidth = 0.8, halo = grid::unit(1.2, "mm"))) +
    coord_3d(azimuth = -50, elevation = 25) + ggplot2::theme_minimal(17)
  list(plot = p, packet = stats_scene(p)$packet,
    camera = new_camera_3d(azimuth = -50, elevation = 25), viewport = new_scene_viewport(140, 90, 254))
}

test_that("raster cube edges share vector visibility and stroke widths", {
  skip_if_not_installed("ragg")
  s <- outline_cube_scene()
  scene <- scene_compile(s$packet, s$camera, s$viewport)
  raster <- raster_scene_3d(s$packet, s$camera, s$viewport, 4L)
  capture <- function(g) {
    read <- ragg::agg_capture(width = 1400, height = 900, res = 254, background = "white")
    on.exit(grDevices::dev.off())
    grid::grid.draw(g)
    matrix(grDevices::col2rgb(read())[1L, ], 900, 1400) < 80
  }
  vector <- capture(render_scene_grid(scene, s$viewport))
  image <- capture(raster_image_grob_3d(raster))
  f <- scene$fragments
  rows <- which(f$kind == 2L & !duplicated(ifelse(f$kind == 2L, f$source_index, NA_integer_)))
  expect_length(rows, 12L)
  coverage <- function(pixels) vapply(rows, function(row) {
    i <- f$offset[row] + c(0L, f$count[row] - 1L)
    x <- scene$vertices$x_npc[i] * 1400
    y <- 900 - scene$vertices$y_npc[i] * 900
    t <- seq(0.15, 0.85, length.out = 101)
    pixels[cbind(pmax(1, pmin(900, round(y[1L] + t * diff(y)))),
                 pmax(1, pmin(1400, round(x[1L] + t * diff(x)))))]
  }, logical(101))
  visible <- colMeans(coverage(vector)) > 0.9
  expect_equal(sum(visible), 9L)
  expect_identical(colMeans(coverage(image)) > 0.9, visible)
  s$packet$faces$opacity[] <- 0
  s$packet$polylines$hidden[] <- 0L
  s$packet$polylines$mask_width_mm <- s$packet$polylines$width_mm
  strokes <- scene_compile(s$packet, s$camera, s$viewport)
  vector_width <- capture(render_scene_grid(strokes, s$viewport))
  raster_width <- capture(raster_image_grob_3d(raster_scene_3d(s$packet, s$camera, s$viewport, 4L)))
  expect_equal(sum(raster_width) / sum(vector_width), 1, tolerance = 0.02)
})

test_that("the halo cube vector SVG has compact compound stroke outlines", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("xml2")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  p <- outline_cube_scene()$plot
  svglite::svglite(path, 6.5, 4.5, pointsize = 14)
  print(p)
  grDevices::dev.off()
  xml <- xml2::read_xml(path)
  expect_lt(file.size(path), 40000)
  expect_lt(length(xml2::xml_find_all(xml, "//*[local-name()='path']")), 50L)
})
