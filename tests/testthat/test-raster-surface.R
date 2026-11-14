source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

raster_pixels <- function(result, n) {
  image <- matrix(as.integer(result$image), n, n, byrow = TRUE)
  pixels <- as.vector(t(image[n:1L, , drop = FALSE]))
  vapply(c(0L, 8L, 16L, 24L), function(shift) bitwAnd(bitwShiftR(pixels, shift), 255L), numeric(length(pixels)))
}

grid_pixels <- function(compiled, viewport, n) {
  read <- ragg::agg_capture(width = n, height = n, res = viewport$dpi, background = "white")
  on.exit(grDevices::dev.off())
  grid::pushViewport(grid::viewport())
  grid::grid.draw(ggplot3d:::render_scene_grid(compiled, viewport))
  image <- read()
  t(grDevices::col2rgb(as.vector(t(image[n:1L, , drop = FALSE]))))
}

# RGB of straight-alpha raster pixels over white.
over_white <- function(rgba) rgba[, 1:3] * rgba[, 4L] / 255 + 255 - rgba[, 4L]

surface_sheet_packet <- function(mode, fill = c("red", "red", "blue")) {
  packet <- ref_packet(faces = list(ref_quad(.3, x = c(.1, .6, .6, .1)), ref_quad(.4, x = c(.3, .8, .8, .3)),
    ref_quad(.7, x = c(.2, .9, .9, .2), y = c(.5, .5, .9, .9))), face_alpha = .5)
  packet$styles <- data.frame(style_id = 1:3, fill = fill, alpha_mode = mode, sheet = c(1L, 1L, 2L))
  packet$style_count <- 3L
  packet$faces$style_id <- 1:3
  packet
}

test_that("raster alpha_mode = \"surface\" paints a sheet's nearest piece once", {
  n <- 200L
  viewport <- ggplot3d:::new_scene_viewport(100, 100, n * 25.4 / 100)
  camera <- ref_cameras()$pole
  xy <- ref_project(rbind(c(.45, .3, .5), c(.45, .7, .5)), ref_camera(camera, viewport))
  at <- 1L + floor(xy[, 1L] * n) + n * floor(xy[, 2L] * n)
  render <- function(mode, fill = c("red", "red", "blue"))
    over_white(raster_pixels(ggplot3d:::raster_scene_3d(surface_sheet_packet(mode, fill), camera, viewport, 1L,
      blend = "srgb"), n))[at, , drop = FALSE]
  expect_lte(max(abs(render("surface") - rbind(c(255, 128, 128), c(128, 64, 191)))), 1)
  expect_lte(max(abs(render("face")[1L, ] - c(255, 64, 64))), 1)
  expect_lte(max(abs(render("surface", c("red", "green", "blue")) - rbind(c(128, 255, 128), c(64, 128, 191)))), 1)
})

test_that("raster and grid agree on nested translucent sheets with alpha_mode = \"surface\"", {
  skip_if_not_installed("ragg")
  n <- 80L
  for (view in names(ref_cameras())) {
    f <- ref_fixture("nested_isosurfaces", view)
    f$viewport$dpi <- n * 25.4 / f$viewport$width_mm
    faces <- length(f$packet$faces$offset)
    sheet <- ggplot3d:::face_sheets_3d(f$packet$vertices$x[f$packet$corners$vertex],
      f$packet$vertices$y[f$packet$corners$vertex], f$packet$vertices$z[f$packet$corners$vertex],
      rep(seq_len(faces), f$packet$faces$count))
    expect_length(unique(sheet), 2L)
    f$packet$styles <- data.frame(style_id = 1:2, fill = c("#D55E00", "#0072B2"), alpha_mode = "surface",
      sheet = sort(unique(sheet)))
    f$packet$style_count <- 2L
    f$packet$faces$style_id <- match(sheet, sort(unique(sheet)))
    oracle <- ref_render(f$packet, f$camera, f$viewport, n)
    interior <- !oracle$boundary & oracle$visible != 0L
    raster <- over_white(raster_pixels(ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L,
      blend = "srgb"), n))
    vector <- grid_pixels(ggplot3d:::scene_compile(f$packet, f$camera, f$viewport), f$viewport, n)
    expect_lte(max(abs(raster[interior, ] - vector[interior, ])), 2, label = paste("RGB difference", view))
    face <- f$packet
    face$styles$alpha_mode <- "face"
    per_face <- over_white(raster_pixels(ggplot3d:::raster_scene_3d(face, f$camera, f$viewport, 1L,
      blend = "srgb"), n))
    expect_gt(max(abs(per_face[interior, ] - raster[interior, ])), 20)
  }
})
