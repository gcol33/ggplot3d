texture_top_camera <- function() new_camera_3d(elevation = 90, azimuth = -90)
texture_tilted_camera <- function() new_camera_3d("perspective", azimuth = -35, elevation = 35, fov = 60, zoom = 1.4)

test_that("texture coordinates keep the image upright in raster and vector output", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  image <- matrix(c("red", "blue", "green", "white"), 2L)
  packet <- texture_square_packet(texture_3d(image, interpolate = FALSE))
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  raster <- image_channels(raster_scene_3d(packet, texture_top_camera(), viewport, 1L)$image)
  vector <- image_channels(grid_scene_png(scene_compile(packet, texture_top_camera(), viewport), viewport, 100L))
  for (img in list(raster, vector)) {
    expect_equal(img[38, 38, ], c(255, 0, 0))
    expect_equal(img[38, 62, ], c(0, 255, 0))
    expect_equal(img[62, 38, ], c(0, 0, 255))
    expect_equal(img[62, 62, ], c(255, 255, 255))
  }
})

test_that("a textured checkerboard matches the geometric one under perspective", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  colours <- checker_colours(8L)
  textured <- texture_square_packet(texture_3d(colours, interpolate = FALSE))
  geometric <- checker_square_packet(colours)
  camera <- texture_tilted_camera()
  viewport <- new_scene_viewport(60, 60, dpi = 200 * 25.4 / 60)
  reference <- image_channels(raster_scene_3d(geometric, camera, viewport, 1L)$image)
  inside <- interior_pixels(reference) & reference[, , 1L] %in% c(0, 255)
  expect_gt(sum(inside), 8000L)
  raster <- image_channels(raster_scene_3d(textured, camera, viewport, 1L)$image)
  expect_identical(raster[, , 1L][inside], reference[, , 1L][inside])
  vector_reference <- image_channels(grid_scene_png(scene_compile(geometric, camera, viewport), viewport, 200L))
  vector <- image_channels(grid_scene_png(scene_compile(textured, camera, viewport), viewport, 200L))
  vector_inside <- interior_pixels(vector_reference) & vector_reference[, , 1L] %in% c(0, 255)
  expect_gt(sum(vector_inside), 800L)
  expect_lte(max(abs(vector[, , 1L][vector_inside] - vector_reference[, , 1L][vector_inside])), 8)
  both <- inside & vector_inside
  expect_lte(mean(abs(vector[, , 1L][both] - raster[, , 1L][both]) > 8), 0.002)
})

test_that("vector textures split triangles under perspective only", {
  P <- list(rbind(c(0, 0)), rbind(c(40, 0)), rbind(c(0, 40)))
  UV <- list(rbind(c(0, 0)), rbind(c(1, 0)), rbind(c(0, 1)))
  flat <- subdivide_texture_triangles_3d(P, UV, list(1, 1, 1), 0.05)
  expect_identical(nrow(flat$P[[1L]]), 1L)
  deep <- subdivide_texture_triangles_3d(P, UV, list(1, 0.25, 1), 0.05)
  expect_gt(nrow(deep$P[[1L]]), 16L)
  A <- texture_affine_3d(deep$P, deep$UV)
  expect_true(all(is.finite(A)))
})

test_that("raster textures are bilinear in linear light and mipmapped when minified", {
  ramp <- texture_3d(matrix(c("black", "white"), 1L))
  packet <- texture_square_packet(ramp)
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  img <- image_channels(raster_scene_3d(packet, texture_top_camera(), viewport, 1L)$image)
  expect_equal(img[50, 50, 1L], round(255 * (1.055 * 0.5^(1 / 2.4) - 0.055)), tolerance = 2)
  expect_equal(img[50, 28, 1L], 0)
  expect_equal(img[50, 72, 1L], 255)
  fine <- texture_square_packet(texture_3d(checker_colours(256L), interpolate = FALSE))
  small <- image_channels(raster_scene_3d(fine, texture_top_camera(), viewport, 1L)$image)[30:70, 30:70, 1L]
  expect_lt(stats::sd(small), 3)
  expect_equal(mean(small), 255 * (1.055 * 0.5^(1 / 2.4) - 0.055), tolerance = 0.02)
})

test_that("textures paint over the fill, and outside the image only the fill shows", {
  holed <- matrix(c("red", "transparent"), 1L)
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  img <- function(packet) image_channels(raster_scene_3d(packet, texture_top_camera(), viewport, 1L)$image)
  over <- img(texture_square_packet(texture_3d(holed, interpolate = FALSE), fill = "#0000FF"))
  expect_equal(over[50, 35, ], c(255, 0, 0))
  expect_equal(over[50, 65, ], c(0, 0, 255))
  bare <- raster_scene_3d(texture_square_packet(texture_3d(holed, interpolate = FALSE)), texture_top_camera(), viewport, 1L)$image
  alpha <- matrix(texture_channels_3d(bare)[4L, ], nrow(bare), byrow = TRUE)
  expect_equal(alpha[50, 35], 255)
  expect_equal(alpha[50, 65], 0)
  shifted <- texture_square_packet(texture_3d(matrix("red")), fill = "#00FF00")
  shifted$corners$u <- c(-1, 1, 1, -1)
  out <- img(shifted)
  expect_equal(out[50, 35, ], c(0, 255, 0))
  expect_equal(out[50, 65, ], c(255, 0, 0))
})

test_that("lit textures darken alike in raster and vector output", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  packet <- texture_square_packet(texture_3d(matrix(c("white", "#C08040"), 1L), interpolate = FALSE))
  packet$styles$shade <- TRUE
  light <- light_3d(direction = c(1, 0, 1), space = "data")
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  raster <- image_channels(raster_scene_3d(packet, texture_top_camera(), viewport, 1L, light = light)$image)
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 100L, 100L, res = viewport$dpi, background = "grey50")
  grid::grid.draw(render_scene_grid(scene_compile(packet, texture_top_camera(), viewport), viewport, light = light))
  grDevices::dev.off()
  vector <- image_channels(path)
  for (at in list(c(50, 35), c(50, 65))) {
    expect_lt(raster[at[1L], at[2L], 1L], 250)
    expect_lte(max(abs(vector[at[1L], at[2L], ] - raster[at[1L], at[2L], ])), 6)
  }
})
