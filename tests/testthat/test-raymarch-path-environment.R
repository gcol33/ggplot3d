write_test_hdr <- function(image, path, rle) {
  h <- dim(image)[1]; w <- dim(image)[2]
  connection <- file(path, "wb"); on.exit(close(connection))
  writeBin(charToRaw(paste0("#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y ", h, " +X ", w, "\n")), connection)
  for (y in seq_len(h)) {
    scan <- matrix(0L, w, 4)
    for (x in seq_len(w)) {
      rgb <- image[y, x, ]; top <- max(rgb)
      if (top > 0) {
        exponent <- floor(log2(top)) + 1
        scan[x, ] <- c(floor(rgb * 2^(8 - exponent)), exponent + 128)
      }
    }
    if (rle) {
      writeBin(as.raw(c(2, 2, w %/% 256, w %% 256)), connection)
      for (channel in 1:4) {
        bytes <- if (length(unique(scan[, channel])) == 1L) c(128 + w, scan[1, channel]) else c(w, scan[, channel])
        writeBin(as.raw(bytes), connection)
      }
    } else writeBin(as.raw(t(scan)), connection)
  }
}

test_that("RGBE flat and RLE scanlines round trip linear radiance", {
  image <- array(0, c(4, 16, 3))
  image[, , 1] <- rep(2^seq(-3, 0), 16)
  image[, , 2] <- 2; image[, , 3] <- rep(c(4, 8), each = 4)
  for (rle in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".hdr"); on.exit(unlink(path), add = TRUE)
    write_test_hdr(image, path, rle)
    expect_equal(light_environment(path)$image, image)
  }
  path <- tempfile(fileext = ".hdr"); on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw("#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y 4 +X 16\n"), path)
  expect_error(light_environment(path), "Truncated")
})

test_that("environment input and weighted ambient mapping are validated", {
  for (image in list(1, array(1, c(2, 3, 4)), array(-1, c(2, 3, 3)), array(NA_real_, c(2, 3, 3))))
    expect_error(light_environment(image), "image")
  image <- array(1, c(4, 8, 3)); image[1, , ] <- 10
  environment <- light_environment(image, intensity = 2, rotation = 90)
  expect_error(light_environment(image, rotation = NA), "rotation")
  expect_error(light_environment(image, intensity = -1), "intensity")
  light <- light_3d(environment, ambient = 0)
  camera <- ggplot3d:::scene_compile(new_scene_packet(), ggplot3d:::new_camera_3d(), ggplot3d:::new_scene_viewport(25.4, 25.4))$camera
  mean <- ggplot3d:::environment_mean_3d(environment)
  expect_equal(ggplot3d:::resolved_light_3d(light, camera)$ambient, mean)
  expect_equal(ggplot3d:::view_lights_3d(light)$ambient, mean)
  expect_equal(ggplot3d:::resolved_light_3d(light, camera, pathtrace = TRUE)$ambient, rep(0, 3))
  expect_equal(mean, rep(2 + 9 * (1 - cospi(.25)), 3))
  encoded <- light_3d(environment, ambient = 0, linear = FALSE)
  expect_equal(ggplot3d:::resolved_light_3d(encoded, camera)$ambient, ggplot3d:::srgb_encode_3d(mean))
  expect_equal(ggplot3d:::view_lights_3d(encoded)$ambient, ggplot3d:::srgb_encode_3d(mean))
})

test_that("PNG environments are decoded to linear RGB", {
  skip_if_not_installed("png")
  path <- tempfile(fileext = ".png"); on.exit(unlink(path))
  image <- array(.5, c(2, 3, 3)); png::writePNG(image, path)
  expect_equal(light_environment(path)$image, ggplot3d:::srgb_decode_3d(png::readPNG(path)))
  image <- array(.5, c(2, 3, 2)); image[, , 2] <- 1; png::writePNG(image, path)
  expected <- array(rep(ggplot3d:::srgb_decode_3d(png::readPNG(path)[, , 1]), 3), c(2, 3, 3))
  expect_equal(light_environment(path)$image, expected)
})

test_that("every directional and point light contributes its colour", {
  skip_on_cran()
  backend <- backend_3d("pathtrace", samples = 8, bounces = 1, light_radius = 0)
  light <- light_3d(light_directional(c(0, 0, 1), space = "data", colour = "red", intensity = 1),
    light_directional(c(0, 0, 1), space = "data", colour = "blue", intensity = 2), ambient = 0)
  render <- pathtrace_test_scene(backend, light = light)
  expect_equal(render$linear[41, ], c(1, 0, 2), tolerance = 1e-8)
  point <- light_3d(light_point(c(.5, .5, 1.3), colour = "red"), light_point(c(.5, .5, 1.3), colour = "blue", intensity = 2), ambient = 0)
  render <- pathtrace_test_scene(backend, light = point)
  expect_gt(render$linear[41, 1], .8)
  expect_equal(render$linear[41, 3], 2 * render$linear[41, 1])
  head <- pathtrace_test_scene(backend, light = light_3d(light_head(intensity = 1), ambient = 0))
  expect_gt(sum(head$linear), 0)
})

test_that("emissive triangles illuminate other surfaces", {
  skip_on_cran()
  wall <- rbind(c(.8, .1, .3), c(.8, .9, .3), c(.8, .9, .9), c(.8, .1, .9))
  packet <- ref_packet(faces = list(ref_quad(.2), wall))
  packet$styles <- data.frame(style_id = 1:2, fill = "white", material_id = c(NA_integer_, 1L))
  packet$materials <- ggplot3d:::material_table_3d(material_3d(emission = "red", emission_strength = 4))
  packet$style_count <- 2L; packet$faces$style_id <- 1:2
  light <- light_3d(light_directional(intensity = 0), ambient = 0)
  render <- pathtrace_test_scene(backend_3d("pathtrace", samples = 256, bounces = 1), packet, light = light)
  expect_gt(render$linear[41, 1], .05)
  expect_equal(render$linear[41, 2:3], c(0, 0))
})

test_that("hemisphere radiance integrates its sky and ground gradient", {
  skip_on_cran()
  light <- light_3d(light_hemisphere(sky = "red", ground = "blue", direction = c(0, 0, 1), space = "data", intensity = 1), ambient = 0)
  result <- pathtrace_test_scene(backend_3d("pathtrace", samples = 4096, bounces = 1), light = light)
  expect_equal(result$linear[41, ], c(5 / 6, 0, 1 / 6), tolerance = .02)
})
