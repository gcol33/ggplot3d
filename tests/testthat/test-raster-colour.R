source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

srgb_decode <- function(v) ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
srgb_encode <- function(v) ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)
test_that("linear blending composites the translucent stack in linear light", {
  f <- ref_fixture("translucent_points", "pole")
  f$packet$styles <- data.frame(style_id = 1:3, fill = c("red", NA, NA), colour = c(NA, "blue", "green"))
  f$packet$style_count <- 3L
  f$packet$markers$style_id <- 2:3
  f$viewport$dpi <- 80 * 25.4 / 100
  compiled <- ggplot3d:::scene_compile(f$packet, f$camera, f$viewport)
  vector <- ref_rasterise(compiled, f$packet, 80L)
  oracle <- ref_render(f$packet, f$camera, f$viewport, 80L)
  raster <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "linear")
  channels <- unpack_rgba(raster$image)
  colours <- srgb_decode(rbind(c(1, 0, 0), c(0, 0, 1), c(0, 1, 0)))
  expected <- matrix(0, 6400, 4)
  for (i in which(!oracle$boundary)) {
    ids <- vector$layers[[i]]; alpha <- vector$alpha[[i]]
    if (!length(ids)) next
    weight <- alpha * cumprod(c(1, 1 - head(alpha, -1L)))
    expected[i, 4L] <- sum(weight)
    expected[i, 1:3] <- srgb_encode(colSums(colours[ids, , drop = FALSE] * weight) / sum(weight))
  }
  inside <- !oracle$boundary
  expect_true(any(expected[inside, 4L] > 0 & expected[inside, 4L] < 1))
  expect_lte(max(abs(channels[inside, ] - expected[inside, ])), 1 / 255)
})

test_that("linear blending averages antialiasing samples in linear light", {
  f <- ref_fixture("marker_silhouette", "pole")
  f$packet$styles <- data.frame(style_id = 1:2, fill = c("red", NA), colour = c(NA, "blue"))
  f$packet$style_count <- 2L
  f$packet$markers$style_id[] <- 2L
  f$viewport$dpi <- 64 * 25.4 / 100
  averaged <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 2L, blend = "linear")
  f$viewport$dpi <- f$viewport$dpi * 2
  samples <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "linear")
  fine <- unpack_rgba(samples$image)
  coarse <- unpack_rgba(averaged$image)
  index <- function(x, y, n) (y - 1L) * n + x
  grid <- expand.grid(x = 1:64, y = 1:64)
  blocks <- lapply(list(c(0L, 0L), c(1L, 0L), c(0L, 1L), c(1L, 1L)), function(o)
    index(2L * grid$x - 1L + o[1L], 2L * grid$y - 1L + o[2L], 128L))
  alpha <- Reduce(`+`, lapply(blocks, function(k) fine[k, 4L])) / 4
  premultiplied <- Reduce(`+`, lapply(blocks, function(k) srgb_decode(fine[k, 1:3]) * fine[k, 4L])) / 4
  covered <- alpha > 0
  expected <- srgb_encode(premultiplied[covered, ] / alpha[covered])
  mixed <- covered & rowSums(abs(coarse[, 1:3] - (Reduce(`+`, lapply(blocks, function(k) fine[k, 1:3])) / 4))) > 0.05
  expect_true(any(mixed))
  expect_lte(max(abs(coarse[covered, 1:3] - expected)), 1 / 255)
  expect_lte(max(abs(coarse[, 4L] - alpha)), 1 / 255)
})

test_that("blending spaces agree on pixels painted by one opaque colour", {
  f <- ref_fixture("A_separate", "oblique")
  f$packet$styles$fill <- "#3366cc"
  linear <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "linear")
  srgb <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "srgb")
  expect_identical(linear$image, srgb$image)
  expect_error(ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "gamma"))
  expect_identical(backend_3d("raster")$blend, "linear")
})
