source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

tile_scene <- function() {
  surface <- ref_surface(function(x, y) .4 * sin(3 * x) * cos(2 * y), 30L)
  markers <- lapply(seq(.1, .9, length.out = 12L), function(t) matrix(c(t, 1 - t, .8), 1L, 3L))
  line <- list(cbind(seq(.05, .95, length.out = 25L), .5, .5 + .3 * sin(seq(0, 6, length.out = 25L))))
  packet <- ref_packet(surface, line, markers, face_alpha = .6, line_width = 1.5)
  packet$styles$fill <- "orange"; packet$styles$colour <- "navy"
  list(packet = packet, camera = ggplot3d:::new_camera_3d(azimuth = -50, elevation = 35),
       viewport = ggplot3d:::new_scene_viewport(90, 70, 300))
}

test_that("tiled raster output does not depend on the thread count", {
  f <- tile_scene()
  render <- function(threads) {
    old <- options(ggplot3d.threads = threads)
    on.exit(options(old))
    ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 2L, buffers = TRUE)
  }
  one <- render(1L); many <- render(4L)
  expect_gt(one$width, 64L)
  expect_gt(one$height, 64L)
  expect_identical(one$threads, 1L)
  expect_identical(many$threads, 4L)
  for (field in c("image", "id", "passes", "text", "buffers")) expect_identical(one[[field]], many[[field]])
})

test_that("raster buffers describe the nearest fragment of each pixel", {
  f <- tile_scene()
  result <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, buffers = TRUE)
  b <- result$buffers
  expect_identical(dim(b$depth), dim(result$id))
  covered <- result$id != 0L
  expect_true(any(covered) && any(!covered))
  expect_identical(!is.na(b$depth), covered)
  expect_identical(b$kind != 0L, covered)
  ids <- list(f$packet$faces$primitive_id, f$packet$polylines$primitive_id, f$packet$markers$primitive_id)
  expected <- mapply(function(kind, row) if (kind == 0L) 0L else ids[[kind]][row], b$kind, b$row)
  expect_identical(as.vector(expected), as.vector(result$id))
  face <- b$kind == 1L
  expect_true(all(abs(b$normal$x[face]^2 + b$normal$y[face]^2 + b$normal$z[face]^2 - 1) < 1e-9))
  expect_true(all(is.na(b$normal$x[!face])))
  opaque <- !is.na(b$opaque_depth)
  expect_true(all(b$opaque_depth[opaque] >= b$depth[opaque]))
  expect_null(ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L)$buffers)
})

test_that("the thread option is validated and limited under check", {
  old <- options(ggplot3d.threads = 6L)
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", NA)
  on.exit({
    options(old)
    if (is.na(limit)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_") else Sys.setenv(`_R_CHECK_LIMIT_CORES_` = limit)
  })
  Sys.setenv(`_R_CHECK_LIMIT_CORES_` = "TRUE")
  expect_identical(ggplot3d:::raster_threads_3d(), 2L)
  Sys.setenv(`_R_CHECK_LIMIT_CORES_` = "false")
  expect_identical(ggplot3d:::raster_threads_3d(), 6L)
  options(ggplot3d.threads = -1)
  expect_error(ggplot3d:::raster_threads_3d(), "nonnegative")
})
