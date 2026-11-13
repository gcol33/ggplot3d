motion_test_plot <- function(...) {
  ggplot2::ggplot(data.frame(x = c(0, 1, .2), y = c(0, .2, 1), z = c(.1, 1, .5)),
    ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) + coord_3d(...) +
    ggplot2::theme_minimal(base_size = 10)
}

test_that("camera animation builds geometry once and retains every frame", {
  calls <- 0L
  collect <- ggplot3d:::collect_scene_layers
  testthat::local_mocked_bindings(collect_scene_layers = function(...) {
    calls <<- calls + 1L
    collect(...)
  })
  p <- motion_test_plot()
  camera <- p$coordinates$camera
  a <- animate_3d(p, frames = 4, fps = 4, width = 240, height = 240, progress = FALSE)
  on.exit(unlink(dirname(a$frames[1L]), recursive = TRUE), add = TRUE)
  expect_s3_class(a, "ggplot3d_animation")
  expect_identical(calls, 1L)
  expect_identical(a$frame_count, 4L)
  expect_length(a$frames, 4L)
  expect_true(all(file.exists(a$frames)))
  expect_true(all(file.info(a$frames)$size > 0))
  expect_length(unique(unname(tools::md5sum(a$frames))), 4L)
  expect_equal(a$camera$azimuth, c(-60, 30, 120, 210))
  expect_equal(a$camera$time, c(0, .25, .5, .75))
  expect_length(a$timings, 4L)
  expect_true(all(is.finite(a$timings) & a$timings >= 0))
  expect_identical(a$frame, "azimuth")
  for (extent in a$panel_extents) expect_identical(extent, a$panel_extents[[1L]])
  expect_identical(p$coordinates$camera, camera)
})

test_that("time functions, zoom and varying elevation use a locked sphere", {
  a <- animate_3d(motion_test_plot(projection = "perspective", roll = 8),
    azimuth = function(t) -45 + 30 * t, elevation = c(-20, 0, 40),
    zoom = c(.8, 1, 1.1), roll = function(t) 8 + t,
    frames = 3, fps = 2, renderer = "vector", width = 260, height = 260,
    progress = FALSE)
  on.exit(unlink(dirname(a$frames[1L]), recursive = TRUE), add = TRUE)
  expect_equal(a$camera$azimuth, c(-45, -30, -15))
  expect_equal(a$camera$roll, c(8, 8.5, 9))
  expect_equal(a$camera$zoom, c(.8, 1, 1.1))
  expect_identical(a$frame, "sphere")
  for (extent in a$panel_extents) expect_identical(extent, a$panel_extents[[1L]])
})

test_that("facets retain fixed panel dimensions", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + ggplot2::facet_wrap(~am) + coord_3d(z_scales = "free")
  a <- animate_3d(p, frames = 2, width = 480, height = 280, progress = FALSE)
  on.exit(unlink(dirname(a$frames[1L]), recursive = TRUE), add = TRUE)
  expect_length(a$panel_extents[[1L]], 2L)
  expect_identical(a$panel_extents[[1L]], a$panel_extents[[2L]])
})

motion_time_plot <- function() {
  grid <- expand.grid(x = seq(0, 1, length.out = 5), y = seq(0, 1, length.out = 5), t = c(0, .5, 1))
  grid$z <- with(grid, sin(pi * x) * sin(pi * y) * cos(pi * t))
  grid$site <- ifelse(grid$t == 1, "b", "a")
  ggplot2::ggplot(grid, ggplot2::aes(x, y, z = z, fill = ggplot2::after_stat(z_mean))) +
    geom_surface_3d() + coord_3d() + ggplot2::theme_minimal(base_size = 10)
}

test_that("time animation draws one subset per frame with limits from all frames", {
  p <- motion_time_plot()
  a <- animate_3d(p, time = "t", width = 240, height = 240, progress = FALSE)
  on.exit(unlink(dirname(a$frames[1L]), recursive = TRUE), add = TRUE)
  expect_identical(a$frame_count, 3L)
  expect_equal(a$camera$value, c(0, .5, 1))
  expect_equal(a$camera$azimuth, rep(-60, 3))
  for (extent in a$panel_extents) expect_identical(extent, a$panel_extents[[1L]])
  limits <- ggplot3d:::motion_frame_limits_3d(p, "t", c(0, .5, 1))
  z <- lapply(c(0, .5, 1), function(value) {
    built <- ggplot2::ggplot_build(ggplot3d:::motion_frame_plot_3d(p, "t", value, limits))
    c(built$layout$panel_scales_z[[1L]]$get_limits(), built$plot$scales$get_scales("fill")$get_limits())
  })
  expect_equal(z[[1L]][1:2], c(-1, 1))
  for (frame in z) expect_equal(frame, z[[1L]])
  expect_error(animate_3d(p, time = "t", frames = 4, progress = FALSE), "3 distinct values")
  expect_error(animate_3d(p, time = "missing", progress = FALSE), "no layer data")
})

test_that("time animation keeps free z scales per facet", {
  p <- motion_time_plot() + ggplot2::facet_wrap(~site) + coord_3d(z_scales = "free")
  limits <- ggplot3d:::motion_frame_limits_3d(p, "t", c(0, .5, 1))
  built <- ggplot2::ggplot_build(ggplot3d:::motion_frame_plot_3d(p, "t", 0, limits))
  z <- lapply(built$layout$panel_scales_z, function(scale) scale$get_limits())
  expect_length(z, 2L)
  expect_equal(z[[1L]], c(0, 1))
  expect_equal(z[[2L]], c(-1, 0))
})

test_that("geom_blank and expand_limits train scales inside coord_3d", {
  p <- motion_test_plot() + ggplot2::expand_limits(z = 5)
  built <- ggplot2::ggplot_build(p)
  expect_equal(max(built$layout$panel_scales_z[[1L]]$get_limits()), 5)
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("GIF output is encoded from the requested frames", {
  skip_if_not_installed("gifski")
  path <- tempfile(fileext = ".gif")
  on.exit(unlink(path), add = TRUE)
  a <- animate_3d(motion_test_plot(), frames = 3, file = path,
    width = 240, height = 240, progress = FALSE)
  expect_identical(a$file, path)
  expect_identical(a$frame_count, 3L)
  expect_length(a$frames, 0L)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(rawToChar(bytes[1:6]), "GIF89a")
  expect_gt(length(bytes), 100L)
  position <- 14L
  flag <- as.integer(bytes[11L])
  if (bitwAnd(flag, 128L)) position <- position + 3L * 2L^(bitwAnd(flag, 7L) + 1L)
  count <- 0L
  skip_blocks <- function() {
    repeat {
      size <- as.integer(bytes[position])
      position <<- position + size + 1L
      if (size == 0L) break
    }
  }
  repeat {
    tag <- as.integer(bytes[position])
    if (tag == 59L) break
    if (tag == 33L) {
      position <- position + 2L
      skip_blocks()
    } else {
      expect_identical(tag, 44L)
      count <- count + 1L
      flag <- as.integer(bytes[position + 9L])
      position <- position + 10L
      if (bitwAnd(flag, 128L)) position <- position + 3L * 2L^(bitwAnd(flag, 7L) + 1L)
      position <- position + 1L
      skip_blocks()
    }
  }
  expect_identical(count, 3L)
})

test_that("MP4 output is written", {
  skip_if_not_installed("av")
  path <- tempfile(fileext = ".mp4")
  on.exit(unlink(path), add = TRUE)
  a <- animate_3d(motion_test_plot(), frames = 2, file = path,
    width = 240, height = 240, progress = FALSE)
  expect_identical(a$frame_count, 2L)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 100)
  info <- av::av_media_info(path)
  expect_equal(info$video$width, 240)
  expect_equal(info$video$height, 240)
})

test_that("invalid paths and cameras fail before drawing", {
  p <- motion_test_plot()
  expect_error(animate_3d(p, frames = 0), "positive integer")
  expect_error(animate_3d(p, frames = 2, azimuth = 1:3), "one or frames")
  expect_error(animate_3d(p, elevation = 91), "invalid camera")
  expect_error(animate_3d(p, zoom = 0), "invalid camera")
  expect_error(animate_3d(p, file = "movie.avi"), "file must end in", fixed = TRUE)
  expect_error(animate_3d(p, width = NA), "positive integer")
  expect_error(animate_3d(ggplot2::ggplot()), "coord_3d")
})
