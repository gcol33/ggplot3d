path_test_plot <- function(...) {
  ggplot2::ggplot(data.frame(x = c(0, 1, .2, .7), y = c(0, .2, 1, .6), z = c(.1, 1, .5, .3)),
    ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) + coord_3d(...) +
    ggplot2::theme_minimal(base_size = 10)
}

path_test_context <- function(plot = path_test_plot(), ratio = 1) {
  built <- ggplot2::ggplot_build(plot)
  params <- built$layout$panel_params[[1L]]
  ggplot3d:::motion_path_context_3d(plot$coordinates$camera, params, function() ratio)
}

path_outward <- function(step) {
  ggplot3d:::camera_frame_3d(step$azimuth, step$elevation, step$roll)[, 3L]
}

test_that("quaternions round-trip camera angles and unwrap next to the previous frame", {
  set.seed(71)
  for (i in 1:100) {
    angles <- c(azimuth = runif(1, -500, 500), elevation = runif(1, -89.9, 89.9), roll = runif(1, -300, 300))
    q <- ggplot3d:::quaternion_from_matrix_3d(do.call(ggplot3d:::camera_frame_3d, as.list(unname(angles))))
    expect_equal(sum(q^2), 1)
    near <- c(azimuth = angles[["azimuth"]] + 7, roll = angles[["roll"]] - 7)
    expect_equal(ggplot3d:::quaternion_angles_3d(q, near), angles, tolerance = 1e-9)
  }
  pole <- ggplot3d:::quaternion_from_matrix_3d(ggplot3d:::camera_frame_3d(25, 90, 40))
  angles <- ggplot3d:::quaternion_angles_3d(pole, c(azimuth = 25, roll = 0))
  expect_equal(unname(angles[c("azimuth", "elevation")]), c(25, 90))
  expect_equal(ggplot3d:::camera_frame_3d(angles[["azimuth"]], 90, angles[["roll"]]),
    ggplot3d:::camera_frame_3d(25, 90, 40), tolerance = 1e-12)
})

test_that("an orientation path passes over the pole on one great circle", {
  over <- camera_path_3d(list(camera_3d(azimuth = 0, elevation = 30),
    camera_3d(azimuth = 0, elevation = 90),
    camera_3d(azimuth = 180, elevation = 30, roll = 180)), at = c(0, 1, 2))
  steps <- ggplot3d:::path_cameras_3d(over, seq(0, 2, by = 0.05), path_test_context())
  outward <- vapply(steps, path_outward, numeric(3))
  expect_lt(max(abs(outward[2L, ])), 1e-9)
  elevation <- vapply(steps, `[[`, numeric(1), "elevation")
  expect_equal(elevation[c(1L, 11L, 21L, 31L, 41L)], c(30, 60, 90, 60, 30), tolerance = 1e-9)
  turns <- acos(pmin(1, colSums(outward[, -1L] * outward[, -ncol(outward)]))) * 180 / pi
  expect_equal(turns, rep(3, length(turns)), tolerance = 1e-7)
  ends <- steps[[length(steps)]]
  expect_equal(c(ends$azimuth %% 360, ends$elevation, ends$roll %% 360), c(180, 30, 180), tolerance = 1e-9)
})

test_that("squad matches slerp for a uniform turn and smooths uneven keys", {
  context <- path_test_context()
  keys <- lapply(c(0, 60, 120), function(a) camera_3d(azimuth = a, elevation = 20))
  times <- seq(0, 2, by = 0.1)
  slerp <- ggplot3d:::path_cameras_3d(camera_path_3d(keys), times, context)
  squad <- ggplot3d:::path_cameras_3d(camera_path_3d(keys, orientation = "squad"), times, context)
  expect_equal(vapply(squad, `[[`, numeric(1), "azimuth"), seq(0, 120, by = 6), tolerance = 1e-9)
  expect_equal(vapply(slerp, `[[`, numeric(1), "azimuth"), vapply(squad, `[[`, numeric(1), "azimuth"),
    tolerance = 1e-9)
  bent <- list(camera_3d(azimuth = 0, elevation = 0), camera_3d(azimuth = 60, elevation = 0),
    camera_3d(azimuth = 60, elevation = 30))
  rate <- function(orientation) {
    h <- 1e-4
    steps <- ggplot3d:::path_cameras_3d(camera_path_3d(bent, orientation = orientation),
      c(1 - h, 1, 1 + h), context)
    q <- lapply(steps, function(s) ggplot3d:::camera_frame_3d(s$azimuth, s$elevation, s$roll))
    angle <- function(a, b) acos(min(1, (sum(diag(crossprod(a, b))) - 1) / 2))
    c(angle(q[[1L]], q[[2L]]), angle(q[[2L]], q[[3L]])) / h
  }
  expect_gt(abs(diff(rate("slerp"))), 0.1)
  expect_lt(abs(diff(rate("squad"))), 1e-2)
})

test_that("targets follow a centripetal Catmull-Rom spline and tracks interpolate in log space", {
  context <- path_test_context(path_test_plot(projection = "perspective"))
  keys <- list(list(target = c(.2, .2, .5), fov = 10, zoom = 1),
    list(target = c(.5, .8, .5), fov = 40, zoom = 4), list(target = c(.8, .3, .5), fov = 40, zoom = 4))
  steps <- ggplot3d:::path_cameras_3d(camera_path_3d(keys), c(0, 0.5, 1, 2), context)
  expect_equal(steps[[1L]]$target, c(.2, .2, .5))
  expect_equal(steps[[3L]]$target, c(.5, .8, .5))
  expect_equal(steps[[4L]]$target, c(.8, .3, .5))
  expect_gt(steps[[2L]]$fov, 10)
  expect_lt(steps[[2L]]$fov, 40)
  linear <- ggplot3d:::path_cameras_3d(camera_path_3d(keys, position = "linear"), 0.5, context)[[1L]]
  expect_equal(linear$fov, 20)
  expect_equal(linear$zoom, 2)
  expect_equal(linear$target, c(.35, .5, .5))
  equal <- ggplot3d:::catmull_rom_3d(rbind(c(0, 0), c(1, 1), c(1, 1), c(2, 0)), c(2L, 2L), c(.3, .7))
  expect_equal(equal, rbind(c(1, 1), c(1, 1)))
  expect_error(ggplot3d:::path_cameras_3d(camera_path_3d(list(list(azimuth = 0), list(distance = 3))),
    0, context), "every key or in none")
  expect_error(ggplot3d:::path_cameras_3d(camera_path_3d(list(camera_3d(), camera_3d("cabinet"))), 0,
    context), "one projection")
})

test_that("named easings run from 0 to 1 and reflect into out and in-out variants", {
  families <- names(ggplot3d:::ease_families_3d)
  for (name in c("linear", paste0(rep(families, each = 3L), c("-in", "-out", "-in-out")))) {
    f <- ggplot3d:::ease_function_3d(name)
    expect_equal(f(c(0, 1)), c(0, 1), tolerance = 1e-12, label = name)
  }
  cubic <- ggplot3d:::ease_function_3d("cubic-in-out")
  expect_equal(cubic(c(.25, .5)), c(.0625, .5))
  expect_equal(ggplot3d:::ease_function_3d("quadratic-out")(.5), .75)
  expect_error(camera_path_3d(list(camera_3d(), camera_3d()), ease = "wobble"), "unknown ease")
  expect_error(camera_path_3d(list(camera_3d(), camera_3d()), at = c(1, 2)), "starting at 0")
  expect_error(camera_path_3d(list(camera_3d())), "at least two")
})

test_that("joined paths continue from the previous part", {
  tour <- c(orbit_3d(90, 1), dolly_zoom_3d(10, 2), orbit_3d(-90, 1, elevation = 60))
  expect_s3_class(tour, "ggplot3d_camera_path")
  expect_equal(tour$duration, 4)
  expect_output(print(tour), "orbit of 90 degrees over 1 s")
  expect_output(print(tour), "dolly zoom to fov 10 over 2 s")
  context <- path_test_context(path_test_plot(projection = "perspective"))
  steps <- ggplot3d:::path_cameras_3d(tour, c(0, 1, 2, 3, 3.5, 4), context)
  expect_equal(vapply(steps, `[[`, numeric(1), "azimuth"), c(-60, 30, 30, 30, -15, -60))
  expect_equal(steps[[4L]]$fov, 10)
  expect_equal(steps[[6L]]$fov, 10)
  expect_equal(steps[[6L]]$elevation, 60)
  expect_equal(steps[[2L]]$fov, 35)
})

test_that("a dolly zoom keeps the target plane's image scale, through to the parallel view", {
  viewport <- list(width_mm = 80, height_mm = 100, dpi = 96)
  plot <- path_test_plot(projection = "perspective", aspect = c(1, 2, 1), azimuth = -30)
  context <- path_test_context(plot, ratio = 0.8)
  steps <- ggplot3d:::path_cameras_3d(dolly_zoom_3d(0, 2, ease = "linear"), seq(0, 2, by = 0.25), context)
  expect_identical(vapply(steps, `[[`, character(1), "projection"), c(rep("perspective", 8L), "orthographic"))
  cameras <- lapply(steps, ggplot3d:::motion_camera_3d, base = plot$coordinates$camera)
  image_of <- function(cameras) vapply(cameras, function(camera) {
    basis <- ggplot3d:::frame_basis_3d(camera, viewport)
    point <- camera$target + (0.15 * basis$basis[, 1L] + 0.1 * basis$basis[, 2L]) / basis$scale
    ggplot3d:::frame_positions_3d(matrix(point, 1L), camera, basis)[1L, 1:2]
  }, numeric(2))
  image <- image_of(cameras)
  expect_equal(image, image[, rep(1L, ncol(image))], tolerance = 1e-9)
  automatic <- ggplot3d:::frame_basis_3d(plot$coordinates$camera, viewport)$distance
  expect_equal(steps[[1L]]$distance, automatic)
  back <- ggplot3d:::path_cameras_3d(c(dolly_zoom_3d(0, 1), dolly_zoom_3d(35, 1)), c(0, 2), context)
  expect_identical(back[[2L]]$projection, "perspective")
  expect_equal(back[[2L]]$fov, back[[1L]]$fov)
  returned <- image_of(lapply(back, ggplot3d:::motion_camera_3d, base = plot$coordinates$camera))
  expect_equal(returned[, 2L], returned[, 1L], tolerance = 1e-9)
})

test_that("a fly-through looks from data positions at a data point", {
  plot <- path_test_plot(projection = "perspective")
  context <- path_test_context(plot)
  fly <- fly_through_3d(data.frame(x = c(-1, .5, 2), y = c(.5, -1, .5), z = c(.5, .5, 2)),
    look_at = c(.5, .6, .55), fov = 50)
  steps <- ggplot3d:::path_cameras_3d(fly, c(0, 1, 2), context)
  box <- context$to_box(rbind(c(-1, .5, .5), c(.5, .6, .55)))
  expect_equal(steps[[1L]]$target, box[2L, ])
  expect_equal(steps[[1L]]$distance, sqrt(sum((box[1L, ] - box[2L, ])^2)))
  expect_equal(path_outward(steps[[1L]]), (box[1L, ] - box[2L, ]) / steps[[1L]]$distance)
  expect_true(all(vapply(steps, `[[`, numeric(1), "fov") == 50))
  expect_error(fly_through_3d(matrix(1:3, 1L)), "at least two")
  expect_error(fly_through_3d(matrix(1:6, 2L), look_at = matrix(1:6, 2L)[, 1:2]), "finite x, y and z")
})

test_that("animate_3d renders paths, closes loops and passes the eye through the box", {
  p <- path_test_plot(projection = "perspective")
  a <- animate_3d(p, path = orbit_3d(360, 1), fps = 4, width = 200, height = 200, progress = FALSE)
  on.exit(unlink(dirname(a$frames[1L]), recursive = TRUE), add = TRUE)
  expect_identical(a$frame_count, 4L)
  expect_equal(a$camera$azimuth, c(-60, 30, 120, 210))
  expect_identical(a$frame, "azimuth")
  b <- animate_3d(p, path = c(orbit_3d(45, 1), dolly_zoom_3d(0, 1)), fps = 2,
    width = 200, height = 200, progress = FALSE)
  on.exit(unlink(dirname(b$frames[1L]), recursive = TRUE), add = TRUE)
  expect_identical(b$frame_count, 5L)
  expect_equal(b$camera$time, seq(0, 2, by = .5))
  expect_identical(b$camera$projection[5L], "orthographic")
  expect_identical(b$frame, "sphere")
  for (extent in b$panel_extents) expect_identical(extent, b$panel_extents[[1L]])
  fly <- fly_through_3d(data.frame(x = c(-1, .5, 2), y = c(.5, .5, .5), z = c(.5, .5, .5)),
    look_at = data.frame(x = c(.5, 1.5, 3), y = .5, z = .5))
  d <- animate_3d(p, path = fly, frames = 3, width = 200, height = 200, progress = FALSE)
  on.exit(unlink(dirname(d$frames[1L]), recursive = TRUE), add = TRUE)
  expect_true(all(file.info(d$frames)$size > 0))
  expect_equal(d$camera$time, c(0, 1, 2))
  expect_error(animate_3d(p, path = orbit_3d(), azimuth = 3), "either path")
  expect_error(animate_3d(p, path = list()), "camera path")
})

test_that("the eye inside the box leaves out the axes", {
  p <- path_test_plot(projection = "perspective", distance = 0.3, frame = "sphere")
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  ragg::agg_png(path, 300, 300, res = 96)
  forced <- grid::grid.force(ggplot2::ggplotGrob(p))
  grDevices::dev.off()
  panel <- ggplot3d:::motion_panels_3d(forced)[[1L]]
  expect_length(panel$furniture$children, 0L)
  expect_true(all(is.na(panel$furniture$corners_mm)))
})

png_test_chunks <- function(file) {
  bytes <- readBin(file, "raw", n = file.info(file)$size)
  chunks <- list(); position <- 9L
  while (position < length(bytes)) {
    size <- sum(as.double(as.integer(bytes[position + 0:3])) * 256^(3:0))
    body <- bytes[position + 4L + seq_len(size + 4L) - 1L]
    crc <- sum(as.double(as.integer(bytes[position + 8L + size + 0:3])) * 256^(3:0))
    chunks[[length(chunks) + 1L]] <- list(type = rawToChar(body[1:4]), data = body[-(1:4)],
      valid = crc == ggplot3d:::png_crc32_cpp(body))
    position <- position + 12L + size
  }
  list(signature = bytes[1:8], chunks = chunks)
}

test_that("animated PNG output carries one control chunk per frame with valid checksums", {
  expect_identical(ggplot3d:::png_crc32_cpp(charToRaw("123456789")), 3421780262)
  file <- tempfile(fileext = ".apng")
  on.exit(unlink(file), add = TRUE)
  a <- animate_3d(path_test_plot(), path = orbit_3d(90, 1), fps = 3, file = file,
    width = 240, height = 200, progress = FALSE)
  expect_identical(a$frame_count, 4L)
  expect_length(a$frames, 0L)
  png <- png_test_chunks(file)
  expect_identical(png$signature, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  types <- vapply(png$chunks, `[[`, character(1), "type")
  expect_true(all(vapply(png$chunks, `[[`, logical(1), "valid")))
  expect_identical(types[1:2], c("IHDR", "acTL"))
  expect_identical(types[length(types)], "IEND")
  expect_identical(as.integer(png$chunks[[2L]]$data[1:4]), c(0L, 0L, 0L, 4L))
  expect_identical(sum(types == "fcTL"), 4L)
  expect_gte(sum(types == "fdAT"), 3L)
  expect_lt(match("fcTL", types), match("IDAT", types))
  sequence <- vapply(png$chunks[types %in% c("fcTL", "fdAT")], function(chunk) {
    sum(as.double(as.integer(chunk$data[1:4])) * 256^(3:0))
  }, numeric(1))
  expect_equal(sequence, seq_along(sequence) - 1)
  delay <- png$chunks[[which(types == "fcTL")[1L]]]$data[21:24]
  expect_identical(as.integer(delay), c(0L, 1L, 0L, 3L))
})

test_that("frame sequences, SVG and PDF pages are written per frame", {
  directory <- tempfile("ggplot3d-out-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  p <- path_test_plot()
  a <- animate_3d(p, frames = 3, file = file.path(directory, "turn-%02d.png"),
    width = 240, height = 240, progress = FALSE)
  expect_identical(basename(a$frames), sprintf("turn-%02d.png", 1:3))
  expect_true(all(file.exists(a$frames)))
  b <- animate_3d(p, frames = 2, file = file.path(directory, "turn.png"),
    width = 240, height = 240, progress = FALSE)
  expect_identical(basename(b$frames), c("turn-0001.png", "turn-0002.png"))
  pdf <- file.path(directory, "turn.pdf")
  animate_3d(p, frames = 3, file = pdf, renderer = "vector", width = 200, height = 200, progress = FALSE)
  bytes <- readBin(pdf, "raw", n = file.info(pdf)$size)
  bytes[bytes == as.raw(0)] <- as.raw(32)
  text <- rawToChar(bytes)
  expect_match(substr(text, 1L, 8L), "%PDF", fixed = TRUE)
  expect_match(text, "/Type /Pages", fixed = TRUE, useBytes = TRUE)
  expect_match(text, "/Count 3", fixed = TRUE, useBytes = TRUE)
  skip_if_not_installed("svglite")
  s <- animate_3d(p, frames = 2, file = file.path(directory, "turn.svg"), renderer = "vector",
    width = 200, height = 200, progress = FALSE)
  expect_length(s$frames, 2L)
  expect_match(readLines(s$frames[2L], warn = FALSE)[1L], "<?xml", fixed = TRUE)
  expect_error(animate_3d(p, file = file.path(directory, "turn.avi")), "file must end in")
})

test_that("WebM output is encoded", {
  skip_if_not_installed("av")
  file <- tempfile(fileext = ".webm")
  on.exit(unlink(file), add = TRUE)
  animate_3d(path_test_plot(), frames = 2, file = file, width = 160, height = 160, progress = FALSE)
  info <- av::av_media_info(file)
  expect_equal(info$video$width, 160)
  expect_match(info$video$codec, "vp")
})

test_that("frames drawn by several workers equal the serial frames", {
  skip_on_cran()
  p <- path_test_plot(projection = "perspective")
  serial <- animate_3d(p, path = orbit_3d(90, 1), fps = 4, width = 160, height = 160, progress = FALSE)
  on.exit(unlink(dirname(serial$frames[1L]), recursive = TRUE), add = TRUE)
  if (.Platform$OS.type == "unix") {
    workers <- 2L
  } else {
    workers <- parallel::makePSOCKcluster(2L)
    on.exit(parallel::stopCluster(workers), add = TRUE)
    root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
    if (file.exists(file.path(root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE) &&
        pkgload::is_dev_package("ggplot3d")) {
      parallel::clusterCall(workers, function(path) {
        pkgload::load_all(path, quiet = TRUE, export_all = FALSE)
        NULL
      }, root)
    } else parallel::clusterEvalQ(workers, library(ggplot3d))
  }
  split <- animate_3d(p, path = orbit_3d(90, 1), fps = 4, width = 160, height = 160,
    progress = FALSE, workers = workers)
  on.exit(unlink(dirname(split$frames[1L]), recursive = TRUE), add = TRUE)
  expect_identical(split$frame_count, serial$frame_count)
  expect_identical(unname(tools::md5sum(split$frames)), unname(tools::md5sum(serial$frames)))
  expect_identical(split$panel_extents, serial$panel_extents)
  expect_error(animate_3d(p, workers = 0), "workers")
})

test_that("the viewer carries the sampled path for playback", {
  skip_if_not_installed("htmlwidgets")
  p <- path_test_plot(projection = "perspective")
  w <- view_3d(p, path = orbit_3d(360, 2), fps = 10, width = 300, height = 300)
  path <- w$x$path
  expect_length(path$azimuth, 20L)
  expect_true(path$loop)
  expect_equal(path$step, 0.1)
  expect_equal(path$azimuth[1:2], c(-60, -42))
  expect_true(all(path$distance == -1))
  expect_true(is.finite(path$zoom_scale) && path$zoom_scale > 0)
  dolly <- view_3d(p, path = dolly_zoom_3d(0, 1), fps = 4)$x$path
  expect_false(dolly$loop)
  expect_identical(dolly$projection[length(dolly$projection)], "orthographic")
  expect_equal(dolly$zoom_scale, 1)
  expect_null(view_3d(p)$x$path)
  expect_error(view_3d(p, path = "orbit"), "camera path")
})

test_that("panels follow a path's projection and target unless their view sets them", {
  base <- coord_3d()$camera
  moved <- motion_camera_3d(list(projection = "perspective", fov = 20, distance = 4,
    target = c(0.2, 0.5, 0.5), azimuth = base$azimuth + 30), base)
  plain <- base
  plain$aspect <- c(2, 1, 1)
  follow <- motion_panel_camera_3d(moved, plain, base)
  expect_identical(follow$projection, "perspective")
  expect_equal(follow$fov, 20)
  expect_equal(follow$distance, 4)
  expect_equal(follow$target, c(0.2, 0.5, 0.5))
  expect_equal(follow$aspect, c(2, 1, 1))
  expect_equal(follow$azimuth, base$azimuth + 30)
  oblique <- base
  oblique$projection <- "oblique"
  oblique$shear <- c(0.5, 0.25)
  own <- motion_panel_camera_3d(moved, oblique, base)
  expect_identical(own$projection, "oblique")
  expect_equal(own$shear, c(0.5, 0.25))
  expect_equal(own$distance, base$distance)
  expect_equal(own$target, c(0.2, 0.5, 0.5))
})
