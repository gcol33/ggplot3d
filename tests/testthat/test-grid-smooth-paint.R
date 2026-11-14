smooth_paint_encode <- function(v) ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)

smooth_paint_packet <- function(rgb, opacity = 1, colour = NA_character_) {
  new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5), y = c(0.1, 0.1, 0.9), z = rep(0.5, 3L), w = rep(1, 3L)),
    corners = list(vertex = 1:3, source_row = 1:3, red = rgb[, 1L], green = rgb[, 2L], blue = rgb[, 3L]),
    faces = list(offset = 1L, layer_id = 1L, opacity = opacity),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = colour, shade = FALSE))
}

# Vector and raster renderings of a packet on a 200 x 200 px device and the
# largest channel difference (in 1/255) over the pixels at least three pixels
# inside the face.
smooth_paint_compare <- function(packet, mode = "auto") {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  old <- options(ggplot3d.smooth_vector = mode)
  on.exit(options(old), add = TRUE)
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  viewport <- new_scene_viewport(50.8, 50.8, dpi = 100)
  draw <- function(grob) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, 200, 200, res = 100, background = "white")
    grid::grid.draw(grob)
    grDevices::dev.off()
    on.exit(unlink(path))
    png::readPNG(path)[, , 1:3]
  }
  scene <- scene_compile(packet, camera, viewport)
  vector_grob <- render_scene_grid(scene, viewport)
  vector <- draw(vector_grob)
  raster <- draw(raster_image_grob_3d(raster_scene_3d(packet, camera, viewport, 4L)))
  inside <- rowSums(abs(raster - 1), dims = 2L) > 0.02
  for (step in 1:3) inside <- inside & cbind(FALSE, inside[, -ncol(inside)]) & cbind(inside[, -1L], FALSE) &
    rbind(FALSE, inside[-nrow(inside), ]) & rbind(inside[-1L, ], FALSE)
  difference <- apply(abs(vector - raster), 1:2, max) * 255
  list(error = max(difference[inside]), pixels = sum(inside), grob = vector_grob)
}

grob_classes <- function(g) {
  out <- class(g)[1L]
  if (inherits(g, "gTree")) out <- c(out, unlist(lapply(g$children, grob_classes)))
  out
}

test_that("a face whose colour varies along one direction is one gradient, without subdivision", {
  rgb <- smooth_paint_encode(c(1, 0.4, 0.05) %o% c(0.6, 0.2, 0.05))
  x <- c(5, 45, 25); y <- c(5, 5, 45)
  linear <- c(1, 0.4, 0.05) %o% c(0.6, 0.2, 0.05)
  expect_false(smooth_image_needed_3d(x, y, rep(1L, 3L), linear, gradients = TRUE))
  painted <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE)
  expect_identical(painted$gradient, TRUE)
  out <- smooth_paint_compare(smooth_paint_packet(rgb))
  expect_gt(out$pixels, 2000L)
  expect_lte(out$error, 2)
  expect_false("rastergrob" %in% grob_classes(out$grob))
})

test_that("a face with colours varying in two directions is painted as an image matching raster", {
  out <- smooth_paint_compare(smooth_paint_packet(diag(3)))
  expect_true("rastergrob" %in% grob_classes(out$grob))
  expect_gt(out$pixels, 2000L)
  expect_lte(out$error, 2)
  translucent <- smooth_paint_compare(smooth_paint_packet(diag(3), opacity = 0.5))
  expect_lte(translucent$error, 2)
  bordered <- smooth_paint_compare(smooth_paint_packet(diag(3), colour = "black"))
  expect_true(all(c("rastergrob", "polygon") %in% grob_classes(bordered$grob)))
})

test_that("opaque flat and gradient faces share continuous seam coverage", {
  xy <- expand.grid(x = c(0.1, 0.5, 0.9), y = c(0.1, 0.9))
  faces <- rbind(c(1L, 2L, 4L), c(2L, 5L, 4L), c(2L, 3L, 5L), c(3L, 6L, 5L))
  vertex <- as.vector(t(faces))
  channel <- smooth_paint_encode(c(0.25, 0.5, 0.5, 0.25, 0.5, 0.5))[vertex]
  packet <- new_scene_packet(vertices = list(x = xy$x, y = xy$y, z = rep(0.5, 6L)),
    corners = list(vertex = vertex, red = channel, green = channel, blue = channel),
    faces = list(offset = seq(1L, 10L, by = 3L), count = rep(3L, 4L), layer_id = rep(1L, 4L)),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = NA_character_, shade = FALSE))
  out <- smooth_paint_compare(packet)
  expect_gt(out$pixels, 0L)
  expect_lte(out$error, 2)
})

test_that("forced subdivision is capped and its pieces overlap inside the face", {
  rgb <- rbind(c(1, 0.8, 0.8), c(0.8, 1, 0.8), c(0.8, 0.8, 1))
  out <- smooth_paint_compare(smooth_paint_packet(rgb), mode = "subdivide")
  expect_false("rastergrob" %in% grob_classes(out$grob))
  expect_lte(out$error, 3)
  x <- c(0, 40, 10); y <- c(0, 0, 30)
  linear <- diag(3)
  painted <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE, pixel_mm = 0.01)
  expect_identical(max(painted$id), 256L)
  coarse <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE, pixel_mm = 15)
  expect_identical(max(coarse$id), 4L)
  pieces <- subdivide_colour_triangles_3d(list(cbind(0, 0), cbind(40, 0), cbind(10, 30)),
    list(linear[1L, , drop = FALSE], linear[2L, , drop = FALSE], linear[3L, , drop = FALSE]), 2 / 255,
    pixel_mm = 1, max_level = 1L)
  area <- function(P) sum(abs((P[[2L]][, 1L] - P[[1L]][, 1L]) * (P[[3L]][, 2L] - P[[1L]][, 2L]) -
    (P[[3L]][, 1L] - P[[1L]][, 1L]) * (P[[2L]][, 2L] - P[[1L]][, 2L]))) / 2
  expect_gt(area(pieces$P), 600)
  X <- do.call(rbind, pieces$P)
  weights <- solve(rbind(c(0, 40, 10), c(0, 0, 30), 1), rbind(t(X), 1))
  expect_gte(min(weights), -1e-9)
})

test_that("an interpolated colour hull draws to a small vector file", {
  skip_on_cran()
  rgb <- expand.grid(r = seq(0, 1, length.out = 5), g = seq(0, 1, length.out = 5), b = seq(0, 1, length.out = 5))
  rgb <- rgb[apply(rgb == 0 | rgb == 1, 1L, any), ]
  lab <- grDevices::convertColor(as.matrix(rgb), from = "sRGB", to = "Lab", scale.in = 1)
  gamut <- data.frame(a = lab[, 2L], b = lab[, 3L], L = lab[, 1L], hex = grDevices::rgb(rgb$r, rgb$g, rgb$b))
  p <- ggplot2::ggplot(gamut, ggplot2::aes(a, b, z = L)) +
    stat_hull_3d(ggplot2::aes(fill = I(hex), group = 1), shading = "smooth", shade = FALSE) +
    coord_3d(aspect = "data")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  time <- system.time({
    grDevices::svg(path, 4, 4)
    print(p)
    grDevices::dev.off()
  })[["elapsed"]]
  expect_lt(file.size(path), 1e6)
  expect_lt(time, 20)
})
