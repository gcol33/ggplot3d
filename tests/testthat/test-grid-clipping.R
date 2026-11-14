grid_path_area <- function(path) {
  abs(sum(vapply(split(seq_along(path$x), path$id), function(i) {
    next_i <- c(i[-1L], i[1L])
    sum(path$x[i] * path$y[next_i] - path$x[next_i] * path$y[i]) / 2
  }, numeric(1))))
}

test_that("native painted paths preserve even-odd footprint holes", {
  x <- c(0, 4, 4, 0)
  y <- c(0, 0, 4, 4)
  clipped <- ggplot3d:::grid_clip_paths_cpp(x, y, rep(1L, 4L),
    c(x, 1, 3, 3, 1), c(y, 1, 1, 3, 3), rep(1:2, each = 4L), 0, 0L, 0L, 10, 1e-5)
  expect_equal(grid_path_area(clipped), 12, tolerance = 1e-7)
  order <- as.vector(rbind(1:4, 5:8))
  interleaved <- ggplot3d:::grid_clip_paths_cpp(x, y, rep(1L, 4L),
    c(x, 1, 3, 3, 1)[order], c(y, 1, 1, 3, 3)[order], rep(1:2, each = 4L)[order], 0, 0L, 0L, 10, 1e-5)
  expect_equal(grid_path_area(interleaved), 12, tolerance = 1e-7)
})

test_that("native stroke outlines honour butt, square and round caps", {
  outline <- function(cap) ggplot3d:::grid_clip_paths_cpp(c(0, 2), c(0, 0), c(1L, 1L),
    c(-1, 3, 3, -1), c(-1, -1, 1, 1), rep(1L, 4L), 1, cap, 0L, 10, 1e-6)
  butt <- outline(0L); square <- outline(2L); round <- outline(1L)
  expect_equal(range(butt$x), c(0, 2))
  expect_equal(range(butt$y), c(-0.5, 0.5))
  expect_equal(grid_path_area(butt), 2)
  expect_equal(range(square$x), c(-0.5, 2.5))
  expect_equal(grid_path_area(square), 3)
  expect_equal(grid_path_area(round), 2 + pi / 4, tolerance = 1e-5)
})

test_that("native stroke mitres bevel at the requested ratio", {
  outline <- function(limit) ggplot3d:::grid_clip_paths_cpp(c(0, 1, 1), c(0, 0, 1), rep(1L, 3L),
    c(-2, 3, 3, -2), c(-2, -2, 3, 3), rep(1L, 4L), 1, 0L, 1L, limit, 1e-6)
  bevel <- outline(1)
  mitre <- outline(2)
  expect_equal(grid_path_area(mitre) - grid_path_area(bevel), 0.125, tolerance = 1e-7)
  cached <- ggplot3d:::grid_clip_paths_cpp(numeric(), numeric(), integer(),
    c(-2, 3, 3, -2), c(-2, -2, 3, 3), rep(1L, 4L), 1, 0L, 1L, 2, 1e-6, mitre$outline)
  expect_identical(cached[c("x", "y", "id")], mitre[c("x", "y", "id")])
})

test_that("separate stroke paths retain their paint operations", {
  path <- ggplot3d:::grid_clip_paths_cpp(c(0, 2, 0, 2), c(0, 0, 0, 0), rep(1:2, each = 2L),
    c(-1, 3, 3, -1), c(-1, -1, 1, 1), rep(1L, 4L), 1, 0L, 0L, 10, 1e-6)
  expect_identical(unique(path$path_id), 1:2)
  expect_equal(grid_path_area(path), 4)
})

test_that("collinear round joins preserve butt endpoints on short paths", {
  path <- ggplot3d:::grid_clip_paths_cpp(c(0, 0.1, 0.2), c(0, 0, 0), rep(1L, 3L),
    c(-2, 3, 3, -2), c(-2, -2, 2, 2), rep(1L, 4L), 2, 0L, 0L, 10, 1e-6)
  expect_equal(range(path$x), c(0, 0.2))
  expect_equal(grid_path_area(path), 0.4)
})

test_that("crossing offset joins retain native stroke paint", {
  path <- ggplot3d:::grid_clip_paths_cpp(c(0, 0.1, 0.1), c(0, 0, 0.1), rep(1L, 3L),
    c(-2, 3, 3, -2), c(-2, -2, 2, 2), rep(1L, 4L), 2, 0L, 0L, 10, 1e-6)
  expect_false(path$supported)
})

clipped_grid_stroke <- function(cap = 0L, linetype = "solid") {
  packet <- new_scene_packet(
    vertices = list(x = c(0.1, 0.9), y = c(0.2, 0.8), z = c(0.5, 0.5)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, cap = cap, width_mm = 4),
    styles = data.frame(style_id = 1L, colour = "red", linetype = linetype))
  scene <- scene_compile(packet, new_camera_3d(azimuth = -90, elevation = 90), new_scene_viewport(100, 100))
  scene$fragments$mask_offset[] <- 1L
  scene$fragments$mask_count[] <- 4L
  scene$masks <- list(x_npc = c(0, 0.5, 0.5, 0), y_npc = c(0, 0, 1, 1), ring = rep(1L, 4L))
  scene
}

test_that("solid butt-ended masked strokes paint exact geometry without device clips", {
  scene <- clipped_grid_stroke()
  scene$fragments$opacity[] <- 0.5
  grob <- render_scene_grid(scene, scene$viewport)$children[[1L]]
  expect_s3_class(grob, "pathgrob")
  expect_null(grob$vp)
  expect_equal(grob$gp$alpha, 0.5)
  expect_identical(grob$gp$fill, "red")
  expect_lte(max(as.numeric(grob$x)), 0.5 + 1e-12)
  f <- scene$fragments
  i <- seq.int(f$offset[1L], length.out = 2L)
  a <- c(scene$vertices$x_npc[i[1L]], scene$vertices$y_npc[i[1L]]) * 100
  b <- c(scene$vertices$x_npc[i[2L]], scene$vertices$y_npc[i[2L]]) * 100
  direction <- (b - a) / sqrt(sum((b - a)^2))
  xy <- cbind(as.numeric(grob$x), as.numeric(grob$y)) * 100
  distance <- sweep(xy, 2L, a) %*% c(-direction[2L], direction[1L])
  expect_equal(range(as.vector(distance)), c(-2, 2), tolerance = 2e-8)
  testthat::local_mocked_bindings(grid_capabilities_3d = function() list(clipping_paths = FALSE))
  expect_no_error(render_scene_grid(scene, scene$viewport))
})

test_that("curved caps and dash patterns use filled visible outlines", {
  for (scene in list(clipped_grid_stroke(1L), clipped_grid_stroke(linetype = "dashed"))) {
    grob <- render_scene_grid(scene, scene$viewport)$children[[1L]]
    expect_s3_class(grob, "pathgrob")
    expect_null(grob$vp)
  }
})

test_that("filled marker footprints preserve native fill and border paint", {
  data <- data.frame(x = 0.5, y = 0.5, z = 0.5, size = 4, stroke = 0.5, shape = 19,
    colour = "red", fill = NA_character_, alpha = 0.5, .layer = 1L, .source = 1L)
  packet <- emit_points_3d(data, list(), list(panel_id = 1L))
  scene <- scene_compile(packet, new_camera_3d(azimuth = -90, elevation = 90), new_scene_viewport(100, 100))
  scene$fragments$mask_offset[] <- 1L
  scene$fragments$mask_count[] <- 4L
  scene$masks <- list(x_npc = c(0, 0.5, 0.5, 0), y_npc = c(0, 0, 1, 1), ring = rep(1L, 4L))
  g <- render_scene_grid(scene, scene$viewport)$children[[1L]]
  expect_s3_class(g, "gTree")
  expect_null(g$vp)
  metrics <- paint_metrics_3d(packet$styles)
  expected <- c(pi * metrics$body_radius_mm^2 / 2,
    pi * (metrics$radius_mm^2 - (metrics$body_radius_mm - metrics$border_mm)^2) / 2)
  for (i in 1:2) {
    child <- g$children[[i]]
    expect_equal(child$gp$alpha, 0.5)
    expect_lte(max(as.numeric(child$x)), 0.5 + 1e-10)
    path <- list(x = as.numeric(child$x) * 100, y = as.numeric(child$y) * 100, id = child$id)
    expect_equal(grid_path_area(path), expected[i], tolerance = 0.001)
  }
  skip_if_not_installed("ragg")
  scene$viewport <- new_scene_viewport(20, 20, 254)
  scene$masks$x_npc <- c(0, 1, 1, 0)
  capture <- function(scene) {
    read <- ragg::agg_capture(width = 200, height = 200, res = 254, background = "white")
    on.exit(grDevices::dev.off())
    grid::grid.draw(render_scene_grid(scene, scene$viewport))
    grDevices::col2rgb(as.vector(read()))
  }
  scene$styles$fill <- "blue"
  for (shape in c(1, 15:21)) {
    scene$styles$shape <- shape
    scene$fragments$mask_count[] <- 4L
    painted <- capture(scene)
    scene$fragments$mask_count[] <- 0L
    native <- capture(scene)
    tolerance <- if (shape %in% c(1, 16, 21)) 3 else 2
    expect_true(max(abs(painted - native)) <= tolerance, info = paste("shape", shape))
  }
})

test_that("crossing offset joins preserve the native masked pixels", {
  skip_if_not_installed("ragg")
  packet <- new_scene_packet(
    vertices = list(x = c(0.5, 0.505, 0.505), y = c(0.5, 0.5, 0.505), z = rep(0.5, 3L)),
    corners = list(vertex = 1:3), polylines = list(offset = 1L, count = 3L, cap = 0L, width_mm = 2, opacity = 0.5),
    styles = data.frame(style_id = 1L, colour = "red"))
  scene <- scene_compile(packet, new_camera_3d(azimuth = -90, elevation = 90), new_scene_viewport(20, 20, 254))
  capture <- function(scene) {
    read <- ragg::agg_capture(width = 200, height = 200, res = 254, background = "white")
    on.exit(grDevices::dev.off())
    grob <- render_scene_grid(scene, scene$viewport)
    grid::grid.draw(grob)
    list(grob = grob$children[[1L]], pixels = read())
  }
  native <- capture(scene)
  scene$fragments$mask_offset[] <- 1L
  scene$fragments$mask_count[] <- 4L
  scene$masks <- list(x_npc = c(0, 1, 1, 0), y_npc = c(0, 0, 1, 1), ring = rep(1L, 4L))
  masked <- capture(scene)
  expect_s3_class(masked$grob, "gTree")
  expect_false(is.null(masked$grob$vp$mask))
  expect_identical(masked$pixels, native$pixels)
})
