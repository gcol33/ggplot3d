source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

split_quad <- function(fill) {
  quad <- ref_quad(.5)
  packet <- ref_packet(faces = list(quad[c(1L, 2L, 3L), ], quad[c(1L, 3L, 4L), ]), face_alpha = .5)
  packet$styles <- data.frame(style_id = seq_along(fill), fill = fill)
  packet$style_count <- length(fill)
  packet$faces$style_id <- rep_len(seq_along(fill), 2L)
  packet
}

capture_grob <- function(grob, n, dpi) {
  read <- ragg::agg_capture(width = n, height = n, res = dpi, background = "white")
  on.exit(grDevices::dev.off())
  grid::pushViewport(grid::viewport())
  grid::grid.draw(grob)
  read()
}

test_that("a translucent one-colour surface covers its shared edges once", {
  skip_if_not_installed("ragg")
  n <- 200L
  viewport <- ggplot3d:::new_scene_viewport(100, 100, n * 25.4 / 100)
  camera <- ref_cameras()$pole
  compiled <- ggplot3d:::scene_compile(split_quad("red"), camera, viewport)
  grob <- ggplot3d:::render_scene_grid(compiled, viewport)
  expect_s3_class(grob$children[[1L]], "pathgrob")
  pixels <- capture_grob(grob, n, viewport$dpi)
  xy <- ref_project(cbind(seq(.2, .8, length.out = 41L), seq(.2, .8, length.out = 41L), .5),
                    ref_camera(camera, viewport))
  diagonal <- cbind(n - floor(xy[, 2L] * n), 1L + floor(xy[, 1L] * n))
  interior <- grDevices::col2rgb(pixels[diagonal[1L, 1L] + 10L, diagonal[1L, 2L]])
  expect_lte(max(abs(as.vector(interior) - c(255, 128, 128))), 1)
  expect_lte(max(abs(grDevices::col2rgb(pixels[diagonal]) - as.vector(interior))), 2)
  f <- compiled$fragments
  i <- rep(f$offset, f$count) + sequence(f$count) - 1L
  per_face <- grid::polygonGrob(compiled$vertices$x_npc[i], compiled$vertices$y_npc[i],
    id = rep(seq_along(f$count), f$count), gp = grid::gpar(fill = "red", col = NA, alpha = .5))
  seam <- grDevices::col2rgb(capture_grob(per_face, n, viewport$dpi)[diagonal])
  expect_gt(max(abs(seam - as.vector(interior))), 2)
})

test_that("translucent colour paths composite atop the union where the device allows it", {
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 72)
  compiled <- ggplot3d:::scene_compile(split_quad(c("red", "blue")), ref_cameras()$pole, viewport)
  render <- function(atop) {
    testthat::local_mocked_bindings(grid_capabilities_3d = function() list(clipping_paths = TRUE, group_atop = atop))
    ggplot3d:::render_scene_grid(compiled, viewport)$children
  }
  plain <- render(FALSE)
  expect_length(plain, 1L)
  expect_s3_class(plain[[1L]], "pathgrob")
  expect_identical(plain[[1L]]$gp$alpha, .5)
  expect_identical(plain[[1L]]$gp$fill, c("red", "blue"))
  skip_if(getRversion() < "4.2.0")
  grouped <- render(TRUE)
  expect_length(grouped, 1L)
  expect_false(is.null(grouped[[1L]]$vp$mask))
  expect_s3_class(grouped[[1L]]$children[[1L]], "GridGroup")
  skip_if_not_installed("ragg")
  n <- 200L
  viewport$dpi <- n * 25.4 / 100
  compiled <- ggplot3d:::scene_compile(split_quad(c("red", "blue")), ref_cameras()$pole, viewport)
  read <- ragg::agg_capture(width = n, height = n, res = viewport$dpi, background = "white")
  grob <- ggplot3d:::render_scene_grid(compiled, viewport)
  grid::pushViewport(grid::viewport())
  grid::grid.draw(grob)
  pixels <- read()
  grDevices::dev.off()
  expect_true(grob$capabilities$group_atop)
  xy <- ref_project(rbind(c(.7, .3, .5), c(.3, .7, .5)), ref_camera(ref_cameras()$pole, viewport))
  at <- cbind(n - floor(xy[, 2L] * n), 1L + floor(xy[, 1L] * n))
  colours <- grDevices::col2rgb(pixels[at])
  expect_lte(max(abs(colours - cbind(c(255, 128, 128), c(128, 128, 255)))), 1)
})

sheet_packet <- function(mode, fill = c("red", "red", "blue")) {
  packet <- ref_packet(faces = list(ref_quad(.3, x = c(.1, .6, .6, .1)), ref_quad(.4, x = c(.3, .8, .8, .3)),
    ref_quad(.7, x = c(.2, .9, .9, .2), y = c(.5, .5, .9, .9))), face_alpha = .5)
  packet$styles <- data.frame(style_id = 1:3, fill = fill, alpha_mode = mode, sheet = c(1L, 1L, 2L))
  packet$style_count <- 3L
  packet$faces$style_id <- 1:3
  packet
}

render_sheets <- function(packet, n = 200L) {
  viewport <- ggplot3d:::new_scene_viewport(100, 100, n * 25.4 / 100)
  camera <- ref_cameras()$pole
  read <- ragg::agg_capture(width = n, height = n, res = viewport$dpi, background = "white")
  on.exit(grDevices::dev.off())
  grob <- ggplot3d:::render_scene_grid(ggplot3d:::scene_compile(packet, camera, viewport), viewport)
  grid::pushViewport(grid::viewport())
  grid::grid.draw(grob)
  xy <- ref_project(rbind(c(.45, .3, .5), c(.45, .7, .5)), ref_camera(camera, viewport))
  at <- cbind(n - floor(xy[, 2L] * n), 1L + floor(xy[, 1L] * n))
  list(grob = grob, pixels = grDevices::col2rgb(read()[at]))
}

test_that("alpha_mode = \"surface\" paints each sheet once and composites sheets in depth order", {
  skip_if_not_installed("ragg")
  surface <- render_sheets(sheet_packet("surface"))
  paths <- surface$grob$children[[1L]]$children
  expect_length(paths, 2L)
  expect_true(all(vapply(paths, function(g) inherits(g, "pathgrob") && identical(g$gp$alpha, .5), logical(1))))
  expect_identical(vapply(paths, function(g) g$gp$fill, "", USE.NAMES = FALSE), c("red", "blue"))
  expect_lte(max(abs(surface$pixels - cbind(c(255, 128, 128), c(128, 64, 191)))), 2)
  face <- render_sheets(sheet_packet("face"))
  expect_lte(max(abs(face$pixels[, 1L] - c(255, 64, 64))), 2)
})

test_that("a sheet of several colours is isolated where the device has Porter-Duff groups", {
  packet <- sheet_packet("surface", c("red", "green", "blue"))
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 72)
  compiled <- ggplot3d:::scene_compile(packet, ref_cameras()$pole, viewport)
  render <- function(over) {
    testthat::local_mocked_bindings(grid_capabilities_3d = function()
      list(clipping_paths = TRUE, group_atop = over, group_over = over))
    ggplot3d:::render_scene_grid(compiled, viewport)$children[[1L]]$children
  }
  plain <- render(FALSE)
  expect_length(plain, 2L)
  expect_null(plain[[1L]]$vp)
  skip_if(getRversion() < "4.2.0")
  grouped <- render(TRUE)
  expect_false(is.null(grouped[[1L]]$vp$mask))
  expect_s3_class(grouped[[1L]]$children[[1L]], "GridGroup")
  expect_s3_class(grouped[[2L]], "pathgrob")
  skip_if_not_installed("ragg")
  expect_lte(max(abs(render_sheets(packet)$pixels - cbind(c(128, 255, 128), c(64, 128, 191)))), 2)
})

test_that("surface layers take alpha_mode and label sheets by shared vertices", {
  expect_error(geom_surface_3d(alpha_mode = "film"), "alpha_mode")
  sheets <- ggplot3d:::face_sheets_3d(c(0, 1, 0, 1, 1, 0, 5, 6, 5), c(0, 0, 1, 0, 1, 1, 5, 5, 6),
    rep(0, 9), rep(1:3, each = 3L))
  expect_identical(sheets, c(1L, 1L, 3L))
  v <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
  v$value <- with(v, sqrt(x^2 + y^2 + z^2))
  p <- ggplot2::ggplot(v, ggplot2::aes(x, y, z = z, value = value)) +
    stat_isosurface_3d(levels = c(0.4, 0.8), alpha = 0.4, alpha_mode = "surface") + coord_3d()
  packet <- stats_scene(p)$packet
  faces <- packet$styles[packet$faces$style_id, , drop = FALSE]
  expect_true(all(faces$alpha_mode == "surface"))
  expect_length(unique(faces$sheet), 2L)
})
