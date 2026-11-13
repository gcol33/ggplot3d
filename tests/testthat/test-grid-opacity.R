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
