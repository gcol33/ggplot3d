source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

viewer_sheet_packet <- function(mode) {
  packet <- ref_packet(faces = list(ref_quad(.3, x = c(.1, .6, .6, .1)), ref_quad(.4, x = c(.3, .8, .8, .3)),
    ref_quad(.7, x = c(.2, .9, .9, .2), y = c(.5, .5, .9, .9))), face_alpha = .5)
  packet$styles <- data.frame(style_id = 1:3, fill = "red", alpha_mode = mode, sheet = c(1L, 1L, 2L))
  packet$style_count <- 3L
  packet$faces$style_id <- 1:3
  packet
}

test_that("viewer packets carry alpha_mode and the sheet key of every face", {
  for (mode in c("surface", "face")) {
    packet <- viewer_sheet_packet(mode)
    view <- ggplot3d:::view_packet_3d(packet)
    styles <- packet$styles[match(packet$faces$style_id, packet$styles$style_id), , drop = FALSE]
    expected <- as.integer(ggplot3d:::surface_sheets_3d(styles, packet$faces$layer_id, packet$faces$opacity))
    expect_identical(decode_column(view$face_sheet), expected)
    expect_identical(vapply(view$materials, `[[`, "", "alpha_mode"), rep(mode, 3L))
  }
  expect_identical(decode_column(ggplot3d:::view_packet_3d(viewer_sheet_packet("surface"))$face_sheet), c(1L, 1L, 2L))
  expect_true(all(decode_column(ggplot3d:::view_packet_3d(viewer_sheet_packet("face"))$face_sheet) == 0L))
})

viewer_spheres_plot <- function(mode) {
  v <- expand.grid(x = seq(-1, 1, length.out = 14), y = seq(-1, 1, length.out = 14), z = seq(-1, 1, length.out = 14))
  v$value <- with(v, sqrt(x^2 + y^2 + z^2))
  ggplot2::ggplot(v, ggplot2::aes(x, y, z = z, value = value)) +
    stat_isosurface_3d(levels = c(0.45, 0.85), fill = "steelblue", alpha = 0.4, shade = FALSE, alpha_mode = mode) +
    coord_3d(azimuth = 0, elevation = 0, light = NULL, panes = character(), box = "none")
}

viewer_fold_plot <- function(mode) {
  d <- expand.grid(x = seq(0, 1, length.out = 40), y = seq(0, 1, length.out = 6))
  d$z <- 0.5 + 0.4 * sin(4 * pi * d$x)
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "red", alpha = 0.5, shade = FALSE, alpha_mode = mode) +
    coord_3d(azimuth = 0, elevation = 15, light = NULL, panes = character(), box = "none")
}

test_that("widget scenes label isosurface sheets as the static backends do", {
  skip_if_not_installed("htmlwidgets")
  scene <- view_3d(viewer_spheres_plot("surface"), panel = 1)$x$panels[[1L]]$scene
  sheet <- decode_column(scene$face_sheet)
  expect_length(sheet, length(decode_column(scene$faces$offset)))
  expect_setequal(unique(sheet), 1:2)
  expect_true(all(vapply(scene$materials, `[[`, "", "alpha_mode") == "surface"))
  scene <- view_3d(viewer_spheres_plot("face"), panel = 1)$x$panels[[1L]]$scene
  expect_true(all(decode_column(scene$face_sheet) == 0L))
})

test_that("a translucent sheet does not darken where it folds over itself in the viewer", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(
    view_3d(viewer_fold_plot("surface"), panel = 1, width = 200, height = 200, elementId = "surface"),
    view_3d(viewer_fold_plot("face"), panel = 1, width = 200, height = 200, elementId = "face")))
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, c("surface", "face"))
  tinted <- function(rgba) rgba[rgba[, 4L] > 0 & rgba[, 1L] > rgba[, 2L] + 40, , drop = FALSE]
  surface <- tinted(px$surface$rgba)
  face <- tinted(px$face$rgba)
  expect_gt(nrow(surface), 500)
  single <- apply(surface[, 1:3], 2, stats::median)
  expect_lt(mean(surface[, 2L] < single[2L] - 30), 0.01)
  expect_gt(sum(face[, 2L] < single[2L] - 30), 200)
})

test_that("nested sheets show through each other once each in the viewer", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(
    view_3d(viewer_spheres_plot("surface"), panel = 1, width = 200, height = 200, elementId = "surface"),
    view_3d(viewer_spheres_plot("face"), panel = 1, width = 200, height = 200, elementId = "face")))
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, c("surface", "face"))
  equator <- function(img) {
    red <- matrix(img$rgba[, 1L], img$w)
    tinted <- matrix(img$rgba[, 3L] > img$rgba[, 1L] + 15, img$w)
    row <- which.max(colSums(tinted))
    columns <- which(tinted[, row])
    c(centre = red[round(stats::median(columns)), row], rim = max(red[columns, row]))
  }
  surface <- equator(px$surface)
  face <- equator(px$face)
  expect_lt(surface[["centre"]], surface[["rim"]] - 10)
  expect_gt(surface[["rim"]], face[["rim"]] + 10)
  expect_gt(surface[["centre"]], face[["centre"]] + 10)
})
