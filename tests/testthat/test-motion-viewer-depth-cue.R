fog_square_plot <- function(cue) {
  square <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0.5)
  ggplot2::ggplot(square, ggplot2::aes(x, y, z = z)) + geom_polygon_3d(fill = "black", shade = FALSE) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) +
    coord_3d(azimuth = -90, elevation = 90, expand = FALSE, light = NULL, panes = character(), box = "none",
      axes = character(), depth_cue = cue)
}

test_that("the viewer receives the depth cue settings", {
  skip_if_not_installed("htmlwidgets")
  cue <- view_3d(fog_square_plot(depth_cue_3d(fog = "white", amount = 0.7, linewidth = TRUE, edl = 2)), panel = 1)$x$panels[[1L]]$depth_cue
  expect_identical(cue$fog, c(1, 1, 1, 1))
  expect_identical(c(cue$amount, cue$near, cue$far, cue$linewidth, cue$edl), c(0.7, 0, 1, 0.5, 2))
  expect_null(view_3d(fog_square_plot(NULL), panel = 1)$x$panels[[1L]]$depth_cue)
})

test_that("viewer fog and static fog give the same colour at the same depth", {
  skip_if_not_installed("htmlwidgets")
  cue <- depth_cue_3d(fog = "white", amount = 1, near = 0, far = 1)
  scene <- drawn_panel_3d(fog_square_plot(cue))$scene
  f <- scene$fragments
  static <- scene$styles$fill[match(f$style_id[f$kind == 1L & f$layer_id > 0L], scene$styles$style_id)]
  expected <- ggplot3d:::fog_colours_3d("black", 0.5, "white")
  expect_true(all(static == expected))
  b <- viewer_browser(list(view_3d(fog_square_plot(cue), panel = 1, width = 200, height = 200, elementId = "fog")))
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, "fog")$fog
  centre <- px$rgba[(px$h %/% 2) * px$w + px$w %/% 2 + 1L, ]
  expect_lte(max(abs(centre[1:3] - grDevices::col2rgb(expected)[, 1L])), 2)
})

test_that("viewer eye-dome lighting darkens the panel at depth steps", {
  skip_if_not_installed("htmlwidgets")
  v <- expand.grid(x = seq(-1, 1, length.out = 30), y = seq(-1, 1, length.out = 30))
  v$z <- sin(3 * v$x) * cos(3 * v$y)
  wave <- function(cue) ggplot2::ggplot(v, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "grey80", colour = NA, shade = FALSE) +
    coord_3d(light = NULL, panes = character(), box = "none", depth_cue = cue)
  b <- viewer_browser(list(
    view_3d(wave(NULL), panel = 1, width = 200, height = 200, elementId = "plain"),
    view_3d(wave(depth_cue_3d(fog = NA, edl = 2)), panel = 1, width = 200, height = 200, elementId = "lit")))
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, c("plain", "lit"))
  expect_identical(px$lit$rgba[, 4L], px$plain$rgba[, 4L])
  loss <- px$plain$rgba[, 2L] - px$lit$rgba[, 2L]
  expect_true(all(loss >= -1))
  expect_gt(sum(loss > 20), 50L)
})
