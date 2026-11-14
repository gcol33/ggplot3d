raster_result_3d <- function(plot) {
  find <- function(g) {
    if (!is.null(g$raster_result)) return(list(g$raster_result))
    unlist(lapply(as.list(g$children), find), recursive = FALSE)
  }
  find(drawn_panel_3d(plot))[[1L]]
}

image_channel_3d <- function(image, shift) {
  packed <- as.numeric(unclass(image))
  packed[is.na(packed)] <- -2^31
  packed[packed < 0] <- packed[packed < 0] + 2^32
  floor(packed / 2^shift) %% 256
}

edl_plot_3d <- function(backend, cue = NULL) {
  v <- expand.grid(x = seq(-1, 1, length.out = 30), y = seq(-1, 1, length.out = 30))
  v$z <- sin(3 * v$x) * cos(3 * v$y)
  ggplot2::ggplot(v, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "grey80", colour = NA, shade = FALSE, raster = TRUE) +
    coord_3d(backend = backend_3d(backend, dpi = 40), light = NULL, depth_cue = cue)
}

test_that("eye-dome lighting of depth_cue_3d() darkens rasterised pixels most at depth steps", {
  for (backend in c("raster", "hybrid")) {
    plain <- raster_result_3d(edl_plot_3d(backend))
    none <- raster_result_3d(edl_plot_3d(backend, depth_cue_3d(fog = NA)))
    lit <- raster_result_3d(edl_plot_3d(backend, depth_cue_3d(fog = NA, edl = 2)))
    expect_identical(none$image, plain$image)
    green <- function(r) image_channel_3d(r$image, 8)
    expect_identical(image_channel_3d(lit$image, 24), image_channel_3d(plain$image, 24))
    expect_true(all(green(lit) <= green(plain)))
    covered <- image_channel_3d(plain$image, 24) > 0
    loss <- (green(plain) - green(lit))[covered]
    expect_gt(max(loss), 4 * stats::median(loss))
  }
})

test_that("splats keep their layer's eye-dome strength under a depth cue", {
  set.seed(2)
  d <- data.frame(x = stats::runif(300), y = stats::runif(300), z = stats::runif(300))
  splats <- function(cue) ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_point_3d(render = "splat", edl = 0, size = 2, raster = TRUE) +
    coord_3d(backend = backend_3d("raster", dpi = 40), depth_cue = cue)
  expect_identical(raster_result_3d(splats(depth_cue_3d(fog = NA, edl = 3)))$image, raster_result_3d(splats(NULL))$image)
})
