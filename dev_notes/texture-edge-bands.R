devtools::load_all()
source("tests/testthat/helper-stats-scene.R")
source("tests/testthat/helper-textures.R")
d <- expand.grid(x = seq(0, 1, length.out = 4L), y = seq(0, 1, length.out = 4L))
d$z <- d$x^2 + .2 * sin(d$y)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x + 2 * y)) +
  geom_surface_3d(texture = texture_3d(checker_colours(), interpolate = FALSE), shade = FALSE) +
  coord_3d(light = NULL)
plain <- stats_scene(p)$packet
banded <- stats_scene(p + ggplot2::scale_fill_steps(n.breaks = 4L))$packet
cat("TEXTURE_BAND_FACES", length(plain$faces$count), length(banded$faces$count), "\n")
stopifnot(length(banded$faces$count) > length(plain$faces$count))
count <- 0L
for (projection in c("orthographic", "perspective")) for (dpi in c(110, 300)) {
  camera <- new_camera_3d(projection, azimuth = -47, elevation = 33, zoom = 1.17)
  viewport <- new_scene_viewport(60, 40, dpi)
  a <- raster_scene_3d(plain, camera, viewport, 2L)$image
  b <- raster_scene_3d(banded, camera, viewport, 2L)$image
  stopifnot(identical(a, b))
  count <- count + 1L
}
cat("BYTE_IDENTICAL_TEXTURES_ACROSS_BAND_CUTS", count, "\n")
