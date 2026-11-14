halo_packet <- function(mask) {
  new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5, 0.5), y = c(0.5, 0.5, 0.1, 0.9), z = c(0.8, 0.8, 0.2, 0.2)),
    corners = list(vertex = 1:4),
    polylines = list(offset = c(1L, 3L), width_mm = c(0.5, 0.5), mask_width_mm = c(mask, 0.5), style_id = 1:2,
                     layer_id = c(1L, 1L)),
    styles = data.frame(style_id = 1:2, colour = c("black", "red")))
}

test_that("the raster backend cuts farther strokes in a nearer stroke's halo", {
  viewport <- new_scene_viewport(40, 40, dpi = 40 * 25.4 / 40 * 4)
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  alpha_along <- function(mask) {
    result <- raster_scene_3d(halo_packet(mask), camera, viewport, 1L)
    scene <- scene_compile(halo_packet(mask), camera, viewport)
    n <- dim(result$image)
    values <- matrix(as.integer(result$image), n[1L], n[2L], byrow = TRUE)
    red <- matrix(bitwAnd(values, 255L), n[1L], n[2L])
    alpha <- matrix(bitwAnd(bitwShiftR(values, 24L), 255L), n[1L], n[2L])
    x <- scene$vertices$x_npc[3L]
    col <- floor((x - result$extent[1L]) / diff(result$extent[c(1L, 3L)]) * n[2L]) + 1
    rows <- seq_len(n[1L])
    y_mm <- (n[1L] - rows + 0.5) / n[1L] * diff(result$extent[c(2L, 4L)]) * viewport$height_mm
    crossing <- scene$vertices$y_npc[1L] * viewport$height_mm
    list(distance = abs(y_mm - crossing), red = red[, col], alpha = alpha[, col])
  }
  haloed <- alpha_along(3)
  plain <- alpha_along(0.5)
  band <- haloed$distance > 0.5 & haloed$distance < 1.3
  outside <- haloed$distance > 2 & haloed$distance < 5
  expect_true(all(plain$alpha[band] == 255L & plain$red[band] == 255L))
  expect_true(all(haloed$alpha[band] == 0L))
  expect_true(all(haloed$alpha[outside] == 255L & haloed$red[outside] == 255L))
  expect_true(any(haloed$distance < 0.2 & haloed$alpha == 255L & haloed$red == 0L))
})
