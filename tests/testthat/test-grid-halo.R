halo_lines <- function(x, y, z, mask, count = c(2L, 2L)) {
  new_scene_packet(
    vertices = list(x = x, y = y, z = z),
    corners = list(vertex = seq_along(x)),
    polylines = list(offset = c(1L, 1L + count[1L]), width_mm = c(0.5, 0.5), mask_width_mm = c(mask, 0.5),
                     cap = c(1L, 1L), style_id = 1:2, layer_id = c(1L, 1L)),
    styles = data.frame(style_id = 1:2, colour = c("black", "red")))
}
halo_crossing <- function(mask) halo_lines(c(0.1, 0.9, 0.5, 0.5), c(0.5, 0.5, 0.1, 0.9), c(0.8, 0.8, 0.2, 0.2), mask)
halo_camera <- function() new_camera_3d(elevation = 90, azimuth = -90)
# Seen from above, the box centre (0.5, 0.5) is the panel centre.

# Red pixels of the column x = 0.5, with each row's signed offset in mm from
# y = 0.5, drawn by the vector backend.
grid_halo_column <- function(packet, n = 400L) {
  viewport <- new_scene_viewport(40, 40, dpi = n * 25.4 / 40)
  scene <- scene_compile(packet, halo_camera(), viewport)
  read <- ragg::agg_capture(width = n, height = n, res = viewport$dpi, background = "white")
  on.exit(grDevices::dev.off())
  grid::pushViewport(grid::viewport())
  grid::grid.draw(render_scene_grid(scene, viewport))
  rgb <- grDevices::col2rgb(read())
  red <- matrix(rgb[1L, ] > 200 & rgb[2L, ] < 80 & rgb[3L, ] < 80, n, n)
  col <- n %/% 2L
  y_mm <- (n - seq_len(n) + 0.5) / n * 40
  list(offset = y_mm - 20, red = red[, col] | red[, col + 1L])
}

test_that("the vector backend leaves a halo gap as wide as the mask", {
  skip_if_not_installed("ragg")
  gap <- function(mask) {
    column <- grid_halo_column(halo_crossing(mask))
    min(abs(column$offset[column$red]))
  }
  expect_equal(gap(0.5), 0.25, tolerance = 0.1 / 0.25)
  expect_equal(gap(3), 1.5, tolerance = 0.1 / 1.5)
  expect_equal(gap(5), 2.5, tolerance = 0.1 / 2.5)
})

joined_lines <- function(mask, z = 0.8) halo_lines(c(0.2, 0.5, 0.5, 0.5), c(0.5, 0.5, 0.5, 0.9), c(0.8, 0.8, z, 0.2), mask)

test_that("haloed lines through a common vertex do not cut each other", {
  pieces <- function(mask, z = 0.8) {
    f <- scene_compile(joined_lines(mask, z), halo_camera(), new_scene_viewport(40, 40))$fragments
    second <- f$parent_primitive_id == 2L
    c(sum(second), sum(f$mask_count[second]))
  }
  expect_equal(pieces(5), pieces(0.5))
  expect_false(identical(pieces(5, 0.79), pieces(0.5, 0.79)))
  skip_if_not_installed("ragg")
  near_end <- function(packet) {
    column <- grid_halo_column(packet)
    column$red[column$offset > 0.4 & column$offset < 2]
  }
  expect_true(all(near_end(joined_lines(5))))
  expect_false(any(near_end(joined_lines(5, 0.79))))
})

test_that("raster halos spare lines through a common vertex and the line's own segments", {
  viewport <- new_scene_viewport(40, 40, dpi = 40 * 25.4 / 40 * 4)
  red_near_end <- function(packet) {
    result <- raster_scene_3d(packet, halo_camera(), viewport, 1L)
    n <- dim(result$image)
    values <- matrix(as.integer(result$image), n[1L], n[2L], byrow = TRUE)
    red <- matrix(bitwAnd(values, 255L), n[1L], n[2L])
    alpha <- matrix(bitwAnd(bitwShiftR(values, 24L), 255L), n[1L], n[2L])
    col <- floor((0.5 - result$extent[1L]) / diff(result$extent[c(1L, 3L)]) * n[2L]) + 1
    y_mm <- (n[1L] - seq_len(n[1L]) + 0.5) / n[1L] * diff(result$extent[c(2L, 4L)]) * viewport$height_mm
    offset <- y_mm - 0.5 * viewport$height_mm
    band <- offset > 0.4 & offset < 2
    red[band, col] == 255L & alpha[band, col] == 255L
  }
  expect_true(all(red_near_end(joined_lines(5))))
  expect_false(any(red_near_end(joined_lines(5, 0.79))))
  bend <- new_scene_packet(
    vertices = list(x = c(0.2, 0.5, 0.5), y = c(0.5, 0.5, 0.9), z = c(0.8, 0.8, 0.2)),
    corners = list(vertex = 1:3),
    polylines = list(offset = 1L, count = 3L, width_mm = 0.5, mask_width_mm = 5, cap = 1L, style_id = 1L, layer_id = 1L),
    styles = data.frame(style_id = 1L, colour = "red"))
  expect_true(all(red_near_end(bend)))
})
