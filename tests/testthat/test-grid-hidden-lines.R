source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

hidden_line_camera <- function() new_camera_3d(elevation = 90, azimuth = -90)

# The compiled scene and the pixel colours of the vector backend's drawing, as
# an n x n matrix indexed [row, column], row 1 at the top.
hidden_line_pixels <- function(packet, n = 300L, camera = hidden_line_camera()) {
  viewport <- new_scene_viewport(40, 40, dpi = n * 25.4 / 40)
  scene <- scene_compile(packet, camera, viewport, centreline_strokes = TRUE)
  read <- ragg::agg_capture(width = n, height = n, res = viewport$dpi, background = "white")
  on.exit(grDevices::dev.off())
  grid::grid.draw(render_scene_grid(scene, viewport))
  list(scene = scene, pixels = matrix(read(), n, n))
}
is_colour <- function(pixels, colour, tolerance = 40) {
  rgb <- grDevices::col2rgb(pixels); target <- grDevices::col2rgb(colour)
  matrix(colSums(abs(rgb - as.vector(target))) <= tolerance, nrow(pixels), ncol(pixels))
}

# A floor, an opaque or translucent square above it and a thick black line
# between the two that passes under the square at an angle.
behind_square_packet <- function(alpha = 1, width = 3) {
  packet <- ref_packet(
    faces = list(ref_quad(.1), ref_quad(.8, x = c(.4, .6, .6, .4), y = c(.3, .3, .7, .7))),
    lines = list(cbind(c(.1, .9), c(.2, .8), c(.4, .4))), line_width = width)
  packet$faces$opacity <- c(1, alpha)
  packet$faces$style_id <- 2:3
  packet$polylines$style_id <- 1L
  packet$styles <- data.frame(style_id = 1:3, colour = c("black", NA, NA), fill = c(NA, "grey80", "blue"))
  packet$style_count <- 3L
  packet
}

test_that("a line between faces loses the parts an opaque face hides, whatever its hidden policy", {
  packet <- behind_square_packet()
  scene <- scene_compile(packet, hidden_line_camera(), new_scene_viewport(40, 40), centreline_strokes = TRUE)
  line <- scene$fragments$kind == 2L
  expect_false(any(scene$fragments$hidden[line]))
  # The visible pieces end at the square's edges x = 0.4 and x = 0.6, so the
  # covered centreline from x = 0.4 to 0.6 is gone.
  x <- unlist(lapply(which(line), function(i) {
    scene$vertices$x[scene$fragments$offset[i] + seq_len(scene$fragments$count[i]) - 1L]
  }))
  expect_false(any(x > .4 + 1e-9 & x < .6 - 1e-9))
  expect_true(all(scene$occluders$kind == 1L))
  expect_true(all(scene$fragments$kind[scene$occluders$fragment] == 2L))
  expect_true(all(scene$occluders$source_index == 2L))
})

test_that("a thick line drawn after the floor it crosses ends under the opaque face it passes behind", {
  skip_if_not_installed("ragg")
  drawn <- hidden_line_pixels(behind_square_packet())
  pixels <- drawn$pixels; scene <- drawn$scene; f <- scene$fragments
  n <- nrow(pixels)
  # The square's screen box, two pixels inside its edges.
  face <- which(f$kind == 1L & f$source_index == 2L)
  at <- unlist(lapply(face, function(i) f$offset[i] + seq_len(f$count[i]) - 1L))
  column <- range(scene$vertices$x_npc[at]) * n; row <- (1 - rev(range(scene$vertices$y_npc[at]))) * n
  square <- matrix(FALSE, n, n)
  square[ceiling(row[1L] + 2):floor(row[2L] - 2), ceiling(column[1L] + 2):floor(column[2L] - 2)] <- TRUE
  black <- is_colour(pixels, "black", 120)
  expect_false(any(black & square))
  expect_true(all(is_colour(pixels, "blue")[square]))
  expect_gt(sum(black & !square), 0)
})

test_that("strokes on an opaque surface are drawn after all its faces in one run", {
  faces <- ref_surface(function(x, y) 0.3 * exp(-2 * (x^2 + y^2)), n = 8L)
  edges <- lapply(faces, function(f) f[c(1L, 2L, 3L, 1L), , drop = FALSE])
  packet <- ref_packet(faces = faces, lines = edges, line_width = 0.4)
  packet$faces$style_id <- rep(2L, length(faces))
  packet$polylines$style_id <- rep(1L, length(edges))
  packet$styles <- data.frame(style_id = 1:2, colour = c("black", NA), fill = c(NA, "grey80"))
  packet$style_count <- 2L
  viewport <- new_scene_viewport(60, 60)
  scene <- scene_compile(packet, new_camera_3d(azimuth = 30, elevation = 35), viewport, centreline_strokes = TRUE)
  g <- render_scene_grid(scene, viewport)
  kinds <- vapply(g$fragment_runs, function(rows) scene$fragments$kind[rows[1L]], integer(1))
  expect_equal(sum(kinds == 2L), 1L)
  expect_equal(kinds[length(kinds)], 2L)
  expect_true(all(scene$fragments$mask_count[g$fragment_runs[[length(kinds)]]] == 0L))
})

test_that("a line under a translucent face stays before it in the draw order", {
  packet <- behind_square_packet(alpha = 0.5)
  viewport <- new_scene_viewport(40, 40)
  scene <- scene_compile(packet, hidden_line_camera(), viewport, centreline_strokes = TRUE)
  g <- render_scene_grid(scene, viewport)
  first <- vapply(g$fragment_runs, `[`, integer(1), 1L)
  kind <- scene$fragments$kind[first]; source <- scene$fragments$source_index[first]
  expect_lt(max(which(kind == 2L)), which(kind == 1L & source == 2L))
})
