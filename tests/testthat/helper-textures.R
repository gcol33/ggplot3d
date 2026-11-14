# A square in the plane z = 0.5 of the unit box with texture coordinates
# (0, 0) .. (1, 1) at its corners, textured with `texture`.
texture_square_packet <- function(texture, fill = NA_character_, opacity = 1) {
  new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.9, 0.1), y = c(0.1, 0.1, 0.9, 0.9), z = rep(0.5, 4L)),
    corners = list(vertex = 1:4, source_row = rep(1L, 4L), u = c(0, 1, 1, 0), v = c(0, 0, 1, 1)),
    faces = list(offset = 1L, count = 4L, layer_id = 1L, opacity = opacity),
    styles = data.frame(style_id = 1L, fill = fill, colour = NA_character_, shade = FALSE,
      texture_id = 1L, texture = I(list(texture))))
}

# The same square cut into n x n flat faces coloured like the cells of a
# checkerboard `colours` (an n x n colour matrix, row one at the top).
checker_square_packet <- function(colours) {
  n <- nrow(colours)
  s <- seq(0.1, 0.9, length.out = n + 1L)
  cell <- expand.grid(i = seq_len(n), j = seq_len(n))
  x <- as.vector(rbind(s[cell$i], s[cell$i + 1L], s[cell$i + 1L], s[cell$i]))
  y <- as.vector(rbind(s[cell$j], s[cell$j], s[cell$j + 1L], s[cell$j + 1L]))
  fill <- colours[cbind(n - cell$j + 1L, cell$i)]
  palette <- unique(fill)
  new_scene_packet(
    vertices = list(x = x, y = y, z = rep(0.5, length(x))),
    corners = list(vertex = seq_along(x), source_row = rep(seq_len(nrow(cell)), each = 4L)),
    faces = list(offset = seq(1L, by = 4L, length.out = nrow(cell)), count = rep(4L, nrow(cell)),
      layer_id = rep(1L, nrow(cell)), style_id = match(fill, palette)),
    styles = data.frame(style_id = seq_along(palette), fill = palette, colour = NA_character_, shade = FALSE))
}

checker_colours <- function(n = 8L) {
  m <- matrix("white", n, n)
  m[(row(m) + col(m)) %% 2L == 0L] <- "black"
  m
}

# Encoded RGB channels (0-255) of an image as an h x w x 3 array: a raster
# result's nativeRaster or a PNG file.
image_channels <- function(x) {
  if (is.character(x)) return(round(png::readPNG(x)[, , 1:3] * 255))
  ch <- texture_channels_3d(x)
  out <- array(0, c(nrow(x), ncol(x), 3L))
  for (k in 1:3) out[, , k] <- matrix(ch[k, ], nrow(x), ncol(x), byrow = TRUE)
  out
}

# Draws a compiled scene with the vector backend into a PNG of
# `pixels` x `pixels` and returns its path.
grid_scene_png <- function(scene, viewport, pixels) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, pixels, pixels, res = viewport$dpi, background = "grey50")
  grid::grid.draw(render_scene_grid(scene, viewport))
  grDevices::dev.off()
  path
}

# Pixels whose 5 x 5 neighbourhood in the reference has one colour.
interior_pixels <- function(reference) {
  key <- reference[, , 1L] * 65536 + reference[, , 2L] * 256 + reference[, , 3L]
  h <- nrow(key); w <- ncol(key)
  same <- matrix(TRUE, h, w)
  for (dy in -2:2) for (dx in -2:2) {
    shifted <- matrix(NA_real_, h, w)
    rows <- max(1L, 1L + dy):min(h, h + dy); cols <- max(1L, 1L + dx):min(w, w + dx)
    shifted[rows - dy, cols - dx] <- key[rows, cols]
    same <- same & !is.na(shifted) & shifted == key
  }
  same
}

crossing_texture_packet <- function() {
  texture <- texture_3d(matrix("#40608080"), interpolate = FALSE)
  new_scene_packet(
    vertices = list(x = c(.1, .9, .9, .1, .1, .9, .9, .1, .3),
      y = c(.1, .1, .9, .9, .1, .1, .9, .9, .2), z = c(.2, .8, .8, .2, rep(.5, 4), .4)),
    corners = list(vertex = 1:9, u = c(rep(c(0, 1, 1, 0), 2), NA_real_),
      v = c(rep(c(0, 0, 1, 1), 2), NA_real_)),
    faces = list(offset = c(1L, 5L), count = c(4L, 4L), layer_id = c(1L, 2L), coverage_opaque = c(FALSE, FALSE)),
    markers = list(offset = 9L, style_id = 2L, footprint_id = 1L),
    footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 1, radius_y_mm = 1, angle = 0),
    styles = data.frame(style_id = 1:2, fill = NA_character_, colour = c(NA, "red"),
      shade = c(TRUE, FALSE), shape = 19, size = 2, linewidth = 0,
      texture_id = c(1L, NA_integer_), texture = I(list(texture, NULL))))
}

texture_viewer_plot <- function(reverse = FALSE) {
  layers <- list(annotation_raster_3d(matrix("#FF000080"), .1, .9, .1, .9, at = .75),
    annotation_raster_3d(matrix("#0000FF80"), .1, .9, .1, .9, at = .25))
  if (reverse) layers <- rev(layers)
  ggplot2::ggplot(data.frame(x = 0:1, y = 0:1, z = 0:1), ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_blank() + layers + coord_3d(elevation = 90, azimuth = -90, light = NULL) + ggplot2::theme_void()
}
