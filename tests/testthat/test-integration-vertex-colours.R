colour_faces_packet <- function(x, y, z, rgb) {
  n <- length(x)
  new_scene_packet(
    vertices = list(x = x, y = y, z = z, w = rep(1, n)),
    corners = list(vertex = seq_len(n), source_row = seq_len(n), red = rgb[, 1L], green = rgb[, 2L], blue = rgb[, 3L]),
    faces = list(offset = seq.int(1L, n, by = 3L), layer_id = rep(1L, n %/% 3L)),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = NA_character_, shade = FALSE))
}

# A horizontal triangle and a tilted one that passes through it.
crossing_packet <- function() {
  colour_faces_packet(x = c(0.1, 0.9, 0.5, 0.2, 0.8, 0.5), y = c(0.2, 0.2, 0.9, 0.5, 0.5, 0.1),
    z = c(0.5, 0.5, 0.5, 0.2, 0.2, 0.9), rgb = rbind(diag(3), diag(3)[c(2L, 3L, 1L), ]))
}

# Corner colours of a viewer payload, decoded from its float32 columns.
view_corner_colours <- function(view) {
  do.call(cbind, lapply(c("red", "green", "blue"), function(name) {
    bytes <- base64_decode_3d(view$corners[[name]]$b64)
    readBin(bytes, "double", size = 4L, n = length(bytes) %/% 4L, endian = "little")
  }))
}

top_view <- function() new_camera_3d(elevation = 90, azimuth = -90)

# Encoded colour at model point p of face `face` of `packet`: the corner
# colours interpolated in linear light (Gouraud over the decoded colours).
gouraud_colour <- function(packet, face, p) {
  corners <- packet$faces$offset[face] + 0:2
  P <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)[packet$corners$vertex[corners], ]
  C <- cbind(packet$corners$red, packet$corners$green, packet$corners$blue)[corners, ]
  b <- qr.solve(rbind(t(P), 1), c(p, 1))
  srgb_encode_3d(drop(b %*% srgb_decode_3d(C)))
}

# Encoded colours the vector backend paints at the vertices of the compiled
# face fragments, with the analytic colours at the same points of their
# source faces.
vector_vertex_colours <- function(packet, reference = packet, parent = NULL) {
  scene <- scene_compile(packet, top_view(), new_scene_viewport(60, 60))
  f <- scene$fragments
  rows <- which(f$kind == 1L)
  styles <- scene$styles[match(f$style_id[rows], scene$styles$style_id), , drop = FALSE]
  painted <- fragment_vertex_linear_3d(scene, rows, styles, scene_linear_attributes_3d(scene), NULL)
  index <- rep(f$offset[rows], f$count[rows]) + sequence(f$count[rows]) - 1L
  source <- f$source_index[rows][painted$owner]
  if (!is.null(parent)) source <- parent[source]
  expected <- t(vapply(seq_along(index), function(k) gouraud_colour(reference, source[k],
    c(scene$vertices$x[index[k]], scene$vertices$y[index[k]], scene$vertices$z[index[k]])), numeric(3)))
  list(painted = srgb_encode_3d(painted$linear), expected = expected, fragments = length(rows))
}

# Raster pixels at interior points of the visible face, away from face edges
# and from where the faces cross, against the analytic colour of that face. Faces are seen from above, so the visible face at a
# point is the one with the larger z there.
raster_matches_gouraud <- function(packet, reference = packet, keep = function(p) TRUE) {
  n <- 80L
  viewport <- new_scene_viewport(40, 40, dpi = n * 25.4 / 40)
  image <- raster_scene_3d(packet, top_view(), viewport, 1L)$image
  faces <- seq_along(reference$faces$offset)
  npc <- lapply(faces, function(face) {
    single <- reference
    single$faces <- lapply(reference$faces, function(column) column[face])
    s <- scene_compile(single, top_view(), viewport)
    list(screen = cbind(s$vertices$x_npc, s$vertices$y_npc), model = cbind(s$vertices$x, s$vertices$y, s$vertices$z))
  })
  values <- as.integer(image)
  worst <- 0; checked <- 0L
  for (row in seq(4L, n - 3L, by = 3L)) for (col in seq(4L, n - 3L, by = 3L)) {
    centre <- c((col - 0.5) / n, (n - row + 0.5) / n)
    hits <- lapply(faces, function(face) {
      b <- solve(rbind(t(npc[[face]]$screen), 1), c(centre, 1))
      if (all(b > -0.05)) list(face = face, inside = min(b), model = drop(b %*% npc[[face]]$model)) else NULL
    })
    hits <- Filter(Negate(is.null), hits)
    if (!length(hits)) next
    z <- vapply(hits, function(h) h$model[3L], numeric(1))
    if (length(z) > 1L && diff(sort(z, decreasing = TRUE)[1:2]) > -0.03) next
    front <- hits[[which.max(z)]]
    if (front$inside < 0.05 || !keep(front$model)) next
    v <- values[(row - 1L) * n + col]
    got <- vapply(c(0L, 8L, 16L), function(shift) bitwAnd(bitwShiftR(v, shift), 255L) / 255, numeric(1))
    worst <- max(worst, abs(got - gouraud_colour(reference, front$face, front$model)))
    checked <- checked + 1L
  }
  list(worst = worst, checked = checked)
}

test_that("faces split by the depth order keep the linear-light interpolation of their corner colours", {
  packet <- crossing_packet()
  vector <- vector_vertex_colours(packet)
  expect_gt(vector$fragments, 2L)
  expect_equal(unname(vector$painted), vector$expected, tolerance = 1e-9)
  raster <- raster_matches_gouraud(packet)
  expect_gt(raster$checked, 30L)
  expect_lte(raster$worst, 1.5 / 255)
})

test_that("clip planes keep the linear-light interpolation of corner colours in every backend", {
  packet <- crossing_packet()
  plane <- list(normal = c(1, 0, 0), offset = -0.45, cap = FALSE)
  clipped <- clip_packet_planes_3d(packet, list(plane))
  parent <- attr(clipped, "parent")$faces
  expect_gt(length(clipped$corners$vertex), length(packet$corners$vertex))
  stored <- cbind(clipped$corners$red, clipped$corners$green, clipped$corners$blue)
  owner <- rep(seq_along(clipped$faces$offset), clipped$faces$count)
  corner <- clipped$faces$offset[owner] + sequence(clipped$faces$count) - 1L
  expected <- t(vapply(seq_along(corner), function(k) {
    v <- clipped$corners$vertex[corner[k]]
    gouraud_colour(packet, parent[owner[k]], c(clipped$vertices$x[v], clipped$vertices$y[v], clipped$vertices$z[v]))
  }, numeric(3)))
  expect_equal(unname(stored[corner, ]), unname(expected), tolerance = 1e-12)
  view <- view_packet_3d(clipped)
  expect_equal(view_corner_colours(view)[corner, ], unname(expected), tolerance = 1e-6)
  vector <- vector_vertex_colours(clipped, packet, parent)
  expect_equal(unname(vector$painted), vector$expected, tolerance = 1e-9)
  raster <- raster_matches_gouraud(clipped, packet, keep = function(p) p[1L] > 0.47)
  expect_gt(raster$checked, 20L)
  expect_lte(raster$worst, 1.5 / 255)
})

test_that("munched faces keep the linear-light interpolation of corner colours", {
  packet <- colour_faces_packet(c(0.2, 0.8, 0.5), c(0.2, 0.2, 0.8), c(0.5, 0.5, 0.5), diag(3))
  out <- munch_packet_3d(coord_cylindrical_3d()$frame, packet)
  rgb <- srgb_decode_3d(cbind(out$corners$red, out$corners$green, out$corners$blue))
  expect_gt(length(out$corners$vertex), 3L)
  expect_equal(rowSums(rgb), rep(1, nrow(rgb)), tolerance = 1e-9)
})

gamut_cube <- function() {
  d <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  d$hex <- grDevices::rgb(d$x, d$y, d$z)
  d
}

test_that("hull vertices carry their identity fill and show it exactly when unlit", {
  d <- gamut_cube()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_hull_3d(ggplot2::aes(fill = I(hex), group = 1), shading = "smooth", shade = FALSE) + coord_3d()
  packet <- stats_scene(p)$packet
  expect_identical(length(packet$faces$offset), 12L)
  expected <- t(grDevices::col2rgb(d$hex[packet$corners$source_row])) / 255
  expect_equal(unname(cbind(packet$corners$red, packet$corners$green, packet$corners$blue)), unname(expected))
  scene <- scene_compile(packet, new_camera_3d(), new_scene_viewport(60, 60))
  f <- scene$fragments
  rows <- which(f$kind == 1L)
  styles <- scene$styles[match(f$style_id[rows], scene$styles$style_id), , drop = FALSE]
  painted <- fragment_vertex_linear_3d(scene, rows, styles, scene_linear_attributes_3d(scene), NULL)
  index <- rep(f$offset[rows], f$count[rows]) + sequence(f$count[rows]) - 1L
  vertices <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)
  row <- packet$corners$source_row[match(seq_len(nrow(vertices)), packet$corners$vertex)]
  at <- cbind(scene$vertices$x[index], scene$vertices$y[index], scene$vertices$z[index])
  nearest <- apply(at, 1L, function(v) which.min(colSums((t(vertices) - v)^2)))
  expect_identical(encode_rgb_3d(painted$linear), d$hex[row[nearest]])
  expect_equal(view_corner_colours(view_packet_3d(packet)), unname(expected), tolerance = 1e-6)
})

test_that("a hull split into one-point groups by a discrete fill warns", {
  d <- gamut_cube()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = I(hex))) + stat_hull_3d() + coord_3d()
  expect_warning(ggplot2::ggplot_build(p), "group = 1")
})
