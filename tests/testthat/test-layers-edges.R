edge_grid <- function(n = 5L, m = 4L) {
  d <- expand.grid(x = seq_len(n), y = seq_len(m))
  d$z <- sin(d$x) + cos(d$y)
  d
}

edge_plot <- function(d, layer, ...) {
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + layer +
    coord_3d(panes = character(), axes = character(), box = "none", ...)
}

# Segments of the packet's polylines with the given colour, as rows of two
# vertex positions rounded for comparison.
edge_segments <- function(packet, colour) {
  rgb <- grDevices::col2rgb(colour)
  style <- packet$styles$style_id[packet$styles$colour %in% grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)]
  lines <- which(packet$polylines$style_id %in% style)
  at <- packet$polylines$offset[lines]
  xyz <- function(k) round(cbind(packet$vertices$x, packet$vertices$y)[packet$corners$vertex[k], , drop = FALSE], 9)
  list(a = xyz(at), b = xyz(at + 1L), count = packet$polylines$count[lines])
}

test_that("edges_3d() strokes the grid lines of a surface without the diagonals", {
  d <- edge_grid()
  packet <- stats_scene(edge_plot(d, geom_surface_3d(edges = edges_3d(colour = "red"))))$packet
  s <- edge_segments(packet, "red")
  expect_true(all(s$count == 2L))
  expect_equal(length(s$count), 4L * 4L + 5L * 3L)
  expect_true(all(s$a[, 1L] == s$b[, 1L] | s$a[, 2L] == s$b[, 2L]))
  triangles <- stats_scene(edge_plot(d, geom_surface_3d(edges = edges_3d("triangles", colour = "red"))))$packet
  expect_equal(length(edge_segments(triangles, "red")$count), 4L * 4L + 5L * 3L + 4L * 3L)
})

test_that("every keeps every n-th grid line and the last", {
  d <- edge_grid(7L, 7L)
  s <- edge_segments(stats_scene(edge_plot(d, geom_surface_3d(edges = edges_3d(every = 3, colour = "red"))))$packet, "red")
  constant_x <- s$a[, 1L] == s$b[, 1L]
  x <- sort(unique(s$a[constant_x, 1L])); y <- sort(unique(s$a[!constant_x, 2L]))
  expect_length(x, 3L); expect_length(y, 3L)
  expect_equal(length(s$count), 2L * 3L * 6L)
  two <- edge_segments(stats_scene(edge_plot(d, geom_surface_3d(edges = edges_3d(every = c(1, 6), colour = "red"))))$packet, "red")
  expect_equal(length(two$count), 7L * 6L + 2L * 6L)
  delaunay <- edge_plot(d, geom_surface_3d(method = "delaunay", edges = edges_3d(every = 2)))
  expect_error(stats_scene(delaunay), "grid")
})

test_that("a surface colour strokes the cell edges, as a mesh colour strokes its polygons", {
  d <- edge_grid()
  s <- edge_segments(stats_scene(edge_plot(d, geom_surface_3d(colour = "blue")))$packet, "blue")
  expect_equal(length(s$count), 4L * 4L + 5L * 3L)
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  mesh <- ggplot2::ggplot() + geom_mesh_3d(vertices = cube, faces = quads, edges = edges_3d(colour = "red")) +
    coord_3d(panes = character(), axes = character(), box = "none")
  expect_equal(length(edge_segments(stats_scene(mesh)$packet, "red")$count), 12L)
})

test_that("coord_3d() sets edges and outlines for every surface layer that sets none", {
  d <- edge_grid()
  inherited <- stats_scene(edge_plot(d, geom_surface_3d(), edges = edges_3d(colour = "red"),
    outline = outline_3d(silhouette = FALSE, crease = NULL, colour = "green")))$packet
  expect_equal(length(edge_segments(inherited, "red")$count), 4L * 4L + 5L * 3L)
  expect_gt(length(edge_segments(inherited, "green")$count), 0L)
  own <- stats_scene(edge_plot(d, geom_surface_3d(edges = edges_3d(colour = "blue"), outline = FALSE),
    edges = edges_3d(colour = "red"), outline = outline_3d(colour = "green")))$packet
  expect_equal(length(edge_segments(own, "red")$count), 0L)
  expect_equal(length(edge_segments(own, "blue")$count), 4L * 4L + 5L * 3L)
  expect_equal(length(edge_segments(own, "green")$count), 0L)
  expect_error(coord_3d(edges = outline_3d()), "edges_3d")
  expect_error(coord_3d(outline = edges_3d()), "outline_3d")
})

test_that("parametric surfaces carry their parameter grid as cells", {
  torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
  p <- ggplot2::ggplot() + geom_parametric_3d(fun = torus, n = c(12, 6), closed = TRUE, adaptive = FALSE,
    edges = edges_3d(colour = "red")) + coord_3d(panes = character(), axes = character(), box = "none")
  expect_equal(length(edge_segments(stats_scene(p)$packet, "red")$count), 2L * 12L * 6L)
})

test_that("edges_3d() checks its arguments", {
  expect_error(edges_3d(every = 0), "every")
  expect_error(edges_3d(every = c(1, 2, 3)), "every")
  expect_error(edges_3d(colour = c("red", "blue")), "colour")
  expect_error(edges_3d(alpha = 2), "alpha")
  expect_identical(edges_3d(every = 2)$every, c(2L, 2L))
})
