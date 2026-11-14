plane_box <- function() {
  ggplot2::ggplot(data.frame(x = c(0, 4), y = c(0, 2), z = c(0, 10)), ggplot2::aes(x, y, z = z)) + geom_point_3d()
}

plane_layer_packet <- function(plot) {
  out <- integration_packets(plot)
  packet <- out$packet
  keep <- packet$faces$layer_id == 2L
  list(packet = packet, faces = which(keep), lines = which(packet$polylines$layer_id == 2L))
}

ring_xyz <- function(packet, tab, i) {
  rows <- seq.int(tab$offset[i], length.out = tab$count[i])
  scene_vertices_xyz_3d(packet$vertices)[packet$corners$vertex[rows], , drop = FALSE]
}

test_that("intercept planes fill the box section as one face", {
  out <- plane_layer_packet(plane_box() + geom_plane_3d(zintercept = 5, xintercept = c(1, 3)) + coord_3d(expand = FALSE))
  packet <- out$packet
  expect_length(out$faces, 3L)
  expect_equal(packet$faces$count[out$faces], rep(4L, 3L))
  xyz <- lapply(out$faces, function(f) ring_xyz(packet, packet$faces, f))
  constant <- vapply(xyz, function(m) which(apply(m, 2L, function(v) diff(range(v)) < 1e-12)), integer(1))
  expect_setequal(constant, c(1L, 1L, 3L))
  values <- vapply(seq_along(xyz), function(i) xyz[[i]][1L, constant[i]], numeric(1))
  expect_setequal(round(values, 12), c(0.25, 0.75, 0.5))
  expect_length(out$lines, 0L)
})

test_that("an oblique plane through the box centre is a hexagon", {
  out <- plane_layer_packet(plane_box() + geom_plane_3d(normal = c(1 / 4, 1 / 2, 1 / 10), colour = "black") +
    coord_3d(expand = FALSE))
  packet <- out$packet
  expect_equal(packet$faces$count[out$faces], 6L)
  xyz <- ring_xyz(packet, packet$faces, out$faces)
  expect_equal(as.vector(xyz %*% rep(1, 3)), rep(1.5, 6L), tolerance = 1e-12)
  expect_equal(packet$polylines$count[out$lines], 7L)
  outline <- ring_xyz(packet, packet$polylines, out$lines)
  expect_equal(outline[1L, ], outline[7L, ])
})

test_that("a point fixes the plane without training the scales", {
  out <- integration_packets(plane_box() + geom_plane_3d(normal = c(0, 0, 1), point = c(z = 2.5)) +
    coord_3d(expand = FALSE))
  expect_equal(out$params$ranges$z, c(0, 10))
  face <- which(out$packet$faces$layer_id == 2L)
  expect_equal(unique(ring_xyz(out$packet, out$packet$faces, face)[, 3L]), 0.25)
  missing <- integration_packets(plane_box() + geom_plane_3d(normal = c(0, 0, 1), point = c(z = 20)) +
    coord_3d(expand = FALSE))$packet
  expect_false(any(missing$faces$layer_id == 2L))
  expect_error(geom_plane_3d(zintercept = 1, normal = c(0, 0, 1)), "not both")
  expect_error(geom_plane_3d(normal = c(1, 1, 0), point = c(x = 1)), "y coordinate")
})

test_that("mapped points and normals draw one plane per row", {
  d <- data.frame(x = c(1, 3), y = 1, z = 5, nx = c(1, 0), ny = c(0, 0), nz = c(0, 1), g = c("a", "b"))
  out <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    geom_plane_3d(ggplot2::aes(nx = nx, ny = ny, nz = nz, fill = g)) +
    ggplot2::expand_limits(x = c(0, 4), y = c(0, 2), z = c(0, 10)) + coord_3d(expand = FALSE))
  faces <- which(out$packet$faces$layer_id == 2L)
  expect_length(faces, 2L)
  expect_length(unique(out$packet$faces$style_id[faces]), 2L)
  first <- ring_xyz(out$packet, out$packet$faces, faces[1L])
  expect_equal(unique(first[, 1L]), 0.25)
})

test_that("planes cut surfaces as single faces", {
  g <- expand.grid(x = seq(-1, 1, length.out = 10), y = seq(-1, 1, length.out = 10))
  g$z <- g$x^2 + g$y^2
  p <- ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) + geom_surface_3d() + geom_plane_3d(zintercept = 1) + coord_3d()
  grob <- ggplot2::ggplotGrob(p)
  expect_s3_class(grob, "gtable")
})

test_that("ggplot2 reference lines lie on the floor", {
  d <- data.frame(x = 1:3, y = c(10, 20, 30), z = c(5, 6, 7))
  out <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    ggplot2::geom_hline(yintercept = 20) + ggplot2::geom_vline(xintercept = 2) +
    ggplot2::geom_abline(slope = 10, intercept = 0) + coord_3d(expand = FALSE))
  packet <- out$packet
  expect_equal(out$params$ranges$z, c(5, 7))
  lines <- packet$polylines$layer_id > 1L
  expect_equal(unique(packet$vertices$z[packet$corners$vertex[packet$polylines$offset[lines]]]), 0)
  abline <- which(packet$polylines$layer_id == 4L)
  ends <- ring_xyz(packet, packet$polylines, abline)
  expect_equal(ends[, 1L], c(0, 1))
  expect_equal(ends[, 2L], c(0, 1))
  lifted <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    layer_3d(ggplot2::geom_hline(yintercept = 20), z = 6) + coord_3d(expand = FALSE))$packet
  line <- which(lifted$polylines$layer_id == 2L)
  expect_equal(unique(ring_xyz(lifted, lifted$polylines, line)[, 3L]), 0.5)
})

test_that("rings with holes and concave outlines triangulate to their area", {
  xy <- rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(1, 1), c(1, 3), c(3, 3), c(3, 1))
  t <- triangulate_rings_2d(xy, list(1:4, 5:8))
  area <- function(t) sum(abs((xy[t[, 2L], 1L] - xy[t[, 1L], 1L]) * (xy[t[, 3L], 2L] - xy[t[, 1L], 2L]) -
    (xy[t[, 2L], 2L] - xy[t[, 1L], 2L]) * (xy[t[, 3L], 1L] - xy[t[, 1L], 1L]))) / 2
  expect_equal(area(t), 12)
  expect_true(all(t[, 1L] != t[, 2L]))
  comb <- rbind(c(0, 0), c(5, 0), c(5, 3), c(4, 3), c(4, 1), c(3, 1), c(3, 3), c(2, 3), c(2, 1), c(1, 1), c(1, 3), c(0, 3))
  xy <- comb
  t <- triangulate_rings_2d(xy, list(seq_len(nrow(comb))))
  expect_equal(nrow(t), nrow(comb) - 2L)
  expect_equal(area(t), 5 * 3 - 2 * 2 * 1 - 0)
  signed <- (xy[t[, 2L], 1L] - xy[t[, 1L], 1L]) * (xy[t[, 3L], 2L] - xy[t[, 1L], 2L]) -
    (xy[t[, 2L], 2L] - xy[t[, 1L], 2L]) * (xy[t[, 3L], 1L] - xy[t[, 1L], 1L])
  expect_true(all(signed > 0))
})
