outline_cube_test <- function(outline) {
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  ggplot2::ggplot() + geom_mesh_3d(vertices = cube, faces = quads, outline = outline) + coord_3d()
}

polyline_segments <- function(packet) sum(packet$polylines$count - 1L)

test_that("outline_3d validates its arguments", {
  expect_s3_class(outline_3d(), "ggplot3d_outline")
  expect_equal(outline_3d(silhouette = FALSE)$silhouette, "none")
  expect_error(outline_3d(silhouette = "fancy"), "silhouette")
  expect_error(outline_3d(crease = 200), "crease")
  expect_error(outline_3d(hidden = "faint"))
  expect_error(outline_3d(colour = c("red", "blue")), "single colour")
  expect_error(stats_scene(outline_cube_test(list(crease = 30))), "outline_3d")
})

test_that("a cube has twelve crease edges and a six-edge silhouette", {
  creases <- stats_scene(outline_cube_test(outline_3d(silhouette = FALSE, boundary = FALSE)))$packet
  expect_equal(polyline_segments(creases), 12L)
  silhouette <- stats_scene(outline_cube_test(outline_3d(silhouette = "mesh", crease = NULL, boundary = FALSE)))$packet
  expect_equal(polyline_segments(silhouette), 6L)
  expect_equal(length(silhouette$polylines$count), 1L)
  none <- stats_scene(outline_cube_test(outline_3d(silhouette = FALSE, crease = NULL, boundary = FALSE)))$packet
  expect_equal(length(none$polylines$count), 0L)
  dashed <- stats_scene(outline_cube_test(outline_3d(hidden = "dashed", colour = "red")))$packet
  expect_true(all(dashed$polylines$hidden == 1L))
  expect_true(all(dashed$styles$colour[dashed$polylines$style_id] == "#FF0000"))
})

test_that("interpolated contours of a sphere lie where the view grazes it", {
  sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
  m <- ggplot2::layer_data(ggplot2::ggplot() +
    stat_parametric_3d(fun = sphere, v = c(0, pi), n = c(40, 21), closed = c(TRUE, FALSE)) + coord_3d())
  ids <- match(m$.vertex, unique(m$.vertex)); rows <- match(seq_len(max(ids)), ids)
  p <- as.matrix(m[rows, c("x", "y", "z")])
  tri <- matrix(ids, ncol = 3L, byrow = TRUE)
  direction <- c(0.3, -0.5, 0.8) / sqrt(0.98)
  ortho <- outline_lines_cpp(p, p, p, tri, direction, FALSE, 2L, NA_real_, FALSE)
  q <- cbind(ortho$x, ortho$y, ortho$z)
  expect_lt(max(abs(q %*% direction)), 1e-12)
  expect_equal(length(unique(ortho$line)), 1L)
  expect_equal(q[1, ], q[nrow(q), ])
  eye <- 4 * direction
  persp <- outline_lines_cpp(p, p, p, tri, eye, TRUE, 2L, NA_real_, FALSE)
  q <- cbind(persp$x, persp$y, persp$z)
  expect_lt(max(abs(q %*% eye - 1)), 1e-12)
  mesh <- outline_lines_cpp(p, p, matrix(0, nrow(p), 0), tri, direction, FALSE, 1L, NA_real_, FALSE)
  expect_true(all(abs(sqrt(mesh$x^2 + mesh$y^2 + mesh$z^2) - 1) < 1e-12))
})

test_that("crease detection reverses faces wound against their neighbour", {
  square <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 1, 0))
  consistent <- outline_lines_cpp(square, square, matrix(0, 4, 0), rbind(c(1L, 2L, 3L), c(1L, 3L, 4L)), c(0, 0, 1), FALSE, 0L, cos(pi / 6), FALSE)
  flipped <- outline_lines_cpp(square, square, matrix(0, 4, 0), rbind(c(1L, 2L, 3L), c(1L, 4L, 3L)), c(0, 0, 1), FALSE, 0L, cos(pi / 6), FALSE)
  expect_length(consistent$x, 0L)
  expect_length(flipped$x, 0L)
  boundary <- outline_lines_cpp(square, square, matrix(0, 4, 0), rbind(c(1L, 2L, 3L), c(1L, 3L, 4L)), c(0, 0, 1), FALSE, 0L, NA_real_, TRUE)
  expect_equal(length(boundary$x), 5L)
})

test_that("surface layers draw outlines with their own hidden policy", {
  d <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
  d$z <- with(d, sin(x) * cos(y))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + coord_3d()
  plain <- stats_scene(base + geom_surface_3d())$packet
  outlined <- stats_scene(base + geom_surface_3d(outline = outline_3d(hidden = "show")))$packet
  expect_equal(length(plain$polylines$count), 0L)
  expect_gt(polyline_segments(outlined), 4L * 11L)
  expect_true(all(outlined$polylines$hidden == 2L))
  expect_equal(length(outlined$faces$count), length(plain$faces$count))
  torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
  ring <- stats_scene(ggplot2::ggplot() + geom_parametric_3d(fun = torus, n = c(24, 12), closed = TRUE,
    outline = outline_3d()) + coord_3d())$packet
  expect_gt(length(ring$polylines$count), 0L)
  expect_s3_class(ggplot2::ggplotGrob(ggplot2::ggplot() + geom_parametric_3d(fun = torus, n = c(24, 12), closed = TRUE,
    outline = outline_3d()) + coord_3d()), "gtable")
})

test_that("mesh outlines keep the mesh's corners", {
  packet <- stats_scene(outline_cube_test(outline_3d(silhouette = FALSE, boundary = FALSE)))$packet
  corners <- packet$corners$vertex[unlist(Map(function(o, n) o + seq_len(n) - 1L, packet$polylines$offset, packet$polylines$count))]
  xyz <- round(scene_vertices_xyz_3d(packet$vertices)[corners, , drop = FALSE], 9)
  expect_equal(nrow(unique(xyz)), 8L)
  ends <- matrix(seq_len(nrow(xyz)), ncol = 2L, byrow = TRUE)
  lengths <- sqrt(rowSums((xyz[ends[, 1L], ] - xyz[ends[, 2L], ])^2))
  expect_equal(lengths, rep(lengths[1L], 12L))
})

test_that("interpolated contours of a torus follow its analytic contour generator", {
  big <- 1; small <- 0.4
  torus_mesh <- function(nu, nv) {
    grid <- expand.grid(u = (seq_len(nu) - 0.5) * 2 * pi / nu, v = (seq_len(nv) - 0.25) * 2 * pi / nv)
    p <- cbind((big + small * cos(grid$v)) * cos(grid$u), (big + small * cos(grid$v)) * sin(grid$u), small * sin(grid$v))
    n <- cbind(cos(grid$v) * cos(grid$u), cos(grid$v) * sin(grid$u), sin(grid$v))
    id <- function(i, j) ((j - 1L) %% nv) * nu + ((i - 1L) %% nu) + 1L
    cells <- expand.grid(i = seq_len(nu), j = seq_len(nv))
    a <- id(cells$i, cells$j); b <- id(cells$i + 1L, cells$j); c <- id(cells$i + 1L, cells$j + 1L); d <- id(cells$i, cells$j + 1L)
    list(p = p, n = n, tri = rbind(cbind(a, b, c), cbind(a, c, d)))
  }
  direction <- c(0.2, -0.6, -0.5); direction <- direction / sqrt(sum(direction^2))
  residual <- function(nu, nv) {
    m <- torus_mesh(nu, nv)
    lines <- outline_lines_cpp(m$p, m$p, m$n, m$tri, direction, FALSE, 2L, NA_real_, FALSE)
    q <- cbind(lines$x, lines$y, lines$z)
    rho <- sqrt(q[, 1L]^2 + q[, 2L]^2); u <- atan2(q[, 2L], q[, 1L]); v <- atan2(q[, 3L], rho - big)
    normal <- cbind(cos(v) * cos(u), cos(v) * sin(u), sin(v))
    list(dot = max(abs(normal %*% direction)), surface = max(abs(sqrt((rho - big)^2 + q[, 3L]^2) - small)),
         loops = length(unique(lines$line)))
  }
  coarse <- residual(48L, 24L); fine <- residual(96L, 48L)
  expect_lt(fine$dot, 0.01)
  expect_lt(fine$dot, coarse$dot / 3)
  expect_lt(fine$surface, coarse$surface / 3)
  expect_equal(fine$loops, coarse$loops)
})

test_that("outline_3d(halo) widens the mask of the feature lines", {
  expect_equal(outline_3d()$halo, 0)
  expect_equal(outline_3d(halo = 0.75)$halo, 0.75)
  expect_equal(outline_3d(halo = grid::unit(2, "pt"))$halo, 2 * 25.4 / 72.27)
  expect_error(outline_3d(halo = -1), "halo")
  expect_error(outline_3d(halo = grid::unit(1, "npc")), "halo")
  plain <- stats_scene(outline_cube_test(outline_3d(linewidth = 0.5)))$packet
  expect_equal(plain$polylines$mask_width_mm, plain$polylines$width_mm)
  haloed <- stats_scene(outline_cube_test(outline_3d(linewidth = 0.5, halo = grid::unit(1, "mm"))))$packet
  expect_equal(haloed$polylines$mask_width_mm, haloed$polylines$width_mm + 2)
  expect_equal(length(haloed$faces$count), length(plain$faces$count))
})
