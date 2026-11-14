crossing_compile <- function(p) {
  scene <- integration_packets(p)
  list(packet = scene$packet, compiled = scene_compile(scene$packet, scene$params$camera, new_scene_viewport(100, 100, 144)))
}

test_that("four-dimensional projections are homogeneous with the eye and pole at infinity", {
  basis <- diag(4)[, 1:3]
  s4 <- projection_normal_3d(basis)[4]
  X <- rbind(c(1, 2, 0, 0.5 * s4), c(1, 0, 0, 2 * s4), c(0.5, 0, 0, 3 * s4))
  H <- project_nd_3d(X, basis, "perspective", 2)
  expect_equal(H[, 4], c(1.5, 0, -1))
  expect_equal(unname(H[1, 1:3] / H[1, 4]), c(1, 2, 0) * 2 / 1.5)
  pole <- projection_normal_3d(basis)
  expect_equal(abs(pole), c(0, 0, 0, 1))
  expect_equal(unname(project_nd_3d(matrix(pole, 1), basis, "stereographic", 4)), matrix(0, 1, 4))
  expect_equal(project_nd_xyz_3d(X, basis, "perspective", 2)[2, ], project_nd_xyz_3d(X, basis, "perspective", 2)[1, ])
  expect_equal(linear_clip_segment_3d(c(0, 0, 0, 1), c(0, 0, 0, 3), matrix(c(0, 0, 0, -1, 2), 1)), matrix(c(0, 0.5), 1))
  clipped <- linear_clip_polygon_3d(rbind(c(0, 0, 0, 1), c(1, 0, 0, 3), c(0, 1, 0, 3)), diag(3), matrix(c(0, 0, 0, -1, 2), 1))
  expect_equal(nrow(clipped$points), 3L)
  expect_equal(clipped$points[, 4], c(1, 2, 2))
  expect_equal(rowSums(clipped$weights), rep(1, 3))
})

test_that("perspective edges crossing the eye hyperplane end in explicit ideal corners", {
  s4 <- projection_normal_3d(diag(4)[, 1:3])[4]
  d <- data.frame(x = c(-0.5, 0.5), y = c(0.2, 0.2), z = 0, w = s4 * c(0, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, w = w)) +
    stat_project_3d(geom = "path", projection = "perspective", eye = 2) +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0), ggplot2::aes(x, y, z = z), inherit.aes = FALSE) +
    coord_3d(xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), expand = FALSE)
  out <- crossing_compile(p)
  w <- out$packet$vertices$w
  expect_true(any(w == 0))
  expect_true(all(w >= 0))
  v <- out$compiled$vertices
  expect_true(all(is.finite(v$x)))
  expect_equal(max(v$y), 1, tolerance = 1e-9)
  expect_equal(max(v$x), 0.75, tolerance = 1e-9)
  dots <- ggplot2::ggplot(data.frame(x = c(0.2, 0.3), y = 0, z = 0, w = s4 * c(1, 5)), ggplot2::aes(x, y, z = z, w = w)) +
    stat_project_3d(projection = "perspective", eye = 2) + coord_3d(xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1))
  expect_equal(length(integration_packets(dots)$packet$markers$offset), 1L)
})

test_that("a tesseract around the perspective eye renders with homogeneous clipping", {
  source(testthat::test_path("..", "fixtures", "scenes", "polytopes4d.R"), local = TRUE)
  m <- tesseract_mesh_4d()
  p <- ggplot2::ggplot(m, ggplot2::aes(w = w)) + stat_project_3d(geom = "mesh", projection = "perspective", eye = 0.5, alpha = 0.4) +
    coord_3d(xlim = c(-3, 3), ylim = c(-3, 3), zlim = c(-3, 3), expand = FALSE)
  out <- crossing_compile(p)
  expect_true(any(out$packet$vertices$w == 0))
  expect_gt(length(out$compiled$draw_order), 0L)
  v <- out$compiled$vertices
  expect_true(all(v$x >= -1e-9 & v$x <= 1 + 1e-9 & v$y >= -1e-9 & v$y <= 1 + 1e-9 & v$z >= -1e-9 & v$z <= 1 + 1e-9))
  for (backend in c("vector", "raster")) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 240, 240, res = 72)
    tryCatch(expect_no_error(print(p + coord_3d(xlim = c(-3, 3), ylim = c(-3, 3), zlim = c(-3, 3), expand = FALSE,
      backend = backend_3d(backend, dpi = 72, antialias = 1)))), finally = grDevices::dev.off())
    expect_gt(file.info(file)$size, 1000)
    unlink(file)
  }
})

test_that("stereographic arcs and faces through the pole reach the panel box", {
  pole <- projection_normal_3d(diag(4)[, 1:3])[4]
  t <- seq(-0.9, 0.9, length.out = 7) * pi / 2
  d <- data.frame(x = sin(t), y = 0, z = 0, w = pole * cos(t))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, w = w)) + stat_project_3d(geom = "path", projection = "stereographic") +
    coord_3d(xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), expand = FALSE)
  packet <- integration_packets(p)$packet
  xyz <- scene_vertices_xyz_3d(packet$vertices)
  expect_gte(length(packet$polylines$offset), 2L)
  expect_equal(range(xyz[, 1]), c(0, 1), tolerance = 1e-9)
  expect_true(all(abs(xyz[, 2:3] - 0.5) < 1e-9))
  lift <- function(x, y) c(2 * x, 2 * y, 0, pole * (x^2 + y^2 - 1)) / (1 + x^2 + y^2)
  face <- as.data.frame(rbind(lift(2.5, 0), lift(0, 0.5), lift(-2.5, 0))); names(face) <- c("x", "y", "z", "w")
  q <- ggplot2::ggplot(face, ggplot2::aes(x, y, z = z, w = w)) + stat_project_3d(geom = "polygon", projection = "stereographic", fill = "steelblue") +
    coord_3d(xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), expand = FALSE)
  out <- crossing_compile(q)
  expect_true(all(out$packet$vertices$w > 0))
  expect_equal(range(out$compiled$vertices$x), c(0, 1), tolerance = 1e-9)
  expect_equal(range(out$compiled$vertices$y), c(0.625, 1), tolerance = 1e-6)
})
