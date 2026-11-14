sphere_mesh <- function(n = 30, method = "cubes") {
  axes <- rep(list(seq(-1.3, 1.3, length.out = n)), 3)
  g <- expand.grid(x = axes[[1]], y = axes[[2]], z = axes[[3]])
  out <- geometry_isosurface_cpp(axes[[1]], axes[[2]], axes[[3]], with(g, x^2 + y^2 + z^2), 1, FALSE, method)
  out[c("x", "y", "z", "nx", "ny", "nz", "level", ".face", ".corner", ".vertex")]
}

grid_mesh <- function(f, nu, nv, closed = TRUE) {
  u <- seq(0, 2 * pi, length.out = nu + closed)[seq_len(nu)]
  v <- seq(0, 2 * pi, length.out = nv + closed)[seq_len(nv)]
  g <- expand.grid(i = seq_len(nu), j = seq_len(nv)); p <- f(u[g$i], v[g$j])
  cells <- expand.grid(i = seq_len(nu - !closed), j = seq_len(nv - !closed))
  id <- function(i, j) ((i - 1) %% nu) + 1 + nu * ((j - 1) %% nv)
  q <- cbind(id(cells$i, cells$j), id(cells$i + 1, cells$j), id(cells$i + 1, cells$j + 1), id(cells$i, cells$j + 1))
  mesh_3d(data.frame(x = p$x, y = p$y, z = p$z), q)
}

# Every edge in exactly two faces, traversed once in each direction.
closed_oriented <- function(m) {
  f <- matrix(m$.vertex[order(m$.face, m$.corner)], ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  !anyDuplicated(paste(e[, 1], e[, 2])) && all(paste(e[, 2], e[, 1]) %in% paste(e[, 1], e[, 2]))
}

euler <- function(m) {
  f <- matrix(m$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  length(unique(m$.vertex)) - length(unique(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))) + nrow(f)
}

radius <- function(m) sqrt(m$x^2 + m$y^2 + m$z^2)

test_that("non-manifold and non-orientable input is refused, inconsistent winding repaired", {
  v <- data.frame(x = c(0, 1, 0, 0, 0), y = c(0, 0, 1, -1, 0), z = c(0, 0, 0, 0, 1))
  fan <- mesh_3d(v, rbind(c(1, 2, 3), c(2, 1, 4), c(1, 2, 5)))
  expect_error(mesh_subdivide(fan), "shared by more than two faces")
  u <- seq(0, 2 * pi, length.out = 25)[-25]; w <- rep(c(-0.5, 0.5), each = 24); u <- rep(u, 2)
  strip <- data.frame(x = (1 + w * cos(u / 2)) * cos(u), y = (1 + w * cos(u / 2)) * sin(u), z = w * sin(u / 2))
  quads <- cbind(1:23, 2:24, 26:48, 25:47)
  expect_s3_class(mesh_normals(mesh_3d(strip, quads)), "ggplot3d_mesh")
  expect_error(mesh_curvature(mesh_3d(strip, rbind(quads, c(24, 25, 1, 48)))), "not orientable")
  cube <- read_mesh_fixture("cube.obj")
  expect_warning(out <- mesh_subdivide(cube), "Reversed the winding of 2 triangles")
  expect_true(closed_oriented(out))
})

test_that("a vertex where two surfaces touch is split into one vertex per fan", {
  v <- data.frame(x = c(0, 1, 0, 0, -1, 0, 0), y = c(0, 0, 1, 0, 0, -1, 0), z = c(0, 0, 0, 1, 0, 0, -1))
  tetra <- function(a, b, c, d) rbind(c(a, c, b), c(a, b, d), c(a, d, c), c(b, c, d))
  bowtie <- mesh_3d(v, rbind(tetra(1, 2, 3, 4), tetra(1, 5, 6, 7)))
  out <- mesh_subdivide(bowtie, scheme = "midpoint")
  expect_equal(max(out$.face), 32L)
  expect_equal(length(unique(out$.vertex)), 2L * 10L)
  expect_true(closed_oriented(out))
  expect_equal(euler(out), 4L)
})

test_that("normals split at creases and are angle weighted", {
  cube <- read_mesh_fixture("cube-ascii.ply")
  sharp <- mesh_normals(cube, crease = 30)
  axis <- abs(cbind(sharp$nx, sharp$ny, sharp$nz))
  expect_true(all(abs(rowSums(axis) - 1) < 1e-12))
  smooth <- mesh_normals(cube, crease = 180)
  expect_equal(abs(smooth$nx), rep(1 / sqrt(3), nrow(cube)), tolerance = 1e-12)
  expect_equal(names(smooth), c(names(cube), "nx", "ny", "nz"))
  s <- mesh_normals(sphere_mesh())
  expect_gt(min((s$nx * s$x + s$ny * s$y + s$nz * s$z) / radius(s)), 0.999)
})

test_that("curvature matches the analytic torus and is NA on the boundary", {
  R <- 1; r <- 0.4
  torus <- grid_mesh(function(u, v) list(x = (R + r * cos(v)) * cos(u), y = (R + r * cos(v)) * sin(u), z = r * sin(v)), 120, 48)
  cu <- mesh_curvature(torus)
  v <- atan2(cu$z, sqrt(cu$x^2 + cu$y^2) - R)
  K <- cos(v) / (r * (R + r * cos(v))); H <- -(R + 2 * r * cos(v)) / (2 * r * (R + r * cos(v)))
  expect_lt(sqrt(mean((cu$gaussian_curvature - K)^2)) / sqrt(mean(K^2)), 0.01)
  expect_lt(sqrt(mean((cu$mean_curvature - H)^2)), 0.01)
  patch <- mesh_curvature(mesh_3d(data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0), matrix(1:4, 1)))
  expect_true(all(is.na(patch$gaussian_curvature)))
  ball <- mesh_curvature(mesh_remesh(sphere_mesh(), 0.12))
  expect_equal(mean(ball$mean_curvature), -1, tolerance = 0.01)
  expect_equal(mean(ball$gaussian_curvature), 1, tolerance = 0.02)
})

test_that("Loop subdivision quadruples faces and keeps a closed surface closed", {
  cube <- read_mesh_fixture("cube-ascii.ply")
  loop <- mesh_subdivide(cube, 2)
  expect_equal(max(loop$.face), 12L * 16L)
  expect_true(closed_oriented(loop))
  expect_equal(euler(loop), 2L)
  expect_true(all(loop$x > 0 & loop$x < 1))
  mid <- mesh_subdivide(cube, 1, scheme = "midpoint")
  on_face <- with(mid, x %in% 0:1 | y %in% 0:1 | z %in% 0:1)
  expect_true(all(on_face))
  expect_equal(unique(mid$vertex_colour[mid$x == 1 & mid$y == 0 & mid$z == 0]), "#00FF00")
  expect_error(mesh_subdivide(cube, -1), "integer of at least 0")
})

test_that("Taubin smoothing removes noise without shrinking; boundaries stay", {
  s <- sphere_mesh(); set.seed(1)
  eps <- rnorm(max(s$.vertex), 0, 0.02)[s$.vertex] / radius(s)
  s$x <- s$x * (1 + eps); s$y <- s$y * (1 + eps); s$z <- s$z * (1 + eps)
  taubin <- mesh_smooth(s, 20)
  laplace <- mesh_smooth(s, 20, lambda = 0.5, mu = -1e-9)
  expect_lt(sd(radius(taubin)), sd(radius(s)) / 2)
  expect_lt(abs(mean(radius(taubin)) - 1), 0.002)
  expect_gt(1 - mean(radius(laplace)), 0.005)
  sheet <- grid_mesh(function(u, v) list(x = u, y = v, z = sin(3 * u) * cos(2 * v)), 12, 12, closed = FALSE)
  flat <- mesh_smooth(sheet, 5)
  key <- function(m) paste(m$x, m$y, m$z)
  edge <- sheet$x %in% range(sheet$x) | sheet$y %in% range(sheet$y)
  expect_true(all(key(sheet)[edge] %in% key(flat)))
  expect_false(all(key(sheet)[!edge] %in% key(flat)))
})

test_that("QEM decimation reaches its target, keeps the surface and its attributes", {
  for (method in c("cubes", "tetra", "dual", "nets")) {
    s <- sphere_mesh(method = method)
    d <- mesh_decimate(s, 0.1)
    expect_equal(max(d$.face), round(0.1 * max(s$.face)), tolerance = 2 / max(d$.face))
    expect_true(closed_oriented(d))
    expect_equal(euler(d), 2L)
    expect_lt(max(abs(radius(d) - 1)), 0.01)
    expect_equal(unique(d$level), 1)
    expect_gt(min((d$nx * d$x + d$ny * d$y + d$nz * d$z) / radius(d)), 0.9)
  }
  s <- sphere_mesh()
  expect_gt(max(mesh_decimate(s, 0.01, max_error = 1e-3)$.face), 0.01 * max(s$.face))
  expect_equal(max(mesh_decimate(s, 300)$.face), 300L, tolerance = 0.01)
  sheet <- grid_mesh(function(u, v) list(x = u, y = v, z = 0.1 * sin(u)), 20, 20, closed = FALSE)
  d <- mesh_decimate(sheet, 0.2)
  expect_equal(range(d$x), range(sheet$x)); expect_equal(range(d$y), range(sheet$y))
})

test_that("remeshing equalises edge lengths on the input surface", {
  s <- sphere_mesh(40)
  out <- mesh_remesh(s, 0.15)
  f <- matrix(out$.vertex, ncol = 3L, byrow = TRUE); p <- out[match(seq_len(max(out$.vertex)), out$.vertex), c("x", "y", "z")]
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)]); len <- sqrt(rowSums((p[e[, 1], ] - p[e[, 2], ])^2))
  expect_equal(mean(len), 0.15, tolerance = 0.1)
  expect_lt(sd(len) / mean(len), 0.2)
  expect_true(closed_oriented(out))
  expect_lt(max(abs(radius(out) - 1)), 0.005)
  expect_equal(range(out$level), c(1, 1))
})

test_that("mesh parameters of geom_mesh_3d and simplify on isosurface stats", {
  cube <- read_mesh_fixture("cube-ascii.ply")
  s <- stats_scene(ggplot2::ggplot() + geom_mesh_3d(data = cube, subdivide = 2, smooth = 2) + coord_3d())
  expect_length(s$packet$faces$primitive_id, 192L)
  p <- ggplot2::ggplot() + geom_mesh_3d(ggplot2::aes(fill = ggplot2::after_stat(mean_curvature)),
    data = cube, subdivide = 2, curvature = TRUE) + coord_3d()
  built <- ggplot2::layer_data(p)
  expect_true(all(c("gaussian_curvature", "mean_curvature") %in% names(built)))
  expect_length(unique(built$fill), length(unique(round(built$mean_curvature, 12))))
  d <- expand.grid(x = seq(-1, 1, length.out = 16), y = seq(-1, 1, length.out = 16), z = seq(-1, 1, length.out = 16))
  d$value <- with(d, x^2 + y^2 + z^2)
  full <- ggplot2::layer_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) + stat_isosurface_3d(levels = 0.6) + coord_3d())
  thin <- ggplot2::layer_data(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
    stat_isosurface_3d(levels = 0.6, simplify = 0.2) + coord_3d())
  expect_equal(max(thin$.face), round(0.2 * max(full$.face)), tolerance = 0.02)
  expect_equal(thin$z_mean, rep(colMeans(matrix(thin$z, 3L)), each = 3L))
  iso <- ggplot2::layer_data(ggplot2::ggplot() + stat_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
    levels = 0.5, n = 14, simplify = 100) + coord_3d())
  expect_true(max(iso$.face) %in% 99:100)
  expect_gt(min((iso$nx * iso$x + iso$ny * iso$y + iso$nz * iso$z) / radius(iso)), 0.9999)
})
