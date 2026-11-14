reconstruct_topology <- function(b) {
  f <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  counts <- table(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))
  list(closed = all(counts == 2), oriented = !anyDuplicated(paste(e[, 1], e[, 2])),
       chi = length(unique(b$.vertex)) - length(counts) + nrow(f))
}

signed_volume <- function(b) {
  p <- as.matrix(b[c("x", "y", "z")]); i <- seq(1L, nrow(p), 3L)
  sum(p[i, 1] * (p[i + 1, 2] * p[i + 2, 3] - p[i + 1, 3] * p[i + 2, 2]) -
      p[i, 2] * (p[i + 1, 1] * p[i + 2, 3] - p[i + 1, 3] * p[i + 2, 1]) +
      p[i, 3] * (p[i + 1, 1] * p[i + 2, 2] - p[i + 1, 2] * p[i + 2, 1])) / 6
}

empty_circles <- function(xy, t) all(vapply(seq_len(nrow(t)), function(i) {
  p <- xy[t[i, ], ]
  a <- 2 * (p[2, ] - p[1, ]); b <- 2 * (p[3, ] - p[1, ])
  centre <- solve(rbind(a, b), c(sum(p[2, ]^2 - p[1, ]^2), sum(p[3, ]^2 - p[1, ]^2)))
  r2 <- sum((p[1, ] - centre)^2)
  !any(rowSums(sweep(xy[-t[i, ], , drop = FALSE], 2L, centre)^2) < r2 * (1 - 1e-9))
}, logical(1)))

test_that("native Delaunay triangulations are complete, counterclockwise and empty-circle", {
  set.seed(4)
  xy <- matrix(runif(600), ncol = 2)
  t <- ggplot3d:::geometry_delaunay_cpp(xy)
  expect_equal(nrow(t), 2 * nrow(xy) - 2 - length(grDevices::chull(xy)))
  area <- function(t, xy) (xy[t[, 2], 1] - xy[t[, 1], 1]) * (xy[t[, 3], 2] - xy[t[, 1], 2]) -
    (xy[t[, 2], 2] - xy[t[, 1], 2]) * (xy[t[, 3], 1] - xy[t[, 1], 1])
  expect_true(all(area(t, xy) > 0))
  expect_true(empty_circles(xy, t))
  # Cocircular lattice points: every unit square is split once.
  lattice <- as.matrix(expand.grid(1:9, 1:7))
  t <- ggplot3d:::geometry_delaunay_cpp(lattice)
  expect_equal(nrow(t), 2 * 8 * 6); expect_equal(sum(area(t, lattice)) / 2, 48)
  expect_true(empty_circles(lattice, t))
  # Points one ulp off a line, where floating-point orientation fails.
  u <- 2^-52
  near <- rbind(cbind(0.5 + (0:20) * u, 0.5 + (0:20) * u), c(12, 12), c(24, 24), c(0.5, 0.75))
  t <- ggplot3d:::geometry_delaunay_cpp(near)
  expect_gt(nrow(t), 0L)
  expect_true(all(area(t, near) >= 0))
  expect_equal(nrow(ggplot3d:::geometry_delaunay_cpp(cbind(1:5, 2 * (1:5)))), 0L)
})

test_that("Delaunay surfaces keep their hull triangles and reject collinear positions", {
  set.seed(8)
  d <- data.frame(x = runif(60), y = runif(60)); d$z <- d$x * d$y
  b <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(method = "delaunay") + coord_3d())$data[[1L]]
  expect_equal(max(b$.face), 2 * 60 - 2 - length(grDevices::chull(d$x, d$y)))
  line <- data.frame(x = 1:4, y = 1:4, z = 1)
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(line, ggplot2::aes(x, y, z = z)) + geom_surface_3d(method = "delaunay") + coord_3d()), "collinear")
})

test_that("native Delaunay tetrahedralisations fill the hull with empty spheres", {
  set.seed(5)
  xyz <- matrix(runif(900), ncol = 3)
  tet <- ggplot3d:::geometry_delaunay3_cpp(xyz)
  vol <- function(r) det(cbind(xyz[r[1], ] - xyz[r[4], ], xyz[r[2], ] - xyz[r[4], ], xyz[r[3], ] - xyz[r[4], ])) / 6
  v <- apply(tet, 1L, vol)
  expect_true(all(v > 0))
  hull <- ggplot3d:::geometry_hull_cpp(xyz)
  centre <- colMeans(xyz)
  hull_volume <- abs(sum(apply(hull, 1L, function(r) det(sweep(xyz[r, ], 2L, centre)) / 6)))
  expect_equal(sum(v), hull_volume, tolerance = 1e-10)
  spheres <- vapply(seq_len(nrow(tet)), function(i) {
    p <- xyz[tet[i, ], ]
    centre <- solve(2 * (p[-1, ] - matrix(p[1, ], 3, 3, byrow = TRUE)), rowSums(p[-1, ]^2) - sum(p[1, ]^2))
    !any(rowSums(sweep(xyz[-tet[i, ], ], 2L, centre)^2) < sum((p[1, ] - centre)^2) * (1 - 1e-9))
  }, logical(1))
  expect_true(all(spheres))
  cube <- as.matrix(expand.grid(0:3, 0:3, 0:3))
  tet <- ggplot3d:::geometry_delaunay3_cpp(cube)
  xyz <- cube
  expect_equal(sum(apply(tet, 1L, vol)), 27)
})

test_that("alpha shapes recover a sampled torus and approach the hull", {
  set.seed(1)
  u <- runif(1500, 0, 2 * pi); v <- runif(1500, 0, 2 * pi)
  torus <- data.frame(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
  build <- function(...) ggplot2::ggplot_build(ggplot2::ggplot(torus, ggplot2::aes(x, y, z = z)) + geom_alpha_shape_3d(...) + coord_3d())$data[[1L]]
  ring <- build(radius = 0.45)
  m <- reconstruct_topology(ring)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 0)
  expect_true(all(ring$solid))
  expect_gt(signed_volume(ring), 0)
  expect_true(all(ring$radius == 0.45))
  wide <- build(radius = Inf)
  m <- reconstruct_topology(wide)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 2)
  expect_equal(nrow(wide) / 3, nrow(ggplot3d:::geometry_hull_cpp(as.matrix(torus))))
  automatic <- build()
  expect_gt(nrow(automatic), 0L)
  expect_lt(automatic$radius[1L], 0.45)
  expect_true(all(c(1L, 1500L) %in% ring$.vertex) || length(unique(ring$.vertex)) > 1000L)
  expect_warning(build(radius = -1), "radius")
  s <- stats_scene(ggplot2::ggplot(torus, ggplot2::aes(x, y, z = z)) + geom_alpha_shape_3d(radius = 0.45, fill = "grey60") + coord_3d())
  expect_silent(validate_scene_packet(s$packet))
  expect_gt(length(s$packet$faces$primitive_id), 0L)
})

test_that("radial basis function fits reconstruct closed surfaces from points", {
  set.seed(2)
  s <- matrix(rnorm(900), ncol = 3); s <- s / sqrt(rowSums(s^2))
  ell <- data.frame(x = s[, 1], y = s[, 2], z = 0.6 * s[, 3])
  error <- function(b) abs(sqrt(b$x^2 + b$y^2 + (b$z / 0.6)^2) - 1)
  fit <- ggplot2::ggplot_build(ggplot2::ggplot(ell, ggplot2::aes(x, y, z = z)) + stat_implicit_fit_3d(n = 24) + coord_3d())$data[[1L]]
  m <- reconstruct_topology(fit)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 2)
  expect_lt(max(error(fit)), 0.015)
  expect_lt(mean(error(fit)), 0.003)
  expect_gt(signed_volume(fit), 0)
  expect_gt(mean(fit$nx * fit$x + fit$ny * fit$y + fit$nz * fit$z > 0), 0.99)
  ell[c("nx", "ny", "nz")] <- cbind(s[, 1], s[, 2], s[, 3] / 0.36)
  given <- ggplot2::ggplot_build(ggplot2::ggplot(ell, ggplot2::aes(x, y, z = z, nx = nx, ny = ny, nz = nz)) +
    stat_implicit_fit_3d(n = 24, extractor = "dual") + coord_3d())$data[[1L]]
  expect_true(reconstruct_topology(given)$oriented)
  expect_lt(max(error(given)), 0.02)
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(ell, ggplot2::aes(x, y, z = z)) + stat_implicit_fit_3d(k = 1) + coord_3d()), "k must")
  expect_error(stat_implicit_fit_3d(method = "spline"), "should be")
  big <- data.frame(x = runif(2001), y = runif(2001), z = runif(2001))
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(big, ggplot2::aes(x, y, z = z)) + stat_implicit_fit_3d() + coord_3d()), "2000")
})

test_that("native normal estimation orients normals outwards on closed surfaces", {
  set.seed(8)
  s <- matrix(rnorm(3000), ncol = 3); s <- s / sqrt(rowSums(s^2))
  n <- ggplot3d:::point_normals_3d(s, 10L)
  expect_equal(sqrt(rowSums(n^2)), rep(1, nrow(s)), tolerance = 1e-12)
  expect_gt(min(rowSums(n * s)), 0.98)
  u <- runif(2000, 0, 2 * pi); v <- runif(2000, 0, 2 * pi)
  t <- cbind((1 + 0.4 * cos(v)) * cos(u), (1 + 0.4 * cos(v)) * sin(u), 0.4 * sin(v))
  n <- ggplot3d:::point_normals_3d(t, 10L)
  expect_true(all(rowSums(n * cbind(cos(v) * cos(u), cos(v) * sin(u), sin(v))) > 0.9))
})

poisson_shape <- function(n, sd, seed, torus = FALSE) {
  set.seed(seed)
  if (torus) {
    u <- runif(n, 0, 2 * pi); v <- runif(n, 0, 2 * pi)
    p <- cbind((1 + 0.4 * cos(v)) * cos(u), (1 + 0.4 * cos(v)) * sin(u), 0.4 * sin(v))
  } else {
    p <- matrix(rnorm(3 * n), ncol = 3); p <- p / sqrt(rowSums(p^2))
  }
  p <- p + rnorm(3 * n, sd = sd)
  data.frame(x = p[, 1], y = p[, 2], z = p[, 3])
}
sphere_error <- function(b) abs(sqrt(b$x^2 + b$y^2 + b$z^2) - 1)
torus_error <- function(b) abs(sqrt((sqrt(b$x^2 + b$y^2) - 1)^2 + b$z^2) - 0.4)
fit_points <- function(d, ...) ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + stat_implicit_fit_3d(...) + coord_3d())$data[[1L]]

test_that("screened Poisson fits reconstruct closed, outward surfaces of noisy spheres and tori", {
  ball <- poisson_shape(4000, 0.003, 3)
  fit <- fit_points(ball, method = "poisson", depth = 6)
  m <- reconstruct_topology(fit)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 2)
  expect_lt(max(sphere_error(fit)), 0.015)
  expect_lt(mean(sphere_error(fit)), 0.003)
  expect_gt(signed_volume(fit), 0)
  expect_equal(mean(fit$nx * fit$x + fit$ny * fit$y + fit$nz * fit$z > 0), 1)

  ring <- poisson_shape(5000, 0.003, 5, torus = TRUE)
  fit <- fit_points(ring, method = "poisson", depth = 6)
  m <- reconstruct_topology(fit)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 0)
  expect_lt(max(torus_error(fit)), 0.025)
  expect_lt(mean(torus_error(fit)), 0.003)
  r <- sqrt(fit$x^2 + fit$y^2)
  expect_equal(mean(fit$nx * (fit$x - fit$x / r) + fit$ny * (fit$y - fit$y / r) + fit$nz * fit$z > 0), 1)

  # Screening pulls the surface onto the points: the unscreened fit
  # (Kazhdan, Bolitho and Hoppe 2006) of the same clean points is further off.
  clean <- poisson_shape(5000, 0, 5, torus = TRUE)
  screened <- mean(torus_error(fit_points(clean, method = "poisson", depth = 6)))
  plain <- fit_points(clean, method = "poisson", depth = 6, screening = 0)
  expect_true(reconstruct_topology(plain)$closed)
  expect_lt(screened, 0.7 * mean(torus_error(plain)))
})

test_that("screened Poisson and radial basis function fits agree at equal point counts", {
  for (torus in c(FALSE, TRUE)) {
    d <- poisson_shape(1000, 0.005, 11, torus)
    error <- if (torus) torus_error else sphere_error
    rbf <- fit_points(d, n = 32)
    poisson <- fit_points(d, method = "poisson", depth = 6)
    expect_true(reconstruct_topology(poisson)$closed)
    expect_lt(mean(error(poisson)), 1.25 * mean(error(rbf)))
    expect_lt(max(error(poisson)), 3 * max(error(rbf)))
  }
})

test_that("screened Poisson fits take normals from aesthetics and validate their parameters", {
  d <- poisson_shape(3000, 0, 13)
  d[c("nx", "ny", "nz")] <- d[c("x", "y", "z")]
  given <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, nx = nx, ny = ny, nz = nz)) +
    stat_implicit_fit_3d(method = "poisson", depth = 6, extractor = "dual") + coord_3d())$data[[1L]]
  expect_true(reconstruct_topology(given)$closed)
  expect_lt(mean(sphere_error(given)), 0.004)
  flipped <- d; flipped[c("nx", "ny", "nz")] <- -d[c("x", "y", "z")]
  inward <- ggplot2::ggplot_build(ggplot2::ggplot(flipped, ggplot2::aes(x, y, z = z, nx = nx, ny = ny, nz = nz)) +
    stat_implicit_fit_3d(method = "poisson", depth = 6) + coord_3d())$data[[1L]]
  expect_lt(signed_volume(inward), 0)
  build <- function(...) ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    stat_implicit_fit_3d(method = "poisson", ...) + coord_3d())
  expect_warning(build(depth = 11), "depth")
  expect_warning(build(screening = -1), "screening")
  expect_warning(build(samples_per_node = 0), "samples_per_node")
  expect_warning(build(extractor = "cubes"), "dual")
})
