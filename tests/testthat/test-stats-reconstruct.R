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
  expect_error(stat_implicit_fit_3d(method = "poisson"), "should be")
  big <- data.frame(x = runif(2001), y = runif(2001), z = runif(2001))
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot(big, ggplot2::aes(x, y, z = z)) + stat_implicit_fit_3d() + coord_3d()), "2000")
})
