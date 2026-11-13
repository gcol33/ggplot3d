slice_grid_data <- function(f, n = 11) {
  ax <- seq(-1, 1, length.out = n)
  d <- expand.grid(x = ax, y = ax, z = ax)
  d$value <- f(d$x, d$y, d$z)
  d
}

slice_build <- function(d, layer) {
  ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) + layer + coord_3d())$data[[1L]]
}

triangle_area_3d <- function(out) {
  p <- as.matrix(out[c("x", "y", "z")])
  a <- p[out$.corner == 1L, , drop = FALSE]; b <- p[out$.corner == 2L, , drop = FALSE]; c <- p[out$.corner == 3L, , drop = FALSE]
  u <- b - a; v <- c - a
  sum(sqrt((u[, 2] * v[, 3] - u[, 3] * v[, 2])^2 + (u[, 3] * v[, 1] - u[, 1] * v[, 3])^2 + (u[, 1] * v[, 2] - u[, 2] * v[, 1])^2)) / 2
}

test_that("axis slices on a grid layer reuse the grid values exactly", {
  f <- function(x, y, z) sin(3 * x) * cos(2 * y) + z^3
  d <- slice_grid_data(f)
  layer <- sort(unique(d$x))[4L]
  out <- slice_build(d, geom_slice_3d(xslice = layer))
  expect_true(all(out$x == layer))
  key <- match(paste(out$x, out$y, out$z), paste(d$x, d$y, d$z))
  expect_false(anyNA(key))
  expect_identical(out$.value, d$value[key])
  expect_equal(nrow(out), 3L * 2L * 10L^2)
  expect_equal(out$value, rep(colMeans(matrix(out$.value, 3L)), each = 3L))
  expect_true(all(out$nx == 1 & out$ny == 0 & out$nz == 0))
})

test_that("slices interpolate trilinearly between grid layers and on oblique planes", {
  f <- function(x, y, z) x + 2 * y - z + x * y - 3 * x * y * z
  d <- slice_grid_data(f, 9)
  out <- slice_build(d, geom_slice_3d(xslice = 0.33, yslice = -0.71, zslice = 0.05))
  expect_equal(levels(out$slice), c("x = 0.33", "y = -0.71", "z = 0.05"))
  expect_equal(out$.value, f(out$x, out$y, out$z), tolerance = 1e-12)
  oblique <- slice_build(d, stat_slice_3d(slice = sdf_plane(c(1, 2, -0.5), c(0.1, 0, 0.2)), n = 15))
  expect_equal(oblique$.value, f(oblique$x, oblique$y, oblique$z), tolerance = 1e-12)
  expect_equal((oblique$x - 0.1) + 2 * oblique$y - 0.5 * (oblique$z - 0.2), rep(0, nrow(oblique)), tolerance = 1e-12)
  expect_true(all(abs(as.matrix(oblique[c("x", "y", "z")])) <= 1 + 1e-12))
  # The plane x + y + z = 0 cuts the cube [-1, 1]^3 in a regular hexagon of side sqrt(2).
  hexagon <- slice_build(d, stat_slice_3d(slice = sdf_plane(c(1, 1, 1)), n = 7))
  expect_equal(triangle_area_3d(hexagon), 3 * sqrt(3), tolerance = 1e-12)
  both <- slice_build(d, stat_slice_3d(slice = list(sdf_plane(c(1, 1, 1)), sdf_plane(c(0, 0, -1), c(0, 0, 0.5)))))
  expect_equal(levels(both$slice), c("plane 1", "plane 2"))
  expect_equal(triangle_area_3d(both[both$slice == "plane 2", ]), 4, tolerance = 1e-12)
  expect_false(anyDuplicated(paste(both$.face, both$.corner)) > 0)
})

test_that("slices default to the three central planes and mask missing values", {
  d <- slice_grid_data(function(x, y, z) x * y * z, 5)
  out <- slice_build(d, geom_slice_3d())
  expect_equal(levels(out$slice), c("x = 0", "y = 0", "z = 0"))
  d$value[d$x > 0.4 & d$y > 0.4] <- NA
  masked <- slice_build(d, geom_slice_3d(zslice = 0))
  expect_true(all(is.finite(masked$.value)))
  expect_lt(nrow(masked), 3L * 2L * 16L)
  expect_equal(nrow(slice_build(d, geom_slice_3d(zslice = 3))), 0L)
  expect_error(geom_slice_3d(slice = sdf_sphere()), "sdf_plane")
  expect_error(geom_slice_3d(xslice = NA_real_), "finite")
})

test_that("density slices share the trivariate kernel density", {
  set.seed(3)
  p <- data.frame(x = rnorm(150), y = rnorm(150), z = rnorm(150))
  p <- p[apply(abs(p), 1L, max) < 3, ]
  base <- ggplot2::ggplot(p, ggplot2::aes(x, y, z = z)) + ggplot2::xlim(-3, 3) + ggplot2::ylim(-3, 3) +
    scale_z_continuous(limits = c(-3, 3)) + coord_3d()
  out <- ggplot2::ggplot_build(base + geom_density_slice_3d(zslice = 0, n = 13, h = 0.7))$data[[1L]]
  axes <- list(seq(-3, 3, length.out = 13), seq(-3, 3, length.out = 13), 0)
  direct <- geometry_kde_cpp(as.matrix(p), axes, rep(0.7, 3L))
  key <- match(out$x, axes[[1L]]) + 13L * (match(out$y, axes[[2L]]) - 1L)
  expect_equal(out$.value, direct[key], tolerance = 1e-12)
  expect_identical(out$density, out$value)
})

test_that("isosurfaces clipped by a plane are capped on it", {
  d <- slice_grid_data(function(x, y, z) x^2 + y^2 + z^2, 11)
  for (method in c("cubes", "tetra", "dual")) {
    out <- slice_build(d, geom_isosurface_3d(levels = 0.5, method = method, caps = TRUE, clip = sdf_plane(c(0, 0, 1), c(0, 0, 0.1))))
    expect_lte(max(out$z), 0.1 + 1e-12)
    cap <- out[out$cap == 1L & abs(out$z - 0.1) < 1e-12, ]
    expect_true(all(cap$nz == 1))
    # The cap is the disc x^2 + y^2 <= 0.49 of the piecewise linear field; its
    # area converges to pi * 0.49 with the grid spacing squared.
    expect_equal(triangle_area_3d(cap), pi * 0.49, tolerance = 0.06)
    rim <- sqrt(cap$x^2 + cap$y^2)
    expect_lt(max(rim), sqrt(0.49) + 1e-9)
    expect_gt(max(rim), sqrt(0.49) - 0.02)
    open <- slice_build(d, geom_isosurface_3d(levels = 0.5, method = method, clip = sdf_plane(c(0, 0, 1), c(0, 0, 0.1))))
    expect_equal(sum(open$cap), 0L)
    expect_lte(max(open$z), 0.1 + 1e-12)
  }
  fine <- slice_build(slice_grid_data(function(x, y, z) x^2 + y^2 + z^2, 41),
    geom_isosurface_3d(levels = 0.5, caps = TRUE, clip = sdf_plane(c(0, 0, 1), c(0, 0, 0.1))))
  fine_cap <- fine[fine$cap == 1L & abs(fine$z - 0.1) < 1e-12, ]
  expect_equal(triangle_area_3d(fine_cap), pi * 0.49, tolerance = 0.003)
  expect_error(geom_isosurface_3d(clip = sdf_sphere()), "clip")
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = sdf_sphere(), adaptive = TRUE,
    clip = sdf_plane()) + coord_3d()), "adaptive")
})
