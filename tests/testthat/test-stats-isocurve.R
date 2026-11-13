curve_build <- function(layer, data = NULL, mapping = ggplot2::aes(), half = 1.5) {
  ggplot2::ggplot_build(ggplot2::ggplot(data, mapping) + layer + ggplot2::xlim(-half, half) + ggplot2::ylim(-half, half) +
    scale_z_continuous(limits = c(-half, half)) + coord_3d())$data[[1L]]
}

test_that("marching triangles contour any value on grid, Delaunay and mesh surfaces", {
  d <- expand.grid(x = seq(-1, 1, length.out = 9), y = seq(-1, 1, length.out = 9))
  d$z <- d$x^2 - d$y^2
  # value = n . p cuts the surface with parallel planes: linear on every
  # triangle, so each contour point lies on its plane exactly.
  out <- curve_build(geom_contour_3d(breaks = c(-0.5, 0.25)), d, ggplot2::aes(x, y, z = z, value = x + 2 * y))
  expect_equal(out$x + 2 * out$y, out$level, tolerance = 1e-12)
  expect_setequal(unique(out$level), c(-0.5, 0.25))
  expect_true(all(abs(out$x) <= 1 & abs(out$y) <= 1))
  # Contour points are on surface triangle edges: z is linear in x, y there.
  expect_true(all(out$z >= -1 - 1e-12 & out$z <= 1 + 1e-12))
  set.seed(4)
  s <- data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1)); s$z <- s$x^2 + s$y^2
  tri <- curve_build(geom_contour_3d(breaks = 0.5, method = "delaunay"), s, ggplot2::aes(x, y, z = z), half = 2)
  expect_equal(tri$z, rep(0.5, nrow(tri)))
  expect_lt(max(abs(sqrt(tri$x^2 + tri$y^2) - sqrt(0.5))), 0.05)
  # Octahedron mesh: the level set of z is a closed square loop.
  v <- data.frame(x = c(1, -1, 0, 0, 0, 0), y = c(0, 0, 1, -1, 0, 0), z = c(0, 0, 0, 0, 1, -1))
  f <- rbind(c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5), c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6))
  m <- curve_build(geom_contour_3d(ggplot2::aes(value = z), breaks = 0.5), mesh_3d(v, f), ggplot2::aes(x, y, z = z))
  expect_equal(length(unique(m$group)), 1L)
  expect_equal(nrow(m), 5L)
  expect_equal(unlist(m[1L, c("x", "y", "z")]), unlist(m[5L, c("x", "y", "z")]))
  expect_equal(abs(m$x) + abs(m$y), rep(0.5, 5L))
  expect_error(curve_build(geom_contour_filled_3d(breaks = c(0, 1)), d, ggplot2::aes(x, y, z = z, value = x)), "value")
})

test_that("slice contours trace level lines of the sliced field", {
  ax <- seq(-1, 1, length.out = 21)
  g <- expand.grid(x = ax, y = ax, z = ax); g$value <- with(g, x^2 + y^2 + z^2)
  out <- curve_build(geom_slice_contour_3d(zslice = 0.2, breaks = 0.5), g, ggplot2::aes(x, y, z = z, value = value))
  expect_true(all(out$z == 0.2))
  expect_equal(levels(out$slice), "z = 0.2")
  # Trilinear values of x^2 + y^2 are exact on the grid and overestimate in
  # between by at most h^2 / 2 per axis.
  r <- sqrt(out$x^2 + out$y^2)
  expect_lt(max(abs(r - sqrt(0.46))), 0.01)
  expect_equal(length(unique(out$group)), 1L)
  # Slices and their contours carry the grid box, which trains the scales.
  expect_true(all(out$zmin == -1 & out$zmax == 1 & out$xmin == -1 & out$ymax == 1))
  faces <- curve_build(geom_slice_3d(zslice = 0.2), g, ggplot2::aes(x, y, z = z, value = value))
  expect_true(all(faces$zmin == -1 & faces$zmax == 1))
})

test_that("isocurves lie on both level sets after Newton steps", {
  circle <- curve_build(geom_isocurve_3d(fun1 = sdf_sphere(radius = 1), fun2 = sdf_plane(c(0, 0, 1), c(0, 0, 0.3)), n = 12))
  # Newton converges quadratically: two steps from the linear intersection.
  expect_lt(max(abs(sqrt(circle$x^2 + circle$y^2 + circle$z^2) - 1)), 1e-9)
  expect_lt(max(abs(circle$z - 0.3)), 1e-12)
  expect_equal(length(unique(circle$group)), 1L)
  expect_equal(unlist(circle[1L, c("x", "y", "z")]), unlist(circle[nrow(circle), c("x", "y", "z")]))
  linear <- curve_build(geom_isocurve_3d(fun1 = sdf_sphere(radius = 1), fun2 = sdf_plane(c(0, 0, 1), c(0, 0, 0.3)), n = 12, refine = 0))
  expect_gt(max(abs(sqrt(linear$x^2 + linear$y^2 + linear$z^2) - 1)), 1e-3)
  expect_lt(max(abs(linear$z - 0.3)), 1e-12)
  # Two plain functions with central-difference gradients: a saddle cut by a
  # cylinder, open curves are not expected inside the box.
  saddle <- curve_build(geom_isocurve_3d(fun1 = function(x, y, z) x^2 + y^2, fun2 = function(x, y, z) z - x * y,
    levels = c(1, 0), n = 16))
  expect_lt(max(abs(saddle$x^2 + saddle$y^2 - 1)), 1e-8)
  expect_lt(max(abs(saddle$z - saddle$x * saddle$y)), 1e-8)
  expect_equal(nrow(curve_build(geom_isocurve_3d(fun1 = sdf_sphere(radius = 0.5), fun2 = sdf_plane(c(0, 0, 1), c(0, 0, 1)), n = 8))), 0L)
  expect_error(geom_isocurve_3d(fun1 = sdf_sphere(), fun2 = sdf_plane(), levels = 0), "levels")
})
