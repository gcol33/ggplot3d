sdf_points <- function(n = 200, seed = 1, half = 2) {
  set.seed(seed)
  matrix(stats::runif(3 * n, -half, half), ncol = 3L)
}

test_that("sdf primitives return analytic signed distances", {
  p <- sdf_points()
  r <- sqrt(rowSums(p^2))
  expect_equal(sdf_sphere(radius = 1)(p[, 1], p[, 2], p[, 3]), r - 1)
  box <- sdf_box(size = c(2, 1, 0.5))
  q <- abs(p) - matrix(c(1, 0.5, 0.25), nrow(p), 3L, byrow = TRUE)
  expected <- sqrt(rowSums(pmax(q, 0)^2)) + pmin(apply(q, 1L, max), 0)
  expect_equal(box(p[, 1], p[, 2], p[, 3]), expected)
  # A rounded box is the box shrunk by the radius, offset by the radius.
  expect_equal(sdf_box(size = c(2, 2, 2), radius = 0.5)(c(2, 0), c(2, 0), c(0, 0)), c(sqrt(2 * 1.5^2) - 0.5, -1))
  torus <- sdf_torus(centre = c(0, 0, 0.5), major = 1, minor = 0.3)
  expect_equal(torus(p[, 1], p[, 2], p[, 3]), sqrt((sqrt(p[, 1]^2 + p[, 2]^2) - 1)^2 + (p[, 3] - 0.5)^2) - 0.3)
  capsule <- sdf_capsule(c(0, 0, -1), c(0, 0, 1), 0.2)
  h <- pmin(pmax(p[, 3], -1), 1)
  expect_equal(capsule(p[, 1], p[, 2], p[, 3]), sqrt(p[, 1]^2 + p[, 2]^2 + (p[, 3] - h)^2) - 0.2)
  plane <- sdf_plane(c(1, 1, 0), c(0, 0, 1))
  expect_equal(plane(p[, 1], p[, 2], p[, 3]), (p[, 1] + p[, 2]) / sqrt(2))
  ball <- sdf_metaballs(matrix(c(0, 0, 0), 1L), radius = 0.7)
  expect_equal(ball(0.7, 0, 0), 0, tolerance = 1e-12)
  expect_lt(ball(0, 0, 0), 0); expect_gt(ball(1, 0, 0), 0)
})

test_that("sdf operators follow min, max and the normalised smooth minimum", {
  p <- sdf_points()
  a <- sdf_sphere(c(-0.4, 0, 0), 1); b <- sdf_box(c(0.4, 0, 0), c(1.5, 1, 1))
  va <- a(p[, 1], p[, 2], p[, 3]); vb <- b(p[, 1], p[, 2], p[, 3])
  expect_equal((a | b)(p[, 1], p[, 2], p[, 3]), pmin(va, vb))
  expect_equal((a & b)(p[, 1], p[, 2], p[, 3]), pmax(va, vb))
  expect_equal((a - b)(p[, 1], p[, 2], p[, 3]), pmax(va, -vb))
  expect_equal((-a)(p[, 1], p[, 2], p[, 3]), -va)
  # Where both operands agree the blend adds exactly k.
  expect_equal(sdf_blend(a, a, k = 0.15)(p[, 1], p[, 2], p[, 3]), va - 0.15)
  far <- abs(va - vb) >= 4 * 0.1
  expect_equal(sdf_blend(a, b, 0.1)(p[, 1], p[, 2], p[, 3])[far], pmin(va, vb)[far])
  expect_equal(sdf_blend(a, b, 0.1, "intersection")(p[, 1], p[, 2], p[, 3])[far], pmax(va, vb)[far])
  apart <- abs(va + vb) >= 4 * 0.1
  expect_equal(sdf_blend(a, b, 0.1, "difference")(p[, 1], p[, 2], p[, 3])[apart], pmax(va, -vb)[apart])
  expect_equal(sdf_offset(a, 0.2)(p[, 1], p[, 2], p[, 3]), va - 0.2)
  expect_equal(sdf_shell(a, 0.2)(p[, 1], p[, 2], p[, 3]), abs(va) - 0.1)
  moved <- sdf_transform(sdf_torus(major = 1, minor = 0.3), translate = c(1, 0, 0), rotate = c(90, 0, 0), scale = 2)
  # The torus now lies in the x-z plane around (1, 0, 0) with radii 2 and 0.6;
  # its centre is 2 from the tube axis.
  expect_equal(moved(c(1, 3, 1), c(0, 0, 0), c(2, 0, 0)), c(-0.6, -0.6, 1.4))
  expect_equal(moved(1, 0.5, 2), sqrt(0.5^2) - 0.6)
  expect_error(a * b, "not defined")
  expect_error(a | 1, "ggplot3d_sdf")
  expect_error(sdf_transform(a, rotate = diag(c(1, 1, -1))), "rotation")
  expect_output(print(a - b), "distance bound \\(Lipschitz 1\\)")
  expect_output(print(a), "exact distance")
  expect_output(print(sdf_metaballs(matrix(0, 2L, 3L))), "implicit function")
})

test_that("sdf gradients are exact and lie in their interval enclosures", {
  p <- sdf_points(50)
  expect_equal(sdf_gradient_3d(sdf_sphere(), p), p / sqrt(rowSums(p^2)))
  shape <- (sdf_shell(sdf_blend(sdf_sphere(c(-0.5, 0, 0), 0.6), sdf_box(c(0.5, 0, 0), c(1, 1, 1)), 0.2), 0.1) &
    sdf_plane(c(0, 1, 0))) | sdf_transform(sdf_torus(), rotate = c(30, 20, 10)) | (sdf_gyroid(2) & sdf_capsule()) |
    sdf_metaballs(rbind(c(1, 1, 1), c(1.4, 1, 1)), 0.3)
  f <- function_evaluator_3d(shape, list())
  difference <- function_gradient_3d(function(q) f(q), list(c(-2, 2), c(-2, 2), c(-2, 2)))
  expect_equal(function_gradient_3d(f, list())(p), difference(p), tolerance = 1e-6)
  # Interval enclosures contain every sampled value and gradient of the box.
  lo <- c(0.1, -0.3, -0.2); hi <- c(0.6, 0.2, 0.3)
  box <- interval_evaluate_3d(interval_function_3d(shape), list(),
    lapply(1:3, function(a) list(lo = lo[a], hi = hi[a])), c(1, 1, 1))
  inner <- sweep(matrix(stats::runif(600), ncol = 3L), 2L, hi - lo, `*`) + rep(lo, each = 200L)
  values <- f(inner); gradient <- sdf_gradient_3d(shape, inner)
  expect_true(all(values >= box$lo - 1e-12 & values <= box$hi + 1e-12))
  for (a in 1:3) expect_true(all(gradient[, a] >= box$d[[a]]$lo - 1e-12 & gradient[, a] <= box$d[[a]]$hi + 1e-12))
})

test_that("sdf values respect their Lipschitz constants", {
  a <- sdf_points(300, 2); b <- a + matrix(stats::rnorm(900, sd = 0.05), ncol = 3L)
  shapes <- list(sdf_sphere() - sdf_box(size = c(1, 1, 3)), sdf_blend(sdf_torus(), sdf_capsule(), 0.3),
    sdf_gyroid(1.5), sdf_transform(sdf_box(radius = 0.1), c(0.2, 0, 0), c(10, 50, 80), 1.7))
  for (s in shapes) {
    ratio <- abs(s(a[, 1], a[, 2], a[, 3]) - s(b[, 1], b[, 2], b[, 3])) / sqrt(rowSums((a - b)^2))
    expect_lte(max(ratio), get("node", environment(s))$lipschitz + 1e-9)
  }
})

test_that("isofunction surfaces of an sdf use level 0 and exact normals", {
  build <- function(...) ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(...) +
    ggplot2::xlim(-1.5, 1.5) + ggplot2::ylim(-1.5, 1.5) + scale_z_continuous(limits = c(-1.5, 1.5)) + coord_3d())$data[[1L]]
  d <- build(fun = sdf_sphere(radius = 1), n = 10, refine = 6L)
  expect_equal(unique(d$level), 0)
  xyz <- as.matrix(d[c("x", "y", "z")])
  expect_lt(max(abs(sqrt(rowSums(xyz^2)) - 1)), 1e-9)
  expect_equal(as.matrix(d[c("nx", "ny", "nz")]), xyz / sqrt(rowSums(xyz^2)), tolerance = 1e-12, ignore_attr = TRUE)
  shape <- sdf_box(size = c(2, 2, 2), radius = 0.3) - sdf_sphere(radius = 1.2)
  a <- build(fun = shape, adaptive = TRUE, depth = c(3L, 6L))
  expect_gt(nrow(a), 0L)
  expect_lt(max(abs(shape(a$x, a$y, a$z))), 0.02)
  expect_equal(unique(a$level), 0)
  expect_equal(unique(build(fun = sdf_sphere(), n = 6, levels = 0.2)$level), 0.2)
})
