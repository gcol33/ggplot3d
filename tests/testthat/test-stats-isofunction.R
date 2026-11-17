test_that("isofunction vertices are polished onto the level with function normals", {
  fun <- function(x, y, z) x^2 + y^2 + z^2
  radius <- function(b) sqrt(b$x^2 + b$y^2 + b$z^2)
  build <- function(refine, method = "cubes") ggplot2::ggplot_build(ggplot2::ggplot() +
    geom_isofunction_3d(fun = fun, n = 8, levels = 0.5, refine = refine, method = method) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + scale_z_continuous(limits = c(-1, 1)) + coord_3d())$data[[1L]]
  linear <- build(0L); polished <- build(2L); exact <- build(6L)
  expect_gt(max(abs(radius(linear) - sqrt(0.5))), 0.01)
  expect_lt(max(abs(radius(polished) - sqrt(0.5))), max(abs(radius(linear) - sqrt(0.5))) / 8)
  expect_lt(max(abs(radius(exact) - sqrt(0.5))), 1e-9)
  xyz <- as.matrix(exact[c("x", "y", "z")])
  expect_equal(as.matrix(exact[c("nx", "ny", "nz")]), xyz / sqrt(rowSums(xyz^2)), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(nrow(polished), nrow(linear))
  tetra <- build(2L, "tetra")
  expect_true(all(is.finite(as.matrix(tetra[c("x", "y", "z", "nx", "ny", "nz")]))))
  capped <- ggplot2::ggplot_build(ggplot2::ggplot() +
    geom_isofunction_3d(fun = function(x, y, z) x^3 + y + z, n = 6, levels = 0.3, caps = TRUE) + coord_3d())$data[[1L]]
  cap <- capped[capped$cap == 1L, ]
  expect_gt(nrow(cap), 0L)
  expect_true(all(rowSums(abs(as.matrix(cap[c("nx", "ny", "nz")]))) == 1))
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = fun, refine = -1) + coord_3d()), "refine")
})

test_that("implicit flat normals follow each face across a sharp crease", {
  fun <- sdf_box(size = c(1, 1, 1)) - sdf_capsule(c(0, 0, -2), c(0, 0, 2), 0.2)
  p <- ggplot2::ggplot() + geom_isofunction_3d(fun = fun, n = 12, method = 'dual') +
    ggplot2::xlim(-0.7, 0.7) + ggplot2::ylim(-0.7, 0.7) + scale_z_continuous(limits = c(-0.7, 0.7)) + coord_3d()
  d <- ggplot2::ggplot_build(p)$data[[1L]]
  face <- match(d$.face, unique(d$.face))
  centre <- rowsum(as.matrix(d[c('x','y','z')]), face, reorder = FALSE) / tabulate(face)
  expected <- sdf_gradient_3d(fun, centre)
  expected <- expected / sqrt(rowSums(expected^2))
  actual <- as.matrix(d[c('.flat_nx','.flat_ny','.flat_nz')])
  expect_equal(unname(actual), unname(expected[face, ]), tolerance = 1e-10)
  expect_true(any(rowSums((actual - as.matrix(d[c('nx','ny','nz')]))^2) > 0.5))
  shuffled <- d[rev(seq_len(nrow(d))), ]
  got <- isosurface_face_normals_3d(shuffled, function(p) sdf_gradient_3d(fun, p))
  expect_equal(got[c('.flat_nx','.flat_ny','.flat_nz')], shuffled[c('.flat_nx','.flat_ny','.flat_nz')])
})
