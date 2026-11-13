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
