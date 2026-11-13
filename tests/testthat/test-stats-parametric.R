parametric_topology <- function(b) {
  f <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  counts <- table(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))
  list(chi = length(unique(b$.vertex)) - length(counts) + nrow(f), interior = sum(counts == 2),
    boundary = sum(counts == 1), other = sum(counts > 2), oriented = !anyDuplicated(paste(e[, 1], e[, 2])))
}

parametric_build <- function(...) ggplot2::ggplot_build(ggplot2::ggplot() + geom_parametric_3d(...) + coord_3d())$data[[1L]]

torus_3d <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
sphere_3d <- function(u, v) list(x = sin(v) * cos(u), y = sin(v) * sin(u), z = cos(v))

test_that("closed and twisted seams weld into the expected surfaces", {
  t <- parametric_topology(parametric_build(fun = torus_3d, n = c(24, 12), closed = TRUE))
  expect_equal(c(t$chi, t$boundary, t$other), c(0, 0, 0)); expect_true(t$oriented)
  s <- parametric_topology(parametric_build(fun = sphere_3d, v = c(0, pi), n = c(24, 13), closed = c(u = TRUE, v = FALSE)))
  expect_equal(c(s$chi, s$boundary, s$other), c(2, 0, 0)); expect_true(s$oriented)
  moebius <- function(u, v) list(x = (1 + v / 2 * cos(u / 2)) * cos(u), y = (1 + v / 2 * cos(u / 2)) * sin(u), z = v / 2 * sin(u / 2))
  m <- parametric_topology(parametric_build(fun = moebius, v = c(-1, 1), n = c(40, 5), closed = c(TRUE, FALSE), twist = c(TRUE, FALSE)))
  expect_equal(c(m$chi, m$other), c(0, 0)); expect_equal(m$boundary, 2 * 40)
  klein <- function(u, v) {
    r <- 4 * (1 - cos(u) / 2)
    list(x = ifelse(u < pi, 6 * cos(u) * (1 + sin(u)) + r * cos(u) * cos(v), 6 * cos(u) * (1 + sin(u)) + r * cos(v + pi)),
      y = ifelse(u < pi, 16 * sin(u) + r * sin(u) * cos(v), 16 * sin(u)), z = r * sin(v))
  }
  k <- parametric_topology(parametric_build(fun = klein, n = c(40, 16), closed = TRUE, twist = c(FALSE, FALSE)))
  expect_equal(c(k$boundary, k$other), c(0, 0))
  expect_error(parametric_build(fun = torus_3d, twist = TRUE), "closed")
})

test_that("normals and curvature follow the fundamental forms", {
  b <- parametric_build(fun = function(u, v) lapply(sphere_3d(u, v), `*`, 2), v = c(0, pi), n = c(24, 13), closed = c(TRUE, FALSE))
  xyz <- as.matrix(b[c("x", "y", "z")]); normal <- as.matrix(b[c("nx", "ny", "nz")])
  expect_equal(abs(rowSums(normal * xyz) / 2), rep(1, nrow(b)), tolerance = 1e-6, ignore_attr = TRUE)
  inner <- abs(b$z) < 1.9
  expect_equal(b$gaussian_curvature[inner], rep(0.25, sum(inner)), tolerance = 1e-4)
  expect_equal(abs(b$mean_curvature[inner]), rep(0.5, sum(inner)), tolerance = 1e-4)
  t <- parametric_build(fun = torus_3d, n = c(24, 12), closed = TRUE)
  outer <- t$v < pi / 2 | t$v > 3 * pi / 2
  expect_true(all(t$gaussian_curvature[outer] > 0)); expect_true(all(t$gaussian_curvature[!outer & abs(t$v - pi) < 1] < 0))
  expect_true(all(c("u", "v", "gaussian_curvature", "mean_curvature") %in% names(t)))
})

test_that("adaptive bisection refines without cracks and reduces chord error", {
  wave <- function(u, v) list(x = u, y = v, z = 0.3 * sin(3 * u) * cos(2 * v))
  coarse <- parametric_build(fun = wave, u = c(-1, 1), v = c(-1, 1), n = 6)
  fine <- parametric_build(fun = wave, u = c(-1, 1), v = c(-1, 1), n = 6, adaptive = TRUE, max_depth = 4)
  expect_gt(nrow(fine), nrow(coarse))
  tc <- parametric_topology(coarse); tf <- parametric_topology(fine)
  expect_equal(tf$chi, tc$chi); expect_equal(tf$other, 0); expect_true(tf$oriented)
  centroid_error <- function(b) {
    i <- seq(1L, nrow(b), 3L)
    cx <- (b$x[i] + b$x[i + 1] + b$x[i + 2]) / 3; cy <- (b$y[i] + b$y[i + 1] + b$y[i + 2]) / 3
    cz <- (b$z[i] + b$z[i + 1] + b$z[i + 2]) / 3
    max(abs(cz - 0.3 * sin(3 * cx) * cos(2 * cy)))
  }
  expect_lt(centroid_error(fine), centroid_error(coarse) / 3)
  closed <- parametric_topology(parametric_build(fun = torus_3d, n = c(10, 6), closed = TRUE, adaptive = TRUE, max_depth = 3))
  expect_equal(c(closed$chi, closed$boundary, closed$other), c(0, 0, 0))
  s <- stats_scene(ggplot2::ggplot() + geom_parametric_3d(ggplot2::aes(fill = ggplot2::after_stat(gaussian_curvature)),
    fun = torus_3d, n = c(16, 8), closed = TRUE) + coord_3d())
  expect_gt(length(s$packet$faces$primitive_id), 0L)
  expect_silent(validate_scene_packet(s$packet))
})
