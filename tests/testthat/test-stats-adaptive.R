adaptive_topology <- function(b) {
  f <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  counts <- table(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))
  list(closed = all(counts == 2), oriented = !anyDuplicated(paste(e[, 1], e[, 2])),
       chi = length(unique(b$.vertex)) - length(counts) + nrow(f))
}

adaptive_build <- function(fun, ...) ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = fun, ...) +
  ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + scale_z_continuous(limits = c(-1, 1)) + coord_3d())$data[[1L]]

test_that("interval evaluation encloses values and gradients over boxes", {
  set.seed(3)
  fun <- function(x, y, z) sin(3 * x) * exp(y / 2) + sqrt(z^2 + 1) - abs(x - y)^3 / (2 + cos(z)) +
    pmax(x * y, log1p(z^2), atan(y)) + tanh(x)^2 - z^-2 + 1 / (3 + x)
  n <- 200L
  lo <- matrix(runif(3 * n, -1, 0.9), n); hi <- lo + matrix(runif(3 * n, 0.001, 0.1), n)
  box <- lapply(1:3, function(a) list(lo = lo[, a], hi = hi[, a]))
  e <- ggplot3d:::interval_evaluate_3d(ggplot3d:::interval_function_3d(fun), list(), box, c(1, 1, 1))
  h <- 1e-6
  for (k in 1:20) {
    p <- lo + (hi - lo) * matrix(runif(3 * n), n)
    v <- fun(p[, 1], p[, 2], p[, 3])
    ok <- is.finite(v)
    expect_true(all(v[ok] >= e$lo[ok] - 1e-9 & v[ok] <= e$hi[ok] + 1e-9))
    for (a in 1:3) {
      q <- p; q[, a] <- q[, a] + h; r <- p; r[, a] <- r[, a] - h
      g <- (fun(q[, 1], q[, 2], q[, 3]) - fun(r[, 1], r[, 2], r[, 3])) / (2 * h)
      ok <- is.finite(g)
      expect_true(all(g[ok] >= e$d[[a]]$lo[ok] - 1e-4 * (1 + abs(g[ok])) & g[ok] <= e$d[[a]]$hi[ok] + 1e-4 * (1 + abs(g[ok]))))
    }
  }
  undefined <- ggplot3d:::interval_evaluate_3d(function(x, y, z) sqrt(x) + y, list(),
    list(list(lo = -2, hi = -1), list(lo = 0, hi = 1), list(lo = 0, hi = 1)), c(1, 1, 1))
  expect_true(is.na(undefined$lo) && is.na(undefined$hi))
})

test_that("functions without an interval extension stop with the offending call", {
  build <- function(fun) ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = fun, levels = 0.2, adaptive = TRUE, depth = c(1, 2)) + coord_3d())
  expect_warning(build(function(x, y, z) floor(x) + y + z), "`floor\\(x\\)` has no interval extension")
  expect_warning(build(function(x, y, z) ifelse(x > 0, x, y) + z), "`x > 0` has no interval extension")
  expect_warning(build(function(x, y, z) stats::pnorm(x) + y + z), "fun cannot be evaluated on intervals in `stats::pnorm\\(x\\)`")
  expect_error(stat_isofunction_3d(fun = function(x, y, z) x, adaptive = NA), "adaptive")
  expect_warning(ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = function(x, y, z) x + y + z,
    levels = 0.2, adaptive = TRUE, method = "cubes") + coord_3d()), "dual")
})

test_that("the octree partitions the box and refines only near the level set", {
  fun <- function(x, y, z) x^2 + y^2 + z^2
  tree <- ggplot3d:::isofunction_octree_3d(fun, list(), list(c(-1, 1), c(-1, 1), c(-1, 1)), 0.5, c(2, 5), 0.002)
  expect_equal(sum(as.double(tree$boxes[, 4])^3), 32^3)
  expect_true(all(tree$boxes[, 1:3] %% tree$boxes[, 4] == 0))
  centre <- (tree$boxes[, 1:3] + tree$boxes[, 4] / 2) / 16 - 1
  r <- sqrt(rowSums(centre^2)); half <- sqrt(3) * tree$boxes[, 4] / 32
  expect_true(all(abs(r[!tree$empty] - sqrt(0.5)) <= half[!tree$empty] + 1e-12))
  expect_true(all(abs(r[tree$empty] - sqrt(0.5)) > 0))
  expect_lt(tree$evaluated, 32^3 / 4)
})

test_that("adaptive dual contouring gives closed manifolds without cracks between box sizes", {
  sphere <- adaptive_build(function(x, y, z) x^2 + y^2 + z^2, levels = 0.5, adaptive = TRUE, depth = c(2, 5))
  m <- adaptive_topology(sphere)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 2)
  expect_lt(max(abs(sqrt(sphere$x^2 + sphere$y^2 + sphere$z^2) - sqrt(0.5))), 0.005)
  torus <- adaptive_build(function(x, y, z) (sqrt(x^2 + y^2) - 0.6)^2 + z^2, levels = 0.06, adaptive = TRUE, depth = c(2, 5))
  m <- adaptive_topology(torus)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 0)
  nested <- adaptive_build(function(x, y, z) x^2 + y^2 + z^2, levels = c(0.3, 0.8), adaptive = TRUE, depth = c(2, 4), method = "nets")
  m <- adaptive_topology(nested)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 4)
  expect_setequal(unique(nested$level), c(0.3, 0.8))
  capped <- adaptive_build(function(x, y, z) x^3 + y + z, levels = 0.3, adaptive = TRUE, depth = c(2, 5), caps = TRUE, method = "nets")
  m <- adaptive_topology(capped)
  expect_true(m$closed && m$oriented); expect_equal(m$chi, 2)
  cap <- capped[capped$cap == 1L, ]
  expect_gt(nrow(cap), 0L)
  expect_true(all(rowSums(abs(as.matrix(cap[c("nx", "ny", "nz")]))) == 1))
  expect_true(all(abs(cap$x) == 1 | abs(cap$y) == 1 | abs(cap$z) == 1))
})

test_that("adaptive extraction keeps sharp corners and beats a uniform grid at fewer evaluations", {
  cube <- adaptive_build(function(x, y, z) pmax(abs(x), abs(y), abs(z)), levels = 0.53, adaptive = TRUE, depth = c(2, 5))
  expect_equal(max(apply(abs(as.matrix(cube[c("x", "y", "z")])), 1, min)), 0.53, tolerance = 1e-9)
  calls <- new.env(); calls$n <- 0
  blob <- function(x, y, z) {
    if (!inherits(x, "ggplot3d_interval")) calls$n <- calls$n + length(x)
    pmin((x - 0.5)^2 + (y - 0.5)^2 + (z - 0.5)^2 - 0.01, z + 0.8)
  }
  distance <- function(b) max(abs(pmin(sqrt((b$x - 0.5)^2 + (b$y - 0.5)^2 + (b$z - 0.5)^2) - 0.1, b$z + 0.8)))
  adaptive <- adaptive_build(blob, levels = 0, adaptive = TRUE, depth = c(3, 7)); adaptive_calls <- calls$n
  calls$n <- 0
  uniform <- adaptive_build(blob, levels = 0, n = 65, method = "dual"); uniform_calls <- calls$n
  expect_lt(distance(adaptive), distance(uniform))
  expect_lt(adaptive_calls, uniform_calls / 10)
})
