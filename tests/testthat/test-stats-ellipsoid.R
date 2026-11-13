iris_plot <- function(...) {
  ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, z = Petal.Length, group = Species)) +
    stat_ellipsoid_3d(...) + coord_3d()
}

setosa <- unname(as.matrix(iris[iris$Species == "setosa", 1:3]))

test_that("the t estimate matches MASS::cov.trob", {
  skip_if_not_installed("MASS")
  ours <- ggplot3d:::cov_trob_3d(setosa)
  theirs <- MASS::cov.trob(setosa)
  expect_equal(ours$cov, unname(theirs$cov), tolerance = 1e-10)
  expect_equal(unname(ours$center), unname(theirs$center), tolerance = 1e-10)
})

test_that("normal data ellipsoids take the stat_ellipse scaling in three dimensions", {
  out <- ggplot2::layer_data(iris_plot(type = "norm", level = 0.9))
  expect_equal(nrow(out), 3L)
  row <- out[order(out$group), ][1L, ]
  n <- nrow(setosa)
  expect_equal(c(row$x, row$y, row$z), unname(colMeans(setosa)))
  expected <- 3 * stats::qf(0.9, 3, n - 1) * stats::cov(setosa)
  expect_equal(row$sigma[[1L]], expected)
  expect_equal(row$level, 0.9)
  expect_equal(row$n, n)
  expect_equal(row$volume, 4 / 3 * pi * sqrt(det(expected)))
})

test_that("standard-error ellipsoids are Hotelling regions of the mean", {
  out <- ggplot2::layer_data(iris_plot(type = "norm", kind = "se"))
  sigma <- out$sigma[[which(out$group == 1L)]]
  n <- nrow(setosa)
  r2 <- 3 * (n - 1) / (n - 3) * stats::qf(0.95, 3, n - 3)
  expect_equal(sigma, r2 * stats::cov(setosa) / n)
  # Coverage of the mean of normal samples reaches the nominal level.
  set.seed(4)
  inside <- vapply(seq_len(400), function(i) {
    x <- matrix(stats::rnorm(30), 10L, 3L)
    s <- ggplot3d:::ellipsoid_shape_3d(x, "norm", 0.8, "se")
    d <- s$center
    drop(d %*% solve(s$cov, d)) <= 1
  }, logical(1))
  expect_equal(mean(inside), 0.8, tolerance = 0.06)
})

test_that("euclid draws a sphere of radius level", {
  out <- ggplot2::layer_data(iris_plot(type = "euclid", level = 0.5))
  expect_equal(out$sigma[[1L]], diag(0.25, 3))
  expect_error(iris_plot(type = "euclid", kind = "se"), "covariance")
  expect_error(iris_plot(level = 1.5), "between zero and one")
  expect_error(stat_ellipsoid_3d(level = -1), "positive")
})

test_that("enclosing ellipsoids contain every point and touch the hull", {
  cube <- as.matrix(expand.grid(c(-1, 1), c(-1, 1), c(-1, 1)))
  hull <- ggplot3d:::ellipsoid_hull_3d(cube)
  expect_equal(unname(hull$center), c(0, 0, 0), tolerance = 1e-6)
  expect_equal(unname(hull$cov), diag(3, 3), tolerance = 1e-6)
  out <- ggplot2::layer_data(iris_plot(kind = "ehull"))
  for (g in 1:3) {
    x <- unname(as.matrix(iris[as.integer(iris$Species) == g, 1:3]))
    row <- out[out$group == g, ]
    centred <- sweep(x, 2L, c(row$x, row$y, row$z))
    q <- rowSums((centred %*% solve(row$sigma[[1L]])) * centred)
    expect_lte(max(q), 1 + 1e-9)
    expect_gt(max(q), 0.999)
  }
  expect_equal(out$level, rep(1, 3))
})

test_that("degenerate groups are dropped with a message", {
  few <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
  p <- ggplot2::ggplot(few, ggplot2::aes(x, y, z = z)) + stat_ellipsoid_3d() + coord_3d()
  expect_message(out <- ggplot2::layer_data(p), "at least four")
  expect_equal(nrow(out), 0L)
  flat <- data.frame(x = c(0, 1, 0, 1, 0.5), y = c(0, 0, 1, 1, 0.5), z = 0)
  for (kind in c("data", "ehull")) {
    p <- ggplot2::ggplot(flat, ggplot2::aes(x, y, z = z)) + stat_ellipsoid_3d(kind = kind) + coord_3d()
    expect_message(out <- ggplot2::layer_data(p), "plane")
    expect_equal(nrow(out), 0L)
  }
})

test_that("ellipsoids are icosphere meshes with optional principal axes", {
  plain <- stats_scene(iris_plot(detail = 1))$packet
  expect_equal(length(plain$faces$primitive_id), 3L * 80L)
  expect_equal(length(plain$polylines$primitive_id), 0L)
  scene <- stats_scene(iris_plot(detail = 1, axes = TRUE, fill = "steelblue", alpha = 0.3))$packet
  expect_equal(length(scene$faces$primitive_id), 3L * 80L)
  expect_equal(length(scene$polylines$primitive_id), 9L)
  strokes <- scene$styles[scene$polylines$style_id, ]
  expect_true(all(strokes$colour == ggplot3d:::axis_colours_3d(NA, "steelblue")))
})

test_that("principal axes run along the semi-axes of the ellipsoid", {
  d <- data.frame(x = 0, y = 0, z = 0, rx = 2, ry = 1, rz = 0.5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, rx = rx, ry = ry, rz = rz)) +
    geom_ellipsoid_3d(axes = TRUE, alpha = 0.5, detail = 4) + coord_3d()
  scene <- stats_scene(p)$packet
  xyz <- cbind(scene$vertices$x, scene$vertices$y, scene$vertices$z)
  ends <- scene$polylines$offset
  a <- xyz[scene$corners$vertex[ends], , drop = FALSE]
  b <- xyz[scene$corners$vertex[ends + 1L], , drop = FALSE]
  surface <- xyz[unique(scene$corners$vertex[seq_len(min(ends) - 1L)]), , drop = FALSE]
  centre <- colMeans(surface)
  expect_equal(unname((a + b) / 2), matrix(centre, 3L, 3L, byrow = TRUE), tolerance = 1e-6)
  # Each axis runs along one coordinate axis and spans the surface along it.
  span <- abs(b - a)
  expect_equal(sort(apply(span, 1L, which.max)), 1:3)
  expect_equal(unname(sort(apply(span, 1L, max))), unname(sort(apply(surface, 2L, function(v) diff(range(v))))), tolerance = 0.01)
  expect_equal(ggplot3d:::axis_colours_3d(c(NA, "red", NA), c("#FF0000", "blue", NA)), c("#7F0000", "red", NA))
})
