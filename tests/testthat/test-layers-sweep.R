sweep_data <- function(p) ggplot2::layer_data(p + coord_3d())

closed_surface <- function(m) {
  f <- matrix(m$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  !anyDuplicated(paste(e[, 1], e[, 2])) && all(paste(e[, 2], e[, 1]) %in% paste(e[, 1], e[, 2]))
}

euler_number <- function(m) {
  f <- matrix(m$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  length(unique(m$.vertex)) - length(unique(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))) + nrow(f)
}

distance_to_curve <- function(m, curve) {
  vapply(seq_len(nrow(m)), function(i) min(sqrt(colSums((t(curve) - c(m$x[i], m$y[i], m$z[i]))^2))), numeric(1))
}

trefoil <- function(n) {
  u <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  data.frame(x = sin(u) + 2 * sin(2 * u), y = cos(u) - 2 * cos(2 * u), z = -sin(3 * u))
}

test_that("double reflection frames are rotation minimising and close on closed curves", {
  t <- seq(0, 4 * pi, length.out = 400); helix <- cbind(cos(t), sin(t), t / 4)
  f <- sweep_frames_cpp(helix, FALSE, c(0, 0, 0))
  expect_lt(max(abs(rowSums(f$tangent * f$normal))), 1e-12)
  expect_lt(max(abs(rowSums(f$normal^2) - 1)), 1e-12)
  frenet <- cbind(-cos(t), -sin(t), 0)
  turn <- atan2(rowSums(f$tangent * cbind(f$normal[, 2] * frenet[, 3] - f$normal[, 3] * frenet[, 2],
    f$normal[, 3] * frenet[, 1] - f$normal[, 1] * frenet[, 3], f$normal[, 1] * frenet[, 2] - f$normal[, 2] * frenet[, 1])),
    rowSums(f$normal * frenet))
  turn <- cumsum(c(turn[1L], (diff(turn) + pi) %% (2 * pi) - pi))
  # The normal of a rotation-minimising frame turns against the Frenet frame
  # at the rate of the torsion b / (a^2 + b^2) of the helix.
  expect_equal(unname(coef(stats::lm(turn ~ I(t * sqrt(1 + 1 / 16))))[2L]), 0.25 / (1 + 1 / 16), tolerance = 1e-6)
  u <- seq(0, 2 * pi, length.out = 61)[-61]
  circle <- sweep_frames_cpp(cbind(cos(u), sin(u), 0), TRUE, c(0, 0, 1))
  expect_equal(circle$normal[, 3], rep(1, 60))
  k <- as.matrix(trefoil(200))
  fk <- sweep_frames_cpp(k, TRUE, c(0, 0, 0))
  step <- acos(pmin(1, rowSums(fk$normal * fk$normal[c(2:200, 1L), ])))
  expect_lt(step[200L], 1.5 * max(step[-200L]))
})

test_that("tubes have the requested radius, caps and topology", {
  t <- seq(0, 2 * pi, length.out = 40)
  path <- data.frame(x = cos(t), y = sin(t), z = t / 3)
  curve <- as.matrix(path)
  open <- sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1, sides = 12, cap = "none"))
  expect_equal(max(open$.face), 2L * 12L * 39L)
  expect_equal(range(distance_to_curve(open, curve)), c(0.1, 0.1), tolerance = 1e-3)
  round <- sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1, sides = 12))
  expect_true(closed_surface(round)); expect_equal(euler_number(round), 2L)
  expect_lt(max(distance_to_curve(round, curve)), 0.1 + 1e-9)
  flat <- sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1, sides = 12, cap = "flat"))
  expect_true(closed_surface(flat)); expect_equal(euler_number(flat), 2L)
  end <- flat$x == 1 & flat$y == 0 & flat$z == 0
  tangent <- c(0, 1, 1 / 3) / sqrt(1 + 1 / 9)
  cap_normals <- as.matrix(flat[end, c("nx", "ny", "nz")]) %*% tangent
  expect_true(all(abs(cap_normals) > 0.99))
  ring <- sweep_data(ggplot2::ggplot(trefoil(60), ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.2, closed = TRUE))
  expect_true(closed_surface(ring)); expect_equal(euler_number(ring), 0L)
  expect_true(all(is.finite(ring$nx)))
})

test_that("tube radius, fill and twist follow the points", {
  path <- data.frame(x = 0:10, y = 0, z = 0, r = seq(0.1, 0.6, length.out = 11), v = 0:10)
  d <- sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z, radius = r, fill = v)) + geom_tube_3d(cap = "none", sides = 8))
  expect_equal(sqrt(d$y^2 + d$z^2), 0.1 + 0.05 * d$x, tolerance = 1e-9)
  expect_equal(sort(unique(round(d$arc_length, 9))), (0:9) + 0.5)
  expect_equal(length(unique(d$fill)), 10L)
  expect_error(sweep_data(ggplot2::ggplot(path[1, ], ggplot2::aes(x, y, z = z)) + geom_tube_3d()), "at least 2 distinct points")
  path$r[3] <- -1
  expect_error(sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z, radius = r)) + geom_tube_3d()), "nonnegative")
})

test_that("ribbons lie along their normal and twist about the curve", {
  u <- seq(0, 2 * pi, length.out = 49)[-49]
  circle <- data.frame(x = cos(u), y = sin(u), z = 0)
  flat <- sweep_data(ggplot2::ggplot(circle, ggplot2::aes(x, y, z = z)) + geom_ribbon_3d(width = 0.4, closed = TRUE))
  expect_equal(unique(flat$z), 0)
  expect_equal(range(sqrt(flat$x^2 + flat$y^2)), c(0.8, 1.2), tolerance = 1e-9)
  expect_true(all(abs(flat$nz) > 1 - 1e-9))
  line <- data.frame(x = 0:4, y = 0, z = 0, a = c(0, 0, 0, 0, 90))
  twisted <- sweep_data(ggplot2::ggplot(line, ggplot2::aes(x, y, z = z, twist = a)) + geom_ribbon_3d(width = 2))
  last <- twisted[twisted$x == 4, ]
  expect_equal(sort(unique(round(abs(last$y), 9))), 0)
  expect_equal(sort(unique(round(abs(last$z), 9))), 1)
})

test_that("profiles are swept with outward normals", {
  star <- t(sapply(0:9, function(k) (1 + (k %% 2)) / 2 * c(cos(k * pi / 5), sin(k * pi / 5))))
  path <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 0, 1), z = c(0, 0.5, 1, 1.5))
  d <- sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + stat_sweep_3d(profile = star * 0.3, spline = "natural"))
  curve <- spline_curve_3d(as.matrix(path), 100L, "natural", FALSE)$xyz
  expect_equal(range(distance_to_curve(d, curve)), c(0.15, 0.3), tolerance = 1e-6)
  expect_true(closed_surface(d))
  clockwise <- sweep_data(ggplot2::ggplot(data.frame(x = 0:3, y = 0, z = 0), ggplot2::aes(x, y, z = z)) +
    stat_sweep_3d(profile = rbind(c(1, 0), c(0, -1), c(-1, 0), c(0, 1)), cap = "none"))
  centre <- cbind(clockwise$x, 0, 0)
  expect_true(all(rowSums((cbind(clockwise$x, clockwise$y, clockwise$z) - centre) *
    cbind(clockwise$nx, clockwise$ny, clockwise$nz)) > 0))
  expect_error(sweep_data(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + stat_sweep_3d(profile = matrix(1:3, 1))),
    "two-column matrix")
})

test_that("surfaces of revolution weld poles and wrap without a seam", {
  a <- seq(0, pi, length.out = 20)
  half <- data.frame(r = sin(a), h = cos(a))
  ball <- sweep_data(ggplot2::ggplot(half, ggplot2::aes(r = r, z = h)) + geom_revolve_3d(n = 24))
  expect_equal(sqrt(ball$x^2 + ball$y^2 + ball$z^2), rep(1, nrow(ball)), tolerance = 1e-9)
  expect_equal(length(unique(ball$.vertex)), 24L * 18L + 2L)
  expect_true(closed_surface(ball)); expect_equal(euler_number(ball), 2L)
  expect_true(all(ball$x * ball$nx + ball$y * ball$ny + ball$z * ball$nz < 0))
  b <- seq(0, 2 * pi, length.out = 13)[-13]
  torus <- sweep_data(ggplot2::ggplot(data.frame(r = 1 + 0.3 * cos(b), x = 0.3 * sin(b)), ggplot2::aes(r = r, x = x)) +
    geom_revolve_3d(axis = "x", n = 30, closed = TRUE, centre = c(1, 2)))
  expect_true(closed_surface(torus)); expect_equal(euler_number(torus), 0L)
  expect_equal(range(sqrt((torus$y - 1)^2 + (torus$z - 2)^2)), c(0.7, 1.3), tolerance = 1e-9)
  part <- sweep_data(ggplot2::ggplot(half, ggplot2::aes(r = r, z = h)) + geom_revolve_3d(n = 10, angle = c(0, 90)))
  expect_true(all(part$x > -1e-12 & part$y > -1e-12))
  expect_error(sweep_data(ggplot2::ggplot(half, ggplot2::aes(r = r, z = h)) + geom_revolve_3d(angle = c(0, 400))), "at most 360")
})

test_that("splines interpolate or approximate the points", {
  p <- data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 1, 0, 1, 0), z = c(0, 1, 0, -1, 0), w = c(1, 2, 3, 4, 5))
  layer <- function(...) sweep_data(ggplot2::ggplot(p, ggplot2::aes(x, y, z = z, linewidth = w)) + stat_spline_3d(...))
  near <- function(d) vapply(seq_len(nrow(p)), function(i) min(sqrt((d$x - p$x[i])^2 + (d$y - p$y[i])^2 + (d$z - p$z[i])^2)), numeric(1))
  cr <- layer(n = 13)
  expect_equal(near(cr), rep(0, 5), tolerance = 1e-12)
  expect_equal(nrow(cr), 13L)
  expect_true(all(diff(cr$linewidth) > 0))
  natural <- layer(n = 2000, type = "natural")
  expect_lt(max(near(natural)), 0.01)
  bs <- layer(n = 50, type = "bspline")
  expect_equal(unlist(bs[1, c("x", "y", "z")]), c(x = 0, y = 0, z = 0))
  expect_equal(unlist(bs[50, c("x", "y", "z")]), c(x = 4, y = 0, z = 0))
  expect_gt(near(bs)[3], 0.1)
  loop <- layer(n = 40, closed = TRUE)
  expect_equal(nrow(loop), 41L)
  expect_equal(unlist(loop[1, c("x", "y", "z")]), unlist(loop[41, c("x", "y", "z")]))
  for (type in c("natural", "bspline")) expect_equal(nrow(layer(n = 30, type = type, closed = TRUE)), 31L)
})
