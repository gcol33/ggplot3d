torus_mesh_test <- function(nu, nv, R = 1, r = 0.4) {
  g <- expand.grid(i = 0:(nu - 1), j = 0:(nv - 1))
  u <- 2 * pi * g$i / nu; v <- 2 * pi * g$j / nv
  vertices <- data.frame(x = (R + r * cos(v)) * cos(u), y = (R + r * cos(v)) * sin(u), z = r * sin(v))
  id <- function(i, j) as.integer((i %% nu) + nu * (j %% nv) + 1L)
  mesh_3d(vertices, rbind(cbind(id(g$i, g$j), id(g$i + 1, g$j), id(g$i + 1, g$j + 1)),
    cbind(id(g$i, g$j), id(g$i + 1, g$j + 1), id(g$i, g$j + 1))))
}

plane_mesh_test <- function(n = 30, jitter = 0.2) {
  g <- expand.grid(i = 0:n, j = 0:n)
  set.seed(7)
  vertices <- data.frame(x = (g$i + stats::runif(nrow(g), -jitter, jitter)) / n,
    y = (g$j + stats::runif(nrow(g), -jitter, jitter)) / n, z = 0)
  id <- function(i, j) as.integer(i + (n + 1) * j + 1L)
  q <- expand.grid(i = 0:(n - 1), j = 0:(n - 1))
  mesh_3d(vertices, rbind(cbind(id(q$i, q$j), id(q$i + 1, q$j), id(q$i + 1, q$j + 1)),
    cbind(id(q$i, q$j), id(q$i + 1, q$j + 1), id(q$i, q$j + 1))))
}

sphere_mesh_test <- function(n = c(48, 25)) {
  sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
  m <- ggplot2::layer_data(ggplot2::ggplot() +
    stat_parametric_3d(fun = sphere, v = c(0, pi), n = n, closed = c(TRUE, FALSE)) + coord_3d())
  m[c("x", "y", "z", ".face", ".corner", ".vertex")]
}

test_that("heat-method distance matches the plane and the sphere", {
  plane <- plane_mesh_test()
  centre <- c(0.5, 0.5, 0)
  d <- mesh_geodesic_distance(plane, centre)
  truth <- sqrt((d$x - 0.5)^2 + (d$y - 0.5)^2)
  inner <- truth < 0.35
  expect_lt(max(abs(d$distance - truth)[inner]), 0.03)
  expect_lt(mean(abs(d$distance - truth)[inner]), 0.015)

  s <- mesh_geodesic_distance(sphere_mesh_test(c(64, 33)), c(0, 0, 1))
  truth <- acos(pmin(1, pmax(-1, s$z)))
  expect_lt(max(abs(s$distance - truth)), 0.006)
  expect_lt(mean(abs(s$distance - truth)), 0.003)
})

test_that("geodesic distance takes the nearest of several sources and leaves sourceless parts NA", {
  plane <- plane_mesh_test(20)
  d <- mesh_geodesic_distance(plane, rbind(c(0.2, 0.5, 0), c(0.8, 0.5, 0)))
  truth <- pmin(sqrt((d$x - 0.2)^2 + (d$y - 0.5)^2), sqrt((d$x - 0.8)^2 + (d$y - 0.5)^2))
  expect_lt(max(abs(d$distance - truth)[truth < 0.25]), 0.04)

  far <- plane; far$x <- far$x + 5; far$.mesh <- 2L; plane$.mesh <- 1L
  both <- rbind(plane, far)
  out <- mesh_geodesic_distance(both, c(0.5, 0.5, 0))
  expect_true(all(is.na(out$distance[out$.mesh == 2L])))
  expect_false(anyNA(out$distance[out$.mesh == 1L]))
  for (boundary in c("neumann", "dirichlet")) {
    d <- mesh_geodesic_distance(plane, c(0.5, 0.5, 0), boundary = boundary)
    truth <- sqrt((d$x - 0.5)^2 + (d$y - 0.5)^2)
    expect_lt(max(abs(d$distance - truth)[truth < 0.25]), 0.03)
  }
})

test_that("FlipOut paths are straight on the plane and great circles on the sphere", {
  plane <- plane_mesh_test()
  vertices <- unique(as.data.frame(plane)[c("x", "y", "z")])
  a <- unlist(vertices[which.min((vertices$x - 0.07)^2 + (vertices$y - 0.1)^2), ])
  b <- unlist(vertices[which.min((vertices$x - 0.9)^2 + (vertices$y - 0.83)^2), ])
  p <- mesh_geodesic_path(plane, a, b)
  expect_equal(attr(p, "length"), sqrt(sum((a - b)^2)), tolerance = 1e-10)
  expect_equal(max(p$s), sqrt(sum((a - b)^2)), tolerance = 1e-10)
  cross <- (p$x - a[1]) * (b[2] - a[2]) - (p$y - a[2]) * (b[1] - a[1])
  expect_lt(max(abs(cross)), 1e-9)

  sphere <- sphere_mesh_test(c(64, 33))
  p <- mesh_geodesic_path(sphere, c(1, 0, 0), c(0, 1, 0))
  expect_equal(attr(p, "length"), pi / 2, tolerance = 2e-3)
  expect_lt(max(abs(p$z)), 0.02)
  expect_equal(p$s[nrow(p)], attr(p, "length"), tolerance = 1e-8)
})

test_that("paths and heat distance agree with the outer equator of a torus", {
  torus <- torus_mesh_test(120, 48)
  angle <- 2 * pi * 12 / 120
  to <- c(1.4 * cos(angle), 1.4 * sin(angle), 0)
  p <- mesh_geodesic_path(torus, c(1.4, 0, 0), to)
  expect_equal(attr(p, "length"), 1.4 * angle, tolerance = 2e-3)
  expect_lt(max(abs(p$z)), 0.01)
  # On a regular grid the heat time t = h^2 scales with the grid, so the
  # relative error along the grid lines stays near 4% under refinement.
  d <- mesh_geodesic_distance(torus, c(1.4, 0, 0))
  at <- which.min((d$x - to[1])^2 + (d$y - to[2])^2 + d$z^2)
  expect_equal(d$distance[at], attr(p, "length"), tolerance = 0.08)
  d <- mesh_geodesic_distance(torus, c(1.4, 0, 0), time = 4)
  expect_equal(d$distance[at], attr(p, "length"), tolerance = 0.03)
})

test_that("the log map gives polar coordinates on the plane and the sphere", {
  plane <- plane_mesh_test(40)
  l <- mesh_logmap(plane, c(0.5, 0.5, 0), direction = c(1, 0, 0))
  nearest <- which.min((l$x - 0.5)^2 + (l$y - 0.5)^2)
  source <- c(l$x[nearest], l$y[nearest])
  dx <- l$x - source[1]; dy <- l$y - source[2]; r <- sqrt(dx^2 + dy^2)
  inner <- r > 0.05 & r < 0.35
  angle <- abs((atan2(l$logmap_v, l$logmap_u) - atan2(dy, dx) + pi) %% (2 * pi) - pi)
  expect_lt(mean(angle[inner]), 0.1)
  expect_lt(max(abs(sqrt(l$logmap_u^2 + l$logmap_v^2) - r)[inner]), 0.06)

  s <- mesh_logmap(sphere_mesh_test(c(64, 33)), c(0, 0, 1))
  colatitude <- acos(pmin(1, pmax(-1, s$z)))
  keep <- colatitude > 0.1 & colatitude < 2.5
  expect_lt(max(abs(sqrt(s$logmap_u^2 + s$logmap_v^2) - colatitude)[keep]), 0.06)
})

test_that("geodesic stats add per-face values and path polylines", {
  m <- sphere_mesh_test(c(24, 13))
  m <- mesh_3d(unique(m[c("x", "y", "z", ".vertex")]), data.frame(.face = m$.face, .corner = m$.corner, .vertex = m$.vertex))
  p <- ggplot2::ggplot(m) +
    stat_geodesic_distance_3d(ggplot2::aes(fill = ggplot2::after_stat(distance)), source = c(0, 0, 1)) +
    stat_logmap_3d(source = c(0, 0, 1)) +
    stat_geodesic_path_3d(from = c(1, 0, 0), to = c(0, 1, 0), colour = "red") +
    coord_3d()
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[1]]
  expect_true(all(tapply(d$distance, d$.face, function(x) diff(range(x))) == 0))
  expect_true(all(c("logmap_u", "logmap_v") %in% names(b$data[[2]])))
  path <- b$data[[3]]
  expect_true(all(c("s", "length") %in% names(path)))
  expect_equal(path$length[1], pi / 2, tolerance = 0.02)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_error(mesh_logmap(m, rbind(c(0, 0, 1), c(1, 0, 0))), "single point")
  expect_error(mesh_geodesic_distance(m, c(0, 0)), "source must be")
})

test_that("parametric geodesics follow great circles and keep Clairaut's invariant", {
  sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
  g <- parametric_geodesics_3d(sphere, c(0, pi / 2), pi / 4, length = 3, n = 60, list())
  tangent <- c(0, cos(pi / 4), -sin(pi / 4))
  truth <- outer(cos(g$s), c(1, 0, 0)) + outer(sin(g$s), tangent)
  expect_lt(max(abs(as.matrix(g[c("x", "y", "z")]) - truth)), 1e-5)

  torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
  g <- parametric_geodesics_3d(torus, c(0, 0), 0.7, length = 10, n = 2000, list())
  xyz <- as.matrix(g[c("x", "y", "z")])
  step <- diff(g$s)[1]
  tangent <- (xyz[-(1:2), ] - xyz[seq_len(nrow(xyz) - 2L), ]) / (2 * step)
  mid <- xyz[2:(nrow(xyz) - 1L), ]
  expect_lt(max(abs(sqrt(rowSums(tangent^2)) - 1)), 1e-4)
  # Clairaut: distance from the axis times the cosine of the angle with the parallel.
  clairaut <- tangent[, 2] * mid[, 1] - tangent[, 1] * mid[, 2]
  expect_lt(diff(range(clairaut)), 1e-4)
  expect_equal(clairaut[1], 1.4 * cos(0.7), tolerance = 1e-4)

  layer <- ggplot2::layer_data(ggplot2::ggplot() +
    stat_parametric_geodesic_3d(fun = torus, start = c(0, 0), angle = c(0.3, 0.9), length = 2, n = 50) + coord_3d())
  expect_equal(nrow(layer), 102L)
  expect_setequal(unique(layer$group), 1:2)
})
