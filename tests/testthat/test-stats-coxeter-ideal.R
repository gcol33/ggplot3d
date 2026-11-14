coxeter_lorentz_3d <- function(X, simplex) rowSums((X %*% simplex$J) * X)

test_that("simplex vertices and cell centres are classified by their Lorentz norm", {
  expect_equal(coxeter_simplex_3d(c(5, 3, 4))$points, rep("finite", 4))
  cases <- list(c(3, 3, 6), c(3, 3, 7), c(6, 3, 3), c(7, 3, 3), c(4, 4, 4))
  expected <- list(c("ideal", "finite"), c("hyperideal", "finite"), c("finite", "ideal"), c("finite", "hyperideal"), c("ideal", "ideal"))
  for (i in seq_along(cases)) {
    s <- coxeter_simplex_3d(cases[[i]])
    expect_equal(s$points[c(1, 4)], expected[[i]])
    norm <- coxeter_lorentz_3d(s$vertices, s)
    expect_equal(sign(round(norm, 9))[c(1, 4)], c(ideal = 0, finite = -1, hyperideal = 1)[expected[[i]]], ignore_attr = TRUE)
    for (M in s$mirrors) expect_equal(t(M) %*% s$J %*% M, s$J, tolerance = 1e-9)
  }
  expect_equal(coxeter_simplex_3d(c(5, Inf))$points[1], "ideal")
  expect_equal(coxeter_simplex_3d(c(Inf, 3))$points[3], "ideal")
})

test_that("ideal vertices are light-cone points on the boundary of the Klein ball", {
  out <- suppressMessages(coxeter_cells_3d(c(3, 3, 6), 0, 1L))
  s <- out$simplex
  expect_equal(c(nrow(out$vertices), length(out$faces)), c(4L, 4L))
  expect_equal(coxeter_lorentz_3d(out$vertices, s), rep(0, 4), tolerance = 1e-9)
  expect_equal(out$vertices[, 4], rep(1, 4))
  expect_equal(rowSums(out$vertices[, 1:3]^2), rep(1, 4), tolerance = 1e-9)
  p <- ggplot2::ggplot() + stat_honeycomb_3d(schlafli = c(3, 3, 6), radius = 0, what = "faces") +
    coord_3d(axes = character(), panes = character(), box = "none")
  packet <- suppressMessages(stats_scene(p)$packet)
  xyz <- scene_vertices_xyz_3d(packet$vertices) * 2 - 1
  expect_equal(max(sqrt(rowSums(xyz^2))), 1, tolerance = 1e-9)
  expect_silent(validate_scene_packet(packet))
  ball <- suppressMessages(stats_scene(p + stat_honeycomb_3d(schlafli = c(3, 3, 6), radius = 0, what = "faces", model = "poincare"))$packet)
  expect_silent(validate_scene_packet(ball))
  X <- s$vertices[1, , drop = FALSE]; Y <- out$vertices[1:2, , drop = FALSE]
  Z <- hyperbolic_geodesic_3d(matrix(c(0, 0, 0, 1), 2, 4, byrow = TRUE), Y, c(0.25, 0.75))
  expect_equal(-lorentz_dot_3d(Z, Z), c(1, 1), tolerance = 1e-12)
  P <- space_model_xyz_3d(hyperbolic_geodesic_3d(Y[c(1, 1, 1), ], Y[c(2, 2, 2), ], c(1e-4, 2e-4, 3e-4)), SpaceHyperbolicFinite, "poincare")
  expect_equal(P[3, ] - P[2, ], P[2, ] - P[1, ], tolerance = 1e-3)
  tiling <- suppressMessages(coxeter_cells_3d(c(5, Inf), 0, 1L))
  expect_equal(coxeter_lorentz_3d(tiling$vertices, tiling$simplex), rep(0, 5), tolerance = 1e-9)
})

test_that("hyperideal vertices are truncated by their polar planes", {
  out <- suppressMessages(coxeter_cells_3d(c(3, 3, 7), 0, 1L))
  s <- out$simplex
  edges <- do.call(rbind, lapply(out$faces, function(f) cbind(f, c(tail(f, -1), f[1]))))
  edges <- unique(cbind(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2])))
  expect_equal(c(nrow(out$vertices), length(out$faces), nrow(edges)), c(12L, 8L, 18L))
  expect_equal(sort(lengths(out$faces)), c(3L, 3L, 3L, 3L, 6L, 6L, 6L, 6L))
  expect_equal(coxeter_lorentz_3d(out$vertices, s), rep(-1, 12), tolerance = 1e-9)
  ideal <- coxeter_orbit_3d(s$vertices[1, ], s$mirrors[1:3], s, 4L)
  for (face in out$faces[lengths(out$faces) == 3L]) {
    products <- abs(out$vertices[face, ] %*% s$J %*% t(ideal))
    expect_true(any(apply(products, 2L, function(x) all(x < 1e-9))))
  }
  p <- ggplot2::ggplot() + stat_honeycomb_3d(schlafli = c(3, 3, 7), radius = 0.5, what = "edges") +
    coord_3d(axes = character(), panes = character(), box = "none")
  packet <- suppressMessages(stats_scene(p)$packet)
  expect_true(all(rowSums((scene_vertices_xyz_3d(packet$vertices) * 2 - 1)^2) < 1))
  expect_silent(validate_scene_packet(packet))
})

test_that("non-compact cells are generated face by face within radius", {
  for (symbol in list(c(6, 3, 3), c(7, 3, 3), c(4, 4, 4))) {
    tiling <- suppressMessages(coxeter_cells_3d(symbol, 2, 2000L))
    expect_identical(tiling$mode, "faces")
    s <- tiling$simplex
    centres <- t(vapply(tiling$pieces, function(piece) coxeter_normalise_3d(colMeans(piece$X), s), numeric(4)))
    expect_true(all(acosh(pmax(1, centres[, 4])) <= 2 + 1e-8))
    expect_false(anyDuplicated(apply(centres, 1L, coxeter_key_3d)) > 0)
    expect_true(all(lengths(lapply(tiling$pieces, function(piece) piece$X)) / 4 == symbol[1]))
    faces <- coxeter_pieces_3d(tiling, "faces"); cells <- coxeter_pieces_3d(tiling, "cells")
    expect_equal(length(cells), 2L * length(faces))
    expect_gt(length(unique(vapply(cells, `[[`, integer(1), "cell"))), 2L)
  }
  expect_message(coxeter_cells_3d(c(6, 3, 3), 1, 2000L), "faces in hyperbolic 3-space")
  both <- suppressMessages(coxeter_cells_3d(c(7, 3, 7), 2, 2000L))
  sizes <- vapply(both$pieces, function(piece) nrow(piece$X), integer(1))
  expect_setequal(unique(sizes), c(14L, 3L))
  points <- do.call(rbind, lapply(both$pieces, `[[`, "X"))
  expect_equal(coxeter_lorentz_3d(points, both$simplex), rep(-1, nrow(points)), tolerance = 1e-9)
  expect_error(suppressMessages(coxeter_cells_3d(c(6, 3, 3), 3, 5L)), "max_cells")
  edges <- suppressMessages(coxeter_cells_3d(c(Inf, 3), 2, 2000L))
  expect_true(all(vapply(edges$pieces, function(piece) nrow(piece$X), integer(1)) == 2L))
  triangles <- coxeter_pieces_3d(edges, "faces")
  expect_equal(length(triangles), 2L * length(edges$pieces))
  centre <- t(vapply(triangles, function(piece) piece$X[3, ], numeric(3)))
  expect_equal(coxeter_lorentz_3d(centre, edges$simplex), rep(0, nrow(centre)), tolerance = 1e-9)
  p <- ggplot2::ggplot() + stat_tiling_3d(schlafli = c(Inf, 3), radius = 1.5, what = "faces") +
    coord_3d(axes = character(), panes = character(), box = "none")
  packet <- suppressMessages(stats_scene(p)$packet)
  expect_gt(length(packet$faces$offset), 0L)
  expect_silent(validate_scene_packet(packet))
  for (what in c("edges", "faces", "cells")) for (model in c("klein", "poincare")) {
    p <- ggplot2::ggplot() + stat_honeycomb_3d(schlafli = c(6, 3, 3), radius = 0.8, what = what, model = model) +
      coord_3d(axes = character(), panes = character(), box = "none")
    packet <- suppressMessages(stats_scene(p)$packet)
    expect_silent(validate_scene_packet(packet))
    expect_true(all(rowSums((scene_vertices_xyz_3d(packet$vertices) * 2 - 1)^2) <= 1 + 1e-9))
  }
})
