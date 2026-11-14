glyph_volume <- function(mesh) {
  v <- mesh$vertices
  sum(unlist(lapply(mesh$faces, function(f) {
    vapply(seq.int(2L, ncol(f) - 1L), function(j) {
      a <- v[f[, 1L], , drop = FALSE]; b <- v[f[, j], , drop = FALSE]; c <- v[f[, j + 1L], , drop = FALSE]
      sum(rowSums(a * ggplot3d:::cross_rows_3d(b, c))) / 6
    }, numeric(1))
  })))
}

glyph_closed <- function(mesh) {
  directed <- do.call(rbind, lapply(mesh$faces, function(f) {
    k <- ncol(f); cbind(as.vector(f), as.vector(f[, c(seq.int(2L, k), 1L), drop = FALSE]))
  }))
  forward <- paste(directed[, 1L], directed[, 2L]); backward <- paste(directed[, 2L], directed[, 1L])
  !anyDuplicated(forward) && all(backward %in% forward)
}

test_that("built-in glyphs are closed, outward and span the unit box", {
  for (detail in 0:3) {
    ico <- ggplot3d:::glyph_mesh_3d("sphere", detail)
    expect_equal(nrow(ico$faces[[1L]]), 20L * 4L^detail)
    expect_equal(sqrt(rowSums(ico$vertices^2)), rep(1, nrow(ico$vertices)))
  }
  volumes <- c(sphere = 4 * pi / 3, cube = 8, octahedron = 4 / 3, cone = 2 * pi / 3, cylinder = 2 * pi)
  for (name in names(volumes)) {
    mesh <- ggplot3d:::glyph_mesh_3d(name, 3L)
    expect_true(glyph_closed(mesh), info = name)
    expect_equal(glyph_volume(mesh), volumes[[name]], tolerance = 0.02, info = name)
    expect_equal(unname(apply(mesh$vertices, 2L, range)), matrix(c(-1, 1), 2L, 3L), tolerance = 1e-12, info = name)
  }
  expect_error(ggplot3d:::glyph_mesh_3d("torus"), "glyph must be")
  expect_error(ggplot3d:::glyph_mesh_3d("sphere", 7), "detail")
})

test_that("user meshes become glyphs anchored at their origin", {
  v <- data.frame(x = c(0, 1, 0, 0), y = c(0, 0, 1, 0), z = c(0, 0, 0, 1))
  tetra <- mesh_3d(v, rbind(c(1, 3, 2), c(1, 2, 4), c(1, 4, 3), c(2, 3, 4)))
  mesh <- ggplot3d:::glyph_mesh_3d(tetra)
  expect_true(glyph_closed(mesh))
  expect_equal(abs(glyph_volume(mesh)), 1 / 6)
  s <- stats_scene(ggplot2::ggplot(data.frame(x = 1:2, y = 1:2, z = 1:2), ggplot2::aes(x, y, z = z)) +
    geom_glyph_3d(glyph = tetra, size = 2, size.unit = "data") + coord_3d())
  expect_length(s$packet$faces$primitive_id, 8L)
  b <- s$built$data[[1L]]
  expect_equal(b$xmax - b$x, c(1, 1)); expect_equal(b$xmin, b$x)
})

test_that("rotations carry the z axis onto each direction", {
  d <- rbind(c(1, 0, 0), c(0, 0, -2), c(1, 2, 3), c(0, 0, 0))
  r <- ggplot3d:::align_z_3d(d)
  for (i in 1:3) {
    expect_equal(crossprod(r[, , i]), diag(3))
    expect_equal(det(r[, , i]), 1)
    expect_equal(r[, 3L, i], d[i, ] / sqrt(sum(d[i, ]^2)))
  }
  expect_equal(r[, , 4L], diag(3))
})

test_that("data-unit spheres have their radius on every axis and train the scales", {
  d <- data.frame(x = c(0, 10), y = c(0, 1), z = c(0, 100), r = c(1, 2))
  s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, radius = r)) + geom_sphere_3d(detail = 1) + coord_3d())
  b <- s$built$data[[1L]]
  expect_equal(b$xmin, d$x - d$r); expect_equal(b$zmax, d$z + d$r)
  ranges <- s$built$layout$panel_params[[1L]]$ranges
  expect_true(ranges$y[1L] <= -1 && ranges$y[2L] >= 3)
  q <- s$packet$quadrics
  expect_identical(q$kind, c(1L, 1L))
  expect_length(s$packet$faces$primitive_id, 0L)
  span <- vapply(ranges, diff, numeric(1))
  linear <- ggplot3d:::quadric_linear_3d(q, 2L)[, , 1L]
  expect_equal(linear * span, diag(2, 3L), tolerance = 1e-12)
  centre <- (c(10, 1, 100) - vapply(ranges, `[`, numeric(1), 1L)) / span
  expect_equal(c(q$transform_13[2L], q$transform_14[2L], q$transform_15[2L]), unname(centre))
  expect_error(stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, radius = -r)) + geom_sphere_3d() + coord_3d()), "nonnegative")
  expect_error(geom_sphere_3d(radius.unit = "cm"), "radius.unit")
})

test_that("millimetre spheres take their on-screen radius from the fitted view", {
  d <- data.frame(x = 0.5, y = 0.5, z = 0.5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_sphere_3d(radius = 5, radius.unit = "mm", detail = 4) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) +
    coord_3d(projection = "orthographic", expand = FALSE, aspect = c(2, 1, 1))
  s <- integration_packets(p)
  packet <- s$grobs[[1L]]$packet
  expect_false(is.null(attr(packet, "quadric_mm")))
  camera <- s$params$camera
  viewport <- new_scene_viewport(120, 90, 96)
  resolved <- ggplot3d:::resolve_glyph_packets_3d(list(packet), camera, viewport)[[1L]]
  outline <- ggplot3d:::quadric_outline_cpp(resolved[names(resolved) != "styles"], camera, viewport, 1L, 0.001)
  expect_equal(diff(range(outline$x_npc * viewport$width_mm)), 10, tolerance = 1e-3)
  expect_equal(diff(range(outline$y_npc * viewport$height_mm)), 10, tolerance = 1e-3)
  again <- ggplot3d:::resolve_glyph_packets_3d(list(resolved), camera, viewport)[[1L]]
  expect_identical(again$quadrics, resolved$quadrics)
})

test_that("glyphs follow u, v, w and scale with size", {
  d <- data.frame(x = 0, y = 0, z = 0, u = 1, v = 0, w = 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w)) +
    geom_glyph_3d(glyph = "cone", size = 4, size.unit = "data", detail = 0) +
    ggplot2::lims(x = c(-4, 4), y = c(-4, 4)) + zlim(-4, 4) + coord_3d(expand = FALSE)
  s <- stats_scene(p)
  v <- scene_vertices_xyz_3d(s$packet$vertices)
  apex <- unname(v[nrow(v), ])
  expect_equal(apex, c(0.5 + 2 / 8, 0.5, 0.5))
  expect_equal(range(v[, 1L]), c(0.25, 0.75))
  expect_equal(s$built$data[[1L]]$xmax, 2)
  s <- stats_scene(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = qsec, size = hp, fill = factor(cyl))) +
    geom_glyph_3d(glyph = "cube") + coord_3d())
  expect_length(s$packet$faces$primitive_id, 6L * nrow(mtcars))
  expect_silent(validate_scene_packet(s$packet))
  expect_error(geom_glyph_3d(glyph = "star"), "glyph must be")
})

test_that("ellipsoids follow semi-axes, rotations and shape matrices", {
  sigma <- matrix(c(4, 1, 0, 1, 2, 0.5, 0, 0.5, 1), 3L)
  d <- data.frame(x = 0, y = 0, z = 0)
  d$sigma <- list(sigma)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, sigma = sigma)) + geom_ellipsoid_3d(detail = 2) + coord_3d()
  s <- stats_scene(p)
  ranges <- s$built$layout$panel_params[[1L]]$ranges
  span <- vapply(ranges, diff, numeric(1)); low <- vapply(ranges, `[`, numeric(1), 1L)
  v <- sweep(sweep(scene_vertices_xyz_3d(s$packet$vertices), 2L, span, "*"), 2L, low, "+")
  expect_equal(rowSums((v %*% solve(sigma)) * v), rep(1, nrow(v)), tolerance = 1e-10)
  b <- s$built$data[[1L]]
  expect_equal(c(b$xmax, b$ymax, b$zmax), sqrt(diag(sigma)))
  turn <- matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), 3L)
  e <- data.frame(x = 0, y = 0, z = 0, rx = 3, ry = 1, rz = 1)
  e$rotation <- list(turn)
  b <- ggplot2::ggplot_build(ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, rx = rx, ry = ry, rz = rz, rotation = rotation)) +
    geom_ellipsoid_3d() + coord_3d())$data[[1L]]
  expect_equal(c(b$xmax, b$ymax, b$zmax), c(1, 3, 1))
  expect_error(stats_scene(ggplot2::ggplot(e, ggplot2::aes(x, y, z = z)) + geom_ellipsoid_3d() + coord_3d()), "rx, ry and rz")
  e$rotation <- list(diag(c(1, 2, 1)))
  expect_error(stats_scene(ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, rx = rx, ry = ry, rz = rz, rotation = rotation)) +
    geom_ellipsoid_3d() + coord_3d()), "orthonormal")
})

test_that("glyphs stay rigid in curvilinear frames and render in every backend", {
  d <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(1, 2, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_sphere_3d(radius = 2, radius.unit = "mm", detail = 1)
  s <- stats_scene(p + coord_cylindrical_3d())
  expect_length(s$packet$quadrics$kind, 3L)
  skip_if_not_installed("ragg")
  for (backend in c("vector", "raster")) {
    grob <- ggplot2::ggplotGrob(p + coord_3d(backend = backend_3d(backend)))
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, 400, 300)
    expect_silent(grid::grid.draw(grob))
    grDevices::dev.off()
    unlink(path)
  }
})
