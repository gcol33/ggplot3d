pole_compile <- function(p) {
  scene <- integration_packets(p)
  list(packet = scene$packet, compiled = scene_compile(scene$packet, scene$params$camera, new_scene_viewport(100, 100, 144)))
}

test_that("stereographic coordinates are homogeneous with the pole at infinity", {
  s <- SpaceSpherical3D
  N <- matrix(c(0, 0, 0, 1), 1)
  expect_equal(unname(s$from_canonical(N, "stereographic")), matrix(0, 1, 4))
  X <- s$to_canonical(data.frame(x = 40, y = 0, z = 0), "stereographic")
  h <- space_model_homogeneous_3d(X, s, "stereographic")
  expect_equal(unname(h[1, 1:3] / h[1, 4]), c(40, 0, 0), tolerance = 1e-10)
  expect_lt(h[1, 4], 2e-3)
  expect_equal(spherical_pole_radius_3d(3), 2 * atan(1 / (6 * sqrt(3))))
  expect_equal(1 / tan(spherical_pole_radius_3d(3) / 2), 2 * sqrt(3) * 3, tolerance = 1e-12)
  expect_false(s$visible(N, "stereographic", NULL))
  expect_true(s$visible(X * 0 + c(1, 0, 0, 0), "stereographic", NULL))
})

test_that("faces through the stereographic pole are drawn up to the panel box", {
  d <- data.frame(x = c(2.5, 0, -2.5), y = c(0, 0.5, 0), z = 0)
  X <- SpaceSpherical3D$to_canonical(d, "stereographic")
  expect_equal(unname(sphere_slerp_3d(X[1, , drop = FALSE], X[3, , drop = FALSE], 0.5)), matrix(c(0, 0, 0, 1), 1), tolerance = 1e-12)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_polygon_3d(fill = "steelblue", colour = NA) +
    coord_spherical_geometry_3d(extent = 2)
  out <- pole_compile(p)
  expect_true(all(out$packet$vertices$w > 0))
  v <- out$compiled$vertices
  expect_equal(range(v$x), c(0, 1), tolerance = 1e-9)
  expect_equal(range(v$y), c(0.625, 1), tolerance = 1e-6)
  expect_true(all(abs(v$z - 0.5) < 1e-9))
})

test_that("Hopf tori near the pole and the pole fibre render in both backends", {
  d <- data.frame(lon = c(0, 0), lat = c(-75, -90))
  torus <- ggplot2::ggplot(d[1, ], ggplot2::aes(lon = lon, lat = lat)) +
    stat_hopf_fibre_3d(what = "torus", n = 24, fill = "steelblue", colour = NA) + coord_spherical_geometry_3d(extent = 2)
  out <- pole_compile(torus)
  expect_gt(length(out$compiled$draw_order), 0L)
  expect_true(all(out$compiled$vertices$x >= -1e-9 & out$compiled$vertices$x <= 1 + 1e-9))
  fibre <- ggplot2::ggplot(d[2, ], ggplot2::aes(lon = lon, lat = lat)) +
    stat_hopf_fibre_3d(radius = 0, n = 32) + coord_spherical_geometry_3d(extent = 2)
  line <- integration_packets(fibre)$packet
  xyz <- scene_vertices_xyz_3d(line$vertices)
  expect_equal(range(xyz[, 3]), c(0, 1), tolerance = 1e-9)
  expect_true(all(abs(xyz[, 1:2] - 0.5) < 1e-9))
  for (backend in c("vector", "raster")) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 240, 240, res = 72)
    tryCatch(expect_no_error(print(torus + coord_spherical_geometry_3d(extent = 2, backend = backend_3d(backend, dpi = 72, antialias = 1)))),
      finally = grDevices::dev.off())
    expect_gt(file.info(file)$size, 1000)
    unlink(file)
  }
})
