projective_test_space <- function() ggplot2::ggproto("ProjectiveTest", SpaceEuclidean, identity = FALSE,
  charts = "homogeneous", models = "homogeneous", default_model = "homogeneous",
  to_canonical = function(data, chart) as.matrix(data[c("x", "y", "z")]),
  from_canonical = function(X, model) cbind(X[, 1:2, drop = FALSE], 0, X[, 3L]),
  geodesic = function(X, Y, s) segment_points_3d(X, Y, s),
  chart_is_projective = function(model) TRUE,
  model_ranges = function(model, panel_params) list(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)))

projective_test_plot <- function(d, ...) {
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d(...) +
    coord_3d(space = projective_test_space(), input = "homogeneous", model = "homogeneous",
      xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1), expand = FALSE,
      axes = character(), panes = character(), box = "none")
}

test_that("Klein packets carry w = X0 and divide to the Klein coordinates", {
  d <- data.frame(x = c(-0.7, 0.7, 0.2), y = c(0.2, 0.2, -0.5), z = c(0, 0.1, 0.3))
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() + coord_hyperbolic_3d())$packet
  expect_true(any(abs(packet$vertices$w - 1) > 1e-3))
  X <- SpaceHyperbolicFinite$to_canonical(d, "klein")
  expect_equal(packet$vertices$w, X[, 4], tolerance = 1e-12)
  xyz <- scene_vertices_xyz_3d(packet$vertices) * 2 - 1
  expect_equal(unname(xyz), unname(as.matrix(d)), tolerance = 1e-12)
})

test_that("ideal points of H^3 are boundary points of the ball models", {
  s <- SpaceHyperbolicFinite
  P <- data.frame(x = c(0.3, 1, 0), y = c(0.1, 0, 0.6), z = c(0, 0, 0.8))
  for (chart in c("klein", "poincare")) {
    X <- s$to_canonical(P, chart)
    expect_equal(unname(hyperbolic_ideal_3d(X)), c(FALSE, TRUE, TRUE))
    expect_equal(unname(space_model_xyz_3d(X, s, chart)), unname(as.matrix(P)), tolerance = 1e-12)
    expect_equal(s$distance(X[1, , drop = FALSE], X[2, , drop = FALSE]), Inf)
    Z <- s$geodesic(X[c(1, 2), , drop = FALSE], X[c(2, 3), , drop = FALSE], 0.5)
    expect_equal(-lorentz_dot_3d(Z, Z), c(1, 1), tolerance = 1e-12)
    expect_equal(s$geodesic(X[1, , drop = FALSE], X[2, , drop = FALSE], 1), X[2, , drop = FALSE])
  }
  expect_error(s$to_canonical(data.frame(x = 1.2, y = 0, z = 0), "klein"), "closed unit ball")
  d <- data.frame(x = c(0, 1), y = 0, z = 0)
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() +
    coord_hyperbolic_3d(model = "poincare", input = "klein"))$packet
  xyz <- scene_vertices_xyz_3d(packet$vertices) * 2 - 1
  expect_equal(max(xyz[, 1]), 1, tolerance = 1e-12)
  expect_true(all(abs(xyz[, 2:3]) < 1e-12))
})

test_that("ideal corners from a space reach the panel box through homogeneous clipping", {
  d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(1, 0, 0))
  p <- projective_test_plot(d)
  scene <- integration_packets(p)
  packet <- scene$packet
  expect_equal(packet$vertices$w, c(1, 0, 0))
  compiled <- scene_compile(packet, scene$params$camera, new_scene_viewport(100, 100, 144))
  v <- compiled$vertices
  expect_true(all(is.finite(v$x)))
  expect_equal(max(v$x), 1, tolerance = 1e-12)
  expect_equal(range(v$x), c(0.5, 1), tolerance = 1e-12)
  expect_true(all(abs(v$y - 0.5) < 1e-12))
  camera <- scene$params$camera
  camera$clip_data <- FALSE; camera$clip <- FALSE
  expect_error(scene_compile(packet, camera, new_scene_viewport(100, 100, 144)), "unbounded geometry")
  expect_equal(unname(space_model_homogeneous_3d(matrix(c(1, 0, -1), 1), projective_test_space(), "homogeneous")), matrix(c(1, 0, 0, -1), 1))
  expect_error(space_model_homogeneous_3d(matrix(0, 1, 3), projective_test_space(), "homogeneous"), "nonzero")
  for (backend in c("vector", "raster")) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 300, 300, res = 72)
    tryCatch(expect_no_error(print(p + coord_3d(space = projective_test_space(), input = "homogeneous",
      model = "homogeneous", xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1), expand = FALSE,
      backend = backend_3d(backend, dpi = 72, antialias = 1)))), finally = grDevices::dev.off())
    expect_gt(file.info(file)$size, 1000)
    unlink(file)
  }
})

test_that("geodesic sampling measures screen error after the camera pre-transform", {
  context <- list(space = SpaceHyperbolicFinite, model = "poincare", ranges = list(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)),
    frame = FrameBox, tolerance = 1e-3, max_depth = 9L, viewport = new_scene_viewport(100, 100))
  X <- SpaceHyperbolicFinite$to_canonical(data.frame(x = c(-0.6, 0.6), y = 0.4, z = 0), "poincare")
  context$camera <- new_camera_3d()
  base <- nrow(polyline_model_3d(space_subdivision_3d(context), X, 9L)$model)
  zoom <- diag(4); zoom[1:3, 1:3] <- 4 * diag(3); zoom[1:3, 4] <- -1.5
  context$camera <- new_camera_3d(pre = zoom)
  expect_gt(nrow(polyline_model_3d(space_subdivision_3d(context), X, 9L)$model), base)
  xyzw <- cbind(rbind(c(0.5, 0.5, 0.5), c(2, 2, 2)), c(1, 0))
  expect_equal(screen_positions_homogeneous_3d(xyzw, new_camera_3d(), frame_basis_3d(new_camera_3d(), new_scene_viewport(100, 100)))[2, ],
               c(Inf, Inf))
})

test_that("affine frames map homogeneous rows without division", {
  frame <- list(linear = TRUE, map = function(p) sweep(p %*% diag(c(2, 1, 0.5)), 2L, c(0.1, 0.2, 0.3), "+"))
  xyzw <- rbind(c(0.2, 0.4, 0.6, 2), c(1, 0, 0, 0))
  out <- frame_map_homogeneous_3d(frame, xyzw)
  expect_equal(out[1, 1:3] / out[1, 4], drop(frame$map(matrix(xyzw[1, 1:3] / 2, 1))), tolerance = 1e-12)
  expect_equal(out[2, ], c(2, 0, 0, 0))
})
