source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)
pathtrace_test_scene <- function(backend, packet = NULL, specs = list(), camera = NULL, light = NULL) {
  if (is.null(packet)) {packet <- ref_packet(faces = list(ref_quad(.3))); packet$styles$fill <- "white"}
  if (is.null(camera)) camera <- ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90)
  ggplot3d:::pathtrace_scene_3d(packet, specs, camera, ggplot3d:::new_scene_viewport(25.4, 25.4, 9), backend,
    light %||% light_3d(light_directional_3d(intensity = 0), ambient = 1))
}

pathtrace_test_volume <- function(sigma = 1, mode = 0L, g = 0, material = NULL) {
  list(kind = "volume", axes = rep(list(c(0, 1)), 3), value = rep(1, 8), knots = c(0, 2),
    sigma = rep(sigma, 2), colours = matrix(1, 2, 3), mode = mode, level = 1, samples = 16L,
    ranges = list(x = c(0, 1), y = c(0, 1), z = c(0, 1)), g = g, material = material)
}
