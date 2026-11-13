test_that("surface materials carry averaged vertex normals through panel scaling", {
  d <- expand.grid(x = c(0, 2, 4), y = c(0, 3, 6))
  d$z <- 2 * d$x + 3 * d$y
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d() + coord_3d(aspect = c(2, 1, 3))
  s <- stats_scene(p)
  b <- s$built$data[[1L]]
  expect_equal(as.matrix(b[c("nx", "ny", "nz")]),
    matrix(rep(c(-2, -3, 1) / sqrt(14), each = nrow(b)), ncol = 3), ignore_attr = TRUE)
  panel <- s$built$layout$panel_params[[1L]]
  expected <- c(-2, -3, 1) * vapply(panel$ranges[c("x", "y", "z")], diff, numeric(1))
  expected <- expected / sqrt(sum(expected^2))
  styles <- s$packet$styles[s$packet$faces$style_id, ]
  expect_equal(as.matrix(styles[c("normal_x", "normal_y", "normal_z")]),
    matrix(rep(expected, each = nrow(styles)), ncol = 3), ignore_attr = TRUE)
  camera <- list(model_scale = c(1, 0.5, 1.5), forward = c(0, 0, -1))
  light <- light_3d(direction = c(0, 0, 1), space = "data", ambient = 0, diffuse = 1)
  normal <- expected / camera$model_scale
  normal <- normal / sqrt(sum(normal^2))
  f <- list(normal_x = 1, normal_y = 0, normal_z = 0)
  actual <- shade_fill_3d("white", f, 1L, camera, light, styles[1, ])
  reference <- shade_fill_3d("white", as.list(stats::setNames(normal,
    c("normal_x", "normal_y", "normal_z"))), 1L, camera, light)
  expect_identical(actual, reference)
})

test_that("isosurface lighting preserves face means in both backends", {
  a <- seq(-1, 1, length.out = 7)
  d <- expand.grid(x = a, y = a, z = a)
  d$value <- with(d, x^2 + y^2 + z^2)
  s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
    geom_isosurface_3d(levels = 0.7) + coord_3d())
  b <- s$built$data[[1L]]
  normal <- as.matrix(b[c("nx", "ny", "nz")])
  mean <- sapply(seq_len(3), function(j) colMeans(matrix(normal[, j], nrow = 3)))
  mean <- mean / sqrt(rowSums(mean^2))
  styles <- s$packet$styles[s$packet$faces$style_id, ]
  expect_equal(as.matrix(styles[c("normal_x", "normal_y", "normal_z")]), mean, ignore_attr = TRUE)
  camera <- new_camera_3d(aspect = c(2, 1, 3))
  viewport <- new_scene_viewport(60, 60, dpi = 72)
  vector <- scene_compile(s$packet, camera, viewport)
  prepared <- ggplot3d:::raster_prepare_cpp(s$packet[names(s$packet) != "styles"], camera, viewport)
  rgba <- raster_colours_3d(s$packet, prepared, light_3d())[[1L]]
  rows <- which(vector$fragments$kind == 1L)
  st <- s$packet$styles[vector$fragments$style_id[rows], ]
  fill <- shade_fill_3d(st$fill, vector$fragments, rows, vector$camera, light_3d(), st)
  expect_equal(colour_channels_3d(fill) / 255,
    rgba[1:3, vector$fragments$source_index[rows], drop = FALSE], ignore_attr = TRUE)
})

test_that("contour defaults use interior pretty levels", {
  bounds <- c(0.13, 0.87)
  expected <- pretty(bounds, n = 10)
  expect_equal(contour_levels_3d(bounds, NULL, NULL, NULL),
    expected[expected > bounds[1] & expected < bounds[2]])
  expect_equal(contour_levels_3d(bounds, NULL, 2, NULL), bounds[1] + diff(bounds) * c(1/3, 2/3))
})
