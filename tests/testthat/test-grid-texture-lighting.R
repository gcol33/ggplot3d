texture_lighting_case <- function(material, light, smooth = FALSE, triangles = FALSE) {
  texture <- texture_3d(matrix(c("#102030", "#A08040"), 1L), interpolate = FALSE)
  packet <- texture_square_packet(texture)
  packet$styles$shade <- TRUE
  packet$styles$material_id <- 1L
  packet$materials <- material_table_3d(material)
  if (smooth) {
    packet$corners$normal_x <- c(-.7, .4, .6, -.3)
    packet$corners$normal_y <- c(-.2, -.5, .1, .7)
    packet$corners$normal_z <- rep(1, 4)
    packet$corners <- packet$corners[c("vertex", "source_row", "normal_x", "normal_y", "normal_z", "u", "v")]
  }
  if (triangles) {
    packet$corners <- lapply(packet$corners, `[`, c(1L, 2L, 3L, 1L, 3L, 4L))
    face <- texture_square_packet(texture)$faces
    packet$faces <- lapply(face, rep, 2L)
    packet$faces$primitive_id <- 1:2
    packet$faces$offset <- c(1L, 4L)
    packet$faces$count <- c(3L, 3L)
  }
  list(packet = packet, light = light)
}

texture_lighting_compare <- function(case, camera = new_camera_3d(elevation = 90, azimuth = -90)) {
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  reference_path <- texture_reference_png(case$packet, camera, viewport, 100L, case$light, blend = "srgb")
  on.exit(unlink(reference_path), add = TRUE)
  reference <- png::readPNG(reference_path, native = TRUE)
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  ragg::agg_png(path, 100, 100, res = viewport$dpi, background = "transparent")
  grob <- render_scene_grid(scene_compile(case$packet, camera, viewport), viewport, light = case$light)
  grid::grid.draw(grob)
  grDevices::dev.off()
  vector <- image_channels(path)
  raster <- image_channels(reference)
  at <- expand.grid(row = 5:96, col = 5:96)
  alpha <- round(png::readPNG(reference_path)[, , 4L] * 255)
  inside <- rep(TRUE, nrow(at))
  for (dy in -2:2) for (dx in -2:2)
    inside <- inside & alpha[cbind(at$row + dy, at$col + dx)] == 255L
  at <- at[inside, , drop = FALSE]
  expect_gt(nrow(at), 100L)
  difference <- vapply(1:3, function(k) abs(vector[, , k][as.matrix(at)] - raster[, , k][as.matrix(at)]), numeric(nrow(at)))
  expect_lte(max(difference), 3)
  list(vector = vector, raster = raster, grob = grob,
    alpha = round(255 * png::readPNG(path)[, , 4L]))
}

test_that("textured lighting matches raster colour and highlights", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  coloured <- lights_3d(light_directional(c(0, 0, 1), space = "data", colour = "#FF5030", intensity = .7), ambient = .05)
  texture_lighting_compare(texture_lighting_case(material_3d(), coloured))
  highlight <- lights_3d(light_head(intensity = .65), ambient = .1)
  painted <- texture_lighting_compare(texture_lighting_case(material_3d("shiny"), highlight))
  expect_gt(min(painted$vector[50, 35, ]), 150)
  texture_lighting_compare(texture_lighting_case(material_3d("shiny"), coloured, smooth = TRUE))
  point <- lights_3d(light_point(c(.7, .3, 1.5), colour = "#40A0FF"), light_head(intensity = .2), ambient = .05)
  texture_lighting_compare(texture_lighting_case(material_3d("metal"), point, smooth = TRUE),
    new_camera_3d("perspective", azimuth = -35, elevation = 35, fov = 60, zoom = 1.4))
})

test_that("one lighting group spans a triangulated face run without hairlines", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  light <- lights_3d(light_head(colour = "#70A0FF", intensity = .5), ambient = .1)
  case <- texture_lighting_case(material_3d("shiny"), light, triangles = TRUE)
  case$packet$styles$texture[[1L]] <- texture_3d(matrix("#506070"))
  painted <- texture_lighting_compare(case)
  expect_lte(max(abs(painted$vector[30:70, 30:70, ] - painted$raster[30:70, 30:70, ])), 3)
  run <- painted$grob$children[[1L]]
  expect_identical(run$texture_lighting, "sampled")
})

test_that("textured lighting retains canonical attributes with ambient occlusion", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  case <- texture_lighting_case(material_3d("shiny"), light_3d())
  case$light$ao <- list(samples = 8L, radius = 1, amount = 1)
  texture_lighting_compare(case)
})

test_that("texture lighting preserves transparent holes and layer alpha", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  case <- texture_lighting_case(material_3d("shiny"), lights_3d(light_head()))
  case$packet$styles$texture[[1L]] <- texture_3d(matrix(c("#102030", "transparent"), 1L), interpolate = FALSE)
  case$packet$faces$opacity <- .5
  case$packet$faces$coverage_opaque <- FALSE
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  viewport <- new_scene_viewport(40, 40, dpi = 100 * 25.4 / 40)
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  ragg::agg_png(path, 100, 100, res = viewport$dpi, background = "transparent")
  grid::grid.draw(render_scene_grid(scene_compile(case$packet, camera, viewport), viewport, light = case$light))
  grDevices::dev.off()
  alpha <- round(255 * png::readPNG(path)[, , 4L])
  expect_equal(alpha[50, 35], 128, tolerance = 1)
  expect_equal(alpha[50, 65], 0)
})

test_that("split texture runs paint only their own source fragments", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  packet <- crossing_texture_packet()
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  light <- light_3d(direction = c(0, 0, 1), space = "data")
  for (pixels in c(100L, 101L)) {
    viewport <- new_scene_viewport(25.4, 25.4, dpi = pixels)
    scene <- scene_compile(packet, camera, viewport)
    kinds <- scene$fragments$kind[scene$draw_order]
    expect_gte(sum(diff(c(FALSE, kinds == 1L, FALSE)) == 1L), 2L)
    # R graphics composites separate runs in encoded sRGB.
    reference_path <- texture_reference_png(packet, camera, viewport, pixels, light, blend = "srgb")
    on.exit(unlink(reference_path), add = TRUE)
    reference <- png::readPNG(reference_path, native = TRUE)
    expect_equal(ncol(reference), pixels)
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    ragg::agg_png(path, pixels, pixels, res = viewport$dpi, background = "transparent")
    grid::grid.draw(render_scene_grid(scene, viewport, light = light))
    grDevices::dev.off()
    vector <- png::readPNG(path)
    alpha <- round(png::readPNG(reference_path)[, , 4L] * 255)
    expect_lte(max(abs(round(vector[32:43, 32:68, 4L] * 255) - alpha[32:43, 32:68])), 1)
    expect_lte(max(abs(image_channels(path)[32:43, 32:68, ] - image_channels(reference)[32:43, 32:68, ])), 3)
  }
})

test_that("sampled textures keep labels last without graphics groups", {
  skip_if_not_installed("ragg")
  caps <- grid_capabilities_3d()
  caps$group_over <- caps$group_atop <- caps$masks <- FALSE
  testthat::with_mocked_bindings({
    d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1), label = c("a", "b"))
    p <- ggplot2::ggplot(expand.grid(x = 0:1, y = 0:1), ggplot2::aes(x, y, z = 0)) +
      geom_surface_3d(texture = texture_3d(matrix("#406080"))) +
      geom_text_3d(data = d, ggplot2::aes(z = z, label = label)) + coord_3d()
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path))
    ragg::agg_png(path, 160, 160)
    forced <- grid::grid.force(ggplot2::ggplotGrob(p))
    grid::grid.draw(forced)
    grDevices::dev.off()
    panel <- motion_panels_3d(forced)[[1L]]
    expect_true(any(grepl("scene-raster-image-3d", grid::grid.ls(forced, print = FALSE)$name, fixed = TRUE)))
    order <- panel$scene$fragments$kind[panel$scene$draw_order]
    expect_true(any(order == 4L))
    expect_true(all(tail(order, sum(order == 4L)) == 4L))
  }, grid_capabilities_3d = function(...) caps)
})
