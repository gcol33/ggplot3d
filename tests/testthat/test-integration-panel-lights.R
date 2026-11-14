panel_light_surface <- function() {
  d <- expand.grid(x = 1:4, y = 1:4); d$z <- with(d, x * y)
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
    geom_surface_3d(colour = NA, material = material_3d("shiny"))
}

panel_light_conditions <- function() list(
  left = lights_3d(light_directional(c(-1, 1, 2), space = "camera"),
    light_hemisphere(intensity = 0.1), ambient = 0.2),
  right = lights_3d(light_point(c(2, 2, 7), space = "data", falloff = 0), ambient = 0.3),
  unlit = NULL)

test_that("crossed camera and lighting conditions retain stable keys and shared data", {
  p <- panel_light_surface() + coord_3d() +
    facet_views(c("front", "isometric"), lights = panel_light_conditions(), ncol = 3)
  built <- ggplot2::ggplot_build(p)
  layout <- built$layout$layout
  expect_identical(layout$VIEW, rep(1:2, each = 3L))
  expect_identical(layout$LIGHT, rep(1:3, 2L))
  expect_identical(layout$ROW, rep(1:2, each = 3L))
  expect_identical(layout$COL, rep(1:3, 2L))
  expect_true(all(built$data[[1L]]$PANEL == 1L))
  params <- built$layout$panel_params
  for (q in params) expect_identical(q$ranges, params[[1L]]$ranges)
  for (i in seq_along(params))
    expect_identical(params[[i]]$coord_params$light, panel_light_conditions()[[layout$LIGHT[i]]])
  expect_identical(built$layout$context$key_context$light, params[[1L]]$coord_params$light)
  panels <- views_panels(p)
  for (q in panels[-1L]) expect_identical(q$packets, panels[[1L]]$packets)
  expect_identical(as.character(layout$view)[1:3], paste("front", names(panel_light_conditions()), sep = " / "))
})

test_that("paired lights broadcast singletons and explicit NULL differs from inheritance", {
  build <- function(facet) ggplot2::ggplot_build(panel_light_surface() + coord_3d(azimuth = 17) + facet)$layout
  paired <- build(facet_views(c("front", "top", "right"), lights = panel_light_conditions(), combine = "pair"))
  expect_identical(paired$layout$VIEW, 1:3)
  expect_identical(paired$layout$LIGHT, 1:3)
  camera <- build(facet_views(NULL, lights = panel_light_conditions(), combine = "pair"))
  expect_true(all(vapply(camera$panel_params, function(p) p$camera$azimuth == 17, logical(1))))
  expect_identical(as.character(camera$layout$view), names(panel_light_conditions()))
  light <- build(facet_views(c("front", "top"), lights = list(off = NULL), combine = "pair"))
  expect_true(all(vapply(light$panel_params, function(p) is.null(p$coord_params$light), logical(1))))
  inherited <- build(facet_views(c("front", "top")))
  expect_true(all(vapply(inherited$panel_params, function(p) !is.null(p$coord_params$light), logical(1))))
  off <- build(facet_views(NULL, lights = list(off = NULL)))
  expect_null(off$context$key_context$light)
  expect_error(facet_views(c("front", "top"), lights = panel_light_conditions(), combine = "pair"), "equal lengths")
  expect_error(facet_views(lights = list(a = NULL, a = NULL)), "unique")
  expect_error(facet_views(lights = list(a = 42)), "light must")
  expect_error(facet_views(lights = lights_3d()), "list of lighting")
  expect_error(facet_views(lights = panel_light_conditions(), layout = "third_angle"), "one lighting")
  expect_identical(build(facet_views(c("front", "top"), layout = "third_angle"))$layout$ROW, c(2L, 1L))
})

test_that("each panel matches its standalone vector and raster shading", {
  conditions <- panel_light_conditions()
  p <- panel_light_surface() + coord_3d() + facet_views(c("front", "isometric"), lights = conditions)
  input <- integration_packets(p)
  viewport <- new_scene_viewport(25.4, 25.4, 20)
  params <- ggplot2::ggplot_build(p)$layout$panel_params
  for (i in seq_along(params)) {
    view <- c("front", "isometric")[(i - 1L) %/% 3L + 1L]
    condition <- conditions[[(i - 1L) %% 3L + 1L]]
    standalone <- integration_packets(panel_light_surface() + do.call(coord_3d, c(camera_3d(view), list(light = condition))))
    expect_identical(input$packet$vertices, standalone$packet$vertices)
    colours <- function(packet, q) {
      scene <- scene_compile(packet, q$camera, viewport)
      styles <- scene$styles[match(scene$fragments$style_id, scene$styles$style_id), , drop = FALSE]
      rows <- which(scene$fragments$kind == 1L)
      face_fill_3d(scene, rows, styles, panel_lights_3d(q$coord_params$light, q))
    }
    expect_identical(colours(input$packet, params[[i]]), colours(standalone$packet, standalone$params))
    raster <- function(packet, q) raster_scene_3d(packet, q$camera, viewport, 1L,
      panel_lights_3d(q$coord_params$light, q))$image
    expect_identical(raster(input$packet, params[[i]]), raster(standalone$packet, standalone$params))
  }
})

test_that("the guide reference binds data-space point lights to its panel ranges", {
  point <- lights_3d(light_point(c(2, 2, 7), space = "data", falloff = 2))
  p <- panel_light_surface() + coord_3d() + facet_views(NULL, lights = list(point = point))
  layout <- ggplot2::ggplot_build(p)$layout
  panel <- layout$panel_params[[1L]]
  reference <- layout$context$key_context$light
  expect_identical(reference, panel_lights_3d(point, panel))
  expect_identical(reference$lights[[1L]]$space, "box")
  low <- vapply(panel$ranges, `[`, numeric(1), 1L)
  span <- vapply(panel$ranges, diff, numeric(1))
  expect_equal(reference$lights[[1L]]$vector, unname((c(2, 2, 7) - low) / span))
})

test_that("viewer payloads keep panel lights and progressive caches include their condition", {
  skip_if_not_installed("htmlwidgets")
  p <- panel_light_surface() + coord_3d() + facet_views(NULL, lights = panel_light_conditions())
  widget <- view_3d(p)
  expect_length(widget$x$panels, 3L)
  panels <- widget$x$panels[match(1:3, vapply(widget$x$panels, `[[`, integer(1), "id"))]
  expect_null(panels[[3L]]$light)
  expect_identical(panels[[1L]]$light$lights[[1L]]$space, "camera")
  expect_identical(panels[[1L]]$light$lights[[2L]]$space, "data")
  expect_identical(panels[[2L]]$light$lights[[1L]]$space, "box")
  input <- integration_packets(panel_light_surface() + coord_3d())
  backend <- backend_3d("pathtrace", samples = 1, progressive = TRUE)
  draw <- function(light) pathtrace_scene_3d(input$packet, list(), input$params$camera,
    new_scene_viewport(25.4, 25.4, 9), backend, light)
  draw(panel_light_conditions()[[1L]])
  expect_equal(draw(panel_light_conditions()[[1L]])$samples, 2L)
  expect_equal(draw(panel_light_conditions()[[2L]])$samples, 1L)
})
