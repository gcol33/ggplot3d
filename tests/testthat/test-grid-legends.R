legend_surface <- function(...) {
  d <- expand.grid(x = 1:3, y = 1:3)
  d$z <- d$x + d$y
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d() + coord_3d(...)
}

legend_render <- function(plot, inspect = identity) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 750, 650, res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  inspect(grid::grid.force(ggplot2::ggplotGrob(plot)))
}

lit_keys <- function(g) {
  if (identical(g$name, "lit-legend-key-3d")) return(list(g))
  children <- if (length(g$children)) as.list(g$children) else g$grobs
  unlist(lapply(children, lit_keys), recursive = FALSE)
}

test_that("axis colourbars use the selected position edge, colours and shared ticks", {
  p <- legend_surface() + ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d(nbin = 16))
  built <- ggplot2::ggplot_build(p)
  expect_length(built$plot@guides$guides, 0L)
  rendered <- legend_render(p, function(g) {
    panel <- motion_panels_3d(g)[[1L]]
    list(panel = panel, furniture = panel_furniture_3d(panel$panel_params, panel$theme,
      panel$outer_viewport, as.list(panel$frame_group)))
  })
  panel <- rendered$panel
  bar <- panel$furniture$children[["panel-guide-3d-fill"]]
  expect_length(bar$colours, 16L)
  scale <- built$plot@scales$get_scales("fill")
  expect_equal(bar$colours, scale$map(scale$transform(bar$values)))
  params <- panel$panel_params
  furniture <- rendered$furniture
  project <- furniture_projector_3d(furniture)
  layout <- axis_layout_3d(frame_of_3d(params), params$axis_guides$z, project, project(frame_of_3d(params)$bounds()))
  expect_equal(bar$axis_index, layout$index)
  z <- (seq(2, 6, length.out = 17L) - params$ranges$z[1L]) / diff(params$ranges$z)
  expect_equal(bar$axis_positions, layout$at(z)[, 1:2], tolerance = 1e-8)
  plain <- ggplot2::ggplot_build(legend_surface())$layout$panel_params[[1L]]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a <- measure_guides_3d(params, panel$theme, new_scene_viewport(150, 150))$metrics$z$tick_length
  b <- measure_guides_3d(plain, panel$theme, new_scene_viewport(150, 150))$metrics$z$tick_length
  expect_equal(a - b, 2.5)
  expect_equal(params$axis_guides$z$labels, plain$axis_guides$z$labels)
})

test_that("axis colourbars match expressions and support distinct scale transformations", {
  p <- legend_surface() + ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d()) + scale_z_reverse()
  panel <- motion_panels_3d(legend_render(p))[[1L]]
  values <- panel$furniture$children[["panel-guide-3d-fill"]]$values
  expect_true(all(diff(values) < 0))
  log <- legend_surface() + scale_z_log10() +
    ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())
  expect_no_error(legend_render(log))
  bad <- legend_surface() + ggplot2::aes(fill = x) +
    ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())
  expect_error(ggplot2::ggplot_build(bad), "same expression")
  fixed <- legend_surface() + geom_surface_3d(ggplot2::aes(fill = x), fill = "grey70") +
    ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())
  expect_no_error(ggplot2::ggplot_build(fixed))
  expect_error(ggplot2::ggplot_build(legend_surface(axes = character()) +
    ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())), "visible box guide")
  expect_error(guide_colourbar_3d(width = grid::unit(1, "npc")), "physical")
  expect_error(guide_colourbar_3d(nbin = 1), "nbin")
})

test_that("lit legend keys use the scene light response and retain unshaded keys", {
  d <- data.frame(x = 1:2, y = 1:2, z = c(1, 2), group = c("a", "b"))
  light <- light_3d(ambient = 0.2, diffuse = 0.6)
  make <- function(lit = TRUE, shade = TRUE, range = TRUE) {
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = group)) + geom_col_3d(shade = shade) +
      ggplot2::guides(fill = guide_legend_3d(lit = lit, shading.range = range)) + coord_3d(light = light)
  }
  keys <- lit_keys(legend_render(make()))
  expect_length(keys, 2L)
  camera <- scene_compile(new_scene_packet(), coord_3d()$camera, new_scene_viewport(100, 100))$camera
  for (key in keys) {
    expected <- shade_fill_3d(key$base_fill, list(normal_x = 0, normal_y = 0, normal_z = 1), 1L, camera, light)
    expect_equal(key$lit_fill, expected)
    expect_length(key$shading_colours, 32L)
    expect_equal(key$shading_colours[c(1L, 32L)], apply_brightness_3d(rep(key$base_fill, 2), c(.2, .8), "multiply"))
  }
  expect_length(lit_keys(legend_render(make(lit = FALSE))), 0L)
  expect_length(lit_keys(legend_render(make(shade = FALSE))), 0L)
  scaled <- make() + coord_3d(aspect = "data", light = light_3d(space = "data")) + facet_views(c("front", "isometric"))
  built <- ggplot2::ggplot_build(scaled)
  actual <- lit_keys(legend_render(scaled))[[1L]]
  camera <- scene_compile(new_scene_packet(), built$layout$panel_params[[1L]]$camera, new_scene_viewport(100, 100))$camera
  expect_equal(actual$lit_fill, shade_fill_3d(actual$base_fill, list(normal_x = 0, normal_y = 0, normal_z = 1),
    1L, camera, light_3d(space = "data")))
  expect_error(guide_legend_3d(normal = c(0, 0, 0)), "nonzero")
})

test_that("3D colour guides render with free facets and each backend", {
  for (type in c("vector", "raster", "hybrid")) {
    expect_no_error(legend_render(legend_surface(backend = backend_3d(type)) +
      ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())))
  }
  d <- expand.grid(x = 1:3, y = 1:3, panel = c("a", "b"))
  d$z <- with(d, (x + y) * ifelse(panel == "a", 1, 5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d() +
    ggplot2::facet_wrap(~panel) + coord_3d(z_scales = "free") +
    ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d())
  panels <- motion_panels_3d(legend_render(p))
  expect_length(panels, 2L)
  expect_lt(max(panels[[1L]]$furniture$children[["panel-guide-3d-fill"]]$values),
    min(panels[[2L]]$furniture$children[["panel-guide-3d-fill"]]$values))
})
