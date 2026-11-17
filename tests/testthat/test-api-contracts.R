test_that("geom constructors put stat and position in the conventional slots", {
  constructors <- c("function", "smooth", "residual", "ridgeline", "parametric", "alpha_shape",
    "isosurface", "isodensity", "isofunction", "slice", "density_slice", "slice_contour",
    "isocurve", "hull", "sweep", "revolve", "tube", "strip", "sf", "splat", "volume")
  for (name in constructors) {
    constructor <- get(paste0("geom_", name, "_3d"))
    expect_identical(names(formals(constructor))[1:5], c("mapping", "data", "stat", "position", "..."))
  }
  expect_s3_class(geom_hull_3d(NULL, NULL, "identity", "identity")$stat, "StatIdentity")
  expect_s3_class(geom_function_3d(fun = function(x, y) x + y, stat = "identity")$stat, "StatIdentity")
  expect_s3_class(geom_tube_3d(stat = "identity")$stat, "StatIdentity")
  expect_s3_class(geom_strip_3d(stat = "identity")$stat, "StatIdentity")
  for (constructor in list(geom_boxplot_3d, geom_violin_3d, geom_contour_3d,
      geom_contour_filled_3d, geom_histogram_3d, geom_mesh_3d, geom_raster_3d,
      geom_surface_3d, geom_hex_3d, geom_bar_3d)) {
    expect_s3_class(constructor(stat = "identity")$stat, "StatIdentity")
  }
})

test_that("one layer path normalises aliases and rejects spelling conflicts", {
  expected <- geom_point_3d(colour = "red", shape = 21, size = 2, fill = "blue")$aes_params
  aliased <- geom_point_3d(color = "red", pch = 21, cex = 2, bg = "blue")$aes_params
  expect_equal(aliased, expected)
  expect_equal(geom_path_3d(lwd = 0.4, lty = 2)$aes_params,
    geom_path_3d(linewidth = 0.4, linetype = 2)$aes_params)
  expect_equal(geom_point_3d(fg = "red")$aes_params$colour, "red")
  expect_equal(geom_surface_3d(color = "red")$aes_params$colour, "red")
  expect_equal(layer_3d(ggplot2::geom_point(), color = "red", pch = 21)$aes_params,
    geom_point_3d(colour = "red", shape = 21)$aes_params)
  expect_error(geom_point_3d(color = "red", colour = "blue"), "once.*colour")
  expect_error(layer_3d(ggplot2::geom_point(), color = "red", colour = "blue"), "once.*colour")
  expect_equal(geom_crossbar_3d(middle.color = "red")$geom_params$middle_gp,
    geom_crossbar_3d(middle.colour = "red")$geom_params$middle_gp)
})

test_that("component strings resolve before parameter validation", {
  expect_s3_class(stat_surface_3d(geom = "point")$geom, "GeomPoint")
  expect_s3_class(stat_surface_3d(geom = "surface_3d")$geom, "GeomSurface3D")
  expect_s3_class(geom_surface_3d(stat = "surface_3d")$stat, "StatSurface3D")
  layer <- geom_point_3d(stat = "summary", fun = mean, position = "nudge_3d")
  expect_s3_class(layer$stat, "StatSummary")
  expect_identical(layer$stat_params$fun, mean)
  expect_s3_class(layer$position, "PositionNudge3D")
  expect_s3_class(stat_summary_3d(geom = "pointrange_3d")$geom, "GeomPointrange3D")
  expect_s3_class(stat_summary_3d(geom = "pointrange")$geom, "GeomPointrange3D")
  expect_error(stat_surface_3d(geom = "banana"), "Unsupported geom.*banana")
  expect_error(geom_point_3d(stat = "banana"), "Unsupported stat.*banana")
  expect_error(geom_point_3d(position = "banana"), "Unsupported position.*banana")
})

test_that("new registered geoms become available to string dispatch", {
  geom_point_3d()
  geom <- ggplot2::ggproto("GeomAPIProbe3D", ggplot2::GeomPoint)
  on.exit({
    rm("GeomAPIProbe3D", envir = ggplot3d:::.emitters_3d)
    rm(list = ls(ggplot3d:::.layer_components_3d), envir = ggplot3d:::.layer_components_3d)
  }, add = TRUE)
  register_geom_3d(geom, function(data, params, panel_params) new_scene_packet())
  expect_s3_class(stat_surface_3d(geom = "api_probe_3d")$geom, "GeomAPIProbe3D")
})

test_that("render flags and max_edge are checked in constructors and layer adapters", {
  for (value in list(NA, 1, "yes", c(TRUE, FALSE), NULL)) {
    expect_error(geom_surface_3d(shade = value), "shade must be TRUE or FALSE")
    expect_error(geom_surface_3d(wireframe = value), "wireframe must be TRUE or FALSE")
    expect_error(layer_3d(ggplot2::geom_point(), shade = value), "shade must be TRUE or FALSE")
  }
  for (value in list("wide", NA, NaN, 0, -1, -Inf, c(1, 2))) {
    expect_error(geom_surface_3d(max_edge = value), "max_edge.*positive.*Inf")
    expect_error(stat_surface_3d(max_edge = value), "max_edge.*positive.*Inf")
  }
  expect_no_error(geom_surface_3d(max_edge = Inf))
  expect_no_error(geom_surface_3d(max_edge = 0.1))
})

test_that("backend configurations validate at creation and use", {
  expect_s3_class(backend_3d(), "ggplot3d_backend")
  expect_no_error(coord_3d(backend = backend_3d("raster")))
  expect_error(coord_3d(backend = "raster"), "backend_3d")
  expect_error(coord_3d(backend = list(type = "banana")), "backend_3d")
  bad <- backend_3d()
  bad$type <- "banana"
  expect_error(coord_3d(backend = bad), "backend type.*vector.*raster")
  bad <- backend_3d("pathtrace")
  bad$samples <- NA
  expect_error(coord_3d(backend = bad), "samples.*integer")
  expect_error(backend_3d(dpi = -1), "dpi.*positive")
  expect_error(backend_3d(antialias = 2^31), "antialias.*representable")
  expect_error(backend_3d(type = NA), "backend type.*vector.*raster")
  expect_error(backend_3d(type = c("raster", "vector")), "backend type.*vector.*raster")
  expect_error(animate_3d(NULL, backend = backend_3d("hybrid")), "animate_3d.*hybrid.*raster.*vector")
})

test_that("integer options reject overflow before coercion", {
  expect_error(geom_violin_3d(n = 2^31), "n.*representable")
  expect_error(geom_edge_3d(n = 2^31), "n.*representable")
  expect_error(geom_edge_3d(sides = 2^31), "sides.*representable")
  expect_error(mesh_subdivide(NULL, n = 2^31), "n.*representable")
  for (name in c("ggplot3d.threads", "ggplot3d.quadric_segments")) {
    withr::local_options(stats::setNames(list(2^31), name))
    reader <- if (name == "ggplot3d.threads") ggplot3d:::raster_threads_3d else ggplot3d:::quadric_segments_3d
    expect_error(reader(), paste0(name, ".*representable"))
  }
  for (value in list(NA, "lots", -1, 0.5, c(1, 2), -Inf)) {
    withr::local_options(ggplot3d.vector_markers = value)
    expect_error(ggplot3d:::vector_marker_limit_3d(new_scene_packet()), "ggplot3d.vector_markers.*whole number or Inf")
  }
  withr::local_options(ggplot3d.vector_markers = Inf)
  expect_no_error(ggplot3d:::vector_marker_limit_3d(new_scene_packet()))
})

test_that("camera overrides validate immediately with named constraints", {
  expect_error(coord_3d(elevation = 91), "elevation.*-90.*90")
  expect_error(camera_3d(elevation = 91), "elevation.*-90.*90")
  expect_error(camera_3d(zoom = -1), "zoom.*positive")
  expect_error(camera_3d(projection = "banana"), "projection.*orthographic.*perspective.*oblique")
  expect_error(coord_3d(axes = "q"), "axes.*x.*y.*z")
  expect_error(coord_3d(panes = "ceiling"), "panes.*zmax")
  expect_no_error(camera_3d("cabinet"))
})

test_that("light and colour guide spellings use the same implementation", {
  expect_equal(light_directional_3d(color = "red"), light_directional_3d(colour = "red"))
  expect_equal(light_point_3d(c(0, 0, 1), color = "red"), light_point_3d(c(0, 0, 1), colour = "red"))
  expect_equal(light_head_3d(color = "red"), light_head_3d(colour = "red"))
  expect_equal(light_hemisphere_3d(color = "red"), light_hemisphere_3d(sky = "red"))
  expect_error(light_head_3d(color = "red", colour = "blue"), "Supply one")
  expect_identical(guide_colorbar_3d, guide_colourbar_3d)
  expect_identical(guide_colorsteps_3d, guide_coloursteps_3d)
  expect_error(guide_colourbar_3d(foo = 1), "Unsupported.*foo")
  guide <- guide_colourbar_3d(title = "My title", reverse = TRUE, alpha = 0.5, order = 2)
  expect_identical(guide$params$title, "My title")
  expect_true(guide$params$reverse)
  expect_equal(guide$params$alpha, 0.5)
  expect_equal(guide$params$order, 2)
})

test_that("space extensions and renamed constructors are public", {
  expect_s3_class(getExportedValue("ggplot3d", "Space3D"), "Space3D")
  expect_s3_class(print_spec_3d(), "ggplot3d_print")
  old <- c("geom_ribbon_3d", "print_3d", paste0("light_", c("directional", "point", "head", "hemisphere", "environment")))
  expect_false(any(old %in% getNamespaceExports("ggplot3d")))
})
