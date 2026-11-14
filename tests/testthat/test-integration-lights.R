kernel_lights <- function(...) {
  lights <- list(...)
  list(kind = vapply(lights, `[[`, integer(1), "kind"),
    vector = do.call(rbind, lapply(lights, `[[`, "vector")),
    colour = do.call(rbind, lapply(lights, function(l) l$colour %||% c(1, 1, 1))),
    ground = do.call(rbind, lapply(lights, function(l) l$ground %||% c(0, 0, 0))),
    falloff = vapply(lights, function(l) l$falloff %||% 0, numeric(1)), ambient = c(0, 0, 0), transfer = 0L,
    range = c(0, 0), neutral = cospi(1 / 4), shadow = c(0, 0, 0), highlight = c(1, 1, 1), frame = rbind(c(1, 1, 1), diag(3)))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
parallel_view <- list(perspective = FALSE, eye = c(0, 0, 10), toward = c(0, 0, 1))
no_material <- kernel_materials_3d(NULL)
shade1 <- function(normal, position, base, lights, material = 0L, materials = no_material, view = parallel_view) {
  shade_cpp(matrix(normal, 1L), matrix(position, 1L), matrix(base, 1L), material, materials, lights, view)[1L, ]
}

test_that("one directional light without specular is the Lambert formula", {
  normal <- rbind(c(0, 0, 1), c(1, 0, 0), c(0, 0, -1))
  base <- rbind(c(0.5, 0.2, 0.1), c(0.5, 0.2, 0.1), c(0.9, 0.9, 0.9))
  camera <- list(right = c(1, 0, 0), up = c(0, 1, 0), forward = c(0, 0, -1), ray = c(0, 0, -1), model_scale = c(1, 1, 1),
                 eye = c(0, 0, 5), distance = 5)
  light <- light_3d(direction = c(0, 0.6, 0.8), ambient = 0.3, diffuse = 0.7)
  out <- shade_linear_3d(base, normal, camera, light)
  b <- 0.3 + 0.7 * pmax(0, c(0.8, 0, 0.8))
  expect_equal(out, base * b)
  light$mode <- "tint"
  expect_equal(shade_linear_3d(base, normal, camera, light), 1 - (1 - base) * b)
  expect_identical(light_3d()$diffuse, 0.65)
  expect_s3_class(light_3d(), "ggplot3d_lights")
})

test_that("point lights fall off with distance and ignore other geometry", {
  lights <- kernel_lights(list(kind = 2L, vector = c(0, 0, 1), falloff = 2))
  near <- shade1(c(0, 0, 1), c(0, 0, 0), c(0.2, 0.2, 0.2), lights)
  lights$vector[1L, 3L] <- 2
  far <- shade1(c(0, 0, 1), c(0, 0, 0), c(0.2, 0.2, 0.2), lights)
  expect_equal(far, near / 4)
  lights$falloff <- 0
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), c(0.2, 0.2, 0.2), lights), near)
  both <- shade_cpp(rbind(c(0, 0, 1), c(1, 0, 0)), rbind(c(0, 0, 0), c(5, 5, 5)), rbind(c(0.2, 0.2, 0.2), c(1, 1, 1)),
    c(0L, 0L), no_material, kernel_lights(list(kind = 2L, vector = c(0, 0, 1), falloff = 2)), parallel_view)
  expect_equal(both[1L, ], near)
  expect_true(all(is.finite(shade1(c(0, 0, 1), c(0, 0, 1), c(0.2, 0.2, 0.2),
    kernel_lights(list(kind = 2L, vector = c(0, 0, 1), falloff = 2))))))
})

test_that("the specular lobe follows the halfway vector and moves with the eye", {
  shiny <- material_3d("shiny")
  materials <- kernel_materials_3d(material_table_3d(shiny))
  lights <- kernel_lights(list(kind = 1L, vector = c(0, 0, 1)))
  black <- c(0, 0, 0)
  peak <- shade1(c(0, 0, 1), c(0, 0, 0), black, lights, 1L, materials)
  expect_equal(peak, rep(shiny$specular, 3L))
  tilted <- c(sin(0.3), 0, cos(0.3))
  half <- c(sin(0.15), 0, cos(0.15))
  expected <- shiny$specular * cos(0.15)^shiny$shininess
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), black, lights, 1L, materials,
    list(perspective = FALSE, eye = c(0, 0, 0), toward = tilted)), rep(expected, 3L))
  moved <- shade1(c(0, 0, 1), c(0, 0, 0), black, lights, 1L, materials,
    list(perspective = TRUE, eye = c(3, 0, 1), toward = c(0, 0, 1)))
  expect_lt(moved[1L], peak[1L])
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), c(0.3, 0.3, 0.3), lights), c(0.3, 0.3, 0.3))
  metal <- kernel_materials_3d(material_table_3d(material_3d("metal")))
  tint <- shade1(c(0, 0, 1), c(0, 0, 0), c(1, 0, 0), kernel_lights(list(kind = 1L, vector = c(0, 0, 1))), 1L, metal)
  expect_gt(tint[1L], tint[2L])
})

test_that("head and hemisphere lights and back colours", {
  head <- kernel_lights(list(kind = 3L, vector = c(0, 0, 0)))
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), c(0.5, 0.5, 0.5), head), c(0.5, 0.5, 0.5))
  hemi <- kernel_lights(list(kind = 4L, vector = c(0, 0, 1), colour = c(1, 0, 0), ground = c(0, 0, 1)))
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), c(1, 1, 1), hemi), c(1, 0, 0))
  expect_equal(shade1(c(1, 0, 0), c(0, 0, 0), c(1, 1, 1), hemi), c(0.5, 0, 0.5))
  backed <- kernel_materials_3d(material_table_3d(material_3d(back = "white")))
  lights <- kernel_lights(list(kind = 1L, vector = c(0, 0, 1)))
  expect_equal(shade1(c(0, 0, -1), c(0, 0, 0), c(0.2, 0, 0), lights, 1L, backed), c(1, 1, 1))
  expect_equal(shade1(c(0, 0, 1), c(0, 0, 0), c(0.2, 0, 0), lights, 1L, backed), c(0.2, 0, 0))
})

test_that("light constructors validate and combine", {
  l <- lights_3d(light_point(c(1, 2, 3)), light_directional(c(0, 0, 1), "data", intensity = 0.4), light_head(),
                 light_hemisphere(), ambient = 0.1)
  expect_identical(vapply(l$lights, `[[`, integer(1), "kind"), c(2L, 1L, 3L, 4L))
  expect_identical(l$diffuse, 0.4)
  expect_identical(l$space, "data")
  expect_error(light_point(c(1, 2)), "three finite")
  expect_error(lights_3d("x"), "light_")
  expect_error(material_3d(shininess = 0), "positive")
  expect_identical(unlist(material_3d("metal")[1:5]), c(ambient = 1, diffuse = 0.375, specular = 1, shininess = 25, specular_colour = 0.5))
  expect_s3_class(as_lights_3d(light_head()), "ggplot3d_lights")
})

test_that("data-space point lights are placed in the panel box", {
  params <- list(ranges = list(x = c(0, 10), y = c(0, 2), z = c(-1, 1)), coord_params = list())
  l <- panel_lights_3d(light_point(c(5, 1, 0)), params)
  expect_equal(l$lights[[1L]]$vector, c(0.5, 0.5, 0.5))
  expect_identical(l$lights[[1L]]$space, "box")
})

test_that("surface layers carry materials and the packet merges them", {
  d <- expand.grid(x = seq(-1, 1, length.out = 4), y = seq(-1, 1, length.out = 4))
  d$z <- with(d, x^2 + y^2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(material = material_3d("shiny")) +
    geom_surface_3d(ggplot2::aes(z = z + 1), material = material_3d("metal", back = "red")) + coord_3d()
  packet <- stats_scene(p)$packet
  expect_identical(packet$materials$material_id, 1:2)
  expect_identical(packet$materials$back, c(NA, "red"))
  expect_setequal(unique(packet$styles$material_id), 1:2)
  expect_error(geom_surface_3d(material = list()), "material_3d")
  expect_error(new_scene_packet(styles = data.frame(style_id = 1L, material_id = 2L)), "material_id")
})

test_that("shiny surfaces show a highlight in raster and vector output", {
  d <- expand.grid(x = seq(-1, 1, length.out = 15), y = seq(-1, 1, length.out = 15))
  d$z <- with(d, 1 - x^2 - y^2)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
  light <- lights_3d(light_head(intensity = 0.65))
  bare <- ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_blank())
  max_level <- function(plot) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, 240, 240, background = "transparent")
    print(plot + bare)
    grDevices::dev.off()
    image <- png::readPNG(path)
    surface <- image[, , 4] > 0.99
    max((image[, , 1] + image[, , 2] + image[, , 3])[surface])
  }
  matte <- base + geom_surface_3d(fill = "navy", shading = "smooth") + coord_3d(light = light, backend = backend_3d("raster"), panes = character(), axes = character(), box = "none")
  shiny <- base + geom_surface_3d(fill = "navy", shading = "smooth", material = material_3d("shiny")) +
    coord_3d(light = light, backend = backend_3d("raster"), panes = character(), axes = character(), box = "none")
  expect_gt(max_level(shiny), max_level(matte) + 1)
  vector <- base + geom_surface_3d(fill = "navy", material = material_3d("shiny")) +
    coord_3d(light = light, panes = character(), axes = character(), box = "none")
  expect_gt(max_level(vector), max_level(matte) + 0.5)
})

test_that("the viewer runs the kernel's GLSL with the same terms", {
  glsl <- shade_glsl_cpp(8L)
  for (k in c("case 1:", "case 2:", "case 3:", "case 4:", "pow(max(dot(n, h), 0.0), sh)")) expect_match(glsl, k, fixed = TRUE)
  d <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), z = c(0, 0, 0, 0))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#4080C0", material = material_3d("shiny")) +
    coord_3d(light = lights_3d(light_directional(c(0.3, 0.2, 1)), ambient = 0.2),
             elevation = 90, azimuth = -90, panes = character(), axes = character(), box = "none")
  widget <- view_3d(p, width = 300, height = 300)
  browser <- viewer_browser(list(widget), 300, 300)
  on.exit(browser$close(), add = TRUE)
  pixel <- browser$eval(paste0("(function() { var c = document.querySelector('canvas.gg3d-surface');",
    "var o = document.createElement('canvas'); o.width = c.width; o.height = c.height;",
    "var g = o.getContext('2d'); g.drawImage(c, 0, 0); var d = g.getImageData(c.width / 2, c.height / 2, 1, 1).data;",
    "return [d[0], d[1], d[2]]; })()"))
  built <- ggplot2::ggplot_build(p)
  params <- built$layout$panel_params[[1L]]
  camera <- scene_compile(new_scene_packet(), params$camera, new_scene_viewport(100, 100))$camera
  materials <- kernel_materials_3d(material_table_3d(material_3d("shiny")))
  expected <- shade_cpp(matrix(c(0, 0, 1), 1L), matrix(camera$eye + camera$distance * camera$ray, 1L),
    linear_rgb_3d("#4080C0"), 1L, materials, resolved_light_3d(params$coord_params$light, camera), kernel_view_3d(camera))
  expect_equal(unlist(pixel) / 255, srgb_encode_3d(pmin(expected[1L, ], 1)), tolerance = 2 / 255)
})
