oklab_ref <- function(linear) {
  m1 <- matrix(c(0.4122214708, 0.5363325363, 0.0514459929, 0.2119034982, 0.6806995451, 0.1073969566,
                 0.0883024619, 0.2817188376, 0.6299787005), 3, byrow = TRUE)
  m2 <- matrix(c(0.2104542553, 0.7936177850, -0.0040720468, 1.9779984951, -2.4285922050, 0.4505937099,
                 0.0259040371, 0.7827717662, -0.8086757660), 3, byrow = TRUE)
  lms <- linear %*% t(m1)
  (sign(lms) * abs(lms)^(1 / 3)) %*% t(m2)
}
hue_ref <- function(lab) atan2(lab[, 3L], lab[, 2L])
swatch_camera <- function() key_camera_3d(coord_3d()$camera)
swatch_linear <- function(colour, x, light, material = NULL) {
  shade_swatch_cpp(linear_rgb_3d(colour), as.double(x), rep(if (is.null(material)) 0L else 1L, length(colour)),
    kernel_materials_3d(material_table_3d(material)), resolved_light_3d(light, swatch_camera()))
}

test_that("signed transfers leave colours unchanged at the neutral angle", {
  colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FFFF33", "#101010")
  for (mode in c("lightness", "tone", "overlay", "soft")) {
    for (neutral in c(30, 45, 100)) {
      light <- lights_3d(light_directional(), mode = mode, neutral = neutral)
      out <- swatch_linear(colours, rep(cospi(neutral / 180), length(colours)), light)
      expect_identical(out, unname(linear_rgb_3d(colours)), info = paste(mode, neutral))
    }
  }
})

test_that("lightness moves OKLab lightness by bounded fractions and keeps the hue", {
  colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#A6CEE3")
  light <- lights_3d(light_directional(), mode = "lightness", range = c(-0.4, 0.3))
  before <- oklab_ref(linear_rgb_3d(colours))
  dark <- oklab_ref(swatch_linear(colours, rep(-1, 5L), light))
  bright <- oklab_ref(swatch_linear(colours, rep(1, 5L), light))
  expect_equal(dark[, 1L], before[, 1L] * 0.6, tolerance = 1e-4)
  expect_equal(bright[, 1L], before[, 1L] + 0.3 * (1 - before[, 1L]), tolerance = 1e-6)
  expect_equal(hue_ref(dark), hue_ref(before), tolerance = 1e-4)
  expect_equal(hue_ref(bright), hue_ref(before), tolerance = 1e-4)
  half <- oklab_ref(swatch_linear(colours, rep(cospi(1 / 4) + (1 - cospi(1 / 4)) / 2, 5L), light))
  expect_equal(half[, 1L], before[, 1L] + 0.15 * (1 - before[, 1L]), tolerance = 1e-6)
})

test_that("gamut compression reduces chroma at the kept lightness and hue", {
  light <- lights_3d(light_directional(), mode = "lightness", range = c(-1, 0.9))
  colours <- c("#0000FF", "#FF00FF", "#00FF00")
  out <- swatch_linear(colours, rep(1, 3L), light)
  expect_true(all(out >= 0 & out <= 1))
  before <- oklab_ref(linear_rgb_3d(colours)); after <- oklab_ref(out)
  expect_equal(after[, 1L], before[, 1L] + 0.9 * (1 - before[, 1L]), tolerance = 1e-4)
  expect_equal(hue_ref(after), hue_ref(before), tolerance = 1e-3)
  chroma <- function(lab) sqrt(lab[, 2L]^2 + lab[, 3L]^2)
  expect_true(all(chroma(after) < chroma(before)))
  black <- swatch_linear("#336699", -1, light)
  expect_equal(black, matrix(0, 1L, 3L), tolerance = 1e-4)
})

test_that("tone reaches its endpoint colours and overlay and soft follow matplotlib", {
  light <- lights_3d(light_directional(), mode = "tone", range = c(-1, 1), shadow = "#203060", highlight = "#FFF0C0")
  expect_equal(swatch_linear(c("#E41A1C", "#4DAF4A"), c(-1, -1), light), unname(linear_rgb_3d(rep("#203060", 2L))), tolerance = 1e-6)
  expect_equal(swatch_linear("#E41A1C", 1, light), unname(linear_rgb_3d("#FFF0C0")), tolerance = 1e-6)
  e <- c(0.2, 0.5, 0.8)
  colour <- grDevices::rgb(e[1L], e[2L], e[3L])
  e <- unname(grDevices::col2rgb(colour)[, 1L]) / 255
  for (mode in c("overlay", "soft")) {
    light <- lights_3d(light_directional(), mode = mode, range = c(-0.8, 0.6))
    for (x in c(-1, 1)) {
      intensity <- 0.5 + 0.5 * if (x < 0) -0.8 else 0.6
      expected <- if (mode == "overlay") ifelse(e <= 0.5, 2 * intensity * e, 1 - 2 * (1 - intensity) * (1 - e)) else
        2 * intensity * e + (1 - 2 * intensity) * e^2
      expect_equal(srgb_encode_3d(swatch_linear(colour, x, light)[1L, ]), expected, tolerance = 1e-9, info = paste(mode, x))
    }
  }
  expect_error(lights_3d(mode = "lightness", range = c(0.1, 0.2)), "range")
  expect_error(lights_3d(mode = "tone", range = c(-2, 0)), "range")
  expect_error(lights_3d(neutral = 180), "neutral")
  expect_error(lights_3d(mode = "hsv"))
})

test_that("faces and swatches use the same transfer; specular stays outside the swatch", {
  light <- lights_3d(light_directional(c(0, 0, 1)), mode = "lightness")
  camera <- list(right = c(1, 0, 0), up = c(0, 1, 0), forward = c(0, 0, -1), ray = c(0, 0, -1), model_scale = c(1, 1, 1),
                 eye = c(0, 0, 5), distance = 5)
  angles <- c(0, 30, 60, 89)
  normal <- cbind(sinpi(angles / 180), 0, cospi(angles / 180))
  base <- linear_rgb_3d(rep("#2C7FB8", 4L))
  faces <- shade_linear_3d(base, normal, camera, light)
  swatch <- shade_swatch_cpp(base, cospi(angles / 180), integer(4L), kernel_materials_3d(NULL), resolved_light_3d(light, camera))
  expect_equal(faces, swatch, tolerance = 1e-12)
  shiny <- material_3d("shiny")
  lit <- shade_linear_3d(base, normal, camera, light, material = rep(1L, 4L), materials = material_table_3d(shiny))
  plain <- shade_swatch_cpp(base, cospi(angles / 180), rep(1L, 4L), kernel_materials_3d(material_table_3d(shiny)),
    resolved_light_3d(light, camera))
  expect_gt(lit[1L, 1L], plain[1L, 1L])
})

test_that("the swatch sampler keeps NA and alpha", {
  light <- lights_3d(light_directional(), mode = "lightness")
  out <- shade_swatch_3d(c("#FF000080", NA, "#00FF00"), c(-1, 1), light, swatch_camera())
  expect_identical(dim(out), c(3L, 2L))
  expect_true(all(is.na(out[2L, ])))
  expect_identical(substr(out[1L, ], 8L, 9L), c("80", "80"))
  expect_identical(nchar(out[3L, ]), c(7L, 7L))
})

transfer_surface <- function(light, guide, scale = ggplot2::scale_fill_distiller, backend = backend_3d()) {
  d <- expand.grid(x = 1:4, y = 1:4)
  d$z <- d$x * d$y
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d() +
    scale(guide = guide) + coord_3d(light = light, backend = backend)
}

transfer_render <- function(plot) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 700, 600, res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  grid::grid.force(ggplot2::ggplotGrob(plot))
}

guide_grobs <- function(plot, name) {
  find <- function(g) {
    if (identical(g$name, name)) return(list(g))
    children <- if (length(g$children)) as.list(g$children) else g$grobs
    unlist(lapply(children, find), recursive = FALSE)
  }
  find(transfer_render(plot))
}

test_that("a legend-box colourbar shows value by light with the face transfer", {
  light <- lights_3d(light_directional(), mode = "lightness")
  for (direction in c("vertical", "horizontal")) {
    bar <- guide_grobs(transfer_surface(light, guide_colourbar_3d(along = NULL, shading.range = TRUE, nbin = 10,
      direction = direction)), "shaded-colourbar-3d")
    expect_length(bar, 1L)
    colours <- bar[[1L]]$shading_colours
    expect_identical(dim(colours), c(10L, 24L))
    built <- ggplot2::ggplot_build(transfer_surface(light, "colourbar"))
    scale <- built$plot@scales$get_scales("fill")
    values <- seq(scale$get_limits()[1L], scale$get_limits()[2L], length.out = 10L)
    expected <- shade_swatch_3d(scale$map(values), cospi(seq(180, 0, length.out = 24L) / 180), light, swatch_camera())
    expect_identical(colours, expected)
    image <- as.matrix(bar[[1L]]$raster)
    if (direction == "vertical") expect_identical(toupper(image[10L, 24L]), toupper(expected[1L, 24L])) else
      expect_identical(toupper(image[1L, 1L]), toupper(expected[1L, 24L]))
  }
  reversed <- guide_grobs(transfer_surface(light, guide_colourbar_3d(along = NULL, shading.range = c(0, 90), nbin = 5)),
    "shaded-colourbar-3d")[[1L]]$shading_colours
  multiply <- guide_grobs(transfer_surface(light_3d(), guide_colourbar_3d(along = NULL, shading.range = c(0, 90), nbin = 5)),
    "shaded-colourbar-3d")[[1L]]$shading_colours
  expect_false(identical(reversed, multiply))
  expect_true(all(col2rgb(multiply[, 24L])[1L, ] <= col2rgb(multiply[, 1L])[1L, ]))
  expect_length(guide_grobs(transfer_surface(light, guide_colourbar_3d(along = NULL)), "shaded-colourbar-3d"), 0L)
  expect_length(guide_grobs(transfer_surface(NULL, guide_colourbar_3d(along = NULL, shading.range = TRUE)), "shaded-colourbar-3d"), 0L)
  expect_error(guide_colourbar_3d(shading.range = c(0, 200)), "shading.range")
})

test_that("colour steps and bins shade binned scales with uneven intervals", {
  light <- lights_3d(light_directional(), mode = "lightness")
  steps <- guide_grobs(transfer_surface(light, guide_coloursteps_3d(even.steps = FALSE),
    function(guide) ggplot2::scale_fill_viridis_b(breaks = c(2, 3, 10), guide = guide)), "shaded-coloursteps-3d")
  expect_length(steps, 1L)
  colours <- steps[[1L]]$shading_colours
  expect_identical(dim(colours), c(4L, 24L))
  heights <- as.numeric(steps[[1L]]$height)
  expect_equal(sum(heights[1:4]), 1, tolerance = 1e-9)
  expect_gt(max(heights[1:4]) / min(heights[1:4]), 5)
  expect_identical(heights, rep(heights[1:4], 24L))
  horizontal <- guide_grobs(transfer_surface(light, guide_coloursteps_3d(direction = "horizontal"),
    ggplot2::scale_fill_viridis_b), "shaded-coloursteps-3d")
  expect_length(horizontal, 1L)
  bins <- transfer_surface(light, guide_bins_3d(shading.range = TRUE), ggplot2::scale_fill_viridis_b)
  keys <- guide_grobs(bins, "lit-legend-key-3d")
  expect_gt(length(keys), 1L)
  for (key in keys) {
    expect_identical(key$shading_colours, drop(shade_swatch_3d(key$base_fill, cospi(seq(180, 0, length.out = 32L) / 180),
      light, swatch_camera())))
  }
  plain <- guide_grobs(transfer_surface(light, guide_coloursteps_3d(shading.range = FALSE), ggplot2::scale_fill_viridis_b),
    "shaded-coloursteps-3d")
  expect_length(plain, 0L)
})

test_that("the axis colourbar shades across its width", {
  light <- lights_3d(light_directional(), mode = "lightness")
  p <- transfer_surface(light, guide_colourbar_3d(nbin = 8, shading.range = TRUE), ggplot2::scale_fill_viridis_c)
  panel <- motion_panels_3d(transfer_render(p))[[1L]]
  bar <- panel$furniture$children[["panel-guide-3d-fill"]]
  expect_identical(dim(bar$shading_colours), c(8L, 24L))
  expect_identical(bar$shading_colours[, 1L], drop(shade_swatch_3d(bar$colours, -1, light, swatch_camera())))
})

test_that("legend keys with a categorical scale get the signed transfer strip", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3, g = c("a", "b", "c"))
  light <- lights_3d(light_directional(), mode = "tone")
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = g)) + geom_col_3d() +
    ggplot2::guides(fill = guide_legend_3d(shading.range = c(150, 10))) + coord_3d(light = light)
  keys <- guide_grobs(p, "lit-legend-key-3d")
  expect_length(keys, 3L)
  for (key in keys) expect_identical(key$shading_colours,
    drop(shade_swatch_3d(key$base_fill, cospi(seq(150, 10, length.out = 32L) / 180), light, swatch_camera())))
})

test_that("the viewer applies the signed transfers like the kernel", {
  skip_if_not_installed("htmlwidgets")
  d <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), z = c(0, 0, 0, 0))
  for (mode in c("lightness", "tone", "soft")) {
    light <- lights_3d(light_directional(c(0.6, 0.2, 1)), ambient = 0.2, mode = mode)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(fill = "#4080C0") +
      coord_3d(light = light, elevation = 90, azimuth = -90, panes = character(), axes = character(), box = "none")
    widget <- view_3d(p, width = 200, height = 200, elementId = paste0("transfer-", mode))
    browser <- viewer_browser(list(widget), 200, 200)
    pixel <- browser$eval(paste0("(function() { var c = document.querySelector('canvas.gg3d-surface');",
      "var o = document.createElement('canvas'); o.width = c.width; o.height = c.height;",
      "var g = o.getContext('2d'); g.drawImage(c, 0, 0); var d = g.getImageData(c.width / 2, c.height / 2, 1, 1).data;",
      "return [d[0], d[1], d[2]]; })()"))
    browser$close()
    params <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
    camera <- scene_compile(new_scene_packet(), params$camera, new_scene_viewport(100, 100))$camera
    expected <- shade_cpp(matrix(c(0, 0, 1), 1L), matrix(camera$eye + camera$distance * camera$ray, 1L),
      linear_rgb_3d("#4080C0"), 0L, kernel_materials_3d(NULL), resolved_light_3d(params$coord_params$light, camera),
      kernel_view_3d(camera))
    expect_equal(unlist(pixel) / 255, srgb_encode_3d(pmin(expected[1L, ], 1)), tolerance = 2 / 255, info = mode)
  }
})

test_that("raster faces use the signed transfer", {
  light <- lights_3d(light_directional(), mode = "lightness")
  raster <- transfer_surface(light, "none", backend = backend_3d("raster"))
  expect_no_error(transfer_render(raster))
})
