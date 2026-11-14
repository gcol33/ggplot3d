npr_camera <- function() key_camera_3d(coord_3d()$camera)
npr_swatch <- function(colour, x, material, light = lights_3d(light_directional(), ambient = 0.35)) {
  n <- length(x)
  shade_swatch_cpp(linear_rgb_3d(rep(colour, n)), as.double(x), rep(1L, n),
    kernel_materials_3d(material_table_3d(material)), resolved_light_3d(light, npr_camera()))
}

test_that("toon bands split exactly at explicit breaks and evenly by cosine", {
  toon <- material_3d(style = "toon", breaks = c(60, 90))
  x <- c(1, 0.51, cospi(1 / 3), 0.3, 0, -0.5)
  out <- npr_swatch("white", x, toon)[, 1L]
  expect_equal(out, 0.35 + 0.65 * c(1, 1, 0.5, 0.5, 0, 0), tolerance = 1e-12)
  even <- npr_swatch("white", c(0.1, 0.25, 0.49, 0.5, 0.74, 0.75, 1), material_3d(style = "toon", bands = 4))[, 1L]
  expect_equal(even, 0.35 + 0.65 * c(0, 1, 1, 2, 2, 3, 3) / 3, tolerance = 1e-12)
  expect_error(material_3d(style = "toon", breaks = c(60, 60)), "breaks")
  expect_error(material_3d(style = "toon", breaks = c(90, 60)), "breaks")
  expect_error(material_3d(style = "toon", breaks = c(0, 60)), "breaks")
  expect_error(material_3d(style = "toon", breaks = seq(10, 170, by = 20)), "breaks")
  expect_error(material_3d(style = "toon", bands = 1), "bands")
})

test_that("toon steps divide the signed level of signed transfers", {
  toon <- material_3d(style = "toon", bands = 3)
  light <- lights_3d(light_directional(), mode = "lightness", range = c(-0.5, 0.5))
  x <- cospi(seq(0, 180, by = 5) / 180)
  out <- npr_swatch("#3366CC", x, toon, light)
  expect_identical(nrow(unique(round(out, 12))), 3L)
  expect_identical(out[x > 0.99, , drop = FALSE][1L, ], out[1L, ])
})

test_that("a quantised specular is a flat spot, a continuous one a lobe", {
  light <- lights_3d(light_directional(c(0, 0, 1), space = "data"), ambient = 0.3)
  view <- list(perspective = FALSE, eye = c(0, 0, 10), toward = c(0, 0, 1))
  angles <- seq(0, 40, by = 1)
  normal <- cbind(sinpi(angles / 180), 0, cospi(angles / 180))
  base <- matrix(0.2, length(angles), 3L)
  run <- function(material) shade_cpp(normal, matrix(0, length(angles), 3L), base, rep(1L, length(angles)),
    kernel_materials_3d(material_table_3d(material)), resolved_light_3d(light, npr_camera()), view)[, 1L]
  continuous <- run(material_3d("shiny"))
  stepped <- run(material_3d("shiny", specular_steps = TRUE))
  smooth <- run(material_3d("shiny", specular_steps = FALSE))
  expect_identical(continuous, smooth)
  diffuse <- run(material_3d("shiny", specular = 0))
  spot <- stepped - diffuse
  expect_true(all(abs(spot) < 1e-12 | abs(spot - 0.65 * 0.9) < 1e-12))
  expect_gt(sum(abs(spot - 0.65 * 0.9) < 1e-12), 0L)
  expect_gt(sum(abs(spot) < 1e-12), 0L)
  expect_gt(length(unique(round(continuous - diffuse, 9))), 10L)
  expect_true(material_3d(style = "toon")$specular_steps)
})

test_that("Gooch shading blends cool and warm by the light value", {
  gooch <- material_3d(style = "gooch")
  b <- unname(linear_rgb_3d("#808080")[1L, ])
  cool <- unname(linear_rgb_3d("#0000AA")[1L, ]) + 0.2 * b
  warm <- unname(linear_rgb_3d("#AAAA00")[1L, ]) + 0.6 * b
  out <- npr_swatch("#808080", c(-1, 0, 1), gooch)
  expect_equal(out[1L, ], cool, tolerance = 1e-12)
  expect_equal(out[2L, ], (cool + warm) / 2, tolerance = 1e-12)
  expect_equal(out[3L, ], warm, tolerance = 1e-12)
  custom <- npr_swatch("black", 1, material_3d(style = "gooch", warm = "red", beta = 0))
  expect_equal(custom[1L, ], c(1, 0, 0), tolerance = 1e-12)
})

normal_colour <- function(normal, camera, material, light = lights_3d(light_directional(c(0, 0, 1)))) {
  view <- list(perspective = FALSE, eye = c(0, 0, 10), toward = -camera$forward)
  n <- nrow(normal)
  srgb_encode_3d(shade_cpp(normal, matrix(0, n, 3L), matrix(0.5, n, 3L), rep(1L, n),
    kernel_materials_3d(material_table_3d(material)), resolved_light_3d(light, camera), view))
}

test_that("normal colours encode axis and diagonal normals in each frame", {
  camera <- list(right = c(1, 0, 0), up = c(0, 1, 0), forward = c(0, 0, -1), ray = c(0, 0, -1), model_scale = c(1, 1, 1),
                 eye = c(0, 0, 10), distance = 10)
  model <- material_3d(style = "normals")
  normals <- rbind(c(0, 0, 1), c(1, 0, 0.001), c(0, 1, 0.001), c(1, 1, 1) / sqrt(3))
  out <- normal_colour(normals, camera, model)
  expect_equal(out, (normals / sqrt(rowSums(normals^2)) + 1) / 2, tolerance = 1e-6)
  stretched <- camera; stretched$model_scale <- c(2, 1, 1)
  n <- c(1, 0, 1) / sqrt(2)
  panel <- n * c(2, 1, 1); panel <- panel / sqrt(sum(panel^2))
  expect_equal(normal_colour(matrix(n, 1L), stretched, model)[1L, ], (panel + 1) / 2, tolerance = 1e-6)
  turned <- list(right = c(0, 1, 0), up = c(0, 0, 1), forward = c(-1, 0, 0), ray = c(-1, 0, 0), model_scale = c(1, 1, 1),
                 eye = c(10, 0, 0), distance = 10)
  facing <- normal_colour(rbind(c(1, 0, 0)), turned, material_3d(style = "normals", normal_space = "camera"))
  expect_equal(facing[1L, ], c(0.5, 0.5, 1), tolerance = 1e-6)
  lit <- normal_colour(rbind(c(0, 0, 1)), camera, material_3d(style = "normals", normal_space = "light"),
    lights_3d(light_directional(c(0, 0, 1), space = "data")))
  expect_equal(lit[1L, ], c(0.5, 0.5, 1), tolerance = 1e-6)
  expect_error(material_3d(style = "normals", normal_space = "world"))
})

test_that("normal colouring replaces the fill legend", {
  d <- expand.grid(x = 1:4, y = 1:4)
  d$z <- d$x * d$y
  legends <- function(material) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d(material = material) + coord_3d()
    pdf(NULL); on.exit(grDevices::dev.off())
    g <- ggplot2::ggplotGrob(p)
    sum(grepl("guide-box", g$layout$name) & !vapply(g$grobs, inherits, logical(1), "zeroGrob"))
  }
  expect_gt(legends(NULL), 0L)
  expect_identical(legends(material_3d(style = "normals")), 0L)
  expect_identical(material_legend_3d(material_3d(style = "normals"), TRUE), c(fill = FALSE, colour = TRUE))
  expect_identical(material_legend_3d(material_3d(), NA), NA)
})

npr_surface <- function(material = NULL, hatch = NULL, light = light_3d(), backend = backend_3d(), fill = "white") {
  d <- expand.grid(x = seq(-2, 2, length.out = 16), y = seq(-2, 2, length.out = 16))
  d$z <- with(d, exp(-(x^2 + y^2)))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = fill, colour = NA, material = material, hatch = hatch) +
    coord_3d(light = light, backend = backend, panes = character(), axes = character(), box = "none")
}

npr_render <- function(plot) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 500, 500, res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  grid::grid.force(ggplot2::ggplotGrob(plot))
}

test_that("model-space hatching lies on its planes, nests by tone and is chained", {
  spec <- hatch_3d(angle = 0, spacing = 0.05, cross = FALSE)
  packet <- stats_scene(npr_surface(hatch = spec))$packet
  lines <- packet$polylines
  expect_gt(length(lines$offset), 0L)
  v <- packet$vertices
  index <- packet$corners$vertex[rep(lines$offset, lines$count) + sequence(lines$count) - 1L]
  k <- v$x[index] / 0.05
  expect_lt(max(abs(k - round(k))), 1e-9)
  flat <- stats_scene(npr_surface(hatch = hatch_3d(angle = 0, spacing = 0.05, cross = FALSE), light = NULL))$packet
  expect_gte(sum(flat$polylines$count), sum(lines$count))
  crossed <- stats_scene(npr_surface(hatch = hatch_3d(angle = 30, spacing = 0.05, cross = TRUE, tone = FALSE), light = NULL))$packet
  a <- 30 * pi / 180
  l <- crossed$polylines
  index <- crossed$corners$vertex[rep(l$offset, l$count) + sequence(l$count) - 1L]
  family <- rep(seq_along(l$offset), l$count)
  h1 <- (crossed$vertices$x[index] * cos(a) + crossed$vertices$y[index] * sin(a)) / 0.05
  h2 <- (-crossed$vertices$x[index] * sin(a) + crossed$vertices$y[index] * cos(a)) / 0.05
  on_plane <- pmin(abs(h1 - round(h1)), abs(h2 - round(h2)))
  expect_lt(max(on_plane), 1e-9)
  expect_lt(length(lines$offset), sum(lines$count) / 3)
})

test_that("hatch ranks nest: darker faces keep a superset of lines", {
  levels <- hatch_levels_3d(c(0, 0.2, 0.4, 0.7, 0.95), TRUE)
  expect_identical(unname(levels[, 1L]), c(0, 1, 2, 3, 3))
  expect_identical(unname(levels[, 2L]), c(0, 0, 0, 1, 3))
  xyz <- rbind(c(0.05, 0, 0), c(1.05, 0, 0), c(0.05, 1, 0))
  faces <- matrix(0:2, 1L)
  count <- function(level) length(unique(hatch_lines_cpp(xyz, faces, rbind(c(1, 0, 0)), 0.1, matrix(level, 1L))$line))
  expect_identical(vapply(0:3, count, integer(1)), c(0L, 2L, 5L, 10L))
  expect_error(hatch_3d(spacing = 0), "spacing")
  expect_error(geom_surface_3d(hatch = list()), "hatch_3d")
})

test_that("the plotter backend draws strokes only, with hatching hidden by the surface", {
  p <- npr_surface(hatch = hatch_3d(spacing = 0.04), backend = backend_3d("plotter"))
  g <- npr_render(p)
  panel <- motion_panels_3d(g)[[1L]]
  scene <- panel$scene
  rendered <- grid::getGrob(g, "scene-inner-3d", grep = TRUE, global = TRUE)
  polygons <- function(x) {
    if (inherits(x, "polygon") || inherits(x, "pathgrob")) return(1L)
    children <- if (length(x$children)) as.list(x$children) else list()
    sum(vapply(children, polygons, integer(1)))
  }
  expect_identical(polygons(panel$children[[3L]]), 0L)
  hidden <- scene$fragments$kind == 2L & scene$fragments$hidden
  expect_true(any(scene$fragments$kind == 2L))
  vector <- motion_panels_3d(npr_render(npr_surface(hatch = hatch_3d(spacing = 0.04))))[[1L]]$scene
  expect_identical(sum(vector$fragments$kind == 2L), sum(scene$fragments$kind == 2L))
})

test_that("screen-space hatching fills faces with a pattern per tone band", {
  p <- npr_surface(hatch = hatch_3d(space = "screen", spacing = 1))
  g <- npr_render(p)
  find <- function(x) {
    if (identical(x$name, "hatch-3d")) return(list(x))
    children <- if (length(x$children)) as.list(x$children) else x$grobs
    unlist(lapply(children, find), recursive = FALSE)
  }
  hatched <- find(g)
  expect_gt(length(hatched), 0L)
  bands <- unlist(lapply(hatched, `[[`, "bands"))
  expect_gt(length(unique(bands)), 1L)
  fills <- unlist(lapply(hatched, function(h) lapply(h$children, function(c) class(c$gp$fill)[1L])))
  expect_true(all(fills == "GridTilingPattern"))
})

test_that("styles render in raster and vector output and colour samples agree", {
  for (material in list(material_3d(style = "toon"), material_3d(style = "gooch"), material_3d(style = "normals"))) {
    for (type in c("vector", "raster")) expect_no_error(npr_render(npr_surface(material, backend = backend_3d(type), fill = "steelblue")))
  }
})

test_that("the viewer applies the styles like the kernel", {
  skip_if_not_installed("htmlwidgets")
  d <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), z = c(0, 0, 0, 0))
  styles <- list(toon = material_3d(style = "toon", bands = 4), gooch = material_3d(style = "gooch"),
    normals = material_3d(style = "normals", normal_space = "camera"))
  for (name in names(styles)) {
    light <- lights_3d(light_directional(c(0.6, 0.2, 1)), ambient = 0.2)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(fill = "#4080C0", material = styles[[name]]) +
      coord_3d(light = light, elevation = 90, azimuth = -90, panes = character(), axes = character(), box = "none")
    browser <- viewer_browser(list(view_3d(p, width = 200, height = 200, elementId = paste0("style-", name))), 200, 200)
    pixel <- browser$eval(paste0("(function() { var c = document.querySelector('canvas.gg3d-surface');",
      "var o = document.createElement('canvas'); o.width = c.width; o.height = c.height;",
      "var g = o.getContext('2d'); g.drawImage(c, 0, 0); var d = g.getImageData(c.width / 2, c.height / 2, 1, 1).data;",
      "return [d[0], d[1], d[2]]; })()"))
    browser$close()
    params <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
    camera <- scene_compile(new_scene_packet(), params$camera, new_scene_viewport(100, 100))$camera
    expected <- shade_cpp(matrix(c(0, 0, 1), 1L), matrix(camera$eye + camera$distance * camera$ray, 1L),
      linear_rgb_3d("#4080C0"), 1L, kernel_materials_3d(material_table_3d(styles[[name]])),
      resolved_light_3d(params$coord_params$light, camera), kernel_view_3d(camera))
    expect_equal(unlist(pixel) / 255, srgb_encode_3d(pmin(expected[1L, ], 1)), tolerance = 2 / 255, info = name)
  }
})
