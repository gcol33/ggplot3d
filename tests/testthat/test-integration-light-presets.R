preset_camera <- function() key_camera_3d(new_camera_3d())
encoded_channels <- function(colour) grDevices::col2rgb(colour)[, 1L] / 255
expect_channels <- function(actual, expected) expect_lt(max(abs(actual - expected)), 1.5 / 255)

test_that("the shade and contrast presets reproduce matplotlib's and ggcube's shading", {
  fill <- "#2E6F9E"
  x <- c(-1, -0.4, 0, 0.5, 1)
  mpl <- shade_swatch_3d(fill, x, light_3d(preset = "shade"), preset_camera())
  for (k in seq_along(x)) expect_channels(encoded_channels(mpl[1L, k]), encoded_channels(fill) * (0.65 + 0.35 * x[k]))
  gg <- shade_swatch_3d(fill, x, light_3d(preset = "contrast"), preset_camera())
  hsv <- grDevices::rgb2hsv(grDevices::col2rgb(fill))
  for (k in seq_along(x)) {
    v <- if (x[k] < 0) hsv[3L] * (1 + x[k]) else hsv[3L] + x[k] * (1 - hsv[3L])
    expect_channels(encoded_channels(gg[1L, k]), encoded_channels(grDevices::hsv(hsv[1L], hsv[2L], v)))
  }
})

test_that("multiply works on encoded values with linear = FALSE, capped at full light", {
  fill <- "#2E6F9E"
  light <- light_3d(light_directional_3d(intensity = 0.8), ambient = 0.8, mode = "multiply", linear = FALSE)
  lit <- shade_swatch_3d(fill, c(0, 0.5, 1), light, preset_camera())
  expect_channels(encoded_channels(lit[1L, 1L]), encoded_channels(fill) * 0.8)
  expect_channels(encoded_channels(lit[1L, 2L]), encoded_channels(fill))
  expect_equal(lit[1L, 3L], lit[1L, 2L])
  linear <- shade_swatch_3d(fill, 0, light_3d(light_directional_3d(intensity = 0.8), ambient = 0.8, mode = "multiply"), preset_camera())
  expect_gt(sum(encoded_channels(linear[1L, 1L])), sum(encoded_channels(lit[1L, 1L])))
})

test_that("a coloured ambient light tints in its own channels", {
  light <- light_3d(light_directional_3d(intensity = 0), ambient = "red", mode = "multiply")
  expect_equal(light$ambient, c(1, 0, 0))
  lit <- shade_swatch_3d("white", 0, light, preset_camera())
  expect_equal(lit[1L, 1L], "#FF0000")
  expect_equal(light_3d(ambient = 0.2)$ambient, rep(0.2, 3L))
  encoded <- light_3d(ambient = grDevices::rgb(0.4, 0.2, 0.2), mode = "multiply", linear = FALSE)$ambient
  expect_channels(encoded, c(0.4, 0.2, 0.2))
  expect_error(light_3d(ambient = -1), "ambient")
  expect_error(light_3d(ambient = c("red", "blue")), "ambient")
})

test_that("presets set their lighting and explicit arguments replace it", {
  for (name in c("default", "contrast", "shade", "headlight", "soft", "studio")) expect_s3_class(light_3d(preset = name), "ggplot3d_lights")
  default <- light_3d()
  expect_identical(default$mode, "lightness")
  expect_equal(default$range, c(-0.45, 0.4))
  expect_equal(default$neutral, 60)
  tone <- light_3d(mode = "tone")
  expect_equal(tone$range, c(-0.6, 0.6))
  expect_equal(tone$neutral, 45)
  rgl <- light_3d(preset = "headlight")
  expect_false(rgl$linear)
  expect_identical(rgl$material$lighting, "vertex")
  expect_equal(rgl$material$shininess, 50)
  own <- light_3d(light_head_3d(), preset = "headlight", ambient = 0.1, material = material_3d("satin"))
  expect_equal(own$lights[[1L]]$kind, light_kinds_3d[["head"]])
  expect_equal(own$ambient, rep(0.1, 3L))
  expect_identical(own$material$lighting, "pixel")
  expect_length(light_3d(preset = "studio")$lights, 4L)
  expect_error(light_3d(preset = "povray"))
})

test_that("the lighting's material reaches shaded layers that set none", {
  d <- expand.grid(x = 1:4, y = 1:4)
  d$z <- d$x * d$y
  plain <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d() + coord_3d())$packet
  shiny <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(material = material_3d("shiny")) + coord_3d())$packet
  light <- light_3d(material = material_3d("satin"))
  out <- light_material_packet_3d(plain, light)
  expect_equal(nrow(out$materials), 1L)
  expect_equal(out$materials$specular, 0.15)
  expect_true(all(out$styles$material_id[out$styles$shade %in% TRUE] == 1L))
  kept <- light_material_packet_3d(shiny, light)
  expect_identical(kept$materials, shiny$materials)
  expect_identical(light_material_packet_3d(plain, light_3d()), plain)
  merged <- merge_scene_packets(list(light_material_packet_3d(shiny, light), out), 1L)
  expect_equal(nrow(merged$materials), 2L)
})

test_that("vertex lighting bakes the lit corner colours into the packet", {
  d <- expand.grid(x = seq(-2, 2, length.out = 9), y = seq(-2, 2, length.out = 9))
  d$z <- exp(-(d$x^2 + d$y^2) / 2)
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#2E6F9E", shading = "smooth", colour = NA,
      material = material_3d(specular = 1, shininess = 50, lighting = "vertex")) + coord_3d())$packet
  viewport <- new_scene_viewport(80, 80, dpi = 100)
  camera <- scene_compile(new_scene_packet(), new_camera_3d(), viewport)$camera
  light <- light_3d(light_directional_3d(c(0, sinpi(15 / 180), cospi(15 / 180)), intensity = 1), ambient = 0, mode = "multiply")
  lit <- vertex_lit_packet_3d(packet, camera, light)
  expect_false(any(lit$styles$shade %in% TRUE))
  rgb <- cbind(lit$corners$red, lit$corners$green, lit$corners$blue)
  expect_true(all(is.finite(rgb)))
  expect_gt(max(rgb), 0.95)
  expect_lt(min(rgb[, 3L]), encoded_channels("#2E6F9E")[3L])
  expect_identical(vertex_lit_packet_3d(packet, camera, NULL), packet)
  pixel <- packet
  pixel$materials$lighting <- "pixel"
  expect_identical(vertex_lit_packet_3d(pixel, camera, light), pixel)
})

test_that("vertex-lit surfaces paint alike in raster and vector output", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  d <- expand.grid(x = seq(-2, 2, length.out = 9), y = seq(-2, 2, length.out = 9))
  d$z <- exp(-(d$x^2 + d$y^2) / 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#2E6F9E", shading = "smooth", colour = NA) +
    ggplot2::theme_void()
  render <- function(backend) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, 240L, 240L, res = 100)
    print(p + coord_3d(light = light_3d(preset = "headlight"), backend = backend_3d(backend), panes = character(),
      axes = character(), box = "none"))
    grDevices::dev.off()
    png::readPNG(path)[, , 1:3] * 255
  }
  raster <- render("raster"); vector <- render("vector")
  inside <- rowSums(abs(raster - 255), dims = 2L) > 30 & rowSums(abs(vector - 255), dims = 2L) > 30
  expect_gt(sum(inside), 5000L)
  difference <- apply(abs(raster - vector), c(1L, 2L), max)[inside]
  expect_lt(stats::quantile(difference, 0.95), 12)
  expect_gt(max(raster), 240)
})

test_that("the viewer receives the encoded flag and vertex lighting", {
  light <- view_lights_3d(light_3d(preset = "headlight"))
  expect_equal(light$encoded, 1)
  expect_equal(view_lights_3d(light_3d())$encoded, 0)
  table <- material_table_3d(material_3d(lighting = "vertex"))
  materials <- view_materials_3d(list(list()), data.frame(material_id = 1L), table)
  expect_true(materials[[1L]]$vertex)
  expect_match(shade_glsl_cpp(8L), "uEncoded", fixed = TRUE)
})

test_that("a viewer distance places the specular eye on the view axis", {
  viewport <- new_scene_viewport(80, 80, dpi = 100)
  camera <- scene_compile(new_scene_packet(), new_camera_3d(), viewport)$camera
  far <- kernel_view_3d(camera, light_3d())
  expect_false(far$perspective)
  near <- kernel_view_3d(camera, light_3d(viewer = 2))
  expect_true(near$perspective)
  target <- camera$eye + camera$distance * camera$ray
  expect_equal(sqrt(sum((near$eye - target)^2)), 2 * 0.5 * sqrt(sum(camera$model_scale^2)))
  expect_equal(light_3d(preset = "headlight")$viewer, 1 / sinpi(15 / 180))
  expect_error(light_3d(viewer = 0.5), "viewer")
  expect_equal(view_lights_3d(light_3d(viewer = 3))$viewer, 3)
})

test_that("colour edges flip toward the diagonal of similar colours, only on flat quads", {
  position <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 0, 0), c(1, 1, 0), c(0, 1, 0))
  vertex <- c(1L, 2L, 3L, 1L, 3L, 4L)
  tri <- rbind(1:3, 4:6)
  rgb <- rbind(c(0, 0, 0), c(0.5, 0.5, 0.5), c(1, 1, 1), c(0, 0, 0), c(1, 1, 1), c(0.5, 0.5, 0.5))
  flipped <- flip_colour_edges_3d(tri, vertex, rgb, c(1L, 1L), position)
  sets <- sort(apply(matrix(vertex[flipped], 2L), 1L, function(t) paste(sort(t), collapse = "")))
  expect_identical(sets, c("124", "234"))
  expect_identical(flip_colour_edges_3d(tri, vertex, rgb, c(1L, 2L), position), tri)
  creased <- position; creased[c(3L, 5L), 3L] <- 1
  expect_identical(flip_colour_edges_3d(tri, vertex, rgb, c(1L, 1L), creased), tri)
  similar <- rbind(c(0.5, 0.5, 0.5), c(0, 0, 0), c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5), c(1, 1, 1))
  expect_identical(flip_colour_edges_3d(tri, vertex, similar, c(1L, 1L), position), tri)
})

test_that("encoded corner colours are subdivided only for encoded vertex lighting", {
  d <- expand.grid(x = seq(-2, 2, length.out = 9), y = seq(-2, 2, length.out = 9))
  d$z <- exp(-(d$x^2 + d$y^2) / 2)
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#2E6F9E", shading = "smooth", colour = NA) + coord_3d())$packet
  camera <- scene_compile(new_scene_packet(), new_camera_3d(), new_scene_viewport(80, 80, dpi = 100))$camera
  encoded <- light_3d(preset = "headlight")
  lit <- vertex_lit_packet_3d(light_material_packet_3d(packet, encoded), camera, encoded)
  expect_gt(length(lit$faces$count), length(packet$faces$count))
  expect_true(all(lit$faces$count == 3L))
  expect_silent(validate_scene_packet(lit))
  linear <- light_3d(preset = "headlight", linear = TRUE)
  plain <- vertex_lit_packet_3d(light_material_packet_3d(packet, linear), camera, linear)
  expect_equal(length(plain$faces$count), length(packet$faces$count))
})

test_that("subdividing encoded corner colours keeps the strokes of the packet in place", {
  d <- expand.grid(x = seq(-2, 2, length.out = 9), y = seq(-2, 2, length.out = 9))
  d$z <- exp(-(d$x^2 + d$y^2) / 2)
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#2E6F9E", shading = "smooth", edges = edges_3d(),
      outline = outline_3d(silhouette = FALSE, crease = NULL)) + coord_3d())$packet
  camera <- scene_compile(new_scene_packet(), new_camera_3d(), new_scene_viewport(80, 80, dpi = 100))$camera
  encoded <- light_3d(preset = "headlight")
  lit <- vertex_lit_packet_3d(light_material_packet_3d(packet, encoded), camera, encoded)
  expect_gt(length(lit$faces$count), length(packet$faces$count))
  stroke_points <- function(p) {
    at <- unlist(lapply(seq_along(p$polylines$offset), function(k) p$polylines$offset[k] + seq_len(p$polylines$count[k]) - 1L))
    v <- p$corners$vertex[at]
    cbind(p$vertices$x[v], p$vertices$y[v], p$vertices$z[v])
  }
  expect_equal(stroke_points(lit), stroke_points(packet))
  expect_identical(lit$polylines$count, packet$polylines$count)
  expect_silent(validate_scene_packet(lit))
})
