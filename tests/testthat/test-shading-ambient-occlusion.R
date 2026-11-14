ao_ceiling_mesh <- function(alpha = 1, height = 0.25) {
  list(position = rbind(c(-2, -2, height), c(2, -2, height), c(2, 2, height), c(-2, 2, height)),
    normal = matrix(0, 4L, 3L), colour = cbind(matrix(0, 4L, 3L), alpha),
    triangles = matrix(c(1L, 2L, 3L, 1L, 3L, 4L), 3L))
}

test_that("hemisphere accessibility recovers a finite-radius ceiling and alpha transmission", {
  prepare <- function(alpha = 1, amount = 1, radius = 0.5)
    ggplot3d:::ambient_occlusion_prepare_cpp(ao_ceiling_mesh(alpha), 64L, radius, amount, 1e-8)
  query <- function(pointer, normal = c(0, 0, 1))
    ggplot3d:::ambient_occlusion_cpp(pointer, matrix(c(0, 0, 0), 1L), matrix(normal, 1L))
  set.seed(453); seed <- .Random.seed
  expect_equal(query(prepare()), 0.5, tolerance = 1e-14)
  expect_equal(query(prepare(0.5)), 0.75, tolerance = 1e-14)
  expect_equal(query(prepare(amount = 0.4)), 0.8, tolerance = 1e-14)
  expect_equal(query(prepare(radius = 0.1)), 1)
  expect_equal(query(prepare(), c(0, 0, -1)), 1)
  expect_equal(query(prepare(0)), 1)
  expect_identical(.Random.seed, seed)
  context <- prepare()
  expect_identical(query(context), query(context))
  expect_error(depth_cue_3d(ao = 2), "ao")
  expect_error(depth_cue_3d(ao_radius = 0), "ao_radius")
  expect_error(depth_cue_3d(ao_samples = 2.5), "positive whole")
})

test_that("AO transmission uses the texture sampler and its alpha over the face fill", {
  mesh <- ao_ceiling_mesh()
  mesh$uv <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
  mesh$texture <- rep(1L, 2L); mesh$fill_alpha <- rep(0, 2L); mesh$interpolate <- FALSE
  for (colour in c("#FFFFFF00", "#FFFFFF80", "#FFFFFFFF")) {
    mesh$textures <- list(texture_3d(matrix(colour, 2L, 2L))$image)
    pointer <- ggplot3d:::ambient_occlusion_prepare_cpp(mesh, 64L, 0.5, 1, 1e-8)
    expected <- 1 - 0.5 * as.double(grDevices::col2rgb(colour, alpha = TRUE)[4L, 1L]) / 255
    expect_equal(as.double(ggplot3d:::ambient_occlusion_cpp(pointer, matrix(c(0, 0, 0), 1L),
      matrix(c(0, 0, 1), 1L))), expected, tolerance = 1e-14)
  }
})

ao_corner_plot <- function(backend = "vector", ao = TRUE, light = light_3d(ambient = 1, diffuse = 0)) {
  floor <- expand.grid(x = seq(0, 1, length.out = 9), y = seq(0, 1, length.out = 9)); floor$z <- 0
  vertices <- rbind(c(0, 0, 0), c(0, 1, 0), c(0, 1, 1), c(0, 0, 1))
  ggplot2::ggplot(floor, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#BBBBBB", colour = NA) +
    geom_mesh_3d(vertices = vertices, faces = rbind(c(1L, 2L, 3L), c(1L, 3L, 4L)),
      fill = "#BBBBBB", colour = NA, inherit.aes = FALSE) +
    coord_3d(azimuth = 40, elevation = 30, expand = FALSE, light = light,
      xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), axes = character(), panes = character(), box = "none",
      backend = backend_3d(backend, dpi = 40), depth_cue = depth_cue_3d(fog = NA, ao = ao, ao_radius = 0.5))
}

test_that("vector vertices and raster samples consume the same geometric visibility", {
  input <- integration_packets(ao_corner_plot())
  q <- input$params; viewport <- new_scene_viewport(50, 50, 30)
  light <- panel_lights_3d(q$coord_params$light, q)
  light <- ambient_light_3d(input$packet, q$camera, viewport, light)
  expect_false(is.null(light$occlusion))
  expect_equal(as.double(ggplot3d:::ambient_occlusion_cpp(light$occlusion, matrix(c(0, 0.5, 0), 1L),
    matrix(c(0, 0, 1), 1L))), 0.5, tolerance = 2 / 64)
  scene <- scene_compile(input$packet, q$camera, viewport)
  styles <- scene$styles[match(scene$fragments$style_id, scene$styles$style_id), , drop = FALSE]
  faces <- which(scene$fragments$kind == 1L)
  attributes <- scene_linear_attributes_3d(scene)
  expect_true(all(smooth_fragments_3d(scene, attributes, styles, light)[faces]))
  lit <- fragment_vertex_linear_3d(scene, faces, styles[faces, , drop = FALSE], attributes, light)$linear
  plain <- light; plain$occlusion <- NULL; plain$ao <- NULL
  base <- fragment_vertex_linear_3d(scene, faces, styles[faces, , drop = FALSE], attributes, plain)$linear
  expect_true(all(lit <= base + 1e-14))
  expect_lt(min(lit[, 1L]), 0.8 * max(base[, 1L]))
  raster <- raster_scene_3d(input$packet, q$camera, viewport, 1L, light)
  unlit <- raster_scene_3d(input$packet, q$camera, viewport, 1L, plain)
  rgba <- unpack_rgba(raster$image); rgb0 <- unpack_rgba(unlit$image)
  interior <- rgba[, 4L] == 1 & rgb0[, 4L] == 1
  expect_true(all(rgba[interior, 1:3] <= rgb0[interior, 1:3] + 1 / 255))
  expect_gt(sum(rgb0[interior, 1L] - rgba[interior, 1L] > 0.05), 0L)
  options_old <- options(ggplot3d.threads = 1L); on.exit(options(options_old))
  serial <- raster_scene_3d(input$packet, q$camera, viewport, 1L, light)
  options(ggplot3d.threads = 2L)
  expect_identical(serial$image, raster_scene_3d(input$packet, q$camera, viewport, 1L, light)$image)
  off <- q; off$coord_params$depth_cue <- NULL
  expect_null(panel_lights_3d(light, off)$occlusion)
  expect_null(panel_lights_3d(light, off)$ao)
})

test_that("AO and fog preserve text and label colours above geometry", {
  p <- ao_corner_plot() + geom_label_3d(data = data.frame(x = 0.5, y = 0.5, z = 0, label = "label"),
    ggplot2::aes(label = label), colour = "red", fill = "yellow", inherit.aes = TRUE)
  for (backend in c("vector", "raster", "hybrid")) {
    plot <- p + coord_3d(backend = backend_3d(backend, dpi = 25),
      depth_cue = depth_cue_3d(fog = "white", amount = 1, ao = TRUE))
    input <- integration_packets(plot)
    original <- input$packet$styles[match(input$packet$text$style_id, input$packet$styles$style_id), , drop = FALSE]
    packet <- depth_cue_packet_3d(input$packet, input$params$coord_params$depth_cue, "white", function(p) rep(1, nrow(p)))
    styles <- packet$styles[match(packet$text$style_id, packet$styles$style_id), , drop = FALSE]
    expect_identical(styles$colour, original$colour)
    expect_identical(styles$fill, original$fill)
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, 250, 250)
    expect_no_error(g <- grid::grid.force(ggplot2::ggplotGrob(plot)))
    if (backend == "vector") {
      scene <- motion_panels_3d(g)[[1L]]$scene
      order <- scene$fragments$kind[scene$draw_order]
      expect_identical(tail(order, 1L), 4L)
      text <- which(scene$fragments$kind == 4L)
      expect_true(all(!scene$fragments$hidden[text]))
    }
    grDevices::dev.off(); unlink(path)
  }
})
