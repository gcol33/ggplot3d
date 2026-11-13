integration_packets <- function(plot) {
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(tempfile(fileext = ".pdf"))
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  built <- ggplot2::ggplot_build(plot)
  grobs <- lapply(seq_along(built$plot@layers), function(i) built$plot@layers[[i]]$draw_geom(built$data[[i]], built$layout)[[1L]])
  collected <- collect_scene_layers(grobs, built$layout$panel_params[[1]], built$plot@theme)
  list(packet = merge_scene_packets(collected$packets, 1L), params = built$layout$panel_params[[1]], grobs = grobs)
}

test_that("all supported geometries merge into one packet with provenance", {
  points <- data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1))
  surface <- expand.grid(x = 0:1, y = 0:1); surface$z <- 0.5
  polygon <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0)
  p <- ggplot2::ggplot(points, ggplot2::aes(x, y, z = z)) +
    geom_point_3d(colour = "#FF000080", alpha = 0.5) + geom_path_3d() +
    geom_segment_3d(ggplot2::aes(zend = 0)) + geom_text_3d(label = "a") +
    geom_polygon_3d(data = polygon) + geom_surface_3d(data = surface) + coord_3d()
  scene <- integration_packets(p)
  packet <- scene$packet
  expect_silent(validate_scene_packet(packet))
  ids <- unlist(lapply(packet[c("faces", "polylines", "markers", "text")], `[[`, "layer_id"))
  expect_equal(sort(unique(ids)), 1:6)
  expect_equal(packet$markers$source_row, 1:2)
  expect_equal(packet$markers$opacity, rep(128 / 255 * 0.5, 2))
  expect_equal(packet$styles$alpha, rep(1, nrow(packet$styles)))
  expect_equal(length(packet$faces$primitive_id), 4)
  expect_equal(length(packet$markers$primitive_id), 2)
  compiled <- scene_compile(packet, scene$params$camera, new_scene_viewport(100, 100))
  expect_equal(compiled$panel_id, 1L)
  expect_true(all(compiled$fragments$layer_id %in% 1:6))
  expect_setequal(unique(unlist(lapply(compiled$source[c("faces", "polylines", "markers", "text")], `[[`, "layer_id"))), 1:6)
})

test_that("path missing values break connectivity and segment endpoints default", {
  data <- data.frame(x = 1:5, y = 1:5, z = c(1, 2, NA, 4, 5))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_path_3d(na.rm = TRUE) + coord_3d()
  packet <- integration_packets(p)$packet
  expect_equal(length(packet$polylines$primitive_id), 2L)
  expect_false(3L %in% packet$corners$source_row)
  p <- ggplot2::ggplot(data[1:2, ], ggplot2::aes(x, y, z = z)) + geom_segment_3d(zend = 0) + coord_3d()
  expect_equal(length(integration_packets(p)$packet$polylines$primitive_id), 2L)
})

test_that("grid surfaces mask missing cells and reject duplicate coordinates", {
  data <- expand.grid(x = 1:3, y = 1:3); data$z <- data$x + data$y
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_surface_3d() + coord_3d()
  expect_equal(length(integration_packets(p)$packet$faces$primitive_id), 8L)
  data$z[data$x == 2] <- NA
  p$data <- data
  expect_equal(length(integration_packets(p)$packet$faces$primitive_id), 0L)
  data$z <- 1; p$data <- rbind(data, data[1, ])
  expect_warning(ggplot2::ggplot_build(p), "unique")
})

test_that("Delaunay surfaces lift scattered data and enforce max_edge", {
  data <- data.frame(x = c(0, 1, 1, 0, 0.4), y = c(0, 0, 1, 1, 0.6), z = c(0, 0, 1, 1, 0.6))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_surface_3d(method = "delaunay") + coord_3d()
  packet <- integration_packets(p)$packet
  expect_equal(length(packet$faces$primitive_id), 4L)
  expect_equal(sort(unique(packet$corners$source_row)), 1:5)
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_surface_3d(method = "delaunay", max_edge = 0.01) + coord_3d()
  expect_equal(length(integration_packets(p)$packet$faces$primitive_id), 0L)
})

test_that("polygon triangulation preserves concavity, holes and boundary edges", {
  data <- data.frame(x = c(0, 4, 4, 0, 1, 1, 3, 3), y = c(0, 0, 4, 4, 1, 3, 3, 1), z = 0,
                     ring = rep(1:2, each = 4))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z, group = 1, subgroup = ring)) +
    geom_polygon_3d(fill = "grey70", colour = "black") + coord_3d(expand = FALSE)
  packet <- integration_packets(p)$packet
  area <- 0
  for (i in seq_along(packet$faces$primitive_id)) {
    ids <- packet$corners$vertex[seq.int(packet$faces$offset[i], length.out = packet$faces$count[i])]
    xy <- cbind(packet$vertices$x[ids], packet$vertices$y[ids])
    area <- area + abs(cross2_3d(xy[2, ] - xy[1, ], xy[3, ] - xy[1, ])) / 2
  }
  expect_equal(area, 12 / 16)
  expect_equal(length(packet$polylines$primitive_id), 2L)
  expect_equal(packet$polylines$count, c(5L, 5L))
})

test_that("packet remapping accepts typed empty buffers and collector rejects untagged drawing", {
  empty <- new_scene_packet()
  expect_identical(merge_scene_packets(list(empty, empty), 1L), empty)
  params <- list(panel_id = 1L)
  expect_error(collect_scene_layers(list(grid::rectGrob()), params, ggplot2::theme_get()), "Unsupported nonempty")
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_tile() + coord_3d()), "No 3D emitter")
})

test_that("concave polygon triangles conserve area and reject invalid topology", {
  data <- data.frame(x = c(0, 2, 2, 1, 1, 0), y = c(0, 0, 1, 1, 2, 2), z = 0)
  topology <- triangulate_polygon_3d(data, list(1:6))
  area <- sum(vapply(topology$triangles, function(ids) {
    xy <- as.matrix(data[ids, c("x", "y")])
    abs(cross2_3d(xy[2, ] - xy[1, ], xy[3, ] - xy[1, ])) / 2
  }, numeric(1)))
  expect_equal(area, 3)
  expect_length(topology$triangles, 4L)
  data$z[3L] <- 1
  expect_error(triangulate_polygon_3d(data, list(1:6)), "planar")
  data <- data.frame(x = c(0, 1, 0, 1), y = c(0, 1, 1, 0), z = 0)
  expect_error(triangulate_polygon_3d(data, list(1:4)), "simple")
})

test_that("projection uses native anchors and keeps whole-source identities", {
  p <- ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(shape = "circle") + coord_3d(expand = FALSE)
  scene <- integration_packets(p)
  built <- ggplot2::ggplot_build(p)
  projected <- built$layout$coord$transform(built$data[[1]], scene$params)
  expect_true(all(projected$.visible))
  expect_equal(projected$.source, 1:2)
  expect_equal(projected$.z_model, c(0, 1))
  compiled <- scene_compile(scene$packet, scene$params$camera, new_scene_viewport(100, 100))
  rows <- match(1:2, compiled$fragments$source_row)
  expect_equal(projected$x, compiled$vertices$x_npc[compiled$fragments$offset[rows]])
  expect_equal(projected$y, compiled$vertices$y_npc[compiled$fragments$offset[rows]])
})

test_that("the minimal panel renderer compiles every layer together once", {
  p <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2, z = 1:2), ggplot2::aes(x, y, z = z)) +
    geom_point_3d() + geom_path_3d() + coord_3d()
  scene <- integration_packets(p)
  count <- 0L
  compiler <- scene_compile
  testthat::local_mocked_bindings(scene_compile = function(...) {count <<- count + 1L; compiler(...)})
  grob <- minimal_scene_panel_3d(list(scene$packet), scene$params)
  expect_equal(count, 1L)
  expect_s3_class(grob, "gTree")
  expect_equal(length(grob$children), length(grob$scene$draw_order))
  expect_setequal(c(grob$scene$source$markers$layer_id, grob$scene$source$polylines$layer_id), 1:2)
})
