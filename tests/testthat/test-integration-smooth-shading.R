smooth_triangle_packet <- function(rgb = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)), normals = NULL, w = c(1, 1, 1)) {
  corners <- list(vertex = 1:3, source_row = 1:3)
  if (!is.null(normals)) corners[c("normal_x", "normal_y", "normal_z")] <- lapply(1:3, function(k) normals[, k])
  if (!is.null(rgb)) corners[c("red", "green", "blue")] <- lapply(1:3, function(k) rgb[, k])
  new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5) * w, y = c(0.1, 0.1, 0.9) * w, z = c(0.5, 0.5, 0.5) * w, w = w),
    corners = corners, faces = list(offset = 1L, layer_id = 1L),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = NA_character_, shade = FALSE))
}

top_camera <- function() new_camera_3d(elevation = 90, azimuth = -90)

unpack_pixels <- function(image) {
  n <- dim(image)
  values <- matrix(as.integer(image), n[1L], n[2L], byrow = TRUE)
  list(n = n, get = function(row, col) {
    v <- values[row, col]
    vapply(c(0L, 8L, 16L), function(shift) bitwAnd(bitwShiftR(v, shift), 255L) / 255, numeric(1))
  })
}

encode <- function(v) ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)
decode <- function(v) ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)

test_that("smooth surfaces emit corner normals and per-vertex fills", {
  d <- expand.grid(x = seq(-1, 1, length.out = 5), y = seq(-1, 1, length.out = 5))
  d$z <- with(d, x^2 + y^2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z))
  flat <- stats_scene(p + geom_surface_3d(shading = "flat") + coord_3d())$packet
  smooth <- stats_scene(p + geom_surface_3d(shading = "smooth") + coord_3d())$packet
  expect_false(any(c("normal_x", "red") %in% names(flat$corners)))
  expect_identical(names(smooth$corners), c("vertex", "source_row", "normal_x", "normal_y", "normal_z", "red", "green", "blue"))
  normal <- cbind(smooth$corners$normal_x, smooth$corners$normal_y, smooth$corners$normal_z)
  expect_equal(sqrt(rowSums(normal^2)), rep(1, nrow(normal)))
  rgb <- cbind(smooth$corners$red, smooth$corners$green, smooth$corners$blue)
  expect_gt(nrow(unique(rgb)), 5L)
  expect_error(geom_surface_3d(shading = "gouraud"), "flat")
})

test_that("compiled vertices interpolate corner attributes with the homogeneous weights", {
  camera <- top_camera()
  viewport <- new_scene_viewport(60, 60)
  for (w in list(c(1, 1, 1), c(2, 0.5, 3))) {
    scene <- scene_compile(smooth_triangle_packet(w = w), camera, viewport)
    attributes <- scene_vertex_attributes_3d(scene, corner_attribute_columns_3d)
    expect_identical(colnames(attributes), c("red", "green", "blue"))
    expect_equal(unname(attributes), diag(3))
  }
  packet <- smooth_triangle_packet()
  packet$vertices$x[2L] <- 1.5
  scene <- scene_compile(packet, camera, viewport)
  attributes <- scene_vertex_attributes_3d(scene, c("red", "green", "blue"))
  xy <- cbind(scene$vertices$x, scene$vertices$y)
  corners <- cbind(c(0.1, 1.5, 0.5), c(0.1, 0.1, 0.9))
  bary <- t(apply(xy, 1L, function(p) solve(rbind(t(corners), 1), c(p, 1))))
  expect_equal(unname(attributes), unname(bary), tolerance = 1e-9)
  expect_true(any(abs(scene$vertices$x - 1) < 1e-12))
})

test_that("munching keeps corner attributes and maps normals", {
  frame <- coord_cylindrical_3d()$frame
  packet <- smooth_triangle_packet(normals = matrix(c(0, 0, 1), 3L, 3L, byrow = TRUE))
  out <- munch_packet_3d(frame, packet)
  expect_true(all(c("normal_z", "red") %in% names(out$corners)))
  expect_equal(out$corners$red + out$corners$green + out$corners$blue, rep(1, length(out$corners$vertex)))
  expect_equal(abs(out$corners$normal_z), rep(1, length(out$corners$vertex)), tolerance = 1e-6)
  linear <- list(linear = TRUE, map = function(p) cbind(2 * p[, 1L], p[, 2L], p[, 3L]))
  tilted <- smooth_triangle_packet(normals = matrix(c(1, 0, 1) / sqrt(2), 3L, 3L, byrow = TRUE))
  mapped <- munch_packet_3d(linear, tilted)
  expect_equal(mapped$corners$normal_x, rep(1 / sqrt(5), 3L))
  expect_equal(mapped$corners$normal_z, rep(2 / sqrt(5), 3L))
})

test_that("the raster backend interpolates corner colours per sample in linear light", {
  packet <- smooth_triangle_packet()
  viewport <- new_scene_viewport(40, 40, dpi = 40 * 25.4 / 40)
  result <- raster_scene_3d(packet, top_camera(), viewport, 1L)
  scene <- scene_compile(packet, top_camera(), viewport)
  pixels <- unpack_pixels(result$image)
  extent <- result$extent
  corners <- cbind(scene$vertices$x_npc, scene$vertices$y_npc)
  colours <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  for (b in list(c(0.8, 0.1, 0.1), c(1, 1, 1) / 3, c(0.2, 0.5, 0.3))) {
    point <- drop(b %*% corners)
    col <- floor((point[1L] - extent[1L]) / (extent[3L] - extent[1L]) * pixels$n[2L]) + 1
    row <- pixels$n[1L] - floor((point[2L] - extent[2L]) / (extent[4L] - extent[2L]) * pixels$n[1L])
    centre <- c((col - 0.5) / pixels$n[2L] * (extent[3L] - extent[1L]) + extent[1L],
                (pixels$n[1L] - row + 0.5) / pixels$n[1L] * (extent[4L] - extent[2L]) + extent[2L])
    weights <- solve(rbind(t(corners), 1), c(centre, 1))
    expect_equal(pixels$get(row, col), encode(drop(weights %*% decode(colours))), tolerance = 1.5 / 255)
  }
})

test_that("lit smooth faces vary across a face in raster and vector output", {
  normals <- rbind(c(-0.6, 0, 0.8), c(0.6, 0, 0.8), c(0, 0.6, 0.8))
  packet <- smooth_triangle_packet(rgb = NULL, normals = normals)
  packet$styles$shade <- TRUE
  viewport <- new_scene_viewport(40, 40, dpi = 40 * 25.4 / 40)
  light <- light_3d(direction = c(1, 0, 1), space = "data")
  result <- raster_scene_3d(packet, top_camera(), viewport, 1L, light = light)
  values <- unique(as.integer(result$image))
  expect_gt(length(values), 20L)
  flat <- packet
  flat$corners <- flat$corners[c("vertex", "source_row")]
  expect_lte(length(unique(as.integer(raster_scene_3d(flat, top_camera(), viewport, 1L, light = light)$image))), 3L)
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 200, 200)
  on.exit(grDevices::dev.off(), add = TRUE)
  scene <- scene_compile(packet, top_camera(), viewport)
  grob <- render_scene_grid(scene, viewport, light = light)
  fill <- grob$children[[1L]]$gp$fill
  expect_true(is.list(fill))
  expect_s3_class(fill[[1L]], "GridLinearGradient")
})

test_that("vector smooth paint stays within the colour tolerance", {
  x <- c(0, 40, 10); y <- c(0, 0, 30)
  linear <- decode(rbind(c(0.9, 0.3, 0.2), c(0.2, 0.8, 0.3), c(0.3, 0.3, 0.9)))
  tolerance <- 4 / 255
  painted <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = FALSE, tolerance = tolerance)
  expect_gt(max(painted$id), 16L)
  expect_true(all(!painted$gradient))
  shape <- split(seq_along(painted$id), painted$id)
  worst <- 0
  for (k in seq_along(shape)) {
    i <- shape[[k]]
    flat <- grDevices::col2rgb(painted$flat[k]) / 255
    for (corner in i) {
      b <- solve(rbind(rbind(x, y), 1), c(painted$x_mm[corner], painted$y_mm[corner], 1))
      worst <- max(worst, abs(encode(drop(b %*% linear)) - flat))
    }
  }
  expect_lte(worst, tolerance + 1 / 255)
  grey <- decode(c(0.2, 0.5, 0.9)) %o% c(1, 1, 1)
  graded <- smooth_face_paint_3d(x, y, rep(1L, 3L), grey, gradients = TRUE, tolerance = tolerance)
  expect_identical(graded$gradient, TRUE)
  stops <- graded$paint[[1L]]
  expect_gt(length(stops$stops), 2L)
})

test_that("a fan triangle of one colour inside a varied face is painted flat", {
  x <- c(0, 40, 40, 0); y <- c(0, 0, 30, 30)
  linear <- decode(rbind(c(0.4, 0.5, 0.6), c(0.4, 0.5, 0.6), c(0.4, 0.5, 0.6), c(0.8, 0.5, 0.2)))
  painted <- smooth_face_paint_3d(x, y, rep(1L, 4L), linear, gradients = TRUE, tolerance = 2 / 255)
  expect_true(all(painted$face == 1L))
  expect_identical(sum(painted$gradient), 1L)
  expect_true(all(is.finite(painted$x_mm)))
  expect_true(painted$flat[!painted$gradient][1L] == encode_rgb_3d(linear[1L, , drop = FALSE]))
})

test_that("isosurface fill is interpolated to the vertices", {
  d <- expand.grid(x = seq(-1, 1, length.out = 7), y = seq(-1, 1, length.out = 7), z = seq(-1, 1, length.out = 7))
  d$value <- with(d, x^2 + y^2 + z^2)
  stat <- StatIsosurface3D
  scales <- list(x = ggplot2::scale_x_continuous(), y = ggplot2::scale_y_continuous(), z = scale_z_continuous())
  d$fill <- d$x + 2 * d$y
  smooth <- stat$compute_group(d, scales, levels = 0.5, shading = "smooth")
  expect_equal(smooth$fill, smooth$x + 2 * smooth$y, tolerance = 1e-12)
  flat <- stat$compute_group(d, scales, levels = 0.5)
  expect_equal(flat$fill, rep(colMeans(matrix(smooth$fill, nrow = 3L)), each = 3L))
})

test_that("viewer packets carry corner normals and colours", {
  packet <- smooth_triangle_packet(normals = matrix(c(0, 0, 1), 3L, 3L, byrow = TRUE))
  view <- view_packet_3d(packet)
  expect_true(all(c("normal_x", "red", "green", "blue") %in% names(view$corners)))
})
