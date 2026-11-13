empty_scene <- function() ggplot3d:::new_scene_packet()

face_scene <- function(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1),
                       z = c(0.5, 0.5, 0.5, 0.5)) {
  ggplot3d:::new_scene_packet(
    vertices = list(x = x, y = y, z = z),
    corners = list(vertex = seq_along(x)),
    faces = list(offset = 1L, count = length(x)),
    styles = data.frame(style_id = 1L, fill = "red"))
}

compile_scene <- function(packet, camera = ggplot3d:::new_camera_3d(), width = 100, height = 100) {
  ggplot3d:::scene_compile(packet, camera, ggplot3d:::new_scene_viewport(width, height))
}

test_that("empty packets preserve the complete typed contract", {
  packet <- empty_scene()
  expect_identical(ggplot3d:::validate_scene_packet(packet), packet)
  result <- compile_scene(packet)
  expect_identical(result$draw_order, integer())
  expect_identical(result$fragments$clipped, logical())
  expect_identical(result$vertices$x_npc, double())
  expect_identical(result$schema_version, 1L)
  expect_identical(result$capabilities$visibility, "centroid")
  expect_false(result$capabilities$exact_visibility)
  expect_false(result$capabilities$footprint_clipping)
  expect_s3_class(ggplot3d:::scene_layer_grob(packet), "zeroGrob")
})

test_that("the packet validator rejects malformed storage and foreign keys", {
  packet <- face_scene()
  expect_s3_class(ggplot3d:::scene_layer_grob(packet), "scene_layer_grob")
  expect_error(ggplot3d:::new_scene_packet(vertices = list(x = 1, y = 2)), "requires z")
  expect_error(ggplot3d:::new_scene_packet(corners = list(vertex = 0)), "corner")
  expect_error(ggplot3d:::new_scene_packet(corners = list(vertex = 1.5)), "integers")
  expect_error(face_scene(x = c(NA, 1, 1, 0)), "finite")
  broken <- packet; broken$faces$count <- 100L
  expect_error(ggplot3d:::validate_scene_packet(broken), "corner ranges")
  broken <- packet; broken$faces$style_id <- 2L
  expect_error(ggplot3d:::validate_scene_packet(broken), "metadata")
  broken <- packet; broken$vertices$x <- as.integer(broken$vertices$x)
  expect_error(ggplot3d:::validate_scene_packet(broken), "type")
  broken <- packet; broken$styles$style_id <- 5L
  expect_error(ggplot3d:::validate_scene_packet(broken), "style_id")
  broken <- packet; broken$faces$opacity <- -1
  expect_error(ggplot3d:::validate_scene_packet(broken), "metadata")
  broken <- packet; broken$faces$primitive_id <- 0L
  expect_error(ggplot3d:::validate_scene_packet(broken), "primitive_id")
  broken <- packet; broken$corners$vertex <- factor(broken$corners$vertex)
  expect_error(ggplot3d:::validate_scene_packet(broken), "type")
})

test_that("native entry validates independently of the R wrapper", {
  native <- function(packet, camera = ggplot3d:::new_camera_3d(),
                     viewport = ggplot3d:::new_scene_viewport(100, 100)) {
    ggplot3d:::scene_compile_cpp(packet[names(packet) != "styles"], camera, viewport)
  }
  packet <- face_scene()
  broken <- packet; broken$corners$vertex[1] <- 99L
  expect_error(native(broken), "corner")
  broken <- packet; broken$faces$count <- 2L
  expect_error(native(broken), "corner range")
  broken <- packet; broken$vertices$x <- 1
  expect_error(native(broken), "length mismatch")
  broken <- packet; broken$schema_version <- 2L
  expect_error(native(broken), "schema version")
  broken <- packet; broken$faces$opacity <- NA_real_
  expect_error(native(broken), "non-finite")
  broken <- packet; broken$vertices$x <- c(0L, 1L, 1L, 0L)
  expect_error(native(broken), "column type")
  broken <- packet; broken$corners$vertex <- factor(broken$corners$vertex)
  expect_error(native(broken), "column type")
  camera <- ggplot3d:::new_camera_3d(); camera$fov <- 180
  expect_error(native(packet, camera), "angles")
  expect_error(native(packet, viewport = list(width_mm = 0, height_mm = 1, dpi = 144)), "positive")
  expect_null(native(packet)$styles)
  expect_identical(compile_scene(packet)$styles, packet$styles)
})

test_that("camera values enforce projection and physical viewport conventions", {
  expect_error(ggplot3d:::new_camera_3d(distance = 3), "orthographic")
  expect_error(ggplot3d:::new_camera_3d(projection = "perspective", distance = Inf), "finite")
  expect_error(ggplot3d:::new_camera_3d(elevation = 91), "angles")
  expect_error(ggplot3d:::new_camera_3d(near = 2, far = 1), "less than")
  expect_error(ggplot3d:::new_camera_3d(aspect = c(1, 0, 1)), "positive")
  expect_error(ggplot3d:::new_camera_3d(zoom = 0), "zoom")
  expect_error(ggplot3d:::new_scene_viewport(-1, 100), "positive")
  vp <- ggplot3d:::new_scene_viewport(grid::unit(10, "cm"), grid::unit(2, "cm"))
  expect_equal(vp$width_mm, 100)
  expect_equal(vp$height_mm, 20)
})

test_that("projection agrees with exported matrices in both modes", {
  packet <- face_scene()
  for (mode in c("orthographic", "perspective")) {
    result <- compile_scene(packet, ggplot3d:::new_camera_3d(
      projection = mode, azimuth = 35, elevation = 25, roll = 18,
      aspect = c(1, 2, 3)), width = 180, height = 90)
    v <- result$vertices; camera <- result$camera
    model <- cbind(v$x, v$y, v$z)
    model <- sweep(model, 2L, camera$model_scale, "*")
    camera_xyz <- camera$view %*% t(cbind(model, 1))
    expect_equal(as.double(camera_xyz[1, ]), v$x_camera)
    expect_equal(as.double(camera_xyz[2, ]), v$y_camera)
    expect_equal(as.double(camera_xyz[3, ]), v$depth)
    clip <- camera$projection %*% camera_xyz
    expect_equal(as.double(clip[1, ] / clip[4, ] / 2 + 0.5), v$x_npc)
    expect_equal(as.double(clip[2, ] / clip[4, ] / 2 + 0.5), v$y_npc)
    expect_equal(as.double(clip[4, ]), v$clip_w)
    expect_true(all(v$depth >= camera$near & v$depth <= camera$far))
  }
})

test_that("top view preserves x right y up and positive roll rotates counterclockwise", {
  p <- face_scene()
  camera <- ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90)
  a <- compile_scene(p, camera)
  expect_gt(a$vertices$x_npc[2], a$vertices$x_npc[1])
  expect_gt(a$vertices$y_npc[3], a$vertices$y_npc[2])
  camera$roll <- 90
  b <- compile_scene(p, camera)
  expect_equal(b$vertices$x_npc - 0.5, -(a$vertices$y_npc - 0.5))
  expect_equal(b$vertices$y_npc - 0.5, a$vertices$x_npc - 0.5)
  expect_equal(a$camera$view_height, b$camera$view_height)
  wide <- compile_scene(p, width = 200, height = 100)
  square <- compile_scene(p)
  expect_equal(diff(range(wide$vertices$x_npc))*200,
               diff(range(square$vertices$x_npc))*100)
})

test_that("all primitive tables join one draw order with stable source identity", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = c(0, 1, 1, 0.5, 0.5, 0.5, 0.5),
                    y = c(0, 0, 1, 0.2, 0.8, 0.5, 0.5),
                    z = c(0.5, 0.5, 0.5, 0.25, 0.25, 0.75, 0.9)),
    corners = list(vertex = 1:7),
    faces = list(offset = 1L),
    polylines = list(offset = 4L, width_mm = 0.5, arrow_start = TRUE, arrow_end = TRUE),
    markers = list(offset = 6L, footprint_id = 1L), text = list(offset = 7L),
    footprints = list(kind = 1L, offset = 1L, count = 0L,
                      radius_x_mm = 2, radius_y_mm = 2, angle = 0),
    styles = data.frame(style_id = 1L, fill = "red"))
  before <- serialize(packet, NULL)
  result <- compile_scene(packet, ggplot3d:::new_camera_3d(elevation = 90))
  expect_identical(result$fragments$kind[result$draw_order], c(2L, 1L, 3L, 4L))
  expect_identical(result$fragments$parent_primitive_id, 1:4)
  expect_identical(result$fragments$style_id, rep(1L, 4))
  expect_identical(result$fragments$arrow_start, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(result$fragments$arrow_end, c(FALSE, TRUE, FALSE, FALSE))
  expect_gt(result$fragments$path_end_mm[2], 0)
  expect_identical(serialize(packet, NULL), before)
  expect_identical(compile_scene(packet, ggplot3d:::new_camera_3d(elevation = 90)), result)
  bad <- packet; bad$text$primitive_id <- 1L
  expect_error(ggplot3d:::validate_scene_packet(bad), "unique")
  bad <- packet; bad$markers$footprint_id <- 2L
  expect_error(ggplot3d:::validate_scene_packet(bad), "footprint_id")
})

test_that("box and near-plane splits reconstruct from original corners", {
  packet <- face_scene(x = c(-1, 1, 1, -1))
  result <- compile_scene(packet)
  expect_true(result$fragments$clipped)
  expect_true(all(result$vertices$x >= 0 & result$vertices$x <= 1))
  check_weights <- function(result, packet) {
    weights <- result$weights
    sums <- rowsum(weights$weight, weights$vertex, reorder = FALSE)
    expect_equal(as.double(sums), rep(1, length(result$vertices$x)))
    for (axis in c("x", "y", "z")) {
      original <- packet$vertices[[axis]][packet$corners$vertex[weights$corner]]
      reconstructed <- rowsum(original*weights$weight, weights$vertex, reorder = FALSE)
      expect_equal(as.double(reconstructed), result$vertices[[axis]])
    }
  }
  check_weights(result, packet)
  packet <- face_scene(x = c(0, 1, 1, 0))
  camera <- ggplot3d:::new_camera_3d(projection = "perspective", azimuth = 0,
    elevation = 0, distance = 0.6, near = 0.3, far = 2, clip = FALSE)
  result <- compile_scene(packet, camera)
  expect_true(result$fragments$clipped)
  expect_true(all(is.finite(result$vertices$x_npc)))
  expect_gte(min(result$vertices$depth), 0.3 - 1e-12)
  check_weights(result, packet)
})

test_that("geometry errors remain explicit and wholly clipped scenes are empty", {
  expect_error(compile_scene(face_scene(z = c(0, 0, 0, 0.5))), "planar")
  expect_error(compile_scene(face_scene(y = rep(0, 4))), "degenerate")
  expect_error(compile_scene(face_scene(x = c(0, 1, 0.2, 0),
                                      y = c(0, 0, 0.2, 1))), "convex")
  result <- compile_scene(face_scene(z = rep(2, 4)))
  expect_length(result$draw_order, 0L)
  packet <- face_scene(); packet$faces$opacity <- 0
  expect_length(compile_scene(packet)$draw_order, 0L)
  expect_length(compile_scene(face_scene(x = c(-1, 0, 0, -1)))$draw_order, 0L)
  expect_error(compile_scene(face_scene(x = c(0, 1, 1, 0, 0),
                                      y = c(0, 0, 1, 1, 0), z = rep(0.5, 5))),
               "implicit closure")
})

test_that("polygon footprint rings are validated on both sides of the boundary", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = 0.5, y = 0.5, z = 0.5), corners = list(vertex = 1L),
    markers = list(offset = 1L, footprint_id = 1L),
    footprints = list(kind = 2L, offset = 1L, count = 6L,
                      radius_x_mm = 0, radius_y_mm = 0, angle = 45),
    footprint_vertices = list(x_mm = c(0, 2, 0, 0.2, 0.4, 0.2),
                              y_mm = c(0, 0, 2, 0.2, 0.2, 0.4),
                              ring = c(1L, 1L, 1L, 2L, 2L, 2L)),
    styles = data.frame(style_id = 1L, shape = 24L))
  expect_length(compile_scene(packet)$draw_order, 1L)
  packet$footprint_vertices$ring <- c(1L, 1L, 2L, 2L, 2L, 2L)
  expect_error(ggplot3d:::validate_scene_packet(packet), "rings")
  expect_error(ggplot3d:::scene_compile_cpp(packet[names(packet) != "styles"],
    ggplot3d:::new_camera_3d(), ggplot3d:::new_scene_viewport(100, 100)), "rings")
})

test_that("depth precedes layer order and explicit partitions surround the scene", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = rep(c(0, 1, 0), 3), y = rep(c(0, 0, 1), 3),
                    z = rep(c(0.2, 0.5, 0.8), each = 3)),
    corners = list(vertex = 1:9),
    faces = list(offset = c(1L, 4L, 7L), layer_id = c(3L, 2L, 1L)),
    styles = data.frame(style_id = 1L))
  camera <- ggplot3d:::new_camera_3d(elevation = 90)
  expect_identical(compile_scene(packet, camera)$draw_order, 1:3)
  packet$faces$layer_id <- 1:3
  expect_identical(compile_scene(packet, camera)$draw_order, 1:3)
  packet$faces$partition <- c(2L, 1L, 0L)
  expect_identical(compile_scene(packet, camera)$draw_order, c(3L, 2L, 1L))
})

test_that("clipped paths preserve physical dash phase and original endpoints", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = c(-0.5, 0.5, 1.5), y = rep(0.5, 3), z = rep(0.5, 3)),
    corners = list(vertex = 1:3),
    polylines = list(offset = 1L, count = 3L, arrow_start = TRUE, arrow_end = TRUE),
    styles = data.frame(style_id = 1L))
  result <- compile_scene(packet, ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90))
  expect_length(result$draw_order, 2L)
  expect_true(all(result$fragments$clipped))
  expect_false(any(result$fragments$arrow_start | result$fragments$arrow_end))
  expect_gt(result$fragments$path_start_mm[1], 0)
  expect_equal(result$fragments$path_end_mm[1], result$fragments$path_start_mm[2])
  expect_equal(result$vertices$x, c(0, 0.5, 0.5, 1))
})
