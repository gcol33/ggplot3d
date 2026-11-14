test_that("coverage guarantees control occluders independently of opacity", {
  face <- texture_square_packet(texture_3d(matrix(c("red", "transparent"), 1L)))
  face$faces$coverage_opaque <- FALSE
  line <- new_scene_packet(
    vertices = list(x = c(.2, .8), y = c(.5, .5), z = c(.2, .2)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, width_mm = 1, hidden = 1L),
    styles = data.frame(style_id = 1L, colour = "blue"))
  packet <- merge_scene_packets(list(face, line), 1L)
  expect_identical(packet$faces$coverage_opaque, FALSE)
  viewport <- new_scene_viewport(40, 40)
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  through <- scene_compile(packet, camera, viewport)
  expect_true(any(through$fragments$kind == 2L))
  expect_false(any(through$fragments$hidden))
  expect_true(all(through$fragments$opacity == 1))
  packet$faces$coverage_opaque <- TRUE
  solid <- scene_compile(packet, camera, viewport)
  expect_true(any(solid$fragments$hidden[solid$fragments$kind == 2L]))
  opaque <- texture_square_packet(texture_3d(matrix("blue")))
  merged <- merge_scene_packets(list(face, opaque), 1L)
  expect_identical(merged$faces$coverage_opaque, c(FALSE, TRUE))
  face$faces$coverage_opaque <- NA
  expect_error(validate_scene_packet(face), "coverage_opaque")
  expect_error(ggplot3d:::scene_compile_cpp(scene_engine_packet_3d(face), camera, viewport), "coverage_opaque")
})

test_that("a face with uncertain coverage opens a closed solid", {
  vertices <- as.matrix(expand.grid(x = c(.2, .8), y = c(.2, .8), z = c(.2, .8)))
  rings <- list(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5),
                c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  packet <- new_scene_packet(vertices = lapply(as.data.frame(vertices), as.double),
    corners = list(vertex = unlist(rings)),
    faces = list(offset = seq(1L, 24L, 4L), count = rep(4L, 6L)),
    styles = data.frame(style_id = 1L))
  camera <- new_camera_3d()
  viewport <- new_scene_viewport(40, 40)
  solid <- scene_compile(packet, camera, viewport)
  packet$faces$coverage_opaque <- c(FALSE, rep(TRUE, 5L))
  opened <- scene_compile(packet, camera, viewport)
  expect_lt(length(unique(solid$fragments$source_index)), 6L)
  expect_length(unique(opened$fragments$source_index), 6L)
  expect_true(all(opened$fragments$opacity == 1))
})
