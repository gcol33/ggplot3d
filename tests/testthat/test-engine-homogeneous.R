homogeneous_viewport <- function() new_scene_viewport(100, 100, 144)

homogeneous_scene <- function(scale = 1) {
  quad <- cbind(c(0.1, 0.9, 0.9, 0.1), c(0.1, 0.1, 0.9, 0.9), c(0.3, 0.4, 0.6, 0.5))
  tilted <- cbind(c(0.2, 0.8, 0.5), c(0.2, 0.3, 0.9), c(0.8, 0.2, 0.5))
  path <- cbind(seq(0.05, 0.95, length.out = 7), 0.45, seq(0.1, 0.9, length.out = 7))
  point <- c(0.6, 0.7, 0.8)
  xyz <- rbind(quad, tilted, path, point)
  w <- rep_len(scale, nrow(xyz))
  new_scene_packet(
    vertices = list(x = xyz[, 1L] * w, y = xyz[, 2L] * w, z = xyz[, 3L] * w, w = w),
    corners = list(vertex = seq_len(nrow(xyz))),
    faces = list(offset = c(1L, 5L), count = c(4L, 3L), opacity = c(1, 0.6)),
    polylines = list(offset = 8L, count = 7L, width_mm = 1.5),
    markers = list(offset = 15L, footprint_id = 1L),
    footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 2, radius_y_mm = 2, angle = 0),
    styles = data.frame(style_id = 1L))
}

compiled_geometry <- function(result) {
  result[c("vertices", "fragments", "masks", "draw_order")]
}

test_that("packets default to w = 1 and validate homogeneous vertices", {
  packet <- new_scene_packet(vertices = list(x = 0.5, y = 0.5, z = 0.5))
  expect_identical(packet$schema_version, 2L)
  expect_identical(names(packet$vertices), c("x", "y", "z", "w"))
  expect_identical(packet$vertices$w, 1)
  expect_silent(new_scene_packet(vertices = list(x = 0.5, y = 0.5, z = 0.5, w = -1)))
  expect_error(new_scene_packet(vertices = list(x = 0, y = 0, z = 0, w = 0)), "nonzero direction")
  expect_error(new_scene_packet(vertices = list(x = 0.5, y = 0.5, z = 0.5, w = NaN)), "finite")
  broken <- packet; broken$vertices[c("x", "y", "z", "w")] <- list(0, 0, 0, 0)
  expect_error(scene_compile_cpp(broken[names(broken) != "styles"], new_camera_3d(),
                                 homogeneous_viewport()), "zero vector")
})

test_that("rows of either sign are distinct points of oriented projective space", {
  packet <- homogeneous_scene()
  camera <- new_camera_3d()
  reference <- scene_compile(packet, camera, homogeneous_viewport())
  antipodal <- packet
  antipodal$vertices <- lapply(packet$vertices, `-`)
  expect_silent(validate_scene_packet(antipodal))
  hidden <- scene_compile(antipodal, camera, homogeneous_viewport())
  expect_length(hidden$fragments$fragment_id, 0L)
  camera$pre <- -as.double(diag(4))
  restored <- scene_compile(antipodal, camera, homogeneous_viewport())
  expect_identical(compiled_geometry(restored), compiled_geometry(reference))
})

test_that("normalise_homogeneous_3d keeps directions at infinity", {
  ranges <- list(x = c(0, 10), y = c(-1, 1), z = c(0, 2))
  out <- normalise_homogeneous_3d(c(5, 10, 1), c(0, 2, 0), c(1, 4, 0), w = c(1, 2, 0), ranges = ranges)
  expect_equal(out$x, c(0.5, 1, 0.1))
  expect_equal(out$y, c(0.5, 2, 0))
  expect_equal(out$z, c(0.5, 2, 0))
  expect_equal(out$x[1:2] / out$w[1:2], c(0.5, 0.5))
  expect_identical(out$w, c(1, 2, 0))
  finite <- normalise_homogeneous_3d(c(2, 7), c(0, 1), c(1, 2), ranges = ranges)
  expect_identical(finite$x, (c(2, 7) - 0) / 10)
  far <- normalise_homogeneous_3d(-5, 0, -1, w = -1, ranges = ranges)
  expect_equal(unlist(far), c(x = -0.5, y = -0.5, z = -0.5, w = -1))
})

test_that("a face with an ideal vertex is clipped to the box before division", {
  packet <- new_scene_packet(
    vertices = list(x = c(0.2, 0.2, 1), y = c(0.2, 0.8, 0), z = c(0.5, 0.5, 0), w = c(1, 1, 0)),
    corners = list(vertex = 1:3), faces = list(offset = 1L),
    styles = data.frame(style_id = 1L))
  result <- scene_compile(packet, new_camera_3d(), homogeneous_viewport())
  v <- result$vertices
  expect_equal(range(v$x), c(0.2, 1))
  expect_true(all(abs(v$z - 0.5) < 1e-12))
  corners <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z, packet$vertices$w)
  w <- result$weights
  rebuilt <- rowsum(w$weight * corners[packet$corners$vertex[w$corner], , drop = FALSE], w$vertex, reorder = TRUE)
  expect_equal(unname(rebuilt), unname(cbind(v$x, v$y, v$z, 1)), tolerance = 1e-12)
  expect_equal(result$fragments$normal_z[1L]^2, 1, tolerance = 1e-12)
})

test_that("ideal vertices that survive every clip are unbounded geometry", {
  packet <- new_scene_packet(
    vertices = list(x = c(0.5, 0.6, 0), y = c(0.5, 0.5, 0), z = c(0.5, 0.5, 1), w = c(1, 1, 0)),
    corners = list(vertex = 1:3), faces = list(offset = 1L),
    styles = data.frame(style_id = 1L))
  camera <- new_camera_3d(azimuth = 0, elevation = 0, clip_data = FALSE, clip = FALSE)
  expect_error(scene_compile(packet, camera, homogeneous_viewport()), "unbounded geometry")
  expect_silent(scene_compile(packet, new_camera_3d(azimuth = 0, elevation = 0), homogeneous_viewport()))
})

test_that("compiled scenes are invariant under positive scaling of (x, y, z, w)", {
  camera <- new_camera_3d(projection = "perspective", azimuth = 35, elevation = 25)
  base <- scene_compile(homogeneous_scene(), camera, homogeneous_viewport())
  doubled <- scene_compile(homogeneous_scene(2), camera, homogeneous_viewport())
  expect_identical(compiled_geometry(doubled), compiled_geometry(base))
  expect_identical(doubled$weights$weight, base$weights$weight / 2)
  scaled <- scene_compile(homogeneous_scene(3.7), camera, homogeneous_viewport())
  expect_identical(scaled$draw_order, base$draw_order)
  expect_equal(compiled_geometry(scaled), compiled_geometry(base), tolerance = 1e-12)
})

test_that("all-w = 1 packets with identity pre give the same scene as a doubled pre", {
  camera <- new_camera_3d(azimuth = -50, elevation = 20)
  base <- scene_compile(homogeneous_scene(), camera, homogeneous_viewport())
  camera$pre <- as.double(diag(2, 4))
  doubled <- scene_compile(homogeneous_scene(), camera, homogeneous_viewport())
  expect_identical(compiled_geometry(doubled), compiled_geometry(base))
  expect_true(all(abs(rowsum(base$weights$weight, base$weights$vertex) - 1) <= 1e-12))
})

test_that("camera pre equals applying the map to the vertices", {
  centre <- rbind(cbind(diag(3), -0.5), c(0, 0, 0, 1))
  boost <- function(rapidity) {
    m <- diag(4); m[1, 1] <- m[4, 4] <- cosh(rapidity); m[1, 4] <- sinh(rapidity) / 2; m[4, 1] <- 2 * sinh(rapidity)
    solve(centre) %*% m %*% centre
  }
  projective <- diag(4); projective[4, 3] <- 0.4
  for (pre in list(boost(0.4), projective)) {
    camera <- new_camera_3d(projection = "perspective", azimuth = 20, elevation = 15, pre = pre)
    packet <- homogeneous_scene()
    mapped <- packet
    xyzw <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z, packet$vertices$w) %*% t(pre)
    mapped$vertices <- list(x = xyzw[, 1L], y = xyzw[, 2L], z = xyzw[, 3L], w = xyzw[, 4L])
    direct <- scene_compile(packet, camera, homogeneous_viewport())
    camera$pre <- as.double(diag(4))
    applied <- scene_compile(mapped, camera, homogeneous_viewport())
    expect_identical(direct$draw_order, applied$draw_order)
    expect_equal(compiled_geometry(direct), compiled_geometry(applied), tolerance = 1e-12)
  }
})

test_that("points a projective pre sends behind infinity are clipped", {
  pre <- diag(4); pre[4, 1] <- -2; pre[4, 4] <- 1.4
  camera <- new_camera_3d(pre = pre)
  result <- scene_compile(homogeneous_scene(), camera, homogeneous_viewport())
  expect_true(all(is.finite(result$vertices$x)))
  expect_true(all(result$vertices$x >= -1e-12 & result$vertices$x <= 1 + 1e-12))
  expect_gt(length(result$draw_order), 0L)
})

test_that("optional corner attributes are validated and carried through merges", {
  packet <- new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5), y = c(0.1, 0.1, 0.9), z = c(0.5, 0.5, 0.5)),
    corners = list(vertex = 1:3, red = c(1, 0, 0), alpha = c(1, 1, NA), u = c(0, 1, 0.5)),
    faces = list(offset = 1L), styles = data.frame(style_id = 1L, texture_id = 2L))
  expect_identical(names(packet$corners), c("vertex", "source_row", "red", "alpha", "u"))
  expect_error(new_scene_packet(vertices = list(x = 0, y = 0, z = 0),
                                corners = list(vertex = 1L, red = 2)), "\\[0, 1\\]")
  expect_error(new_scene_packet(vertices = list(x = 0, y = 0, z = 0),
                                corners = list(vertex = 1L, colour = 1)), "unknown columns")
  expect_error(new_scene_packet(styles = data.frame(style_id = 1L, texture_id = 0L)), "texture_id")
  misordered <- packet; misordered$corners <- packet$corners[c(1, 2, 5, 3, 4)]
  expect_error(validate_scene_packet(misordered), "invalid schema")
  expect_silent(scene_compile(packet, new_camera_3d(), homogeneous_viewport()))
  merged <- merge_scene_packets(list(packet, homogeneous_scene()), 1L)
  expect_identical(names(merged$corners), c("vertex", "source_row", "red", "alpha", "u"))
  expect_identical(merged$corners$red, c(1, 0, 0, rep(NA_real_, 15)))
})

test_that("clip planes interpolate corner attributes and keep w", {
  packet <- new_scene_packet(
    vertices = list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0.5, 0.5, 0.5, 0.5)),
    corners = list(vertex = 1:4, red = c(0, 1, 1, 0), normal_x = c(0, 1, 1, 0)), faces = list(offset = 1L, count = 4L),
    styles = data.frame(style_id = 1L))
  cut <- clip_packet_planes_3d(packet, list(list(normal = c(-1, 0, 0), offset = 0.25, cap = FALSE)))
  f <- cut$faces
  rows <- seq.int(f$offset, length.out = f$count)
  x <- cut$vertices$x[cut$corners$vertex[rows]]
  expect_equal(sort(unique(x)), c(0, 0.25))
  expect_equal(cut$corners$normal_x[rows], x)
  expect_equal(cut$corners$red[rows], srgb_encode_3d(x))
  expect_true(all(cut$vertices$w == 1))
})

test_that("quadrics are part of the contract and compile to quadric fragments", {
  packet <- new_scene_packet(
    quadrics = list(kind = c(1L, 3L), transform_1 = c(0.1, 0.1), transform_6 = c(0.1, 0.1),
                    transform_11 = c(0.1, 0.3), transform_13 = c(0.3, 0.6), transform_14 = c(0.5, 0.5),
                    transform_15 = c(0.5, 0.3), radius_ratio = c(1, 0)),
    styles = data.frame(style_id = 1L))
  expect_identical(packet$quadrics$primitive_id, 1:2)
  expect_identical(packet$quadrics$cap_0, c(1L, 1L))
  expect_identical(packet$quadrics$transform_16, c(1, 1))
  expect_error(new_scene_packet(quadrics = list(kind = 2L, radius_ratio = 2), styles = data.frame(style_id = 1L)),
    "radius ratio")
  expect_error(new_scene_packet(quadrics = list(kind = 1L, transform_4 = 1), styles = data.frame(style_id = 1L)),
    "affine")
  expect_error(new_scene_packet(quadrics = list(kind = 1L, transform_1 = 0), styles = data.frame(style_id = 1L)),
    "invertible")
  scene <- scene_compile(packet, new_camera_3d(), homogeneous_viewport())
  f <- scene$fragments
  expect_true(all(f$kind == 5L))
  expect_setequal(f$source_index, 1:2)
  expect_identical(f$primitive_id, f$source_index)
})

test_that("mask_width_mm defaults to width_mm and may not be narrower", {
  packet <- new_scene_packet(vertices = list(x = c(0, 1), y = c(0, 1), z = c(0, 1)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, width_mm = c(1.5)),
    styles = data.frame(style_id = 1L))
  expect_identical(packet$polylines$mask_width_mm, 1.5)
  expect_error(new_scene_packet(vertices = list(x = c(0, 1), y = c(0, 1), z = c(0, 1)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, width_mm = 2, mask_width_mm = 1),
    styles = data.frame(style_id = 1L)), "mask_width_mm")
})

test_that("a haloed line hides more of the line behind it", {
  halo_scene <- function(mask) {
    new_scene_packet(
      vertices = list(x = c(0.1, 0.9, 0.5, 0.5), y = c(0.5, 0.5, 0.1, 0.9), z = c(0.8, 0.8, 0.2, 0.2)),
      corners = list(vertex = 1:4),
      polylines = list(offset = c(1L, 3L), width_mm = c(1, 0), mask_width_mm = c(mask, 0)),
      styles = data.frame(style_id = 1L))
  }
  camera <- new_camera_3d(azimuth = 0, elevation = 90)
  visible_length <- function(mask) {
    result <- scene_compile(halo_scene(mask), camera, homogeneous_viewport())
    f <- result$fragments
    rows <- which(f$parent_primitive_id == 2L)
    sum(vapply(rows, function(i) {
      at <- f$offset[i] + c(0L, f$count[i] - 1L)
      sqrt(diff(result$vertices$x_npc[at] * 100)^2 + diff(result$vertices$y_npc[at] * 100)^2)
    }, numeric(1)))
  }
  plain <- visible_length(1)
  expect_equal(plain - visible_length(5), 5, tolerance = 1e-9)
  expect_equal(plain - visible_length(3), 3, tolerance = 1e-9)
})
