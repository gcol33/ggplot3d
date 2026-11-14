face_corners_xyz <- function(packet, faces = seq_along(packet$faces$offset)) {
  rows <- unlist(lapply(faces, function(f) seq.int(packet$faces$offset[f], length.out = packet$faces$count[f])))
  scene_vertices_xyz_3d(packet$vertices)[packet$corners$vertex[rows], , drop = FALSE]
}

face_area_3d <- function(packet, faces) {
  sum(vapply(faces, function(f) {
    xyz <- face_corners_xyz(packet, f)
    a <- xyz[-1L, , drop = FALSE] - matrix(xyz[1L, ], nrow(xyz) - 1L, 3L, byrow = TRUE)
    if (nrow(a) < 2L) return(0)
    sqrt(sum(colSums(cross_rows_3d(a[-nrow(a), , drop = FALSE], a[-1L, , drop = FALSE]))^2)) / 2
  }, numeric(1)))
}

torus_plot <- function(...) {
  box <- data.frame(x = c(-1.6, 1.6), y = c(-1.6, 1.6), z = c(-0.6, 0.6))
  ggplot2::ggplot(box, ggplot2::aes(x, y, z = z)) +
    geom_isofunction_3d(fun = function(x, y, z) (sqrt(x^2 + y^2) - 1)^2 + z^2, levels = 0.25, n = 28L,
      fill = "grey70") +
    coord_3d(expand = FALSE, ...)
}

test_that("clip_plane_3d validates its plane and prints it", {
  plane <- clip_plane_3d(c(1, 0, 0), c(x = 5), cap = TRUE)
  expect_s3_class(plane, "ggplot3d_clip_plane")
  expect_equal(plane$normal, c(1, 0, 0))
  expect_equal(plane$point$x, 5)
  expect_null(plane$point$y)
  expect_output(print(plane), "capped")
  expect_output(print(clip_plane_3d(c(0, 0, 1))), "box centre")
  expect_error(clip_plane_3d(c(0, 0, 0), c(0, 0, 0)), "not all zero")
  expect_error(clip_plane_3d(c(1, 1, 0), c(x = 1)), "y coordinate")
  expect_error(clip_plane_3d(c(1, 0, 0), c(x = 1), cap = NA), "cap")
  expect_error(coord_3d(clip_planes = list(1)), "clip_plane_3d")
})

test_that("planes are set in data units through the scales", {
  d <- data.frame(x = c(1, 10, 100), y = c(0, 1, 2), z = c(0, 5, 10))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() + ggplot2::scale_x_log10() +
    coord_3d(expand = FALSE)
  params <- integration_packets(p)$params
  plane <- unit_plane_3d(clip_plane_3d(c(1, 0, 0), c(x = 10)), params)
  expect_equal(plane$normal, c(1, 0, 0))
  expect_equal(plane$offset, -0.5)
  tilted <- unit_plane_3d(clip_plane_3d(c(0, 1, -0.2), c(y = 1, z = 5)), params)
  expect_equal(tilted$normal, c(0, 2, -2) / sqrt(8))
  expect_equal(sum(tilted$normal * c(0.3, 0.5, 0.5)) + tilted$offset, 0)
})

test_that("coord clip planes drop points, cut lines and cut faces exactly", {
  d <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 2, 3), z = c(0, 1, 2, 3))
  points <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    coord_3d(clip_planes = list(clip_plane_3d(c(-1, 0, 0), c(x = 1.5)))))$packet
  expect_length(points$markers$offset, 2L)
  path <- data.frame(x = c(0, 3, 0, 3), y = c(0, 1, 2, 3), z = 0)
  lines <- integration_packets(ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) + geom_path_3d() +
    coord_3d(expand = FALSE, clip_planes = list(clip_plane_3d(c(-1, 0, 0), c(x = 1.5)))))$packet
  expect_length(lines$polylines$offset, 2L)
  ends <- face_corners_xyz(list(faces = lines$polylines, corners = lines$corners, vertices = lines$vertices))
  expect_true(all(ends[, 1L] <= 0.5 + 1e-12))
  expect_equal(sum(abs(ends[, 1L] - 0.5) < 1e-12), 3L)
  g <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
  g$z <- g$x^2 - g$y^2
  surface <- integration_packets(ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) + geom_surface_3d() +
    coord_3d(expand = FALSE, clip_planes = list(clip_plane_3d(c(1, 1, 0), c(0, 0, 0)))))$packet
  xyz <- face_corners_xyz(surface)
  expect_true(all(xyz[, 1L] + xyz[, 2L] >= 1 - 1e-9))
  whole <- integration_packets(ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) + geom_surface_3d() +
    coord_3d(expand = FALSE))$packet
  expect_equal(face_area_3d(surface, seq_along(surface$faces$offset)),
    face_area_3d(whole, seq_along(whole$faces$offset)) / 2, tolerance = 0.01)
})

test_that("a layer's clip_planes replace the coordinate system's", {
  d <- data.frame(x = c(0, 1, 2, 3), y = 0, z = 0)
  cut <- list(clip_plane_3d(c(-1, 0, 0), c(x = 1.5)))
  out <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_point_3d() + geom_point_3d(clip_planes = list(), colour = "red") +
    geom_point_3d(clip_planes = list(clip_plane_3d(c(1, 0, 0), c(x = 2.5))), colour = "blue") +
    coord_3d(clip_planes = cut))$packet
  expect_equal(as.vector(table(out$markers$layer_id)), c(2L, 4L, 1L))
})

test_that("capped planes close cut solids with faces on the plane", {
  s <- data.frame(x = 0, y = 0, z = 0)
  base <- ggplot2::ggplot(s, ggplot2::aes(x, y, z = z)) + geom_sphere_3d(radius = 1, detail = 3L)
  limits <- list(xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1), expand = FALSE)
  open <- integration_packets(base + do.call(coord_3d, c(limits, list(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 0)))))))$packet
  closed <- integration_packets(base + do.call(coord_3d, c(limits, list(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 0), cap = TRUE))))))$packet
  added <- setdiff(seq_along(closed$faces$offset), seq_along(open$faces$offset))
  expect_gt(length(added), 0L)
  caps <- face_corners_xyz(closed, added)
  expect_true(all(abs(caps[, 3L] - 0.5) < 1e-12))
  rim <- face_corners_xyz(open)
  rim <- rim[abs(rim[, 3L] - 0.5) < 1e-12, 1:2, drop = FALSE]
  angle <- atan2(rim[, 2L] - 0.5, rim[, 1L] - 0.5)
  rim <- unique(rim[order(angle), , drop = FALSE])
  polygon <- abs(sum(rim[, 1L] * rim[c(2:nrow(rim), 1L), 2L] - rim[c(2:nrow(rim), 1L), 1L] * rim[, 2L])) / 2
  expect_equal(face_area_3d(closed, added), polygon, tolerance = 1e-9)
  expect_true(all(closed$faces$style_id[added] %in% closed$faces$style_id[-added]))
})

test_that("caps leave holes where the cut outline is nested", {
  closed <- integration_packets(torus_plot(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 0), cap = TRUE))))$packet
  open <- integration_packets(torus_plot(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 0)))))$packet
  added <- setdiff(seq_along(closed$faces$offset), seq_along(open$faces$offset))
  caps <- face_corners_xyz(closed, added)
  centroid <- (caps[c(TRUE, FALSE, FALSE), ] + caps[c(FALSE, TRUE, FALSE), ] + caps[c(FALSE, FALSE, TRUE), ]) / 3
  expect_true(all(sqrt(rowSums((centroid[, 1:2] - 0.5)^2)) > 0.1))
  rim <- unique(face_corners_xyz(open))
  rim <- rim[abs(rim[, 3L] - 0.5) < 1e-12, 1:2, drop = FALSE] - 0.5
  ring_area <- function(xy) {
    xy <- xy[order(atan2(xy[, 2L], xy[, 1L])), , drop = FALSE]
    abs(sum(xy[, 1L] * xy[c(2:nrow(xy), 1L), 2L] - xy[c(2:nrow(xy), 1L), 1L] * xy[, 2L])) / 2
  }
  inner <- sqrt(rowSums(rim^2)) < 0.25
  expect_equal(face_area_3d(closed, added), ring_area(rim[!inner, ]) - ring_area(rim[inner, ]), tolerance = 1e-9)
})

test_that("later planes cut the caps of earlier ones", {
  s <- data.frame(x = 0, y = 0, z = 0)
  planes <- list(clip_plane_3d(c(0, 0, -1), c(z = 0), cap = TRUE), clip_plane_3d(c(-1, 0, 0), c(x = 0), cap = TRUE))
  packet <- integration_packets(ggplot2::ggplot(s, ggplot2::aes(x, y, z = z)) + geom_sphere_3d(radius = 1) +
    coord_3d(xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1), expand = FALSE, clip_planes = planes))$packet
  xyz <- face_corners_xyz(packet)
  expect_true(all(xyz[, 1L] <= 0.5 + 1e-12 & xyz[, 3L] <= 0.5 + 1e-12))
  on_x <- vapply(seq_along(packet$faces$offset), function(f) all(abs(face_corners_xyz(packet, f)[, 1L] - 0.5) < 1e-12), logical(1))
  on_z <- vapply(seq_along(packet$faces$offset), function(f) all(abs(face_corners_xyz(packet, f)[, 3L] - 0.5) < 1e-12), logical(1))
  expect_true(any(on_x) && any(on_z))
})

test_that("clip planes apply in scale space before curved frames", {
  d <- data.frame(x = c(0, 1, 2, 3), y = c(1, 1, 1, 1), z = c(0, 1, 2, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    coord_cylindrical_3d(clip_planes = list(clip_plane_3d(c(0, 0, 1), c(z = 1.5))))
  expect_length(integration_packets(p)$packet$markers$offset, 2L)
  spheres <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_sphere_3d(radius = 0.2) +
    coord_cylindrical_3d(clip_planes = list(clip_plane_3d(c(0, 0, 1), c(z = 1.5))))
  packet <- integration_packets(spheres)$packet
  expect_setequal(unique(packet$quadrics$source_row), 3:4)
})
