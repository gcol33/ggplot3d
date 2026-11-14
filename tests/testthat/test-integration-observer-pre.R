observer_scene <- function(coord) {
  d <- data.frame(x = c(-0.6, 0.5, 0.1), y = c(0.1, 0.3, -0.5), z = c(0.2, -0.1, 0.3))
  integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_polygon_3d(fill = "steelblue", colour = "black") + coord)
}

test_that("Klein observers and centres are carried by the camera pre-transform", {
  base <- observer_scene(coord_hyperbolic_3d(inside = TRUE, azimuth = 180, elevation = 0))
  moved <- observer_scene(coord_hyperbolic_3d(inside = TRUE, observer = c(0.3, -0.1, 0.05), azimuth = 180, elevation = 0))
  expect_identical(as.double(base$params$camera$pre), as.double(diag(4)))
  expect_false(isTRUE(all.equal(as.double(moved$params$camera$pre), as.double(diag(4)))))
  expect_null(moved$params$space_context$isometry)
  expect_identical(scene_vertex_matrix_3d(moved$packet$vertices), scene_vertex_matrix_3d(base$packet$vertices))
  action <- moved$params$space_context$action
  expect_equal(t(action) %*% diag(c(1, 1, 1, -1)) %*% action, diag(c(1, 1, 1, -1)), tolerance = 1e-12)
  pre <- matrix(moved$params$camera$pre, 4L, 4L)
  applied <- base$packet
  xyzw <- scene_vertex_matrix_3d(applied$vertices) %*% t(pre)
  applied$vertices <- scene_vertices_3d(xyzw)
  viewport <- new_scene_viewport(100, 100, 144)
  direct <- scene_compile(moved$packet, moved$params$camera, viewport)
  camera <- moved$params$camera; camera$pre <- as.double(diag(4))
  reference <- scene_compile(applied, camera, viewport)
  expect_identical(direct$draw_order, reference$draw_order)
  expect_equal(direct$vertices$x_npc, reference$vertices$x_npc, tolerance = 1e-10)
  expect_equal(direct$vertices$y_npc, reference$vertices$y_npc, tolerance = 1e-10)
  X <- SpaceHyperbolicFinite$to_canonical(data.frame(x = 0.3, y = -0.1, z = 0.05), "klein")
  origin <- c(0.5, 0.5, 0.5, 1)
  observer <- c((X[1:3] / X[4] + 1) / 2, 1)
  image <- drop(pre %*% observer)
  expect_equal(image[1:3] / image[4], origin[1:3], tolerance = 1e-12)
  expect_null(attr(moved$packet, "draw_recipe"))
})

test_that("observer-dependent normals and depth cues use the moved observer", {
  observer <- c(0.4, 0.1, -0.2)
  d <- data.frame(x = observer[1] + c(0.1, 0.25), y = observer[2], z = observer[3])
  coords <- list(pre = coord_hyperbolic_3d(inside = TRUE, observer = observer, fog = 0.1))
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(colour = "white") + coords$pre)$packet
  colour <- packet$styles$colour[match(packet$markers$style_id, packet$styles$style_id)]
  X <- SpaceHyperbolicFinite$to_canonical(d, "klein")
  O <- SpaceHyperbolicFinite$to_canonical(data.frame(x = observer[1], y = observer[2], z = observer[3]), "klein")
  r <- hyperbolic_distance_3d(X, O[rep(1, 2), , drop = FALSE])
  expected <- pmin(1, 0.1 / sinh(r)^2)
  expect_equal(as.numeric(grDevices::col2rgb(colour)[1, ]), round(255 * srgb_encode_3d(expected)))
})

test_that("isometry animation in the Klein model changes only the camera pre-transform", {
  d <- data.frame(x = c(-0.5, 0.4, 0), y = c(0, 0.2, -0.4), z = c(0.1, 0, 0.3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_polygon_3d(fill = "tomato") +
    coord_hyperbolic_3d(fog = 0.02)
  built <- integration_packets(p)
  packets <- collect_scene_layers(built$grobs, built$params, ggplot2::theme_grey())$packets
  expect_false(is.null(attr(packets[[1]], "space_unstyled")))
  panel <- list(packets = packets, panel_params = built$params)
  boost <- lorentz_boost_3d(1.2, c(1, 0, 0))
  moved <- motion_isometry_panel_3d(panel, boost)
  expect_identical(scene_vertex_matrix_3d(moved$packets[[1]]$vertices), scene_vertex_matrix_3d(packets[[1]]$vertices))
  expect_equal(moved$panel_params$space_context$action, boost)
  expect_equal(matrix(moved$panel_params$camera$pre, 4L, 4L), unname(space_action_pre_3d(moved$panel_params$space_context)))
  expect_false(identical(moved$packets[[1]]$styles$fill, packets[[1]]$styles$fill))
  reference <- integration_packets(p + coord_hyperbolic_3d(fog = 0.02, isometry = boost))
  expect_equal(moved$packets[[1]]$styles$fill, reference$packet$styles$fill)
  a <- animate_3d(p, isometry = list(diag(4), boost), frames = 2, width = 120, height = 120, progress = FALSE, workers = 1)
  on.exit(unlink(a$frames), add = TRUE)
  expect_equal(a$frame_count, 2L)
  expect_false(identical(readBin(a$frames[1], "raw", n = file.info(a$frames[1])$size),
                         readBin(a$frames[2], "raw", n = file.info(a$frames[2])$size)))
})

test_that("the Poincare model keeps isometries in emission", {
  scene <- observer_scene(coord_hyperbolic_3d(model = "poincare", centre = c(0.2, 0, 0)))
  expect_identical(as.double(scene$params$camera$pre), as.double(diag(4)))
  expect_false(is.null(scene$params$space_context$isometry))
})

spherical_squares <- function(w) {
  a <- 0.25
  P <- rbind(c(1, -a, -a), c(1, a, -a), c(1, a, a), c(1, -a, a))
  do.call(rbind, lapply(seq_along(w), function(i) {
    Q <- P / sqrt(rowSums(P^2)) * sqrt(1 - w[i]^2)
    data.frame(x = Q[, 1], y = Q[, 2], z = Q[, 3], w = w[i], id = i)
  }))
}

spherical_observer_scene <- function(...) {
  d <- spherical_squares(c(0.6, -0.6))
  integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, w = w, group = id)) +
    geom_polygon_3d(fill = "steelblue") +
    coord_spherical_geometry_3d(inside = TRUE, input = "ambient", azimuth = 180, elevation = 0, ...))
}

visible_squares <- function(scene) {
  compiled <- scene_compile(scene$packet, scene$params$camera, new_scene_viewport(100, 100, 144))
  faces <- compiled$fragments$kind == 1L
  sort(unique((compiled$fragments$source_row[faces] - 1L) %/% 4L + 1L))
}

test_that("S^3 observers turn the far hemisphere into view through the camera pre-transform", {
  base <- spherical_observer_scene()
  expect_true(any(base$packet$vertices$w < 0))
  expect_silent(validate_scene_packet(base$packet))
  expect_identical(visible_squares(base), 1L)
  turn <- rotate_4d("xw", 2 * asin(0.6))
  moved <- spherical_observer_scene(isometry = turn)
  expect_null(moved$params$space_context$isometry)
  expect_equal(moved$params$space_context$action, turn)
  expect_identical(scene_vertex_matrix_3d(moved$packet$vertices), scene_vertex_matrix_3d(base$packet$vertices))
  expect_identical(visible_squares(moved), 2L)
  pre <- matrix(moved$params$camera$pre, 4L, 4L)
  applied <- base$packet
  applied$vertices <- scene_vertices_3d(scene_vertex_matrix_3d(applied$vertices) %*% t(pre))
  viewport <- new_scene_viewport(100, 100, 144)
  direct <- scene_compile(moved$packet, moved$params$camera, viewport)
  camera <- moved$params$camera; camera$pre <- as.double(diag(4))
  reference <- scene_compile(applied, camera, viewport)
  expect_identical(direct$draw_order, reference$draw_order)
  expect_equal(direct$vertices$x_npc, reference$vertices$x_npc, tolerance = 1e-10)
  expect_equal(direct$vertices$y_npc, reference$vertices$y_npc, tolerance = 1e-10)
  angle <- 110 * pi / 180
  observer <- spherical_observer_scene(observer = c(sin(angle), 0, 0, cos(angle)))
  expect_identical(visible_squares(observer), 2L)
})

test_that("the identity observer leaves the S^3 scene unchanged", {
  base <- spherical_observer_scene()
  identity <- spherical_observer_scene(observer = c(0, 0, 0, 1))
  expect_identical(identity$packet$vertices, base$packet$vertices)
  viewport <- new_scene_viewport(100, 100, 144)
  expect_identical(scene_compile(identity$packet, identity$params$camera, viewport)[c("vertices", "fragments", "draw_order")],
                   scene_compile(base$packet, base$params$camera, viewport)[c("vertices", "fragments", "draw_order")])
})

test_that("a full turn of a Clifford translation returns the S^3 scene", {
  d <- spherical_squares(c(0.6, -0.6))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, w = w, group = id)) + geom_polygon_3d(fill = "tomato") +
    coord_spherical_geometry_3d(inside = TRUE, input = "ambient", azimuth = 180, elevation = 0, fog = 0.3)
  built <- integration_packets(p)
  packets <- collect_scene_layers(built$grobs, built$params, ggplot2::theme_grey())$packets
  expect_false(is.null(attr(packets[[1]], "space_unstyled")))
  panel <- list(packets = packets, panel_params = built$params)
  clifford <- function(angle) rotate_4d(left = c(sin(angle), 0, 0, cos(angle)), right = c(0, 0, 0, 1))
  expect_equal(clifford(pi / 2) %*% c(0, 0, 0, 1), matrix(c(1, 0, 0, 0)), tolerance = 1e-12)
  viewport <- new_scene_viewport(100, 100, 144)
  frame <- function(angle) {
    moved <- motion_isometry_panel_3d(panel, clifford(angle))
    expect_identical(scene_vertex_matrix_3d(moved$packets[[1]]$vertices), scene_vertex_matrix_3d(packets[[1]]$vertices))
    list(scene = scene_compile(moved$packets[[1]], moved$panel_params$camera, viewport), fill = moved$packets[[1]]$styles$fill)
  }
  start <- frame(0); half <- frame(pi / 2); full <- frame(2 * pi)
  expect_identical(start$scene$draw_order, full$scene$draw_order)
  expect_equal(full$scene$vertices$x_npc, start$scene$vertices$x_npc, tolerance = 1e-10)
  expect_equal(full$scene$vertices$y_npc, start$scene$vertices$y_npc, tolerance = 1e-10)
  expect_identical(full$fill, start$fill)
  expect_false(identical(half$scene$vertices$x_npc, start$scene$vertices$x_npc))
  outside <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, w = w, group = id)) + geom_polygon_3d(fill = "tomato") +
    coord_spherical_geometry_3d(model = "gnomonic", input = "ambient", fog = 0.3)
  a <- animate_3d(outside, isometry = list(clifford(0), clifford(pi / 4)), frames = 2, width = 120, height = 120, progress = FALSE, workers = 1)
  on.exit(unlink(a$frames), add = TRUE)
  expect_equal(a$frame_count, 2L)
})

test_that("gnomonic Hopf tubes are geodesic tubes about their fibres", {
  d <- data.frame(lon = c(0, 120), lat = 30)
  scene <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(lon = lon, lat = lat)) +
    stat_hopf_fibre_3d(n = 12, radius = 0.1, sides = 6) +
    coord_spherical_geometry_3d(inside = TRUE, isometry = rotate_4d("xw", 2)))
  H <- space_box_model_3d(scene_vertex_matrix_3d(scene$packet$vertices), scene$params$space_context)
  X <- H / sqrt(rowSums(H^2))
  distance <- sapply(d$lon, function(lon) {
    plane <- t(hopf_points_3d(lon, 30, c(0, pi / 2)))
    acos(pmin(1, sqrt(rowSums((X %*% plane)^2))))
  })
  expect_equal(apply(distance, 1L, min), rep(0.1, nrow(X)), tolerance = 1e-10)
  expect_true(any(scene$packet$vertices$w < 0))
  compiled <- scene_compile(scene$packet, scene$params$camera, new_scene_viewport(100, 100, 144))
  expect_gt(length(compiled$fragments$fragment_id), 0L)
})
