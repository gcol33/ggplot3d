shadow_fragments_3d <- function(scene) {
  f <- scene$fragments
  styles <- scene$styles[match(f$style_id, scene$styles$style_id), , drop = FALSE]
  which(f$kind == 1L & f$layer_id == 0L & ggplot3d:::style_value_3d(styles, "alpha_mode", "face") == "surface")
}

face_points_3d <- function(packet) {
  faces <- packet$faces
  corner <- rep(faces$offset, faces$count) + sequence(faces$count) - 1L
  do.call(cbind, packet$vertices)[packet$corners$vertex[corner], , drop = FALSE]
}

unit_limits_3d <- function() list(ggplot2::lims(x = c(0, 1), y = c(0, 1)), zlim(0, 1))

test_that("shadow_3d() and depth_cue_3d() validate their settings", {
  expect_s3_class(shadow_3d(), "ggplot3d_shadow")
  expect_error(shadow_3d(on = "ceiling"), "on must name panes")
  expect_error(shadow_3d(alpha = 0), "alpha")
  expect_error(shadow_3d(colour = "nocolour"), "colour must be one colour")
  expect_error(shadow_3d(light = c(0, 0, 1)), "light_3d")
  cue <- depth_cue_3d(linewidth = TRUE, edl = TRUE)
  expect_identical(c(cue$linewidth, cue$edl), c(0.5, 1))
  expect_true(is.na(depth_cue_3d(fog = NA)$fog))
  expect_error(depth_cue_3d(near = 1, far = 0), "near < far")
  expect_error(depth_cue_3d(amount = 2), "amount")
  expect_error(depth_cue_3d(edl = -1), "edl")
  expect_error(coord_3d(shadow = list()), "shadow_3d")
  expect_error(coord_3d(depth_cue = TRUE), "depth_cue_3d")
  expect_output(print(shadow_3d()), "floor")
  expect_output(print(depth_cue_3d()), "panel background")
})

test_that("a face casts its shadow along the light onto the floor and is cut to the pane", {
  square <- data.frame(x = c(.6, .9, .9, .6), y = c(.2, .2, .5, .5), z = .5)
  p <- ggplot2::ggplot(square, ggplot2::aes(x, y, z = z)) + geom_polygon_3d(fill = "red") +
    unit_limits_3d() + coord_3d(expand = FALSE)
  s <- integration_packets(p)
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 72)
  spec <- shadow_3d(light = light_3d(c(1, 0, 1), space = "data"))
  shadow <- ggplot3d:::shadow_packet_3d(list(s$packet), spec, s$params, s$params$camera, viewport)
  xyz <- face_points_3d(shadow)
  expect_equal(range(xyz[, 1L]), c(.1, .4), tolerance = 1e-9)
  expect_equal(range(xyz[, 2L]), c(.2, .5), tolerance = 1e-9)
  expect_true(all(xyz[, 3L] == 0))
  expect_identical(shadow$styles$alpha_mode, "surface")
  expect_identical(unique(shadow$faces$opacity), 0.3)
  expect_identical(unique(shadow$faces$layer_id), 0L)
  far <- shadow_3d(light = light_3d(c(1.5, 0, 1), space = "data"))
  cut <- ggplot3d:::shadow_packet_3d(list(s$packet), far, s$params, s$params$camera, viewport)
  xyz <- face_points_3d(cut)
  expect_equal(range(xyz[, 1L]), c(0, .15), tolerance = 1e-9)
  below <- shadow_3d(light = light_3d(c(0, 0, -1), space = "data"))
  expect_null(ggplot3d:::shadow_packet_3d(list(s$packet), below, s$params, s$params$camera, viewport))
})

test_that("points cast discs of their drawn radius and strokes ribbons of their width", {
  d <- data.frame(x = .5, y = .5, z = .7)
  overhead <- light_3d(c(0, 0, 1), space = "data")
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 6) + unit_limits_3d() +
    coord_3d(azimuth = -90, elevation = 90, expand = FALSE, shadow = shadow_3d(light = overhead), panes = "floor")
  drawn <- drawn_panel_3d(p)
  scene <- drawn$scene
  rows <- shadow_fragments_3d(scene)
  expect_gt(length(rows), 0L)
  f <- scene$fragments
  i <- unlist(lapply(rows, function(r) f$offset[r] + seq_len(f$count[r]) - 1L))
  width_mm <- diff(range(scene$vertices$x_npc[i])) * drawn$inner_viewport$width_mm
  marker <- which(f$kind == 3L)
  radius_mm <- integration_packets(p)$packet$footprints$radius_x_mm
  expect_equal(width_mm, 2 * radius_mm, tolerance = 0.02)
  expect_equal(mean(range(scene$vertices$x_npc[i])), scene$vertices$x_npc[f$offset[marker]], tolerance = 1e-6)
  line <- data.frame(x = c(.2, .8), y = .5, z = .6)
  p <- ggplot2::ggplot(line, ggplot2::aes(x, y, z = z)) + geom_path_3d(linewidth = 2) + unit_limits_3d() +
    coord_3d(azimuth = -90, elevation = 90, expand = FALSE, shadow = shadow_3d(light = overhead), panes = "floor")
  drawn <- drawn_panel_3d(p)
  scene <- drawn$scene; f <- scene$fragments
  rows <- shadow_fragments_3d(scene)
  i <- unlist(lapply(rows, function(r) f$offset[r] + seq_len(f$count[r]) - 1L))
  stroke <- which(f$kind == 2L & f$layer_id > 0L)
  expect_equal(diff(range(scene$vertices$y_npc[i])) * drawn$inner_viewport$height_mm,
    integration_packets(p)$packet$polylines$width_mm, tolerance = 0.02)
})

test_that("a shadow film lies between the pane fill and the grid lines and is one compound path per layer", {
  d <- data.frame(x = c(.3, .6, .5), y = c(.3, .4, .7), z = c(.5, .6, .8))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 4) +
    geom_path_3d(colour = "blue") + unit_limits_3d() + coord_3d(shadow = shadow_3d(alpha = 0.5))
  drawn <- drawn_panel_3d(p)
  scene <- drawn$scene; f <- scene$fragments
  rows <- shadow_fragments_3d(scene)
  position <- match(rows, scene$draw_order)
  panes <- which(f$kind == 1L & f$layer_id == 0L & f$partition == 0L & !(seq_along(f$kind) %in% rows))
  grid_lines <- which(f$kind == 2L & f$layer_id == 0L & f$partition == 0L)
  expect_gt(min(position), max(match(panes, scene$draw_order)))
  expect_lt(max(position), min(match(grid_lines, scene$draw_order)))
  styles <- scene$styles[match(f$style_id[rows], scene$styles$style_id), , drop = FALSE]
  expect_identical(length(unique(styles$sheet)), 2L)
  vector <- drawn$children[[which(vapply(drawn$children, function(g) identical(g$name, "scene-vector-3d"), logical(1)))]]
  paths <- Filter(function(g) inherits(g, "pathgrob"), unlist(lapply(vector$children, function(g)
    if (inherits(g, "gTree")) as.list(g$children) else list(g)), recursive = FALSE))
  films <- Filter(function(g) identical(g$gp$alpha, 0.5) && identical(g$rule, "winding"), paths)
  expect_length(films, 2L)
})

test_that("raster and hybrid panels draw the shadow film with the furniture", {
  d <- data.frame(x = c(.3, .6), y = c(.3, .6), z = c(.6, .8))
  for (type in c("raster", "hybrid")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 5, raster = TRUE) + unit_limits_3d() +
      coord_3d(backend = backend_3d(type, dpi = 40), shadow = shadow_3d())
    expect_gt(length(shadow_fragments_3d(drawn_panel_3d(p)$scene)), 0L)
  }
})

test_that("fog moves colours toward the fog colour in linear light by camera depth", {
  expect_identical(ggplot3d:::fog_colours_3d(c("#FF0000", NA), c(0, 1), "white"), c("#FF0000FF", NA))
  expect_identical(ggplot3d:::fog_colours_3d("#000000", 1, "#336699"), "#336699FF")
  half <- ggplot3d:::fog_colours_3d("#000000", 0.5, "white")
  level <- ggplot3d:::srgb_encode_3d(0.5)
  expect_identical(half, grDevices::rgb(level, level, level, 1))
  expect_identical(ggplot3d:::fog_colours_3d("#00000080", 0, "white"), "#00000080")
  cue <- depth_cue_3d(near = 0.2, far = 0.6)
  expect_equal(ggplot3d:::depth_cue_weight_3d(c(0, 0.2, 0.4, 0.6, 1), cue), c(0, 0, 0.5, 1, 1))
  d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(colour = "black") + unit_limits_3d()
  plain <- drawn_panel_3d(base + coord_3d(expand = FALSE))$scene
  fogged <- drawn_panel_3d(base + coord_3d(expand = FALSE, depth_cue = depth_cue_3d(fog = "white", amount = 1)))$scene
  colour_of <- function(scene) {
    f <- scene$fragments; rows <- which(f$kind == 3L)
    stats::setNames(scene$styles$colour[match(f$style_id[rows], scene$styles$style_id)], scene$source$markers$source_row[f$source_index[rows]])
  }
  fog <- colour_of(fogged)
  grey <- colSums(grDevices::col2rgb(fog[order(names(fog))]))
  expect_gt(grey[1L], grey[2L])
  pane_fill <- function(scene) {
    f <- scene$fragments
    scene$styles$fill[match(f$style_id[f$layer_id == 0L & f$kind == 1L], scene$styles$style_id)]
  }
  expect_identical(pane_fill(plain), pane_fill(fogged))
})

test_that("depth-thinned strokes narrow along a path away from the camera", {
  d <- data.frame(x = seq(0, 1, length.out = 11), y = 0.5, z = seq(0, 1, length.out = 11))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d(linewidth = 2) + unit_limits_3d() +
    coord_3d(expand = FALSE, depth_cue = depth_cue_3d(fog = NA, linewidth = 0.8))
  s <- integration_packets(p)
  camera <- s$params$camera
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 72)
  params <- s$params
  params$coord_params$depth_cue <- depth_cue_3d(fog = NA, linewidth = 0.8)
  cued <- ggplot3d:::depth_cue_packets_3d(list(s$packet), params, camera, viewport, ggplot2::theme_grey())[[1L]]
  expect_length(cued$polylines$offset, 10L)
  depth <- ggplot3d:::box_depth_fraction_3d(camera, viewport, ggplot3d:::FrameBox)(
    ggplot3d:::primitive_centres_3d(cued)[[2L]])
  width <- cued$polylines$width_mm
  expect_equal(width, s$packet$polylines$width_mm * (1 - 0.8 * pmin(1, pmax(0, depth))), tolerance = 1e-12)
  expect_gt(diff(range(width)), 0)
})
