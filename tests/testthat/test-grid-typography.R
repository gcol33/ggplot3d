typography_panel <- function(plot, width = 6, height = 6) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = width, height = height, units = "in", res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(plot)))[[1L]]
}

typography_plot <- function(..., coord = coord_3d(), labels = ggplot2::waiver()) {
  d <- data.frame(x = c(0, 4), y = c(0, 4), z = c(0, 4))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    ggplot2::scale_x_continuous(breaks = 0:4, labels = labels, minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = 0:4, minor_breaks = NULL) +
    scale_z_continuous(breaks = 0:4, minor_breaks = NULL) +
    ggplot2::guides(...) + coord
}

# The typography of one guide, recomputed from a rendered panel.
typography_of <- function(panel, name) {
  furniture <- panel_furniture_3d(panel$panel_params, panel$theme, panel$outer_viewport)
  project <- furniture_projector_3d(furniture)
  frame <- furniture$overlay$frame
  ctx <- typography_context_3d(frame, furniture$camera, furniture$viewport, project)
  guide <- panel$panel_params$axis_guides[[name]]
  layout <- axis_layout_3d(frame, guide, project, project(frame$bounds()))
  list(typography = guide_typography_3d(ctx, guide, furniture$overlay$fit$metrics[[name]], layout), layout = layout, ctx = ctx)
}

overlay_grobs <- function(panel, class) Filter(function(g) inherits(g, class), as.list(panel$furniture$children))

test_that("guide_axis_3d validates and stores the typography options", {
  guide <- guide_axis_3d(label.orientation = "edge", plane = "xz", title.position = "near", depth.scale = 0.5, ticks = "in_plane")
  expect_identical(guide$params$label.orientation, "edge")
  expect_identical(guide$params$plane, "xz")
  expect_identical(guide$params$title.position, "near")
  expect_identical(guide$params$depth.scale, 0.5)
  expect_identical(guide$params$ticks, "in_plane")
  expect_identical(guide_axis_3d(depth.scale = TRUE)$params$depth.scale, 1)
  expect_identical(guide_axis_3d()$params$depth.scale, 0)
  expect_error(guide_axis_3d(depth.scale = -1), "depth.scale")
  expect_error(guide_axis_3d(label.orientation = "flat"))
  expect_error(guide_axis_3d(plane = "xx"))
  expect_identical(normalise_guide_3d(ggplot2::guide_axis(angle = 30), "x")$params$label.orientation, "screen")
})

test_that("edge labels turn with the projected edge and stay upright", {
  panel <- typography_panel(typography_plot(x = guide_axis_3d(label.orientation = "edge"),
    y = guide_axis_3d(label.orientation = "edge"), z = guide_axis_3d(label.orientation = "edge")))
  for (name in c("x", "y", "z")) {
    t <- typography_of(panel, name)
    tangent <- t$layout$tangent
    expected <- upright_angle_3d(tangent[1L], tangent[2L])
    expect_equal(t$typography$labels$angle, rep(expected, 5L))
    expect_true(all(t$typography$labels$angle > -90 & t$typography$labels$angle <= 90))
    expect_gt(abs(expected), 1)
  }
  rotations <- unlist(lapply(overlay_grobs(panel, "text"), `[[`, "rot"))
  expect_true(any(abs(rotations) > 1))
  screen <- typography_of(typography_panel(typography_plot()), "x")$typography$labels$angle
  expect_equal(screen, rep(0, 5L))
})

test_that("edge labels sit beyond the ticks by their height, not their bounding box", {
  panel <- typography_panel(typography_plot(x = guide_axis_3d(label.orientation = "edge")))
  t <- typography_of(panel, "x")
  m <- panel_furniture_3d(panel$panel_params, panel$theme, panel$outer_viewport)$overlay$fit$metrics$x
  normal <- t$layout$normal
  distance <- drop((t$typography$labels$location - t$typography$major$from) %*% normal)
  expect_equal(distance, m$tick_length + m$gap + m$label[, 2L] / 2, tolerance = 1e-8)
})

test_that("plane labels are glyph outlines in the pane, foreshortened by perspective", {
  plot <- typography_plot(x = guide_axis_3d(label.orientation = "plane"), labels = rep("W", 5L),
    coord = coord_3d(projection = "perspective", fov = 60))
  panel <- typography_panel(plot)
  furniture <- panel_furniture_3d(panel$panel_params, panel$theme, panel$outer_viewport)
  expect_gt(sum(furniture$packet$faces$layer_id == scene_furniture_layer_3d), 10L)
  expect_length(overlay_grobs(panel, "pathgrob"), 0L)
  t <- typography_of(panel, "x")
  expect_identical(t$typography$labels$kind, "path")
  sizes <- sqrt(rowSums(t$typography$labels$size^2))
  depth <- t$layout$at(0:4)[, 3L]
  # Nearer labels are drawn larger.
  expect_equal(order(sizes), order(-depth))
  expect_gt(max(sizes) / min(sizes), 1.05)
  expect_true(all(vapply(t$typography$labels$paths, function(p) length(p$x) > 4L, logical(1))))
})

test_that("plane labels take the requested pane and reject panes without the axis", {
  for (plane in c("xy", "xz")) {
    panel <- typography_panel(typography_plot(x = guide_axis_3d(label.orientation = "plane", plane = plane, ticks = "in_plane")))
    t <- typography_of(panel, "x")
    pane <- guide_pane_direction_3d(t$ctx, panel$panel_params$axis_guides$x, t$layout)
    expect_equal(which(pane != 0), if (plane == "xy") 2L else 3L)
    candidate <- t$layout$candidate
    expected <- t$ctx$project(rbind(candidate$start, candidate$start + 1e-3 * pane))
    direction <- t$typography$major$to[1L, ] - t$typography$major$from[1L, ]
    page <- expected[2L, 1:2] - expected[1L, 1:2]
    expect_equal(sum(direction * page) / sqrt(sum(direction^2) * sum(page^2)), 1, tolerance = 1e-4)
  }
  expect_error(typography_panel(typography_plot(z = guide_axis_3d(label.orientation = "plane", plane = "floor"))), "vertical")
  expect_error(typography_panel(typography_plot(x = guide_axis_3d(ticks = "in_plane", plane = "yz"))), "does not contain")
})

test_that("in-plane ticks shrink with distance and end where the labels start", {
  plot <- typography_plot(x = guide_axis_3d(ticks = "in_plane"), coord = coord_3d(projection = "perspective", fov = 60))
  t <- typography_of(typography_panel(plot), "x")
  lengths <- sqrt(rowSums((t$typography$major$to - t$typography$major$from)^2))
  depth <- t$layout$at(0:4)[, 3L]
  expect_equal(order(lengths), order(-depth))
  away <- rowSums((t$typography$labels$location - t$typography$major$to) * (t$typography$major$to - t$typography$major$from))
  expect_true(all(away > 0))
})

test_that("depth.scale grows near labels and ticks under perspective only", {
  near_far <- function(coord) {
    t <- typography_of(typography_panel(typography_plot(x = guide_axis_3d(depth.scale = TRUE), coord = coord)), "x")
    t$typography$labels$factor
  }
  factor <- near_far(coord_3d(projection = "perspective", fov = 60))
  expect_gt(max(factor), 1)
  expect_lt(min(factor), 1)
  expect_equal(near_far(coord_3d()), rep(1, 5L))
  panel <- typography_panel(typography_plot(x = guide_axis_3d(depth.scale = 2), coord = coord_3d(projection = "perspective", fov = 60)))
  sizes <- unlist(lapply(overlay_grobs(panel, "text"), function(g) g$gp$fontsize))
  expect_gt(length(unique(round(sizes, 6))), 2L)
})

test_that("title.position near puts the title past the end nearer the camera", {
  plot <- typography_plot(x = guide_axis_3d(title.position = "near"), coord = coord_3d(projection = "perspective"))
  t <- typography_of(typography_panel(plot), "x")
  ends <- t$layout$at(c(0, 1))
  near <- which.min(ends[, 3L])
  outward <- ends[near, 1:2] - ends[3L - near, 1:2]
  beyond <- sum((t$typography$title$location - ends[near, 1:2]) * outward)
  expect_gt(beyond, 0)
  centre <- typography_of(typography_panel(typography_plot(coord = coord_3d(projection = "perspective"))), "x")$typography$title$location
  expect_gt(sqrt(sum((centre - t$typography$title$location)^2)), 5)
})

test_that("box typography keeps labels and titles inside the panel in every mode", {
  for (guide in list(guide_axis_3d(label.orientation = "edge", title.position = "near"),
                     guide_axis_3d(label.orientation = "plane", ticks = "in_plane"),
                     guide_axis_3d(depth.scale = TRUE, ticks = "in_plane"))) {
    panel <- typography_panel(typography_plot(x = guide, y = guide, z = guide, coord = coord_3d(projection = "perspective", roll = 15)))
    for (bounds in panel$furniture$bounds) {
      expect_true(all(is.finite(bounds)))
      expect_gte(min(bounds[1:2]), -1e-6)
      expect_lte(bounds[3], panel$outer_viewport$width_mm + 1e-6)
      expect_lte(bounds[4], panel$outer_viewport$height_mm + 1e-6)
    }
  }
})

test_that("origin axes draw plane labels as scene faces and in-plane ticks in model units", {
  plot <- typography_plot(x = guide_axis_3d(label.orientation = "plane", ticks = "in_plane"),
    y = guide_axis_3d(label.orientation = "edge"), coord = coord_3d(axis_lines = "origin", origin = c(x = 2, y = 2, z = 2)))
  params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = 6, height = 6, units = "in", res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  furniture <- panel_furniture_3d(params, ggplot2::complete_theme(ggplot2::theme_get()), new_scene_viewport(150, 150))
  packet <- furniture$packet
  faces <- which(packet$faces$layer_id == scene_furniture_layer_3d)
  expect_gt(length(faces), 10L)
  expect_true(all(packet$faces$partition[faces] == 1L))
  text <- which(packet$text$layer_id == scene_furniture_layer_3d)
  labels <- packet$styles$label[packet$text$style_id[text]]
  expect_false("x" %in% labels)
  expect_gte(sum(abs(packet$text$angle[text]) > 1), 3L)
  k <- model_units_per_mm_3d(furniture$camera, furniture$viewport)
  lines <- which(packet$polylines$layer_id == scene_furniture_layer_3d)
  xyz <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)
  lengths <- vapply(lines, function(i) {
    ids <- packet$corners$vertex[packet$polylines$offset[i] + 0:1]
    sqrt(sum(((xyz[ids[2L], ] - xyz[ids[1L], ]) * params$camera$aspect / max(params$camera$aspect))^2))
  }, numeric(1))
  m <- furniture$overlay$fit$metrics$x
  expect_true(any(abs(lengths - m$tick_length * k) < 1e-6))
})
