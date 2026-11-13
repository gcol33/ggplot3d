furniture_test_device <- function(code) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = 6, height = 6, units = "in", res = 100)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  force(code)
}

furniture_test_plot <- function(data = data.frame(x = c(-2, 1, 2), y = c(1, -2, 2), z = c(-1, 2, 1)), ...) {
  ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d(...) +
    ggplot2::scale_x_continuous(breaks = c(-1, 0, 1), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = c(-1, 0, 1), minor_breaks = NULL) +
    scale_z_continuous(breaks = c(-1, 0, 1), minor_breaks = NULL)
}

furniture_test_build <- function(plot, theme = ggplot2::theme()) {
  params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
  panel_furniture_3d(params, ggplot2::complete_theme(ggplot2::theme_get() + theme), new_scene_viewport(150, 150))
}

furniture_test_lines <- function(furniture, pane) {
  sum(vapply(furniture$tags, function(tag) identical(tag$type, "pane") && identical(tag$panes, pane), logical(1)) &
    seq_along(furniture$tags) %in% furniture$packet$polylines$primitive_id)
}

furniture_test_rows <- function(packet, kind, i) {
  tab <- packet[[kind]]
  ids <- seq.int(tab$offset[i], length.out = tab$count[i])
  cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)[packet$corners$vertex[ids], , drop = FALSE]
}

test_that("per-plane grid elements inherit from the axis grid and remove lines from one plane only", {
  theme <- ggplot2::complete_theme()
  for (level in c("major", "minor")) for (axis in c("x", "y", "z")) for (plane in c("xy", "xz", "yz")) {
    expect_equal(ggplot2::calc_element(paste0("panel.grid.", level, ".", axis, ".", plane), theme),
      ggplot2::calc_element(paste0("panel.grid.", level, ".", axis), theme))
  }
  furniture_test_device({
    plot <- furniture_test_plot(panes = c("xmin", "ymin", "zmin"))
    full <- furniture_test_build(plot)
    cut <- furniture_test_build(plot, ggplot2::theme(panel.grid.major.z.xz = ggplot2::element_blank()))
    expect_equal(furniture_test_lines(full, "ymin"), 6L)
    expect_equal(furniture_test_lines(cut, "ymin"), 3L)
    expect_equal(furniture_test_lines(cut, "xmin"), furniture_test_lines(full, "xmin"))
    expect_equal(furniture_test_lines(cut, "zmin"), furniture_test_lines(full, "zmin"))
    floor <- furniture_test_build(plot, ggplot2::theme(panel.grid.major.x.xy = ggplot2::element_blank()))
    expect_equal(furniture_test_lines(floor, "zmin"), 3L)
    expect_equal(furniture_test_lines(floor, "ymin"), 6L)
  })
})

test_that("front panes cover the facing panes left undrawn, over the data", {
  furniture_test_device({
    fill <- "#4682B4"
    plot <- furniture_test_plot(panes = c("back", "floor", "front"))
    furniture <- furniture_test_build(plot, ggplot2::theme(panel.pane.front = ggplot2::element_rect(fill = fill)))
    faces <- furniture$packet$faces
    front <- faces$partition == 2L
    expect_equal(sum(front), 3L)
    expect_equal(sum(faces$partition == 0L), 3L)
    expect_true(all(furniture$packet$styles$fill[faces$style_id[front]] == fill))
    expect_true(all(vapply(furniture$tags[faces$primitive_id[front]], is.null, logical(1))))
    loose <- furniture_test_build(furniture_test_plot(panes = c("back", "floor", "front"), clip_data = FALSE))
    expect_equal(loose$packet$faces$partition, rep(1L, 6L))
    default <- ggplot2::calc_element("panel.pane.front", ggplot2::complete_theme())
    expect_lt(grDevices::col2rgb(default$fill, alpha = TRUE)[4L, 1L], 255)
    cylinder <- ggplot2::ggplot(data.frame(x = 1:4, y = 1:4, z = 1:4), ggplot2::aes(x, y, z = z)) +
      geom_point_3d() + coord_cylindrical_3d(panes = c("back", "front"))
    built <- furniture_test_build(cylinder)
    expect_true(any(built$packet$faces$partition == 2L))
    expect_error(coord_3d(panes = "sides"), "Unknown cube pane")
  })
})

test_that("zero planes and zero lines sit at transformed value zero when it is in range", {
  furniture_test_device({
    plot <- furniture_test_plot(panes = "floor")
    params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
    zero <- range_position_3d(params, "z", 0)
    plain <- furniture_test_build(plot)
    expect_false(any(plain$packet$faces$layer_id == scene_furniture_layer_3d))
    furniture <- furniture_test_build(plot, ggplot2::theme(panel.zero.z = ggplot2::element_rect(colour = "red")))
    faces <- furniture$packet$faces
    plane <- which(faces$layer_id == scene_furniture_layer_3d)
    expect_length(plane, 1L)
    expect_equal(faces$partition[plane], 1L)
    expect_equal(unique(furniture_test_rows(furniture$packet, "faces", plane)[, 3L]), zero)
    expect_true(is.na(furniture$packet$styles$colour[faces$style_id[plane]]))
    ring <- which(furniture$packet$polylines$layer_id == scene_furniture_layer_3d)
    expect_length(ring, 1L)
    expect_equal(furniture$packet$styles$colour[furniture$packet$polylines$style_id[ring]], "#FF0000")
    expect_equal(nrow(furniture_test_rows(furniture$packet, "polylines", ring)), 5L)
    expect_false(is.null(furniture$split))
    expect_equal(furniture$split$scene$faces$layer_id, scene_furniture_layer_3d)
    expect_no_warning(away <- furniture_test_build(furniture_test_plot(data.frame(x = 1:3, y = 1:3, z = 4:6), panes = "floor"),
      ggplot2::theme(panel.zero.z = ggplot2::element_rect())))
    expect_false(any(away$packet$faces$layer_id == scene_furniture_layer_3d))
    lines <- furniture_test_build(plot, ggplot2::theme(panel.zero.line = ggplot2::element_line(colour = "red")))
    expect_equal(furniture_test_lines(lines, "zmin") - furniture_test_lines(plain, "zmin"), 2L)
    red <- which(lines$packet$styles$colour[lines$packet$polylines$style_id] == "#FF0000")
    expect_length(red, 2L)
    on_zero <- vapply(red, function(i) {
      xyz <- furniture_test_rows(lines$packet, "polylines", i)
      isTRUE(all.equal(xyz[, 1L], rep(range_position_3d(params, "x", 0), 2L))) ||
        isTRUE(all.equal(xyz[, 2L], rep(range_position_3d(params, "y", 0), 2L)))
    }, logical(1))
    expect_true(all(on_zero))
  })
})

test_that("the origin is given in data units and moved into the box", {
  expect_equal(check_origin_3d(c(z = 2)), c(x = 0, y = 0, z = 2))
  expect_error(coord_3d(origin = c(w = 1)), "origin must be")
  expect_error(coord_3d(origin = c(1, 2, 3)), "origin must be")
  expect_error(coord_3d(axis_lines = "centre"), "should be one of")
  logged <- ggplot2::ggplot(data.frame(x = c(1, 10, 100), y = 1:3, z = 1:3), ggplot2::aes(x, y, z = z)) +
    geom_point_3d() + ggplot2::scale_x_log10() + coord_3d(origin = c(x = 10, y = 50))
  params <- ggplot2::ggplot_build(logged)$layout$panel_params[[1L]]
  origin <- origin_position_3d(params)
  expect_equal(origin[["x"]], range_position_3d(params, "x", 1))
  expect_equal(origin[["y"]], 1)
  expect_equal(origin[["z"]], 0)
})

test_that("origin axes are scene primitives with ticks, labels and titles", {
  furniture_test_device({
    plot <- furniture_test_plot(axis_lines = "origin", panes = "floor")
    furniture <- furniture_test_build(plot)
    packet <- furniture$packet
    params <- furniture$overlay$params
    origin <- origin_position_3d(params)
    scene_lines <- which(packet$polylines$layer_id == scene_furniture_layer_3d)
    expect_true(all(packet$polylines$partition[scene_lines] == 1L))
    # Three axis lines and three ticks per axis.
    expect_length(scene_lines, 3L + 9L)
    spans <- lapply(scene_lines, function(i) furniture_test_rows(packet, "polylines", i))
    for (k in 1:3) {
      line <- Filter(function(xyz) isTRUE(all.equal(sort(xyz[, k]), c(0, 1))), spans)
      expect_length(line, 1L)
      expect_equal(unname(line[[1L]][1L, -k]), unname(origin[-k]))
    }
    text <- which(packet$text$layer_id == scene_furniture_layer_3d)
    labels <- packet$styles$label[packet$text$style_id[text]]
    # The label at the crossing point is left out on every axis.
    expect_setequal(labels, c("-1", "1", "x", "y", "z"))
    expect_equal(sum(labels == "-1"), 3L)
    expect_true(all(packet$text$partition[text] == 1L))
    for (i in seq_along(text)) {
      xyz <- furniture_test_rows(packet, "text", text[i])
      expect_true(all(xyz >= -1e-9 & xyz <= 1 + 1e-9))
    }
    overlay <- furniture_overlay_3d(furniture, NULL, ggplot2::complete_theme())
    expect_length(overlay$bounds, 0L)
    expect_false(guide_on_box_3d(params$axis_guides$x, params))
  })
})

test_that("guide position origin moves one axis and axis_lines none drops box axis lines", {
  furniture_test_device({
    plot <- ggplot2::ggplot(data.frame(x = c(-2, 1, 2), y = c(1, -2, 2), z = c(-1, 2, 1)), ggplot2::aes(x, y, z = z)) +
      geom_point_3d() + coord_3d(panes = "floor") +
      scale_z_continuous(breaks = c(-1, 0, 1), minor_breaks = NULL, guide = guide_axis_3d(position = "origin"))
    furniture <- furniture_test_build(plot)
    params <- furniture$overlay$params
    expect_true(guide_at_origin_3d(params$axis_guides$z, params))
    expect_true(guide_on_box_3d(params$axis_guides$x, params))
    text <- which(furniture$packet$text$layer_id == scene_furniture_layer_3d)
    expect_setequal(furniture$packet$styles$label[furniture$packet$text$style_id[text]], c("-1", "0", "1", "z"))
    expect_error(guide_axis_3d(position = "centre"), "Invalid 3D axis position")
    lined <- ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"))
    count_segments <- function(axis_lines) {
      built <- furniture_test_build(furniture_test_plot(axis_lines = axis_lines) + lined, lined)
      overlay <- furniture_overlay_3d(built, NULL, ggplot2::complete_theme(ggplot2::theme_get() + lined))
      sum(vapply(overlay$children, inherits, logical(1), "segments"))
    }
    expect_equal(count_segments("box") - count_segments("none"), 3L)
  })
})

test_that("origin axes and zero planes draw in every backend and need a cube frame", {
  furniture_test_device({
    for (type in c("vector", "raster", "hybrid")) {
      plot <- furniture_test_plot(axis_lines = "origin", backend = backend_3d(type)) +
        ggplot2::theme(panel.zero.z = ggplot2::element_rect())
      expect_no_error(grid::grid.force(ggplot2::ggplotGrob(plot)))
    }
    ternary <- ggplot2::ggplot(data.frame(a = c(1, 2, 3), b = c(2, 1, 1), c = c(1, 1, 2), z = 1:3),
      ggplot2::aes(a = a, b = b, c = c, z = z)) + geom_point_3d() + coord_ternary_3d(axis_lines = "origin")
    expect_error(grid::grid.force(ggplot2::ggplotGrob(ternary)), "Origin axes need a frame")
    expect_null(frame_tetrahedron_3d()$section(3L, 0.5))
    expect_null(frame_ternary_3d()$section(1L, 0.5))
    expect_equal(unique(frame_ternary_3d()$section(3L, 0.5)$xyz[, 3L]), 0.5)
  })
})
