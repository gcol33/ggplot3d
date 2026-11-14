project_points <- function(n = 12) {
  set.seed(42)
  data.frame(x = stats::runif(n, 1, 5), y = stats::runif(n, -2, 2), z = stats::runif(n, 10, 20))
}

project_plot <- function(layer, coord = coord_3d(), data = project_points()) {
  ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + geom_point_3d() + layer + coord
}

projected_layer <- function(plot, layer = 2L) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  grob <- built$plot@layers[[layer]]$draw_geom(built$data[[layer]], built$layout)[[1L]]
  list(packet = grob$packet, params = built$layout$panel_params[[1L]], built = built)
}

test_that("floor shadows keep x and y, sit on the floor and are painted with it", {
  d <- project_points()
  out <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("floor")), data = d))
  v <- out$packet$vertices
  expect_true(all(v$z == 0))
  expected_x <- (d$x - out$params$ranges$x[1L]) / diff(out$params$ranges$x)
  expect_equal(sort(v$x), sort(expected_x))
  expect_true(all(out$packet$markers$partition == 0L))
  plain <- projected_layer(project_plot(ggplot2::geom_blank(), data = d))
  expect_equal(out$params$ranges, plain$params$ranges)
})

test_that("back walls follow the camera like the back panes", {
  for (azimuth in c(-60, 120)) {
    out <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("back")),
      coord_3d(azimuth = azimuth)))
    back <- frame_of_3d(out$params)$back_panes(pane_partitions_3d(out$params))
    v <- scene_vertices_xyz_3d(out$packet$vertices)
    on_pane <- vapply(back, function(name) {
      axis <- match(substr(name, 1L, 1L), c("x", "y", "z")); value <- as.double(endsWith(name, "max"))
      sum(v[, axis] == value)
    }, numeric(1))
    expect_equal(unname(on_pane), c(12, 12))
    expect_true(all(out$packet$markers$partition == 0L))
  }
  first <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("back")), coord_3d(azimuth = -60)))
  flipped <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("back")), coord_3d(azimuth = 120)))
  expect_false(identical(frame_of_3d(first$params)$back_panes(pane_partitions_3d(first$params)),
                         frame_of_3d(flipped$params)$back_panes(pane_partitions_3d(flipped$params))))
})

test_that("several targets draw the layer once each", {
  out <- projected_layer(project_plot(geom_point_3d(position = position_project_3d(list("floor", "back", list(axis = "z", at = 15))))))
  expect_length(out$packet$markers$offset, 48L)
  expect_silent(validate_scene_packet(out$packet))
})

test_that("planes at data values are depth-sorted and use the scale transformation", {
  out <- projected_layer(project_plot(geom_point_3d(position = position_project_3d(list(axis = "y", at = 0)))))
  expect_equal(unique(out$packet$vertices$y), (0 - out$params$ranges$y[1L]) / diff(out$params$ranges$y))
  expect_true(all(out$packet$markers$partition == 1L))
  logged <- projected_layer(project_plot(geom_point_3d(position = position_project_3d(list(axis = "z", at = 12)))) +
    scale_z_log10())
  expect_equal(unique(logged$packet$vertices$z), (log10(12) - logged$params$ranges$z[1L]) / diff(logged$params$ranges$z))
  edge <- projected_layer(project_plot(geom_point_3d(position = position_project_3d(list(axis = "x", at = Inf)))))
  expect_true(all(edge$packet$vertices$x == 1))
})

test_that("panes facing the camera and unclipped scenes keep their partitions", {
  below <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("floor")), coord_3d(elevation = -30)))
  expect_true(all(below$packet$markers$partition == 2L))
  unclipped <- projected_layer(project_plot(geom_point_3d(position = position_project_3d("floor")), coord_3d(clip_data = FALSE)))
  expect_true(all(unclipped$packet$markers$partition == 1L))
  overlay <- projected_layer(project_plot(geom_point_3d(occlusion = "overlay", position = position_project_3d("floor"))))
  expect_true(all(overlay$packet$markers$partition == 2L))
})

test_that("projection arguments are validated", {
  expect_error(position_project_3d(list(axis = "w", at = 0)), "onto")
  expect_error(position_project_3d(character()), "onto")
  expect_error(position_project_3d("floor", axes = c("x", "x")), "two different axes")
  expect_error(projected_layer(project_plot(geom_point_3d(position = position_project_3d("ceiling")))), "Unknown pane ceiling")
  expect_error(geom_contour_3d(plane = "floor", position = position_project_3d("xmin")), "plane or position")
  expect_error(projected_layer(project_plot(ggplot2::annotation_custom(grid::nullGrob()) |>
    (\(l) { l$position <- position_project_3d("floor"); l })())), "scene geometry")
})

test_that("contours project onto a side wall through the same mechanism", {
  d <- expand.grid(x = 0:4, y = 0:4); d$z <- d$x + d$y
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_contour_3d(breaks = c(2, 5), position = position_project_3d("xmin")) + coord_3d()
  out <- projected_layer(p, 1L)
  expect_true(all(out$packet$vertices$x == 0))
  expect_gt(length(out$packet$polylines$offset), 0L)
  floor <- projected_layer(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_contour_filled_3d(breaks = c(1, 3, 5), plane = "floor") + coord_3d(), 1L)
  expect_true(all(floor$packet$faces$partition == 0L))
})

test_that("flattened faces drop repeated corners and collapsed faces", {
  d <- data.frame(x = c(0, 1, 2), y = 0, zmin = 0, zmax = c(1, 2, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, zmin = zmin, zmax = zmax)) +
    geom_band_3d(position = position_project_3d("floor")) +
    geom_band_3d(position = position_project_3d("ymax")) + coord_3d()
  expect_length(projected_layer(p, 1L)$packet$faces$offset, 0L)
  expect_gt(length(projected_layer(p, 2L)$packet$faces$offset), 0L)
  packet <- new_scene_packet(vertices = list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = rep(0, 4)),
    corners = list(vertex = c(1L, 2L, 2L, 3L, 4L, 1L, 1L, 2L), source_row = rep(1L, 8L)),
    faces = list(offset = c(1L, 6L), count = c(5L, 3L)), styles = data.frame(style_id = 1L))
  cleaned <- clean_flat_faces_3d(packet)
  expect_equal(cleaned$faces$count, 4L)
  expect_equal(cleaned$corners$vertex[cleaned$faces$offset + 0:3], 1:4)
  expect_silent(validate_scene_packet(cleaned))
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("two-dimensional layers stand on a wall as marginals", {
  d <- project_points(40)
  hist_layer <- ggplot2::geom_histogram(ggplot2::aes(x = x), inherit.aes = FALSE, bins = 6,
    position = position_project_3d("back", axes = c("x", "z")))
  p <- project_plot(hist_layer, data = d)
  out <- projected_layer(p)
  built <- out$built$data[[2L]]
  expect_true(all(c("zmin", "zmax") %in% names(built)))
  expect_false(any(c("y", "ymin", "ymax") %in% names(built)))
  expect_lte(out$params$ranges$z[1L], 0)
  v <- out$packet$vertices
  expect_length(unique(v$y), 1L)
  expect_true(unique(v$y) %in% c(0, 1))
  expect_true(all(out$packet$faces$partition == 0L))
  expect_equal(length(out$packet$faces$offset), sum(built$count > 0))
  density <- projected_layer(project_plot(ggplot2::geom_density(ggplot2::aes(x = y, y = ggplot2::after_stat(scaled)),
    inherit.aes = FALSE, fill = "grey60", position = position_project_3d("xmin", axes = c("y", "z"))), data = d))
  expect_true(all(density$packet$vertices$x == 0))
  expect_gt(length(density$packet$faces$offset), 10L)
  expect_error(position_project_3d("xmin", axes = c("x", "z")) |>
    (\(pos) projected_layer(project_plot(ggplot2::geom_histogram(ggplot2::aes(x = x), inherit.aes = FALSE, bins = 4, position = pos), data = d)))(),
    "plane of constant y")
  floor <- projected_layer(project_plot(ggplot2::geom_point(ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
    position = position_project_3d("floor", axes = c("x", "y"))), data = d))
  expect_true(all(floor$packet$vertices$z == 0))
})

test_that("projection works on the panes of curvilinear frames", {
  d <- project_points()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_point_3d(position = position_project_3d("floor")) + coord_cylindrical_3d()
  out <- projected_layer(p, 1L)
  expect_length(out$packet$markers$offset, 12L)
  expect_true(all(abs(out$packet$vertices$z - out$packet$vertices$z[1L]) < 1e-9))
})

test_that("ribbons and areas are flat bands inside coord_3d", {
  d <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 3), z = 1)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, ymin = 0, ymax = y, z = z)) + ggplot2::geom_ribbon(colour = "black") + coord_3d()
  out <- projected_layer(p, 1L)
  expect_length(out$packet$faces$offset, 8L)
  expect_length(out$packet$polylines$offset, 8L)
  expect_true(all(out$packet$vertices$z == out$packet$vertices$z[1L]))
  area <- projected_layer(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_area(colour = "black", position = "identity") + coord_3d(), 1L)
  expect_length(area$packet$polylines$offset, 4L)
  flipped <- projected_layer(ggplot2::ggplot(d, ggplot2::aes(xmin = 0, xmax = y, y = x, z = z)) +
    ggplot2::geom_ribbon(orientation = "y") + coord_3d(), 1L)
  expect_equal(range(flipped$packet$vertices$y), c(0, 1), tolerance = 0.1)
  expect_length(flipped$packet$faces$offset, 8L)
})

test_that("rug ticks mark values on the camera-side edge of their pane", {
  d <- project_points()
  out <- projected_layer(project_plot(geom_rug_3d(sides = "floor:x", length = 0.05), data = d))
  v <- out$packet$vertices; p <- out$packet$polylines
  expect_length(p$offset, 12L)
  expect_true(all(v$z == 0))
  start <- out$packet$corners$vertex[p$offset]; end <- out$packet$corners$vertex[p$offset + 1L]
  expect_equal(v$x[start], v$x[end])
  expect_equal(abs(v$y[end] - v$y[start]), rep(0.05, 12L))
  partitions <- pane_partitions_3d(out$params)
  front <- if (partitions[["ymin"]] == 2L) 0 else 1
  expect_true(all(v$y[start] == front))
  expect_true(all(p$partition == 0L))
  edge <- projected_layer(project_plot(geom_rug_3d(sides = "floor:x:ymax"), data = d))
  ends <- edge$packet$vertices$y[edge$packet$corners$vertex[edge$packet$polylines$offset]]
  expect_true(all(ends == 1))
  back <- projected_layer(project_plot(geom_rug_3d(sides = "back:z"), data = d))
  expect_length(back$packet$polylines$offset, 24L)
  none <- projected_layer(project_plot(geom_rug_3d(ggplot2::aes(x = x, y = y, z = NULL), sides = "back:z"), data = d))
  expect_length(none$packet$polylines$offset, 0L)
  expect_error(geom_rug_3d(sides = "floor"), "pane:axis")
  expect_error(geom_rug_3d(sides = "floor:x:zmax2"), "pane:axis")
  expect_error(geom_rug_3d(length = 0), "length")
  expect_error(projected_layer(project_plot(geom_rug_3d(sides = "floor:z"), data = d)), "holds constant")
  expect_error(projected_layer(project_plot(geom_rug_3d(sides = "floor:x:zmax"), data = d)), "not an edge")
})
