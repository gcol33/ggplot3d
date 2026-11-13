test_that("ggplot2 filled contours draw terraced bands with mapped fills", {
  d <- expand.grid(x = 1:10, y = 1:10)
  d$z <- d$x * d$y
  for (layer in list(ggplot2::geom_contour_filled(bins = 4),
                     ggplot2::stat_contour_filled(bins = 4))) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + layer + coord_3d()
    expect_no_warning(scene <- stats_scene(p))
    bands <- scene$built$data[[1L]]
    packet <- scene$packet
    expect_true(is.ordered(bands$level))
    expect_equal(bands$z, bands$level_mid)
    expect_equal(length(unique(bands$z)), 4L)
    expect_gt(length(packet$faces$primitive_id), 0L)
    expect_silent(validate_scene_packet(packet))
    rows <- packet$faces$source_row
    expect_equal(grDevices::col2rgb(packet$styles$fill[packet$faces$style_id], alpha = TRUE),
                 grDevices::col2rgb(bands$fill[rows], alpha = TRUE))
    params <- scene$built$layout$panel_params[[1L]]
    normal <- normalise_positions_3d(bands, params)
    expect_equal(packet$vertices$z[packet$corners$vertex], normal$z[packet$corners$source_row])
    for (i in seq_along(packet$faces$offset)) {
      corners <- seq.int(packet$faces$offset[i], length.out = packet$faces$count[i])
      expect_length(unique(packet$vertices$z[packet$corners$vertex[corners]]), 1L)
    }
    expect_no_error(ggplot2::ggplotGrob(p))
  }
})

test_that("ggplot2 filled contour triangulation preserves band holes", {
  d <- expand.grid(x = seq(-2, 2, length.out = 21), y = seq(-2, 2, length.out = 21))
  d$z <- d$x^2 + d$y^2
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_contour_filled(breaks = c(0.5, 1, 2), colour = "black") +
    coord_3d(expand = FALSE)
  expect_no_warning(scene <- stats_scene(p))
  bands <- scene$built$data[[1L]]
  packet <- scene$packet
  expect_true(all(vapply(split(bands$subgroup, bands$group), function(x) length(unique(x)) == 2L, logical(1))))
  params <- scene$built$layout$panel_params[[1L]]
  normal <- normalise_positions_3d(bands, params)
  for (group in unique(bands$group)) {
    rows <- which(bands$group == group)
    rings <- split(rows, bands$subgroup[rows])
    ring_areas <- vapply(rings, function(r) abs(ring_area_3d(as.matrix(normal[c("x", "y")]), r)), numeric(1))
    expected <- max(ring_areas) - min(ring_areas)
    faces <- which(packet$faces$source_row %in% rows)
    area <- sum(vapply(faces, function(i) {
      ids <- packet$corners$vertex[seq.int(packet$faces$offset[i], length.out = packet$faces$count[i])]
      xy <- cbind(packet$vertices$x[ids], packet$vertices$y[ids])
      abs(cross2_3d(xy[2, ] - xy[1, ], xy[3, ] - xy[1, ])) / 2
    }, numeric(1)))
    expect_gt(expected, 0)
    expect_equal(area, expected)
  }
  expect_length(packet$polylines$primitive_id, 4L)
  expect_no_error(ggplot2::ggplotGrob(p))
})
