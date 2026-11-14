rendered_panels_3d <- function(plot) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(plot)))
}

test_that("rectangles and tiles emit one quad face and one closed outline per row", {
  d <- data.frame(xmin = c(0, 2), xmax = c(1, 3), ymin = 0, ymax = 1)
  p <- ggplot2::ggplot(d, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    ggplot2::geom_rect(colour = "black") + coord_3d()
  packet <- integration_packets(p)$packet
  expect_equal(packet$faces$count, c(4L, 4L))
  expect_equal(packet$polylines$count, c(5L, 5L))
  expect_length(unique(packet$vertices$z), 1L)
  tiles <- ggplot2::ggplot(expand.grid(x = 1:3, y = 1:2), ggplot2::aes(x, y, fill = x + 3 * y, z = y)) +
    ggplot2::geom_tile() + coord_3d()
  packet <- integration_packets(tiles)$packet
  expect_equal(packet$faces$count, rep(4L, 6L))
  expect_length(packet$polylines$count, 0L)
  expect_length(unique(packet$styles$fill), 6L)
})

test_that("reference lines span the box at their intercepts", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_hline(yintercept = c(1.5, 2.5)) + coord_3d()
  out <- integration_packets(p)
  expect_equal(out$packet$polylines$count, c(2L, 2L))
  expect_setequal(out$packet$vertices$x, c(0, 1))
  v <- integration_packets(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_vline(xintercept = 2) + coord_3d())$packet
  expect_setequal(v$vertices$y, c(0, 1))
  expect_length(unique(v$vertices$x), 1L)
})

test_that("labels measure their padded box and draw it in both backends", {
  d <- data.frame(x = 1:2, y = 1:2, z = 1:2, label = c("a", "bb"))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label))
  text <- integration_packets(base + ggplot2::geom_text() + coord_3d())$packet
  label <- integration_packets(base + ggplot2::geom_label() + coord_3d())$packet
  expect_true(all(label$text$width_mm > text$text$width_mm))
  expect_true(all(label$text$height_mm > text$text$height_mm))
  expect_true(all(is.finite(label$styles$label_padding)))
  for (backend in c("vector", "raster")) {
    panel <- rendered_panels_3d(base + ggplot2::geom_label(fill = "white") +
      coord_3d(backend = backend_3d(backend)))[[1L]]
    boxes <- grid::getGrob(panel, "roundrect", grep = TRUE, global = TRUE)
    expect_length(boxes, 2L)
  }
})

test_that("annotation grobs are placed over the projected floor rectangle", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    ggplot2::annotation_custom(grid::rectGrob(name = "annotation-rect"), xmin = 1, xmax = 2, ymin = 1, ymax = 2) +
    coord_3d()
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1L]]$ranges$z, c(0.9, 3.1))
  panel <- rendered_panels_3d(p)[[1L]]
  placed <- Filter(function(g) inherits(g, "scene_annotation_grob"), panel$children)
  expect_length(placed, 1L)
  expect_s3_class(placed[[1L]]$vp, "viewport")
  expect_gt(grid::convertWidth(placed[[1L]]$vp$width, "mm", TRUE), 0)
})

test_that("annotate_3d adds z positions and checks lengths", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    annotate_3d("point", x = 2, y = 2, z = 10) +
    annotate_3d("segment", x = 1, y = 1, z = 0, xend = 1, yend = 1, zend = c(2, 3)) + coord_3d()
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[2L]]$z, 10)
  expect_equal(built$data[[3L]]$zend, c(2, 3))
  expect_true(all(c(0, 10) >= built$layout$panel_params[[1L]]$ranges$z[1L] &
                  c(0, 10) <= built$layout$panel_params[[1L]]$ranges$z[2L]))
  expect_error(annotate_3d("point", x = 1:2, y = 1:3, z = 1), "Unequal position lengths")
})

test_that("annotate_3d forwards registered bracket parameters to the scene", {
  span <- data.frame(x = 0, y = 0, z = 1, xend = 2, yend = 0, zend = 1)
  mapping <- ggplot2::aes(x, y, z = z, xend = xend, yend = yend, zend = zend)
  arrow <- grid::arrow(ends = "first", type = "open", length = grid::unit(4, "mm"))
  for (type in c("bracket", "dimension")) {
    params <- list(type = type, offset = c(0, 0.15, 0), tip_length = 0.04,
      arrow = arrow, label = "2 m", colour = "blue")
    direct <- do.call(geom_bracket_3d, c(list(mapping = mapping, data = span), params))
    expected <- integration_packets(ggplot2::ggplot() + direct + coord_3d())$packet
    for (geom in list("bracket", GeomBracket3D)) {
      annotation <- do.call(annotate_3d, c(list(geom = geom), as.list(span), params))
      expect_equal(nrow(annotation$data), 1L)
      expect_false(annotation$inherit.aes)
      expect_false(annotation$show.legend)
      packet <- integration_packets(ggplot2::ggplot() + annotation + coord_3d())$packet
      expect_equal(packet, expected)
      if (type == "dimension") {
        expect_equal(packet$polylines$count, c(2L, 2L, 2L))
        expect_equal(packet$polylines$arrow_start, c(FALSE, FALSE, TRUE))
        expect_equal(packet$polylines$arrow_end, c(FALSE, FALSE, FALSE))
      }
    }
  }
})

test_that("annotate_3d forwards common and other geom parameters", {
  segment <- annotate_3d("segment", x = 0, y = 0, z = 1,
    xend = 2, yend = 1, zend = 2, lineend = "round", occlusion = "overlay")
  expect_identical(segment$geom_params$lineend, "round")
  expect_identical(segment$geom_params$occlusion, "overlay")
  padding <- grid::unit(c(0.2, 0.3, 0.4, 0.5), "lines")
  label <- annotate_3d("label", x = 1:2, y = 0, z = 1, label = c("a", "b"),
    label.padding = padding, border.colour = "red")
  expect_identical(label$geom_params$label.padding, padding)
  expect_identical(label$geom_params$border.colour, "red")
  expect_equal(nrow(label$data), 2L)
  point <- annotate_3d("point", x = 1, y = 1, z = 1, colour = c("red", "blue"))
  expect_equal(nrow(point$data), 2L)
  expect_equal(point$data$z, c(1, 1))
  expect_error(annotate_3d("point", x = 1:2, colour = letters[1:3]), "Unequal parameter lengths")
  expect_error(annotate_3d("point", x = 1, misspelled = TRUE), "Unknown 3D layer arguments")
  expect_error(annotate_3d("missing"), "No 3D emitter registered")
})

test_that("labs(axis = NULL) removes a 3D axis title", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d()
  titles <- function(plot) ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]$axis_titles
  expect_null((titles(p + ggplot2::labs(y = NULL)))$y)
  expect_null((titles(p + ggplot2::labs(z = NULL)))$z)
  expect_identical(titles(p + ggplot2::labs(y = "height"))$y, "height")
  expect_identical(titles(p)$x, "x")
})

test_that("dodge rows stack directly outside one another and the title follows them", {
  metrics <- list(label = cbind(c(10, 20, 10, 20), 3), tick_length = 1, gap = 1)
  major <- cbind(seq(10, 40, 10), 0)
  at <- axis_label_positions_3d(major, c(0, -1), metrics, 2L)
  expect_equal(at[, 2L], c(-3.5, -7, -3.5, -7))
  rows <- axis_label_rows_3d(metrics$label, c(0, -1), metrics, 2L)
  expect_equal(rows$band, 6.5)
  metrics$title_size <- matrix(c(5, 4), 1L); metrics$title_gap <- 1
  title <- axis_title_geometry_3d(c(0, 0), c(50, 0), c(0, -1), metrics, rows$band)
  expect_equal(title$location[2L], -(1 + 1 + 6.5 + 1 + 2))
})

test_that("discrete axes keep every tick label unless check.overlap is set", {
  d <- data.frame(x = factor(paste("a long category name", 1:9)), y = 1:9, z = 1:9)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d()
  layout <- rendered_panels_3d(p)[[1L]]$furniture$label_layout
  expect_true(all(layout$x$keep))
  thinned <- rendered_panels_3d(p + ggplot2::guides(x = guide_axis_3d(check.overlap = TRUE)))[[1L]]$furniture$label_layout
  expect_lt(sum(thinned$x$keep), 9L)
})

test_that("stat_density_2d_3d draws on the floor or a plane, filled by density", {
  floor <- integration_packets(ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
    stat_density_2d_3d(n = 8, plane = "floor") + coord_3d())$packet
  expect_true(all(floor$vertices$z == 0))
  expect_gt(length(unique(floor$styles$fill)), 1L)
  expect_false(any(floor$styles$shade))
  out <- integration_packets(ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting)) +
    stat_density_2d_3d(n = 8, plane = 0.01) + coord_3d())
  z <- (0.01 - out$params$ranges$z[1L]) / diff(out$params$ranges$z)
  expect_equal(unique(out$packet$vertices$z), z)
  expect_error(stat_density_2d_3d(plane = "wall"), "should be one of")
})

test_that("camera arguments take one value per panel", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  views <- rbind(transform(d, view = "a"), transform(d, view = "b"))
  p <- ggplot2::ggplot(views, ggplot2::aes(x, y, z = z)) + geom_point_3d() + ggplot2::facet_wrap(~view)
  params <- ggplot2::ggplot_build(p + coord_3d(azimuth = c(-60, 30), elevation = c(30, 10)))$layout$panel_params
  expect_equal(vapply(params, function(q) q$camera$azimuth, numeric(1)), c(-60, 30))
  expect_equal(vapply(params, function(q) q$camera$elevation, numeric(1)), c(30, 10))
  expect_error(ggplot2::ggplot_build(p + coord_3d(azimuth = c(-60, 0, 60))), "3 camera views but the plot has 2 panels")
  expect_error(coord_3d(azimuth = c(1, 2), elevation = c(1, 2, 3)), "one value or the same number")
  panels <- rendered_panels_3d(p + coord_3d(azimuth = c(-60, 30)))
  expect_equal(panels[[1L]]$inner_viewport, panels[[2L]]$inner_viewport)
  base <- coord_3d()$camera
  moved <- motion_panel_camera_3d(base, params[[2L]]$camera, base)
  expect_equal(moved$azimuth, 30)
})
