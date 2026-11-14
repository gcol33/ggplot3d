plotter_hatch_plot <- function() {
  d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
  d$z <- with(d, exp(-(x^2 + y^2)) + 0.5 * exp(-((x - 1)^2 + (y + 1)^2) * 2))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "white", colour = NA,
      hatch = hatch_3d(angle = 0, spacing = 0.02), outline = outline_3d(linewidth = 0.4)) +
    coord_3d(backend = backend_3d("plotter"), panes = character(), box = "none") +
    ggplot2::theme_minimal(17)
}

test_that("plotter strokes always use clipped centrelines, including wide oblique cuts", {
  packet <- new_scene_packet(vertices = list(x = c(0.1, 0.9), y = c(0.2, 0.8), z = c(0.5, 0.5)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, cap = 1L, width_mm = 4),
    styles = data.frame(style_id = 1L, colour = "red", linetype = "dashed"))
  scene <- scene_compile(packet, new_camera_3d(azimuth = -90, elevation = 90), new_scene_viewport(100, 100))
  scene$fragments$mask_offset[] <- 1L; scene$fragments$mask_count[] <- 4L
  scene$masks <- list(x_npc = c(0, 0.5, 0.5, 0), y_npc = c(0, 0, 1, 1), ring = rep(1L, 4L))
  g <- render_scene_grid(scene, scene$viewport, strokes_only = TRUE)
  expect_gt(length(g$children), 0L)
  expect_true(all(vapply(g$children, inherits, logical(1), "polyline")))
  expect_true(all(vapply(g$children, function(p) is.null(p$vp), logical(1))))
  expect_lte(max(vapply(g$children, function(p) max(as.numeric(p$x)), numeric(1))), 50)
})

test_that("plotter joins continued intervals and sorts by either endpoint", {
  path <- function(x, y = rep(0, length(x))) list(x = x, y = y)
  joined <- join_stroke_paths_3d(list(path(c(1, 2)), path(c(0, 1)), path(c(3, 4)), path(c(2, 3))))
  expect_length(joined, 1L)
  expect_equal(joined[[1L]]$x, 0:4)
  sorted <- sort_stroke_paths_3d(list(path(c(8, 6)), path(c(4, 1)), path(c(5, 6))))
  expect_equal(lapply(sorted, `[[`, "x"), list(c(1, 4), c(5, 6), c(6, 8)))
  keep <- compact_path_points_3d(c(0, 0, 0.001, 1, 1), rep(0, 5), rep(1L, 5), 0.01)
  expect_identical(keep, c(1L, 4L))
})

test_that("plotter closes arrowheads with unfilled strokes and draws labels last", {
  packet <- new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5), y = c(0.5, 0.5, 0.5), z = c(0.5, 0.5, 0.1)),
    corners = list(vertex = 1:3),
    polylines = list(offset = 1L, arrow_end = TRUE), text = list(offset = 3L, style_id = 2L),
    styles = data.frame(style_id = 1:2, colour = c("red", "black"), label = c("", "label"),
      arrow = I(list(grid::arrow(type = "closed"), NULL))))
  scene <- scene_compile(packet, new_camera_3d(azimuth = -90, elevation = 90), new_scene_viewport(100, 100))
  grob <- render_scene_grid(scene, scene$viewport, strokes_only = TRUE)
  expect_s3_class(tail(grob$children, 1L)[[1L]], "text")
  strokes <- Filter(function(g) inherits(g, "polyline"), as.list(grob$children))
  expect_length(strokes, 2L)
  expect_true(any(vapply(strokes, function(g) length(g$x) == 4L, logical(1))))
})

test_that("the lighting hatch SVG contains only compact stroked scene geometry", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("xml2")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  svglite::svglite(path, 6.5, 4.5, pointsize = 14, bg = "transparent")
  print(plotter_hatch_plot())
  grDevices::dev.off()
  xml <- xml2::read_xml(path)
  shapes <- xml2::xml_find_all(xml, "//*[local-name()='path' or local-name()='polygon' or local-name()='polyline']")
  expect_gt(length(shapes), 0L)
  expect_false(any(grepl("fill:\\s*(?!none)[^;]+", xml2::xml_attr(shapes, "style"), perl = TRUE), na.rm = TRUE))
  expect_length(xml2::xml_find_all(xml, "//*[local-name()='mask']"), 0L)
  expect_length(xml2::xml_find_all(xml, "//*[local-name()='path' and contains(@d, 'Z')]"), 0L)
  lines <- xml2::xml_find_all(xml, "//*[local-name()='polyline']")
  repeated <- vapply(xml2::xml_attr(lines, "points"), function(points) {
    xy <- matrix(as.numeric(strsplit(trimws(points), "[ ,]+")[[1L]]), ncol = 2L, byrow = TRUE)
    any(rowSums(diff(xy)^2) == 0)
  }, logical(1))
  expect_false(any(repeated))
  expect_lt(file.size(path), 50000)
})

test_that("plotter and grid visible hatch strokes agree at interior pixels", {
  skip_if_not_installed("ragg")
  packet <- stats_scene(plotter_hatch_plot())$packet
  vp <- new_scene_viewport(152.4, 101.6, 300)
  scene <- scene_compile(packet, new_camera_3d(), vp)
  scene$styles$fill[!is.na(scene$styles$fill)] <- "white"
  capture <- function(plotter) {
    read <- ragg::agg_capture(width = 1800, height = 1200, res = 300, background = "white")
    on.exit(grDevices::dev.off())
    grid::grid.draw(render_scene_grid(scene, vp, strokes_only = plotter))
    matrix(grDevices::col2rgb(read())[1L, ], 1200, 1800) < 40
  }
  vector <- capture(FALSE); plotter <- capture(TRUE)
  near <- function(x) {
    out <- x
    for (dy in -1:1) for (dx in -1:1) {
      rows <- 2:(nrow(x) - 1L); cols <- 2:(ncol(x) - 1L)
      out[rows, cols] <- out[rows, cols] | x[rows + dy, cols + dx]
    }
    out
  }
  expect_gt(sum(vector), 1000)
  expect_lt(mean(vector & !near(plotter)), 0.001)
  expect_lt(mean(plotter & !near(vector)), 0.001)
})
