seam_article_plot <- function(tube = FALSE) {
  if (tube) {
    angle <- seq(0, 3 * pi, length.out = 40)
    d <- data.frame(x = cos(angle), y = sin(angle), z = angle / 4)
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
      geom_tube_3d(radius = 1.5, radius.unit = "mm", fill = "#2E6F9E") +
      coord_3d(aspect = c(3, 1, 1.6))
  } else {
    a <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
    a$z <- a$x
    b <- a; b$z <- -b$x
    ggplot2::ggplot(a, ggplot2::aes(x, y, z = z)) +
      geom_surface_3d(fill = "#2E6F9E") + geom_surface_3d(data = b, fill = "#9E5E0F") +
      coord_3d(elevation = 60)
  }
}

seam_article_panel <- function(tube = FALSE) {
  capture <- ragg::agg_capture(width = 1950, height = 1350, res = 300, background = "#F5F6F8")
  on.exit(grDevices::dev.off())
  p <- seam_article_plot(tube) + ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), legend.position = "none",
      aspect.ratio = 1)
  g <- grid::grid.force(ggplot2::ggplotGrob(p))
  grid::grid.draw(g)
  list(panel = motion_panels_3d(g)[[1L]], pixels = capture())
}

test_that("tube bend interiors expose curved surfaces at the lower helix joints", {
  skip_if_not_installed("ragg")
  result <- seam_article_panel(TRUE)
  panel <- result$panel; scene <- panel$scene
  vp <- panel$children[["scene-vector-3d"]]$vp
  origin <- panel$device_origin_in * 25.4 + c(as.numeric(vp$x), as.numeric(vp$y))
  pixels <- rbind(c(780, 737), c(875, 755))
  xy <- sweep(cbind(pixels[, 1L], 1350 - pixels[, 2L]) * 25.4 / 300, 2L, origin)
  xy <- sweep(xy, 2L, c(scene$viewport$width_mm, scene$viewport$height_mm), "/")
  hits <- lapply(seq_along(scene$source$quadrics$kind), function(q)
    ggplot3d:::quadric_cast_cpp(scene$source, scene$camera_input, scene$viewport,
      q, xy[, 1L], xy[, 2L], 0L, FALSE))
  for (i in seq_len(nrow(xy))) {
    depths <- vapply(hits, function(h) if (h$hit[i]) h$depth[i] else Inf, numeric(1))
    expect_true(any(is.finite(depths)))
    expect_identical(hits[[which.min(depths)]]$part[i], 0L)
  }
})

test_that("zero-area seam unions have no paint", {
  paths <- seam_underlay_3d(c(0, 0.5, 1), c(0, 0, 0), rep(1L, 3), "blue")
  expect_length(paths, 1L)
  expect_s3_class(paths[[1L]], "null")
  grob <- seam_group_grob_3d(c(0, 0.5, 1), c(0, 0, 0), rep(1L, 3), "blue",
    grid::gpar(fill = "blue", col = NA), 1, list())
  expect_s3_class(grob, "null")
  expect_null(grob$gp)
})

test_that("each flat opaque crossing run paints one compound path", {
  skip_if_not_installed("ragg")
  panel <- seam_article_panel()$panel
  scene <- panel$scene
  grob <- render_scene_grid(scene, scene$viewport, light = panel$panel_params$coord_params$light)
  faces <- Filter(function(run) !inherits(run, "polyline") && !inherits(run, "text"), as.list(grob$children))
  expect_gt(length(faces), 0L)
  expect_length(faces, 2L)
  for (run in faces) {
    expect_s3_class(run, "pathgrob")
    expect_length(run$gp$fill, 1L)
    expect_null(run$children)
  }
})

test_that("crossing SVG paints two filled paths without triangle elements", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("svglite")
  skip_if_not_installed("xml2")
  panel <- seam_article_panel()$panel
  scene <- panel$scene
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  svglite::svglite(path, width = 6.5, height = 4.5)
  grid::grid.draw(render_scene_grid(scene, scene$viewport, light = panel$panel_params$coord_params$light))
  grDevices::dev.off()
  xml <- xml2::read_xml(path)
  filled <- xml2::xml_find_all(xml, "//*[local-name()='path' or local-name()='polygon'][contains(@style, 'fill: #5A9BCC') or contains(@style, 'fill: #9A5A06')]")
  expect_length(filled, 2L)
  expect_true(all(xml2::xml_name(filled) == "path"))
})

test_that("crossing interiors contain only their flat fill colour", {
  skip_if_not_installed("ragg")
  image <- seam_article_panel()$pixels[520:770, 940:1190]
  rgb <- grDevices::col2rgb(image)
  palette <- grDevices::col2rgb(c("#5A9BCC", "#9A5A06"))
  distance <- vapply(seq_len(ncol(palette)), function(i) apply(abs(rgb - palette[, i]), 2L, max), numeric(ncol(rgb)))
  # Exclude the true crossing boundary, where the two fills blend.
  blue <- matrix(distance[, 1L] < distance[, 2L], nrow(image), ncol(image))
  interior <- matrix(TRUE, nrow(image) - 4L, ncol(image) - 4L)
  for (dy in -2:2) for (dx in -2:2)
    interior <- interior & blue[3:(nrow(image) - 2L) + dy, 3:(ncol(image) - 2L) + dx] ==
      blue[3:(nrow(image) - 2L), 3:(ncol(image) - 2L)]
  error <- matrix(pmin(distance[, 1L], distance[, 2L]), nrow(image), ncol(image))[3:(nrow(image) - 2L), 3:(ncol(image) - 2L)]
  expect_gt(sum(interior), 50000)
  expect_lte(max(error[interior]), 1)
})

test_that("physical quadric thickness bypasses data clipping within the viewport", {
  q <- quadric_rows(1L, diag(0.2, 3L), c(0, 0.5, 0.5))
  clipped <- quadric_packet(q)
  physical <- quadric_packet(c(q, list(clip_data = FALSE)))
  camera <- new_camera_3d(azimuth = -90, elevation = 0)
  vp <- new_scene_viewport(100, 100, 150)
  a <- scene_compile(clipped, camera, vp)
  b <- scene_compile(physical, camera, vp)
  expect_true(all(a$vertices$x >= -1e-12))
  expect_lt(min(b$vertices$x), -0.1)
  expect_true(all(b$vertices$x_npc >= 0 & b$vertices$x_npc <= 1))
  merged <- merge_scene_packets(list(clipped, physical), 1L)
  expect_identical(merged$quadrics$clip_data, c(TRUE, FALSE))
})
