test_that("points use ggplot2 physical size and stroke conventions", {
  capture <- function(grob) {
    read <- ragg::agg_capture(width = 400, height = 400, res = 254, background = "white")
    on.exit(grDevices::dev.off())
    grid::pushViewport(grid::viewport(xscale = c(0,1), yscale = c(0,1)))
    grid::grid.draw(grob)
    read()
  }
  identity <- ggplot2::ggproto(NULL, ggplot2::Coord, transform = function(data, panel_params) data)
  for (shape in c(19L, 21L)) for (size in c(1.5, 4, 10)) {
    data <- data.frame(x = .5, y = .5, z = .5, size = size, stroke = .5, shape = shape,
      colour = "black", fill = "red", alpha = 1, .layer = 1L, .source = 1L)
    packet <- ggplot3d:::emit_points_3d(data, list(), list(panel_id = 1L))
    viewport <- ggplot3d:::new_scene_viewport(40, 40, 254)
    scene <- ggplot3d:::scene_compile(packet, ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90), viewport)
    ordinary <- capture(ggplot2::GeomPoint$draw_panel(data, list(), identity))
    spatial <- capture(ggplot3d:::render_scene_grid(scene, viewport))
    expect_identical(spatial, ordinary)
    covered <- matrix(grDevices::col2rgb(spatial)[2L, ] < 128, 400, 400)
    diameter <- sum(colSums(covered) > 0) / 10
    expect_equal(diameter, 2 * packet$footprints$radius_x_mm, tolerance = .12)
  }
})

test_that("packed line widths have the same physical paint as ggplot2", {
  styles <- data.frame(linewidth = c(.1, .5, 2), size = 2, stroke = .5)
  metrics <- ggplot3d:::paint_metrics_3d(styles)
  expect_equal(metrics$line_lwd, styles$linewidth * ggplot2::.pt)
  expect_equal(metrics$width_mm, metrics$line_lwd * 25.4 / 96)
  expect_equal(ggplot3d:::dash_pattern_3d("dashed", metrics$width_mm[2]), rep(4 * metrics$width_mm[2], 2))
})

test_that("hidden and visible coverage retain separate drawing styles", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)
  packet <- ref_packet(faces = list(ref_quad(.5)),
    lines = list(rbind(c(.1,.2,.2),c(.5,.8,.8),c(.9,.2,.3))), line_width = 8)
  packet$polylines$hidden <- 1L
  scene <- ggplot3d:::scene_compile(packet, ref_cameras()$pole, ggplot3d:::new_scene_viewport(100,100))
  capture <- ragg::agg_capture(width = 400, height = 400)
  on.exit(grDevices::dev.off())
  grob <- ggplot3d:::render_scene_grid(scene, scene$viewport)
  for (run in grob$fragment_runs) {
    if (all(scene$fragments$kind[run] == 2L))
      expect_length(unique(scene$fragments$hidden[run]), 1L)
  }
})

test_that("visible cube boundary centres retain their black strokes", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  plot <- ggplot2::ggplot(data.frame(x=1,y=1,z=1),ggplot2::aes(x,y,z=z)) +
    geom_col_3d(colour="black",linewidth=1) + coord_3d(expand=FALSE)
  built <- ggplot2::ggplot_build(plot)
  packet <- built$plot@layers[[1L]]$draw_geom(built$data[[1L]],built$layout)[[1L]]$packet
  viewport <- ggplot3d:::new_scene_viewport(100,100,101.6)
  camera <- built$layout$panel_params[[1L]]$camera
  compiled <- ggplot3d:::scene_compile(packet,camera,viewport)
  reference <- ref_camera(camera,viewport)
  read <- ragg::agg_capture(width=400,height=400,res=101.6,background="white")
  on.exit(grDevices::dev.off())
  grid::pushViewport(grid::viewport(xscale=c(0,1),yscale=c(0,1)))
  grid::grid.draw(ggplot3d:::render_scene_grid(compiled,viewport))
  pixels <- read()
  faces <- compiled
  faces$draw_order <- faces$draw_order[faces$fragments$kind[faces$draw_order] == 1L]
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale=c(0,1),yscale=c(0,1)))
  grid::grid.draw(ggplot3d:::render_scene_grid(faces,viewport))
  background <- read()
  visible <- 0L
  for (i in seq_along(packet$polylines$count)) {
    midpoint <- matrix(colMeans(ref_primitive_xyz(packet,"polylines",i)),nrow=1L)
    xy <- ref_project(midpoint,reference)
    depths <- vapply(seq_along(packet$faces$count),function(j)
      ref_face_depth(xy[,1:2,drop=FALSE],ref_primitive_xyz(packet,"faces",j),reference),numeric(1))
    if (any(depths < xy[1L,3L] - 1e-10)) next
    visible <- visible + 1L
    pixel <- c(400L-floor(xy[1L,2L]*400),1L+floor(xy[1L,1L]*400))
    expect_lt(max(grDevices::col2rgb(pixels[pixel[1L],pixel[2L]])),
      max(grDevices::col2rgb(background[pixel[1L],pixel[2L]]))/2)
  }
  expect_gt(visible,0L)
})
