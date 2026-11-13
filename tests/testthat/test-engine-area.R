test_that("variable-depth joined strokes own every covered sample once at depth cuts", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)
  for (projection in c("orthographic", "perspective")) for (cap in 0:2) for (join in 0:2) {
    packet <- ref_packet(faces = list(cbind(c(.1,.9,.9,.1), c(.1,.1,.9,.9), .5)),
      lines = list(rbind(c(.2,.2,.2), c(.5,.8,.8), c(.8,.2,.3))), line_width = 8)
    packet$faces$opacity <- .5
    packet$polylines$cap <- as.integer(cap); packet$polylines$join <- as.integer(join)
    packet$polylines$opacity <- .5
    camera <- ref_cameras()$pole; camera$projection <- projection
    camera$distance <- if (projection == "perspective") 3 else Inf
    compiled <- ggplot3d:::scene_compile(packet, camera, ggplot3d:::new_scene_viewport(100,100))
    painted <- ref_rasterise(compiled, packet, 80L)
    expect_false(any(vapply(painted$layers, function(ids) anyDuplicated(ids)>0L, logical(1))))
    lines <- which(compiled$fragments$kind == 2L)
    expect_gt(length(lines), 1L)
    expect_true(all(compiled$fragments$count[lines] == 3L))
  }
})

test_that("surface contours follow their coplanar fill in paint order", {
  packet <- ggplot3d:::new_scene_packet(
    vertices = list(x = c(.1,.9,.9,.1,.2,.8), y = c(.1,.1,.9,.9,.5,.5), z = c(.34,.66,.66,.34,.38,.62)),
    corners = list(vertex = 1:6), faces = list(offset = 1L, count = 4L),
    polylines = list(offset = 5L, width_mm = 4), styles = data.frame(style_id = 1L))
  scene <- ggplot3d:::scene_compile(packet, ggplot3d:::new_camera_3d(), ggplot3d:::new_scene_viewport(100,100))
  expect_identical(scene$fragments$kind[scene$draw_order[1L]], 1L)
  expect_true(all(scene$fragments$kind[scene$draw_order[-1L]] == 2L))
})

test_that("joined coverage remains disjoint when the near slab cuts endpoints", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)
  packet <- ref_packet(lines=list(rbind(c(.2,.2,.2),c(.5,.8,.8),c(.8,.2,.3))),line_width=8)
  packet$polylines$cap <- 1L;packet$polylines$opacity <- .5
  camera <- ref_cameras()$pole
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  camera$near <- ref_camera(camera,viewport)$distance - .1
  scene <- ggplot3d:::scene_compile(packet,camera,viewport)
  painted <- ref_rasterise(scene,packet,100L)
  expect_false(any(vapply(painted$layers,function(ids) anyDuplicated(ids)>0L,logical(1))))
  expect_true(all(scene$vertices$depth >= camera$near - scene$camera$tolerance))
})

test_that("closed solid grouping preserves the visible surface", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  data <- expand.grid(x=1:3,y=1:3);data$z <- seq_len(9)/9
  plot <- ggplot2::ggplot(data,ggplot2::aes(x,y,z=z)) + geom_col_3d(width=.6,depth=.6) + coord_3d()
  built <- ggplot2::ggplot_build(plot)
  packet <- built$plot@layers[[1L]]$draw_geom(built$data[[1L]],built$layout)[[1L]]$packet
  camera <- built$layout$panel_params[[1L]]$camera
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  compiled <- ggplot3d:::scene_compile(packet,camera,viewport)
  score <- ref_score(packet,camera,viewport,compiled,100L)
  expect_equal(score$wrong,0L)
  expect_lt(sum(compiled$fragments$kind==1L),length(packet$faces$primitive_id))
})
