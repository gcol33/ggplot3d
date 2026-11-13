source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

test_that("raster ids agree with independent rays at every interior fixture pixel", {
  for (name in names(ref_fixture_builders())) for (view in names(ref_cameras())) {
    f <- ref_fixture(name, view)
    n <- 80L
    f$viewport$dpi <- n * 25.4 / f$viewport$width_mm
    oracle <- ref_render(f$packet, f$camera, f$viewport, n)
    raster <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, id_pass = TRUE)
    visible <- as.vector(t(raster$id[n:1L, , drop = FALSE]))
    expect_identical(visible[!oracle$boundary], oracle$visible[!oracle$boundary],
      info = paste(name, view))
  }
})

test_that("depth peeling matches the vector translucent stack", {
  for (view in names(ref_cameras())) {
    f <- ref_fixture("translucent_points", view)
    f$packet$styles <- data.frame(style_id = 1:3, fill = c("red", NA, NA),
      colour = c(NA, "blue", "green"))
    f$packet$style_count <- 3L
    f$packet$markers$style_id <- 2:3
    f$viewport$dpi <- 80 * 25.4 / 100
    compiled <- ggplot3d:::scene_compile(f$packet, f$camera, f$viewport)
    vector <- ref_rasterise(compiled, f$packet, 80L)
    oracle <- ref_render(f$packet, f$camera, f$viewport, 80L)
    raster <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L, blend = "srgb")
    image <- matrix(as.integer(raster$image), 80L, 80L, byrow = TRUE)
    pixels <- as.vector(t(image[80:1L, , drop = FALSE]))
    channels <- vapply(c(0L, 8L, 16L, 24L), function(shift)
      bitwAnd(bitwShiftR(pixels, shift), 255L) / 255, numeric(length(pixels)))
    expected <- matrix(0, 6400, 4)
    colours <- rbind(c(1, 0, 0), c(0, 0, 1), c(0, 1, 0))
    for (i in which(!oracle$boundary)) {
      ids <- vector$layers[[i]]; alpha <- vector$alpha[[i]]
      if (!length(ids)) next
      weight <- alpha * cumprod(c(1, 1 - head(alpha, -1L)))
      expected[i, 4L] <- sum(weight)
      expected[i, 1:3] <- colSums(colours[ids, , drop = FALSE] * weight) / sum(weight)
    }
    expect_lte(max(abs(channels[!oracle$boundary, ] - expected[!oracle$boundary, ])), 1 / 255,
      label = paste("maximum RGBA error", view))
    expect_identical(raster$passes, max(lengths(oracle$layers)))
  }
})

test_that("shared triangle edges paint translucent interiors exactly once", {
  quad <- ref_quad(.7)
  triangulations <- list(list(1:4), list(c(1L, 2L, 3L), c(1L, 3L, 4L)),
    list(c(1L, 2L, 4L), c(2L, 3L, 4L)))
  for (n in c(80L, 160L)) for (elevation in c(-90, 90)) {
    camera <- ref_cameras()$pole
    camera$elevation <- elevation
    viewport <- ggplot3d:::new_scene_viewport(100, 100, n * 25.4 / 100)
    reference <- ref_packet(faces = list(quad), face_alpha = .4)
    oracle <- ref_render(reference, camera, viewport, n)
    interior <- !oracle$boundary & oracle$visible != 0L
    for (triangles in triangulations) for (reverse in c(FALSE, TRUE)) {
      faces <- lapply(triangles, function(i) quad[if (reverse) rev(i) else i, , drop = FALSE])
      packet <- ref_packet(faces = faces, face_alpha = .4)
      packet$styles$fill <- "red"
      raster <- ggplot3d:::raster_scene_3d(packet, camera, viewport, 1L)
      image <- matrix(as.integer(raster$image), n, n, byrow = TRUE)
      pixels <- as.vector(t(image[n:1L, , drop = FALSE]))
      alpha <- bitwAnd(bitwShiftR(pixels, 24L), 255L)
      expect_true(any(interior))
      expect_equal(alpha[interior], rep(102L, sum(interior)))
    }
  }
})

test_that("whole text anchors are hidden by nearer opaque coverage", {
  f <- ref_fixture("coplanar_layers", "pole")
  f$packet$text <- ggplot3d:::new_scene_packet(
    vertices = list(x = .5, y = .5, z = .2), corners = list(vertex = 1L),
    text = list(offset = 1L), styles = data.frame(style_id = 1L, colour = "black", label = "hidden"))
  text <- f$packet$text
  f$packet$text <- ggplot3d:::new_scene_packet()$text
  f$packet$styles$fill <- "red"
  packet <- ggplot3d:::merge_scene_packets(list(f$packet, text), 1L)
  result <- ggplot3d:::raster_scene_3d(packet, f$camera, f$viewport, 1L)
  expect_length(result$text$row, 0L)
  packet$text$partition[] <- 2L
  result <- ggplot3d:::raster_scene_3d(packet, f$camera, f$viewport, 1L)
  expect_identical(result$text$row, 1L)
})

test_that("device-rendered translucent colours agree at shared interior samples", {
  f <- ref_fixture("translucent_points", "pole")
  f$packet$styles <- data.frame(style_id = 1:3, fill = c("red", NA, NA),
    colour = c(NA, "blue", "green"), size = 4.4, stroke = 0, shape = 19L)
  f$packet$style_count <- 3L
  f$packet$markers$style_id <- 2:3
  f$viewport$dpi <- 400 * 25.4 / 100
  compiled <- ggplot3d:::scene_compile(f$packet, f$camera, f$viewport)
  result <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 2L, blend = "srgb")
  capture <- function(grob) {
    read <- ragg::agg_capture(width = 400, height = 400, res = f$viewport$dpi, background = "white")
    on.exit(grDevices::dev.off())
    grid::pushViewport(grid::viewport())
    grid::grid.draw(grob)
    read()
  }
  vector <- capture(ggplot3d:::render_scene_grid(compiled, f$viewport))
  raster <- capture(grid::rasterGrob(result$image, interpolate = FALSE))
  xyz <- rbind(c(.4, .5, .3), c(.6, .5, .9), c(.5, .5, .7), c(.5, .7, .7))
  xy <- ref_project(xyz, ref_camera(f$camera, f$viewport))
  pixels <- cbind(400L - floor(xy[, 2L] * 400), 1L + floor(xy[, 1L] * 400))
  expect_lte(max(abs(grDevices::col2rgb(vector[pixels]) - grDevices::col2rgb(raster[pixels]))), 1)
})

test_that("supersampling averages premultiplied colour and physical dimensions", {
  f <- ref_fixture("marker_silhouette", "pole")
  f$packet$styles <- data.frame(style_id = 1:2, fill = c("red", NA), colour = c(NA, "blue"))
  f$packet$style_count <- 2L
  f$packet$markers$style_id[] <- 2L
  f$viewport$dpi <- 64 * 25.4 / 100
  a <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 2L)
  f$viewport$dpi <- f$viewport$dpi * 2
  b <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L)
  expect_identical(dim(a$image), c(64L, 64L))
  expect_identical(dim(b$image), c(128L, 128L))
  alpha <- function(x) bitwAnd(bitwShiftR(x, 24L), 255L)
  averaged <- matrix(alpha(b$image), 128, 128, byrow = TRUE)
  averaged <- (averaged[seq(1, 128, 2), seq(1, 128, 2)] + averaged[seq(2, 128, 2), seq(1, 128, 2)] +
    averaged[seq(1, 128, 2), seq(2, 128, 2)] + averaged[seq(2, 128, 2), seq(2, 128, 2)]) / 4
  expect_lte(max(abs(matrix(alpha(a$image), 64, 64, byrow = TRUE) - averaged)), 0.5)
})

test_that("stroke joins have round, mitre and bevel physical footprints", {
  f <- ref_packet(lines = list(rbind(c(.2, .5, .5), c(.5, .5, .5), c(.5, .8, .5))), line_width = 20)
  f$styles$colour <- "red"
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 25.4)
  camera <- ref_cameras()$pole
  covered <- logical(3)
  for (join in 0:2) {
    f$polylines$join[] <- join
    result <- ggplot3d:::raster_scene_3d(f, camera, viewport, 1L)
    covered[join + 1L] <- result$id[57L, 57L] != 0L
  }
  expect_identical(covered, c(TRUE, TRUE, FALSE))
})

test_that("raster styles include retained hidden strokes, outlines, arrows and an extended canvas", {
  f <- ref_fixture("thick_line", "pole")
  f$packet$styles$fill <- "red"
  f$packet$styles$colour <- "blue"
  f$viewport$dpi <- 50.8
  render <- function() ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, 1L)
  count_blue <- function(x) sum(bitwAnd(bitwShiftR(as.integer(x$image),16L),255L) > 200L)
  hide <- render()
  f$packet$polylines$hidden[] <- 2L
  show <- render()
  f$packet$polylines$hidden[] <- 1L
  dashed <- render()
  expect_gte(count_blue(show), count_blue(dashed))
  expect_gte(count_blue(dashed), count_blue(hide))
  f$packet$polylines$hidden[] <- 0L
  f$packet$styles$arrow <- I(rep(list(grid::arrow(length = grid::unit(8, "mm"), type = "closed")), nrow(f$packet$styles)))
  f$packet$polylines$arrow_end[] <- TRUE
  arrow <- render()
  expect_gt(count_blue(arrow), count_blue(hide))
  f$camera$clip <- FALSE
  f$camera$zoom <- 4
  extended <- render()
  expect_true(any(extended$extent[c(1L, 2L)] < 0) || any(extended$extent[c(3L, 4L)] > 1))
  expect_gt(length(extended$image), length(hide$image))
})

test_that("marker fill and border retain their separate device colours", {
  data <- data.frame(x=.5, y=.5, z=.5, size=8, stroke=2, shape=21L,
    colour="black", fill="red", alpha=1, .layer=1L, .source=1L)
  packet <- ggplot3d:::emit_points_3d(data, list(), list(panel_id=1L))
  viewport <- ggplot3d:::new_scene_viewport(40,40,254)
  camera <- ref_cameras()$pole
  compiled <- ggplot3d:::scene_compile(packet,camera,viewport)
  native <- ggplot3d:::raster_scene_3d(packet,camera,viewport,4L)
  capture <- function(grob) {
    pixels <- ragg::agg_capture(width=400,height=400,res=254,background="white")
    on.exit(grDevices::dev.off())
    grid::pushViewport(grid::viewport(xscale=c(0,1),yscale=c(0,1)))
    grid::grid.draw(grob)
    pixels()
  }
  vector <- capture(ggplot3d:::render_scene_grid(compiled,viewport))
  raster <- capture(grid::rasterGrob(native$image,interpolate=FALSE))
  radius <- packet$footprints$radius_x_mm
  width <- ggplot3d:::paint_metrics_3d(data)$border_mm * 2
  columns <- 200L + round(c(0,radius-width/2,radius+1)*10)
  expect_equal(grDevices::col2rgb(raster[200L,columns]),grDevices::col2rgb(c("red","black","white")))
  expect_equal(grDevices::col2rgb(raster[200L,columns]),grDevices::col2rgb(vector[200L,columns]))
})

test_that("an arrowhead fill covers its own line body", {
  packet <- ref_packet(lines=list(rbind(c(.2,.5,.5),c(.8,.5,.5))),line_width=2)
  packet$styles$colour <- "blue"
  packet$styles$arrow <- I(list(grid::arrow(length=grid::unit(10,"mm"),type="closed")))
  packet$styles$arrow_fill <- "red"
  packet$polylines$arrow_end <- TRUE
  viewport <- ggplot3d:::new_scene_viewport(100,100,101.6)
  camera <- ref_cameras()$pole
  result <- ggplot3d:::raster_scene_3d(packet,camera,viewport,1L)
  tip <- ref_project(rbind(c(.8,.5,.5)),ref_camera(camera,viewport))
  pixel <- c(400L-floor(tip[1,2L]*400),1L+floor(tip[1,1L]*400)-20L)
  packed <- as.integer(result$image)[(pixel[1L]-1L)*400L+pixel[2L]]
  rgb <- bitwAnd(bitwShiftR(packed,c(0L,8L,16L)),255L)
  expect_equal(rgb,c(255L,0L,0L))
})
