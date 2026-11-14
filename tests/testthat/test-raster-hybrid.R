source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

crossing_scene <- function(aa = 2L) {
  camera <- ref_cameras()$pole
  viewport <- ggplot3d:::new_scene_viewport(100, 100, 100)
  quad <- ref_packet(faces = list(ref_quad(.5)))
  quad$styles$fill <- "grey70"
  line <- ref_packet(lines = list(rbind(c(.2, .5, .2), c(.8, .5, .8))), line_width = 1)
  line$styles$colour <- "red"
  result <- ggplot3d:::raster_scene_3d(quad, camera, viewport, aa, buffers = TRUE)
  list(camera = camera, viewport = viewport, quad = quad, line = line, result = result,
       occlusion = ggplot3d:::raster_occlusion_3d(line, camera, viewport, result))
}

test_that("a vector stroke through a rasterised face is hidden on the far side only", {
  s <- crossing_scene()
  expect_identical(s$occlusion$status$polylines, 2L)
  rings <- s$occlusion$rings
  expect_true(all(rings$kind == 2L & rings$row == 1L))
  xy <- ref_project(rbind(c(.2, .5, .2), c(.5, .5, .5), c(.8, .5, .8)), ref_camera(s$camera, s$viewport))
  pixel <- 1 / s$result$width
  far <- range(xy[1:2, 1L]) + c(-3, 3) * pixel
  expect_true(all(rings$x >= far[1L] & rings$x <= far[2L]))
  expect_false(any(abs(rings$x - xy[3L, 1L]) < 3 * pixel))
  expect_identical(crossing_scene(1L)$occlusion$status, s$occlusion$status)
  s$line$polylines$partition[] <- 2L
  front <- ggplot3d:::raster_occlusion_3d(s$line, s$camera, s$viewport, s$result)
  expect_identical(front$status$polylines, 0L)
  expect_length(front$rings$x, 0L)
})

test_that("traced rings cover exactly the hidden pixels under the winding rule", {
  s <- crossing_scene()
  rings <- s$occlusion$rings
  area <- vapply(split(seq_along(rings$x), rings$ring), function(i) {
    x <- rings$x[i]; y <- rings$y[i]; j <- c(i[-1L], i[1L])
    sum(x * rings$y[j] - rings$x[j] * y) / 2
  }, numeric(1))
  expect_true(all(area != 0))
  expect_gt(sum(area), 0)
})

test_that("hybrid panels draw rasterised layers as one image between two vector passes", {
  skip_if_not_installed("ragg")
  d <- expand.grid(x = seq(-1, 1, length.out = 20), y = seq(-1, 1, length.out = 20))
  d$z <- d$x * d$y
  path <- data.frame(x = c(-1, 1), y = c(0, 0), z = c(-1.5, 1.5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "grey80", colour = NA, raster = TRUE) +
    geom_path_3d(data = path, colour = "red")
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, 400, 400, res = 100)
  grob <- grid::grid.force(ggplot2::ggplotGrob(p + coord_3d(backend = backend_3d("hybrid", dpi = 100))))
  grDevices::dev.off()
  unlink(file)
  find <- function(g) {
    if (identical(g$name, "scene-hybrid-3d")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  panel <- find(grob)[[1L]]
  expect_identical(names(panel$children),
    c("scene-hybrid-behind-3d", "scene-raster-image-3d", "scene-hybrid-front-3d"))
  expect_s3_class(panel$children[[2L]], "rastergrob")
  expect_true(panel$children[[2L]]$interpolate)
  expect_true(any(panel$raster_result$id != 0L))
  expect_identical(utils::tail(panel$occlusion$status$polylines, 1L), 2L)
  expect_error(ggplot2::ggplotGrob(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(raster = "yes") + coord_3d()), "raster must be TRUE or FALSE")
})

test_that("rasterised layers must be depth tested", {
  d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 0))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_polygon_3d(fill = "grey", raster = TRUE, occlusion = "overlay") +
    coord_3d(backend = backend_3d("hybrid", dpi = 50))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  find <- function(g) {
    if (inherits(g, "scene_panel_grob")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  expect_error(grid::makeContent(find(ggplot2::ggplotGrob(p))[[1L]]), "occlusion = \"depth\"")
})

test_that("raster panels keep the furniture underlay, overlay and labels as distinct children", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(occlusion = "overlay") +
    coord_3d(backend = backend_3d("raster", dpi = 50))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  find <- function(g) {
    if (identical(g$name, "scene-raster-3d")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  panel <- find(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
  expect_false(anyDuplicated(panel$childrenOrder) > 0L)
  expect_identical(panel$childrenOrder,
    c("scene-raster-underlay-3d", "scene-raster-image-3d", "scene-raster-overlay-3d", "scene-raster-labels-3d"))
})

test_that("hybrid panels with rasterise = \"surfaces\" draw unmarked surface layers into the image", {
  d <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
  d$z <- d$x * d$y
  path <- data.frame(x = c(-1, 1), y = c(0, 0), z = c(-1.5, 1.5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "grey80", colour = NA) +
    geom_path_3d(data = path, colour = "red") +
    geom_polygon_3d(data = data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = 1), fill = "blue", occlusion = "overlay")
  find <- function(g) {
    if (identical(g$name, "scene-hybrid-3d")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  panel <- function(backend) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    find(grid::grid.force(ggplot2::ggplotGrob(p + coord_3d(backend = backend))))[[1L]]
  }
  expect_identical(backend_3d("hybrid")$rasterise, "marked")
  expect_null(backend_3d("raster")$rasterise)
  expect_error(backend_3d("hybrid", rasterise = "every"), "should be one of")
  expect_length(panel(backend_3d("hybrid", dpi = 50))$capabilities$image_layers, 0L)
  surfaces <- panel(backend_3d("hybrid", dpi = 50, rasterise = "surfaces"))
  expect_identical(surfaces$capabilities$image_layers, 1L)
  expect_s3_class(surfaces$children[["scene-raster-image-3d"]], "rastergrob")
  expect_true(any(surfaces$raster_result$id != 0L))
  expect_identical(panel(backend_3d("hybrid", dpi = 50, rasterise = "all"))$capabilities$image_layers, 1:2)
})

test_that("vector contours on a rasterised surface are seen, and paths under it hidden", {
  d <- expand.grid(x = seq(-1, 1, length.out = 20), y = seq(-1, 1, length.out = 20))
  d$z <- sin(pi * d$x) * cos(pi * d$y)
  under <- data.frame(x = seq(-0.9, 0.9, length.out = 40), y = 0.5, z = -0.3)
  status <- function(elevation, layer) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
      geom_surface_3d(fill = "grey80", colour = NA) + layer +
      coord_3d(elevation = elevation, backend = backend_3d("hybrid", dpi = 72, rasterise = "surfaces"))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    find <- function(g) {
      if (identical(g$name, "scene-hybrid-3d")) return(list(g))
      unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
    }
    s <- find(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]$occlusion$status$polylines
    utils::tail(s, length(unique(ggplot2::layer_data(p, 2L)$group)))
  }
  contours <- geom_contour_3d(colour = "red")
  expect_true(all(status(89, contours) == 0L))
  expect_identical(status(89, geom_path_3d(data = under, colour = "red")), 1L)
  expect_true(2L %in% status(20, contours))
})

test_that("hybrid images take rear-pane layers under their depth-tested paint", {
  d <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
  d$z <- exp(-2 * (d$x^2 + d$y^2))
  above <- data.frame(x = c(0, 0.5), y = c(0, -0.5), z = c(1.2, 0.6))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_contour_filled_3d(plane = "floor", bins = 4) +
    geom_contour_3d(plane = "floor", bins = 4, colour = "black") +
    geom_point_3d(data = above, colour = "red", size = 3) +
    coord_3d(elevation = 40, backend = backend_3d("hybrid", dpi = 50, rasterise = "surfaces"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  find <- function(g) {
    if (identical(g$name, "scene-hybrid-3d")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  panel <- find(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
  expect_identical(panel$capabilities$image_layers, 1:2)
  expect_true(all(panel$occlusion$status$markers == 0L))
  covered <- !is.na(panel$raster_result$buffers$depth)
  expect_true(any(covered))
  expect_true(all(is.na(panel$raster_result$buffers$coverage_depth[covered])))
})
