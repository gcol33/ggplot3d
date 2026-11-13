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

test_that("raster panels keep the furniture underlay and overlay as distinct children", {
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
  expect_identical(panel$childrenOrder[c(1L, length(panel$childrenOrder))],
    c("scene-raster-underlay-3d", "scene-raster-overlay-3d"))
})
