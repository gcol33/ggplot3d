source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

splat_packet <- function(xyz, render = "splat", edl = 0, radius = 8) {
  packet <- ref_packet(markers = lapply(seq_len(nrow(xyz)), function(i) xyz[i, , drop = FALSE]), marker_radius = radius)
  packet$styles$colour <- "grey50"
  if (render == "splat") { packet$styles$render <- "splat"; packet$styles$edl <- edl }
  packet
}

pixel_of <- function(xyz, camera, viewport, n) {
  xy <- ref_project(xyz, ref_camera(camera, viewport))
  cbind(n + 1L - (1L + floor(xy[, 2L] * n)), 1L + floor(xy[, 1L] * n))
}

test_that("a splat is a depth-tested sphere of the marker radius", {
  camera <- ref_cameras()$pole
  n <- 200L
  viewport <- ggplot3d:::new_scene_viewport(100, 100, n * 25.4 / 100)
  centre <- matrix(c(.5, .5, .5), 1L)
  splat <- ggplot3d:::raster_scene_3d(splat_packet(centre), camera, viewport, 1L, buffers = TRUE)
  flat <- ggplot3d:::raster_scene_3d(splat_packet(centre, "marker"), camera, viewport, 1L, buffers = TRUE)
  expect_identical(splat$id != 0L, flat$id != 0L)
  at <- pixel_of(centre, camera, viewport, n)
  depth <- splat$buffers$depth
  expect_lt(depth[at], flat$buffers$depth[at])
  covered <- which(!is.na(depth))
  expect_identical(which.min(depth), covered[which.min(depth[covered])])
  expect_lte(abs(depth[at] - min(depth, na.rm = TRUE)), 1e-3 * diff(range(depth, na.rm = TRUE)) + 1e-12)
  expect_true(all(depth[covered] <= flat$buffers$depth[at] + 1e-12))
  two <- rbind(c(.45, .5, .5), c(.55, .5, .52))
  near_far <- function(render) {
    r <- ggplot3d:::raster_scene_3d(splat_packet(two, render), camera, viewport, 1L)
    r$id[pixel_of(two, camera, viewport, n)]
  }
  ids <- near_far("marker")
  spheres <- near_far("splat")
  expect_identical(length(unique(ids)), 1L)
  expect_identical(length(unique(spheres)), 2L)
})

test_that("eye-dome lighting darkens splats behind nearer splats only", {
  set.seed(4)
  xyz <- cbind(stats::runif(400, .2, .8), stats::runif(400, .2, .8), stats::runif(400, .2, .8))
  camera <- ref_cameras()$oblique
  viewport <- ggplot3d:::new_scene_viewport(60, 60, 150)
  render <- function(edl, mode = "splat") ggplot3d:::raster_scene_3d(splat_packet(xyz, mode, edl, radius = 1.5),
    camera, viewport, 2L, buffers = TRUE)
  none <- render(0); lit <- render(1); flat <- render(0, "marker")
  channel <- function(r, shift) {
    packed <- as.numeric(unclass(r$image))
    packed[is.na(packed)] <- -2^31
    packed[packed < 0] <- packed[packed < 0] + 2^32
    floor(packed / 2^shift) %% 256
  }
  green <- function(r) channel(r, 8)
  alpha <- function(r) channel(r, 24)
  expect_identical(alpha(none), alpha(lit))
  expect_true(all(green(lit) <= green(none)))
  expect_gt(sum(green(lit) < green(none)), 100L)
  expect_identical(none$id != 0L, flat$id != 0L)
})

test_that("splat and edl arguments are validated", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_error(ggplot2::ggplotGrob(p + geom_path_3d(render = "splat") + coord_3d()), "point layer")
  expect_error(ggplot2::ggplotGrob(p + geom_point_3d(render = "splat", edl = -1) + coord_3d()), "edl")
  expect_error(ggplot2::ggplotGrob(p + geom_point_3d(render = "sprite") + coord_3d()))
  packet <- stats_scene(p + geom_point_3d(render = "splat", edl = 2) + coord_3d())$packet
  expect_identical(unique(packet$styles$render), "splat")
  expect_identical(unique(packet$styles$edl), 2)
})

test_that("the vector backend refuses panels above the marker limit", {
  old <- options(ggplot3d.vector_markers = 10)
  on.exit(options(old))
  d <- data.frame(x = 1:20, y = 1:20, z = 1:20)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  panel <- function(plot) {
    find <- function(g) {
      if (inherits(g, "scene_panel_grob")) return(list(g))
      unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
    }
    find(ggplot2::ggplotGrob(plot))[[1L]]
  }
  expect_error(grid::makeContent(panel(p + coord_3d())), "at most 10 markers per panel and this panel has 20")
  expect_s3_class(grid::makeContent(panel(p + coord_3d(backend = backend_3d("raster", dpi = 30)))), "scene_panel_grob")
})
