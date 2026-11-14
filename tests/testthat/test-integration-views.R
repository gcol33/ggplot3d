views_block <- function() {
  block <- expand.grid(x = 1:4, y = 1:3, z = 1:3)
  block <- block[block$z == 1 | (block$x == 1 & block$y < 3), ]
  ggplot2::ggplot(block, ggplot2::aes(x, y, z = z)) +
    geom_voxel_3d(fill = "grey80", colour = "grey20")
}

test_that("principal views follow the third- and first-angle placements of ISO 5456-2", {
  labels <- c("front", "top", "right", "iso")
  third <- views_layout_3d(labels, "third_angle")
  expect_equal(third$ROW, c(2L, 1L, 2L, 1L))
  expect_equal(third$COL, c(1L, 1L, 2L, 2L))
  first <- views_layout_3d(labels, "first_angle")
  expect_equal(first$ROW, c(2L, 3L, 2L, 1L))
  expect_equal(first$COL, c(2L, 2L, 1L, 3L))
  six <- views_layout_3d(c("front", "top", "bottom", "left", "right", "rear"), "third_angle")
  expect_equal(six$ROW, c(2L, 1L, 3L, 2L, 2L, 2L))
  expect_equal(six$COL, c(2L, 2L, 2L, 1L, 3L, 4L))
  mirrored <- views_layout_3d(c("front", "top", "bottom", "left", "right", "rear"), "first_angle")
  expect_equal(mirrored$ROW, c(2L, 3L, 1L, 2L, 2L, 2L))
  expect_equal(mirrored$COL, c(2L, 2L, 2L, 3L, 1L, 4L))
  wrap <- views_layout_3d(c("a", "b", "c", "d", "e"), "wrap")
  expect_equal(wrap$ROW, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(wrap$COL, c(1L, 2L, 3L, 1L, 2L))
  expect_equal(views_layout_3d(c("a", "b", "c"), "wrap", ncol = 1)$ROW, 1:3)
  expect_equal(views_layout_3d(labels, "wrap")$VIEW, 1:4)
  expect_error(views_layout_3d(c("a", "b", "c"), "wrap", nrow = 1, ncol = 2), "at least the number of views")
  expect_error(views_layout_3d(c("front", letters[1:7]), "third_angle"), "at most 6 views")
})

test_that("views are presets or named camera argument lists", {
  views <- views_spec_3d(c("front", iso = "isometric"))
  expect_named(views, c("front", "iso"))
  expect_identical(views$iso, camera_3d("isometric"))
  custom <- views_spec_3d(list(a = list(azimuth = 30), b = "top"))
  expect_identical(custom$a, list(azimuth = 30))
  expect_error(views_spec_3d("sideways"), "Unknown camera view")
  expect_error(views_spec_3d(list(list(azimuth = 30))), "needs a name")
  expect_error(views_spec_3d(list(a = list(spin = 3))), "camera_3d\\(\\) arguments")
  expect_error(views_spec_3d(c(a = "front", a = "top")), "unique")
  expect_error(facet_views(nrow = 0), "positive whole number")
})

test_that("each panel takes its view's camera over the coord's", {
  p <- views_block() + coord_3d(projection = "perspective", zoom = 0.9) +
    facet_views(list(front = "front", turned = list(azimuth = 30, elevation = 10)))
  built <- ggplot2::ggplot_build(p)
  cameras <- lapply(built$layout$panel_params, `[[`, "camera")
  expect_identical(cameras[[1L]]$projection, "orthographic")
  expect_equal(c(cameras[[1L]]$azimuth, cameras[[1L]]$elevation), c(-90, 0))
  expect_identical(cameras[[2L]]$projection, "perspective")
  expect_equal(c(cameras[[2L]]$azimuth, cameras[[2L]]$elevation, cameras[[2L]]$zoom), c(30, 10, 0.9))
  expect_identical(as.character(built$layout$layout$view), c("front", "turned"))
  expect_error(ggplot2::ggplot_build(views_block() + coord_3d(azimuth = c(0, 90)) + facet_views(c("front", "top"))),
    "not both")
})

test_that("the scene is computed once and shared by every view", {
  p <- views_block() + coord_3d(z_scales = "free") + facet_views()
  built <- ggplot2::ggplot_build(p)
  expect_true(all(built$data[[1L]]$PANEL == 1L))
  expect_equal(built$layout$layout$SCALE_Z, rep(1L, 4L))
  panels <- views_panels(p)
  expect_length(panels, 4L)
  for (panel in panels[-1L]) expect_identical(panel$packets, panels[[1L]]$packets)
  expect_equal(vapply(panels, function(q) q$panel_params$panel_id, integer(1)), 1:4)
})

test_that("views of a projection layout share one magnification and line up", {
  p <- views_block() + coord_3d() + facet_views(c("front", "top", "right"), layout = "third_angle")
  panels <- views_panels(p)
  zoom <- vapply(panels, function(q) q$panel_params$camera$zoom, numeric(1))
  expect_equal(zoom, rep(1, 3L))
  for (panel in panels[-1L]) expect_equal(panel$inner_viewport, panels[[1L]]$inner_viewport)
  corners <- lapply(panels, function(q) q$furniture$corners_mm)
  extent <- function(m, k) range(m[, k])
  expect_equal(extent(corners[[1L]], 1L), extent(corners[[2L]], 1L), tolerance = 1e-6)
  expect_equal(extent(corners[[1L]], 2L), extent(corners[[3L]], 2L), tolerance = 1e-6)
  expect_equal(diff(extent(corners[[1L]], 1L)), diff(extent(corners[[3L]], 1L)), tolerance = 1e-6)
})

test_that("principal presets look along the axes", {
  expect_equal(unlist(camera_3d("bottom")[c("azimuth", "elevation")]), c(azimuth = -90, elevation = -90))
  expect_equal(unlist(camera_3d("rear")[c("azimuth", "elevation")]), c(azimuth = 90, elevation = 0))
  expect_equal(unlist(camera_3d("left")[c("azimuth", "elevation")]), c(azimuth = 180, elevation = 0))
  skip_if_not_installed("ragg")
  png <- tempfile(fileext = ".png")
  ragg::agg_png(png, 600, 600)
  on.exit({grDevices::dev.off(); unlink(png)}, add = TRUE)
  expect_no_error(print(views_block() + coord_3d() +
    facet_views(c("front", "top", "bottom", "left", "right", "rear"), layout = "first_angle")))
})

test_that("facet_views() requires coord_3d()", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() + facet_views()
  expect_error(ggplot2::ggplotGrob(p), "requires coord_3d")
})

test_that("animated views keep their own projection and offsets", {
  built <- ggplot2::ggplot_build(views_block() + coord_3d() + facet_views(c("front", "cabinet")))
  base <- coord_3d()$camera
  turned <- base
  turned$azimuth <- base$azimuth + 40
  turned$zoom <- 2
  cabinet <- built$layout$panel_params[[2L]]$camera
  moved <- motion_panel_camera_3d(turned, cabinet, base)
  expect_identical(moved$projection, "oblique")
  expect_equal(moved$shear, cabinet$shear)
  expect_equal(moved$azimuth, cabinet$azimuth + 40)
  expect_equal(moved$zoom, 2 * cabinet$zoom)
})

test_that("outlined faces seen edge-on compile in an axis-aligned view", {
  skip_if_not_installed("ragg")
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, 400, 400)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  panels <- motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(views_block() + do.call(coord_3d, camera_3d("front")))))
  expect_gt(length(panels[[1L]]$scene$draw_order), 0L)
})
