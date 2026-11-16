test_that("shared furniture fits follow panel size and camera changes", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)),
    ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d() +
    facet_views_3d(c("front", "isometric"))
  panels <- ggplot3d:::motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(p)))
  panel <- panels[[1L]]
  group <- as.list(panel$frame_group)
  cache <- new.env(parent = emptyenv())
  check <- function(viewport, group) {
    cached <- ggplot3d:::panel_furniture_3d(panel$panel_params, panel$theme, viewport, group, cache)
    fresh <- ggplot3d:::panel_furniture_3d(panel$panel_params, panel$theme, viewport, group)
    expect_identical(cached$camera, fresh$camera)
    expect_identical(cached$viewport, fresh$viewport)
    expect_identical(cached$overlay$fit, fresh$overlay$fit)
  }
  small <- ggplot3d:::new_scene_viewport(90, 90, 100)
  check(small, group)
  check(small, group)
  check(ggplot3d:::new_scene_viewport(140, 110, 100), group)
  group[[2L]]$params$camera$elevation <- 15
  check(small, group)
})
