# Measures physical panel layout and guide furniture without compiling data
# visibility or drawing a backend.
layout_scene_panels_3d <- function(grob) {
  probed <- motion_map_panels_3d(grob, function(panel) {
    class(panel) <- c("scene_layout_probe", class(panel))
    panel
  })
  motion_panels_3d(grid::grid.force(probed))
}

#' @export
makeContent.scene_layout_probe <- function(x) {
  x$outer_viewport <- panel_outer_viewport_3d(x$panel_params)
  x$layout_furniture <- panel_furniture_3d(x$panel_params, x$theme,
    x$outer_viewport, as.list(x$frame_group))
  x$inner_viewport <- x$layout_furniture$viewport
  x$device_origin_in <- unlist(grid::deviceLoc(grid::unit(0, "npc"),
    grid::unit(0, "npc"), valueOnly = TRUE), use.names = FALSE)
  grid::setChildren(x, grid::gList())
}
