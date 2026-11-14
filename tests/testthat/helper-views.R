views_panels <- function(plot, width = 8, height = 8) {
  grDevices::pdf(tempfile(fileext = ".pdf"), width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  panels <- unname(motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(plot))))
  panels[order(vapply(panels, function(q) q$panel_params$panel_id, integer(1)))]
}
