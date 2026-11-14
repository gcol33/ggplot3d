# The scene panel of a plot's first panel after draw-time compilation.
drawn_panel_3d <- function(plot) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  find <- function(g) {
    if (inherits(g, "scene_panel_grob")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  grid::makeContent(find(ggplot2::ggplotGrob(plot))[[1L]])
}
