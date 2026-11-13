.theme_state_3d <- new.env(parent = emptyenv())

register_theme_3d <- function() {
  definitions <- list()
  add <- function(name, class, parent = NULL) {
    definitions[[name]] <<- ggplot2::el_def(class, parent)
  }
  for (part in c("text", "title", "ticks", "line")) {
    add(paste0("axis.", part, ".z"), if (part %in% c("text", "title")) "element_text" else "element_line", paste0("axis.", part))
  }
  add("axis.ticks.length.z", "unit", "axis.ticks.length")
  add("axis.minor.ticks.z", "element_line", "axis.ticks.z")
  add("axis.minor.ticks.length.z", "unit", "axis.ticks.length.z")
  for (level in c("major", "minor")) add(paste0("panel.grid.", level, ".z"), "element_line", paste0("panel.grid.", level))
  add("panel.pane", "element_rect", "panel.background")
  for (axis in c("x", "y", "z")) add(paste0("panel.pane.", axis), "element_rect", "panel.pane")
  add("panel.box", "element_line", "line")
  for (side in c("back", "front")) add(paste0("panel.box.", side), "element_line", "panel.box")
  add("panel.margin.3d", "margin")
  ggplot2::register_theme_elements(
    axis.minor.ticks.length.z = grid::unit(1, "pt"),
    panel.box = ggplot2::element_line(colour = "grey50", linewidth = 0.3, inherit.blank = TRUE),
    panel.margin.3d = ggplot2::margin(8, 8, 8, 8, unit = "pt"),
    element_tree = definitions
  )
  .theme_state_3d$registered <- TRUE
  invisible(NULL)
}

element_3d <- function(name, theme) ggplot2::calc_element(name, theme)

visible_element_3d <- function(element) !is.null(element) && !inherits(element, "element_blank")

text_gp_3d <- function(element) {
  grid::gpar(col = element$colour, fontsize = element$size,
    fontfamily = element$family, fontface = element$face, lineheight = element$lineheight)
}

line_gp_3d <- function(element) {
  grid::gpar(col = element$colour, lwd = element$linewidth * 72.27 / 25.4,
    lty = element$linetype, lineend = element$lineend %or3d% "butt")
}
