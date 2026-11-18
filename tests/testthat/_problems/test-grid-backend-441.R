# Extracted from test-grid-backend.R:441

# prequel ----------------------------------------------------------------------
grid_test_device <- function(code, width = 7, height = 7) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = width, height = height, units = "in", res = 150)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  force(code)
}
grid_test_scene <- function() {
  packet <- new_scene_packet(
    vertices = list(x = c(0, 1, 0, 0.4, 0.6), y = c(0, 0, 1, 0.4, 0.6), z = c(0, 0, 0, 0.4, 0.6)),
    corners = list(vertex = 1:5), faces = list(offset = 1L, style_id = 1L),
    text = list(offset = 4:5, style_id = 2:3),
    styles = data.frame(style_id = 1:3, fill = c("red", NA, NA), colour = c(NA, "blue", "black"), label = c("", "a", "b")))
  scene <- scene_compile(packet, new_camera_3d(), new_scene_viewport(100, 100, 150))
  permutation <- order(scene$fragments$kind)
  scene$fragments <- lapply(scene$fragments, `[`, permutation)
  scene$fragments$fragment_id <- seq_along(permutation)
  scene$draw_order <- seq_along(permutation)
  scene
}
grid_test_panels <- function(grob) {
  if (inherits(grob, "scene_panel_grob") && !is.null(grob$scene)) return(list(grob))
  children <- if (length(grob$children)) as.list(grob$children) else grob$grobs
  unlist(lapply(children, grid_test_panels), recursive = FALSE)
}
grid_test_plot <- function(...) {
  ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_point() + coord_3d(...) + ggplot2::labs(x = "Horizontal x", y = "Horizontal y", z = "Height z")
}

# test -------------------------------------------------------------------------
d <- data.frame(x = c(-0.4, 0, 0.96), y = c(0, 0.93, 0), z = c(-0.25, 0, 0.25))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    coord_3d(aspect = "data") + ggplot2::theme_minimal(base_size = 17)
grDevices::pdf(tempfile(fileext = ".pdf"), width = 6, height = 5)
panel <- motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
grDevices::dev.off()
layout <- panel$furniture$label_layout$z
kept <- layout$boxes[layout$keep, , drop = FALSE]
expect_lt(sum(layout$keep), sum(is.finite(layout$boxes[, 1L])))
