text_facing_scene <- function(..., coord = coord_3d(), labels = c("BO", "O")) {
  d <- data.frame(x = c(.3, .7), y = c(.4, .6), z = c(.35, .65), label = labels)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label)) +
    geom_text_3d(orientation = "plane", size = 8, ...) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) + coord
  s <- integration_packets(p)
  list(plot = p, packet = s$grobs[[1L]]$packet, camera = s$params$camera, params = s$params)
}
