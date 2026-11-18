# Extracted from test-grid-seams.R:44

# prequel ----------------------------------------------------------------------
seam_article_plot <- function(tube = FALSE) {
  if (tube) {
    angle <- seq(0, 3 * pi, length.out = 40)
    d <- data.frame(x = cos(angle), y = sin(angle), z = angle / 4)
    ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
      geom_tube_3d(radius = 1.5, radius.unit = "mm", fill = "#2E6F9E") +
      coord_3d(aspect = c(3, 1, 1.6))
  } else {
    a <- expand.grid(x = seq(-1, 1, length.out = 8), y = seq(-1, 1, length.out = 8))
    a$z <- a$x
    b <- a; b$z <- -b$x
    ggplot2::ggplot(a, ggplot2::aes(x, y, z = z)) +
      geom_surface_3d(fill = "#2E6F9E") + geom_surface_3d(data = b, fill = "#9E5E0F") +
      coord_3d(elevation = 60)
  }
}
seam_article_panel <- function(tube = FALSE) {
  capture <- ragg::agg_capture(width = 1950, height = 1350, res = 300, background = "#F5F6F8")
  on.exit(grDevices::dev.off())
  p <- seam_article_plot(tube) + ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), legend.position = "none")
  g <- grid::grid.force(ggplot2::ggplotGrob(p))
  grid::grid.draw(g)
  list(panel = motion_panels_3d(g)[[1L]], pixels = capture())
}

# test -------------------------------------------------------------------------
skip_if_not_installed("ragg")
result <- seam_article_panel(TRUE)
panel <- result$panel
scene <- panel$scene
vp <- panel$children[["scene-vector-3d"]]$vp
origin <- panel$device_origin_in * 25.4 + c(as.numeric(vp$x), as.numeric(vp$y))
pixels <- rbind(c(780, 737), c(875, 755))
xy <- sweep(cbind(pixels[, 1L], 1350 - pixels[, 2L]) * 25.4 / 300, 2L, origin)
xy <- sweep(xy, 2L, c(scene$viewport$width_mm, scene$viewport$height_mm), "/")
hits <- lapply(seq_along(scene$source$quadrics$kind), function(q)
    ggplot3d:::quadric_cast_cpp(scene$source, scene$camera_input, scene$viewport,
      q, xy[, 1L], xy[, 2L], 0L, FALSE))
for (i in seq_len(nrow(xy))) {
    depths <- vapply(hits, function(h) if (h$hit[i]) h$depth[i] else Inf, numeric(1))
    expect_true(any(is.finite(depths)))
    expect_identical(hits[[which.min(depths)]]$part[i], 0L)
  }
