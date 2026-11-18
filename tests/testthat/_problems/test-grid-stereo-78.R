# Extracted from test-grid-stereo.R:78

# prequel ----------------------------------------------------------------------
stereo_plot <- function(stereo, ...) {
  d <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1), z = c(0, 1, 0.5))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 2) +
    coord_3d(stereo = stereo, ...)
}
stereo_drawn <- function(plot, device = function(path) grDevices::pdf(path, width = 8, height = 4)) {
  path <- tempfile()
  device(path)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  forced <- grid::grid.force(ggplot2::ggplotGrob(plot))
  grid::grid.draw(forced)
  motion_panels_3d(forced)
}
find_grobs <- function(grob, test) {
  found <- if (test(grob)) list(grob) else list()
  children <- if (inherits(grob, "gTree")) as.list(grob$children) else list()
  if (inherits(grob, "GridGroup")) children <- c(children, list(grob$src, grob$dst))
  c(found, unlist(lapply(children, find_grobs, test), recursive = FALSE))
}
basis_of <- function(camera) {
  b <- frame_basis_3d(camera, new_scene_viewport(100, 100))
  list(right = b$basis[, 1L], up = b$basis[, 2L], forward = b$forward)
}

# test -------------------------------------------------------------------------
skip_on_cran()
coord <- coord_3d(stereo = stereo_3d("pair"))
expect_equal(coord$aspect(list()), 0.5)
expect_equal(coord_3d(stereo = stereo_3d("anaglyph"))$aspect(list()), 1)
expect_equal(coord_3d()$aspect(list()), 1)
