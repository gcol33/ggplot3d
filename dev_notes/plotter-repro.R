suppressMessages(devtools::load_all('.', quiet = TRUE))
library(ggplot2)
out <- file.path('dev_notes', paste0('plotter-', commandArgs(TRUE)[1L]))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 17) + theme(
  plot.background = element_rect(fill = 'transparent', colour = NA),
  panel.background = element_rect(fill = 'transparent', colour = NA),
  legend.background = element_rect(fill = 'transparent', colour = NA)))
d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
d$z <- with(d, exp(-(x^2 + y^2)) + 0.5 * exp(-((x - 1)^2 + (y + 1)^2) * 2))
hatch <- ggplot(d, aes(x, y, z = z)) +
  geom_surface_3d(fill = 'white', colour = NA, hatch = hatch_3d(angle = 0, spacing = 0.02),
    outline = outline_3d(linewidth = 0.4)) +
  coord_3d(backend = backend_3d('plotter'), panes = character(), box = 'none')
cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5),
               c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
p <- ggplot() + geom_mesh_3d(vertices = cube, faces = quads, fill = 'grey95',
  outline = outline_3d(hidden = 'dashed', linewidth = 0.8, halo = grid::unit(1.2, 'mm'))) +
  coord_3d(azimuth = -50, elevation = 25)
plots <- list(hatch = hatch, cube = p, hybrid = p + coord_3d(azimuth = -50, elevation = 25,
  backend = backend_3d('hybrid', dpi = 120, rasterise = 'all')))
for (name in names(plots)) {
  svglite::svglite(file.path(out, paste0(name, '.svg')), 6.5, 4.5, pointsize = 14)
  print(plots[[name]])
  dev.off()
  ragg::agg_png(file.path(out, paste0(name, '.png')), width = 780, height = 540, res = 120, pointsize = 14)
  print(plots[[name]])
  dev.off()
}
print(setNames(file.size(file.path(out, paste0(names(plots), '.svg'))), names(plots)))
