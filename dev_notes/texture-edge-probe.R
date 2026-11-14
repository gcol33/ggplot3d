devtools::load_all()
library(ggplot2)
out <- Sys.getenv("TEXTURE_EDGE_OUT", "dev_notes/texture-edge-current")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
volcano_grid <- expand.grid(x = seq_len(nrow(volcano)), y = seq_len(ncol(volcano)))
volcano_grid$z <- as.vector(volcano)
coarse <- volcano_grid[volcano_grid$x %% 3 == 1 & volcano_grid$y %% 3 == 1, ]
checker <- matrix("#F2EFE6", 8, 8)
checker[(row(checker) + col(checker)) %% 2 == 1] <- "#2B5F75"
p <- ggplot(coarse, aes(x, y, z = z)) +
  geom_surface_3d(texture = texture_3d(checker, interpolate = FALSE)) + coord_3d()
metrics <- list()
for (dpi in c(110, 300)) for (backend in c("vector", "raster", "hybrid")) {
  plot <- plot_backend_3d(p, backend_3d(backend, dpi = dpi, rasterise = "surfaces"))
  for (device in c("png", "svg")) {
    file <- file.path(out, paste0(backend, "-", dpi, ".", device))
    elapsed <- system.time(ggsave(file, plot, device = switch(device,
      png = ragg::agg_png, svg = svglite::svglite), width = 6.5, height = 4.5, dpi = dpi))["elapsed"]
    metrics[[length(metrics) + 1L]] <- data.frame(backend, dpi, device, bytes = file.size(file), seconds = unname(elapsed))
    cat(backend, dpi, device, file.size(file), elapsed, "\n")
  }
}
write.csv(do.call(rbind, metrics), file.path(out, "metrics.csv"), row.names = FALSE)
cat("PACKAGES", paste(c("rsvg", "pdftools", "png"), vapply(c("rsvg", "pdftools", "png"), requireNamespace, logical(1), quietly = TRUE), collapse = ";"), "\n")
