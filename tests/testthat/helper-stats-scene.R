stats_scene <- function(plot) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  grobs <- lapply(seq_along(built$plot@layers), function(i) built$plot@layers[[i]]$draw_geom(built$data[[i]], built$layout)[[1L]])
  packets <- collect_scene_layers(grobs, built$layout$panel_params[[1L]], built$plot@theme)$packets
  list(packet = merge_scene_packets(packets, 1L), built = built)
}

