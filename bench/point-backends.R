# Point-cloud timings by backend: ggsave of geom_point_3d() to PNG through ragg.
#   Rscript bench/point-backends.R <output.csv> [sizes] [backends]
Sys.setlocale("LC_ALL", "C")
args <- commandArgs(TRUE)
output <- if (length(args) >= 1L) args[1L] else "bench/point-backends.csv"
sizes <- if (length(args) >= 2L && nzchar(args[2L])) as.numeric(strsplit(args[2L], ",")[[1L]]) else c(1e3, 1e4, 3e4, 1e5)
backends <- if (length(args) >= 3L) strsplit(args[3L], ",")[[1L]] else c("vector", "raster", "splat")
suppressMessages(devtools::load_all(quiet = TRUE))
rows <- list()
for (n in sizes) for (backend in backends) {
  set.seed(1)
  d <- data.frame(x = stats::rnorm(n), y = stats::rnorm(n), z = stats::rnorm(n))
  layer <- if (backend == "splat") geom_point_3d(size = 0.6, render = "splat") else geom_point_3d(size = 0.6)
  coord <- coord_3d(backend = backend_3d(if (backend == "vector") "vector" else "raster", dpi = 150))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + layer + coord
  file <- tempfile(fileext = ".png")
  status <- "ok"
  elapsed <- tryCatch(system.time(ggplot2::ggsave(file, p, width = 6, height = 6, dpi = 150,
    device = ragg::agg_png, bg = "white"))[["elapsed"]], error = function(e) { status <<- conditionMessage(e); NA_real_ })
  unlink(file)
  rows[[length(rows) + 1L]] <- data.frame(points = n, backend = backend, seconds = elapsed, status = status)
  print(rows[[length(rows)]])
  utils::write.csv(do.call(rbind, rows), output, row.names = FALSE)
}
