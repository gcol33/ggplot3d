# Raster pipeline timings.
#   Rscript bench/raster-bench.R <output.csv> [cases] [threads] [antialias] [repetitions]
# cases is a comma-separated subset of names(raster_cases); threads 0 uses the default.
Sys.setlocale("LC_ALL", "C")
args <- commandArgs(TRUE)
output <- if (length(args) >= 1L) args[1L] else "bench/raster-results.csv"
source("bench/raster-cases.R")
cases <- if (length(args) >= 2L && nzchar(args[2L])) strsplit(args[2L], ",")[[1L]] else names(raster_cases)
threads <- if (length(args) >= 3L) as.integer(args[3L]) else 0L
antialias <- if (length(args) >= 4L) as.integer(args[4L]) else 2L
repetitions <- if (length(args) >= 5L) as.integer(args[5L]) else 3L
suppressMessages(devtools::load_all(quiet = TRUE))
if (threads > 0L) options(ggplot3d.threads = threads)
viewport <- raster_case_viewport()
camera <- raster_case_camera()
rows <- list()
for (case in cases) {
  packet <- raster_cases[[case]]()
  gc(reset = TRUE)
  for (rep in seq_len(repetitions)) {
    total <- system.time(result <- ggplot3d:::raster_scene_3d(packet, camera, viewport, antialias))[["elapsed"]]
    rows[[length(rows) + 1L]] <- data.frame(case = case, rep = rep,
      faces = length(packet$faces$offset), markers = length(packet$markers$offset),
      text = length(packet$text$offset), width = result$width, height = result$height,
      antialias = antialias, threads = if (is.null(result$threads)) 1L else result$threads,
      passes = result$passes, prepare_s = result$engine_seconds, draw_s = result$raster_seconds,
      setup_s = if (is.null(result$timing)) NA_real_ else result$timing[["setup"]],
      tiles_s = if (is.null(result$timing)) NA_real_ else result$timing[["tiles"]],
      total_s = total, r_max_mb = sum(gc()[, 6L]))
    print(rows[[length(rows)]])
    utils::write.csv(do.call(rbind, rows), output, row.names = FALSE)
  }
  rm(packet, result); gc()
}
