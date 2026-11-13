# Pixel comparison of the raster renderer against a saved reference run.
#   Rscript bench/raster-compare.R save <reference.rds>
#   Rscript bench/raster-compare.R compare <reference.rds> [linear|srgb]
# The scene set covers the oracle fixtures under three cameras, the stroke
# styling scenes of the raster tests and reduced bench scenes.
Sys.setlocale("LC_ALL", "C")
args <- commandArgs(TRUE)
mode <- args[1L]; path <- args[2L]
blend <- if (length(args) >= 3L) args[3L] else "linear"
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/fixtures/oracle/reference.R")
source("tests/fixtures/scenes/builders.R")
source("bench/raster-cases.R")

compare_scenes <- function() {
  out <- list()
  for (name in names(ref_fixture_builders())) for (view in names(ref_cameras())) {
    f <- ref_fixture(name, view)
    f$packet$styles$fill <- "red"; f$packet$styles$colour <- "blue"
    out[[paste(name, view, "opaque")]] <- f
    g <- f
    g$packet$faces$opacity[] <- 0.45; g$packet$markers$opacity[] <- 0.6; g$packet$polylines$opacity[] <- 0.7
    out[[paste(name, view, "translucent")]] <- g
  }
  f <- ref_fixture("thick_line", "oblique")
  f$packet$styles$fill <- "red"; f$packet$styles$colour <- "blue"
  for (hidden in 0:2) { g <- f; g$packet$polylines$hidden[] <- hidden; out[[paste("hidden", hidden)]] <- g }
  g <- f
  g$packet$styles$arrow <- I(rep(list(grid::arrow(length = grid::unit(8, "mm"), type = "closed")), nrow(g$packet$styles)))
  g$packet$polylines$arrow_end[] <- TRUE
  out$arrow <- g
  g$camera$clip <- FALSE; g$camera$zoom <- 4
  out$unclipped <- g
  g <- f; g$packet$polylines$opacity[] <- 0.5; g$packet$styles$linetype <- "dashed"
  out$dashed_translucent <- g
  viewport <- ggplot3d:::new_scene_viewport(60, 60, 150)
  camera <- raster_case_camera()
  small <- function(packet) list(packet = packet, camera = camera, viewport = viewport)
  out$faces_1e5 <- small(raster_cases$faces_1e5())
  out$translucent_8 <- small(raster_cases$translucent_8())
  out$text_2e3 <- small(raster_cases$text_2e3())
  out$markers_1e5 <- small(raster_cases$markers_1e6(1e5))
  out
}

render <- function(scenes) lapply(scenes, function(f) lapply(c(1L, 2L), function(aa) {
  r <- ggplot3d:::raster_scene_3d(f$packet, f$camera, f$viewport, aa, blend = blend)
  r[c("image", "id", "text", "passes", "extent", "width", "height")]
}))

scenes <- compare_scenes()
if (mode == "save") {
  saveRDS(render(scenes), path)
} else {
  reference <- readRDS(path)
  current <- render(scenes)
  differences <- 0L
  for (name in names(reference)) for (k in 1:2) {
    a <- reference[[name]][[k]]; b <- current[[name]][[k]]
    for (field in names(a)) {
      if (!identical(unclass(a[[field]]), unclass(b[[field]]))) {
        differences <- differences + 1L
        extra <- if (field %in% c("image", "id")) sprintf(" (%d of %d pixels)", sum(a[[field]] != b[[field]]), length(a[[field]])) else ""
        cat(sprintf("%s aa=%d: %s differs%s\n", name, k, field, extra))
      }
    }
  }
  cat(sprintf("%d scenes x 2 antialias levels compared, %d differing fields\n", length(reference), differences))
}
