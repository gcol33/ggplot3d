args <- commandArgs(TRUE)
stage <- args[1L]
pkgload::load_all()
if (stage == "before") {
  baseline <- "dev_notes/check-pdf-crash/backend-grid-before.R"
  if (!file.exists(baseline)) writeLines(system2("git", c("show", "13c4f58:R/backend-grid.R"), stdout = TRUE), baseline)
  original <- new.env(parent = asNamespace("ggplot3d"))
  sys.source(baseline, original)
  environment(original$render_scene_grid) <- asNamespace("ggplot3d")
  assignInNamespace("render_scene_grid", original$render_scene_grid, "ggplot3d")
}
out <- file.path("dev_notes/check-pdf-crash", stage)
dir.create(out, showWarnings = FALSE, recursive = TRUE)
counts <- new.env()
counts$scenes <- list()
trace("render_scene_grid", where = asNamespace("ggplot3d"), print = FALSE,
  tracer = quote(counts$scenes[[length(counts$scenes) + 1L]] <- list(scene = scene, viewport = viewport, light = light, regions = regions)))
if (stage == "before") {
  res <- testthat::test_dir("tests/testthat", filter = "^grid-(backend|opacity|parity)$", reporter = "summary", env = new.env(parent = asNamespace("ggplot3d")), stop_on_failure = FALSE)
  print(as.data.frame(res)[c("test", "failed", "error")])
  saveRDS(counts$scenes, "dev_notes/check-pdf-crash/grid-scenes.rds")
}
untrace("render_scene_grid", where = asNamespace("ggplot3d"))
scenes <- readRDS("dev_notes/check-pdf-crash/grid-scenes.rds")
for (i in seq_along(scenes)) {
  a <- scenes[[i]]
  for (device in c("png", "pdf")) {
    file <- file.path(out, sprintf("grid-%03d.%s", i, device))
    if (device == "png") ragg::agg_png(file, width = a$viewport$width_mm, height = a$viewport$height_mm, units = "mm", res = 120)
    else grDevices::cairo_pdf(file, width = a$viewport$width_mm / 25.4, height = a$viewport$height_mm / 25.4)
    tryCatch(grid::grid.draw(do.call(ggplot3d:::render_scene_grid, a)), error = function(e) cat("REPLAY", i, conditionMessage(e), "\n"))
    grDevices::dev.off()
  }
}
lines <- readLines("vignettes/gallery.Rmd")
starts <- grep("^```\\{r", lines)
ends <- grep("^```$", lines)
for (device in c("png", "pdf")) {
  env <- new.env(parent = globalenv())
  for (i in seq_along(starts)) {
    a <- starts[i]; b <- min(ends[ends > a])
    code <- parse(text = lines[seq.int(a + 1L, b - 1L)])
    file <- file.path(out, sprintf("gallery-%02d.%s", i, device))
    if (device == "png") ragg::agg_png(file, width = 6.5, height = 4.5, units = "in", res = 120)
    else grDevices::cairo_pdf(file, width = 6.5, height = 4.5)
    tryCatch(for (e in code) {
      v <- withVisible(eval(e, env))
      if (v$visible) print(v$value)
    }, error = function(e) cat("GALLERY", i, conditionMessage(e), "\n"))
    grDevices::dev.off()
    cat("GALLERY DONE", device, i, "\n")
  }
}
