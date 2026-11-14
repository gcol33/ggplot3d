args <- commandArgs(TRUE)
stage <- if (length(args)) args[1L] else "before"
pkgload::load_all()
if (nzchar(Sys.getenv("PROFILE_GRID_KINDS"))) {
  kind_counts <- new.env()
  kind_counts$values <- character()
  trace("clip_grob_3d", where = asNamespace("ggplot3d"), print = FALSE,
    tracer = quote(if (!is.null(clip)) kind_counts$values <- c(kind_counts$values, class(g)[1L])))
}
out <- file.path("dev_notes/check-pdf-crash", stage)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
example_file <- if (length(args) >= 2L) args[2L] else "ggplot3d.Rcheck/ggplot3d-Ex.R"
examples <- readLines(example_file)
starts <- grep("^### \\*\\* Examples", examples)
ends <- grep('base::assign(".dptime"', examples, fixed = TRUE)
topics <- grep('nameEx(', examples, fixed = TRUE)
counts <- new.env()
counts$clips <- counts$rects <- counts$masks <- 0L
trace("resolveClipPath", where = asNamespace("grid"), print = FALSE,
  tracer = quote(if (is.null(path$ref)) counts$clips <- counts$clips + 1L))
trace("viewport", where = asNamespace("grid"), print = FALSE,
  tracer = quote({
    if (is.character(clip) && identical(clip, "on")) counts$rects <- counts$rects + 1L
    if (!is.character(mask) && !is.null(mask)) counts$masks <- counts$masks + 1L
  }))
results <- list()
for (j in seq_along(starts)) {
  a <- starts[j]
  topic <- eval(parse(text = examples[max(topics[topics < a])])[[1L]][[2L]])
  if (length(args) > 2L && !topic %in% args[-c(1L, 2L)]) next
  b <- min(ends[ends > a])
  code <- parse(text = examples[seq.int(a + 1L, b - 1L)])
  counts$clips <- counts$rects <- counts$masks <- 0L
  ragg::agg_png(file.path(out, paste0(topic, ".png")), width = 7, height = 7, units = "in", res = 100)
  env <- new.env(parent = globalenv())
  set.seed(1)
  error <- tryCatch({
    for (expr in code) {
      value <- withVisible(eval(expr, env))
      if (value$visible) print(value$value)
    }
    ""
  }, error = function(e) conditionMessage(e))
  grDevices::dev.off()
  results[[length(results) + 1L]] <- data.frame(topic, clips = counts$clips, rectangles = counts$rects,
    masks = counts$masks, error)
  utils::write.csv(do.call(rbind, results), file.path(out, "example-clips.csv"), row.names = FALSE)
  cat(topic, counts$clips, error, "\n")
}
untrace("resolveClipPath", where = asNamespace("grid"))
untrace("viewport", where = asNamespace("grid"))
print(head(do.call(rbind, results)[order(-vapply(results, function(x) x$clips, integer(1))), ], 15))
if (exists("kind_counts")) print(table(kind_counts$values))
if (any(nzchar(vapply(results, function(x) x$error, character(1))))) quit(status = 1L)
