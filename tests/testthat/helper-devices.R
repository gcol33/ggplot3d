# Whether cairo devices open and write files. capabilities("cairo") can report
# TRUE while grDevices' cairo library fails to load, as on macOS without
# XQuartz, so a small svg() file is written and checked.
cairo_devices_work <- local({
  known <- NULL
  function() {
    if (!is.null(known)) return(known)
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path))
    before <- grDevices::dev.cur()
    opened <- tryCatch(suppressWarnings({ grDevices::svg(path, 1, 1); TRUE }), error = function(e) FALSE)
    if (isTRUE(opened) && grDevices::dev.cur() != before) {
      grid::grid.newpage()
      grDevices::dev.off()
    }
    known <<- isTRUE(opened) && file.exists(path) && file.size(path) > 0
    known
  }
})
