#' Describe a three-dimensional plot for a document
#'
#' Describes the layers, mapped variables and initial camera. An explicit
#' `labs(alt = ...)` takes precedence. Review the description when the plot
#' communicates a finding that needs words beyond its mapped variables.
#' @param plot A ggplot with [coord_3d()].
#' @return One character string.
#' @details In knitr and Quarto documents, `view3d = TRUE` in a chunk selects
#'   [view_3d()] for HTML output. PDF and Word receive a static plot drawn
#'   with the vector scene backend. The chunk's `fig.alt` overrides the
#'   generated description. Widget dimensions follow `fig.width` and
#'   `fig.height` in inches at 96 pixels per inch.
#'
#'   The chunk option `raster3d` keeps a static plot of dense geometry
#'   small: `TRUE` draws its vector-backend panels with
#'   `backend_3d("hybrid", rasterise = "all")`, which draws the faces, paths
#'   and points of every depth-tested layer into one image while axes, panes,
#'   grid lines and text stay vector graphics. A number is a file size in
#'   bytes: the plot is drawn as vector graphics and, when the file exceeds
#'   that size, again with the hybrid backend, and the smaller file is kept.
#'   The image has the chunk's `dpi` pixels per inch, which knitr multiplies
#'   by `fig.retina` for HTML output: the density of a raster figure from the
#'   same chunk. Panels with another backend are left as they are.
#'
#'   For a raster scene saved with [ggplot2::ggsave()], set both
#'   `backend_3d("raster", dpi = 300)` and `ggsave(..., dpi = 300)` for a
#'   300 dpi image. The backend dpi controls scene sampling at its physical
#'   panel size; the device dpi controls the saved image's pixel dimensions.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'   geom_point_3d() + coord_3d(azimuth = 40, elevation = 25)
#' alt_text_3d(p)
#' alt_text_3d(p + ggplot2::labs(alt = "Fuel economy, weight and horsepower."))
#' @export
alt_text_3d <- function(plot) {
  coord <- motion_plot_coord_3d(plot)
  alt <- plot@labels$alt
  if (is.function(alt)) alt <- alt(plot)
  if (is.character(alt) && length(alt) == 1L && !is.na(alt) && nzchar(alt)) return(alt)
  layers <- vapply(plot@layers, function(layer) {
    name <- class(layer$geom)[1L]
    name <- sub("^Geom", "", name)
    name <- sub("3D$", "", name)
    mapping <- if (isTRUE(layer$inherit.aes)) plot@mapping else ggplot2::aes()
    for (aesthetic in names(layer$mapping)) mapping[[aesthetic]] <- layer$mapping[[aesthetic]]
    mapping <- mapping[setdiff(names(mapping), names(layer$aes_params))]
    variables <- vapply(mapping, rlang::as_label, character(1))
    paste0(name, if (length(variables)) paste0(" (", paste(paste(names(variables), variables, sep = " = "), collapse = ", "), ")"))
  }, character(1))
  camera <- coord$camera
  paste0("3D plot. Layers: ", if (length(layers)) paste(layers, collapse = "; ") else "empty",
    ". Camera: azimuth ", camera$azimuth, " degrees, elevation ", camera$elevation,
    " degrees, roll ", camera$roll, " degrees; ", camera$projection, " projection.")
}

#' @exportS3Method knitr::knit_print
knit_print.ggplot3d <- function(x, ..., options = knitr::opts_current$get(), inline = FALSE) {
  if (inline) return(alt_text_3d(x))
  options <- utils::modifyList(knitr::opts_chunk$get(), options)
  counter <- knitr::opts_current$get(".ggplot3d_count") %or3d% 0L
  counter <- counter + 1L
  knitr::opts_current$lock(FALSE)
  knitr::opts_current$set(.ggplot3d_count = counter)
  knitr::opts_current$lock(TRUE)
  explicit <- options$fig.alt
  if (is.language(explicit)) explicit <- eval(explicit, knitr::knit_global())
  alt <- if (length(explicit)) rep_len(explicit, counter)[counter] else NULL
  if (is.null(alt) || is.na(alt) || !nzchar(alt)) alt <- alt_text_3d(x)
  options$fig.alt <- alt
  if (isTRUE(options$view3d) && knitr::is_html_output()) {
    widget <- view_3d(x, width = round(options$fig.width * 96), height = round(options$fig.height * 96))
    result <- knitr::knit_print(widget, options = options, ...)
    result[] <- paste0('<div role="group" aria-label="', knit_escape_3d(alt), '">', result, '</div>')
    return(result)
  }
  if (identical(options$fig.keep, "none")) return(invisible(NULL))
  if (isTRUE(options$view3d)) x <- plot_backend_3d(x, backend_3d("vector"))
  device <- options$dev[[1L]]
  extension <- options$fig.ext[[1L]] %or3d% switch(device, svglite = "svg", cairo_pdf = "pdf", ragg_png = "png", device)
  path <- knitr::fig_path(extension, options = options, number = counter)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  device <- switch(device,
    svglite = { rlang::check_installed("svglite"); svglite::svglite },
    ragg_png = { rlang::check_installed("ragg"); ragg::agg_png },
    cairo_pdf = grDevices::cairo_pdf, device)
  save <- function(plot) ggplot2::ggsave(path, plot, device = device, width = options$fig.width,
    height = options$fig.height, units = "in", dpi = options$dpi,
    limitsize = FALSE)
  raster <- options$raster3d %or3d% FALSE
  if (!(isTRUE(raster) || isFALSE(raster) || (is.numeric(raster) && length(raster) == 1L && !is.na(raster) && raster >= 0)))
    stop("Chunk option raster3d must be TRUE, FALSE or a size in bytes.", call. = FALSE)
  vector <- identical(x@coordinates$config$backend$type, "vector")
  hybrid <- plot_backend_3d(x, backend_3d("hybrid", dpi = options$dpi, rasterise = "all"))
  if (vector && isTRUE(raster)) {
    save(hybrid)
  } else {
    save(x)
    if (vector && is.numeric(raster) && file.size(path) > raster) {
      drawn <- tempfile(fileext = paste0(".", extension))
      on.exit(unlink(drawn), add = TRUE)
      file.copy(path, drawn)
      save(hybrid)
      if (file.size(path) >= file.size(drawn)) file.copy(drawn, path, overwrite = TRUE)
    }
  }
  if (identical(options$fig.show, "hide")) return(invisible(NULL))
  options$fig.cur <- counter
  options$fig.num <- options$fig.num %or3d% counter
  knitr::asis_output(knitr::knit_hooks$get("plot")(path, options))
}

knit_escape_3d <- function(text) {
  for (pair in list(c("&", "&amp;"), c('"', "&quot;"), c("<", "&lt;"), c(">", "&gt;"))) {
    text <- gsub(pair[1L], pair[2L], text, fixed = TRUE)
  }
  text
}
