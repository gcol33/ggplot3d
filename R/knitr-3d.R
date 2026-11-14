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
#'   `backend_3d("hybrid", rasterise = "surfaces")`, which draws the faces of
#'   every depth-tested or rear-pane layer into one image while paths,
#'   points, axes, panes, grid lines and text stay vector graphics. A number
#'   is a file size in bytes: the plot is drawn as vector graphics and, while
#'   the file exceeds that size, again with `rasterise = "surfaces"` and then
#'   with `rasterise = "all"`, which also draws paths and points into the
#'   image; the smallest file is kept. The image has the chunk's `dpi` pixels
#'   per inch, which knitr multiplies by `fig.retina` for HTML output: the
#'   density of a raster figure from the same chunk. Panels with another
#'   backend are left as they are. The same applies to patchwork compositions
#'   that contain ggplot3d plots.
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
  options <- knit_figure_options_3d(options, alt_text_3d(x))
  if (isTRUE(options$view3d) && knitr::is_html_output()) {
    widget <- view_3d(x, width = round(options$fig.width * 96), height = round(options$fig.height * 96))
    result <- knitr::knit_print(widget, options = options, ...)
    result[] <- paste0('<div role="group" aria-label="', knit_escape_3d(options$fig.alt), '">', result, '</div>')
    return(result)
  }
  if (isTRUE(options$view3d)) x <- map_plots_3d(x, function(p) plot_backend_3d(p, backend_3d("vector")))
  knit_static_3d(x, options)
}

#' @exportS3Method knitr::knit_print
knit_print.patchwork <- function(x, ..., options = knitr::opts_current$get(), inline = FALSE) {
  plots <- list()
  map_plots_3d(x, function(p) { plots[[length(plots) + 1L]] <<- p; p })
  if (inline || !length(plots)) return(NextMethod())
  options <- knit_figure_options_3d(options, paste(vapply(plots, alt_text_3d, character(1)), collapse = " "))
  knit_static_3d(x, options)
}

# Applies `f` to every ggplot3d plot of a plot or a patchwork composition,
# nested compositions included.
map_plots_3d <- function(x, f) {
  if (inherits(x, "patchwork")) {
    patches <- x$patches
    patches$plots <- lapply(patches$plots, map_plots_3d, f)
    x$patches <- patches
  }
  if (inherits(x, "ggplot3d")) x <- f(x)
  x
}

# Chunk options for the next figure of the chunk: its number, and the
# chunk's alt text for it or `alt`.
knit_figure_options_3d <- function(options, alt) {
  options <- utils::modifyList(knitr::opts_chunk$get(), options)
  counter <- (knitr::opts_current$get(".ggplot3d_count") %or3d% 0L) + 1L
  knitr::opts_current$lock(FALSE)
  knitr::opts_current$set(.ggplot3d_count = counter)
  knitr::opts_current$lock(TRUE)
  explicit <- options$fig.alt
  if (is.language(explicit)) explicit <- eval(explicit, knitr::knit_global())
  chosen <- if (length(explicit)) rep_len(explicit, counter)[counter] else NULL
  options$fig.alt <- if (is.null(chosen) || is.na(chosen) || !nzchar(chosen)) alt else chosen
  options$fig.cur <- counter
  options
}

# Writes a static figure through the chunk's device, redrawing vector panels
# with the hybrid backend as the raster3d option asks.
knit_static_3d <- function(x, options) {
  if (identical(options$fig.keep, "none")) return(invisible(NULL))
  counter <- options$fig.cur
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
  vector <- FALSE
  map_plots_3d(x, function(p) { vector <<- vector || identical(p@coordinates$config$backend$type, "vector"); p })
  hybrid <- function(rasterise) map_plots_3d(x, function(p)
    if (identical(p@coordinates$config$backend$type, "vector"))
      plot_backend_3d(p, backend_3d("hybrid", dpi = options$dpi, rasterise = rasterise)) else p)
  if (vector && isTRUE(raster)) {
    save(hybrid("surfaces"))
  } else {
    save(x)
    if (vector && is.numeric(raster)) {
      best <- tempfile(fileext = paste0(".", extension))
      on.exit(unlink(best), add = TRUE)
      file.copy(path, best)
      for (rasterise in c("surfaces", "all")) {
        if (file.size(best) <= raster) break
        save(hybrid(rasterise))
        if (file.size(path) < file.size(best)) file.copy(path, best, overwrite = TRUE)
      }
      file.copy(best, path, overwrite = TRUE)
    }
  }
  if (identical(options$fig.show, "hide")) return(invisible(NULL))
  options$fig.num <- options$fig.num %or3d% counter
  knitr::asis_output(knitr::knit_hooks$get("plot")(path, options))
}

knit_escape_3d <- function(text) {
  for (pair in list(c("&", "&amp;"), c('"', "&quot;"), c("<", "&lt;"), c(">", "&gt;"))) {
    text <- gsub(pair[1L], pair[2L], text, fixed = TRUE)
  }
  text
}
