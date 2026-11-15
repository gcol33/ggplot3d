docs_setup <- function(legend = "none", ...) {
  knitr::opts_chunk$set(
    collapse = TRUE, comment = "#>", fig.width = 6.5, fig.height = 4.5,
    dpi = 120, out.width = "100%", raster3d = 1e5, dev = "svglite",
    fig.ext = "svg", dev.args = list(pointsize = 14)
  )
  knitr::opts_chunk$set(...)
  clear <- ggplot2::element_rect(fill = "transparent", colour = NA)
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 17) + ggplot2::theme(
    plot.background = clear, panel.background = clear,
    legend.background = clear, legend.key = clear,
    legend.box.background = clear, legend.position = legend
  ))
  invisible()
}

docs_palette <- list(
  light = c(paper = "#F5F6F8", ink = "#3E3F3A", rule = "#DFD7CA"),
  dark = c(paper = "#343739", ink = "#DFD7CA", rule = "#495057")
)

# Runs a chunk shown with eval = FALSE once per site theme and embeds both
# animations; the page stylesheet shows the one matching the active theme.
docs_animation <- function(chunk) {
  code <- parse(text = knitr::knit_code$get(chunk))
  env <- knitr::knit_global()
  base <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(base), add = TRUE)
  html <- vapply(names(docs_palette), function(mode) {
    colours <- docs_palette[[mode]]
    ggplot2::theme_set(base + ggplot2::theme(
      text = ggplot2::element_text(colour = colours[["ink"]]),
      axis.text = ggplot2::element_text(colour = colours[["ink"]]),
      panel.grid = ggplot2::element_line(colour = colours[["rule"]]),
      plot.background = ggplot2::element_rect(fill = colours[["paper"]], colour = NA)
    ))
    out <- knitr::knit_print(eval(code, envir = env))
    sub("<(img|video) ", sprintf('<\\1 class="docs-animation docs-animation-%s" loading="lazy" ', mode), out)
  }, character(1))
  knitr::asis_output(paste(html, collapse = ""))
}
