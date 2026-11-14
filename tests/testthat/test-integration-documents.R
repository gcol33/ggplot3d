document_plot <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + coord_3d(azimuth = 40, elevation = 25)
}

test_that("alt text names layers, mappings, camera and explicit descriptions", {
  p <- document_plot()
  alt <- alt_text_3d(p)
  expect_match(alt, "Point")
  expect_match(alt, "x = wt, y = mpg, z = hp", fixed = TRUE)
  expect_match(alt, "azimuth 40 degrees, elevation 25 degrees", fixed = TRUE)
  expect_identical(alt_text_3d(p + ggplot2::labs(alt = "My description")), "My description")
  expect_identical(alt_text_3d(p + ggplot2::labs(alt = function(p) "Computed description")), "Computed description")
  expect_identical(knit_print.ggplot3d(p, inline = TRUE), alt)
})

knit_document_3d <- function(p, format, chunks) {
  dir <- tempfile("document-"); dir.create(dir)
  old <- getwd(); setwd(dir)
  on.exit({setwd(old); unlink(dir, recursive = TRUE)})
  old_opts <- knitr::opts_knit$get()
  on.exit(knitr::opts_knit$restore(old_opts), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = format)
  env <- new.env(parent = globalenv()); env$p <- p
  knitr::opts_knit$set(progress = FALSE)
  text <- knitr::knit(text = chunks, quiet = TRUE, envir = env)
  list(text = paste(text, collapse = "\n"), files = list.files("figure", full.names = TRUE))
}

test_that("knitting chooses HTML viewers and static PDF and Word plots", {
  skip_if_not_installed("knitr")
  p <- document_plot()
  html <- knit_document_3d(p, "html", c("```{r, view3d=TRUE, echo=FALSE}", "p", "```"))
  expect_match(html$text, "ggplot3d_view")
  expect_match(html$text, 'aria-label="3D plot')
  expect_length(html$files, 0L)
  for (format in c("latex", "docx")) {
    out <- knit_document_3d(p, format,
      c("```{r, view3d=TRUE, echo=FALSE, dev='pdf'}", "p", "```"))
    expect_false(grepl("ggplot3d_view", out$text, fixed = TRUE))
    expect_length(out$files, 1L)
    expect_match(out$text, "figure/")
  }
})

test_that("HTML chunk dimensions round fractional pixels at the knitr boundary", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("svglite")
  sizes <- NULL
  view <- view_3d
  local_mocked_bindings(view_3d = function(plot, width, height) {
    sizes <<- c(width, height)
    view(plot, width = width, height = height)
  })
  out <- knit_document_3d(document_plot(), "html", c(
    "```{r, view3d=TRUE, echo=FALSE, error=FALSE, fig.width=6.5, fig.asp=0.6180469715698392}",
    "p", "```"))
  expect_equal(sizes, c(624, 386))
  expect_match(out$text, "ggplot3d_view")
  out <- knit_document_3d(document_plot(), "html", c(
    "```{r, view3d=TRUE, echo=FALSE, error=FALSE, fig.width=6.501, fig.height=4.499}",
    "p", "```"))
  expect_equal(sizes, c(624, 432))
  expect_match(out$text, "ggplot3d_view")
  expect_error(view(document_plot(), width = 624, height = 385.6613102595796),
    "positive integer pixels")
})

test_that("figure alt defaults and overrides are independent within a chunk", {
  skip_if_not_installed("knitr")
  out <- knit_document_3d(document_plot(), "html", c(
    "```{r, echo=FALSE, fig.alt=c('first custom', 'second custom')}", "p", "p", "```"))
  expect_match(out$text, "first custom")
  expect_match(out$text, "second custom")
  expect_length(out$files, 2L)
  out <- knit_document_3d(document_plot(), "html", c("```{r, echo=FALSE}", "p", "```"))
  expect_match(out$text, "3D plot")
})

test_that("patchwork and cowplot preserve separate scenes through drawing", {
  p <- document_plot()
  q <- p + ggplot2::facet_wrap(~am)
  grDevices::pdf(tempfile(fileext = ".pdf"), width = 8, height = 4)
  on.exit(grDevices::dev.off())
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- patchwork::patchworkGrob(patchwork::wrap_plots(p, q))
    expect_length(motion_panels_3d(combined), 3L)
    forced <- grid::grid.force(combined)
    panels <- motion_panels_3d(forced)
    expect_length(panels, 3L)
    expect_true(all(vapply(panels, function(panel) inherits(panel$scene, "ggplot3d_compiled_scene"), logical(1))))
  }
  skip_if_not_installed("cowplot")
  combined <- ggplot2::ggplotGrob(cowplot::plot_grid(p, q))
  expect_length(motion_panels_3d(combined), 3L)
  panels <- motion_panels_3d(grid::grid.force(combined))
  expect_length(panels, 3L)
  expect_true(all(vapply(panels, function(panel) inherits(panel$scene, "ggplot3d_compiled_scene"), logical(1))))
})
