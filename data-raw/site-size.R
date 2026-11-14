# Page and figure weight of the built pkgdown site.
#
# Every HTML page under the site directory is measured as a browser loads it:
# the page itself, which carries the inlined SVG figures, plus the local
# images it still links. Each figure is listed with its size, and for an SVG
# the share taken by embedded raster images. The script fails when a page or a
# figure exceeds its budget. The gzip column is the page as a server that
# compresses HTML sends it; the budgets apply to the uncompressed size, which
# the browser parses.
#
# Usage, from the package root after a site build:
#   Rscript data-raw/site-size.R [site_dir] [page_budget_kb] [figure_budget_kb]
# Defaults: docs, 2000 and 600.

site_size_figures <- function(html_path) {
  html <- readChar(html_path, file.size(html_path), useBytes = TRUE)
  open <- gregexpr("<svg[^>]*inline-svg[^>]*>", html, perl = TRUE)[[1]]
  open <- open[open > 0]
  close <- gregexpr("</svg>", html, fixed = TRUE)[[1]]
  inline <- vapply(open, function(start) {
    end <- close[close > start][1L]
    substr(html, start, end + 5L)
  }, character(1))
  inline_label <- vapply(open, function(start) {
    head <- substr(html, start, start + 4000L)
    tag <- regmatches(head, regexpr("<svg[^>]*>", head))
    label <- sub(".*aria-label=\"([^\"]*)\".*", "\\1", tag)
    if (identical(label, tag)) "" else substr(label, 1L, 40L)
  }, character(1))
  images <- regmatches(html, gregexpr("<img[^>]*src=\"[^\"]+\"[^>]*>", html, perl = TRUE))[[1]]
  images <- images[!grepl("class=\"[^\"]*\\blogo\\b", images, perl = TRUE)]
  src <- sub(".*src=\"([^\"]+)\".*", "\\1", images)
  src <- src[!grepl("^(https?:|data:|//)", src)]
  linked <- normalizePath(file.path(dirname(html_path), src), winslash = "/", mustWork = FALSE)
  linked <- unique(linked[file.exists(linked)])
  embedded <- function(svg) {
    starts <- gregexpr("data:image/[a-z]+;base64,", svg, perl = TRUE)[[1]]
    if (starts[1L] < 0) return(0)
    quotes <- gregexpr("['\"]", svg, perl = TRUE)[[1]]
    sum(vapply(starts, function(s) quotes[quotes > s][1L] - s, numeric(1)))
  }
  linked_text <- vapply(linked, function(p) {
    if (grepl("\\.svg$", p)) readChar(p, file.size(p), useBytes = TRUE) else ""
  }, character(1))
  data.frame(
    figure = c(if (length(inline)) paste0("#", seq_along(inline), " ", inline_label), basename(linked)),
    bytes = c(nchar(inline, type = "bytes"), file.size(linked)),
    raster_bytes = c(vapply(inline, embedded, numeric(1)), vapply(linked_text, embedded, numeric(1))),
    linked = c(rep(FALSE, length(inline)), rep(TRUE, length(linked))),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

site_size_report <- function(site_dir = "docs", page_budget_kb = 2000, figure_budget_kb = 600) {
  pages <- list.files(site_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (!length(pages)) stop("No HTML pages under ", site_dir, call. = FALSE)
  kb <- function(b) round(b / 1024)
  rows <- lapply(pages, function(p) {
    figs <- site_size_figures(p)
    list(page = sub(paste0("^", site_dir, "/?"), "", p),
      html = file.size(p), load = file.size(p) + sum(figs$bytes[figs$linked]),
      gzip = length(memCompress(readBin(p, "raw", file.size(p)), "gzip")), figures = figs)
  })
  summary <- data.frame(
    page = vapply(rows, `[[`, "", "page"),
    html_kb = kb(vapply(rows, `[[`, 0, "html")),
    load_kb = kb(vapply(rows, `[[`, 0, "load")),
    html_gzip_kb = kb(vapply(rows, `[[`, 0, "gzip")),
    figures = vapply(rows, function(r) nrow(r$figures), 0L),
    largest_figure_kb = kb(vapply(rows, function(r) max(c(0, r$figures$bytes)), 0)),
    stringsAsFactors = FALSE
  )
  summary <- summary[order(-summary$load_kb), ]
  print(summary[summary$figures > 0 | summary$load_kb > 200, ], row.names = FALSE)

  over_page <- summary$page[summary$load_kb > page_budget_kb]
  over_fig <- do.call(rbind, lapply(rows, function(r) {
    f <- r$figures[r$figures$bytes > figure_budget_kb * 1024, , drop = FALSE]
    if (nrow(f)) data.frame(page = r$page, figure = f$figure, kb = kb(f$bytes),
      raster_kb = kb(f$raster_bytes), stringsAsFactors = FALSE)
  }))
  if (length(over_page)) {
    cat(sprintf("\nPages over %d KB:\n", page_budget_kb))
    cat(paste0("  ", over_page, collapse = "\n"), "\n")
  }
  if (!is.null(over_fig) && nrow(over_fig)) {
    cat(sprintf("\nFigures over %d KB:\n", figure_budget_kb))
    print(over_fig, row.names = FALSE)
  }
  invisible(list(summary = summary, ok = !length(over_page) && (is.null(over_fig) || !nrow(over_fig))))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  site_dir <- if (length(args) >= 1L) args[[1L]] else "docs"
  page_budget <- if (length(args) >= 2L) as.numeric(args[[2L]]) else 2000
  figure_budget <- if (length(args) >= 3L) as.numeric(args[[3L]]) else 600
  result <- site_size_report(site_dir, page_budget, figure_budget)
  if (!result$ok) quit(status = 1L)
  cat("\nAll pages and figures within budget.\n")
}
