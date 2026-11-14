test_that("animation URLs resolve against output directories", {
  root <- tempfile("animation-paths-")
  on.exit(unlink(root, recursive = TRUE))
  for (folder in c("vignettes", "docs/articles/figure/nested"))
    dir.create(file.path(root, folder), recursive = TRUE)
  path <- file.path(root, "docs/articles/figure/nested/wave & crest.mp4")
  writeBin(as.raw(1:4), path)
  expect_identical(knit_relative_media_3d(path, file.path(root, "docs/articles")),
    "figure/nested/wave%20%26%20crest.mp4")
  expect_identical(knit_relative_media_3d(path, file.path(root, "vignettes")),
    "../docs/articles/figure/nested/wave%20%26%20crest.mp4")
  old <- setwd(file.path(root, "vignettes"))
  on.exit(setwd(old), add = TRUE)
  expect_identical(knit_relative_media_3d("../docs/articles/figure/nested/wave & crest.mp4",
    "../docs/articles"), "figure/nested/wave%20%26%20crest.mp4")
})

test_that("knitr embeds animation files with chunk size and alt text", {
  skip_if_not_installed("knitr")
  root <- tempfile("animation-knit-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = globalenv())
  for (extension in c("gif", "mp4", "webm")) {
    path <- file.path(root, paste0("source.", extension))
    writeBin(as.raw(1:8), path)
    env$movie <- structure(list(file = path, width = 480, height = 480), class = "ggplot3d_animation")
    text <- c('```{r motion, echo=FALSE, fig.alt="Waves <rise> & fall", out.width="75%"}',
      'movie', 'movie', '```')
    result <- knitr::knit(text = text, envir = env, quiet = TRUE)
    tag <- if (extension == "gif") "img" else "video"
    expect_match(result, paste0("<", tag, " src=\"figure/motion-1\\.", extension))
    expect_true(file.exists(paste0("figure/motion-2.", extension)))
    expect_identical(readBin(paste0("figure/motion-1.", extension), "raw", 100), as.raw(1:8))
    expect_match(result, "Waves &lt;rise&gt; &amp; fall", fixed = TRUE)
    expect_match(result, "width: 75%; max-width: min(100%, 480px); height: auto;", fixed = TRUE)
    if (extension != "gif") {
      expect_match(result, "autoplay loop muted playsinline", fixed = TRUE)
      expect_match(result, 'aria-label="Waves &lt;rise&gt; &amp; fall"', fixed = TRUE)
    }
  }
})

test_that("rmarkdown animation URLs resolve from the final output directory", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("xml2")
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("animation-render-")
  dir.create(file.path(root, "input"), recursive = TRUE)
  dir.create(file.path(root, "output"))
  on.exit(unlink(root, recursive = TRUE))
  gif <- file.path(root, "input/source.gif")
  writeBin(as.raw(c(71, 73, 70, 56, 57, 97)), gif)
  env <- new.env(parent = globalenv())
  env$movie <- structure(list(file = gif, width = 240, height = 240), class = "ggplot3d_animation")
  input <- file.path(root, "input/movie.Rmd")
  writeLines(c('---', 'title: "Animation"', 'vignette: >',
    '  %\\VignetteIndexEntry{Animation}', '  %\\VignetteEngine{knitr::rmarkdown}',
    '  %\\VignetteEncoding{UTF-8}', '---',
    '```{r media, echo=FALSE, fig.alt="Moving surface", out.width=120}',
    'movie', '```'), input)
  for (format in list(rmarkdown::html_document(self_contained = FALSE), rmarkdown::html_vignette())) {
    output <- rmarkdown::render(input, output_format = format,
      output_dir = file.path(root, "output"), envir = env, quiet = TRUE)
    html <- paste(readLines(output, warn = FALSE), collapse = "\n")
    image <- xml2::xml_find_first(xml2::read_html(html), '//img[@alt="Moving surface"]')
    src <- xml2::xml_attr(image, "src")
    if (startsWith(src, "data:")) {
      expect_match(src, "^data:image/gif;base64,")
    } else {
      expect_true(file.exists(file.path(dirname(output), utils::URLdecode(src))))
    }
    expect_false(grepl(normalizePath(root, winslash = "/"), src, fixed = TRUE))
    expect_match(html, "width: 120px", fixed = TRUE)
  }
})

test_that("format-only animations embed from knitted calls", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("ragg")
  root <- tempfile("animation-format-knit-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = globalenv())
  env$p <- ggplot2::ggplot(mtcars[1:3, ], ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d() + coord_3d()
  for (format in c("apng", "gif", "mp4", "webm")) {
    if (any(!vapply(motion_outputs_3d[[format]]$packages, requireNamespace, logical(1), quietly = TRUE))) next
    text <- c('```{r motion, echo=FALSE, fig.alt="Moving points"}',
      sprintf('movie <- animate_3d(p, frames = 2, format = "%s", width = 160, height = 160, progress = FALSE)', format),
      'movie', '```')
    result <- knitr::knit(text = text, envir = env, quiet = TRUE)
    tag <- if (format %in% c("apng", "gif")) "img" else "video"
    expect_match(result, paste0('<', tag, ' src="figure/motion-1.', format, '"'), fixed = TRUE)
    destination <- paste0("figure/motion-1.", format)
    expect_true(file.exists(destination))
    expect_identical(unname(tools::md5sum(destination)), unname(tools::md5sum(env$movie$file)))
    expect_match(result, "Moving points", fixed = TRUE)
    unlink(env$movie$file)
  }
})
