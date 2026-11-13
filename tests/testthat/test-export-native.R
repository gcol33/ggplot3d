native_plot <- function() {
  d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3), label = c("<one>&\"", "two", "three"))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, tooltip = label)) + geom_point_3d() +
    geom_path_3d() + coord_3d()
}

test_that("the shared base64 codec preserves binary geometry and embedded images", {
  bytes <- as.raw(0:255)
  expect_identical(base64_decode_3d(base64_3d(bytes)), bytes)
  expect_identical(base64_decode_3d("aGVsbG8="), charToRaw("hello"))
  expect_identical(base64_decode_3d(""), raw())
  expect_error(base64_decode_3d("a=b="), "padding")
})

test_that("native SVG preserves source identities tooltips and whole plot furniture", {
  skip_if_not_installed("xml2");skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg");on.exit(unlink(file))
  p <- native_plot() + ggplot2::labs(title = "Plot <title>") + ggplot2::facet_wrap(~label)
  save_svg_3d(p, file, width = 720, height = 360)
  doc <- xml2::read_xml(file)
  ids <- xml2::xml_attr(xml2::xml_find_all(doc, ".//*[@id]"), "id")
  expect_false(anyDuplicated(ids) > 0)
  expect_length(xml2::xml_find_all(doc, ".//*[@data-panel]"), 3L)
  primitives <- xml2::xml_find_all(doc, ".//*[@class='gg3d-primitive']")
  expect_gt(length(primitives), 0L)
  expect_true(all(!is.na(xml2::xml_attr(primitives, "data-layer"))))
  expect_true(all(!is.na(xml2::xml_attr(primitives, "data-row"))))
  titles <- xml2::xml_text(xml2::xml_find_all(doc, ".//*[local-name()='title']"))
  expect_true("<one>&\"" %in% titles)
  expect_true(any(grepl("Plot <title>", xml2::xml_text(doc), fixed = TRUE)))
  static <- xml2::read_xml(native_svg_3d(native_plot(), 400, 300, FALSE))
  expect_length(xml2::xml_find_all(static, ".//*[local-name()='script']"), 0L)
})

test_that("native SVG writes glyph masks and compound surface seam paths", {
  skip_if_not_installed("xml2");skip_if_not_installed("svglite")
  p <- ggplot2::ggplot(data.frame(x = .5, y = .5, z = 0), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(size = 12) + annotate_3d("rect", xmin = .5, xmax = 1, ymin = 0, ymax = 1, z = .5, fill = "grey60") +
    coord_3d(azimuth = -90, elevation = 90, xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), expand = FALSE)
  doc <- xml2::read_xml(native_svg_3d(p, 400, 400, TRUE))
  expect_gt(length(xml2::xml_find_all(doc, ".//*[local-name()='clipPath']")), 1L)
  d <- expand.grid(x = 0:2, y = 0:2); d$z <- d$x * d$y / 4
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d(alpha = .5) + coord_3d()
  doc <- xml2::read_xml(native_svg_3d(p, 400, 400, FALSE))
  expect_gt(length(xml2::xml_find_all(doc, ".//*[local-name()='mask']")), 0L)
  paths <- xml2::xml_attr(xml2::xml_find_all(doc, ".//*[local-name()='mask']/*[local-name()='path']"), "d")
  expect_true(any(vapply(strsplit(paths, "M", fixed = TRUE), length, integer(1)) > 2L))
  if (requireNamespace("png", quietly = TRUE)) {
    code <- export_code_3d(p, "tikz", standalone = FALSE, width = 400, height = 400)
    expect_match(code, "fill opacity=", fixed = TRUE)
    expect_false(grepl("NApt", code, fixed = TRUE))
  }
})

test_that("native SVG hover links actual browser primitives", {
  skip_on_cran();skip_if_not_installed("chromote");skip_if_not_installed("xml2");skip_if_not_installed("svglite")
  if (!nzchar(chromote::find_chrome() %or3d% "")) skip("Chrome is unavailable")
  file <- tempfile(fileext = ".svg");on.exit(unlink(file))
  save_svg_3d(native_plot(), file, width = 400, height = 400)
  browser <- chromote::ChromoteSession$new(width = 500, height = 500)
  on.exit(browser$close(), add = TRUE)
  browser$Page$navigate(paste0("file:///", normalizePath(file, winslash = "/")))
  ready <- browser$Runtime$evaluate("document.readyState", returnByValue = TRUE)$result$value
  for (i in seq_len(50L)) {
    count <- browser$Runtime$evaluate("document.querySelectorAll('.gg3d-primitive').length", returnByValue = TRUE)$result$value
    if (count > 0) break
    Sys.sleep(.05)
  }
  expect_gt(count, 0)
  out <- browser$Runtime$evaluate(paste0("(function(){var p=document.querySelector('.gg3d-primitive');",
    "p.dispatchEvent(new PointerEvent('pointerover',{bubbles:true}));",
    "return document.querySelectorAll('.gg3d-hover').length;})()"), returnByValue = TRUE)$result$value
  expect_gt(out, 0)
})

test_that("code exports write editable 2D paths and a camera-bearing 3D scene", {
  skip_if_not_installed("xml2");skip_if_not_installed("svglite")
  p <- native_plot()
  tikz <- export_code_3d(p, "tikz", standalone = FALSE, width = 400, height = 300)
  expect_match(tikz, "\\begin{tikzpicture}", fixed = TRUE)
  expect_match(tikz, "\\draw[", fixed = TRUE)
  expect_match(tikz, "\\fill[", fixed = TRUE)
  expect_match(tikz, "\\clip", fixed = TRUE)
  asy <- export_code_3d(p, "asy", prc = TRUE)
  expect_match(asy, "settings.prc=true;", fixed = TRUE)
  expect_match(asy, "currentprojection=orthographic", fixed = TRUE)
  expect_match(asy, "draw(ggplotModel,surfacepen=ggplotMaterials", fixed = TRUE)
  perspective <- suppressMessages(p + coord_3d(projection = "perspective"))
  expect_match(export_code_3d(perspective, "asy"), "autoadjust=false,angle=", fixed = TRUE)
  expect_identical(tex_escape_3d("a_b & c%"), "a\\_b \\& c\\%")
})

test_that("generated TikZ compiles with pdfLaTeX", {
  skip_on_cran();skip_if_not_installed("xml2");skip_if_not_installed("svglite")
  compiler <- Sys.which("pdflatex")
  skip_if(!nzchar(compiler), "pdfLaTeX is unavailable")
  dir <- tempfile("tikz-");dir.create(dir)
  old <- getwd();setwd(dir);on.exit({setwd(old);unlink(dir, recursive = TRUE)})
  export_code_3d(native_plot(), "tikz", file = "plot.tex", width = 400, height = 300)
  status <- system2(compiler, c("-interaction=nonstopmode", "-halt-on-error", "plot.tex"), stdout = "compile.log", stderr = "compile.log")
  expect_equal(status, 0L, info = paste(tail(readLines("compile.log", warn = FALSE), 12L), collapse = "\n"))
  expect_true(file.exists("plot.pdf"))
})

test_that("generated Asymptote compiles with its renderer", {
  skip_on_cran()
  compiler <- Sys.getenv("GGPLOT3D_TEST_ASY", Sys.which("asy"))
  skip_if(!nzchar(compiler), "Asymptote is unavailable")
  dir <- tempfile("asy-");dir.create(dir)
  old <- getwd();setwd(dir);on.exit({setwd(old);unlink(dir, recursive = TRUE)})
  export_code_3d(native_plot(), "asy", file = "plot.asy", width = 400, height = 300)
  arguments <- c("-noV", "-f", "pdf", "-render=0")
  source <- Sys.getenv("GGPLOT3D_TEST_ASY_DIR")
  gs <- Sys.getenv("GGPLOT3D_TEST_GS")
  if (nzchar(source)) arguments <- c(arguments, "-dir", shQuote(source))
  if (nzchar(gs)) arguments <- c(arguments, "-gs", shQuote(gs))
  status <- system2(compiler, c(arguments, "plot.asy"), stdout = "compile.log", stderr = "compile.log")
  expect_equal(status, 0L, info = paste(tail(readLines("compile.log", warn = FALSE), 12L), collapse = "\n"))
  expect_true(file.exists("plot.pdf"))
})
