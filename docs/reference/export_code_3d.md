# Export TikZ paths or an Asymptote scene

TikZ writes the complete SVG paint order as editable fill, draw and text
commands, including clip paths, furniture and legends. Requires TikZ's
`svg.path` library. Raster colourbars and annotations become coloured
pixel rectangles using the optional png decoder. TeX selects the text
font. Asymptote writes model-space triangles, paths, marker solids, text
and a camera, so it can re-render the scene from another view or produce
an interactive PRC PDF. For a PRC PDF, compile with
`asy -tex pdflatex -f pdf plot.asy` and install the TeX packages media9
and ocgx2. A PDF reader with PRC support supplies the interactive view.

## Usage

``` r
export_code_3d(
  plot,
  format = c("tikz", "asy"),
  file = NULL,
  width = 640L,
  height = 640L,
  panel = 1L,
  standalone = TRUE,
  prc = FALSE
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- format:

  `"tikz"` or `"asy"`.

- file:

  Optional output path; NULL returns the code without writing.

- width, height:

  Plot size in pixels at 96 pixels per inch.

- panel:

  Panel index for the Asymptote scene; TikZ includes every panel.

- standalone:

  Whether TikZ includes a LaTeX document preamble.

- prc:

  Whether Asymptote should include PRC in its PDF output.

## Value

One character string of source code, invisibly when `file` is given.

## Examples

``` r
p <- ggplot2::ggplot(data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3)),
  ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d()
code <- export_code_3d(p, "asy")
if (requireNamespace("xml2", quietly = TRUE) && requireNamespace("svglite", quietly = TRUE)) {
  code <- export_code_3d(p, "tikz")
}
```
