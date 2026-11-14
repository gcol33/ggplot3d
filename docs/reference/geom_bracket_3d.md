# Brackets and dimension lines between two points

Marks the span from (`x`, `y`, `z`) to (`xend`, `yend`, `zend`) with a
line set off to one side and an optional label at its middle.
`type = "bracket"` draws a bracket whose tips point back at the two
ends, as
[`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html)
and
[`ggpubr::geom_bracket()`](https://rpkgs.datanovia.com/ggpubr/reference/geom_bracket.html)
mark compared groups. `type = "dimension"` draws a dimension line as in
technical drawing: extension lines from the two ends, and a line with
arrowheads between them.

## Usage

``` r
geom_bracket_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  type = c("bracket", "dimension"),
  offset = c(0, 0, 0.05),
  tip_length = 0.02,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments. `...` takes fixed aesthetics and the 3D
  drawing parameters, and `arrow` replaces the arrowheads of dimension
  lines.

- type:

  `"bracket"` or `"dimension"`.

- offset:

  Displacement of the line from the two ends, a vector `c(x, y, z)` in
  fractions of the panel box.

- tip_length:

  Length of the bracket tips, and of the extension lines beyond the
  dimension line, in fractions of the panel box. Tips never reach past
  the two ends.

## Value

A ggplot2 layer.

## Details

The offset is in units of the panel box, so `offset = c(0, 0, 0.05)`
lifts the line by 5% of the box height whatever the z scale. Without a
`zend` mapping the span is level at `z`. The label is centred a little
past the middle of the line, on the side of the offset: `tip_length`
plus 3% of the box. Like other data, parts outside the panel box are
clipped; dimension lines drawn outside an object at the edge of the box
need `coord_3d(clip_data = FALSE)`.

## Aesthetics

`x`, `y`, `xend` and `yend` are required; `z`, `zend`, `label`,
`colour`, `linewidth`, `linetype`, `alpha` and the text aesthetics
`size`, `family`, `fontface`, `lineheight`, `hjust`, `vjust` and `angle`
are optional.

## Examples

``` r
cols <- data.frame(x = 1:3, y = 1, z = c(4, 7, 5))
pair <- data.frame(x = 1, xend = 2, y = 1, yend = 1, z = 7.5, label = "p = 0.01")
ggplot2::ggplot(cols, ggplot2::aes(x, y, z = z)) +
  geom_col_3d() +
  geom_bracket_3d(ggplot2::aes(xend = xend, yend = yend, label = label), data = pair) +
  coord_3d()

box <- data.frame(x = 0, y = 0, z = 0, xend = 4, yend = 0, zend = 0, label = "4 m")
ggplot2::ggplot(box, ggplot2::aes(x, y, z = z, xend = xend, yend = yend,
                                zend = zend, label = label)) +
  geom_bracket_3d(type = "dimension", offset = c(0, -0.08, 0)) +
  coord_3d(clip_data = FALSE)
```
