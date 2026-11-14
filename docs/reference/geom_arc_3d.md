# Flow arcs between two points

Draws a parabolic arc from (`x`, `y`, `z`) to (`xend`, `yend`, `zend`),
the flow-map arcs of deck.gl's `ArcLayer`. Arcs rise above their chord
by `height` times the chord's horizontal length as drawn in the box
(with the `aspect` of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)),
so long flows arch higher than short ones and the shape does not depend
on the x/y units. Arcs that rise above the box are clipped with the data
unless `coord_3d(clip_data = FALSE)`. Without z, arcs start and end on
the floor of the box, over a map drawn with
[`geom_sf_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sf_3d.md).
`height` can also be mapped.

## Usage

``` r
geom_arc_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  height = 0.25,
  n = 32L,
  arrow = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- height:

  Peak rise as a fraction of the horizontal chord length. Setting it
  overrides a mapped `height`.

- n:

  Number of straight pieces per arc.

- arrow:

  Arrow specification from
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

## Value

A ggplot2 layer.

## Examples

``` r
flows <- data.frame(x = 0, y = 0, xend = c(4, -3, 1), yend = c(1, 2, -4),
  volume = c(3, 1, 2))
ggplot2::ggplot(flows, ggplot2::aes(x, y, xend = xend, yend = yend)) +
  geom_arc_3d(ggplot2::aes(colour = volume, linewidth = volume)) +
  coord_3d()
```
