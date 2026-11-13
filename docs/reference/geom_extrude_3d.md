# Extrude polygon rings along z

Extrude polygon rings along z

## Usage

``` r
geom_extrude_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  base = 0,
  cap = "both",
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- base:

  Baseline in original z units, overridden by zmin.

- cap:

  Caps to draw: both, top, bottom or none.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

Input is ordered x/y rings with group/subgroup, or projected sf
POLYGON/MULTIPOLYGON features. Map z as the endpoint or height as an
offset from zmin. Holes retain their interior walls and open cap
interiors.

## Examples

``` r
d <- data.frame(x = c(0, 2, 2, 0), y = c(0, 0, 1, 1), z = 2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_extrude_3d(colour = "grey30") + coord_3d()
```
