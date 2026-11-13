# Nudge all three position families

Nudge all three position families

## Usage

``` r
position_nudge_3d(x = 0, y = 0, z = 0, nudge_z = NULL)
```

## Arguments

- x, y, z:

  Displacements in transformed coordinate units.

- nudge_z:

  Optional spelling of the z displacement.

## Value

A ggplot2 position adjustment.

## Examples

``` r
ggplot2::ggplot(data.frame(x = 1, y = 1, z = 1), ggplot2::aes(x, y, z = z)) +
  geom_point_3d(position = position_nudge_3d(nudge_z = 0.5)) + coord_3d()
```
