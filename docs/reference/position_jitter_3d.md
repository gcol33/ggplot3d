# Jitter points in three dimensions

Adds uniform random noise to the x, y and z positions of each row. Every
column of one position family (for example `z`, `zmin`, `zend`) moves by
the same amount, so segments and columns keep their shape.

## Usage

``` r
position_jitter_3d(width = NULL, depth = NULL, height = NULL, seed = NA)
```

## Arguments

- width, depth, height:

  Amount of jitter along x, y and z in transformed units: positions move
  by up to this amount in either direction. Defaults to 40% of the
  resolution of the data along that axis, as in
  [`ggplot2::position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).

- seed:

  Random seed. `NA` (default) draws one seed when the position is
  created, so the jitter is the same each time the plot is drawn; `NULL`
  uses the current random state.

## Value

A ggplot2 position adjustment.

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:3, rep = 1:10)
d$z <- d$x + d$y
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d(position = position_jitter_3d(seed = 1)) + coord_3d()
```
