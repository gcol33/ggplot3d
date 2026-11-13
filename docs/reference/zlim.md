# Set z scale limits

Set z scale limits

## Usage

``` r
zlim(...)
```

## Arguments

- ...:

  Two limits or one vector of limits, in original data units.

## Value

A z position scale.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 2, 1), z = 1:3)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d() + zlim(0, 4) + coord_3d()
```
