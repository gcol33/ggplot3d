# Draw a three-dimensional ribbon or wall

Draw a three-dimensional ribbon or wall

## Usage

``` r
geom_band_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

Map x/y and zmin/zmax for a vertical wall, or map both x/y/z and
xend/yend/zend boundaries for a general ruled band. Input order is
retained within groups. Missing endpoints break the band.

## Examples

``` r
d <- data.frame(x = seq(0, 6, length.out = 30), y = 0)
d$low <- sin(d$x); d$high <- d$low + 0.5
ggplot2::ggplot(d, ggplot2::aes(x, y, zmin = low, zmax = high)) +
  geom_band_3d(fill = "steelblue") + coord_3d()
```
