# Adapt a registered ggplot2 layer to three dimensions

Adapt a registered ggplot2 layer to three dimensions

## Usage

``` r
layer_3d(
  layer,
  mapping = NULL,
  ...,
  occlusion = "depth",
  shade = FALSE,
  hidden = "hide"
)
```

## Arguments

- layer:

  A ggplot2 layer.

- mapping:

  Additional aesthetic mappings.

- ...:

  Fixed aesthetics and rendering parameters.

- occlusion:

  Depth testing or overlay drawing.

- shade:

  Whether to shade faces.

- hidden:

  Hidden stroke policy.

## Value

A ggplot2 layer adapted to the 3D scene.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  layer_3d(ggplot2::geom_point(size = 3)) + coord_3d()
```
