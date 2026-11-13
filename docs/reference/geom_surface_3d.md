# Triangulated surfaces in three dimensions

Grid surfaces estimate vertex normals from height differences along each
axis. Lighting uses the normalised mean of each face's vertex normals.

## Usage

``` r
geom_surface_3d(
  mapping = NULL,
  data = NULL,
  stat = StatSurface3D,
  position = "identity",
  ...,
  method = "grid",
  max_edge = Inf,
  shade = TRUE,
  wireframe = FALSE,
  hidden = "hide",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_surface_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomSurface3D,
  position = "identity",
  ...,
  method = "grid",
  max_edge = Inf,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- method:

  Grid connectivity or Delaunay triangulation.

- max_edge:

  Maximum triangle edge length in transformed x/y units.

- shade:

  Whether to apply lighting.

- wireframe:

  Whether to draw triangle edges.

- hidden:

  Hidden stroke policy.

- geom:

  Geom prototype used to draw the computed triangles.

## Value

A ggplot2 layer.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 6),
                 y = seq(-1, 1, length.out = 6))
d$z <- with(d, x^2 + y^2)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_surface_3d(fill = "steelblue") + coord_3d()
p + stat_surface_3d() + coord_3d()
```
