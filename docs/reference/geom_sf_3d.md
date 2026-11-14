# Simple features in three dimensions

Draws sf data inside
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md):
points as markers, lines as paths and polygons as flat faces, each
feature keeping its attributes for mapping. Geometries with a Z
dimension stand at their own heights; other features lie on the floor of
the box unless `z` is mapped or set. Mapping `height` (or `zmax`) turns
polygons into prisms from `base` or `zmin`, which makes a prism map, and
lines into walls filled with their colour unless `fill` is set. Polygons
must be planar.

## Usage

``` r
geom_sf_3d(
  mapping = NULL,
  data = NULL,
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

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- base:

  Prism baseline in original z units, overridden by zmin.

- cap:

  Prism caps to draw: both, top, bottom or none.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
layers are drawn the same way; add
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
after them, because
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) brings
its own default coordinate system. Geographic coordinates require a
projected target, `coord_3d(crs = )`, which also reprojects data with
other coordinate systems.

## See also

[`geom_extrude_3d()`](https://gillescolling.com/ggplot3d/reference/geom_extrude_3d.md)
for polygons given as x/y rings,
[`geom_spatraster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_spatraster_3d.md)
for rasters,
[`geom_arc_3d()`](https://gillescolling.com/ggplot3d/reference/geom_arc_3d.md)
for flows.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggplot2::ggplot(nc) +
    geom_sf_3d(ggplot2::aes(height = BIR74 / 1000, fill = BIR74), colour = "grey30") +
    coord_3d(crs = 32119, aspect = c(4, 1.6, 1), elevation = 40)
  ggplot2::ggplot(nc[1:20, ]) + ggplot2::geom_sf(fill = "grey90") +
    coord_3d(crs = 32119, aspect = c(2, 1, 0.3))
}
```
