# Rasters as height surfaces

Draws each layer of a terra `SpatRaster` or a `stars` object as a
surface whose height and fill are the cell values. Cells are the grid
nodes at their centres; missing cells leave holes. Rasters with more
than `maxcell` cells are first resampled to a regular sample of about
`maxcell` cells, as tidyterra's `geom_spatraster()` does. Layers (or
bands) are separate surfaces grouped by the computed `lyr` variable,
ready for `facet_wrap(~lyr)`.

## Usage

``` r
geom_spatraster_3d(
  mapping = NULL,
  data,
  position = "identity",
  ...,
  maxcell = 50000,
  shade = TRUE,
  wireframe = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- data:

  A terra `SpatRaster` or a `stars` object.

- maxcell:

  Largest number of cells drawn per layer.

- shade:

  Whether to apply lighting.

- wireframe:

  Whether to draw triangle edges.

## Value

A ggplot2 layer.

## Details

The computed variables are `value` (the mean cell value of each
triangle, the default fill), `lyr`, and `z_mean`. A geographic raster
needs a projected target, `coord_3d(crs = )`; the cells keep their
lattice when reprojected.

[`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
methods turn `SpatRaster` and `stars` objects into data frames with `x`,
`y`, `value` and `lyr` columns at full resolution, and lidR `LAS` point
clouds into one row per point with `x`, `y`, `z`, `intensity`,
`classification` and the other point attributes in snake case.

## See also

[`stat_profile_3d()`](https://gillescolling.com/ggplot3d/reference/stat_profile_3d.md)
for cross-sections through the surface.

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
  r <- terra::rast(volcano[nrow(volcano):1, ], extent = terra::ext(0, 610, 0, 870))
  ggplot2::ggplot() + geom_spatraster_3d(data = r, maxcell = 2000) +
    ggplot2::scale_fill_viridis_c() + coord_3d(aspect = c(610, 870, 300))
}
if (requireNamespace("stars", quietly = TRUE)) {
  s <- stars::st_as_stars(volcano)
  ggplot2::ggplot() + geom_spatraster_3d(data = s, maxcell = 1500) + coord_3d()
}
if (requireNamespace("lidR", quietly = TRUE)) {
  las <- lidR::readLAS(system.file("extdata", "MixedConifer.laz", package = "lidR"),
    filter = "-keep_random_fraction 0.05")
  ggplot2::ggplot(ggplot2::fortify(las), ggplot2::aes(x, y, z = z, colour = z)) +
    geom_point_3d(size = 0.3) + coord_3d(aspect = c(1, 1, 0.4))
}
```
