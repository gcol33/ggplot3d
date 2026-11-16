# Get started

ggplot3d adds three-dimensional layers and a z position scale to
[ggplot2](https://ggplot2.tidyverse.org/), the R package for building
plots from data and aesthetic mappings. This tutorial assumes basic R
and data frames. It ends with a surface, observations and a saved plot.

1.  **Map** positions with
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

2.  **Draw** a surface with
    [`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md).

3.  **Save** the plot with
    [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Installation

``` r

# install.packages("pak")
pak::pak("gcol33/ggplot3d")
```

Source installation needs a C++17 compiler. The [README installation
section](https://github.com/gcol33/ggplot3d#installation) lists the
tools for each operating system.

## Plotting observations

Each row records three positions in arbitrary units. Map the column
names to x, y and z. The call returns a ggplot object that can be
printed, extended and saved.

``` r

library(ggplot2)
library(ggplot3d)
```

``` r

observations <- data.frame(id = c("a", "b", "c"),
  east = c(-1, 0, 1), north = c(0, 1, -1), height = c(0.4, 1.2, 0.8))
p <- ggplot(observations, aes(east, north, z = height)) +
  geom_point_3d(size = 3, colour = "#B5342B") + coord_3d()
p
```

![Three observations positioned along x, y and z
axes.](ggplot3d_files/figure-html/points-1.svg)

The z axis labels the height column. The camera projects these three
positions onto the plotting page. Change its angles in [Choosing camera
views](https://gillescolling.com/ggplot3d/articles/camera.md).

## Adding a surface

The surface data contain one row for every pair of grid positions. Its
heights describe a curved surface in the same units as the observations.
Both layers contribute geometry to the panel scene, the collection used
to resolve which parts are visible.

``` r

surface <- expand.grid(east = seq(-1.5, 1.5, length.out = 12),
                       north = seq(-1.5, 1.5, length.out = 12))
surface$height <- with(surface, exp(-(east^2 + north^2)))
p <- ggplot(surface, aes(east, north, z = height)) +
  geom_surface_3d(fill = "#2E6F9E") +
  geom_point_3d(data = observations, size = 3, colour = "#B5342B") +
  coord_3d()
p
```

![A blue curved surface with three red observations at different
heights.](ggplot3d_files/figure-html/surface-1.svg)

Grid neighbours form triangles. Surface pieces cover observations
wherever they lie in front of them. [Choosing layers for observations
and grids](https://gillescolling.com/ggplot3d/articles/layers.md) also
shows columns and contours, and [Building surfaces and
solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.md)
shows meshes and statistical surfaces.

## Reading another table

For a file with other column names, change the file path and the names
inside the mapping. This example writes a temporary comma-separated file
so the whole example can run in a fresh session.

``` r

path <- tempfile(fileext = ".csv")
write.csv(observations, path, row.names = FALSE)
measured <- read.csv(path)
ggplot(measured, aes(east, north, z = height)) +
  geom_point_3d(size = 3) + coord_3d()
```

![Three observations read from a comma-separated file and mapped to the
east, north and height
axes.](ggplot3d_files/figure-html/read-table-1.svg)

## Supplying a surface height

A surface needs a z mapping. The following call shows the message
produced by a missing mapping.

``` r

ggplot(surface, aes(east, north)) + geom_surface_3d() + coord_3d()
#> Error in `make_layer_3d()`:
#> ! Problem while computing stat.
#> ℹ Error occurred in the 1st layer.
#> Caused by error:
#> ! A surface requires a z aesthetic.
```

Map the height column to z to connect the grid points.

``` r

ggplot(surface, aes(east, north, z = height)) +
  geom_surface_3d(fill = "#2E6F9E") + coord_3d()
```

![A blue curved surface after supplying its height
mapping.](ggplot3d_files/figure-html/corrected-1.svg)

## Saving the plot

The default backend draws vector geometry. The file extension selects
the graphics device. Use the raster backend for a scene rendered into
pixels; [Choosing rendering and image
formats](https://gillescolling.com/ggplot3d/articles/backends.md)
explains resolution and device requirements.

``` r

path <- tempfile(fileext = ".pdf")
ggsave(path, p, width = 5, height = 4.5)
```

## Where next

- [Preparing data for 3D
  layers](https://gillescolling.com/ggplot3d/articles/data-preparation.md):
  read coordinate tables, join attributes and check surface grids.

- [Choosing layers for observations and
  grids](https://gillescolling.com/ggplot3d/articles/layers.md): select
  geometry for your measurements.

- [Scaling heights and facet
  ranges](https://gillescolling.com/ggplot3d/articles/z-scale.md):
  transform heights and control ranges across facets.

- [Styling axes, legends and
  panes](https://gillescolling.com/ggplot3d/articles/guides-themes.md):
  label the plot and adjust its panes.

- [Choosing camera
  views](https://gillescolling.com/ggplot3d/articles/camera.md): set the
  view and keep its bounds stable.

- [Choosing rendering and image
  formats](https://gillescolling.com/ggplot3d/articles/backends.md):
  choose rendering and save an image.

- [How overlapping geometry is
  drawn](https://gillescolling.com/ggplot3d/articles/visibility.md):
  understand overlapping geometry.

The [article
index](https://gillescolling.com/ggplot3d/articles/index.md) groups
further guides by building plots, data types, appearance, output and
sharing, scientific examples, mathematical examples, and internals and
extensions.
