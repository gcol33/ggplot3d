# Mesh lines over surfaces

`edges_3d()` draws the edges of a surface's cells over its faces, passed
as the `edges` argument of a surface or mesh layer, or to
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
for every such layer that sets none. The cells of a surface on a grid
([`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md),
the surface stats,
[`geom_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md),
[`geom_spatraster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_spatraster_3d.md))
are the quadrilaterals between neighbouring grid points, so the lines
run along the grid and leave out the diagonals that split each cell into
triangles; the cells of a mesh are its polygons; a Delaunay surface has
triangles as cells. `every` keeps every n-th grid line and the last one,
as the strides of matplotlib's `plot_surface()` or Mathematica's
`Mesh -> n` thin a dense mesh. The lines are strokes of the scene,
hidden by nearer surfaces like any other line.

## Usage

``` r
edges_3d(
  type = c("cells", "triangles"),
  every = 1L,
  colour = "white",
  linewidth = 0.2,
  linetype = 1,
  alpha = 0.6,
  hidden = c("hide", "dashed", "show")
)
```

## Arguments

- type:

  `"cells"` for the edges of the cells, `"triangles"` for every triangle
  edge.

- every:

  Positive integer stride of the grid lines: one value for both
  directions or two, for the lines along x (or u) and along y (or v). A
  stride above 1 needs a surface on a grid.

- colour, linewidth, linetype, alpha:

  Stroke of the lines.

- hidden:

  Hidden stroke policy of the lines: `"hide"`, `"dashed"` or `"show"`.

## Value

An edges specification for the `edges` argument of
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md),
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md),
the surface stats and
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Details

Thin light lines with some transparency over a dark fill trace the grid
without hiding the shading; dark lines over a light fill give the look
of MATLAB's `surf()`.

## See also

[`outline_3d()`](https://gillescolling.com/ggplot3d/reference/outline_3d.md)
for silhouettes, creases and boundary lines.

## Examples

``` r
d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
d$z <- with(d, exp(-(x^2 + y^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "#2E6F9E", shading = "smooth", edges = edges_3d()) +
  coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey90", shading = "smooth") +
  coord_3d(edges = edges_3d(every = 5, colour = "grey20", alpha = 1),
           outline = outline_3d(silhouette = FALSE, crease = NULL))
```
