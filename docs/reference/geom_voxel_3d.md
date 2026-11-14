# Voxels

Draws one box per row, centred on x, y, z. Box sizes are `width`,
`depth` and `height` along x, y and z, by default the smallest spacing
between distinct centres in each panel; explicit xmin/xmax, ymin/ymax
and zmin/zmax take precedence. All voxels of a panel share one lattice.

## Usage

``` r
geom_voxel_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  width = NULL,
  depth = NULL,
  height = NULL,
  greedy = TRUE,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- width, depth, height:

  Voxel sizes along x, y and z.

- greedy:

  Whether to merge coplanar faces of one material.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

Only faces that can be seen are drawn: a face between two filled voxels
is kept only when the voxel behind it is translucent (alpha below one)
and differs in fill, colour, alpha, linewidth or linetype. With
`greedy = TRUE` the visible faces of one material are merged, per
lattice slice and direction, into maximal rectangles (greedy meshing),
so the visibility engine receives far fewer polygons. Merged rectangles
meet smaller ones in T-junctions; vector output covers the edges between
opaque faces with the geometric union of the faces, which does not
depend on shared vertices.

## References

Lysenko (2012). Meshing in a Minecraft game.
<https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>

## Examples

``` r
d <- expand.grid(x = 1:4, y = 1:4, z = 1:4)
d <- d[(d$x - 2.5)^2 + (d$y - 2.5)^2 + (d$z - 2.5)^2 < 4, ]
d$class <- ifelse(d$z > 2, "top", "bottom")
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = class)) +
  geom_voxel_3d() + coord_3d()
```
