# Hyperplane sections of four-dimensional tetrahedral meshes

Cuts each tetrahedral boundary cell by dot(normal, position)=offset,
interpolates shared mesh edges once, and triangulates the resulting
three-dimensional section. Cells lying entirely in the section
contribute their boundary triangles; shared internal triangles are
removed. The output basis is an orthonormal basis of the hyperplane's
direction space.

## Usage

``` r
stat_section_4d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  cells,
  normal = c(0, 0, 0, 1),
  offset = 0,
  basis = NULL,
  tolerance = 1e-10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- cells:

  Four-column matrix of vertex indices for each tetrahedral cell.

- normal:

  Nonzero hyperplane normal in four dimensions.

- offset:

  Hyperplane offset using the supplied normal's scale.

- basis:

  Optional 4 by 3 orthonormal basis perpendicular to normal.

- tolerance:

  Absolute tolerance in normalised signed plane distance.

## Value

A mesh layer with cell identifiers.

## Examples

``` r
v <- data.frame(x=c(0,1,0,0,0),y=c(0,0,1,0,0),z=c(0,0,0,1,0),w=c(0,0,0,0,1))
cells <- t(utils::combn(5,4))
ggplot2::ggplot(v,ggplot2::aes(x,y,z=z,w=w))+
  stat_section_4d(cells=cells,offset=.3)+coord_3d()
```
