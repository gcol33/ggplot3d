# Draw an external triangle or polygon mesh

Draw an external triangle or polygon mesh

## Usage

``` r
geom_mesh_3d(
  mapping = NULL,
  data = NULL,
  stat = StatMesh3D,
  position = "identity",
  ...,
  vertices = NULL,
  faces = NULL,
  shade = TRUE,
  wireframe = FALSE,
  hidden = "hide",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- vertices, faces:

  Optional vertex data frame and face index matrix, passed to mesh_3d().
  With faces supplied, data may hold the vertices.

- shade:

  Whether to apply lighting.

- wireframe:

  Draw original mesh edges with shared edges deduplicated.

- hidden:

  Hidden stroke policy.

## Value

A ggplot2 layer.

## Details

Data accepts mesh_3d() output, rgl mesh3d objects and projected sf
POLYGON Z or TIN Z objects. Numeric vertex fill is averaged per face
before colour scaling. Other materials must be constant within each
face.

## Examples

``` r
v <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 1, 1))
ggplot2::ggplot() + geom_mesh_3d(vertices = v, faces = matrix(1:4, 1)) + coord_3d()
```
