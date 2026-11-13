# Fortify an indexed three-dimensional mesh

Fortify an indexed three-dimensional mesh

## Usage

``` r
mesh_3d(vertices, faces, vertex_data = NULL, face_data = NULL)
```

## Arguments

- vertices:

  Data frame containing x, y, z and optional .vertex and .mesh keys.

- faces:

  Matrix with one triangle or quad per row, or a data frame with .face,
  .corner, .vertex and optional .mesh keys. Indices are one-based.

- vertex_data, face_data:

  Attribute tables joined on .vertex or .face, together with .mesh when
  supplied. Keys must be unique.

## Value

A keyed corner data frame of class ggplot3d_mesh.

## Examples

``` r
v <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 1))
m <- mesh_3d(v, matrix(1:3, nrow = 1))
ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
```
