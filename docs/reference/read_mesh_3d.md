# Read a triangle or polygon mesh file

Reads Wavefront OBJ, STL and PLY files into a
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
corner table. The reader is chosen by file extension from a registry of
native parsers.

## Usage

``` r
read_mesh_3d(path)
```

## Arguments

- path:

  Path to a `.obj`, `.stl` or `.ply` file.

## Value

A keyed corner data frame of class ggplot3d_mesh.

## Details

- OBJ (ASCII): `v` vertices and `f` faces of any size, with `v/vt/vn`
  references, negative (relative) indices and `\` line continuation; `o`
  and `g` names become the face attribute `group`. Other statements are
  ignored.

- STL (ASCII or binary): every facet is a triangle; vertices with
  exactly equal coordinates are welded. A file whose size is 84 bytes
  plus 50 bytes per triangle counted in its header is read as binary,
  whatever its first bytes say.

- PLY (ASCII, binary little- or big-endian): header-driven elements and
  properties of every scalar type; `vertex` x, y, z and `face`
  `vertex_indices` (or `vertex_index`). Vertex `red`, `green`, `blue`
  (and `alpha`) become the vertex attribute `vertex_colour`, face
  colours the face attribute `face_colour`; other scalar vertex
  properties are kept as vertex attributes.

## References

Wavefront OBJ: <https://paulbourke.net/dataformats/obj/>. PLY (Turk):
<https://paulbourke.net/dataformats/ply/>. STL:
<https://en.wikipedia.org/wiki/STL_(file_format)>.

## Examples

``` r
path <- tempfile(fileext = ".obj")
writeLines(c("v 0 0 0", "v 1 0 0", "v 1 1 0", "v 0 1 0", "f 1 2 3 4"), path)
m <- read_mesh_3d(path)
ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
```
