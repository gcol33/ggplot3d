# Read a triangle or polygon mesh file

Reads Wavefront OBJ, STL, PLY and glTF 2.0 (`.gltf`, `.glb`) files into
a [`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
corner table. The reader is chosen by file extension from a registry of
native parsers. Scans from phone and tablet LiDAR apps (Polycam,
Scaniverse, 3d Scanner App) export these formats; point clouds without
faces are read by
[`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md).

## Usage

``` r
read_mesh_3d(path)
```

## Arguments

- path:

  Path to a `.obj`, `.stl`, `.ply`, `.gltf` or `.glb` file.

## Value

A keyed corner data frame of class ggplot3d_mesh.

## Details

- OBJ (ASCII): `v` vertices (with optional red, green and blue vertex
  colours), `vt` texture coordinates and `f` faces of any size, with
  `v/vt/vn` references, negative (relative) indices and `\` line
  continuation; `o` and `g` names become the face attribute `group`,
  `usemtl` names the face attribute `material`. Materials from `mtllib`
  files give the face attribute `face_colour` (from `Kd` and `d`) and
  the texture image (`map_Kd`). Other statements are ignored.

- STL (ASCII or binary): every facet is a triangle; vertices with
  exactly equal coordinates are welded. A file whose size is 84 bytes
  plus 50 bytes per triangle counted in its header is read as binary,
  whatever its first bytes say.

- PLY (ASCII, binary little- or big-endian): header-driven elements and
  properties of every scalar type; `vertex` x, y, z and `face`
  `vertex_indices` (or `vertex_index`). Vertex `red`, `green`, `blue`
  (and `alpha`) become the vertex attribute `vertex_colour`, face
  colours the face attribute `face_colour`; other scalar vertex
  properties are kept as vertex attributes. Face `texcoord` lists (six
  numbers per triangle) give texture coordinates; the image named by a
  `comment TextureFile` header line is the texture.

- glTF 2.0 (JSON `.gltf` with external or embedded buffers, binary
  `.glb`): triangle, strip and fan primitives of every mesh in the
  default scene, placed by their node transforms. `COLOR_0` (times the
  base colour factor) becomes `vertex_colour`, the base colour factor
  alone `face_colour`, the material name `material`; `TEXCOORD_n` and
  the base colour texture give the texture. glTF colours are linear and
  are encoded to sRGB.

A textured mesh carries its image as the attribute `"texture"` (a
[`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)
with extent `c(0, 1, 0, 1)`) and its texture coordinates as `.texture_u`
and `.texture_v` corner columns;
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
drapes it when the mesh is the layer's data. Several images are packed
into one atlas, one above the other, and their texture coordinates are
clamped to the unit square. PNG textures need png, JPEG textures jpeg.

glTF positions are y-up by specification, and most scanning apps export
y-up OBJ and PLY files as well; draw them with `coord_3d(up = "y")`.

## References

Wavefront OBJ and MTL: <https://paulbourke.net/dataformats/obj/>,
<https://paulbourke.net/dataformats/mtl/>. PLY (Turk):
<https://paulbourke.net/dataformats/ply/>. STL:
<https://en.wikipedia.org/wiki/STL_(file_format)>. glTF 2.0 (Khronos
Group): <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html>.

## See also

[`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md)
for point clouds,
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md).

## Examples

``` r
path <- tempfile(fileext = ".obj")
writeLines(c("v 0 0 0", "v 1 0 0", "v 1 1 0", "v 0 1 0", "f 1 2 3 4"), path)
m <- read_mesh_3d(path)
ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
```
