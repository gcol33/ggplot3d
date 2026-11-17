# Export a plot to a 3D model, vector or source-code file

Writes the scene of a plot to a file that other software opens. Model
formats hold the scene of one panel in 3D: glTF binary (`.glb`) or glTF
with a separate buffer (`.gltf` and `.bin`) for Blender and the web,
PLY, OBJ with an MTL material file, binary STL and 3MF. Faces are
triangulated, paths become tubes or line primitives, and points become
small solids or point primitives, all in model coordinates: the panel
box scaled by the aspect of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
with its longest side `size` long; 3MF and STL written with `print` hold
millimetre solids instead. Vector and source formats hold the projected
plot: SVG with source rows and hover text, TikZ (`.tex`) and an
Asymptote scene (`.asy`).

## Usage

``` r
export_3d(
  plot,
  file = NULL,
  format = NULL,
  panel = 1L,
  furniture = FALSE,
  lines = c("tube", "line"),
  points = c("mesh", "point"),
  size = 1,
  width = 640L,
  height = 640L,
  dpi = 96,
  print = NULL,
  interactive = TRUE,
  standalone = TRUE,
  prc = FALSE
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- file:

  Output path, or NULL to use `format`. Its extension selects the
  format: `.glb`, `.gltf`, `.ply`, `.obj`, `.stl`, `.3mf`, `.svg`,
  `.tex` (TikZ) or `.asy`.

- format:

  Output format as one of these extension names. With `file = NULL`, the
  format writes to a session temporary path. When both are supplied, the
  extension of `file` must agree with `format`.

- panel:

  Panel index in layout order, default 1, for model formats and
  Asymptote; SVG and TikZ hold every panel.

- furniture:

  Whether to include the panes, grid lines and box edges, default FALSE.

- lines:

  `"tube"` (default) writes each path as a closed tube with the drawn
  line width as diameter; `"line"` writes line primitives.

- points:

  `"mesh"` (default) writes each point as a solid of its drawn size: a
  cube for square shapes, a tetrahedron for triangles, an octahedron for
  diamonds and a sphere for the other shapes; `"point"` writes point
  primitives.

- size:

  Length of the longest side of the box in file units, default 1.

- width, height:

  Device size in pixels used to lay out the plot, default 640 each.
  Tubes and solids keep the size relative to the box that lines and
  points have in a plot of this size.

- dpi:

  Device resolution, default 96 dots per inch.

- print:

  A
  [`print_spec_3d()`](https://gillescolling.com/ggplot3d/reference/print_spec_3d.md)
  specification for closed millimetre solids: optional for STL, required
  for 3MF.

- interactive:

  SVG: whether to include tooltips, keyboard focus and linked hover
  highlighting for fragments of the same source row.

- standalone:

  TikZ: whether to include a LaTeX document preamble.

- prc:

  Asymptote: whether to include PRC in its PDF output.

## Value

The written path, invisibly.

## Details

glTF output follows the glTF 2.0 specification and is the route into
Blender, whose importer reads its meshes, materials, cameras and
animation. The vertical axis of the plot becomes +y. Each layer is one
named node (`layer1_point`, `layer2_surface`, ...), and the furniture is
a node `furniture` with the children `panes`, `grid` and `box`, so
layers can be selected, hidden and restyled after import. Mapped colours
are vertex colours (`COLOR_0`, linear light), so a colour scale needs no
material per colour. Faces drawn with lighting
([`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
and `shade`) use one lit material whose base colour multiplies the
vertex colours; all other geometry uses the `KHR_materials_unlit`
extension. Translucent geometry uses alpha blending and every material
is double sided. A node `camera` holds the view of the panel, so the
file opens on the plot's view; an oblique projection is written as an
orthographic camera along the sight direction.
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
writes animated glTF files.

PLY output is binary with vertex normals and 8-bit sRGB vertex colours;
line primitives are `edge` elements. OBJ output writes one object per
node, sRGB vertex colours after each vertex position, and one MTL
material per distinct colour; its vertical axis is +y, as OBJ importers
expect. PLY and STL keep the vertical axis of the plot as +z. STL holds
triangles only, so it requires `lines = "tube"` and `points = "mesh"`.

Ordinary model exports omit text and write dashed lines solid. Printing
exports include extruded text according to
[`print_spec_3d()`](https://gillescolling.com/ggplot3d/reference/print_spec_3d.md).

SVG output writes the compiled, depth-ordered fragments as SVG geometry.
Each fragment carries data-layer, data-row and data-primitive
attributes. Consecutive runs of each layer share a group so that
interleaved layers retain their visibility order. Scene masks become
clipping paths, and surface seams share compound-path underlays. Static
plot furniture is embedded as SVG. A `tooltip` aesthetic sets the hover
text.

TikZ writes the complete SVG paint order as editable fill, draw and text
commands, including clip paths, furniture and legends. Requires TikZ's
`svg.path` library. Raster colourbars and annotations become coloured
pixel rectangles using the optional png decoder. TeX selects the text
font. Asymptote writes model-space triangles, paths, marker solids, text
and a camera, so it can re-render the scene from another view or produce
an interactive PRC PDF. For a PRC PDF, compile with
`asy -tex pdflatex -f pdf plot.asy` and install the TeX packages media9
and ocgx2. A PDF reader with PRC support supplies the interactive view.

## See also

[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
for animated glTF files.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 15), y = seq(-1, 1, length.out = 15))
d$z <- d$x * exp(-d$x^2 - d$y^2)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(ggplot2::aes(fill = z)) + coord_3d()
glb <- export_3d(p, tempfile(fileext = ".glb"))
file.size(glb)
export_3d(p, tempfile(fileext = ".stl"), size = 50)
asy <- export_3d(p, format = "asy")
if (requireNamespace("svglite", quietly = TRUE) && requireNamespace("xml2", quietly = TRUE)) {
  export_3d(p, format = "svg")
  export_3d(p, format = "tex", standalone = FALSE)
}
```
