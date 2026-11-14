# Exporting plots and scenes

Export a panel as a 3D model for another application, or save a
projected figure with editable paths and source-row information. This
article writes both kinds of output from small plots. Installation is
covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).
Optional bridges use [rgl](https://dmurdoch.github.io/rgl/), an R
package for interactive 3D scenes, and
[rayrender](https://www.rayrender.net), an R path tracer.

## Example

The built-in motor-car data record weight in thousands of pounds, fuel
economy in miles per US gallon and gross horsepower. Model names provide
hover text and selection keys. Start with a view of these three
variables.

``` r

cars <- data.frame(mtcars, model = rownames(mtcars))
p <- ggplot(cars, aes(wt, mpg, z = hp, tooltip = model, data_id = model)) +
  geom_point_3d(size = 3, colour = PAL["blue"]) +
  coord_3d() + labs(x = "weight", y = "mpg", z = "horsepower")
p
```

![Motor cars positioned by weight, fuel economy and
horsepower.](export_files/figure-html/cars-1.svg)

## Model files

[`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md)
writes one panel, selected by its index in layout order. The extension
chooses the format. The following example writes a binary glTF model,
called a GLB file, to a temporary directory and checks the file.

``` r

out <- tempfile("ggplot3d-export-")
dir.create(out)
glb <- file.path(out, "cars.glb")
export_3d(p, glb)
file.info(glb)$size > 0
#> [1] TRUE
```

The glTF 2.0 format transfers meshes, mapped vertex colours, materials
and a camera to applications such as Blender. Each layer becomes a named
node, so it can be selected and restyled after import. The plot’s
vertical axis becomes positive y. An oblique camera is exported as an
orthographic camera along its viewing direction.

Keep companion files together when copying an export. Text is omitted
from these model formats, and dashed paths become solid. Panes, grid
lines and box edges are included with `furniture = TRUE`.

| Extension | Contents | Companion file |
|----|----|----|
| `.glb` | glTF scene and buffer in one binary file | None |
| `.gltf` | glTF scene description | `.bin` buffer |
| `.ply` | Binary vertices, normals, colours and faces; optional edges | None |
| `.obj` | Objects, vertices, colours and faces | `.mtl` materials |
| `.stl` | Binary triangles | None |

Write the other formats with the same call. The temporary directory
makes every example runnable without choosing an output folder first.

``` r

formats <- c("gltf", "ply", "obj", "stl")
for (extension in formats) {
  export_3d(p, file.path(out, paste0("cars.", extension)))
}
list.files(out)
#> [1] "cars.bin"  "cars.glb"  "cars.gltf" "cars.mtl"  "cars.obj"  "cars.ply" 
#> [7] "cars.stl"
```

## Scale and primitive choices

Model coordinates follow the aspect-scaled panel box. Its longest side
is `size` file units long; expanded scale limits also contribute to that
box. PLY and STL use positive z as the vertical axis. OBJ uses positive
y. The format’s units are interpreted by the importing application.

Paths become closed tubes with their drawn width as diameter, and
markers become small solids. Width and height set the layout used to
convert physical line and point sizes relative to the box. Use line and
point primitives when the receiving format and software support them.

``` r

export_3d(p, file.path(out, "points.ply"), points = "point")
export_3d(p, file.path(out, "large.stl"), size = 50,
  lines = "tube", points = "mesh")
file.info(file.path(out, c("points.ply", "large.stl")))$size > 0
#> [1] TRUE TRUE
```

STL requires tube paths and mesh points because it stores triangles
only. Exporting a surface to STL transfers its triangles; closing the
entire model for manufacturing is a separate modelling step. Traced SDF
and volume layers produce images in the plot. Use their mesh-producing
surface counterparts when exporting model geometry, as shown in [Ray
marching and
volumes](https://gillescolling.com/ggplot3d/articles/ray-marching.md).

## Animated glTF

[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
writes camera translation, rotation and zoom tracks to glTF. The first
panel is exported. Start with a three-frame turn of the same motor cars;
the result records the file and frame count.

``` r

turn <- animate_3d(p, azimuth = c(-60, -20, 20), frames = 3,
  file = file.path(out, "turn.glb"), fps = 2, width = 320, height = 320,
  workers = 1, progress = FALSE)
turn$frame_count
#> [1] 3
```

For changing data, a time column selects the rows drawn in each frame.
Scales train on all frames, and visibility tracks select the frame’s
geometry in the file. Here the horsepower coordinate increases by twenty
in the second frame. These changed coordinates are an illustration of
animation.

``` r

states <- rbind(transform(cars, step = 1),
  transform(cars, step = 2, hp = hp + 20))
changing <- ggplot(states, aes(wt, mpg, z = hp)) +
  geom_point_3d(size = 3, colour = PAL["blue"]) + coord_3d()
motion <- animate_3d(changing, time = "step", azimuth = -60,
  file = file.path(out, "states.gltf"), fps = 2,
  width = 320, height = 320, workers = 1, progress = FALSE)
motion$frame_count
#> [1] 2
```

Keep a projection type fixed within an animation. Camera paths and frame
timing are described in [Camera animation and
interaction](https://gillescolling.com/ggplot3d/articles/motion.md).

## rgl meshes and scenes

The rgl bridge dispatches through its own generic functions. Convert the
plot to a mesh with vertex colours, then inspect the vertex and triangle
counts. Point primitives can instead populate the mesh’s point indices.

``` r

mesh <- rgl::as.mesh3d(p)
c(vertices = ncol(mesh$vb), triangles = ncol(mesh$it))
#>  vertices triangles 
#>      7680      2560
```

Draw with the plot’s camera angles and projection through
[`rgl::plot3d()`](https://dmurdoch.github.io/rgl/dev/reference/plot3d.html).
This example uses rgl’s null device so it also runs during document
builds. rgl centres the drawn geometry itself, so its framing may differ
from the original plot. The scene includes text through rgl’s native
text primitives.

``` r

old <- options(rgl.useNULL = TRUE)
ids <- rgl::plot3d(p)
length(ids)
#> [1] 1
rgl::close3d()
options(old)
```

## rayrender scenes

Build a rayrender scene to continue with that package’s materials,
lights and renderer. The bridge creates one diffuse mesh object per
nonempty geometry layer, with the mapped vertex colours. Paths become
tubes and points become solids; translucency is omitted.

``` r

scene <- as_rayrender_3d(p)
nrow(scene)
#> [1] 1
```

Setting `render = TRUE` passes the scene and plot camera to rayrender’s
renderer. Additional arguments control that render. The native path
tracer in [Ray marching and
volumes](https://gillescolling.com/ggplot3d/articles/ray-marching.md)
provides diffuse rendering directly within a ggplot panel.

## Semantic SVG

The native Scalable Vector Graphics (SVG) writer preserves the
connection between a visible fragment and its source layer and row.
Hovering or focusing a fragment highlights fragments from the same
source row, and its title provides tooltip text. The projected view
stays fixed.

``` r

svg <- file.path(out, "cars.svg")
save_svg_3d(p, svg, interactive = TRUE, width = 624, height = 432)
doc <- xml2::read_xml(svg)
fragments <- xml2::xml_find_all(doc, ".//*[@data-primitive]")
head(unique(xml2::xml_attr(fragments, "data-layer")))
#> [1] "0" "1"
```

Each fragment carries `data-layer`, `data-row` and `data-primitive`
attributes. Consecutive runs from each layer are grouped in paint order,
preserving depth ordering across layers. Clipping paths and compound
seam underlays retain the visibility of the compiled vector scene. Set
`interactive = FALSE` for a file without the hover script.

The embedded preview below retains hover and keyboard interaction. It
uses an HTML object containing the exported SVG itself.

Motor-car scatterplot with weight, mpg and horsepower axes.

Open the SVG directly in a browser or embed it as an HTML object to
retain its interaction. An SVG displayed through an image element
supplies a static preview. [Interactive
viewing](https://gillescolling.com/ggplot3d/articles/viewer.md) provides
a camera you can rotate.

## TikZ

TikZ is a LaTeX drawing language. Export the whole projected plot as
editable fill, draw, text and clipping commands. The standalone setting
includes a document preamble; disable it for code to insert into an
existing document.

``` r

tikz <- export_code_3d(p, "tikz", file = file.path(out, "cars.tex"))
cat(paste(head(strsplit(tikz, "\n")[[1]], 5), collapse = "\n"))
#> \documentclass[tikz,border=0pt]{standalone}
#> \usepackage{graphicx}
#> \usetikzlibrary{svg.path}
#> \begin{document}
#> \begin{tikzpicture}[x=1pt,y=1pt]
```

Compile the file with `pdflatex cars.tex`. The generated code uses
TikZ’s `svg.path` library, and TeX selects the text font. Raster
colourbars and annotations become coloured pixel rectangles. The
complete figure includes all panels, furniture and legends.

## Asymptote and PRC

Asymptote is a language for mathematical graphics. Its exporter writes
model-space geometry, materials, text and a camera, which you can edit
to render another view. The panel index selects the scene to export.

``` r

asy <- export_code_3d(p, "asy", panel = 1,
  file = file.path(out, "cars.asy"), prc = TRUE)
cat(paste(head(strsplit(asy, "\n")[[1]], 5), collapse = "\n"))
#> settings.prc=true;
#> import three;
#> size(169.333333333mm,169.333333333mm);
#> currentprojection=orthographic(camera=(1.68301270189,-1.54903810568,1.86602540378),up=(-0.250000000000,0.433012701892,0.866025403784),target=(0.500000000000,0.500000000000,0.500000000000),center=false);
#> surface ggplotModel;
```

The PRC setting requests embedded 3D content in PDF. Compile with
`asy -tex pdflatex -f pdf cars.asy` and install the TeX packages media9
and ocgx2. A PDF reader with PRC support supplies the interactive view.
For a static PDF or ordinary SVG through an R graphics device, use
[Output
backends](https://gillescolling.com/ggplot3d/articles/backends.md). \##
3D printing

[`print_3d()`](https://gillescolling.com/ggplot3d/reference/print_3d.md)
maps the panel box into a physical bounding box in millimetres, keeping
its aspect. Tube widths, marker sizes, shell thicknesses and the base
plate are applied in millimetres after scaling. Open surfaces can become
normal-offset shells or close vertically onto the base. Layer overrides
use the exported layer names, such as `c(layer1_surface = "base")`.

``` r

settings <- print_3d(size_mm = c(100, 100, 60), min_mm = 0.8,
  shell_mm = 1.2, base_mm = 2, text = "raised", furniture = c("base", "axes"))
export_3d(p, "cars.stl", print = settings)
export_3d(p, "cars.3mf", print = settings)
```

Printing uses the bundled Manifold C++ library for Boolean union with
double-precision meshes and colour propagation. Native checks validate
edge incidence, closure, winding and self-intersection and identify
failing layers. Disconnected closed components share one exported model
object.

Character labels use font outlines extruded on backing plaques. Raised
labels project from the plaque; engraved labels cut into it.
`text = "none"` omits labels. Axis geometry follows the plot’s in-plane
guide layout. Furniture and text can extend beyond the panel bounding
box, so check the full model bounds in the slicer before printing.

Binary STL stores geometry. 3MF stores millimetre units and triangle
colours using the 3MF Materials and Properties colour-group extension.
Import and re-export checks with PrusaSlicer 2.9.7 and Bambu Studio
2.9.1.70 preserved the test model’s geometry and omitted its standard
colour groups. Assigning these colours to printer filaments requires
support in the target slicer; vendor paint formats are separate
extensions.
