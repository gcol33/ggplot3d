# Feature lines of surfaces and meshes

`outline_3d()` describes the lines a surface or mesh layer derives from
its own triangles, passed as its `outline` argument: silhouettes, where
the surface turns away from the camera, creases, where neighbouring
faces meet at a sharp angle, and boundaries, the edges of open surfaces
(Bénard and Hertzmann 2019). They are drawn as strokes of the scene, so
they are hidden by nearer surfaces like any other line, and `hidden`
chooses whether their hidden parts are left out, dashed or drawn.

## Usage

``` r
outline_3d(
  silhouette = TRUE,
  crease = 30,
  boundary = TRUE,
  colour = "black",
  linewidth = 0.5,
  linetype = 1,
  alpha = 1,
  hidden = c("hide", "dashed", "show"),
  halo = NULL
)
```

## Arguments

- silhouette:

  `TRUE`, `FALSE`, `"smooth"` or `"mesh"`.

- crease:

  Crease angle in degrees, or `NULL` for no creases.

- boundary:

  Whether to draw boundary edges.

- colour, linewidth, linetype, alpha:

  Stroke of the lines.

- hidden:

  Hidden stroke policy of the lines: `"hide"`, `"dashed"` or `"show"`.

- halo:

  Width of the gap left on each side of a line where farther lines pass
  behind it: a physical
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) or a number in
  millimetres. `NULL` (the default) for no gap.

## Value

An outline specification for the `outline` argument of
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md),
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
and the surface stats.

## Details

An edge is a boundary when it belongs to one triangle and a crease when
the normals of its two triangles differ by more than `crease` degrees.
Silhouettes depend on the view and are recomputed from the panel's
camera when the plot is built. On a mesh (`silhouette = "mesh"`) they
are the edges between a triangle facing the camera and one facing away;
they follow the edges, so they zigzag across a finely curved surface. On
a smooth surface (`silhouette = "smooth"`) they are the interpolated
contours of Hertzmann and Zorin (2000): the zero set of n . (c - p), the
vertex normal against the direction to the camera, interpolated linearly
along each edge, which crosses the faces as a smooth curve. Because that
curve lies on the triangles a little inside the mesh's own outline,
nearer triangles can hide short pieces of it. `silhouette = TRUE` takes
the smooth contour for layers with vertex normals (`nx`, `ny`, `nz`, as
grid, parametric and isosurface layers have) and the mesh silhouette
otherwise. In the interactive viewer, silhouettes stay those of the
initial camera.

`halo` draws haloed lines (Appel, Rohlf and Stein 1979): each feature
line masks a band of the given width on both sides of its stroke, and
any line behind it is cut back inside that band, so a line passing
behind another stops short of it instead of touching it. The stroke
itself stays `linewidth` wide. The band only cuts lines lying farther
from the camera, never faces, and lines that end at a common vertex,
such as the edges at a corner of a cube, still meet there.

## References

Appel, Rohlf and Stein (1979). The haloed line effect for hidden line
elimination. SIGGRAPH '79. <https://history.siggraph.org/?p=104577>

Bénard and Hertzmann (2019). Line drawings from 3D models: a tutorial.
Foundations and Trends in Computer Graphics and Vision 11, 1-159.
[doi:10.1561/0600000075](https://doi.org/10.1561/0600000075)

Hertzmann and Zorin (2000). Illustrating smooth surfaces. SIGGRAPH 2000,
517-526.
[doi:10.1145/344779.345074](https://doi.org/10.1145/344779.345074)

## Examples

``` r
torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u),
  y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
ggplot2::ggplot() +
  geom_parametric_3d(fun = torus, n = c(48, 24), closed = TRUE, fill = "white",
    outline = outline_3d(linewidth = 0.8)) +
  coord_3d()
cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4),
  c(1, 5, 7, 3), c(2, 4, 8, 6))
ggplot2::ggplot() +
  geom_mesh_3d(vertices = cube, faces = quads, fill = "grey90",
    outline = outline_3d(hidden = "dashed")) +
  coord_3d()
ggplot2::ggplot() +
  geom_parametric_3d(fun = torus, n = c(48, 24), closed = TRUE, fill = "white",
    outline = outline_3d(crease = NULL, hidden = "show", halo = grid::unit(1, "mm"))) +
  coord_3d(elevation = 30)
```
