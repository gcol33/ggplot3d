# Position guides for three-dimensional axes

Tick labels and titles are measured in the drawing viewport. Automatic
placement uses an exterior cube edge. Integer positions 1 through 4
select a fixed edge in lexicographic order of its two fixed coordinates.
Position `"origin"` draws the axis through the `origin` point of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
as part of the scene, with ticks, tick labels and title in three
dimensions; `coord_3d(axis_lines = "origin")` places every guide with
position `"auto"` there. Labels are thinned automatically using their
measured extents along the edge.

## Usage

``` r
guide_axis_3d(
  title = ggplot2::waiver(),
  theme = NULL,
  position = "auto",
  check.overlap = FALSE,
  angle = ggplot2::waiver(),
  n.dodge = 1L,
  minor.ticks = FALSE,
  cap = "none",
  order = 0L,
  label.orientation = "screen",
  plane = "auto",
  title.position = "centre",
  depth.scale = FALSE,
  ticks = "outward"
)
```

## Arguments

- title:

  Guide title, with waiver inheriting the scale or plot title.

- theme:

  Local theme overrides.

- position:

  "auto", a side (top, bottom, left, right), edge 1 to 4, or "origin".

- check.overlap:

  Use extra clearance between measured label rectangles.

- angle:

  Label rotation in degrees for `label.orientation = "screen"`. Waiver
  keeps labels horizontal.

- n.dodge:

  Number of parallel label rows.

- minor.ticks:

  Draw minor ticks.

- cap:

  Trim the lower, upper, both, or neither end to major breaks.

- order:

  Guide ordering value.

- label.orientation:

  `"screen"`, `"edge"` or `"plane"`: tick labels upright on the page,
  turned along the projected edge and kept upright, or lying in a pane.
  Titles follow the edge in every orientation and lie in the pane with
  `"plane"`.

- plane:

  Pane for plane labels and in-plane ticks: `"auto"`, `"floor"`, `"xy"`,
  `"xz"` or `"yz"`. The pane must contain the axis.

- title.position:

  `"centre"` places the title beside the middle of the axis, beyond the
  tick labels; `"near"` places it at the end nearer the camera. Screen
  titles sit past that endpoint; plane titles sit inside the pane
  boundary.

- depth.scale:

  Under perspective projection, scale tick labels and tick lengths by
  their distance from the camera: `FALSE` (none), `TRUE` (full), or a
  nonnegative strength `s`, which maps the ratio `d` of the distance of
  the camera target to the distance of the tick to `d^s`, as
  `scale_depth` in ggcube. Titles are not scaled. Plane labels and
  in-plane ticks are foreshortened by the projection itself.

- ticks:

  `"outward"` draws ticks on the page, perpendicular to the projected
  edge; `"in_plane"` draws them into the pane, perpendicular to the edge
  in three dimensions. Plane labels and ticks depth-sort with data.

## Value

A ggplot2 guide for a three-dimensional position axis.

## Details

Tick labels can stand upright on the page
(`label.orientation = "screen"`), turn with the projected edge they
label (`"edge"`, as `rotate_labels` in ggcube and the automatic label
rotation of Makie's `Axis3`), or lie flat in one of the two box panes
that meet at the edge (`"plane"`), drawn from the glyph outlines of the
font so that perspective foreshortens them like the panes.
`ticks = "in_plane"` points the ticks into the same pane, with a length
that shrinks with distance from the camera. The pane is chosen with
`plane`: `"auto"` takes the pane in which the ticks stand out furthest
from the edge on the page, `"floor"` the pane normal to the vertical
axis of the camera, and `"xy"`, `"xz"` or `"yz"` a pane by the two axes
it spans, as with the edge codes of rgl's `axis3d()`.

Plane typography needs the 'systemfonts' package, which reads the glyph
outlines, and character labels. Use screen orientation for plotmath
expressions.

## References

Kling M (2025). ggcube: 3D plotting with ggplot2.
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
arguments `rotate_labels`, `scale_depth` and `title_position`.

Danisch S, Krumbiegel J (2021). Makie.jl: Flexible high-performance data
visualization for Julia. Journal of Open Source Software 6(65), 3349.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 2, 1), z = 1:3)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
p + scale_z_continuous(guide = guide_axis_3d()) + coord_3d()
p + scale_z_continuous(guide = guide_axis_3d(position = "origin")) +
  coord_3d(origin = c(x = 2, y = 1.5))
p + ggplot2::guides(x = guide_axis_3d(label.orientation = "edge"),
                    y = guide_axis_3d(label.orientation = "edge")) +
  coord_3d()
if (requireNamespace("systemfonts", quietly = TRUE)) {
  p + ggplot2::guides(x = guide_axis_3d(label.orientation = "plane", ticks = "in_plane"),
                      y = guide_axis_3d(label.orientation = "plane", ticks = "in_plane")) +
    coord_3d(projection = "perspective")
}
p + ggplot2::guides(z = guide_axis_3d(title.position = "near", depth.scale = TRUE)) +
  coord_3d(projection = "perspective")
```
