# Labels with boxes and labels that repel each other

`geom_label_3d()` draws text on a rounded box at each 3D position, like
[`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
`geom_text_repel_3d()` and `geom_label_repel_3d()` move each label away
from the other labels and from the data points, and join a moved label
to its point with a leader line, as in the ggrepel package's
`geom_text_repel()` and `geom_label_repel()`.

## Usage

``` r
geom_label_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  label.padding = grid::unit(0.25, "lines"),
  label.r = grid::unit(0.15, "lines"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_text_repel_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  box.padding = 0.25,
  point.padding = 1e-06,
  min.segment.length = 0.5,
  force = 1,
  force_pull = 1,
  max.time = Inf,
  max.iter = 10000,
  max.overlaps = getOption("ggplot3d.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  direction = c("both", "y", "x"),
  seed = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_label_repel_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  box.padding = 0.25,
  label.padding = grid::unit(0.25, "lines"),
  point.padding = 1e-06,
  label.r = grid::unit(0.15, "lines"),
  min.segment.length = 0.5,
  force = 1,
  force_pull = 1,
  max.time = Inf,
  max.iter = 10000,
  max.overlaps = getOption("ggplot3d.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  direction = c("both", "y", "x"),
  seed = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- parse:

  Whether to parse labels as plotmath expressions.

- label.padding, label.r:

  Padding around the text and corner radius of the box, as grid units or
  numbers of lines.

- box.padding:

  Space kept free around each label box, as a grid unit or a number of
  lines (default 0.25).

- point.padding:

  Space kept free around each data point, as a grid unit or a number of
  lines.

- min.segment.length:

  Leader lines shorter than this are not drawn, as a grid unit or a
  number of lines (default 0.5). `Inf` draws none, 0 draws every leader
  of a label that does not cover its point.

- force:

  Strength of the push between overlapping labels and between a label
  and a data point it covers (default 1).

- force_pull:

  Strength of the pull of each label back to its point (default 1).

- max.time:

  Maximum solver time in seconds per panel. The default `Inf` leaves the
  iteration count as the only limit, so a seeded layout is the same on
  every machine.

- max.iter:

  Maximum number of solver iterations (default 10000).

- max.overlaps:

  A label that overlaps more than this number of other labels and data
  points where it starts is not drawn. Defaults to the option
  `ggplot3d.max.overlaps`, or 10.

- nudge_x, nudge_y:

  Starting offset of each label from its point, in millimetres on the
  page (default 0).

- xlim, ylim:

  Range the labels stay within, as fractions of the panel width and
  height. `NA` stands for the panel edge.

- direction:

  Move labels in both directions (default), or only along `"x"` or `"y"`
  of the page.

- seed:

  Seed of the random starting directions. `NA` (default) draws one from
  R's random number generator at each draw.

## Value

A ggplot2 layer.

## Details

Labels are placed on the page when the panel is drawn, after projection,
so they never overlap whatever the camera angle. A force-directed solver
in C++ moves the label boxes: overlapping boxes push each other apart
along the line between their centres, every data point of the layer
pushes boxes off its disc of radius `point.size` plus `point.padding`,
and a spring pulls each box back until it touches its own point. Each
step keeps part of the previous velocity, which damps oscillation. The
solver stops once no box overlaps and the boxes are at rest.

Rows with an empty or missing label are not drawn but still push labels
away. The repelled labels and their leader lines are drawn over the
scene, not hidden by surfaces in front of their points.
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
starts each frame's labels from their offsets in the previous frame,
matched by label text, so labels keep their side of the point while the
camera moves.
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
does not show repelled labels.

Extra aesthetics of the repel layers: `point.size` (size of the data
points to keep clear of, in the units of the `size` aesthetic of points,
default 1.5), `segment.colour`, `segment.size`, `segment.alpha` and
`segment.linetype` (leader colour, width in mm, opacity and line type;
default the label colour, 0.5, the label opacity and solid).

## Examples

``` r
d <- data.frame(x = c(1, 1.1, 1.2, 2, 3), y = c(1, 1.1, 1, 2, 1),
                z = c(1, 1.2, 1.1, 2, 3), name = c("alpha", "beta", "gamma", "delta", "epsilon"))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = name)) + geom_point_3d()
p + geom_label_3d(fill = "white") + coord_3d()
p + geom_text_repel_3d(seed = 1) + coord_3d()
p + geom_label_repel_3d(seed = 1, segment.colour = "grey40") + coord_3d()
```
