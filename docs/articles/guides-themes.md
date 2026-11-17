# Styling axes, legends and panes

Axes and legends connect the displayed geometry to the variables in a
table. This article labels a height surface, places its colourbar beside
the z axis and adjusts the surrounding panes and grids. It assumes the
mappings introduced in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).
[Choosing camera
views](https://gillescolling.com/ggplot3d/articles/camera.md) explains
the view angles used here.

1.  **Label** the axes with the position scales and
    [`guide_axis_3d()`](https://gillescolling.com/ggplot3d/reference/guide_axis_3d.md).

2.  **Align** a colourbar with the z axis with
    [`guide_colourbar_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md).

3.  **Style** the panes and grids with
    [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Example

``` r

library(ggplot2)
library(ggplot3d)
```

### Labelling positions

The surface is a regular grid with a height computed at every x/y pair.
Scales choose the breaks and their text;
[guide_axis_3d()](https://gillescolling.com/ggplot3d/reference/guide_axis_3d.md)
places that text beside the projected box. Automatic placement selects
an exterior edge for the current view.

``` r

surface <- expand.grid(x = seq(-1, 1, length.out = 12),
                       y = seq(-1, 1, length.out = 12))
surface$z <- with(surface, x + y / 2)
p <- ggplot(surface, aes(x, y, z = z, fill = z)) +
  geom_surface_3d(colour = NA) +
  labs(x = "East", y = "North", z = "Height")
p + guides(fill = "none", x = guide_axis_3d(label_orientation = "edge"),
           y = guide_axis_3d(label_orientation = "edge")) +
  scale_z_continuous(breaks = c(-1, 0, 1)) + coord_3d()
```

![A sloping surface labelled East, North and Height, with tick labels
following the x and y
edges.](guides-themes_files/figure-html/axes-1.svg)

Screen-oriented tick labels remain upright on the page. Edge orientation
rotates them along the projected axis. Plane orientation uses font
outlines that participate in scene visibility and requires the
systemfonts package. Use fewer scale breaks or additional label rows
when labels crowd together. Setting an axis title to `NULL` through
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) removes
that title.

### Matching a colourbar to height

When fill and height map the same expression,
[guide_colourbar_3d()](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
can place the colourbar beside the z axis. The axis supplies the breaks,
labels and title. Both mappings below use the column z.

``` r

p + scale_fill_gradient(low = "#DCEAF3", high = "#2E6F9E",
      guide = guide_colourbar_3d(along = "z")) +
  scale_z_continuous(breaks = c(-1, 0, 1)) + coord_3d()
```

![A height-coloured surface with its colour scale aligned to the z
axis.](guides-themes_files/figure-html/axis-colourbar-1.svg)

The mapped expression must match in every layer using that colour scale.
For a colour representing another variable, set `along = NULL` to use a
separate legend. Adding the shading range shows how lighting changes
each fill colour.

``` r

p + scale_fill_gradient(low = "#DCEAF3", high = "#2E6F9E",
      breaks = c(-1, 0, 1), name = "Height",
      guide = guide_colourbar_3d(along = NULL, shading_range = TRUE)) +
  coord_3d()
```

![A height-coloured surface with a separate colourbar showing the range
from shaded to illuminated
colours.](guides-themes_files/figure-html/legend-1.svg)

Discrete fills use
[guide_legend_3d()](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md).
Binned scales use the colour-step or bin guides from the same family.
[Lighting and styling
surfaces](https://gillescolling.com/ggplot3d/articles/lighting.html#legends-that-show-the-shading)
demonstrates their keys and the lighting controls.

### Styling panes and grids

Theme elements for the z axis inherit the general axis settings. Pane
names identify their normal axis: the z pane lies in the x/y plane. Grid
element names identify both the measured axis and the plane containing
the grid line.

``` r

p + guides(fill = "none") + coord_3d() +
  theme(axis.title.z = element_text(colour = "#2E6F9E"),
        panel.pane.z = element_rect(fill = "#DCEAF3", colour = NA),
        panel.grid.major.x.xy = element_blank(),
        panel.grid.minor = element_blank(),
        panel.box = element_line(linewidth = 0.5))
```

![A surface above a pale floor pane, with the x grid removed from that
floor and the height title coloured
blue.](guides-themes_files/figure-html/panes-1.svg)

| Element                 | Applies to                   |
|-------------------------|------------------------------|
| `axis.text.z`           | z tick labels                |
| `axis.title.z`          | z title                      |
| `panel.pane.x`          | y/z pane                     |
| `panel.pane.y`          | x/z pane                     |
| `panel.pane.z`          | x/y pane                     |
| `panel.grid.major.z.xz` | z grid lines in the x/z pane |
| `panel.box.front`       | front box edges              |

### Drawing axes through a data point

An origin axis passes through a supplied data position. Its lines and
ticks participate in scene visibility, so data can pass in front of them
or behind them. The following helix winds twice around the z axis; all
three axes pass through zero and the surrounding box is removed.

``` r

turns <- seq(-1, 1, length.out = 120)
helix <- data.frame(x = cos(2 * pi * turns), y = sin(2 * pi * turns),
                    z = turns)
ggplot(helix, aes(x, y, z = z)) +
  geom_path_3d(colour = "#2E6F9E", linewidth = 0.9) +
  scale_x_continuous(breaks = c(-1, 1), labels = c("-1", "x = 1")) +
  scale_y_continuous(breaks = c(-1, 1), labels = c("-1", "y = 1")) +
  scale_z_continuous(breaks = c(-1, 1), labels = c("-1", "z = 1")) +
  labs(x = NULL, y = NULL, z = NULL) +
  coord_3d(axis_lines = "origin", origin = c(x = 0, y = 0, z = 0),
           panes = character(), box = "none")
```

![A helix winding twice around the z axis, with three axes crossing at
the origin inside it.](guides-themes_files/figure-html/origin-1.svg)

Use `guide_axis_3d(position = "origin")` on an individual axis to move
only that guide. [Scaling heights and facet
ranges](https://gillescolling.com/ggplot3d/articles/z-scale.md) covers
transformed breaks and facet ranges, and [Embedding plots in
reports](https://gillescolling.com/ggplot3d/articles/documents.html#accessible-descriptions)
shows how to add a description of the finished figure.
