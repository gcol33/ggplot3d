# Layers

A geom draws a shape from mapped data. A stat computes new values or
geometry before drawing. This article gives small examples of the geoms
and stats that draw data rows: points, paths, gridded surfaces, columns,
distributions, intervals and reference geometry. Installation is covered
in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).
[Surfaces, solids and
fields](https://gillescolling.com/ggplot3d/articles/surfaces-solids.md)
covers meshes, function and fitted surfaces, level surfaces, hulls,
solid glyphs and sweeps. [Vector
fields](https://gillescolling.com/ggplot3d/articles/vector-fields.md)
covers arrows and streamlines, and [Implicit
surfaces](https://gillescolling.com/ggplot3d/articles/implicit-surfaces.md)
covers reconstruction from point clouds.

## Points, paths, segments and text

Each row below contains a position and a label. Points draw one mark per
row. Paths connect rows in their input order within each group.

``` r

d <- data.frame(x = c(0, 1, 2), y = c(0, 2, 1), z = c(1, 3, 2),
                label = c("a", "b", "c"))
p <- ggplot(d, aes(x, y, z = z))
p + geom_point_3d(size = 3, colour = PAL["blue"]) + coord_3d()
```

![Three blue points in three
dimensions.](layers_files/figure-html/points-1.svg)

``` r

p + geom_path_3d(linewidth = 0.8, colour = PAL["blue"]) + coord_3d()
```

![A blue path connecting the same three positions in input
order.](layers_files/figure-html/paths-1.svg)

Segments use a start and an endpoint from each row. Text places labels
at the mapped positions. A dropline joins each position to the selected
face of the data box.

``` r

p + geom_segment_3d(aes(xend = x, yend = y, zend = 0),
                     linewidth = 0.8, colour = PAL["blue"]) + coord_3d()
```

![Three blue segments dropping to z equal to
zero.](layers_files/figure-html/segments-1.svg)

``` r

p + geom_text_3d(aes(label = label), size = 5) + coord_3d()
```

![The labels a, b and c at three
positions.](layers_files/figure-html/text-1.svg)

Text outlines can lie in a plane through each anchor. `normal` gives its
direction in normalized panel coordinates; `up` gives the reading-up
hint. The frame and aspect scaling transform those directions into the
displayed model. `angle` rotates within the plane.

``` r

p + geom_text_3d(aes(label = label), orientation = "plane",
  normal = c(1, 1, 1), up = c(0, 0, 1), size = 8) + coord_3d()
```

![Font outlines lying on tilted planes through three
anchors.](layers_files/figure-html/tilted-text-1.svg)

`facing = "camera_plane"` gives the outlines a shared view-facing
direction. `facing = "camera_point"` turns each label toward the
perspective eye from its own anchor. Each panel resolves its orientation
using its drawing camera; the HTML viewer updates it during orbit and
playback. The outlines participate in the scene’s depth testing and
picking.

``` r

p + geom_text_3d(aes(label = label), orientation = "plane",
  facing = "camera_point", size = 8) + coord_3d(projection = "perspective")
```

![Font outlines facing the perspective camera at three
anchors.](layers_files/figure-html/camera-text-1.svg)

``` r

p + geom_dropline_3d(face = "zmin", linewidth = 0.8) +
  geom_point_3d(size = 3) + coord_3d()
```

![Three points with lines connecting them to the floor of the
box.](layers_files/figure-html/droplines-1.svg)

## Polygons and bands

A polygon is an ordered ring of vertices in a plane after position
transformations; the triangle below lies on a sloping plane. Use a group
for each feature and a subgroup for each ring when a feature has holes.

``` r

triangle <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 1))
ggplot(triangle, aes(x, y, z = z)) +
  geom_polygon_3d(fill = PAL["blue"]) + coord_3d()
```

![A blue triangular polygon in a sloping
plane.](layers_files/figure-html/polygon-1.svg)

A band joins two boundaries. Mapping zmin and zmax produces a vertical
wall along the x/y path. Six endpoint aesthetics can describe a band
with boundaries that also differ in x and y.

``` r

band <- data.frame(x = seq(0, 4, length.out = 16), y = 0)
band$low <- sin(band$x)
band$high <- band$low + 0.5
ggplot(band, aes(x, y, zmin = low, zmax = high)) +
  geom_band_3d(fill = PAL["blue"]) + coord_3d()
```

![A blue vertical band following a sine
curve.](layers_files/figure-html/band-1.svg)

## Surfaces and contours

A gridded surface connects adjacent pairs of x/y coordinates with
triangles. Scattered points can use Delaunay triangulation, which
connects nearby points in the transformed x/y plane, with
`method = "delaunay"`. Within each group, each x/y pair must be unique.

``` r

surface <- expand.grid(x = seq(-2, 2, length.out = 10),
                       y = seq(-2, 2, length.out = 10))
surface$z <- with(surface, exp(-(x^2 + y^2) / 2))
s <- ggplot(surface, aes(x, y, z = z))
s + geom_surface_3d(fill = PAL["blue"]) + coord_3d()
```

![A curved blue surface sampled on a regular
grid.](layers_files/figure-html/surface-1.svg)

The surface stat exposes the triangulation step. The computed height
`z_mean` is the mean height of each triangle and can be mapped to fill
after the stat has run.

``` r

s + stat_surface_3d(aes(fill = after_stat(z_mean))) +
  scale_fill_gradient(low = "#DCEAF3", high = PAL["blue"]) + coord_3d()
```

![A curved surface coloured by each triangle's mean
height.](layers_files/figure-html/stat-surface-1.svg)

Contours trace equal heights. Filled contours draw bands between height
thresholds. The plane argument chooses the surface, the floor, or a
fixed height in original units.

``` r

s + geom_contour_3d(bins = 4, linewidth = 0.8) + coord_3d()
```

![Height contours on a curved
surface.](layers_files/figure-html/contours-1.svg)

``` r

s + geom_contour_filled_3d(bins = 4, plane = "floor") +
  scale_fill_gradientn(colours = unname(PAL[c("blue", "teal", "green", "orange", "purple")])) +
  coord_3d()
```

![Filled height bands projected onto the floor of the data
box.](layers_files/figure-html/filled-contours-1.svg)

## Images and textures

An image draped over a surface carries detail that would otherwise need
a mesh as fine as the image. `texture` takes a
[`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)
image, or anything it reads (a colour matrix, a numeric array from
[`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html), a PNG
file). Each face shows the image at its texture coordinates, which
default to the x and y positions spread over the image; `extent` places
the image in data units instead, and faces outside it keep their fill.

``` r

volcano_grid <- expand.grid(x = seq_len(nrow(volcano)), y = seq_len(ncol(volcano)))
volcano_grid$z <- as.vector(volcano)
coarse <- volcano_grid[volcano_grid$x %% 3 == 1 & volcano_grid$y %% 3 == 1, ]
checker <- matrix("#F2EFE6", 8, 8)
checker[(row(checker) + col(checker)) %% 2 == 1] <- "#2B5F75"
ggplot(coarse, aes(x, y, z = z)) +
  geom_surface_3d(texture = texture_3d(checker, interpolate = FALSE)) + coord_3d()
```

![The volcano height map draped with a checkerboard image whose squares
bend over the slopes.](layers_files/figure-html/drape-1.svg)

An image can also lie on a plane of the panel box.
[`geom_raster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_raster_3d.md)
turns values on a regular grid into one image, one per level of the
remaining axis, and
[`annotation_raster_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_raster_3d.md)
lays a given image on a plane. Both are single faces of the scene, so
they are foreshortened and hidden like any other face; here the
full-resolution height map lies on the floor under the coarse surface.

``` r

ggplot(coarse, aes(x, y, z = z)) +
  geom_raster_3d(aes(fill = z), data = volcano_grid, at = -Inf) +
  annotation_raster_3d(matrix(grDevices::hcl.colors(8, "Zissou 1"), 1), plane = "xz", at = Inf) +
  geom_surface_3d(fill = "grey85", colour = NA) +
  scale_fill_viridis_c() +
  coord_3d()
```

![The coarse volcano surface above a full-resolution coloured height map
on the floor of the box and a colour ramp image on its back
wall.](layers_files/figure-html/raster-planes-1.svg)

## Columns, counts and extrusions

Columns occupy cells on an x/y lattice and extend from a baseline to z.
A bar layer counts observations at each x/y combination. The count stat
gives direct access to that calculation.

``` r

columns <- expand.grid(x = 1:3, y = 1:3)
columns$z <- seq_len(nrow(columns))
ggplot(columns, aes(x, y, z = z)) +
  geom_col_3d(fill = PAL["blue"]) + coord_3d()
```

![Nine blue columns with heights from one to
nine.](layers_files/figure-html/columns-1.svg)

``` r

counts <- ggplot(mtcars, aes(factor(cyl), factor(gear)))
counts + geom_bar_3d(fill = PAL["blue"]) + coord_3d()
```

![Columns showing counts of cars by cylinder and gear
categories.](layers_files/figure-html/bars-1.svg)

``` r

counts + stat_count_3d(fill = PAL["blue"]) + coord_3d()
```

![The count stat drawing counts of cars by cylinder and gear
categories.](layers_files/figure-html/stat-count-1.svg)

Two continuous variables are binned first.
[`geom_histogram_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
counts observations in rectangles and
[`geom_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
in hexagons, drawn as hexagonal prisms.
[`stat_summary_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
and
[`stat_summary_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
bin the same way and set each height to a summary of z, the mean by
default.

``` r

ggplot(faithful, aes(eruptions, waiting)) +
  geom_hex_3d(aes(fill = after_stat(count)), bins = 12) + coord_3d()
```

![Hexagonal prisms counting Old Faithful eruptions by duration and
waiting time, coloured by count.](layers_files/figure-html/hex-1.svg)

Extrusion extends an ordered x/y polygon between two heights. The z
value gives its endpoint. The `base` argument supplies the starting
height; a mapped zmin overrides it. Caps close the top and bottom.

``` r

ring <- data.frame(x = c(0, 2, 2, 0), y = c(0, 0, 1, 1), z = 2)
ggplot(ring, aes(x, y, z = z)) + geom_extrude_3d(fill = PAL["blue"]) +
  coord_3d()
```

![A blue rectangular polygon extruded to a height of
two.](layers_files/figure-html/extrusion-1.svg)

Stacking accumulates column heights separately above and below zero. It
requires a linear continuous z scale and a shared zero baseline. Nudging
shifts positions in transformed units.

``` r

stack <- data.frame(x = 1, y = 1, z = c(2, 3), part = c("a", "b"))
ggplot(stack, aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_stack_3d()) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange")])) + coord_3d()
```

![Two coloured column sections stacked at a single x/y
position.](layers_files/figure-html/positions-1.svg)

## Distributions at x/y positions

The iris data record flower measurements in centimetres for three
species. Place each species at one x position and use petal length as
the distribution along z. A boxplot draws a prism between the quartiles,
a median face, whiskers and individual outliers.

``` r

flowers <- ggplot(iris, aes(Species, y = 0, z = Petal.Length, fill = Species)) +
  scale_y_continuous(limits = c(-1, 1), breaks = NULL, name = NULL)
flowers + geom_boxplot_3d(width = 0.6, depth = 0.4) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) +
  coord_3d()
```

![Three box prisms summarising petal length for the iris species, with
whiskers and outlier points.](layers_files/figure-html/boxplots-1.svg)

A violin shows the estimated density along z. The default wall mirrors
that density along x; a revolved violin surrounds the vertical axis. Use
discrete positions, or an explicit group for each numeric x/y cell.

``` r

flowers + geom_violin_3d(shape = "wall", n = 32) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) +
  coord_3d()
```

![Three flat violin walls showing the petal-length distributions of the
iris species.](layers_files/figure-html/violin-wall-1.svg)

``` r

flowers + geom_violin_3d(shape = "revolve", n = 32, sides = 12) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) +
  coord_3d()
```

![The same three petal-length densities drawn as solid violins around
their vertical axes.](layers_files/figure-html/violin-solid-1.svg)

## Waterfalls and stacked areas

A waterfall draws each group as a curtain from a baseline to its
heights, ordered by x. Take three cross-sections of the built-in volcano
elevation matrix. Each section occupies its own y position, with
elevations measured in metres.

``` r

profiles <- expand.grid(x = seq(1, nrow(volcano), by = 4), y = c(15, 30, 45))
profiles$z <- volcano[cbind(profiles$x, profiles$y)]
ggplot(profiles, aes(x, y, z = z, group = y, fill = factor(y))) +
  geom_waterfall_3d(base = 90) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) +
  coord_3d()
```

![Three filled elevation profiles from the volcano matrix, separated
along y and rising from a 90 metre
baseline.](layers_files/figure-html/waterfall-1.svg)

An area wall stacks groups that share an x lattice and a y position. The
built-in US personal expenditure table records spending in billions of
dollars. Place its five spending categories on one wall, with year along
x and expenditure along z. Positive and negative values stack separately
from zero.

``` r

spending <- as.data.frame(as.table(USPersonalExpenditure))
names(spending) <- c("category", "year", "expenditure")
spending$year <- as.numeric(as.character(spending$year))
ggplot(spending, aes(year, y = 0, z = expenditure,
                     group = category, fill = category)) +
  geom_area_3d() + scale_fill_manual(values = unname(PAL[1:5]),
    labels = c("Food & tobacco", "Household", "Medical", "Personal care", "Education")) +
  guides(fill = guide_legend(nrow = 3)) +
  coord_3d() + theme(legend.position = "bottom")
```

![Five spending categories stacked in a single wall over the five years
of the US personal expenditure
table.](layers_files/figure-html/stacked-area-1.svg)

The default area position is
[position_stack_3d()](https://gillescolling.com/ggplot3d/reference/position_stack_3d.md).
Choose
[position_fill_3d()](https://gillescolling.com/ggplot3d/reference/position_stack_3d.md)
to show proportions at each x position. Wall stacking requires a linear
continuous z scale and a common zero baseline.

## Intervals and summaries

An interval layer accepts lower and upper bounds on any combination of
x, y and z. Summarise iris petal length at each species position with a
mean and an interval extending one standard error on either side.

``` r

ggplot(iris, aes(Species, y = 0, z = Petal.Length)) +
  stat_summary_3d(fun.data = mean_se, colour = PAL["blue"], linewidth = 0.8) +
  coord_3d()
```

![Mean petal length with one-standard-error point ranges for each iris
species.](layers_files/figure-html/summary-intervals-1.svg)

Summary tables containing cells with a single observation can currently
fail with `numbers of columns of arguments do not match`. For those
tables, compute the summaries first and pass their bounds to an interval
layer.

For supplied intervals, map both bounds of each axis. Here a small table
gives three measured positions with stated x, y and z uncertainties.
Cross-shaped caps extend along both perpendicular axes, so the endpoints
remain visible as the camera turns.

``` r

measurements <- data.frame(x = c(1, 2, 3), y = c(2, 1, 3), z = c(3, 4, 2))
intervals <- ggplot(measurements, aes(x, y, z = z,
  xmin = x - 0.2, xmax = x + 0.2, ymin = y - 0.3, ymax = y + 0.3,
  zmin = z - 0.5, zmax = z + 0.5))
intervals + geom_errorbar_3d(width = 0.12, cap.plane = "cross",
                            linewidth = 0.8) +
  geom_point_3d(size = 2.5, colour = PAL["blue"]) + coord_3d()
```

![Three measured positions with capped uncertainty intervals along all
three axes.](layers_files/figure-html/errorbars-1.svg)

[geom_linerange_3d()](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
draws the interval segments, and
[geom_pointrange_3d()](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
adds their centres. A crossbar draws a box and marks the centre with a
line. Restrict this example to z bounds to see that box clearly.

``` r

ggplot(measurements, aes(x, y, z = z, zmin = z - 0.5, zmax = z + 0.5)) +
  geom_crossbar_3d(width = 0.25, cap.plane = "cross", fill = PAL["blue"]) +
  coord_3d()
```

![Three crossbar boxes spanning the supplied z intervals, with a line at
each centre.](layers_files/figure-html/crossbars-1.svg)

A stem connects each position to a fixed value on one axis. Here each
car’s fuel consumption is connected to zero. Change the base or map an
endpoint to give individual stems different starting values.

``` r

ggplot(mtcars, aes(wt, hp, z = mpg)) +
  geom_stem_3d(base = 0, colour = PAL["blue"], linewidth = 0.7) + coord_3d()
```

![Fuel consumption for the cars as vertical stems from zero, with a
point at each observed value.](layers_files/figure-html/stems-1.svg)

## Reference lines and dimensions

Fix two intercepts to draw a reference line along the remaining axis.
For an arbitrary line, supply a point and direction in transformed scale
units; the line is cut to the box.

``` r

p + geom_point_3d(size = 3) +
  geom_refline_3d(xintercept = 1, yintercept = 1, colour = PAL["red"]) +
  geom_abline_3d(point = c(0, 0, 1), direction = c(1, 1, 1),
                 colour = PAL["blue"], linetype = 2) +
  annotate_3d("text", x = 1, y = 1, z = 3, label = "reference", size = 5) +
  coord_3d()
```

![Three points with a vertical reference line and an oblique diagonal
through the box.](layers_files/figure-html/reference-lines-1.svg)

Fixed segments can be added with
[annotate_3d()](https://gillescolling.com/ggplot3d/reference/annotate_3d.md).
Supply all three coordinates at each end. This arrow joins the first and
last positions in the point table.

``` r

p + geom_point_3d(size = 3) +
  annotate_3d("segment", x = 0, y = 0, z = 1,
    xend = 2, yend = 1, zend = 2, colour = PAL["blue"], linewidth = 0.8,
    arrow = grid::arrow(length = grid::unit(3, "mm"))) + coord_3d()
```

![Three points with a blue arrow joining the first position at zero,
zero, one to the last at two, one,
two.](layers_files/figure-html/annotation-segment-1.svg)

Annotate a fixed span with `annotate_3d("bracket", ...)`. The two
endpoints use data coordinates; the offset uses fractions of the box
dimensions. These examples label a supplied distance, with extension
lines and arrowheads on the dimension version. Use
[geom_bracket_3d()](https://gillescolling.com/ggplot3d/reference/geom_bracket_3d.md)
to map spans from a data table.

``` r

span <- ggplot(data.frame(x = c(0, 2), y = 0, z = 1), aes(x, y, z = z)) +
  geom_point_3d(size = 3)
span + annotate_3d("bracket", x = 0, y = 0, z = 1, xend = 2, yend = 0, zend = 1,
  label = "span", size = 5, offset = c(0, 0, 0.12)) +
  coord_3d(clip_data = FALSE)
```

![Two points connected by a bracket labelled
span.](layers_files/figure-html/bracket-1.svg)

``` r

span + annotate_3d("bracket", x = 0, y = 0, z = 1, xend = 2, yend = 0, zend = 1,
  type = "dimension", label = "2 m", size = 5,
  offset = c(0, -0.12, 0)) + coord_3d(clip_data = FALSE)
```

![Two points with a dimension line and extension lines labelled 2
m.](layers_files/figure-html/dimension-1.svg)

## Reference and clipping planes

A reference plane marks a threshold through the box. An oblique plane
takes a normal vector and a point on the plane; for the plane z = a + b
x + c y, use the normal (-b, -c, 1) and point (0, 0, a).

``` r

s + geom_surface_3d(fill = PAL["blue"]) +
  geom_plane_3d(zintercept = 0.5, fill = PAL["orange"], alpha = 0.4) + coord_3d()
```

![The curved surface intersected by a translucent horizontal plane at
height one half.](layers_files/figure-html/reference-plane-1.svg)

A clipping plane keeps the half-space its normal points into. Close a
cut solid by enabling its cap. Here the normal points towards positive
y, leaving the part of a sphere at y greater than or equal to zero.

``` r

ggplot(data.frame(x = 0, y = 0, z = 0), aes(x, y, z = z)) +
  geom_sphere_3d(radius = 1, detail = 2, fill = PAL["teal"]) +
  coord_3d(aspect = "data", clip_planes = list(
    clip_plane_3d(normal = c(0, 1, 0), point = c(y = 0), cap = TRUE)))
```

![A sphere cut in half at y equal to zero, with a flat cap closing the
cut.](layers_files/figure-html/clipping-plane-1.svg)

The coordinate’s clipping planes apply to every layer. A layer’s own
plane list replaces that list; an empty list keeps the layer whole. Open
surfaces receive caps only where the cut forms a closed outline. See
[clip_plane_3d()](https://gillescolling.com/ggplot3d/reference/clip_plane_3d.md)
for the plane conventions.

## Adapting a layer

The layer adapter equips a registered ggplot2 geom with
three-dimensional scene emission. Extension authors register an emitter
to describe how another geom produces geometry. The registration help
page describes the callback arguments.

``` r

p + layer_3d(geom_point(size = 3)) + coord_3d()
```

![Three points drawn by an adapted ggplot2 point
layer.](layers_files/figure-html/adapter-1.svg)
