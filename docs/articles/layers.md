# Layers

A geom draws a shape from mapped data. A stat computes new values or
geometry before drawing. This article gives a small example of every
three-dimensional geom and stat. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).

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

## Indexed meshes

A mesh describes vertices and faces using integer indices. Indices start
at one. Each row in the face matrix identifies the corners of a triangle
or quadrilateral. The mesh constructor returns a corner table keyed by
vertex, face and corner.

``` r

vertices <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1),
                       z = c(0, 0, 1, 1))
mesh <- mesh_3d(vertices, matrix(1:4, nrow = 1))
ggplot() + geom_mesh_3d(data = mesh, fill = PAL["blue"]) + coord_3d()
```

![A sloping quadrilateral supplied as an indexed
mesh.](layers_files/figure-html/mesh-1.svg)

## Function and fitted surfaces

Function surfaces sample a vectorized function of x and y. The grid is
evenly spaced in transformed x and y coordinates. Function inputs and
returned z values use original data units. The z scale transforms the
returned heights.

``` r

ggplot() + geom_function_3d(fun = function(x, y) sin(x) * cos(y), n = 10) +
  xlim(-2, 2) + ylim(-2, 2) + coord_3d()
```

![A sampled sine-cosine
surface.](layers_files/figure-html/function-1.svg)

``` r

ggplot() + stat_function_3d(fun = function(x, y) x^2 + y^2, n = 10) +
  xlim(-1, 1) + ylim(-1, 1) + coord_3d()
```

![The function stat sampling a quadratic
surface.](layers_files/figure-html/stat-function-1.svg)

Fitted surfaces model z using x and y. Fitting and prediction use
transformed positions. The linear model’s default formula is z as a
function of x plus y. Confidence surfaces, when requested, describe
pointwise uncertainty in the conditional mean.

``` r

fit_data <- surface
fit_data$z <- with(fit_data, x + y + sin(3 * x * y) / 5)
f <- ggplot(fit_data, aes(x, y, z = z))
f + geom_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()
```

![A fitted linear surface for a grid with a deterministic curved
deviation.](layers_files/figure-html/smooth-1.svg)

``` r

f + stat_smooth_3d(method = "lm", se = FALSE, n = 8) + coord_3d()
```

![The smooth stat drawing a fitted linear response
surface.](layers_files/figure-html/stat-smooth-1.svg)

## Density surfaces and ridges

A bivariate density surface estimates how observations are distributed
over x and y. Its height is density, computed with a Gaussian product
kernel. The bandwidth controls smoothing along each input axis.

``` r

ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_2d_3d(n = 12) + coord_3d()
```

![A density surface for eruption duration and waiting time in the
faithful data.](layers_files/figure-html/density2d-1.svg)

Ridgelines estimate a one-dimensional density for each group and place
the groups along the perpendicular axis. Here each species occupies a
single y position. Height multiplies the density in transformed z units.

``` r

r <- ggplot(iris, aes(Sepal.Length, as.numeric(Species),
                     group = Species, fill = Species))
r + geom_ridgeline_3d(n = 24) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) + coord_3d()
```

![Three density ridges for sepal length, one per iris
species.](layers_files/figure-html/ridge-1.svg)

``` r

r + stat_ridgeline_3d(n = 24) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange", "teal")])) + coord_3d()
```

![The ridgeline stat drawing the same species-specific sepal length
densities.](layers_files/figure-html/stat-ridge-1.svg)

## Scalar-field level surfaces

A scalar field assigns a numeric value to each x/y/z position. An
isosurface joins positions with the same field value. Supply a complete
rectilinear grid and map the field to `value`; missing field values mask
their incident cells.

``` r

field <- expand.grid(x = seq(-1, 1, length.out = 8),
  y = seq(-1, 1, length.out = 8), z = seq(-1, 1, length.out = 8))
field$value <- with(field, x^2 + y^2 + z^2)
v <- ggplot(field, aes(x, y, z = z, value = value))
v + geom_isosurface_3d(levels = 0.6, fill = PAL["blue"]) + coord_3d()
```

![A level surface of squared distance from the
origin.](layers_files/figure-html/isosurface-1.svg)

``` r

v + stat_isosurface_3d(levels = 0.6, fill = PAL["blue"]) + coord_3d()
```

![The isosurface stat extracting the same squared-distance
threshold.](layers_files/figure-html/stat-isosurface-1.svg)

A function can supply field values directly. It receives transformed
coordinates in all three axes. Grid resolution sets how finely the
function is sampled.

``` r

ball <- function(x, y, z) x^2 + y^2 + z^2
ggplot() + geom_isofunction_3d(fun = ball, n = 8, levels = 0.5) +
  xlim(-1, 1) + ylim(-1, 1) + zlim(-1, 1) + coord_3d()
```

![A spherical level surface sampled from a three-variable
function.](layers_files/figure-html/isofunction-1.svg)

``` r

ggplot() + stat_isofunction_3d(fun = ball, n = 8, levels = 0.5) +
  xlim(-1, 1) + ylim(-1, 1) + zlim(-1, 1) + coord_3d()
```

![The isofunction stat sampling the squared-distance
function.](layers_files/figure-html/stat-isofunction-1.svg)

## Trivariate density

A trivariate density estimates the distribution of observations in three
dimensions. Numeric levels select density thresholds. Probability
contents, specified with
[`probs()`](https://gillescolling.com/ggplot3d/reference/probs.md),
select enclosed mass estimated from the sampled grid. The grid ranges
need to include the density tails for probability-content requests.

``` r

set.seed(4)
cloud <- data.frame(x = rnorm(40), y = rnorm(40), z = rnorm(40))
c <- ggplot(cloud, aes(x, y, z = z))
c + geom_isodensity_3d(h = 0.7, levels = 0.02, n = 8) + coord_3d()
```

![A density level surface around a deterministic three-dimensional point
sample.](layers_files/figure-html/isodensity-1.svg)

``` r

c + stat_density_3d(h = 0.7, levels = 0.02, n = 8) + coord_3d()
```

![The density stat extracting a level surface from the same
sample.](layers_files/figure-html/stat-density-1.svg)

``` r

probs(0.5, 0.9)
#> [1] 0.5 0.9
#> attr(,"class")
#> [1] "ggplot3d_probs"
```

## Convex hulls

A convex hull is the smallest convex shape containing the input
positions. This example uses the eight corners of a cube. Coplanar input
produces a planar polygon, and collinear input produces no faces.

``` r

corners <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
h <- ggplot(corners, aes(x, y, z = z))
h + geom_hull_3d(fill = PAL["blue"]) + coord_3d()
```

![The convex hull of the eight corners of a
cube.](layers_files/figure-html/hull-1.svg)

``` r

h + stat_hull_3d(fill = PAL["blue"]) + coord_3d()
```

![The hull stat drawing the cube defined by the same eight
corners.](layers_files/figure-html/stat-hull-1.svg)

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
