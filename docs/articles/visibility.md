# Visibility

Visibility determines which parts of three-dimensional geometry can be
seen from a camera. This article explains overlapping surfaces, drawing
order and finite-width strokes. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).

## Overlapping geometry

Occlusion occurs when nearer geometry covers farther geometry in the
projected image. A panel scene combines geometry from its
three-dimensional layers before resolving visibility. An intersection
can make different parts of a surface lie in front of and behind another
surface.

``` r

a <- expand.grid(x = seq(-1, 1, length.out = 8),
                 y = seq(-1, 1, length.out = 8))
a$z <- a$x
b <- a
b$z <- -b$x
ggplot(a, aes(x, y, z = z)) +
  geom_surface_3d(fill = PAL["blue"]) +
  geom_surface_3d(data = b, fill = PAL["orange"]) + coord_3d(elevation = 60)
```

![Two intersecting planes, each covering part of the other in the
projected view.](visibility_files/figure-html/crossing-1.svg)

Camera projection places the scene’s faces, strokes, point marks and
text on the page and assigns their depth along the viewing direction.
Visibility refers to this represented geometry, including the triangles
used to approximate a curved surface.

## The occlusion graph

An occlusion graph records drawing dependencies. Each node represents a
piece of geometry. A directed edge says that one piece must be drawn
before another where their projected areas overlap. Following these
dependencies produces an order from the back of the scene towards the
camera.

Some configurations create a cycle: A covers part of B, B covers part of
C, and C covers part of A. Intersections can also require a change of
drawing order within a face. Splitting the affected geometry supplies
smaller pieces that can be ordered. Geometry whose overlap relationships
already admit an order can stay intact.

The vector renderer uses this ordering and localized splitting to draw
the scene. This belongs to the family of visibility methods discussed by
Snyder and Lengyel [1998](https://doi.org/10.1145/280814.280878). Their
paper studies visibility ordering and compositing for image layers. It
provides background for thinking about occlusion dependencies.

## Splitting geometry

Splitting inserts vertices where geometry crosses a dividing plane. The
resulting pieces preserve the represented surface. The classical
hidden-surface method of Newell, Newell and Sancha
[1972](https://doi.org/10.1145/800193.569954) and the spatial
partitioning method of Fuchs, Kedem and Naylor
[1980](https://doi.org/10.1145/800250.807481) establish related
approaches to obtaining a visible drawing order.

Localized splitting limits the extra geometry to relationships that need
it, though a dense collection of interpenetrating shapes may still
require many pieces. Choose the raster backend in [Output
backends](https://gillescolling.com/ggplot3d/articles/backends.md) when
the intended output is a fixed-size image of dense geometry.

## Strokes and marks

A stroke covers an area determined by its width, joins and end caps. Its
visible area can extend on either side of the path coordinates. Nehab
[2020](https://doi.org/10.1145/3386569.3392392) explains how stroked
paths define filled regions and why joins and caps matter.

Point marks also have a footprint on the page. Visibility near the
boundary of a surface therefore depends on the mark’s size. Physical
output dimensions affect these footprints, so inspect the plot at its
intended size.

``` r

points <- data.frame(x = c(-0.5, 0, 0.5), y = c(0, 0.5, 0),
                     z = c(0.6, -0.4, 0.7))
ggplot(a, aes(x, y, z = z)) + geom_surface_3d(fill = PAL["blue"]) +
  geom_segment_3d(data = points, aes(xend = x, yend = y, zend = -1),
                  linewidth = 0.8) +
  geom_point_3d(data = points, size = 4, colour = PAL["red"]) +
  coord_3d(elevation = 60)
```

![A blue surface with red observations and vertical black segments
extending through it.](visibility_files/figure-html/strokes-1.svg)

## Transparency and overlays

An opacity below one lets farther geometry contribute to the image. The
order of contributions affects the resulting colour. Exact geometric
ordering describes the represented shapes; finite raster sampling and
device antialiasing also affect boundary pixels.

The layer adapter accepts `occlusion = "overlay"` for geometry intended
to be drawn above the depth-tested scene. Use that setting for
annotations whose role requires them to remain visible.

## References

- Snyder, J. and Lengyel, J. (1998). Visibility Sorting and Compositing
  Without Splitting for Image Layer Decompositions. SIGGRAPH, 219-230.
  [doi:10.1145/280814.280878](https://doi.org/10.1145/280814.280878).

- Newell, M. E., Newell, R. G. and Sancha, T. L. (1972). A Solution to
  the Hidden Surface Problem. ACM Annual Conference, 443-450.
  [doi:10.1145/800193.569954](https://doi.org/10.1145/800193.569954).

- Fuchs, H., Kedem, Z. M. and Naylor, B. F. (1980). On Visible Surface
  Generation by A Priori Tree Structures. SIGGRAPH, 124-133.
  [doi:10.1145/800250.807481](https://doi.org/10.1145/800250.807481).

- Nehab, D. (2020). Converting Stroked Primitives to Filled Primitives.
  ACM Transactions on Graphics 39(4).
  [doi:10.1145/3386569.3392392](https://doi.org/10.1145/3386569.3392392).
