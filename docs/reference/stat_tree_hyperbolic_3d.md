# Deterministic tree layouts in hyperbolic space

Takes directed from/to edges forming a rooted tree. Children are sorted
by node key. In H^2 a regular star is placed at each parent by a Lorentz
translation, as in Sarkar's construction. Its default edge length
exceeds the regular-star Delaunay separation bound. In H^3 children are
distributed on the outward hemisphere using equal-area golden-angle
samples, following the hemisphere placement used by Munzner. Every edge
has the requested intrinsic length. Coordinates are returned in the
Poincare ball.

## Usage

``` r
stat_tree_hyperbolic_3d(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  what = c("edges", "nodes"),
  dimension = 3L,
  edge_length = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- what:

  Draw edges or nodes.

- dimension:

  Two for a disk, three for a ball.

- edge_length:

  Positive intrinsic edge length; NULL chooses one for H^3 or the
  regular-star separation bound plus one for H^2.

## Value

A segment or point layer; add
[`coord_hyperbolic_3d()`](https://gillescolling.com/ggplot3d/reference/coord_hyperbolic_3d.md).

## References

Munzner (1997). H3. <https://graphics.stanford.edu/papers/h3/>. Sarkar
(2011). Low distortion Delaunay embedding of trees in hyperbolic plane.
<https://homepages.inf.ed.ac.uk/rsarkar/papers/HyperbolicDelaunayFull.pdf>.

## Examples

``` r
edges <- data.frame(from=c("a","a","b","b"),to=c("b","c","d","e"))
ggplot2::ggplot(edges)+stat_tree_hyperbolic_3d()+
  stat_tree_hyperbolic_3d(what="nodes",size=3)+coord_hyperbolic_3d()
```
