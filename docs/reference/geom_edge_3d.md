# Network edges and nodes

`geom_edge_3d()` draws one edge per row from (x, y, z) to (xend, yend,
zend), as a straight line, a circular arc or a shaded tube.
`geom_node_3d()` draws nodes as points. Both map the position columns of
a node or edge table by default, so after `ggplot(graph)` (see
[`fortify.igraph()`](https://gillescolling.com/ggplot3d/reference/fortify.igraph.md))
they need no mapping; `geom_edge_3d()` reads the edge table with
[`get_edges_3d()`](https://gillescolling.com/ggplot3d/reference/get_edges_3d.md).

## Usage

``` r
geom_edge_3d(
  mapping = NULL,
  data = get_edges_3d(),
  stat = "identity",
  position = "identity",
  ...,
  shape = "line",
  curvature = NULL,
  n = 32L,
  radius = 0.01,
  sides = 12L,
  shade = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_node_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments. The default mapping of `geom_edge_3d()` is
  `aes(x, y, z = z, xend = xend, yend = yend, zend = zend)` and that of
  `geom_node_3d()` is `aes(x, y, z = z)`; a supplied mapping overrides
  these aesthetic by aesthetic.

- shape:

  `"line"`, `"arc"` or `"tube"`.

- curvature:

  Arc curvature; defaults to 0.5 for arcs and 0 (straight) for tubes.
  Ignored for lines.

- n:

  Number of points along a curved edge.

- radius:

  Tube radius as a fraction of the longest side of the panel box, for
  linewidth 0.5; the radius scales with linewidth.

- sides:

  Number of corners of the tube cross-section.

- shade:

  Whether to light faces; defaults to `TRUE` for tubes.

## Value

A ggplot2 layer.

## Details

Arcs and tubes are built in the model box, after the panel aspect is
applied, so they keep their shape for any `coord_3d(aspect = )`. An arc
bends in the plane spanned by the edge and the horizontal direction
perpendicular to it; its sagitta is `curvature` times half the edge
length, so `curvature = 1` is a half circle and negative values bend the
other way, as in
[`ggplot2::geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).
Tubes follow the straight edge, or the arc when `curvature` is set;
their cross-section is a regular polygon with `sides` corners, carried
along the edge with rotation-minimising frames, and they are closed at
both ends. The tube colour is the edge `colour`.

## References

Bishop (1975). There is more than one way to frame a curve. American
Mathematical Monthly 82, 246-251.
[doi:10.2307/2319846](https://doi.org/10.2307/2319846)

## See also

[`fortify.igraph()`](https://gillescolling.com/ggplot3d/reference/fortify.igraph.md),
[`stat_layers_3d()`](https://gillescolling.com/ggplot3d/reference/stat_layers_3d.md)

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  set.seed(2)
  g <- igraph::sample_pa(40, directed = FALSE)
  ggplot2::ggplot(g, layout = igraph::layout_with_fr(g, dim = 3)) +
    geom_edge_3d() + geom_node_3d(colour = "steelblue", size = 2) + coord_3d()
  ggplot2::ggplot(g) +
    geom_edge_3d(shape = "tube", colour = "grey60") +
    geom_node_3d(ggplot2::aes(colour = igraph::degree(g)), size = 2) + coord_3d()
}
d <- data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1)
ggplot2::ggplot(d) + geom_edge_3d(shape = "arc", curvature = 0.8) + coord_3d()
```
