# Fortify a graph for three-dimensional plotting

Turns an igraph graph into a node table with x, y and z positions. The
edge table, with start and end positions taken from the node table, is
attached to it and read by
[`geom_edge_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md)
through
[`get_edges_3d()`](https://gillescolling.com/ggplot3d/reference/get_edges_3d.md);
`ggplot(graph)` therefore supports node and edge layers from one plot
data set. Calling `ggplot(graph, layout = )` passes the layout through
to this method.

## Usage

``` r
# S3 method for class 'igraph'
fortify(model, data = NULL, layout = NULL, dim = 3L, ...)
```

## Arguments

- model:

  An igraph graph (including tidygraph `tbl_graph` objects).

- data:

  Unused, for compatibility with the generic.

- layout:

  `NULL`, a numeric matrix with one row per vertex and two or three
  columns, or a layout function such as
  [`igraph::layout_with_kk()`](https://r.igraph.org/reference/layout_with_kk.html).

- dim:

  Number of layout dimensions requested from a layout function.

- ...:

  Unused.

## Value

A data frame with one row per vertex: `x`, `y`, `z`, `node` (the vertex
index), `name` and the vertex attributes. Its attribute `"edges"` holds
one row per edge: `from`, `to` (vertex indices), `x`, `y`, `z`, `xend`,
`yend`, `zend` and the edge attributes.

## Details

The layout is computed by igraph, not by this package. With
`layout = NULL` the vertex attributes `x`, `y` and `z` are used when the
graph carries them (as for
[`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html)),
otherwise the three-dimensional Fruchterman-Reingold layout
[`igraph::layout_with_fr()`](https://r.igraph.org/reference/layout_with_fr.html)
with `dim = 3`. A layout function is called with the graph, and with
`dim` when it accepts it. A two-column layout places every node at z =
0.

## References

Fruchterman and Reingold (1991). Graph drawing by force-directed
placement. Software: Practice and Experience 21, 1129-1164.
[doi:10.1002/spe.4380211102](https://doi.org/10.1002/spe.4380211102)

## See also

[`geom_edge_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md),
[`stat_layers_3d()`](https://gillescolling.com/ggplot3d/reference/stat_layers_3d.md)

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  set.seed(1)
  g <- igraph::sample_gnp(30, 0.1)
  nodes <- fortify(g, layout = igraph::layout_with_fr(g, dim = 3))
  head(nodes)
  head(get_edges_3d()(nodes))
}
```
