# Edge table of a fortified graph

Returns a function that ggplot2 calls on the plot data, as the `data`
argument of a layer. For a node table made by
[`fortify.igraph()`](https://gillescolling.com/ggplot3d/reference/fortify.igraph.md)
it returns the attached edge table; any other data frame is returned
unchanged.

## Usage

``` r
get_edges_3d()
```

## Value

A function of the plot data returning a data frame.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  nodes <- fortify(igraph::make_ring(5))
  get_edges_3d()(nodes)
}
```
