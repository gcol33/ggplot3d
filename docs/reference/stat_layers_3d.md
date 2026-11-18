# Multilayer networks

Lays out a multilayer network given as an edge list, with each layer's
two-dimensional layout on its own z plane. Each row is an edge from node
`from` in layer `layer` to node `to` in layer `layer_end` (by default
the same layer); rows whose two layers differ are inter-layer edges,
such as the couplings that join a node to its copy in the next layer. A
numeric layer sits at z equal to its value, a discrete layer at z equal
to its level index.

## Usage

``` r
stat_layers_3d(
  mapping = NULL,
  data = NULL,
  geom = NULL,
  position = "identity",
  ...,
  layout = NULL,
  align = TRUE,
  what = "edges",
  margin = 0.1,
  seed = 1L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  Geom to draw the output; by default
  [`geom_edge_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md)
  for edges, points for nodes and polygons for planes.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- layout:

  A layout function of an igraph graph returning one row per node with
  at least two columns; `NULL` uses
  [`igraph::layout_with_fr()`](https://r.igraph.org/reference/layout_with_fr.html).

- align:

  Whether all layers share one layout.

- what:

  `"edges"`, `"nodes"` or `"planes"`.

- margin:

  Margin of the layer planes, as a fraction of the layout size.

- seed:

  Random seed for the layout, or `NULL` to use the current random number
  stream.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A ggplot2 layer.

## Details

The layout is computed by igraph. With `align = TRUE` one layout of the
aggregated network places every node at the same x and y in all layers,
so a node's copies stack vertically and the layers can be compared, as
in multiplex network visualisation (De Domenico et al. 2015). With
`align = FALSE` each layer is laid out from its own intra-layer edges.
Layout functions draw random starting positions; `seed` fixes them so
the plot is the same at every rendering, without changing the caller's
random number stream.

`what` selects the output: `"edges"` (one row per input row, with x, y,
z, xend, yend, zend and `interlayer`), `"nodes"` (one row per node and
layer, with `node`, `layer` and `degree`) or `"planes"` (one rectangle
per layer around the layout, extended by `margin` times its larger
side).

## References

De Domenico, Porter and Arenas (2015). MuxViz: a tool for multilayer
analysis and visualization of networks. Journal of Complex Networks 3,
159-176.
[doi:10.1093/comnet/cnu038](https://doi.org/10.1093/comnet/cnu038)

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  set.seed(3)
  layers <- do.call(rbind, lapply(c("a", "b", "c"), function(l) {
    e <- igraph::as_edgelist(igraph::sample_gnp(12, 0.25))
    data.frame(from = e[, 1], to = e[, 2], layer = l, layer_end = l)
  }))
  couplings <- data.frame(from = 1:12, to = 1:12, layer = "a", layer_end = "b")
  d <- rbind(layers, couplings)
  m <- ggplot2::aes(from = from, to = to, layer = layer, layer_end = layer_end)
  ggplot2::ggplot(d, m) +
    stat_layers_3d(what = "planes", fill = "grey85", alpha = 0.4) +
    stat_layers_3d(ggplot2::aes(colour = ggplot2::after_stat(interlayer))) +
    stat_layers_3d(what = "nodes", size = 1.5) +
    coord_3d()
}
```
