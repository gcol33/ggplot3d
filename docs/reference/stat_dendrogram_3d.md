# Dendrograms on a plane

Draws the tree of a hierarchical clustering as segments in a coordinate
plane of the panel, such as a wall beside a heatmap drawn on the floor.
The first axis of `plane` carries the leaves, the second the merge
heights; the remaining axis is fixed at `at`, where `Inf` and `-Inf`
place the tree on the box sides at the upper and lower end of that axis
(0 when no other layer trains that axis). Leaves sit at 1, 2, ..., n in
the order of the tree (`hclust$order`), which matches a discrete axis
whose levels are `hclust$labels[hclust$order]`; `positions` moves them.
Each merge is centred over its two children, as in
[`stats::plot.hclust()`](https://rdrr.io/r/stats/hclust.html);
`type = "triangle"` joins the children directly to the merge point.

## Usage

``` r
stat_dendrogram_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomSegment,
  position = "identity",
  ...,
  hclust,
  plane = "xz",
  at = Inf,
  positions = NULL,
  height = NULL,
  type = "rectangle",
  k = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
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

  Geom drawing the segments.

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

- hclust:

  A hierarchical clustering, an object with a
  [`stats::as.hclust()`](https://rdrr.io/r/stats/as.hclust.html) method
  such as an `hclust` or a `dendrogram`.

- plane:

  Two distinct axis letters: the leaf axis then the height axis, for
  example `"xz"`, `"yz"` or `"xy"`.

- at:

  Position on the remaining axis.

- positions:

  Leaf positions in tree order; by default `seq_len(n)`.

- height:

  `NULL` to draw merge heights as they are, or two numbers: the
  positions of height zero and of the root. A reversed pair draws the
  tree hanging down.

- type:

  `"rectangle"` or `"triangle"`.

- k:

  Number of clusters for the computed `cluster` variable.

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

Computed variables are `height` (the merge height), `node` (the merge
index in `hclust$merge`) and, with `k`, `cluster`: the cluster of
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html) with `k` groups
that the branch belongs to, `NA` above the cut, for colouring branches
by cluster as in dendextend.

## References

de Vries and Ripley (2024). ggdendro: create dendrograms and tree
diagrams using ggplot2. <https://CRAN.R-project.org/package=ggdendro>

## Examples

``` r
m <- as.matrix(scale(mtcars[1:12, c("mpg", "hp", "wt", "qsec", "disp")]))
rows <- stats::hclust(stats::dist(m))
d <- data.frame(car = factor(rep(rownames(m), ncol(m)), levels = rownames(m)[rows$order]),
                variable = rep(colnames(m), each = nrow(m)), value = as.vector(m))
ggplot2::ggplot(d, ggplot2::aes(car, variable)) +
  ggplot2::geom_tile(ggplot2::aes(fill = value)) +
  stat_dendrogram_3d(ggplot2::aes(colour = ggplot2::after_stat(cluster)), hclust = rows,
    plane = "xz", height = c(0, 3), k = 3, linewidth = 0.6) +
  coord_3d()
```
