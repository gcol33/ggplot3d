# Ordination biplots

An ordination places observations on a small set of axes computed from
their measured variables. A biplot draws the observations and variable
directions in the same view.
[`stat_biplot_3d()`](https://gillescolling.com/ggplot3d/reference/stat_biplot_3d.md)
composes site points, solid-headed quiver arrows and optional group
regions.

``` r

fit <- prcomp(iris[1:4], scale. = TRUE)
groups <- setNames(iris$Species, rownames(iris))
ggplot() + stat_biplot_3d(fit, groups = groups, region = "ellipsoid",
                        loading_scale = 2, label_size = 5) + coord_3d()
```

![3D plot. Layers: Point (x = x, y = y, z = z, colour = .biplot_group,
fill = .biplot_group, group = .biplot_group); Ellipsoid (x = x, y = y, z
= z, colour = .biplot_group, fill = .biplot_group, group =
.biplot_group); Quiver (x = x, y = y, z = z, u = u, v = v, w = w);
ArrowLabel (x = u, y = v, z = w, xend = x, yend = y, zend = z, label =
id). Camera: azimuth -60 degrees, elevation 30 degrees, roll 0 degrees;
orthographic projection.](ordination_files/figure-html/iris-1.svg)

For `prcomp`, sites use `fit$x` and arrows use `fit$rotation`.
`loading_scale` explicitly multiplies the arrow vectors for display. The
chosen multiplier therefore belongs in a figure’s methods or caption.
`choices` selects the three axes.

Site identifiers are the score matrix’s row names, or sequential
character identifiers when row names are absent. Group membership is
joined by these identifiers. A named vector or a table with `id` and
`group` columns can supply membership in any order.

``` r

membership <- data.frame(id = names(groups), group = unname(groups))
membership <- membership[rev(seq_len(nrow(membership))), ]
ggplot() + stat_biplot_3d(fit, groups = membership, region = "hull",
                        loading_scale = 2, label_size = 5) + coord_3d()
```

![3D plot. Layers: Point (x = x, y = y, z = z, colour = .biplot_group,
fill = .biplot_group, group = .biplot_group); StatsSurface (x = x, y =
y, z = z, colour = .biplot_group, fill = .biplot_group, group =
.biplot_group); Quiver (x = x, y = y, z = z, u = u, v = v, w = w);
ArrowLabel (x = u, y = v, z = w, xend = x, yend = y, zend = z, label =
id). Camera: azimuth -60 degrees, elevation 30 degrees, roll 0 degrees;
orthographic projection.](ordination_files/figure-html/hull-1.svg)

## Vegan scores

[vegan](https://vegandevs.github.io/vegan/) supplies ecological
ordination methods and a `scores()` generic that applies their scaling
conventions.
[`stat_biplot_3d()`](https://gillescolling.com/ggplot3d/reference/stat_biplot_3d.md)
calls that generic for vegan fits. Pass method-specific arguments in
`scores_args`, or supply a list containing `sites` and `species` score
matrices directly.

``` r

model <- vegan::rda(iris[1:4], scale = TRUE)
ggplot() + stat_biplot_3d(model, groups = groups,
                        scores_args = list(scaling = 2), label_size = 5) + coord_3d(clip = "off")
```

![3D plot. Layers: Point (x = x, y = y, z = z, colour = .biplot_group,
fill = .biplot_group, group = .biplot_group); Quiver (x = x, y = y, z =
z, u = u, v = v, w = w); ArrowLabel (x = u, y = v, z = w, xend = x, yend
= y, zend = z, label = id). Camera: azimuth -60 degrees, elevation 30
degrees, roll 0 degrees; orthographic
projection.](ordination_files/figure-html/vegan-1.svg)

The species arrows of this fit reach the edge of the box. `clip = "off"`
lets their labels extend past the drawing area of the panel instead of
being cut at its edge.

The group ellipsoids use `stat_ellipsoid_3d(type = "norm")`. Their level
describes the spread of observations under that covariance model.
Individual point, quiver, text, hull and ellipsoid layers can also be
composed directly when a figure needs separate mappings.
[vegan3d](https://rdrr.io/cran/vegan3d/man/vegan3d-package.html)
documents the corresponding ordination and group-region conventions.
