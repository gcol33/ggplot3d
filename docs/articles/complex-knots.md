# Complex functions, knots and immersed surfaces

A complex function of a complex variable maps a plane to a plane, which
needs four real dimensions to graph. Colour can carry two of them, and a
surface over the plane or the sphere carries the rest. Knots and
immersed surfaces are curves and surfaces whose shape in space is the
subject. This article draws each with a dedicated stat. Installation is
covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md), and
[Surfaces, solids and
fields](https://gillescolling.com/ggplot3d/articles/surfaces-solids.md)
covers parametric surfaces in general.

``` r

library(ggplot2)
library(ggplot3d)
```

## Domain colouring on the Riemann sphere

Domain colouring paints each point $`z`$ with the phase of $`f(z)`$ as
hue (Wegert [2010](https://arxiv.org/abs/1007.2295)). Zeros are the
points where all hues meet in counter-clockwise order, and poles are
where they meet in the opposite order. On the Riemann sphere, the
complex plane closed by a point at infinity, the behaviour at infinity
is visible too. $`f(z) = (z - 1)/(z^2 + 1)`$ has one zero at 1 and poles
at $`\pm i`$.

``` r

ggplot() +
  stat_riemann_sphere_3d(fun = function(z) (z - 1) / (z^2 + 1), n = 32) +
  coord_3d(backend = backend_3d("raster"), axes = character(),
           panes = character(), box = "none")
```

![A sphere painted by the phase of a rational function, with colours
circling one zero and two
poles.](complex-knots_files/figure-html/riemann-sphere-1.svg)

The raster backend keeps the many small faces of the colouring from
producing a large vector file.

## Riemann surfaces

An inverse function such as $`\sqrt[3]{w}`$ has several values at each
point. Its Riemann surface stacks one sheet per value and joins them so
the function is single-valued on the surface (Poelke and Polthier
[2009](https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742)).
[`stat_riemann_surface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_riemann_surface_3d.md)
takes the forward function, here $`w = z^3`$, and the number of sheets.

``` r

ggplot() +
  stat_riemann_surface_3d(fun_inverse = function(w) w^3, sheets = 3, n = 24,
                          fill = "#2E6F9E") +
  coord_3d()
```

![The three-sheeted Riemann surface of the cube root, spiralling three
times around its branch
point.](complex-knots_files/figure-html/riemann-surface-1.svg)

## Möbius transformations

A Möbius transformation $`f(z) = (az + b)/(cz + d)`$ maps circles to
circles on the Riemann sphere.
[`mobius_3d()`](https://gillescolling.com/ggplot3d/reference/mobius_3d.md)
returns the map as a function that accepts complex numbers, points on
the sphere or points in the upper half-space. The inversion
$`f(z) = 1/z`$ swaps 0 and infinity and fixes 1.

``` r

f <- mobius_3d(a = 0, b = 1, c = 1, d = 0)
f(c(0, 1, Inf))
#> [1] Inf+0i   1+0i   0+0i
f(rbind(c(0, 0, 1), c(0, 0, -1)), model = "sphere")
#>      x y  z
#> [1,] 0 0 -1
#> [2,] 0 0  1
```

On the sphere the same map swaps the north and south poles.

## Torus knots

A $`(p, q)`$ torus knot winds $`p`$ times around the axis of a torus and
$`q`$ times through its hole; it is a knot when $`p`$ and $`q`$ share no
factor.
[`stat_knot_3d()`](https://gillescolling.com/ggplot3d/reference/stat_knot_3d.md)
draws it as a tube. The $`(2, 3)`$ torus knot is the trefoil.

``` r

ggplot() +
  stat_knot_3d(p = 2, q = 3, radius = 0.12, n = 120, sides = 10, fill = "#B5342B") +
  coord_3d(aspect = "data")
```

![A trefoil knot drawn as a tube winding twice around and three times
through a torus.](complex-knots_files/figure-html/knot-1.svg)

[Gallery](https://gillescolling.com/ggplot3d/articles/gallery.html#parametric-curves)
builds the same trefoil from a data frame with
[`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md).

## Immersed surfaces

The Klein bottle and Boy’s surface cannot be built in three-dimensional
space without passing through themselves.
[`stat_immersed_3d()`](https://gillescolling.com/ggplot3d/reference/stat_immersed_3d.md)
draws standard immersions of such surfaces: `"klein_figure8"`,
`"klein_bottle"`, `"boy"`, `"cross_cap"` and the Möbius strip
`"moebius"`.

``` r

klein <- ggplot() +
  stat_immersed_3d(type = "klein_figure8", n = c(32, 20), fill = "#2A7A7A") +
  coord_3d()
klein
```

![The figure-eight immersion of the Klein bottle, a tube with a
figure-eight cross-section that passes through
itself.](complex-knots_files/figure-html/klein-1.svg)

The scene resolves each crossing exactly, so the faces in front cover
the ones behind on both sides of the self-intersection. [Meshes and
intrinsic
geometry](https://gillescolling.com/ggplot3d/articles/meshes.html#where-a-mesh-crosses-itself)
draws the crossing curve itself with
[`stat_self_intersection_3d()`](https://gillescolling.com/ggplot3d/reference/stat_self_intersection_3d.md).

## References

Poelke, K. and Polthier, K. (2009). Lifted domain coloring. *Computer
Graphics Forum* 28(3), 735-742.
<https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742>

Wegert, E. (2010). Phase plots of complex functions: a journey in
illustration. <https://arxiv.org/abs/1007.2295>
