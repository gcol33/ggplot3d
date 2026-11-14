# Domain colouring on the Riemann sphere

Samples a complex function after inverse stereographic projection. Hue
encodes phase cyclically, and optional brightness bands mark powers of
two in modulus. The computed arg and mod variables support custom
scales. The north pole represents infinity at the finite point (0,0,1).
Values are sampled at projected triangle centroids and paint is constant
per triangle. Increase n with the raster backend for detailed phase
portraits.

## Usage

``` r
stat_riemann_sphere_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun,
  n = 60L,
  contours = TRUE,
  transform = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- fun:

  Vectorised complex function.

- n:

  Longitude resolution; latitude uses half as many samples.

- contours:

  Draw modulus bands in the domain colour.

- transform:

  Optional
  [`mobius()`](https://gillescolling.com/ggplot3d/reference/mobius.md)
  map applied to the sphere mesh.

## Value

A surface layer.

## References

Wegert (2010). <https://arxiv.org/abs/1007.2295>. Poelke and Polthier
(2009). Lifted domain coloring.
<https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742>

## Examples

``` r
ggplot2::ggplot()+stat_riemann_sphere_3d(fun=function(z) (z-1)/(z^2+1),n=24)+
  coord_3d(backend=backend_3d("raster"),axes=character(),panes=character(),box="none")
```
