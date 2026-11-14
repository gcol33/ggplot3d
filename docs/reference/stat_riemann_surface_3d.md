# Riemann surfaces parametrised by an inverse function

For w in the parameter domain, draws (Re g(w), Im g(w), Re w), coloured
by Im w. For z=w^k, a full polar disk covers k sheets over z. For
z=exp(w), a rectangular domain with imaginary range spanning several
turns covers several logarithm sheets. sheets labels equal sectors of
the v interval. Crossings are resolved by the visibility engine. The
supplied inverse g must be single-valued and finite throughout the
sampled parameter domain.

## Usage

``` r
stat_riemann_surface_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun_inverse,
  sheets = 2L,
  domain = c("polar", "cartesian"),
  u = c(0, 1),
  v = NULL,
  n = 40L,
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

- fun_inverse:

  Vectorised single-valued complex function z=g(w).

- sheets:

  Number of sheet sectors, also sets the default imaginary range
  `[-pi*sheets, pi*sheets]` for a Cartesian domain.

- domain:

  Polar (u radius, v angle) or Cartesian (u real, v imaginary).

- u, v:

  Parameter ranges; v=NULL chooses the domain's default.

- n:

  Number of samples along u and v, scalar or two integers.

## Value

A surface layer with re_w, im_w and sheet computed variables.

## References

Poelke and Polthier (2009). Lifted domain coloring.
<https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742>.

## Examples

``` r
ggplot2::ggplot()+stat_riemann_surface_3d(fun_inverse=function(w) w^3,sheets=3,n=20)+coord_3d()
ggplot2::ggplot() +
  stat_riemann_surface_3d(fun_inverse=exp,sheets=2,domain="cartesian",
    u=c(-1,1),n=20) + coord_3d()
```
