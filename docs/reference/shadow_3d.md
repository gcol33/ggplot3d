# Planar cast shadows

Casts the shadows of a panel's plot layers onto panes of the panel box,
for the `shadow` argument of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
Every filled face, stroke and point is projected along the light
direction onto the plane of each chosen pane (Blinn 1988) and cut to the
pane. The shadows of one layer are painted as one translucent film, the
union of their pieces filled once with the nonzero winding rule, so
overlapping pieces do not darken twice. The film lies in the pane's
plane and is drawn over the pane fill and under its grid lines.

## Usage

``` r
shadow_3d(on = "floor", alpha = 0.3, colour = "black", light = NULL)
```

## Arguments

- on:

  Panes that receive shadows: `"floor"`, `"back"`, or pane names
  `"xmin"` to `"zmax"`.

- alpha:

  Opacity of the shadow film, greater than 0 and at most 1.

- colour:

  Shadow colour.

- light:

  A
  [`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
  configuration whose direction casts the shadows. `NULL` uses the light
  of
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
  or the default
  [`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
  when that is `NULL` too.

## Value

A shadow configuration for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Details

Strokes cast ribbons of their drawn width and points cast discs of their
drawn radius, both measured where the caster lies. Text casts no shadow,
and neither do ray-traced layers. A pane receives shadows only when the
light reaches it from inside the box, and only while it faces away from
the camera, as panes drawn behind the data do. The shadows are vector
geometry in every backend: the raster and hybrid backends draw them with
the panel furniture.

## References

Blinn, J. (1988). Me and my (fake) shadow. IEEE Computer Graphics and
Applications 8(1), 82-86.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 25), y = seq(-1, 1, length.out = 25))
d$z <- exp(-2 * (d$x^2 + d$y^2))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "steelblue", colour = NA) +
  coord_3d(shadow = shadow_3d(light = light_3d(c(1, 1, 3), space = "data")),
    zlim = c(-1, 1))
```
