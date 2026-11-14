# Torus knots and knot tubes

A (p,q) torus knot winds p times around the central axis and q times
around the tube. The integers must be coprime. A positive radius sweeps
a closed tube using the shared rotation-minimising frame.

## Usage

``` r
stat_knot_3d(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  type = "torus",
  p = 2L,
  q = 3L,
  n = 200L,
  major = 2,
  minor = 0.7,
  radius = NULL,
  sides = 12L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- type:

  Knot family, currently "torus".

- p, q:

  Coprime nonzero integer winding numbers.

- n:

  Number of samples around the closed knot.

- major, minor:

  Major and minor radii of the supporting torus.

- radius:

  Tube radius; NULL draws a path.

- sides:

  Number of sides of a tube.

## Value

A path or surface layer.

## References

van Wijk and Cohen (2006).
[doi:10.1109/TVCG.2006.83](https://doi.org/10.1109/TVCG.2006.83) .

## Examples

``` r
ggplot2::ggplot() + stat_knot_3d(p=2,q=3,radius=.12,n=80,sides=8) + coord_3d()
```
