# Immersed surface examples

Generates a figure-eight Klein bottle, a bottle immersion, Boy's
surface, a cross-cap with pinch points, or a Moebius band. Uses the
indexed and twisted parameter seams of
[`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md).
Lighting is two-sided.

## Usage

``` r
stat_immersed_3d(
  type = c("klein_figure8", "klein_bottle", "boy", "cross_cap", "moebius"),
  n = 40L,
  ...
)
```

## Arguments

- type:

  Surface name.

- n:

  Samples per parameter, a scalar or pair of integers.

- ...:

  Arguments to
  [`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md).

## Value

A surface layer.

## References

VTK parametric surface definitions:
<https://vtk.org/doc/nightly/html/classvtkParametricBoy.html>,
<https://vtk.org/doc/nightly/html/classvtkParametricFigure8Klein.html>.

## Examples

``` r
ggplot2::ggplot() + stat_immersed_3d(type="klein_figure8",n=c(24,16)) + coord_3d()
```
