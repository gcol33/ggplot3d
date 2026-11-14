# Grand tours of three-dimensional projections

Creates a sequence of orthonormal projection frames interpolated along
Grassmann geodesics, using principal angles from an SVD. Target frames
are aligned by an orthogonal Procrustes rotation for a continuous
projected orientation. A tourr history array (p by 3 by k) is accepted
directly. Random targets use a local seed and preserve the caller's
random state. Pass the result to animate_3d(tour=...) on a plot using
[`stat_project_3d()`](https://gillescolling.com/ggplot3d/reference/stat_project_3d.md).

## Usage

``` r
tour_grand(d = 3L, history = NULL, steps = 30L, seed = 1L)
```

## Arguments

- d:

  Projection dimension, currently three.

- history:

  Optional p by 3 by k array of orthonormal projection frames.

- steps:

  Frames between random target subspaces.

- seed:

  Local random seed for a generated grand tour.

## Value

A ggplot3d_tour specification.

## References

Wickham, Cook, Hofmann and Buja (2011). tourr.
[doi:10.18637/jss.v040.i02](https://doi.org/10.18637/jss.v040.i02) .

## Examples

``` r
tour_grand(d=3,steps=20,seed=1)
```
