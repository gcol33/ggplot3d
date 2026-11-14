# Focus paths for a hyperbolic embedding

Returns Lorentz boosts taking a Poincare-ball point to the origin along
a geodesic. Pass the matrices to animate_3d(isometry=...).

## Usage

``` r
hyperbolic_focus_3d(point, frames = 30L)
```

## Arguments

- point:

  Finite point strictly inside the Poincare ball.

- frames:

  Positive number of frames.

## Value

A list of Lorentz matrices, from identity to the focus transform.

## Examples

``` r
hyperbolic_focus_3d(c(.4,0,0),frames=3)
```
