# Lorentz boosts in canonical coordinates

Uses the metric diag(1,...,1,-1), with time in the last coordinate.
Positive rapidity sends the time axis toward direction. In velocity
units with c=1 the speed is tanh(rapidity). The returned matrix acts on
column vectors; multiply a row matrix by its transpose.

## Usage

``` r
lorentz_boost_3d(rapidity, direction = c(1, 0, 0))
```

## Arguments

- rapidity:

  Finite scalar rapidity.

- direction:

  Nonzero spatial direction in two or three dimensions.

## Value

A 3 by 3 or 4 by 4 Lorentz matrix.

## Examples

``` r
B <- lorentz_boost_3d(.5,c(1,0,0))
B %*% c(0,0,0,1)
```
