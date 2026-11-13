# Request probability-content density levels

Converts requested enclosed probabilities into a tagged specification
for stat_density_3d. Grid quadrature accumulates Gaussian density from
highest values downwards until each absolute probability content is
reached. Scale limits should include the density tails; inadequate
captured mass warns.

## Usage

``` r
probs(...)
```

## Arguments

- ...:

  Probabilities strictly between zero and one.

## Value

A numeric vector tagged as a probability-content specification.

## Examples

``` r
probs(0.5, 0.9)
```
