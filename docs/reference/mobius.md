# Mobius transformations of complex numbers, spheres and half-space

Returns the map (a z + b)/(c z + d). On the Riemann sphere it acts on
complex homogeneous pairs, keeping the north pole finite in R^3. On the
upper half-space it uses the quaternionic extension to hyperbolic H^3.
These maps are conformal on the round sphere; the SU(2) subgroup
preserves its round metric. Every nonsingular complex matrix acts
isometrically on H^3.

## Usage

``` r
mobius(a = 1, b = 0, c = 0, d = 1)
```

## Arguments

- a, b, c, d:

  Complex scalar matrix coefficients with nonzero determinant.

## Value

A function of z and model ("complex", "sphere" or "halfspace"). Sphere
and half-space inputs are matrices with three columns; complex inputs
are vectors and may contain Inf. Half-space heights are positive.

## References

Wegert (2010). Phase plots of complex functions.
<https://arxiv.org/abs/1007.2295>.

## Examples

``` r
f <- mobius(a=0,b=1,c=1,d=0)
f(c(0,1,Inf))
f(rbind(c(0,0,1),c(0,0,-1)),model="sphere")
```
