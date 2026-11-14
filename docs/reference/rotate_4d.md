# Rotations in four dimensions

Returns an SO(4) matrix for a rotation in one of the six coordinate
planes. Alternatively, supply two unit quaternions (vector xyz, scalar w
last) for the double rotation q -\> left*q*Conj(right). Row coordinates
multiply by the transpose of the matrix.

## Usage

``` r
rotate_4d(plane = "xw", angle = 0, left = NULL, right = NULL)
```

## Arguments

- plane:

  Two coordinate letters from x,y,z,w.

- angle:

  Rotation angle in radians.

- left, right:

  Unit quaternions specifying a double rotation.

## Value

A 4 by 4 orthogonal matrix with determinant one.

## Examples

``` r
rotate_4d("xw",pi/4)
rotate_4d(left=c(0,0,sin(.2),cos(.2)),right=c(sin(.3),0,0,cos(.3)))
```
