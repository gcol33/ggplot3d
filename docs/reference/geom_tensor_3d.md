# Symmetric tensor glyphs

Eigen-decomposes each symmetric positive semidefinite tensor. Its
eigenvalues times `scale` are the three semi-axis lengths, in
scale-transformed position units, and its eigenvectors give their
directions. Kindlmann superquadrics vary continuously between spherical,
linear and planar tensor shapes. `gamma = 0` gives ellipsoids;
increasing gamma sharpens anisotropic shapes. Position scales expand to
contain the tessellated glyphs.

## Usage

``` r
geom_tensor_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  glyph = c("ellipsoid", "superquadric"),
  gamma = 3,
  scale = 1,
  detail = 2L,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- glyph:

  `"ellipsoid"` or `"superquadric"`.

- gamma:

  Nonnegative edge-sharpness exponent.

- scale:

  Nonnegative multiplier of tensor eigenvalues.

- detail:

  Tessellation level of the built-in round glyphs, from 0 to 6.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## References

Kindlmann (2004). Superquadric tensor glyphs.
[doi:10.2312/VisSym/VisSym04/147-154](https://doi.org/10.2312/VisSym/VisSym04/147-154)

## Examples

``` r
d <- data.frame(x = 1:3, y = 0, z = 0)
d$tensor <- list(diag(c(1, 0.2, 0.2)), diag(c(1, 1, 0.2)), diag(3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, tensor = tensor)) +
  geom_tensor_3d(glyph = "superquadric", scale = 0.3) + coord_3d(aspect = "data")
```
