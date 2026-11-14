# Normalise homogeneous positions to the panel box

Homogeneous coordinates (x, y, z, w) with w \> 0 stand for the point (x
/ w, y / w, z / w); with w = 0 they stand for the direction (x, y, z), a
point at infinity. Rows with w \< 0 are points of the far half of
oriented projective space (Stolfi 1991): (x, y, z, w) and (-x, -y, -z,
-w) are distinct, and only the half with w \>= 0 after the camera `pre`
map is drawn. This maps homogeneous positions in scale units to the
homogeneous panel-box fractions that
[`new_scene_packet()`](https://gillescolling.com/ggplot3d/reference/new_scene_packet.md)
takes: each coordinate becomes (x - lo \* w) / (hi - lo) and w is kept,
so that positive multiples of a row give the same point and directions
stay at infinity.

## Usage

``` r
normalise_homogeneous_3d(x, y, z, w = 1, ranges)
```

## Arguments

- x, y, z:

  Homogeneous coordinates in scale units.

- w:

  Homogeneous weight of either sign; 1 for ordinary points.

- ranges:

  List with x, y and z, each the lower and upper limit of the panel
  range, as in `panel_params$ranges`.

## Value

A list with x, y, z and w, the `vertices` of
[`new_scene_packet()`](https://gillescolling.com/ggplot3d/reference/new_scene_packet.md).

## Examples

``` r
normalise_homogeneous_3d(c(5, 1), c(0, 0), c(1, 0), w = c(1, 0),
  ranges = list(x = c(0, 10), y = c(-1, 1), z = c(0, 2)))
```
