# Interpolate built layer data between animation states

Adds `n` in-between frames to each adjacent pair of states. Statistics
run on each state before interpolation. Positions interpolate in
transformed scale units; colours interpolate in linear light. The scales
are trained on all states. Use an explicit group aesthetic for
persistent identities.

## Usage

``` r
tween_3d(
  n = 10L,
  ease = "linear",
  match = "group",
  enter = "fade",
  exit = "fade"
)
```

## Arguments

- n:

  Number of in-between frames per adjacent state pair.

- ease:

  An easing name accepted by
  [`camera_path_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md),
  or a function taking and returning numeric fractions between zero and
  one.

- match:

  `"group"` matches marks by their group values and then by occurrence
  within a group, as in gganimate's `transition_states()`. `".source"`
  matches the source row numbers within each state.

- enter, exit:

  `"fade"` fades unmatched geometry, `"keep"` shows it throughout the
  transition, or a function returning an altered data frame with the
  same rows and columns. An enter function supplies starting attributes;
  an exit function supplies ending attributes.

## Value

A tween specification for
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md).

## Details

Paths and polygon rings are resampled by three-dimensional arc length
when their vertex counts differ. Polygon reading order is aligned by
ring winding and the nearest start vertex. Fixed-topology surfaces and
meshes match their face/corner keys; changes of mesh connectivity
cross-fade the two shapes. Rotations use quaternion interpolation.

## References

gganimate, `transition_states()`; transformr, `tween_polygon()`.

## Examples

``` r
d <- data.frame(x = c(0, 1, 1, 2), y = c(0, 1, 1, 0),
                z = c(0, 1, 2, 0), id = c("a", "b", "a", "b"), t = c(0, 0, 1, 1))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id)) +
  geom_point_3d() + coord_3d()
if (requireNamespace("ragg", quietly = TRUE)) {
  animation <- animate_3d(p, time = "t", tween = tween_3d(n = 2),
    width = 320, height = 320, progress = FALSE)
  unlink(animation$frames)
}
```
