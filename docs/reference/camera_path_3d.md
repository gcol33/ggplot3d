# Camera paths through keyframes

Describe a camera move as a timed sequence of views.
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
renders the path and
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
plays it in the browser. Paths join with
[`c()`](https://rdrr.io/r/base/c.html): each part starts from the camera
the previous part ends on, and the first part from the plot's
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
camera.

## Usage

``` r
camera_path_3d(
  keys,
  at = NULL,
  ease = "linear",
  orientation = c("slerp", "squad"),
  position = c("catmull-rom", "linear")
)

orbit_3d(degrees = 360, duration = 4, elevation = NULL, ease = "linear")

fly_through_3d(
  path,
  look_at = NULL,
  at = NULL,
  ease = "linear",
  fov = NULL,
  roll = 0,
  position = c("catmull-rom", "linear")
)

dolly_zoom_3d(fov = 10, duration = 3, ease = "cubic-in-out")
```

## Arguments

- keys:

  A list of camera views, each a
  [`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)
  result or a named list with any of `projection`, `azimuth`,
  `elevation`, `roll`, `distance`, `fov`, `zoom`, `target` and
  `oblique`. A field a key omits keeps the value of the previous key,
  and the first key's omitted fields come from the camera the path
  starts from. A
  [`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)
  result always names its projection, so pass
  `projection = "perspective"` to it for a perspective path.

- at:

  Key times in seconds, increasing, starting at 0. The default puts the
  keys one second apart.

- ease:

  Easing of time between consecutive keys: a name such as `"linear"`,
  `"cubic-in-out"` or `"sine-in"`, a function mapping `[0, 1]` onto
  itself, or a list or character vector with one entry per segment.
  Named easings are `"linear"` and `"quadratic"`, `"cubic"`,
  `"quartic"`, `"quintic"`, `"sine"`, `"circular"`, `"exponential"`,
  `"back"`, `"elastic"` and `"bounce"`, each followed by `-in`, `-out`
  or `-in-out` (Penner 2002, as in tweenr and CSS).

- orientation:

  `"slerp"` (default) or `"squad"`.

- position:

  Interpolation of the target, the eye of a fly-through and the
  log-scaled tracks: `"catmull-rom"` (default) or `"linear"`.

- degrees:

  Turn about the vertical axis in degrees; positive values increase the
  azimuth. Default one full turn.

- duration:

  Length of the move in seconds.

- elevation:

  Elevation at the end of the orbit, reached gradually, or NULL
  (default) to keep the starting elevation.

- path:

  Eye positions in data coordinates: a data frame or matrix with columns
  `x`, `y` and `z` (or three unnamed columns), one row per key.

- look_at:

  The point the camera looks at, in data coordinates: one row for a
  fixed point, or one row per eye position. The default looks at the
  centre of the plot box.

- fov:

  Field of view in degrees. For `fly_through_3d()` NULL keeps the
  starting value. For `dolly_zoom_3d()` the final value, or the start
  and end values; 0 stands for the orthographic projection, and a single
  value starts from the current camera's field of view (0 when it is
  parallel).

- roll:

  Camera roll in degrees, one value or one per eye position.

## Value

A `ggplot3d_camera_path` object.

## Details

`camera_path_3d()` interpolates keyframes. The orientation, that is
azimuth, elevation and roll together, is converted to a unit quaternion
and interpolated on the sphere of rotations (Shoemake 1985): `"slerp"`
turns at constant angular speed between neighbouring keys, `"squad"`
adds Shoemake's spherical cubic so the angular velocity is continuous at
the keys. Paths therefore pass through `elevation = 90` without the spin
that interpolating the angles separately produces there. The target
moves on a centripetal Catmull-Rom spline (Yuksel, Schaefer and Keyser
2011), which passes through every key without cusps or
self-intersections. Field of view, zoom and an explicit perspective
distance are interpolated in log space, where equal steps are equal
ratios, with a monotone cubic (Fritsch and Carlson 1980) that does not
overshoot the keys.

`orbit_3d()` turns the camera about the vertical axis through the
target. `fly_through_3d()` moves the eye along a spline through points
in data coordinates, looking at a fixed or moving point; while the eye
is inside the plot box the axes are left out, since the box outline they
follow surrounds the camera. `dolly_zoom_3d()` changes the field of view
while the camera moves along the line of sight so that the plane through
the target keeps its size on the image; a field of view of 0 is the
orthographic projection, so `fov = 0` morphs a perspective view into a
parallel one at constant image scale.

## References

Shoemake, K. (1985). Animating rotation with quaternion curves. SIGGRAPH
'85, 245-254.
[doi:10.1145/325165.325242](https://doi.org/10.1145/325165.325242)

Yuksel, C., Schaefer, S. and Keyser, J. (2011). Parameterization and
applications of Catmull-Rom curves. Computer-Aided Design 43, 747-755.
[doi:10.1016/j.cad.2010.08.008](https://doi.org/10.1016/j.cad.2010.08.008)

Fritsch, F. N. and Carlson, R. E. (1980). Monotone piecewise cubic
interpolation. SIAM Journal on Numerical Analysis 17, 238-246.
[doi:10.1137/0717021](https://doi.org/10.1137/0717021)

## See also

[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md),
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md),
[`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)

## Examples

``` r
over_the_top <- camera_path_3d(
  list(camera_3d(azimuth = 0, elevation = 30),
       camera_3d(azimuth = 0, elevation = 90),
       camera_3d(azimuth = 180, elevation = 30, roll = 180)),
  at = c(0, 1.5, 3), ease = "sine-in-out")
over_the_top
tour <- c(orbit_3d(degrees = 90, duration = 2),
          dolly_zoom_3d(fov = 0, duration = 2))
tour
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
  geom_point_3d() + coord_3d(projection = "perspective")
if (requireNamespace("ragg", quietly = TRUE)) {
  movie <- animate_3d(p, path = tour, fps = 4, width = 240, height = 240,
                      file = tempfile(fileext = ".apng"))
  movie$camera[, c("time", "azimuth", "fov", "projection")]
}
```
