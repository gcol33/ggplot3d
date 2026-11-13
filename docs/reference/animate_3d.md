# Render a camera path or data that change over time

Render a scene from successive camera positions, or draw one subset of
the data per frame. Projection, visibility and lighting are evaluated
per frame.

## Usage

``` r
animate_3d(
  plot,
  azimuth = NULL,
  elevation = NULL,
  zoom = NULL,
  frames = 60L,
  fps = 20,
  file = NULL,
  renderer = c("raster", "vector"),
  roll = NULL,
  width = 640L,
  height = 640L,
  dpi = 96,
  antialias = 1L,
  progress = interactive(),
  time = NULL
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- azimuth, elevation, zoom, roll:

  A scalar, a numeric vector of length `frames`, or a function receiving
  times in seconds and returning either. NULL retains the plot setting,
  except azimuth without `time`, which makes one full turn.

- frames:

  Number of frames, default 60. The closing view is excluded from the
  default turn so that looping repeats each view once. With `time`, one
  frame per distinct value of the time column.

- fps:

  Frames per second, default 20.

- file:

  Output path ending in .gif or .mp4. NULL returns PNG frame paths.

- renderer:

  Scene backend, "raster" (default) or "vector". Both produce PNG frames
  for the encoder.

- width, height:

  Frame dimensions in pixels, default 640 each.

- dpi:

  Device resolution, default 96 dots per inch.

- antialias:

  Raster supersampling factor, default 1.

- progress:

  Report rendering time for each frame, default interactive().

- time:

  Name of a data column holding the frame of each row, or NULL (default)
  for a fixed scene. Frame `i` draws the rows whose value equals the
  `i`-th sorted distinct value; layers without the column appear in
  every frame. Scales are trained on all frames, so limits, breaks and
  colour mappings are the same in each frame.

## Value

A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
(one row per frame, with the frame's `value` of the time column when
`time` is set), `timings` in seconds, `build_seconds`, `frame`,
`panel_extents` in millimetres, and `frames` (PNG paths when file is
NULL). PNG frames returned with file = NULL belong to the caller.

## Details

Framing is "azimuth" for constant elevation and roll, and "sphere"
otherwise. Axis reserves and physical panel extents are fixed. Zoom
remains an explicit magnification. GIF needs gifski; MP4 needs av.
Optional packages are checked before rendering. Rendering streams frames
to disk. A fixed scene is built once; with `time`, each frame is built
from its subset with the scale limits of the full data, as in
gganimate's `transition_manual()` and plotly's `frame` aesthetic.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
  geom_point_3d() + coord_3d()
if (requireNamespace("gifski", quietly = TRUE)) {
  movie <- animate_3d(p, frames = 3, width = 320, height = 320,
                      file = tempfile(fileext = ".gif"))
  movie$timings
}
waves <- expand.grid(x = seq(0, 1, length.out = 20), y = seq(0, 1, length.out = 20),
                     t = seq(0, 0.75, by = 0.25))
waves$z <- sin(pi * waves$x) * sin(pi * waves$y) * cos(2 * pi * waves$t)
wave <- ggplot2::ggplot(waves, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d() + coord_3d()
if (requireNamespace("gifski", quietly = TRUE)) {
  movie <- animate_3d(wave, time = "t", width = 320, height = 320,
                      file = tempfile(fileext = ".gif"))
  movie$camera$value
}
```
