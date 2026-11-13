# Render a camera path

Render a fixed scene from successive camera positions. Geometry and
scales are built once; projection, visibility and lighting are evaluated
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
  progress = interactive()
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- azimuth, elevation, zoom, roll:

  A scalar, a numeric vector of length `frames`, or a function receiving
  times in seconds and returning either. NULL retains the plot setting,
  except azimuth, which makes one full turn.

- frames:

  Number of frames, default 60. The closing view is excluded from the
  default turn so that looping repeats each view once.

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

## Value

A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
(one row per frame), `timings` in seconds, `build_seconds`, `frame`,
`panel_extents` in millimetres, and `frames` (PNG paths when file is
NULL). PNG frames returned with file = NULL belong to the caller.

## Details

Framing is "azimuth" for constant elevation and roll, and "sphere"
otherwise. Axis reserves and physical panel extents are fixed. Zoom
remains an explicit magnification. GIF needs gifski; MP4 needs av.
Optional packages are checked before rendering. Rendering streams frames
to disk.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
  geom_point_3d() + coord_3d()
if (requireNamespace("gifski", quietly = TRUE)) {
  movie <- animate_3d(p, frames = 3, width = 320, height = 320,
                      file = tempfile(fileext = ".gif"))
  movie$timings
}
```
