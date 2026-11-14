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
  time = NULL,
  path = NULL,
  workers = 1L,
  tween = NULL,
  trail = NULL,
  isometry = NULL,
  tour = NULL,
  stable_partition = FALSE,
  ...
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- azimuth, elevation, zoom, roll:

  A scalar, a numeric vector of length `frames`, or a function receiving
  times in seconds and returning either. NULL retains the plot setting,
  except azimuth without `time` or `path`, which makes one full turn.

- frames:

  Number of frames, default 60. The closing view is excluded from the
  default turn so that looping repeats each view once. With `time`, one
  frame per distinct value of the time column. With `path`, the default
  samples the path at `fps` frames per second, and a given number of
  frames is spread evenly over the path.

- fps:

  Frames per second, default 20.

- file:

  Output path, or NULL to return PNG frame paths. The extension selects
  the output: `.gif` (needs gifski), `.mp4` or `.webm` (need av),
  `.apng` (an animated PNG), `.png` or `.svg` (one file per frame; svg
  needs svglite) and `.pdf` (one page per frame). A file per frame takes
  its number from a [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
  pattern in the name, such as `"frames/turn-%03d.png"`, or else gets it
  appended, as in `turn-0001.png`. `.glb` and `.gltf` hold the first
  panel as an animated 3D scene for Blender and other glTF software:
  camera translation, rotation and zoom tracks, with per-frame
  visibility tracks for changing geometry. Each glTF animation uses one
  projection type; export perspective and orthographic paths separately.

- renderer:

  Scene backend, "raster" (default) or "vector". Use "vector" for vector
  pages in SVG and PDF output.

- width, height:

  Frame dimensions in pixels, default 640 each.

- dpi:

  Device resolution, default 96 dots per inch. SVG and PDF pages measure
  `width / dpi` by `height / dpi` inches.

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

- path:

  A camera path from
  [`camera_path_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md),
  [`orbit_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md),
  [`fly_through_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
  or
  [`dolly_zoom_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md),
  or NULL. A path replaces `azimuth`, `elevation`, `zoom` and `roll` and
  also moves the target, distance, field of view and projection.

- workers:

  Number of processes drawing frames, default 1, or a cluster from
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  with ggplot3d loaded on its nodes. Each worker draws a contiguous run
  of frames on its own devices, so the frames are identical for every
  number of workers. Workers fork where the platform allows it and
  otherwise start socket processes, which load the installed ggplot3d.
  PDF pages are drawn in one process.

- tween:

  A
  [`tween_3d()`](https://gillescolling.com/ggplot3d/reference/tween_3d.md)
  specification for in-between data frames, or NULL for one frame per
  state. Requires `time`. The number of frames is then
  `(number of states - 1) * (tween$n + 1) + 1`.

- trail:

  NULL (default), a positive integer giving the number of preceding
  rendered frames in a motion trail, or a list with `length` and
  optional `radius`, `width`, `sides` (default 8), `taper` (default
  FALSE), `normal` and `layers` (layer indices). Trails follow the
  anchors of time-varying row-based marks, including points, text and
  solid glyphs. Map `group` to a persistent object identifier. Trails
  use
  [`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)'s
  sweep; setting `width` selects a ribbon. Cross-section sizes use
  transformed position units; the default radius is 0.5% of the panel
  range diagonal. Mapped `radius`, `width` and `twist` are retained.
  Alpha fades linearly with frame age, multiplying the mark's alpha.
  `taper = TRUE` also scales the cross section by this fraction.
  Requires `time`; tweened frames count towards the window. Static
  layers stay fixed. Missing observations break a trail.

- isometry:

  A list of one canonical-space isometry matrix per frame, a matrix
  shared by all frames, or a function of frame times returning the list.
  Re-emits geometry through the space adapter; an isometry that acts
  linearly on the display model (Lorentz transformations in the Klein
  model) changes only the camera pre-transform. Limits remain fixed.

- tour:

  A
  [`tour_grand()`](https://gillescolling.com/ggplot3d/reference/tour_grand.md)
  specification or tourr history array. Applies Grassmann-interpolated
  bases to stat_project_3d() layers with orthographic or finite
  perspective projection. Uses fixed limits across frames. Requires
  static input data, linear position scales, and no tween or trail.

- stable_partition:

  Cut overlapping faces on planes fixed by the geometry, default FALSE.
  Faces that occlude one another in a cycle must be cut before they can
  be painted back to front. By default each frame chooses its cuts from
  the view, so cut edges move between frames. With TRUE, the cuts come
  from one partition of the plot's faces and quadric surfaces by their
  own planes (Fuchs, Kedem and Naylor 1980), built once and reused while
  the geometry stays the same; each frame only walks it back to front
  for its camera. The drawn image is the same as without it; only the
  position of the cut edges differs. Quadric stand-ins are then refined
  for every viewing direction at the camera's distance and zoom. Applies
  to scenes the vector and hybrid renderers draw.

- ...:

  Arguments to
  [`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md)
  for glTF output: `furniture`, `lines`, `points` and `size`.

## Value

A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
(one row per frame with the time, the camera angles, zoom, projection,
field of view, distance (NA when automatic) and target, and the frame's
`value` of the time column when `time` is set), `timings` in seconds,
`transition` (source state, destination state and eased fraction when
tweening), `build_seconds`, `frame`, `panel_extents` in millimetres, and
`frames` (the PNG paths when file is NULL, the written files of a
per-frame output). PNG frames returned with file = NULL belong to the
caller.

## Details

Framing is "azimuth" when only the azimuth and zoom change, and "sphere"
otherwise. Axis reserves and physical panel extents are fixed. Zoom
remains an explicit magnification. Optional packages are checked before
rendering. Rendering streams frames to disk. A fixed scene is built
once; with `time`, each frame is built from its subset with the scale
limits of the full data, as in gganimate's `transition_manual()` and
plotly's `frame` aesthetic. The animated PNG follows the APNG 1.0
specification: browsers play it and viewers without APNG support show
its first frame.

gganimate's `transition_states()` and `transition_time()` use a separate
build pipeline. These transitions produce guidance to use
`animate_3d(time = , tween = tween_3d())` for a 3D plot.

## See also

[`camera_path_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
for keyframed camera moves,
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
to play a path in the browser.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
  geom_point_3d() + coord_3d()
if (requireNamespace("gifski", quietly = TRUE)) {
  movie <- animate_3d(p, frames = 3, width = 320, height = 320,
                      file = tempfile(fileext = ".gif"))
  movie$timings
}
if (requireNamespace("ragg", quietly = TRUE)) {
  turn <- animate_3d(p, path = orbit_3d(degrees = 90, duration = 1), fps = 3,
                     width = 240, height = 240, file = tempfile(fileext = ".apng"))
  turn$camera$azimuth
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
track <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0), z = c(0, 0.5, 1), t = 1:3)
moving <- ggplot2::ggplot(track, ggplot2::aes(x, y, z = z, group = 1)) +
  geom_point_3d(colour = "steelblue", size = 3) + coord_3d()
if (requireNamespace("ragg", quietly = TRUE)) {
  wake <- animate_3d(moving, time = "t", tween = tween_3d(n = 1),
    trail = list(length = 3, radius = 0.04, taper = TRUE),
    width = 240, height = 240, progress = FALSE)
  unlink(wake$frames)
}
```
