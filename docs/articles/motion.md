# Camera animation and interaction

Camera motion shows a fixed scene from several directions. Data
animation changes the observations or geometry between states. This
article renders both kinds of animation and opens a panel with orbit
controls. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md). Movie
output uses [gifski](https://r-rust.r-universe.dev/gifski) for GIF files
or [av](https://docs.ropensci.org/av/) for MP4 files; the viewer uses
[htmlwidgets](https://www.htmlwidgets.org/).

1.  **Build** the scene with
    [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

2.  **Render** a camera path with
    [`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md).

3.  **Explore** the plot with
    [`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md).

## Example

``` r

library(ggplot2)
library(ggplot3d)
theme_set(theme_minimal(base_size = 17) + theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA),
  legend.position = "none"))
d <- expand.grid(x = seq(-2, 2, length.out = 12),
                 y = seq(-2, 2, length.out = 12))
d$z <- with(d, exp(-(x^2 + y^2) / 2))
p <- ggplot(d, aes(x, y, z = z)) +
  geom_surface_3d(fill = "#2E6F9E") +
  coord_3d(azimuth = -60, elevation = 30, frame = "azimuth")
```

The data describe a curved surface sampled on a regular grid. The camera
looks toward the centre of its expanded box.

``` r

p
```

![A blue curved surface viewed at azimuth minus sixty degrees and
elevation thirty degrees.](motion_files/figure-html/static-1.svg)

### Save a camera path

The default path makes a full azimuth turn. Each frame retains the panel
dimensions and framing bounds; the final angle precedes the starting
angle so a loop repeats each view once. The result records the rendering
time of each frame in seconds.

``` r

if (requireNamespace("gifski", quietly = TRUE)) {
  path <- tempfile(fileext = ".gif")
  movie <- animate_3d(p, frames = 12, fps = 6, file = path,
                     width = 480, height = 480, progress = FALSE)
  data.frame(frames = movie$frame_count, fps = movie$fps,
             file_written = file.exists(movie$file))
  range(movie$timings)
  unlink(path)
}
```

Camera arguments accept a scalar, a vector with one value per frame, or
a function of time in seconds. This path raises the camera while it
rotates. Varying elevation or roll uses sphere framing, which reserves
bounds for arbitrary rotation.

``` r

frames <- animate_3d(p, frames = 3, fps = 2,
  azimuth = function(t) -60 + 90 * t,
  elevation = c(20, 35, 50), renderer = "vector",
  width = 320, height = 320, progress = FALSE)
frames$camera
#>   time azimuth elevation zoom roll   projection fov distance target_x target_y
#> 1  0.0     -60        20    1    0 orthographic  35       NA      0.5      0.5
#> 2  0.5     -15        35    1    0 orthographic  35       NA      0.5      0.5
#> 3  1.0      30        50    1    0 orthographic  35       NA      0.5      0.5
#>   target_z
#> 1      0.5
#> 2      0.5
#> 3      0.5
frames$panel_extents[[1]]
#> $`panel.9-7-9-7.scene-panel-3d`
#>  width_mm height_mm 
#>  37.08663  37.08663
unlink(dirname(frames$frames[1]), recursive = TRUE)
```

Raster rendering is the default. Selecting vector rendering uses the
exact vector scene backend to produce each PNG frame, and an output
filename ending in `.mp4` selects the video encoder.

``` r

if (requireNamespace("av", quietly = TRUE)) {
  path <- tempfile(fileext = ".mp4")
  movie <- animate_3d(p, frames = 3, fps = 6, file = path,
                     width = 320, height = 320, progress = FALSE)
  file.exists(movie$file)
  unlink(path)
}
```

### Tween observations between states

The time argument selects a column naming the states. Layers without
that column appear in every frame. The following example uses three cars
from the built-in data, then constructs a hypothetical second state with
lower weight and two more miles per gallon. Each car keeps its model
name as its group, including when the second state’s rows are reversed.

``` r

observed <- mtcars[c(1, 3, 8), c("wt", "hp", "mpg")]
observed$id <- rownames(observed)
observed$state <- 0
changed <- transform(observed[3:1, ], wt = wt * 0.9, mpg = mpg + 2, state = 1)
states <- rbind(observed, changed)
moving <- ggplot(states, aes(wt, hp, z = mpg, group = id, colour = id)) +
  geom_point_3d(size = 4) +
  scale_colour_manual(values = unname(PAL[c("blue", "orange", "teal")])) +
  coord_3d()
```

The tween specification inserts three frames between the two supplied
states, producing five frames in total. Statistics run on the supplied
states before interpolation, and all states train one set of scales.

``` r

movement <- animate_3d(moving, time = "state", tween = tween_3d(n = 3),
  renderer = "vector", width = 480, height = 360, fps = 5, progress = FALSE)
movement$frame_count
#> [1] 5
movement$transition
#>   from to fraction
#> 1    0  1     0.00
#> 2    0  1     0.25
#> 3    0  1     0.50
#> 4    0  1     0.75
#> 5    1  1     0.00
```

The first, middle and final frames show the three cars moving to their
new positions. The camera and axis ranges remain fixed.

![Three cars at their observed weights and fuel
consumption.](motion_files/figure-html/point-state-1.png)![The same
three cars halfway to their hypothetical second
positions.](motion_files/figure-html/point-state-3.png)![The three cars
at the hypothetical lower weights and higher fuel
consumption.](motion_files/figure-html/point-state-5.png)

Group matching pairs occurrences within each persistent group. Map one
identifier per moving mark when rows can change order. Source-row
matching is also available, with row numbers interpreted within each
state. Unmatched groups fade in or out by default; the enter and exit
arguments can retain them or define starting and ending attributes.

### Motion trails

The trail window counts preceding rendered frames, including the frames
inserted by
[`tween_3d()`](https://gillescolling.com/ggplot3d/reference/tween_3d.md).
Each moving mark leaves a tube whose alpha fades with age. A group
identifier keeps each object’s history together when rows change order.
Layers with fixed data appear in every frame, and gaps in an object’s
observations break its trail.

``` r

wake <- animate_3d(moving, time = "state", tween = tween_3d(n = 3),
  trail = list(length = 4, radius = 0.05, taper = TRUE),
  renderer = "vector", width = 480, height = 360, progress = FALSE)
```

![Three cars halfway along their motion, with fading tapered
trails.](motion_files/figure-html/motion-trail-3.png)![The cars at their
final positions, with trails showing their recent
paths.](motion_files/figure-html/motion-trail-5.png)

`trail = 4` uses a default tube radius of 0.5% of the panel range
diagonal. Setting `width` in the list selects a ribbon; `normal` sets
its initial orientation. Radius and width use transformed position
units. Mapped `radius`, `width` and `twist` aesthetics also reach the
sweep. `taper = TRUE` multiplies the cross-section size by the age
fraction used for fading. `layers` selects the indices of the moving
layers that leave trails. Trails follow the frame-history approach of
gganimate’s
[shadow_trail()](https://gganimate.com/reference/shadow_trail.html) and
[shadow_wake()](https://gganimate.com/reference/shadow_wake.html), with
connected surfaces built by the rotation-minimising tube and ribbon
sweep.

### Morph a surface

A surface with the same grid and mesh connectivity in every state can
interpolate its heights and fill. Here the two states contain the same
sampled wave with opposite signs. The middle frame is flat because every
pair of heights averages to zero.

``` r

waves <- expand.grid(x = seq(-1, 1, length.out = 8),
                     y = seq(-1, 1, length.out = 8), state = 0:1)
waves$z <- with(waves, sin(pi * x) * cos(pi * y) * (1 - 2 * state))
wave <- ggplot(waves, aes(x, y, z = z, fill = z)) + geom_surface_3d() +
  scale_fill_gradient2(low = PAL["blue"], mid = "white", high = PAL["red"]) +
  coord_3d()
morph <- animate_3d(wave, time = "state", tween = tween_3d(n = 1),
  renderer = "vector", width = 480, height = 360, progress = FALSE)
```

![A wave surface in its first state, coloured by
height.](motion_files/figure-html/surface-state-1.png)![The flat middle
frame interpolated between opposite wave
heights.](motion_files/figure-html/surface-state-2.png)![The final wave
with all heights
reversed.](motion_files/figure-html/surface-state-3.png)

Positions interpolate in transformed scale units, and colours
interpolate in linear light. Paths with different vertex counts are
resampled along their lengths. Polygon rings are aligned before
interpolation. Meshes with changed connectivity cross-fade as complete
shapes. See
[tween_3d()](https://gillescolling.com/ggplot3d/reference/tween_3d.md)
for matching and transition options.

### Using gganimate

The state and time transitions of
[gganimate](https://gganimate.com/reference/transition_time.html) use a
separate plot-building pipeline. Calling its animation renderer with
those transitions and a 3D coordinate system produces guidance to use
the native animation call. Build the ordinary 3D plot, supply its state
column as the time argument, and add a tween specification, as in the
examples above.

The native renderer evaluates visibility for each frame. Reuse of one
geometry partition across successive camera frames is currently
unavailable.

### Explore the scene

The viewer opens one panel at its static camera position. Drag to orbit
and use the wheel or a two-finger pinch to zoom. Arrow keys rotate the
view; plus and minus change magnification.

``` r

if (requireNamespace("htmlwidgets", quietly = TRUE) &&
    requireNamespace("svglite", quietly = TRUE)) {
  view_3d(p, width = 560, height = 560)
}
```

Select **Camera code** to display the current coordinate call, then copy
it into the plot code. **Reset** restores the initial camera. In a Shiny
application, camera changes also update the input whose name is the
widget identifier followed by `_camera`.

The viewer shows the whole plot: every facet panel is live, and the
parts outside the panels (strips, legends, titles) are the static plot’s
own, embedded as SVG around them. Orbiting one panel turns every panel
by the same amount; `view_3d(link = FALSE)` moves each panel alone, and
`panel =` shows one panel by itself.

Hovering a point, line, face or label shows its tooltip, and clicking
selects it. Two aesthetics drive this, following
[ggiraph](https://davidgohel.github.io/ggiraph/): `tooltip` sets the
text, and `data_id` sets the selection key, so rows that share a key
highlight together across layers. Without a `tooltip`, the tooltip lists
the mapped values of the row. Shift-click adds a key to the selection, a
click on empty space clears it, and unselected primitives dim while a
selection exists. In Shiny, the input named by the widget identifier
followed by `_selected` holds the selected keys.

``` r

cars <- data.frame(mtcars, model = rownames(mtcars))
picked <- ggplot(cars, aes(wt, mpg, z = hp, colour = factor(cyl),
    tooltip = model, data_id = model)) +
  geom_point_3d(size = 3) +
  coord_3d() +
  facet_wrap(~am)
if (requireNamespace("htmlwidgets", quietly = TRUE) &&
    requireNamespace("svglite", quietly = TRUE)) {
  view_3d(picked, width = 620, height = 360)
}
```

Linked brushing across widgets uses
[crosstalk](https://rstudio.github.io/crosstalk/): pass the `SharedData`
whose `origData()` the plot draws as `view_3d(plot, data = shared)`.
Selections then travel between the viewer and the other widgets of the
group, and a crosstalk filter hides the rows it removes.

The browser draws opaque geometry with a depth buffer, which records the
nearest painted surface at each pixel. Transparent primitives are
ordered by centre depth, so intersecting transparent surfaces can differ
from static vector output. Browser labels use plain text. The pointer
finds its primitive in an offscreen image of primitive identifiers, the
same `primitive_id` the raster backend returns in its id buffer.

## Camera conventions

The camera path keeps the plot data fixed.
[gganimate](https://gganimate.com/reference/transition_time.html)
separates changes in data over time from its [fixed
view](https://gganimate.com/reference/view_static.html); the fixed-view
principle informs the locked animation bounds here. [rayshader’s movie
renderer](https://www.rayshader.com/reference/render_movie.html)
supplies the precedent for scalar and per-frame camera vectors and
extension-selected encoders. [rgl’s movie
recorder](https://dmurdoch.github.io/rgl/reference/play3d.html) and
[spin function](https://dmurdoch.github.io/rgl/reference/spin3d.html)
supply the time-function pattern.

The viewer orbits around the coordinate system’s target and retains its
projection and roll. [Plotly’s scene
camera](https://plotly.com/javascript/reference/layout/scene/#scene-camera)
exposes target, eye and up vectors with a projection choice. Dragging
and wheel zoom follow [three.js orbit
controls](https://threejs.org/docs/pages/OrbitControls.html). Geometry,
colours and lighting travel to the browser once; camera changes are
evaluated there.
