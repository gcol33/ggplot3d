# Output backends

A backend turns scene geometry into drawing commands or pixels. This
article shows how to select one and save its output. Installation is
covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).

1.  **Choose** rendering with
    [`backend_3d()`](https://gillescolling.com/ggplot3d/reference/backend_3d.md).

2.  **Save** the resulting plot with
    [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Example

The data describe a curved surface on a regular grid. The same layers
and camera can be rendered with either backend.

``` r

d <- expand.grid(x = seq(-2, 2, length.out = 16),
                 y = seq(-2, 2, length.out = 16))
d$z <- with(d, exp(-(x^2 + y^2) / 2))
p <- ggplot(d, aes(x, y, z = z)) + geom_surface_3d(fill = PAL["blue"])
```

### Vector rendering

Vector rendering draws paths, filled faces and text with R’s grid
graphics system. Use it for scalable figures and geometry that will be
edited in a vector graphics application. A device must support the
clipping paths required by partial visibility masks.

``` r

vector_plot <- p + coord_3d(backend = backend_3d("vector"))
vector_plot
```

![A curved surface drawn using the vector scene
backend.](backends_files/figure-html/vector-1.svg)

The backend describes scene rendering; the file extension selects the
output device. A vector scene saved to PNG is rasterized by that device.
Save to PDF or SVG to retain vector geometry.

``` r

path <- tempfile(fileext = ".pdf")
ggsave(path, vector_plot, width = 5, height = 4.5)
file.info(path)$size > 0
#> [1] TRUE
unlink(path)
```

### Raster rendering

Raster rendering samples the scene into pixels. Use it for dense
surfaces or an image whose final dimensions are known. Resolution is
measured in dots per inch, abbreviated dpi. The antialias factor
increases the sampling resolution before reducing the image to its
output size.

``` r

raster_plot <- p + coord_3d(backend = backend_3d("raster", dpi = 144,
                                              antialias = 2))
raster_plot
```

![The same curved surface sampled by the raster scene
backend.](backends_files/figure-html/raster-1.svg)

The scene raster resolution and the saved PNG resolution are separate
settings. Set both for the intended physical size. Increasing either
raster dpi or the antialias factor increases memory use.

``` r

path <- tempfile(fileext = ".png")
ggsave(path, raster_plot, width = 5, height = 4.5, dpi = 144)
file.info(path)$size > 0
#> [1] TRUE
unlink(path)
```

Axes and labels are composed through grid around the raster scene. A PDF
containing this plot includes the raster image at its chosen resolution.

## Choosing output

| Task                                 | Backend | File                         |
|--------------------------------------|---------|------------------------------|
| Edit paths and faces                 | Vector  | PDF or SVG                   |
| Scale a modest scene for print       | Vector  | PDF or SVG                   |
| Export a dense scene at a fixed size | Raster  | PNG or PDF                   |
| Insert a figure into a slide         | Either  | PNG, PDF or SVG as supported |

Visibility is resolved across each panel’s layers.
[Visibility](https://gillescolling.com/ggplot3d/articles/visibility.md)
explains the geometric ordering used for vector output and the role of
finite stroke width.
