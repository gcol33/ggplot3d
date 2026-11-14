# Longitude, latitude and height on a globe

Positions are longitude and latitude in degrees and height above a
sphere. Paths follow the shorter great circle and interpolate height
linearly. Supply an intermediate point for antipodal endpoints. Polygon
edges and interiors use the shared geodesic sampler. Simple features are
transformed to EPSG:4326. Heights of two-dimensional sf features default
to zero. Data-space lighting illuminates the opaque sphere and its
day/night boundary.

## Usage

``` r
coord_globe_3d(
  radius = 1,
  exaggeration = 1,
  graticule = c(30, 30),
  centre = c(0, 20),
  inner = TRUE,
  ...
)
```

## Arguments

- radius:

  Positive sphere radius, in height units.

- exaggeration:

  Positive height multiplier.

- graticule:

  Longitude and latitude spacing in degrees; zero disables it.

- centre:

  Longitude and latitude facing the camera, in degrees.

- inner:

  Draw an opaque sphere hiding the far side.

- ...:

  Other arguments to
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Value

A Coord3D object with a globe frame.

## References

GeoMakie: <https://geo.makie.org/stable/>.

## Examples

``` r
route <- data.frame(lon = c(-74, 140), lat = c(41, 36), height = 0.02)
ggplot2::ggplot(route, ggplot2::aes(lon, lat, z = height)) +
  geom_path_3d(colour = "#B54532", linewidth = 1) +
  coord_globe_3d(centre = c(-150, 35))
```
