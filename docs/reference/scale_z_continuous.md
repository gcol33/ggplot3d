# Continuous z position scales

These scales use ggplot2 position scale transformations, limits and
breaks.

## Usage

``` r
scale_z_continuous(..., position = "auto")

scale_z_discrete(..., position = "auto")

scale_z_log10(...)

scale_z_sqrt(...)

scale_z_reverse(...)

scale_z_date(..., position = "auto")

scale_z_datetime(..., position = "auto")

scale_z_time(..., position = "auto")
```

## Arguments

- ...:

  Arguments passed to the corresponding ggplot2 x scale constructor.

- position:

  Cube edge used for the z guide.

## Value

A ggplot2 position scale for the z aesthetic family.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 2, 1), z = c(1, 10, 100))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d() + coord_3d()
p + scale_z_continuous(name = "Height")
p + scale_z_log10()
p + scale_z_sqrt()
p + scale_z_reverse()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = factor(z))) +
  geom_point_3d() + scale_z_discrete() + coord_3d()
d$day <- as.Date("2026-01-01") + 0:2
ggplot2::ggplot(d, ggplot2::aes(x, y, z = day)) +
  geom_point_3d() + scale_z_date() + coord_3d()
d$time <- as.POSIXct("2026-01-01", tz = "UTC") + (0:2) * 3600
ggplot2::ggplot(d, ggplot2::aes(x, y, z = time)) +
  geom_point_3d() + scale_z_datetime() + coord_3d()
if (requireNamespace("hms", quietly = TRUE)) {
  d$elapsed <- as.difftime(1:3, units = "hours")
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = elapsed)) +
    geom_point_3d() + scale_z_time() + coord_3d()
}
```
