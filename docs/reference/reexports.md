# Objects exported from other packages

These objects are imported from other packages. Follow the links below
to see their documentation.

- ggplot2:

  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)

## Examples

``` r
v <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 1))
fortify(mesh_3d(v, matrix(1:3, nrow = 1)))
```
