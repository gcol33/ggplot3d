# Stack column heights along z

Stack column heights along z

## Usage

``` r
position_stack_3d(vjust = 1, reverse = FALSE)
```

## Arguments

- vjust:

  Fraction of each stacked interval used for z.

- reverse:

  Reverse group order.

## Value

A ggplot2 position adjustment.

## Examples

``` r
d <- data.frame(x = 1, y = 1, z = c(2, 3), part = c("a", "b"))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_stack_3d()) + coord_3d()
```
