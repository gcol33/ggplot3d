# Select a 3D rendering backend

Select a 3D rendering backend

## Usage

``` r
backend_3d(type = "vector", dpi = 144, antialias = 2L)
```

## Arguments

- type:

  Backend, vector or raster.

- dpi:

  Physical raster resolution.

- antialias:

  Positive integer supersampling factor.

## Value

A backend configuration for coord_3d().

## Examples

``` r
backend_3d("vector")
backend_3d("raster", dpi = 144, antialias = 2)
```
