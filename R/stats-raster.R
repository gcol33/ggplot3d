# A raster as one row per cell and layer: cell centre x/y, value, layer name
# and the cell's column and row, which keep the lattice when x/y are reprojected.
raster_frame_3d <- function(raster, maxcell = Inf) {
  if (!is.numeric(maxcell) || length(maxcell) != 1L || is.na(maxcell) || maxcell < 4) stop("maxcell must be a number of at least 4.", call. = FALSE)
  if (inherits(raster, "SpatRaster")) return(spatraster_frame_3d(raster, maxcell))
  if (inherits(raster, "stars")) return(stars_frame_3d(raster, maxcell))
  stop("Raster input must be a terra SpatRaster or a stars object.", call. = FALSE)
}

raster_long_3d <- function(xy, col, row, values, layers, crs, geographic) {
  n <- nrow(xy); k <- length(layers)
  result <- data.frame(x = rep(unname(xy[, 1L]), k), y = rep(unname(xy[, 2L]), k), value = as.vector(values),
    lyr = factor(rep(layers, each = n), levels = unique(layers)),
    .col = rep(as.integer(col), k), .row = rep(as.integer(row), k))
  attr(result, "geometry_crs_3d") <- crs
  attr(result, "geometry_geographic_3d") <- isTRUE(geographic)
  result
}

spatraster_frame_3d <- function(raster, maxcell) {
  require_geometry_package_3d("terra")
  if (terra::ncell(raster) > maxcell) raster <- terra::spatSample(raster, size = maxcell, method = "regular", as.raster = TRUE)
  cells <- seq_len(terra::ncell(raster))
  crs <- terra::crs(raster)
  raster_long_3d(terra::xyFromCell(raster, cells), terra::colFromCell(raster, cells), terra::rowFromCell(raster, cells),
    terra::values(raster, mat = TRUE), names(raster), if (nzchar(crs)) crs else NA_character_,
    nzchar(crs) && terra::is.lonlat(raster))
}

stars_frame_3d <- function(raster, maxcell) {
  require_geometry_package_3d("stars")
  dims <- dim(raster)
  if (length(dims) < 2L || length(dims) > 3L) stop("stars input requires two spatial dimensions and at most one band dimension.", call. = FALSE)
  cells <- dims[[1L]] * dims[[2L]]
  if (cells > maxcell) {
    step <- ceiling(sqrt(cells / maxcell)) - 1L
    raster <- stars::st_downsample(raster, c(step, step, rep(0L, length(dims) - 2L)))
    dims <- dim(raster); cells <- dims[[1L]] * dims[[2L]]
  }
  coords <- as.matrix(as.data.frame(raster[1L], center = TRUE)[seq_len(cells), 1:2])
  bands <- if (length(dims) == 3L) stars::st_get_dimension_values(raster, 3L) else NULL
  attributes <- names(raster)
  layers <- if (is.null(bands)) attributes else if (length(attributes) == 1L) as.character(bands) else
    paste(rep(attributes, each = length(bands)), rep(as.character(bands), length(attributes)), sep = ".")
  values <- matrix(unlist(lapply(attributes, function(a) as.numeric(raster[[a]])), use.names = FALSE), nrow = cells)
  raster_long_3d(coords, rep(seq_len(dims[[1L]]), dims[[2L]]), rep(seq_len(dims[[2L]]), each = dims[[1L]]),
    values, layers, sf::st_crs(raster), sf::st_is_longlat(raster))
}

#' @export
fortify.SpatRaster <- function(model, data = NULL, ...) raster_frame_3d(model)

#' @export
fortify.stars <- function(model, data = NULL, ...) raster_frame_3d(model)

StatSpatRaster3D <- ggplot2::ggproto("StatSpatRaster3D", StatGeometry3D,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(value)),
  compute_group = function(data, scales) {
    if (is.null(data$value) || is.null(data$.col)) stop("geom_spatraster_3d() requires raster data.", call. = FALSE)
    data$z <- stats_scale_3d(scales, "z")$get_transformation()$transform(data$value)
    lattice <- list(i = data$.col, j = data$.row)
    triangles <- lattice_triangles_3d(lattice, data$z)
    if (!nrow(triangles)) return(data.frame())
    data <- surface_normals_3d(data, lattice)
    rows <- as.vector(t(triangles))
    out <- data[rows, , drop = FALSE]
    out$.face <- rep(seq_len(nrow(triangles)), each = 3L)
    out$.corner <- rep.int(1:3, nrow(triangles))
    out$value <- rep(rowMeans(matrix(data$value[triangles], ncol = 3L)), each = 3L)
    out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
    out
  }
)

GeomSpatRaster3D <- ggplot2::ggproto("GeomSpatRaster3D", GeomStatsSurface3D,
  optional_aes = union(GeomStatsSurface3D$optional_aes, "value")
)

prepare_raster_aesthetics_3d <- function(out, data, plot, params) {
  for (key in c("value", ".col", ".row")) out[[key]] <- data[[key]]
  xy <- spatial_xy_3d(data.frame(x = out$x, y = out$y), data, plot)
  out$x <- xy$x; out$y <- xy$y
  out
}

register_geom_3d(GeomSpatRaster3D, emit_stats_surface_3d, parameters = surface_parameters_3d,
  prepare_aesthetics = prepare_raster_aesthetics_3d)

#' Rasters as height surfaces
#'
#' Draws each layer of a terra `SpatRaster` or a `stars` object as a surface
#' whose height and fill are the cell values. Cells are the grid nodes at their
#' centres; missing cells leave holes. Rasters with more than `maxcell` cells
#' are first resampled to a regular sample of about `maxcell` cells, as
#' tidyterra's `geom_spatraster()` does. Layers (or bands) are separate
#' surfaces grouped by the computed `lyr` variable, ready for
#' `facet_wrap(~lyr)`.
#'
#' The computed variables are `value` (the mean cell value of each triangle,
#' the default fill), `lyr`, and `z_mean`. A geographic raster needs a
#' projected target, `coord_3d(crs = )`; the cells keep their lattice when
#' reprojected.
#'
#' `fortify()` methods turn `SpatRaster` and `stars` objects into data frames
#' with `x`, `y`, `value` and `lyr` columns at full resolution, and lidR `LAS`
#' point clouds into one row per point with `x`, `y`, `z`, `intensity`,
#' `classification` and the other point attributes in snake case.
#' @param mapping,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param data A terra `SpatRaster` or a `stars` object.
#' @param maxcell Largest number of cells drawn per layer.
#' @param shade Whether to apply lighting.
#' @param wireframe Whether to draw triangle edges.
#' @return A ggplot2 layer.
#' @seealso [stat_profile_3d()] for cross-sections through the surface.
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(volcano[nrow(volcano):1, ], extent = terra::ext(0, 610, 0, 870))
#'   ggplot2::ggplot() + geom_spatraster_3d(data = r, maxcell = 2000) +
#'     ggplot2::scale_fill_viridis_c() + coord_3d(aspect = c(610, 870, 300))
#' }
#' if (requireNamespace("stars", quietly = TRUE)) {
#'   s <- stars::st_as_stars(volcano)
#'   ggplot2::ggplot() + geom_spatraster_3d(data = s, maxcell = 1500) + coord_3d()
#' }
#' if (requireNamespace("lidR", quietly = TRUE)) {
#'   las <- lidR::readLAS(system.file("extdata", "MixedConifer.laz", package = "lidR"),
#'     filter = "-keep_random_fraction 0.05")
#'   ggplot2::ggplot(ggplot2::fortify(las), ggplot2::aes(x, y, z = z, colour = z)) +
#'     geom_point_3d(size = 0.3) + coord_3d(aspect = c(1, 1, 0.4))
#' }
#' @export
geom_spatraster_3d <- function(mapping = NULL, data, position = "identity", ..., maxcell = 50000,
                               shade = TRUE, wireframe = FALSE,
                               na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  frame <- raster_frame_3d(data, maxcell)
  defaults <- ggplot2::aes(x = x, y = y, group = lyr)
  for (name in names(mapping)) defaults[[name]] <- mapping[[name]]
  make_layer_3d(GeomSpatRaster3D, defaults, frame, StatSpatRaster3D, position,
    c(list(shade = shade, wireframe = wireframe, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

utils::globalVariables(c("x", "y", "lyr", "value"))
