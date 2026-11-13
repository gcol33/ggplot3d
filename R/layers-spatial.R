spatial_kinds_3d <- c("point", "line", "polygon")

# Each part of a simple feature is a kind and a list of coordinate matrices:
# one per point, one per line, or the rings of one polygon.
spatial_parts_3d <- function(g) {
  type <- class(g)[2L]
  if (!length(g) || (type == "POINT" && all(is.na(g)))) return(list())
  part <- function(kind, rings) list(list(kind = kind, rings = rings))
  switch(type,
    POINT = part("point", list(matrix(unclass(g), nrow = 1L))),
    MULTIPOINT = do.call(c, lapply(seq_len(nrow(g)), function(i) part("point", list(unclass(g)[i, , drop = FALSE])))),
    LINESTRING = part("line", list(unclass(g))),
    MULTILINESTRING = do.call(c, lapply(g, function(l) part("line", list(l)))),
    POLYGON = , TRIANGLE = part("polygon", unclass(g)),
    MULTIPOLYGON = , TIN = , POLYHEDRALSURFACE = do.call(c, lapply(g, function(p) part("polygon", p))),
    GEOMETRYCOLLECTION = do.call(c, lapply(g, spatial_parts_3d)),
    stop("Unsupported spatial geometry: ", type, ".", call. = FALSE))
}

# One row per vertex with the feature's attributes. Polygon rings drop their
# closing vertex; .z3d is NA for geometries without a Z dimension.
spatial_rows_3d <- function(data) {
  require_geometry_package_3d("sf")
  if (inherits(data, "sfc")) data <- sf::st_sf(geometry = data)
  geometry <- sf::st_geometry(data)
  attrs <- sf::st_drop_geometry(data)
  coords <- feature <- part <- ring <- kind <- vector("list", length(geometry))
  for (f in seq_along(geometry)) {
    g <- geometry[[f]]
    has_z <- class(g)[1L] %in% c("XYZ", "XYZM")
    parts <- spatial_parts_3d(g)
    rings <- unlist(lapply(parts, `[[`, "rings"), recursive = FALSE)
    rings <- lapply(rings, function(xyz) {
      xyz <- unclass(xyz)
      cbind(xyz[, 1L], xyz[, 2L], if (has_z) xyz[, 3L] else NA_real_)
    })
    polygon <- rep(vapply(parts, `[[`, character(1), "kind") == "polygon", lengths(lapply(parts, `[[`, "rings")))
    rings[polygon] <- lapply(rings[polygon], function(xyz) {
      n <- nrow(xyz)
      if (n > 1L && all(xyz[1L, 1:2] == xyz[n, 1:2])) xyz[-n, , drop = FALSE] else xyz
    })
    sizes <- vapply(rings, nrow, integer(1))
    part_id <- rep(seq_along(parts), lengths(lapply(parts, `[[`, "rings")))
    coords[[f]] <- do.call(rbind, rings)
    feature[[f]] <- rep.int(f, sum(sizes))
    part[[f]] <- rep(part_id, sizes)
    ring[[f]] <- rep(unlist(lapply(parts, function(p) seq_along(p$rings))), sizes)
    kind[[f]] <- rep(vapply(parts, `[[`, character(1), "kind")[part_id], sizes)
  }
  xyz <- do.call(rbind, coords) %or3d% matrix(numeric(), ncol = 3L)
  feature <- unlist(feature, use.names = FALSE) %or3d% integer()
  result <- attrs[feature, , drop = FALSE]
  rownames(result) <- NULL
  result$.x3d <- xyz[, 1L]; result$.y3d <- xyz[, 2L]; result$.z3d <- xyz[, 3L]
  result$.feature <- feature
  result$.part <- unlist(part, use.names = FALSE) %or3d% integer()
  result$.ring <- unlist(ring, use.names = FALSE) %or3d% integer()
  result$.kind <- unlist(kind, use.names = FALSE) %or3d% character()
  attr(result, "geometry_crs_3d") <- sf::st_crs(data)
  attr(result, "geometry_geographic_3d") <- isTRUE(sf::st_is_longlat(data))
  result
}

spatial_xy_3d <- function(xy, data, plot) {
  target <- plot@coordinates$config$crs
  if (!is.null(target)) {
    require_geometry_package_3d("sf")
    source <- attr(data, "geometry_crs_3d")
    if (is.null(source) || is.na(source)) stop("A target CRS requires a declared source CRS.", call. = FALSE)
    projected <- sf::st_transform(sf::st_as_sf(xy, coords = names(xy), crs = source, na.fail = FALSE), target)
    if (isTRUE(sf::st_is_longlat(projected))) stop("Planar geometry requires a projected target CRS.", call. = FALSE)
    xy[] <- as.data.frame(sf::st_coordinates(projected))[1:2]
  } else if (isTRUE(attr(data, "geometry_geographic_3d"))) stop("Geographic geometry requires coord_3d(crs = <projected CRS>).", call. = FALSE)
  xy
}

prepare_sf_data_3d <- function(data) {
  if (inherits(data, "sf") || inherits(data, "sfc")) spatial_rows_3d(data) else data
}

# Empty geometries of each kind let ggplot2's GeomSf choose point, line or
# polygon defaults and legend keys without repeating each feature's geometry.
spatial_prototypes_3d <- function(kind) {
  prototypes <- sf::st_sfc(sf::st_point(), sf::st_linestring(), sf::st_polygon())
  prototypes[match(kind, spatial_kinds_3d)]
}

prepare_sf_aesthetics_3d <- function(out, data, plot, params) {
  if (!".x3d" %in% names(data)) stop("Spatial layers require sf data.", call. = FALSE)
  out <- prepare_spatial_aesthetics_3d(out, data, plot, params, z = FALSE)
  out$.kind <- data$.kind
  out$geometry <- spatial_prototypes_3d(data$.kind)
  own <- data$.z3d
  has_z <- !is.na(own)
  extrude <- !is.null(out$height %or3d% params$height) || !is.null(out$zmax %or3d% params$zmax)
  if (extrude) {
    if (any(has_z) && is.null(out$zmin)) {
      out$zmin <- rep_len(params$zmin %or3d% params$base %or3d% 0, nrow(out))
      out$zmin[has_z] <- own[has_z]
    }
    if (is.null(out$zmax) && is.null(params$zmax)) out$z <- NULL
    out <- solid_heights_3d(out, params)
  } else if (any(has_z)) {
    if (is.null(out[["z"]])) out$z <- rep(-Inf, nrow(out))
    out$z[has_z] <- own[has_z]
  }
  out
}

emit_sf_3d <- function(data, params, panel_params) {
  data$geometry <- NULL
  extrude <- !is.null(data$zmin)
  kind <- data$.kind
  part <- function(k) data[kind == k, , drop = FALSE]
  packets <- list()
  if (any(kind == "point")) packets$point <- emit_points_3d(part("point"), params, panel_params)
  if (any(kind == "line")) {
    lines <- part("line")
    packets$line <- if (extrude) {
      lines$xend <- lines$x; lines$yend <- lines$y; lines$zend <- lines$zmin
      fill <- lines$fill %or3d% rep(NA_character_, nrow(lines))
      lines$fill <- ifelse(is.na(fill), lines$colour, fill)
      emit_bands_3d(lines, params, panel_params)
    } else emit_paths_3d(lines, params, panel_params)
  }
  if (any(kind == "polygon")) {
    polygons <- part("polygon")
    packets$polygon <- if (extrude) emit_extrusions_3d(polygons, params, panel_params) else emit_polygons_3d(polygons, params, panel_params)
  }
  merge_scene_packets(unname(packets), panel_params$panel_id)
}

register_geom_3d(ggplot2::GeomSf, emit_sf_3d, aesthetics = c("height", "subgroup"),
  stat_adapter = ggplot2::StatIdentity, default_z = -Inf,
  prepare_data = prepare_sf_data_3d, prepare_aesthetics = prepare_sf_aesthetics_3d,
  position_check = function(position, scales) check_position_3d(position, scales))

#' Simple features in three dimensions
#'
#' Draws sf data inside [coord_3d()]: points as markers, lines as paths and
#' polygons as flat faces, each feature keeping its attributes for mapping.
#' Geometries with a Z dimension stand at their own heights; other features
#' lie on the floor of the box unless `z` is mapped or set. Mapping `height`
#' (or `zmax`) turns polygons into prisms from `base` or `zmin`, which makes a
#' prism map, and lines into walls filled with their colour unless `fill` is
#' set. Polygons must be planar.
#'
#' `ggplot2::geom_sf()` layers are drawn the same way; add [coord_3d()] after
#' them, because `geom_sf()` brings its own default coordinate system.
#' Geographic coordinates require a projected target, `coord_3d(crs = )`,
#' which also reprojects data with other coordinate systems.
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param base Prism baseline in original z units, overridden by zmin.
#' @param cap Prism caps to draw: both, top, bottom or none.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @seealso [geom_extrude_3d()] for polygons given as x/y rings,
#'   [geom_spatraster_3d()] for rasters, [geom_arc_3d()] for flows.
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   ggplot2::ggplot(nc) +
#'     geom_sf_3d(ggplot2::aes(height = BIR74 / 1000, fill = BIR74), colour = "grey30") +
#'     coord_3d(crs = 32119, aspect = c(4, 1.6, 1), elevation = 40)
#'   ggplot2::ggplot(nc[1:20, ]) + ggplot2::geom_sf(fill = "grey90") +
#'     coord_3d(crs = 32119, aspect = c(2, 1, 0.3))
#' }
#' @export
geom_sf_3d <- function(mapping = NULL, data = NULL, position = "identity", ...,
                       base = 0, cap = "both", shade = TRUE,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  cap <- match.arg(cap, c("both", "top", "bottom", "none"))
  make_layer_3d(ggplot2::GeomSf, mapping, data, ggplot2::StatIdentity, position,
    c(list(base = base, cap = cap, shade = shade, na.rm = na.rm), list(...)),
    show.legend, inherit.aes, constructor = ggplot2::layer_sf)
}

#' @export
fortify.LAS <- function(model, data = NULL, ...) {
  require_geometry_package_3d("lidR")
  points <- as.data.frame(model@data)
  names(points) <- tolower(gsub("([a-z])([A-Z])", "\\1_\\2", names(points)))
  points
}
