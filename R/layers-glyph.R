glyph_default_radius_3d <- 0.025
glyph_default_size_3d <- 3

draw_key_glyph_3d <- function(data, params, size) {
  fill <- if (is.na(data$fill %or3d% NA)) NA else scales::alpha(data$fill, data$alpha %or3d% 1)
  r <- if (is.null(data$size)) grid::unit(0.38, "npc") else grid::unit(data$size / 2, "mm")
  grid::circleGrob(r = r,
    gp = grid::gpar(fill = fill, col = data$colour %or3d% NA, lwd = (data$linewidth %or3d% 0.3) * ggplot2::.pt))
}

identity_maps_3d <- function(scale) {
  n <- length(scale)
  maps <- array(0, c(3L, 3L, n))
  for (k in 1:3) maps[k, k, ] <- scale
  maps
}

# Maps from data units to normalised scale space.
normalise_maps_3d <- function(maps, panel_params) {
  span <- vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
  maps / span
}

model_scale_3d <- function(panel_params) panel_params$camera$aspect / max(panel_params$camera$aspect)

# Directions in normalised scale space carried into aspect-scaled model space
# at each centre.
model_directions_3d <- function(frame, centre, direction, scale) {
  if (isTRUE(frame$identity)) return(sweep(direction, 2L, scale, "*"))
  h <- 1e-6
  moved <- (frame$map(centre + h * direction) - frame$map(centre - h * direction)) / (2 * h)
  sweep(moved, 2L, scale, "*")
}

# Aesthetics set as constants reach setup_data() as parameters.
constant_aes_3d <- function(data, params, names) {
  for (name in names) if (is.null(data[[name]]) && !is.null(params[[name]])) data[[name]] <- rep(params[[name]], length.out = nrow(data))
  data
}

check_unit_3d <- function(unit, name) {
  if (!is.character(unit) || length(unit) != 1L || !unit %in% c("data", "mm")) stop(name, " must be \"data\" or \"mm\".", call. = FALSE)
  unit
}

# Spheres ---------------------------------------------------------------------

sphere_maps_3d <- function(data) identity_maps_3d(as.double(data$radius %or3d% rep(NA_real_, nrow(data))))

GeomSphere3D <- ggplot2::ggproto("GeomSphere3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = utils::modifyList(solid_defaults_3d(), ggplot2::aes(radius = NA)),
  optional_aes = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = draw_key_glyph_3d,
  setup_data = function(data, params) {
    data <- constant_aes_3d(data, params, "radius")
    if (!is.null(data$radius) && any(data$radius < 0, na.rm = TRUE)) stop("Sphere radii must be nonnegative.", call. = FALSE)
    if (identical(params$radius.unit %or3d% "data", "data") && !is.null(data$radius)) {
      data <- glyph_limits_3d(data, sphere_maps_3d(data), glyph_mesh_3d("sphere", 0L), is.finite(data$radius))
    }
    data
  }
)

emit_spheres_3d <- function(data, params, panel_params) {
  unit <- check_unit_3d(params$radius.unit, "radius.unit")
  maps <- sphere_maps_3d(data)
  space <- ifelse(is.finite(data$radius %or3d% rep(NA_real_, nrow(data))), unit, "model")
  if (unit == "data") maps[, , space == "data"] <- normalise_maps_3d(maps[, , space == "data", drop = FALSE], panel_params)
  maps[, , space == "model"] <- identity_maps_3d(rep(glyph_default_radius_3d, sum(space == "model")))
  emit_glyph_instances_3d(data, maps, space, glyph_mesh_3d("sphere", params$detail), params, panel_params)
}

register_geom_3d(GeomSphere3D, emit_spheres_3d, parameters = list(radius.unit = "data", detail = 2L),
  position_check = function(position, scales) check_position_3d(position, scales))

#' Spheres
#'
#' Draws one solid sphere per row, centred on x, y, z. Spheres are triangle
#' meshes in the panel scene: they intersect surfaces and other solids exactly,
#' take light from [light_3d()], and shrink with distance under a perspective
#' camera, unlike the flat markers of [geom_point_3d()].
#'
#' With `radius.unit = "data"` the `radius` aesthetic is measured in the
#' scale-transformed units of each position axis, as in Makie's `meshscatter`
#' and rgl's `spheres3d()`. A sphere is then round on screen only when the box
#' sides are proportional to the data ranges, `coord_3d(aspect = "data")`;
#' otherwise it is the ellipsoid those units describe. Data-unit spheres
#' expand the position scales to contain them. With `radius.unit = "mm"` the
#' radius is the on-screen radius of a sphere at the centre of the view, so
#' spheres stay round whatever the box proportions. Rows without a radius get
#' 2.5% of the longest box side.
#'
#' Each sphere is an icosahedron subdivided `detail` times: 20 * 4^detail
#' triangles, 320 at the default `detail = 2`. In curvilinear frames each
#' sphere is placed at its mapped centre and keeps its shape.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param radius.unit Unit of the `radius` aesthetic: `"data"` or `"mm"`.
#' @param detail Subdivision level of the icosphere, from 0 to 6.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @seealso [geom_ellipsoid_3d()], [geom_glyph_3d()].
#' @examples
#' d <- data.frame(x = c(1, 2, 3, 2), y = c(1, 3, 1, 2), z = c(1, 1, 1, 2.5),
#'                 r = c(0.4, 0.6, 0.5, 0.8), g = c("a", "b", "a", "b"))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, radius = r, fill = g)) +
#'   geom_sphere_3d() + coord_3d(aspect = "data")
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = qsec, fill = hp)) +
#'   geom_sphere_3d(radius = 2, radius.unit = "mm", detail = 1) + coord_3d()
#' @export
geom_sphere_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                           radius.unit = "data", detail = 2L, shade = TRUE,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSphere3D, mapping, data, stat, position,
    c(list(radius.unit = check_unit_3d(radius.unit, "radius.unit"), detail = check_detail_3d(detail),
      shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Glyphs ----------------------------------------------------------------------

glyph_directions_3d <- function(data) {
  if (!any(c("u", "v", "w") %in% names(data))) return(NULL)
  direction <- vapply(c("u", "v", "w"), function(a) as.double(data[[a]] %or3d% rep(0, nrow(data))), numeric(nrow(data)))
  matrix(direction, nrow(data), 3L)
}

# Data-unit maps: the glyph scaled by size / 2 and turned so its z axis
# follows u, v, w.
glyph_data_maps_3d <- function(data) {
  half <- as.double(data$size) / 2
  direction <- glyph_directions_3d(data)
  rotation <- if (is.null(direction)) identity_maps_3d(rep(1, nrow(data))) else align_z_3d(direction)
  rotation * rep(half, each = 9L)
}

GeomGlyph3D <- ggplot2::ggproto("GeomGlyph3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = utils::modifyList(solid_defaults_3d(), ggplot2::aes(size = !!glyph_default_size_3d)),
  optional_aes = c("u", "v", "w", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = draw_key_glyph_3d,
  setup_data = function(data, params) {
    data <- constant_aes_3d(data, params, "size")
    if (is.null(data$size)) data$size <- rep(glyph_default_size_3d, nrow(data))
    if (any(data$size < 0, na.rm = TRUE)) stop("Glyph sizes must be nonnegative.", call. = FALSE)
    if (identical(params$size.unit %or3d% "mm", "data")) {
      maps <- glyph_data_maps_3d(data)
      data <- glyph_limits_3d(data, maps, glyph_mesh_3d(params$glyph %or3d% "sphere", 0L), apply(maps, 3L, function(m) all(is.finite(m))))
    }
    data
  }
)

emit_glyphs_3d <- function(data, params, panel_params) {
  unit <- check_unit_3d(params$size.unit, "size.unit")
  mesh <- glyph_mesh_3d(params$glyph, params$detail)
  if (unit == "data") {
    maps <- normalise_maps_3d(glyph_data_maps_3d(data), panel_params)
  } else {
    direction <- glyph_directions_3d(data)
    maps <- if (is.null(direction)) identity_maps_3d(rep(1, nrow(data))) else {
      span <- vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
      align_z_3d(model_directions_3d(frame_of_3d(panel_params), as.matrix(data[c("x", "y", "z")]),
        sweep(direction, 2L, span, "/"), model_scale_3d(panel_params)))
    }
    maps <- maps * rep(as.double(data$size) / 2, each = 9L)
  }
  params$size.unit <- "mm"
  emit_glyph_instances_3d(data, maps, rep(unit, nrow(data)), mesh, params, panel_params)
}

register_geom_3d(GeomGlyph3D, emit_glyphs_3d, parameters = list(glyph = "sphere", detail = 2L),
  position_check = function(position, scales) check_position_3d(position, scales))

#' One solid glyph per observation
#'
#' Places a copy of a small mesh at each x, y, z, as Makie's `meshscatter` and
#' PyVista's `glyph()` do. Built-in glyphs are `"sphere"`, `"cube"`, `"cone"`,
#' `"cylinder"` and `"octahedron"`, all centred on the origin and spanning -1 to
#' 1 along each axis; any [mesh_3d()] or rgl `mesh3d` object can be used
#' instead, with its origin at the observation. The glyphs are solids in the
#' panel scene: they intersect other geometry exactly and take light.
#'
#' The `size` aesthetic is the width of a built-in glyph: a mesh is scaled by
#' `size / 2`. With `size.unit = "mm"` (the default) it is measured on screen at
#' the centre of the view, so a mapped `size` follows the usual size scale; with
#' `size.unit = "data"` it is measured in the scale-transformed units of each
#' position axis (use [ggplot2::scale_size_identity()] to keep mapped values as
#' they are) and the glyphs expand the position scales.
#'
#' The optional `u`, `v` and `w` aesthetics give a direction in data units; the
#' glyph's z axis, the apex of the cone, turns to follow it. Cones and
#' cylinders have `8 * 2^detail` sides; spheres are icospheres of
#' `20 * 4^detail` triangles.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param glyph A built-in glyph name or a mesh from [mesh_3d()] or [read_mesh_3d()].
#' @param size.unit Unit of the `size` aesthetic: `"mm"` or `"data"`.
#' @param detail Tessellation level of the built-in round glyphs, from 0 to 6.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @seealso [geom_sphere_3d()], [geom_ellipsoid_3d()].
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:3, z = 1)
#' d$u <- d$y - 2; d$v <- 2 - d$x; d$w <- 1
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w)) +
#'   geom_glyph_3d(glyph = "cone", size = 6, fill = "tomato") + coord_3d()
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = qsec, size = hp, fill = factor(cyl))) +
#'   geom_glyph_3d(glyph = "cube") + coord_3d()
#' @export
geom_glyph_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          glyph = "sphere", size.unit = "mm", detail = 2L, shade = TRUE,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  glyph_mesh_3d(glyph, detail)
  make_layer_3d(GeomGlyph3D, mapping, data, stat, position,
    c(list(glyph = glyph, size.unit = check_unit_3d(size.unit, "size.unit"), detail = check_detail_3d(detail),
      shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Ellipsoids ------------------------------------------------------------------

as_matrix_list_3d <- function(x, name, n) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) stop(name, " must be a list-column of 3 x 3 matrices.", call. = FALSE)
  lapply(x, function(m) {
    if (is.null(m) || (length(m) == 1L && is.na(m))) return(matrix(NA_real_, 3L, 3L))
    if (!is.numeric(m) || length(m) != 9L) stop(name, " must be a list-column of 3 x 3 matrices.", call. = FALSE)
    matrix(as.double(m), 3L, 3L)
  })
}

# Data-unit maps taking the unit sphere onto each ellipsoid: R diag(rx, ry, rz),
# or for a shape matrix S the symmetric square root, so that the ellipsoid is
# {p : (p - centre)' S^-1 (p - centre) <= 1}.
ellipsoid_maps_3d <- function(data) {
  n <- nrow(data)
  sigma <- as_matrix_list_3d(data$sigma, "sigma", n)
  rotation <- as_matrix_list_3d(data$rotation, "rotation", n)
  maps <- array(NA_real_, c(3L, 3L, n))
  for (i in seq_len(n)) {
    if (!is.null(sigma) && all(is.finite(sigma[[i]]))) {
      s <- sigma[[i]]
      if (max(abs(s - t(s))) > 1e-8 * max(1, abs(s))) stop("sigma matrices must be symmetric.", call. = FALSE)
      e <- eigen((s + t(s)) / 2, symmetric = TRUE)
      if (any(e$values < -1e-12 * max(abs(e$values)))) stop("sigma matrices must be positive semidefinite.", call. = FALSE)
      maps[, , i] <- e$vectors %*% diag(sqrt(pmax(e$values, 0)), 3L)
      next
    }
    axes <- c(data$rx[i] %or3d% NA, data$ry[i] %or3d% NA, data$rz[i] %or3d% NA)
    if (any(!is.finite(axes))) next
    if (any(axes < 0)) stop("Ellipsoid semi-axes must be nonnegative.", call. = FALSE)
    r <- if (is.null(rotation)) diag(3) else rotation[[i]]
    if (any(!is.finite(r))) r <- diag(3)
    if (max(abs(crossprod(r) - diag(3))) > 1e-6) stop("rotation matrices must be orthonormal.", call. = FALSE)
    maps[, , i] <- r %*% diag(axes, 3L)
  }
  maps
}

GeomEllipsoid3D <- ggplot2::ggproto("GeomEllipsoid3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = solid_defaults_3d(),
  optional_aes = c("rx", "ry", "rz", "rotation", "sigma", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = draw_key_glyph_3d,
  setup_data = function(data, params) {
    data <- constant_aes_3d(data, params, c("rx", "ry", "rz"))
    if (is.null(data$sigma) && !all(c("rx", "ry", "rz") %in% names(data))) stop("geom_ellipsoid_3d() requires rx, ry and rz, or sigma.", call. = FALSE)
    maps <- ellipsoid_maps_3d(data)
    data <- glyph_limits_3d(data, maps, glyph_mesh_3d("sphere", 0L), apply(maps, 3L, function(m) all(is.finite(m))))
    data
  }
)

emit_ellipsoids_3d <- function(data, params, panel_params) {
  maps <- normalise_maps_3d(ellipsoid_maps_3d(data), panel_params)
  emit_glyph_instances_3d(data, maps, rep("data", nrow(data)), glyph_mesh_3d("sphere", params$detail), params, panel_params)
}

register_geom_3d(GeomEllipsoid3D, emit_ellipsoids_3d, parameters = list(detail = 2L),
  position_check = function(position, scales) check_position_3d(position, scales))

#' Ellipsoids
#'
#' Draws one solid ellipsoid per row, centred on x, y, z. Give either the
#' semi-axes `rx`, `ry` and `rz`, turned by an optional `rotation` (a
#' list-column of 3 x 3 rotation matrices whose columns are the directions of
#' the three semi-axes), or `sigma`, a list-column of symmetric positive
#' semidefinite 3 x 3 shape matrices: the ellipsoid is then the set of points p
#' with (p - centre)' sigma^-1 (p - centre) <= 1, as in rgl's `ellipse3d()` at
#' `t = 1`. For a covariance matrix and a confidence level, scale sigma by
#' `qchisq(level, 3)`. All lengths are in the scale-transformed units of the
#' position axes, and the ellipsoids expand the position scales.
#'
#' Each ellipsoid is an icosphere of `20 * 4^detail` triangles stretched onto
#' the principal axes; in curvilinear frames it is placed at its mapped centre
#' and keeps its shape.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param detail Subdivision level of the icosphere, from 0 to 6.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @seealso [geom_sphere_3d()], [geom_glyph_3d()].
#' @examples
#' d <- data.frame(x = c(0, 3), y = c(0, 1), z = c(0, 1), rx = c(1, 0.5),
#'                 ry = c(0.5, 0.5), rz = c(0.3, 1.2))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, rx = rx, ry = ry, rz = rz)) +
#'   geom_ellipsoid_3d(fill = "steelblue") + coord_3d(aspect = "data")
#'
#' groups <- split(iris[1:3], iris$Species)
#' e <- data.frame(t(sapply(groups, colMeans)), species = names(groups))
#' names(e)[1:3] <- c("x", "y", "z")
#' e$sigma <- lapply(groups, function(g) cov(g) * qchisq(0.95, 3))
#' ggplot2::ggplot(e, ggplot2::aes(x, y, z = z, sigma = sigma, fill = species)) +
#'   geom_ellipsoid_3d(alpha = 0.6) + coord_3d()
#' @export
geom_ellipsoid_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                              detail = 2L, shade = TRUE,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomEllipsoid3D, mapping, data, stat, position,
    c(list(detail = check_detail_3d(detail), shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
