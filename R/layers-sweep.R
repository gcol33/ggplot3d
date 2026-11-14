#' Tubes and ribbons along curves
#'
#' Sweeps a circle (tubes) or a line segment (ribbons) along each group's
#' path in the order of the rows, in transformed position units. The cross
#' section lies in the plane of a rotation-minimising frame, computed by the
#' double reflection method of Wang et al. (2008), so it does not spin about
#' the curve the way a Frenet frame does, and does not flip at inflections.
#' On a closed curve (`closed = TRUE`) the frame carried once around the curve
#' would miss its starting orientation by an angle; that angle is spread over
#' the curve in proportion to arc length, so the surface closes without a
#' seam.
#'
#' The `radius` (tubes) or `width` (ribbons) aesthetic varies the cross section
#' per point, and `twist` turns it about the curve by that many degrees.
#' Without either, the size is the `radius` or `width` parameter, by default
#' 2% (tubes) or 8% (ribbons) of the diagonal of the position ranges. Tube
#' ends are closed with hemispheres (`cap = "round"`) or disks (`"flat"`), or
#' left open (`"none"`). A ribbon starts with its surface normal as close to
#' `normal` as the curve allows.
#'
#' Numeric aesthetics such as a continuous fill are taken at every point and
#' averaged over each face, so they change gradually from ring to ring.
#' `spline` passes the points through [stat_spline_3d()] first, with `n`
#' samples. Normals are angle-weighted and split where faces meet at more than
#' 60 degrees. Computed variables: `arc_length` from the first point and
#' `z_mean`.
#'
#' With `radius.unit = "mm"` (`width.unit` for ribbons) the size is measured
#' on the page, like a line width: the cross section is swept when the plot is
#' drawn, in the box the camera sees, so a tube is round on screen whatever
#' the box proportions and has its radius in millimetres at the centre of the
#' view, shrinking with distance under a perspective camera, as millimetre
#' spheres of [geom_sphere_3d()] do. Sizes then default to 1 mm (tube radius)
#' and 4 mm (ribbon width), and `z_mean` is the height of the curve point.
#' Millimetre sizes need the Euclidean coordinates of [coord_3d()].
#' @inheritParams stat_function_3d
#' @param sides Number of points on the tube's circle.
#' @param radius,width Cross-section size where the aesthetic is not mapped.
#' @param radius.unit,width.unit Unit of the cross-section size: `"data"`
#'   (transformed position units) or `"mm"` (millimetres on the page).
#' @param cap End caps of a tube on an open curve: `"round"`, `"flat"` or `"none"`.
#' @param normal Direction the first cross section is oriented towards: the
#'   ribbon's surface normal, or the profile's first axis in stat_sweep_3d().
#' @param closed Whether the path is a closed loop.
#' @param spline NULL, or a [stat_spline_3d()] type to smooth the path first.
#' @param n Number of spline samples.
#' @return A ggplot2 layer.
#' @references Wang, Juettler, Zheng and Liu (2008). Computation of rotation
#'   minimizing frames. ACM Transactions on Graphics 27, 2. \doi{10.1145/1330511.1330513}
#' @export
#' @examples
#' t <- seq(0, 4 * pi, length.out = 120)
#' helix <- data.frame(x = cos(t), y = sin(t), z = t / 4, t = t)
#' ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, fill = t)) +
#'   geom_tube_3d(radius = 0.12) + coord_3d()
#' knot <- data.frame(u = seq(0, 2 * pi, length.out = 13)[-13])
#' knot <- transform(knot, x = sin(u) + 2 * sin(2 * u), y = cos(u) - 2 * cos(2 * u), z = -sin(3 * u))
#' ggplot2::ggplot(knot, ggplot2::aes(x, y, z = z)) +
#'   geom_tube_3d(radius = 0.3, closed = TRUE, spline = "catmull-rom", n = 200) + coord_3d()
#' ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, twist = t * 20)) +
#'   geom_ribbon_3d(width = 0.4) + coord_3d()
#' ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, fill = t)) +
#'   geom_tube_3d(radius = 1.5, radius.unit = "mm") + coord_3d(aspect = c(3, 1, 1))
geom_tube_3d <- function(mapping = NULL, data = NULL, position = "identity", ..., sides = 16L, radius = NULL,
                         radius.unit = "data", cap = c("round", "flat", "none"), closed = FALSE, spline = NULL, n = 100L,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSweep3D, mapping, data, StatSweep3D, position,
    stats_params_3d(list(sides = sides, radius = radius, radius.unit = check_unit_3d(radius.unit, "radius.unit"),
      cap = match.arg(cap), closed = closed, spline = spline, n = n, na.rm = na.rm, shade = TRUE), list(...)),
    show.legend, inherit.aes)
}

#' @rdname geom_tube_3d
#' @export
geom_ribbon_3d <- function(mapping = NULL, data = NULL, position = "identity", ..., width = NULL,
                           width.unit = "data", normal = c(0, 0, 1), closed = FALSE, spline = NULL, n = 100L,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSweep3D, mapping, data, StatSweep3D, position,
    stats_params_3d(list(profile = rbind(c(0, -0.5), c(0, 0.5)), width = width,
      radius.unit = check_unit_3d(width.unit, "width.unit"), normal = normal, closed = closed,
      spline = spline, n = n, cap = "none", na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' Sweep a profile along a curve
#'
#' Sweeps a two-dimensional `profile` along each group's path as
#' [geom_tube_3d()] does with a circle: profile point (a, b) goes to
#' `p + radius * (a * r + b * s)`, where r and s are the rotation-minimising
#' normal and binormal at path point p, after turning by the `twist`
#' aesthetic. A profile of three or more points is a closed polygon (a
#' repeated last point is dropped), wound counterclockwise so that normals
#' point outwards; two points, or `profile_closed = FALSE`, give an open
#' strip. `radius` (aesthetic or parameter, default 1) scales the profile,
#' in millimetres on the page with `radius.unit = "mm"`.
#' @inheritParams geom_tube_3d
#' @param geom Geom used to draw the surface.
#' @param profile Two-column matrix or data frame of profile points.
#' @param radius Scale factor for the profile, also available as an aesthetic.
#' @param profile_closed Whether the profile is a closed polygon; NULL treats
#'   profiles of three or more points as closed.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' star <- t(sapply(0:9, function(k) (1 + (k %% 2)) / 2 * c(cos(k * pi / 5), sin(k * pi / 5))))
#' path <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 0, 1), z = c(0, 0.5, 1, 1.5))
#' ggplot2::ggplot(path, ggplot2::aes(x, y, z = z)) +
#'   stat_sweep_3d(profile = star * 0.3, spline = "natural") + coord_3d()
stat_sweep_3d <- function(mapping = NULL, data = NULL, geom = GeomSweep3D, position = "identity", ...,
                          profile, radius = 1, radius.unit = "data", cap = c("round", "flat", "none"), normal = NULL,
                          closed = FALSE, profile_closed = NULL, spline = NULL, n = 100L,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSweep3D, position,
    stats_params_3d(list(profile = profile, radius = radius, radius.unit = check_unit_3d(radius.unit, "radius.unit"),
      cap = match.arg(cap), normal = normal, closed = closed, profile_closed = profile_closed, spline = spline, n = n,
      na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_sweep_3d
#' @export
geom_sweep_3d <- function(...) stat_sweep_3d(...)

# Swept surfaces: the mesh of StatSweep3D, or, with radius.unit = "mm", its
# centre line.
GeomSweep3D <- ggplot2::ggproto("GeomSweep3D", GeomStatsSurface3D,
  required_aes = c("x", "y", "z"),
  optional_aes = c(GeomStatsSurface3D$optional_aes, ".face", ".corner", ".sweep_size", ".sweep_twist")
)

# A millimetre cross section is swept at draw time, when the fitted camera
# and the panel size are known: in the aspect-scaled model space the camera
# sees, so it is round on screen, with millimetres converted to model units
# at the centre of the view as for millimetre glyphs. Before then the layer
# emits an empty packet, which emit_scene_3d() keeps with its recipe for
# resolve_draw_packets_3d().
emit_sweep_3d <- function(data, params, panel_params) {
  if (!identical(params$radius.unit, "mm")) return(emit_stats_surface_3d(data, params, panel_params))
  if (!is.null(panel_params$space_context)) stop("radius.unit = \"mm\" needs the Euclidean coordinates of coord_3d().", call. = FALSE)
  packet <- if (is.null(panel_params$viewport)) new_scene_packet(panel_id = panel_params$panel_id) else
    emit_stats_surface_3d(sweep_model_mesh_3d(data, params, panel_params), params, panel_params)
  attr(packet, "draw_time") <- TRUE
  attr(packet, "model_space") <- TRUE
  packet
}

# Mesh in model space of the centre lines in data (positions normalised to
# the panel box), with normals in the same space.
sweep_model_mesh_3d <- function(data, params, panel_params) {
  frame <- frame_of_3d(panel_params)
  scale <- panel_params$camera$aspect / max(panel_params$camera$aspect)
  k <- model_units_per_mm_3d(panel_params$camera, panel_params$viewport)
  shape <- sweep_profile_3d(params$profile, params$sides, params$profile_closed)
  axes <- c("x", "y", "z"); normals <- c("nx", "ny", "nz")
  meshes <- lapply(split(seq_len(nrow(data)), data$group), function(rows) {
    curve <- data[rows, , drop = FALSE]
    curve[axes] <- as.data.frame(sweep(frame$map(as.matrix(curve[axes])), 2L, scale, "*"))
    mesh <- sweep_surface_3d(curve, curve$.sweep_size * k, curve$.sweep_twist, shape, params$closed, params$cap, params$normal)
    for (j in seq_len(if (nrow(mesh)) 3L else 0L)) {
      mesh[[axes[j]]] <- mesh[[axes[j]]] / scale[j]
      mesh[[normals[j]]] <- mesh[[normals[j]]] * scale[j]
    }
    mesh
  })
  do.call(rbind, meshes)
}

register_geom_3d(GeomSweep3D, emit_sweep_3d, parameters = c(surface_parameters_3d,
  list(radius.unit = "data", profile = NULL, sides = 16L, cap = "round", closed = FALSE, normal = NULL,
    profile_closed = NULL)))

#' Surfaces of revolution
#'
#' Turns each group's profile, given by the `r` aesthetic (distance from the
#' axis) and the aesthetic of the axis (`z` for `axis = "z"`), about a line
#' parallel to that axis through `centre`, in transformed position units. The
#' profile rows are joined in order; `closed = TRUE` joins the last to the
#' first (a torus from a circle). `n` angles cover `angle` degrees; a full
#' turn wraps without a seam, and profile points at zero `r` become single
#' poles. Normals point outwards where the profile runs towards increasing
#' axis values. Numeric aesthetics are averaged over each face.
#' @inheritParams stat_function_3d
#' @param axis Axis of revolution: `"z"` (r in the x-y plane), `"x"` or `"y"`.
#' @param n Number of angles.
#' @param closed Whether the profile is a closed loop.
#' @param angle Range of the revolution in degrees.
#' @param centre Position of the axis in the other two coordinates, in the
#'   cyclic order (x, y), (y, z) or (z, x).
#' @return A ggplot2 layer.
#' @export
#' @examples
#' vase <- data.frame(h = seq(0, 2, length.out = 30))
#' vase$r <- 0.6 + 0.3 * sin(2.5 * vase$h) - 0.15 * vase$h
#' ggplot2::ggplot(vase, ggplot2::aes(r = r, z = h, fill = h)) +
#'   geom_revolve_3d(n = 36) + coord_3d()
stat_revolve_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D, position = "identity", ...,
                            axis = c("z", "x", "y"), n = 48L, closed = FALSE, angle = c(0, 360), centre = c(0, 0),
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatRevolve3D, position,
    stats_params_3d(list(axis = match.arg(axis), n = n, closed = closed, angle = angle, centre = centre,
      na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_revolve_3d
#' @export
geom_revolve_3d <- function(...) stat_revolve_3d(...)

#' Smooth curves through points
#'
#' Replaces each group's points, in row order, by `n` samples of a smooth
#' curve. `"catmull-rom"` interpolates the points with the centripetal
#' Catmull-Rom spline (knot spacing the square root of the chord length),
#' which has no cusps or self-intersections within a segment; `"natural"`
#' interpolates with a natural (or, when closed, periodic) cubic spline in
#' chord length; `"bspline"` approximates the points with a uniform cubic
#' B-spline that starts and ends on the end points. Other numeric aesthetics
#' are interpolated linearly between the points; other aesthetics keep the
#' value of the preceding point. A closed curve repeats its first sample at
#' the end so that a path closes.
#' @inheritParams stat_function_3d
#' @param geom Geom used to draw the curve.
#' @param n Number of samples.
#' @param type `"catmull-rom"`, `"bspline"` or `"natural"`.
#' @param closed Whether the curve is a closed loop.
#' @return A ggplot2 layer.
#' @references Yuksel, Schaefer and Keyser (2011). Parameterization and
#'   applications of Catmull-Rom curves. Computer-Aided Design 43, 747-755.
#'   \doi{10.1016/j.cad.2010.08.008}
#' @export
#' @examples
#' d <- data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 1, 0, 1, 0), z = c(0, 1, 0, -1, 0))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d() + stat_spline_3d(type = "natural") + coord_3d()
stat_spline_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomPath, position = "identity", ...,
                           n = 100L, type = c("catmull-rom", "bspline", "natural"), closed = FALSE,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSpline3D, position,
    stats_params_3d(list(n = n, type = match.arg(type), closed = closed, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
