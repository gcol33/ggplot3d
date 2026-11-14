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
#' Opaque filled tubes without an edge colour are analytic quadrics: a
#' cylinder, or a cone where the radius changes, along each segment of the
#' path and a sphere at each joint, so the tube is the sweep of a ball along
#' the path, drawn with exact outlines and per-sample depth (Sigg et al. 2006).
#' A segment takes the colour of its first point. Translucent tubes, and tubes
#' with edges, are swept meshes with `sides` points on each cross section.
#'
#' The `radius` (tubes) or `width` (ribbons) aesthetic varies the cross section
#' per point, and `twist` turns it about the curve by that many degrees.
#' Without either, the size is the `radius` or `width` parameter, by default
#' 2% (tubes) or 8% (ribbons) of the diagonal of the position ranges. Tube
#' ends are closed with hemispheres (`cap = "round"`) or disks (`"flat"`), or
#' left open (`"none"`). A ribbon starts with its surface normal as close to
#' `normal` as the curve allows.
#'
#' Numeric aesthetics such as a continuous fill are taken at every point; on
#' meshes they are averaged over each face, so they change gradually from
#' ring to ring.
#' `spline` passes the points through [stat_spline_3d()] first, with `n`
#' samples. Normals are angle-weighted and split where faces meet at more than
#' 60 degrees. Computed variables: `arc_length` from the first point and
#' `z_mean`, the height of the curve point for tubes and the mean height of a
#' face for other sweeps.
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
#' @param radius.unit Unit of the cross-section size: `"data"`
#'   (transformed position units) or `"mm"` (millimetres on the page).
#' @param width.unit Unit of ribbon width, with the same choices as `radius.unit`.
#' @param cap End caps of a tube on an open curve: `"round"`, `"flat"` or `"none"`.
#' @param normal Direction the first cross section is oriented towards: the
#'   ribbon's surface normal, or the profile's first axis in stat_sweep_3d().
#' @param closed Whether the path is a closed loop.
#' @param spline NULL, or a [stat_spline_3d()] type to smooth the path first.
#' @param n Number of spline samples.
#' @return A ggplot2 layer.
#' @references Wang, Juettler, Zheng and Liu (2008). Computation of rotation
#'   minimizing frames. ACM Transactions on Graphics 27, 2. \doi{10.1145/1330511.1330513}
#'
#'   Sigg, Weyrich, Botsch and Gross (2006). GPU-based ray-casting of
#'   quadratic surfaces. Symposium on Point-Based Graphics.
#'   \doi{10.2312/SPBG/SPBG06/059-065}
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

# Tubes (a circular cross section, the centre line from StatSweep3D) are
# analytic quadrics where they are opaque, filled and without edge colour: a
# cylinder or cone per segment and a sphere at each joint, so the tube is the
# sweep of a ball along the path (Sigg et al. 2006). Other tubes and
# millimetre sweeps are meshes swept from the centre line.
emit_sweep_3d <- function(data, params, panel_params) {
  if (is.null(data$.sweep_size)) return(emit_stats_surface_3d(data, params, panel_params))
  unit <- check_unit_3d(params$radius.unit, "radius.unit")
  if (unit == "mm" && !is.null(panel_params$space_context)) stop("radius.unit = \"mm\" needs the Euclidean coordinates of coord_3d().", call. = FALSE)
  if (sweep_quadrics_3d(data, params, panel_params)) return(emit_tube_quadrics_3d(data, params, panel_params, unit))
  if (unit == "data") return(emit_stats_surface_3d(sweep_space_mesh_3d(data, params, panel_params, "data"), params, panel_params))
  # A millimetre cross section is swept at draw time, when the fitted camera
  # and the panel size are known: in the aspect-scaled model space the camera
  # sees, so it is round on screen, with millimetres converted to model units
  # at the centre of the view as for millimetre glyphs. Before then the layer
  # emits an empty packet, which emit_scene_3d() keeps with its recipe for
  # resolve_draw_packets_3d().
  packet <- if (is.null(panel_params$viewport)) new_scene_packet(panel_id = panel_params$panel_id) else
    emit_stats_surface_3d(sweep_space_mesh_3d(data, params, panel_params, "mm"), params, panel_params)
  attr(packet, "draw_time") <- TRUE
  attr(packet, "model_space") <- TRUE
  packet
}

# Whether a centre line is drawn as quadrics: a circular cross section,
# opaque filled rows without edge colour, in the Euclidean space of coord_3d().
sweep_quadrics_3d <- function(data, params, panel_params) {
  if (!is.null(params$profile) || isTRUE(params$wireframe) || !is.null(panel_params$space_context) || !nrow(data)) return(FALSE)
  fill <- data$fill
  if (is.null(fill) || anyNA(fill) || !all(is.na(data$colour %or3d% NA))) return(FALSE)
  alpha <- (data$alpha %or3d% 1) * colour_channels_3d(fill, alpha = TRUE)[4L, ] / 255
  all(!is.na(alpha) & alpha >= 1)
}

# Mesh of the centre lines in data (positions normalised to the panel box),
# swept in data space (unit "data": sizes in data units, the mesh mapped by
# the frame afterwards) or in the aspect-scaled model space after the frame
# (unit "mm": sizes in millimetres), with normals in the same space.
sweep_space_mesh_3d <- function(data, params, panel_params, unit) {
  frame <- frame_of_3d(panel_params)
  millimetres <- unit == "mm"
  scale <- if (millimetres) model_scale_3d(panel_params) else vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
  k <- if (millimetres) model_units_per_mm_3d(panel_params$camera, panel_params$viewport) else 1
  shape <- sweep_profile_3d(params$profile, params$sides, params$profile_closed)
  axes <- c("x", "y", "z"); normals <- c("nx", "ny", "nz")
  meshes <- lapply(split(seq_len(nrow(data)), data$group), function(rows) {
    curve <- data[rows, , drop = FALSE]
    xyz <- as.matrix(curve[axes])
    curve[axes] <- as.data.frame(sweep(if (millimetres) frame$map(xyz) else xyz, 2L, scale, "*"))
    mesh <- sweep_surface_3d(curve, curve$.sweep_size * k, curve$.sweep_twist, shape, params$closed, params$cap, params$normal)
    for (j in seq_len(if (nrow(mesh)) 3L else 0L)) {
      mesh[[axes[j]]] <- mesh[[axes[j]]] / scale[j]
      mesh[[normals[j]]] <- mesh[[normals[j]]] * scale[j]
    }
    mesh
  })
  do.call(rbind, meshes)
}

# Largest bend, in degrees, that a tube joint closes by extending its two
# segments (a mitre) instead of with a sphere: the extended segments then
# depart from the round joint by r (1 / cos(bend / 2) - 1), under 0.9% of the
# radius.
tube_mitre_degrees_3d <- 15

# Quadrics of the tubes along the centre lines in data: per segment a
# cylinder (equal end radii) or cone, closed by disks. A joint that bends by
# at most tube_mitre_degrees_3d extends both segments by r tan(bend / 2), so
# they overlap past the outer side; a sharper joint, and a round end, takes a
# sphere, so the tube is the sweep of a ball along the path. The cross
# section is round in data space (unit "data") or in the model space the
# camera sees (unit "mm"); a segment takes the style of its first point.
emit_tube_quadrics_3d <- function(data, params, panel_params, unit) {
  frame <- frame_of_3d(panel_params)
  span <- vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
  scale <- model_scale_3d(panel_params)
  millimetres <- unit == "mm"
  cap <- params$cap %or3d% "round"
  closed <- isTRUE(params$closed)
  parts <- list()
  add <- function(...) parts[[length(parts) + 1L]] <<- list(...)
  for (group in split(seq_len(nrow(data)), data$group)) {
    n <- length(group)
    xyz <- as.matrix(data[group, c("x", "y", "z")])
    mapped <- frame$map(xyz)
    size <- data$.sweep_size[group]
    ends <- if (n < 2L) matrix(integer(), 0L, 2L) else if (closed && n > 2L) cbind(seq_len(n), c(seq.int(2L, n), 1L)) else
      cbind(seq_len(n - 1L), seq.int(2L, n))
    round_space <- if (millimetres) sweep(mapped, 2L, scale, "*") else sweep(xyz, 2L, span, "*")
    direction <- round_space[ends[, 2L], , drop = FALSE] - round_space[ends[, 1L], , drop = FALSE]
    length <- sqrt(rowSums(direction^2))
    ends <- ends[length > 0, , drop = FALSE]
    direction <- direction[length > 0, , drop = FALSE] / length[length > 0]
    length <- length[length > 0]
    # Bend and extension at each point between its incoming and outgoing segments.
    extension <- numeric(n); sphere <- rep(FALSE, n)
    incoming <- match(seq_len(n), ends[, 2L]); outgoing <- match(seq_len(n), ends[, 1L])
    joint <- !is.na(incoming) & !is.na(outgoing)
    bend <- rep(NA_real_, n)
    bend[joint] <- acos(pmin(1, pmax(-1, rowSums(direction[incoming[joint], , drop = FALSE] * direction[outgoing[joint], , drop = FALSE]))))
    mitre <- joint & bend <= tube_mitre_degrees_3d * pi / 180
    extension[mitre] <- size[mitre] * tan(bend[mitre] / 2)
    sphere[joint & !mitre] <- TRUE
    if (!closed && cap == "round" && n >= 1L) sphere[c(1L, n)] <- TRUE
    for (s in seq_len(nrow(ends))) {
      a <- ends[s, 1L]; b <- ends[s, 2L]
      if (size[a] == 0 && size[b] == 0) next
      flip <- size[a] == 0
      first <- if (flip) b else a; last <- if (flip) a else b
      along <- if (flip) -direction[s, ] else direction[s, ]
      basis <- align_z_3d(matrix(along, 1L))[, , 1L]
      # An end disk is left open only at an end of an open path with cap = "none".
      open <- !closed & cap == "none" & c(first, last) %in% c(1L, n)
      axis <- mapped[last, ] - mapped[first, ]
      start <- extension[first]; end <- extension[last]
      map <- cbind(size[first] * basis[, 1L], size[first] * basis[, 2L], 0)
      shift <- c(0, 0, 0)
      if (millimetres) {
        map[, 3L] <- (start + end) * along
        shift <- -start * along
        origin <- mapped[first, ]
      } else {
        origin <- mapped[first, ] - start / length[s] * axis
        axis <- axis * (1 + (start + end) / length[s])
      }
      add(row = group[a], origin = xyz[first, ], centre = origin, kind = if (size[a] == size[b]) 2L else 3L,
        ratio = size[last] / size[first], cap = as.integer(!open), map = map, fixed = cbind(0, 0, axis), shift = shift)
    }
    for (j in which(sphere & size > 0)) add(row = group[j], origin = xyz[j, ], centre = mapped[j, ], kind = 1L, ratio = 1,
      cap = c(1L, 1L), map = diag(size[j], 3L), fixed = matrix(0, 3L, 3L), shift = c(0, 0, 0))
  }
  if (!length(parts)) return(new_scene_packet(panel_id = panel_params$panel_id))
  field <- function(name) lapply(parts, `[[`, name)
  caps <- do.call(rbind, field("cap"))
  emit_quadric_instances_3d(data[unlist(field("row")), , drop = FALSE], do.call(rbind, field("origin")),
    simplify2array(field("map")), rep(unit, length(parts)), unlist(field("kind")), params, panel_params,
    fixed = simplify2array(field("fixed")), radius_ratio = unlist(field("ratio")), cap_0 = caps[, 1L], cap_1 = caps[, 2L],
    centre = do.call(rbind, field("centre")), shift = do.call(rbind, field("shift")))
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
