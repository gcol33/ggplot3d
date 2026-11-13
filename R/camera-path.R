#' Camera paths through keyframes
#'
#' Describe a camera move as a timed sequence of views. [animate_3d()] renders
#' the path and [view_3d()] plays it in the browser. Paths join with `c()`:
#' each part starts from the camera the previous part ends on, and the first
#' part from the plot's [coord_3d()] camera.
#'
#' `camera_path_3d()` interpolates keyframes. The orientation, that is
#' azimuth, elevation and roll together, is converted to a unit quaternion
#' and interpolated on the sphere of rotations (Shoemake 1985): `"slerp"`
#' turns at constant angular speed between neighbouring keys, `"squad"` adds
#' Shoemake's spherical cubic so the angular velocity is continuous at the
#' keys. Paths therefore pass through `elevation = 90` without the spin that
#' interpolating the angles separately produces there. The target moves on a
#' centripetal Catmull-Rom spline (Yuksel, Schaefer and Keyser 2011), which
#' passes through every key without cusps or self-intersections. Field of
#' view, zoom and an explicit perspective distance are interpolated in log
#' space, where equal steps are equal ratios, with a monotone cubic
#' (Fritsch and Carlson 1980) that does not overshoot the keys.
#'
#' `orbit_3d()` turns the camera about the vertical axis through the target.
#' `fly_through_3d()` moves the eye along a spline through points in data
#' coordinates, looking at a fixed or moving point; while the eye is inside
#' the plot box the axes are left out, since the box outline they follow
#' surrounds the camera. `dolly_zoom_3d()` changes
#' the field of view while the camera moves along the line of sight so that
#' the plane through the target keeps its size on the image; a field of view
#' of 0 is the orthographic projection, so `fov = 0` morphs a perspective
#' view into a parallel one at constant image scale.
#'
#' @param keys A list of camera views, each a [camera_3d()] result or a named
#'   list with any of `projection`, `azimuth`, `elevation`, `roll`,
#'   `distance`, `fov`, `zoom`, `target` and `oblique`. A field a key omits
#'   keeps the value of the previous key, and the first key's omitted fields
#'   come from the camera the path starts from. A [camera_3d()] result always
#'   names its projection, so pass `projection = "perspective"` to it for a
#'   perspective path.
#' @param at Key times in seconds, increasing, starting at 0. The default puts
#'   the keys one second apart.
#' @param ease Easing of time between consecutive keys: a name such as
#'   `"linear"`, `"cubic-in-out"` or `"sine-in"`, a function mapping `[0, 1]`
#'   onto itself, or a list or character vector with one entry per segment.
#'   Named easings are `"linear"` and `"quadratic"`, `"cubic"`, `"quartic"`,
#'   `"quintic"`, `"sine"`, `"circular"`, `"exponential"`, `"back"`,
#'   `"elastic"` and `"bounce"`, each followed by `-in`, `-out` or `-in-out`
#'   (Penner 2002, as in tweenr and CSS).
#' @param orientation `"slerp"` (default) or `"squad"`.
#' @param position Interpolation of the target, the eye of a fly-through and
#'   the log-scaled tracks: `"catmull-rom"` (default) or `"linear"`.
#' @return A `ggplot3d_camera_path` object.
#' @references
#' Shoemake, K. (1985). Animating rotation with quaternion curves.
#' SIGGRAPH '85, 245-254. \doi{10.1145/325165.325242}
#'
#' Yuksel, C., Schaefer, S. and Keyser, J. (2011). Parameterization and
#' applications of Catmull-Rom curves. Computer-Aided Design 43, 747-755.
#' \doi{10.1016/j.cad.2010.08.008}
#'
#' Fritsch, F. N. and Carlson, R. E. (1980). Monotone piecewise cubic
#' interpolation. SIAM Journal on Numerical Analysis 17, 238-246.
#' \doi{10.1137/0717021}
#' @seealso [animate_3d()], [view_3d()], [camera_3d()]
#' @examples
#' over_the_top <- camera_path_3d(
#'   list(camera_3d(azimuth = 0, elevation = 30),
#'        camera_3d(azimuth = 0, elevation = 90),
#'        camera_3d(azimuth = 180, elevation = 30, roll = 180)),
#'   at = c(0, 1.5, 3), ease = "sine-in-out")
#' over_the_top
#' tour <- c(orbit_3d(degrees = 90, duration = 2),
#'           dolly_zoom_3d(fov = 0, duration = 2))
#' tour
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'   geom_point_3d() + coord_3d(projection = "perspective")
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   movie <- animate_3d(p, path = tour, fps = 4, width = 240, height = 240,
#'                       file = tempfile(fileext = ".apng"))
#'   movie$camera[, c("time", "azimuth", "fov", "projection")]
#' }
#' @export
camera_path_3d <- function(keys, at = NULL, ease = "linear",
                           orientation = c("slerp", "squad"),
                           position = c("catmull-rom", "linear")) {
  orientation <- match.arg(orientation)
  position <- match.arg(position)
  scene_assert(is.list(keys) && length(keys) >= 2L && all(vapply(keys, is.list, logical(1))),
    "keys must be a list of at least two camera views")
  fields <- c("projection", "azimuth", "elevation", "roll", "distance", "fov", "zoom",
    "target", "oblique", "up")
  for (key in keys) {
    scene_assert(length(key) == 0L || (!is.null(names(key)) && all(names(key) %in% fields)),
      paste("camera keys take", paste(fields[-length(fields)], collapse = ", ")))
  }
  at <- path_times_3d(at, length(keys))
  camera_path_piece_3d(list(type = "keys", keys = keys, at = at,
    ease = path_eases_3d(ease, length(keys) - 1L), orientation = orientation,
    position = position))
}

#' @rdname camera_path_3d
#' @param degrees Turn about the vertical axis in degrees; positive values
#'   increase the azimuth. Default one full turn.
#' @param duration Length of the move in seconds.
#' @param elevation Elevation at the end of the orbit, reached gradually, or
#'   NULL (default) to keep the starting elevation.
#' @export
orbit_3d <- function(degrees = 360, duration = 4, elevation = NULL, ease = "linear") {
  scene_assert(is.numeric(degrees) && length(degrees) == 1L && is.finite(degrees),
    "degrees must be one finite number")
  scene_assert(is.null(elevation) || (is.numeric(elevation) && length(elevation) == 1L &&
    is.finite(elevation) && abs(elevation) <= 90), "elevation must be NULL or one angle in [-90, 90]")
  camera_path_piece_3d(list(type = "orbit", degrees = as.double(degrees),
    elevation = elevation, at = path_times_3d(c(0, path_duration_3d(duration)), 2L),
    ease = path_eases_3d(ease, 1L)))
}

#' @rdname camera_path_3d
#' @param path Eye positions in data coordinates: a data frame or matrix with
#'   columns `x`, `y` and `z` (or three unnamed columns), one row per key.
#' @param look_at The point the camera looks at, in data coordinates: one row
#'   for a fixed point, or one row per eye position. The default looks at the
#'   centre of the plot box.
#' @param fov Field of view in degrees. For `fly_through_3d()` NULL keeps the
#'   starting value. For `dolly_zoom_3d()` the final value, or the start and
#'   end values; 0 stands for the orthographic projection, and a single value
#'   starts from the current camera's field of view (0 when it is parallel).
#' @param roll Camera roll in degrees, one value or one per eye position.
#' @export
fly_through_3d <- function(path, look_at = NULL, at = NULL, ease = "linear",
                           fov = NULL, roll = 0, position = c("catmull-rom", "linear")) {
  position <- match.arg(position)
  eye <- path_points_3d(path, "path")
  scene_assert(nrow(eye) >= 2L, "path needs at least two eye positions")
  look <- if (is.null(look_at)) NULL else path_points_3d(look_at, "look_at")
  scene_assert(is.null(look) || nrow(look) %in% c(1L, nrow(eye)),
    "look_at must have one row or one row per eye position")
  scene_assert(is.numeric(roll) && length(roll) %in% c(1L, nrow(eye)) && all(is.finite(roll)),
    "roll must be one finite value or one per eye position")
  scene_assert(is.null(fov) || (is.numeric(fov) && length(fov) == 1L && is.finite(fov) &&
    fov > 0 && fov < 180), "fov must be NULL or one angle between 0 and 180")
  camera_path_piece_3d(list(type = "fly", eye = eye, look = look,
    roll = rep_len(as.double(roll), nrow(eye)), fov = fov,
    at = path_times_3d(at, nrow(eye)), ease = path_eases_3d(ease, nrow(eye) - 1L),
    position = position))
}

#' @rdname camera_path_3d
#' @export
dolly_zoom_3d <- function(fov = 10, duration = 3, ease = "cubic-in-out") {
  scene_assert(is.numeric(fov) && length(fov) %in% 1:2 && all(is.finite(fov)) &&
    all(fov >= 0 & fov < 180) && (length(fov) == 1L || any(fov > 0)),
    "fov must be one or two angles in [0, 180), not both 0")
  camera_path_piece_3d(list(type = "dolly", fov = as.double(fov),
    at = path_times_3d(c(0, path_duration_3d(duration)), 2L), ease = path_eases_3d(ease, 1L)))
}

camera_path_piece_3d <- function(piece) {
  structure(list(pieces = list(piece), duration = piece$at[length(piece$at)]),
    class = "ggplot3d_camera_path")
}

#' @export
c.ggplot3d_camera_path <- function(...) {
  paths <- list(...)
  scene_assert(all(vapply(paths, inherits, logical(1), "ggplot3d_camera_path")),
    "only camera paths combine with c()")
  pieces <- unlist(lapply(paths, `[[`, "pieces"), recursive = FALSE)
  structure(list(pieces = pieces, duration = sum(vapply(paths, `[[`, numeric(1), "duration"))),
    class = "ggplot3d_camera_path")
}

#' @export
print.ggplot3d_camera_path <- function(x, ...) {
  labels <- vapply(x$pieces, function(piece) {
    span <- piece$at[length(piece$at)]
    switch(piece$type,
      keys = sprintf("%d keys, %s orientation, %s position", length(piece$keys),
        piece$orientation, piece$position),
      orbit = sprintf("orbit of %g degrees", piece$degrees),
      fly = sprintf("fly-through of %d eye positions", nrow(piece$eye)),
      dolly = if (length(piece$fov) == 2L) sprintf("dolly zoom from fov %g to %g", piece$fov[1L], piece$fov[2L]) else
        sprintf("dolly zoom to fov %g", piece$fov)) |>
      paste0(sprintf(" over %g s", span))
  }, character(1))
  cat(sprintf("<ggplot3d camera path: %g s>\n", x$duration))
  cat(paste0("  ", seq_along(labels), ". ", labels, "\n"), sep = "")
  invisible(x)
}

path_duration_3d <- function(duration) {
  scene_assert(is.numeric(duration) && length(duration) == 1L && is.finite(duration) &&
    duration > 0, "duration must be a positive number of seconds")
  as.double(duration)
}

path_times_3d <- function(at, n) {
  if (is.null(at)) return(as.double(seq_len(n) - 1L))
  scene_assert(is.numeric(at) && length(at) == n && all(is.finite(at)) && at[1L] == 0 &&
    all(diff(at) > 0), "at must hold increasing key times starting at 0, one per key")
  as.double(at)
}

path_points_3d <- function(points, what) {
  if (is.data.frame(points) && all(c("x", "y", "z") %in% names(points))) points <- points[c("x", "y", "z")]
  points <- if (is.null(dim(points)) && !is.list(points)) matrix(points, 1L) else as.matrix(points)
  scene_assert(is.numeric(points) && ncol(points) == 3L && nrow(points) >= 1L && all(is.finite(points)),
    paste(what, "must hold finite x, y and z coordinates"))
  unname(points)
}

# Easing curves after Penner (2002): each family is its "in" curve, and the
# "out" and "in-out" variants are its reflections.
ease_families_3d <- list(
  quadratic = function(t) t^2,
  cubic = function(t) t^3,
  quartic = function(t) t^4,
  quintic = function(t) t^5,
  sine = function(t) 1 - cos(t * pi / 2),
  circular = function(t) 1 - sqrt(pmax(0, 1 - t^2)),
  exponential = function(t) ifelse(t <= 0, 0, 2^(10 * (t - 1))),
  back = function(t) t^2 * (2.70158 * t - 1.70158),
  elastic = function(t) ifelse(t <= 0 | t >= 1, t, -2^(10 * (t - 1)) * sin((t - 1.075) * 2 * pi / 0.3)),
  bounce = function(t) 1 - ease_bounce_out_3d(1 - t)
)

ease_bounce_out_3d <- function(t) {
  ifelse(t < 1 / 2.75, 7.5625 * t^2,
    ifelse(t < 2 / 2.75, 7.5625 * (t - 1.5 / 2.75)^2 + 0.75,
      ifelse(t < 2.5 / 2.75, 7.5625 * (t - 2.25 / 2.75)^2 + 0.9375,
        7.5625 * (t - 2.625 / 2.75)^2 + 0.984375)))
}

ease_function_3d <- function(name) {
  if (is.function(name)) return(name)
  scene_assert(is.character(name) && length(name) == 1L && !is.na(name), "ease must be a name or a function")
  if (name == "linear") return(function(t) t)
  parts <- regmatches(name, regexec("^([a-z]+)-(in|out|in-out)$", name))[[1L]]
  known <- c("linear", paste0(rep(names(ease_families_3d), each = 3L), c("-in", "-out", "-in-out")))
  scene_assert(length(parts) == 3L && parts[2L] %in% names(ease_families_3d),
    paste0("unknown ease '", name, "'; use one of ", paste(known, collapse = ", ")))
  f <- ease_families_3d[[parts[2L]]]
  switch(parts[3L],
    `in` = f,
    out = function(t) 1 - f(1 - t),
    `in-out` = function(t) ifelse(t < 0.5, f(2 * t) / 2, 1 - f(2 - 2 * t) / 2))
}

path_eases_3d <- function(ease, segments) {
  eases <- if (is.function(ease)) list(ease) else as.list(ease)
  scene_assert(length(eases) %in% c(1L, segments), "ease must have one entry or one per segment")
  rep_len(lapply(eases, ease_function_3d), segments)
}

# Segment index and eased local parameter of each time on the key grid.
path_segments_3d <- function(times, at, eases) {
  n <- length(at)
  segment <- pmin(pmax(findInterval(times, at), 1L), n - 1L)
  u <- (pmin(pmax(times, at[1L]), at[n]) - at[segment]) / (at[segment + 1L] - at[segment])
  eased <- vapply(seq_along(times), function(i) as.double(eases[[segment[i]]](u[i])), numeric(1))
  scene_assert(all(is.finite(eased)), "ease must return finite values")
  list(segment = segment, u = eased)
}

# Quaternions are c(w, x, y, z) for rotations of the camera frame, whose
# columns are the right, up and outward directions in the z-up frame.
camera_frame_3d <- function(azimuth, elevation, roll) {
  a <- azimuth * pi / 180; e <- elevation * pi / 180; r <- roll * pi / 180
  right0 <- c(-sin(a), cos(a), 0)
  up0 <- c(-sin(e) * cos(a), -sin(e) * sin(a), cos(e))
  outward <- c(cos(e) * cos(a), cos(e) * sin(a), sin(e))
  cbind(cos(r) * right0 - sin(r) * up0, sin(r) * right0 + cos(r) * up0, outward)
}

quaternion_from_matrix_3d <- function(m) {
  trace <- m[1L, 1L] + m[2L, 2L] + m[3L, 3L]
  q <- if (trace > 0) {
    s <- 2 * sqrt(trace + 1)
    c(s / 4, (m[3L, 2L] - m[2L, 3L]) / s, (m[1L, 3L] - m[3L, 1L]) / s, (m[2L, 1L] - m[1L, 2L]) / s)
  } else if (m[1L, 1L] > m[2L, 2L] && m[1L, 1L] > m[3L, 3L]) {
    s <- 2 * sqrt(1 + m[1L, 1L] - m[2L, 2L] - m[3L, 3L])
    c((m[3L, 2L] - m[2L, 3L]) / s, s / 4, (m[1L, 2L] + m[2L, 1L]) / s, (m[1L, 3L] + m[3L, 1L]) / s)
  } else if (m[2L, 2L] > m[3L, 3L]) {
    s <- 2 * sqrt(1 + m[2L, 2L] - m[1L, 1L] - m[3L, 3L])
    c((m[1L, 3L] - m[3L, 1L]) / s, (m[1L, 2L] + m[2L, 1L]) / s, s / 4, (m[2L, 3L] + m[3L, 2L]) / s)
  } else {
    s <- 2 * sqrt(1 + m[3L, 3L] - m[1L, 1L] - m[2L, 2L])
    c((m[2L, 1L] - m[1L, 2L]) / s, (m[1L, 3L] + m[3L, 1L]) / s, (m[2L, 3L] + m[3L, 2L]) / s, s / 4)
  }
  q / sqrt(sum(q^2))
}

quaternion_matrix_3d <- function(q) {
  w <- q[1L]; x <- q[2L]; y <- q[3L]; z <- q[4L]
  matrix(c(1 - 2 * (y^2 + z^2), 2 * (x * y + w * z), 2 * (x * z - w * y),
           2 * (x * y - w * z), 1 - 2 * (x^2 + z^2), 2 * (y * z + w * x),
           2 * (x * z + w * y), 2 * (y * z - w * x), 1 - 2 * (x^2 + y^2)), 3L)
}

quaternion_multiply_3d <- function(a, b) {
  c(a[1L] * b[1L] - sum(a[-1L] * b[-1L]),
    a[1L] * b[-1L] + b[1L] * a[-1L] + c(a[3L] * b[4L] - a[4L] * b[3L],
      a[4L] * b[2L] - a[2L] * b[4L], a[2L] * b[3L] - a[3L] * b[2L]))
}

quaternion_conjugate_3d <- function(q) c(q[1L], -q[-1L])

quaternion_log_3d <- function(q) {
  v <- sqrt(sum(q[-1L]^2))
  if (v < 1e-12) return(c(0, 0, 0, 0))
  c(0, q[-1L] / v * atan2(v, q[1L]))
}

quaternion_exp_3d <- function(q) {
  angle <- sqrt(sum(q[-1L]^2))
  if (angle < 1e-12) return(c(1, 0, 0, 0))
  c(cos(angle), q[-1L] / angle * sin(angle))
}

slerp_3d <- function(a, b, t) {
  cosine <- sum(a * b)
  if (cosine < 0) { b <- -b; cosine <- -cosine }
  if (cosine > 1 - 1e-10) {
    q <- a + t * (b - a)
    return(q / sqrt(sum(q^2)))
  }
  angle <- acos(min(1, cosine))
  (sin((1 - t) * angle) * a + sin(t * angle) * b) / sin(angle)
}

# Shoemake's inner control points for squad: s_i = q_i exp(-(log(q_i^-1 q_i+1)
# + log(q_i^-1 q_i-1)) / 4), with the end keys as their own controls.
squad_controls_3d <- function(q) {
  n <- length(q)
  lapply(seq_len(n), function(i) {
    if (i == 1L || i == n) return(q[[i]])
    inverse <- quaternion_conjugate_3d(q[[i]])
    tangent <- quaternion_log_3d(quaternion_multiply_3d(inverse, q[[i + 1L]])) +
      quaternion_log_3d(quaternion_multiply_3d(inverse, q[[i - 1L]]))
    quaternion_multiply_3d(q[[i]], quaternion_exp_3d(-tangent / 4))
  })
}

# Camera angles of an orientation. Azimuth and roll are unwrapped next to the
# previous frame's, and at a pole, where only their difference is defined, the
# previous azimuth is kept.
quaternion_angles_3d <- function(q, previous) {
  m <- quaternion_matrix_3d(q)
  outward <- m[, 3L]
  elevation <- asin(max(-1, min(1, outward[3L]))) * 180 / pi
  horizontal <- sqrt(sum(outward[1:2]^2))
  azimuth <- if (horizontal > 1e-9) atan2(outward[2L], outward[1L]) * 180 / pi else previous[["azimuth"]]
  azimuth <- azimuth - 360 * round((azimuth - previous[["azimuth"]]) / 360)
  base <- camera_frame_3d(azimuth, elevation, 0)
  roll <- atan2(-sum(m[, 1L] * base[, 2L]), sum(m[, 1L] * base[, 1L])) * 180 / pi
  roll <- roll - 360 * round((roll - previous[["roll"]]) / 360)
  c(azimuth = azimuth, elevation = elevation, roll = roll)
}

# Centripetal Catmull-Rom (alpha = 1/2) through the rows of `points`, by the
# Barry-Goldman pyramid; phantom end points continue the end segments.
catmull_rom_3d <- function(points, segment, u) {
  n <- nrow(points)
  padded <- rbind(2 * points[1L, ] - points[min(2L, n), ], points, 2 * points[n, ] - points[max(1L, n - 1L), ])
  t(vapply(seq_along(segment), function(i) {
    p <- padded[segment[i] + 0:3, , drop = FALSE]
    if (sum((p[3L, ] - p[2L, ])^2) == 0) return(p[2L, ])
    knots <- c(0, cumsum(pmax(sqrt(sqrt(rowSums(diff(p)^2))), 1e-6)))
    t <- knots[2L] + u[i] * (knots[3L] - knots[2L])
    lerp <- function(a, b, ta, tb) ((tb - t) * a + (t - ta) * b) / (tb - ta)
    a1 <- lerp(p[1L, ], p[2L, ], knots[1L], knots[2L])
    a2 <- lerp(p[2L, ], p[3L, ], knots[2L], knots[3L])
    a3 <- lerp(p[3L, ], p[4L, ], knots[3L], knots[4L])
    lerp(lerp(a1, a2, knots[1L], knots[3L]), lerp(a2, a3, knots[2L], knots[4L]), knots[2L], knots[3L])
  }, numeric(ncol(points))))
}

path_points_at_3d <- function(points, segment, u, position) {
  if (position == "linear") {
    return((1 - u) * points[segment, , drop = FALSE] + u * points[segment + 1L, , drop = FALSE])
  }
  catmull_rom_3d(points, segment, u)
}

# Positive tracks interpolate in log space; the monotone cubic keeps every
# value between its neighbouring keys.
path_log_track_3d <- function(values, segment, u, position) {
  if (length(unique(values)) == 1L) return(rep(values[1L], length(segment)))
  x <- segment - 1 + u
  logs <- log(values)
  out <- if (position == "linear") stats::approx(seq_along(values) - 1, logs, xout = x)$y else
    stats::splinefun(seq_along(values) - 1, logs, method = "monoH.FC")(x)
  exp(out)
}

camera_values_3d <- function(camera) {
  camera <- unclass(camera)
  list(projection = camera$projection, azimuth = camera$azimuth, elevation = camera$elevation,
    roll = camera$roll, distance = camera$distance, fov = camera$fov, zoom = camera$zoom,
    target = camera$target, shear = camera$shear)
}

# Full camera values of a key over the values it continues from.
path_key_values_3d <- function(key, previous, up) {
  values <- previous
  if (!is.null(key$up)) {
    axis <- if (is.character(key$up)) match(key$up, c("x", "y", "z")) else key$up
    scene_assert(identical(as.double(axis), as.double(up)),
      "camera keys keep the vertical axis of coord_3d()")
  }
  for (name in intersect(names(key), c("projection", "azimuth", "elevation", "roll", "fov", "zoom", "target"))) {
    values[[name]] <- if (name == "projection") key[[name]] else as.double(key[[name]])
  }
  if (!is.null(key$projection) && !identical(key$projection, previous$projection) && is.null(key$distance)) {
    values$distance <- if (identical(key$projection, "perspective")) NA_real_ else Inf
  }
  if (!is.null(key$distance)) values$distance <- as.double(key$distance)
  if (!is.null(key$oblique)) {
    values$shear <- oblique_shear_3d(values$projection, key$oblique)
  } else if (values$projection != "oblique") {
    values$shear <- c(0, 0)
  } else if (!identical(previous$projection, "oblique")) {
    values$shear <- oblique_shear_3d("oblique", c(angle = 45, ratio = 0.5))
  }
  values
}

path_evaluators_3d <- list(
  keys = function(piece, times, start, context) {
    values <- Reduce(function(previous, key) path_key_values_3d(key, previous, context$up),
      piece$keys, start, accumulate = TRUE)[-1L]
    for (v in values) context$validate(v)
    same <- function(name) all(vapply(values, function(v) identical(v[[name]], values[[1L]][[name]]), logical(1)))
    scene_assert(same("projection") && same("shear"),
      "camera keys share one projection; dolly_zoom_3d() morphs between perspective and orthographic")
    distances <- vapply(values, `[[`, numeric(1), "distance")
    scene_assert(all(is.na(distances)) || all(!is.na(distances)),
      "give a perspective distance in every key or in none")
    where <- path_segments_3d(times, piece$at, piece$ease)
    s <- where$segment; u <- where$u
    q <- lapply(values, function(v) quaternion_from_matrix_3d(camera_frame_3d(v$azimuth, v$elevation, v$roll)))
    for (i in seq_along(q)[-1L]) if (sum(q[[i]] * q[[i - 1L]]) < 0) q[[i]] <- -q[[i]]
    controls <- if (piece$orientation == "squad") squad_controls_3d(q) else NULL
    targets <- path_points_at_3d(do.call(rbind, lapply(values, `[[`, "target")), s, u, piece$position)
    track <- function(name) path_log_track_3d(vapply(values, `[[`, numeric(1), name), s, u, piece$position)
    fov <- track("fov"); zoom <- track("zoom")
    distance <- if (all(is.finite(distances))) track("distance") else rep(distances[1L], length(s))
    previous <- c(azimuth = values[[1L]]$azimuth, roll = values[[1L]]$roll)
    lapply(seq_along(times), function(i) {
      a <- q[[s[i]]]; b <- q[[s[i] + 1L]]
      orientation <- if (is.null(controls)) slerp_3d(a, b, u[i]) else
        slerp_3d(slerp_3d(a, b, u[i]), slerp_3d(controls[[s[i]]], controls[[s[i] + 1L]], u[i]), 2 * u[i] * (1 - u[i]))
      angles <- quaternion_angles_3d(orientation, previous)
      previous <<- angles[c("azimuth", "roll")]
      out <- values[[1L]]
      out$azimuth <- angles[["azimuth"]]; out$elevation <- angles[["elevation"]]; out$roll <- angles[["roll"]]
      out$target <- targets[i, ]; out$fov <- fov[i]; out$zoom <- zoom[i]; out$distance <- distance[i]
      out
    })
  },
  orbit = function(piece, times, start, context) {
    where <- path_segments_3d(times, piece$at, piece$ease)
    lapply(where$u, function(u) {
      out <- start
      out$azimuth <- start$azimuth + piece$degrees * u
      if (!is.null(piece$elevation)) out$elevation <- start$elevation + (piece$elevation - start$elevation) * u
      out
    })
  },
  fly = function(piece, times, start, context) {
    scale <- context$aspect / max(context$aspect)
    model <- function(points) sweep(context$to_box(points), 2L, scale, "*")
    eye <- model(piece$eye)
    look <- if (is.null(piece$look)) matrix(scale / 2, 1L) else model(piece$look)
    where <- path_segments_3d(times, piece$at, piece$ease)
    eye <- path_points_at_3d(eye, where$segment, where$u, piece$position)
    look <- if (nrow(look) == 1L) look[rep(1L, length(times)), , drop = FALSE] else
      path_points_at_3d(look, where$segment, where$u, piece$position)
    roll <- piece$roll[where$segment] + (piece$roll[where$segment + 1L] - piece$roll[where$segment]) * where$u
    standard <- (0:2 + context$up) %% 3L + 1L
    previous <- start$azimuth
    lapply(seq_along(times), function(i) {
      offset <- eye[i, ] - look[i, ]
      distance <- sqrt(sum(offset^2))
      scene_assert(distance > 0, "the eye must not coincide with the point it looks at")
      outward <- offset[standard] / distance
      azimuth <- if (sum(outward[1:2]^2) > 1e-18) atan2(outward[2L], outward[1L]) * 180 / pi else previous
      azimuth <- azimuth - 360 * round((azimuth - previous) / 360)
      previous <<- azimuth
      out <- start
      out$projection <- "perspective"; out$shear <- c(0, 0)
      out$azimuth <- azimuth
      out$elevation <- asin(max(-1, min(1, outward[3L]))) * 180 / pi
      out$roll <- roll[i]
      out$target <- look[i, ] / scale
      out$distance <- distance
      if (!is.null(piece$fov)) out$fov <- piece$fov
      out
    })
  },
  dolly = function(piece, times, start, context) {
    where <- path_segments_3d(times, piece$at, piece$ease)
    radius <- camera_radius_3d(start$target, context$aspect)
    fov <- if (length(piece$fov) == 2L) piece$fov else
      c(if (identical(start$projection, "perspective")) start$fov else 0, piece$fov)
    scene_assert(any(fov > 0), "a dolly zoom from a parallel view needs a field of view above 0")
    slope <- tan(fov * pi / 360)
    begin <- if (fov[1L] > 0) "perspective" else "orthographic"
    zoom <- start$zoom
    reach <- if (begin == "orthographic") {
      radius / context$ratio()
    } else {
      distance <- if (identical(start$projection, "perspective") && is.finite(start$distance)) start$distance else
        radius / sin(atan(slope[1L] * context$ratio())) + 64 * .Machine$double.eps * max(1, radius)
      slope[1L] * distance
    }
    zoom_parallel <- zoom * radius / (reach * if (any(slope == 0)) context$ratio() else 1)
    lapply(where$u, function(u) {
      t <- if (all(slope > 0)) exp(log(slope[1L]) + (log(slope[2L]) - log(slope[1L])) * u) else
        slope[1L] + (slope[2L] - slope[1L]) * u
      out <- start
      out$shear <- c(0, 0)
      if (t <= 0) {
        out$projection <- "orthographic"; out$distance <- Inf; out$zoom <- zoom_parallel
      } else {
        out$projection <- "perspective"; out$fov <- 2 * atan(t) * 180 / pi
        out$distance <- reach / t; out$zoom <- zoom
      }
      out
    })
  }
)

camera_radius_3d <- function(target, aspect) {
  scale <- aspect / max(aspect)
  sqrt(sum(pmax(abs(target * scale), abs((1 - target) * scale))^2))
}

# Camera values of a path at the given times. `context` carries the starting
# camera, the vertical axis and aspect, a map from data to box coordinates, and
# the panel width-to-height ratio clamped to 1 (computed on demand).
path_cameras_3d <- function(path, times, context) {
  start <- camera_values_3d(context$camera)
  context$up <- as.integer(context$camera$up)
  context$aspect <- context$camera$aspect
  out <- vector("list", length(times))
  begin <- 0
  for (k in seq_along(path$pieces)) {
    piece <- path$pieces[[k]]
    span <- piece$at[length(piece$at)]
    last <- k == length(path$pieces)
    inside <- which(times >= begin & (times < begin + span | (last & times >= begin)))
    evaluate <- path_evaluators_3d[[piece$type]]
    if (length(inside)) out[inside] <- evaluate(piece, times[inside] - begin, start, context)
    if (!last) start <- evaluate(piece, span, start, context)[[1L]]
    begin <- begin + span
  }
  out
}

# Camera values of a path at evenly spaced times: `frames` of them, or by
# default `fps` per second. A closed path leaves out its closing view so that
# looping shows each view once.
path_sample_3d <- function(path, frames, fps, context) {
  ends <- path_cameras_3d(path, c(0, path$duration), context)
  closed <- path_closed_3d(ends[[1L]], ends[[2L]])
  if (is.null(frames)) frames <- max(1, round(path$duration * fps)) + !closed
  times <- if (frames == 1L) 0 else if (closed) path$duration * (seq_len(frames) - 1) / frames else
    seq(0, path$duration, length.out = frames)
  list(times = times, steps = path_cameras_3d(path, times, context), closed = closed)
}

path_closed_3d <- function(first, last) {
  angles <- c("azimuth", "roll")
  same_angles <- all(abs(((unlist(first[angles]) - unlist(last[angles])) + 180) %% 360 - 180) < 1e-9)
  rest <- setdiff(names(first), angles)
  same_angles && isTRUE(all.equal(first[rest], last[rest]))
}
