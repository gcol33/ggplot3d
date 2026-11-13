# Curvilinear frames: scale axes take the roles of angle, radius and height.
# Panes are the cube faces of scale space under the frame map; a pane whose
# image collapses or lies inside the solid (the seam of a full turn) is dropped.

role_points_3d <- function(roles, values) {
  out <- matrix(0, nrow(values), 3L)
  out[, roles] <- values
  out
}

FrameCurvilinear <- ggplot2::ggproto("FrameCurvilinear", Frame3D,
  linear = FALSE,
  roles = c(1L, 2L, 3L),
  pane_labels = character(),
  full = TRUE,
  start = 0,
  end = 2 * pi,
  direction = 1,
  inner = 0,
  # Cube panes, renamed by role, without collapsed images or interior seams.
  panes = function(self) {
    panes <- cube_panes_3d()
    keep <- list()
    for (pane in panes) {
      label <- self$pane_labels[[pane$name]]
      if (is.null(label) || frame_pane_collapsed_3d(self, pane)) next
      pane$name <- label
      keep[[length(keep) + 1L]] <- pane
    }
    keep
  },
  edges = function(self) {
    labels <- self$pane_labels
    out <- list()
    for (edge in cube_frame_edges_3d()) {
      incident <- unname(unlist(labels[edge$incident]))
      if (length(incident) < 2L) next
      path <- self$map(segment_points_3d(edge$xyz[rep(1L, 9L), ], edge$xyz[rep(2L, 9L), ], (0:8) / 8))
      if (max(apply(path, 2L, function(v) diff(range(v)))) < 1e-9) next
      if (!all(incident %in% vapply(self$panes(), `[[`, character(1), "name"))) next
      edge$incident <- incident
      out[[length(out) + 1L]] <- edge
    }
    out
  },
  rear_only = function(self, pane) TRUE,
  back_candidates = function(self) {
    names <- vapply(self$panes(), `[[`, character(1), "name")
    self$back_panes(stats::setNames(integer(length(names)), names))
  },
  angle = function(self, u) self$direction * (self$start + u * (self$end - self$start)),
  radius = function(self, u) 0.5 * (self$inner + (1 - self$inner) * u)
)

frame_pane_collapsed_3d <- function(frame, pane) {
  grid <- frame_surface_samples_3d(list(pane), 6L)
  model <- frame$map(grid)
  spread <- apply(model, 2L, function(v) diff(range(v)))
  sum(spread > 1e-9) < 2L
}

pane_label_map_3d <- function(roles, names) {
  out <- list()
  for (k in seq_len(3L)) {
    axis <- c("x", "y", "z")[roles[k]]
    out[[paste0(axis, "min")]] <- names[[k]][1L]
    out[[paste0(axis, "max")]] <- names[[k]][2L]
  }
  out[!is.na(unlist(out))]
}

FrameCylindrical <- ggplot2::ggproto("FrameCylindrical", FrameCurvilinear,
  name = "cylindrical",
  map = function(self, xyz) {
    u <- xyz[, self$roles, drop = FALSE]
    phi <- self$angle(u[, 1L]); r <- self$radius(u[, 2L])
    cbind(0.5 + r * sin(phi), 0.5 + r * cos(phi), u[, 3L])
  },
  back_panes = function(self, partitions) intersect(c("wall", "inner", "start", "end"), names(partitions)),
  floor_panes = function(self) "floor",
  axis_candidates = function(self, guide) {
    role <- match(match(guide$axis, c("x", "y", "z")), self$roles)
    point <- function(t, r, h) role_points_3d(self$roles, cbind(t, r, h))[1L, ]
    candidates <- switch(role,
      lapply(c(0, 1), function(h) list(start = point(0, 1, h), end = point(1, 1, h))),
      lapply(if (self$full) 0 else c(0, 1), function(t) lapply(c(0, 1), function(h) list(start = point(t, 0, h), end = point(t, 1, h)))),
      lapply(frame_angle_samples_3d(self$full), function(t) list(start = point(t, 1, 0), end = point(t, 1, 1))))
    if (role == 2L) candidates <- unlist(candidates, recursive = FALSE)
    attr(candidates, "auto") <- if (role == 3L) "outer" else "lower"
    candidates
  }
)

FrameSpherical <- ggplot2::ggproto("FrameSpherical", FrameCurvilinear,
  name = "spherical",
  philim = c(-pi / 2, pi / 2),
  shells = "outer",
  shell_breaks = numeric(),
  # Inner graticule shells sit at the major radius breaks of each panel.
  bind = function(self, panel_params) {
    if (self$shells != "breaks") return(self)
    view <- panel_params[[c("x", "y", "z")[self$roles[3L]]]]
    breaks <- view$break_positions()
    breaks <- breaks[is.finite(breaks) & breaks > 0 & breaks < 1]
    ggplot2::ggproto(NULL, self, shell_breaks = breaks)
  },
  panes = function(self) {
    panes <- ggplot2::ggproto_parent(FrameCurvilinear, self)$panes()
    outer <- Filter(function(p) p$name == "shell", panes)
    if (!length(outer)) return(panes)
    role <- self$roles[3L]
    shells <- lapply(seq_along(self$shell_breaks), function(k) {
      pane <- outer[[1L]]
      pane$xyz[, role] <- self$shell_breaks[k]
      pane$name <- paste0("shell", k)
      pane$fill <- FALSE
      pane
    })
    c(panes, shells)
  },
  latitude = function(self, u) self$philim[1L] + u * (self$philim[2L] - self$philim[1L]),
  map = function(self, xyz) {
    u <- xyz[, self$roles, drop = FALSE]
    lon <- self$angle(u[, 1L]); lat <- self$latitude(u[, 2L]); r <- self$radius(u[, 3L])
    cbind(0.5 + r * cos(lat) * sin(lon), 0.5 + r * cos(lat) * cos(lon), 0.5 + r * sin(lat))
  },
  back_panes = function(self, partitions) intersect(c("shell", paste0("shell", seq_along(self$shell_breaks)), "core", "south", "north", "start", "end"), names(partitions)),
  floor_panes = function(self) character(),
  equator = function(self) {
    u <- (0 - self$philim[1L]) / (self$philim[2L] - self$philim[1L])
    if (u < 0 || u > 1) 0.5 else u
  },
  axis_candidates = function(self, guide) {
    role <- match(match(guide$axis, c("x", "y", "z")), self$roles)
    point <- function(t, p, r) role_points_3d(self$roles, cbind(t, p, r))[1L, ]
    e <- self$equator()
    candidates <- switch(role,
      list(list(start = point(0, e, 1), end = point(1, e, 1))),
      lapply(frame_angle_samples_3d(self$full), function(t) list(start = point(t, 0, 1), end = point(t, 1, 1))),
      lapply(frame_angle_samples_3d(self$full), function(t) list(start = point(t, e, 0), end = point(t, e, 1))))
    attr(candidates, "auto") <- if (role == 2L) "outer" else "lower"
    candidates
  }
)

frame_angle_samples_3d <- function(full, n = 16L) if (full) (seq_len(n) - 1) / n else seq(0, 1, length.out = n + 1L)

polar_roles_3d <- function(theta, second, name) {
  axes <- c("x", "y", "z")
  theta <- match.arg(theta, axes); second <- match.arg(second, axes)
  if (theta == second) stop("theta and ", name, " must name different position aesthetics.", call. = FALSE)
  match(c(theta, second, setdiff(axes, c(theta, second))), axes)
}

polar_angles_3d <- function(start, end, direction) {
  if (!(is.numeric(start) && length(start) == 1L && is.finite(start))) stop("start must be a finite number of radians.", call. = FALSE)
  if (is.null(end)) end <- start + 2 * pi
  if (!(is.numeric(end) && length(end) == 1L && is.finite(end) && end != start)) stop("end must be a finite number of radians different from start.", call. = FALSE)
  if (abs(end - start) > 2 * pi + 1e-12) stop("The angular range spans more than one turn.", call. = FALSE)
  if (!direction %in% c(-1, 1)) stop("direction must be 1 (clockwise) or -1 (anticlockwise).", call. = FALSE)
  list(start = start, end = end, direction = direction, full = abs(abs(end - start) - 2 * pi) < 1e-9)
}

check_inner_radius_3d <- function(inner.radius) {
  if (!(is.numeric(inner.radius) && length(inner.radius) == 1L && is.finite(inner.radius) && inner.radius >= 0 && inner.radius < 1)) {
    stop("inner.radius must be a number in [0, 1).", call. = FALSE)
  }
  inner.radius
}

coord_with_frame_3d <- function(coord, frame, panes) {
  check_frame_panes_3d(frame, panes)
  if (identical(coord$model_aspect, "data")) stop("aspect = \"data\" applies to Cartesian frames only.", call. = FALSE)
  coord$frame <- frame
  coord$config$panes <- unique(panes)
  # An angle wraps around, so its range takes no expansion.
  coord$expand_axes[[c("x", "y", "z")[frame$roles[1L]]]] <- FALSE
  coord
}

#' Cylindrical three-dimensional coordinates
#'
#' One position aesthetic sets the angle around a vertical axis, another the
#' distance from that axis, and the third the height. Lines and faces that are
#' straight in the data are subdivided so they follow the cylinder, so paths
#' become arcs and bars become ring sectors with exact visibility.
#' @param theta,r Position aesthetics, `"x"`, `"y"` or `"z"`, used for the
#'   angle and the radius. The remaining one is the height.
#' @param start Angle in radians, measured from 12 o'clock seen from above,
#'   where the angle axis starts.
#' @param end Angle in radians where the angle axis ends; `NULL` is one full
#'   turn after `start`.
#' @param inner.radius Fraction of the outer radius left empty at the centre.
#' @param direction 1 for clockwise, -1 for anticlockwise.
#' @param panes Panes to draw: `"back"` (the rear half of the outer wall, of the
#'   inner wall and of the cut planes of a partial turn), `"floor"`,
#'   `"front"` (the parts of the other panes that face the viewer, see
#'   [coord_3d()]), or names among `"floor"`, `"top"`, `"wall"`, `"inner"`,
#'   `"start"`, `"end"`.
#' @param ... Other [coord_3d()] arguments: camera, limits, axes, box, light,
#'   backend.
#' @return A Coord3D ggproto object.
#' @examples
#' d <- data.frame(month = 1:12, site = rep(c(1, 2), 6), value = c(3:8, 8:3))
#' ggplot2::ggplot(d, ggplot2::aes(month, site, z = value)) +
#'   geom_point_3d(size = 2) + coord_cylindrical_3d(elevation = 35)
#' @export
coord_cylindrical_3d <- function(theta = "x", r = "y", start = 0, end = NULL, inner.radius = 0,
                                 direction = 1, panes = c("back", "floor"), ...) {
  angles <- polar_angles_3d(start, end, direction)
  roles <- polar_roles_3d(theta, r, "r")
  frame <- ggplot2::ggproto(NULL, FrameCylindrical, roles = roles,
    start = angles$start, end = angles$end, direction = angles$direction, full = angles$full,
    inner = check_inner_radius_3d(inner.radius),
    pane_labels = pane_label_map_3d(roles, list(if (angles$full) c(NA, NA) else c("start", "end"),
      c(if (inner.radius > 0) "inner" else NA, "wall"), c("floor", "top"))))
  coord_with_frame_3d(coord_3d(...), frame, panes)
}

#' Spherical three-dimensional coordinates
#'
#' Position aesthetics set longitude, latitude and distance from the centre.
#' Lines and faces are subdivided so they follow the sphere; the rear half of
#' the outer shell carries the graticule of longitude and latitude breaks.
#' @inheritParams coord_cylindrical_3d
#' @param theta,phi,r Position aesthetics, `"x"`, `"y"` or `"z"`, used for
#'   longitude, latitude and radius.
#' @param start,end Longitude range in radians, measured from 12 o'clock seen
#'   from above; `end = NULL` is one full turn.
#' @param philim Latitude range in radians, default from the south pole to the
#'   north pole.
#' @param panes Panes to draw: `"back"` (the rear half of the outer shell and
#'   of any cut surfaces), `"front"` (the parts of the other panes that face
#'   the viewer), or names among `"shell"`, `"core"`, `"south"`, `"north"`,
#'   `"start"`, `"end"`.
#' @param shells `"outer"` draws the graticule on the outer shell; `"breaks"`
#'   adds the rear half of a graticule shell, without fill, at each major break
#'   of the radius scale.
#' @return A Coord3D ggproto object.
#' @examples
#' g <- expand.grid(lon = seq(0, 330, by = 30), lat = seq(-60, 60, by = 30))
#' g$r <- 1 + 0.2 * cos(g$lat * pi / 90)
#' ggplot2::ggplot(g, ggplot2::aes(lon, lat, z = r)) +
#'   geom_point_3d() + coord_spherical_3d(elevation = 20)
#' @export
coord_spherical_3d <- function(theta = "x", phi = "y", r = "z", start = 0, end = NULL,
                               philim = NULL, inner.radius = 0, direction = 1,
                               panes = "back", shells = c("outer", "breaks"), ...) {
  angles <- polar_angles_3d(start, end, direction)
  roles <- polar_roles_3d(theta, phi, "phi")
  if (!identical(c("x", "y", "z")[roles[3L]], match.arg(r, c("x", "y", "z")))) stop("theta, phi and r must name x, y and z in some order.", call. = FALSE)
  if (is.null(philim)) philim <- c(-pi / 2, pi / 2)
  if (!(is.numeric(philim) && length(philim) == 2L && all(is.finite(philim)) && philim[1L] < philim[2L] &&
        philim[1L] >= -pi / 2 && philim[2L] <= pi / 2)) stop("philim must be two increasing latitudes in [-pi/2, pi/2].", call. = FALSE)
  pole <- function(value, label) if (abs(abs(value) - pi / 2) < 1e-12) NA else label
  frame <- ggplot2::ggproto(NULL, FrameSpherical, roles = roles,
    start = angles$start, end = angles$end, direction = angles$direction, full = angles$full,
    inner = check_inner_radius_3d(inner.radius), philim = philim, shells = match.arg(shells),
    pane_labels = pane_label_map_3d(roles, list(if (angles$full) c(NA, NA) else c("start", "end"),
      c(pole(philim[1L], "south"), pole(philim[2L], "north")), c(if (inner.radius > 0) "core" else NA, "shell"))))
  coord_with_frame_3d(coord_3d(...), frame, panes)
}
