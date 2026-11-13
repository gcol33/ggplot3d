# Projection of layers onto the panes of the panel frame. A target is a plane
# of constant scale coordinate: a pane of the frame (resolved against the
# camera for "back" and "floor") or a plane at a data value. Each target draws
# the layer once with that coordinate replaced; primitives drawn on a pane take
# the pane's partition, so they are painted with the pane and its grid rather
# than depth-sorted against the data in front of it.

xy_suffixes_3d <- c("", "min", "max", "end", "intercept", "min_final", "max_final", "lower", "middle", "upper", "0")
z_suffixes_3d <- c("", "min", "max", "end", "intercept")

position_family_3d <- function(axis) paste0(axis, if (axis == "z") z_suffixes_3d else xy_suffixes_3d)

#' Project a layer onto the walls of the panel box
#'
#' Draws a layer flattened onto panes of the panel box: shadows of points on
#' the floor, contours on a side wall, or a whole two-dimensional ggplot2 layer
#' such as a histogram or density drawn as a marginal on a wall.
#'
#' @param onto Where the layer is drawn. `"floor"` is the low face of the
#'   vertical axis; `"back"` the rear one of each pair of vertical faces, chosen
#'   for the camera as for the panes drawn by [coord_3d()]; a pane name of the
#'   panel frame (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"zmin"`, `"zmax"` for
#'   the box, or a pane of a curvilinear frame); or `list(axis = "y", at = 0)`
#'   for the plane where that axis takes a value in data units. A character
#'   vector, or a list mixing names and planes, draws the layer once per target.
#' @param axes `NULL` for a three-dimensional layer. For a two-dimensional
#'   ggplot2 layer, the two 3D axes that receive its x and y, such as
#'   `c("x", "z")` to stand a histogram of x on a y wall. The layer is then
#'   drawn on the targets perpendicular to the remaining axis, and its x and y
#'   train the scales of those axes.
#' @details The replaced coordinate does not train the scales: a projected
#'   layer is drawn on the expanded panel box of the other layers. Primitives on
#'   a pane are painted with that pane: behind every data layer on a rear pane,
#'   over them on a pane facing the camera, so a shadow is never hidden by the
#'   pane it lies on. Planes at a data value are depth-sorted with the data.
#'   Surface normals of a flattened layer point into the box.
#'
#'   The approach follows `position_on_face()` in ggcube, contour projections
#'   in matplotlib (`zdir`, `offset`) and plot3D (`contour = list(side = )`).
#' @return A ggplot2 position adjustment.
#' @seealso [geom_rug_3d()] for marginal ticks on the walls.
#' @examples
#' d <- data.frame(x = rnorm(60), y = rnorm(60), z = rnorm(60))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_point_3d(colour = "grey40", position = position_project_3d(c("floor", "back"))) +
#'   geom_point_3d(colour = "steelblue") + coord_3d()
#'
#' g <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
#' g$z <- exp(-g$x^2 - g$y^2)
#' ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey85", alpha = 0.6) +
#'   geom_contour_3d(bins = 5, colour = "navy", position = position_project_3d("floor")) +
#'   geom_contour_filled_3d(bins = 5, position = position_project_3d("back")) +
#'   coord_3d()
#'
#' p + geom_point_3d() +
#'   ggplot2::geom_density(ggplot2::aes(x = x, y = ggplot2::after_stat(scaled) * 2),
#'     inherit.aes = FALSE, fill = "grey60", alpha = 0.6,
#'     position = position_project_3d("back", axes = c("x", "z"))) +
#'   coord_3d()
#' @export
position_project_3d <- function(onto = "floor", axes = NULL) {
  targets <- project_spec_3d(onto)
  if (!is.null(axes)) {
    if (!is.character(axes) || length(axes) != 2L || anyDuplicated(axes) || !all(axes %in% c("x", "y", "z"))) {
      stop("axes must be NULL or two different axes among x, y and z.", call. = FALSE)
    }
  }
  ggplot2::ggproto(NULL, PositionProject3D, onto = targets, axes = axes)
}

PositionProject3D <- ggplot2::ggproto("PositionProject3D", ggplot2::Position,
  kind_3d = "project",
  onto = list(),
  axes = NULL,
  setup_params = function(self, data) list(axes = self$axes),
  compute_layer = function(self, data, params, layout) {
    if (is.null(params$axes)) data else rename_planar_3d(data, params$axes)
  }
)

project_spec_3d <- function(onto) {
  if (is.list(onto) && !is.null(names(onto)) && all(c("axis", "at") %in% names(onto))) onto <- list(onto)
  if (is.character(onto)) onto <- as.list(onto)
  if (!is.list(onto) || !length(onto)) stop("onto must name panes or give list(axis = , at = ) planes.", call. = FALSE)
  lapply(onto, function(target) {
    if (is.character(target) && length(target) == 1L && !is.na(target) && nzchar(target)) return(list(pane = target))
    if (is.list(target) && setequal(names(target), c("axis", "at")) &&
        is.character(target$axis) && length(target$axis) == 1L && target$axis %in% c("x", "y", "z") &&
        is.numeric(target$at) && length(target$at) == 1L && !is.na(target$at)) {
      return(list(axis = target$axis, at = as.double(target$at)))
    }
    stop("onto must name panes or give list(axis = , at = ) planes.", call. = FALSE)
  })
}

# A two-dimensional layer's x and y families move to two 3D axes; its own z
# family is dropped, so the default height does not train the z scale.
rename_planar_3d <- function(data, axes) {
  data <- data[setdiff(names(data), position_family_3d("z"))]
  rename_families_3d(data, c("x", "y"), axes)
}

# Simultaneous renaming of position families, member by member.
rename_families_3d <- function(data, from, to) {
  old <- names(data); new <- old
  for (k in seq_along(from)) {
    for (suffix in xy_suffixes_3d) {
      column <- paste0(from[k], suffix)
      if (!column %in% old) next
      if ((from[k] == "z" || to[k] == "z") && !suffix %in% z_suffixes_3d) {
        stop("Column ", column, " has no counterpart on the ", to[k], " axis.", call. = FALSE)
      }
      new[old == column] <- paste0(to[k], suffix)
    }
  }
  names(data) <- new
  data
}

emit_viewport_3d <- function(panel_params) panel_params$viewport %or3d% new_scene_viewport(100, 100)

pane_partitions_3d <- function(panel_params) {
  furniture_partitions_3d(panel_params$camera, emit_viewport_3d(panel_params), frame_of_3d(panel_params))
}

# The plane of a pane: the scale coordinate it holds constant and its value.
pane_plane_3d <- function(pane) {
  fixed <- which(apply(pane$xyz, 2L, function(v) diff(range(v)) == 0))
  if (length(fixed) != 1L) stop("Pane ", pane$name, " is not a face of constant coordinate; layers cannot be projected onto it.", call. = FALSE)
  list(axis = fixed, value = pane$xyz[1L, fixed], pane = pane)
}

# Panes named directly or through "back" and "floor", each once.
resolve_panes_3d <- function(names, panel_params) {
  frame <- frame_of_3d(panel_params)
  panes <- frame$panes()
  pane_names <- vapply(panes, `[[`, character(1), "name")
  chosen <- unique(unlist(lapply(names, function(name) switch(name,
    back = frame$back_panes(pane_partitions_3d(panel_params)),
    floor = frame$floor_panes(),
    name)), use.names = FALSE))
  unknown <- setdiff(chosen, pane_names)
  if (length(unknown)) {
    stop("Unknown pane ", paste(unknown, collapse = ", "), "; this frame has ",
         paste(c("back", "floor", pane_names), collapse = ", "), ".", call. = FALSE)
  }
  lapply(panes[match(chosen, pane_names)], pane_plane_3d)
}

plane_target_3d <- function(axis, at, panel_params) {
  k <- match(axis, c("x", "y", "z"))
  if (is.infinite(at)) {
    value <- as.double(at > 0)
    panes <- frame_of_3d(panel_params)$panes()
    for (pane in panes) {
      plane <- tryCatch(pane_plane_3d(pane), error = function(e) NULL)
      if (!is.null(plane) && plane$axis == k && plane$value == value) return(plane)
    }
    return(list(axis = k, value = value, pane = NULL))
  }
  view <- panel_params[[axis]]
  position <- if (view$is_discrete()) at else view$get_transformation()$transform(at)
  range <- panel_params$ranges[[axis]]
  value <- (position - range[1L]) / diff(range)
  if (!is.finite(value)) stop("The ", axis, " plane at ", at, " is undefined on this scale.", call. = FALSE)
  list(axis = k, value = value, pane = NULL)
}

# Targets of a projection; with planar axes, only those perpendicular to the
# axis the layer does not use. Keywords may resolve to panes of other axes.
projection_targets_3d <- function(spec, panel_params) {
  normal <- if (is.null(spec$axes)) NULL else match(setdiff(c("x", "y", "z"), spec$axes), c("x", "y", "z"))
  targets <- list()
  for (target in spec$onto) {
    resolved <- if (is.null(target$pane)) list(plane_target_3d(target$axis, target$at, panel_params))
      else resolve_panes_3d(target$pane, panel_params)
    if (!is.null(normal)) {
      keyword <- !is.null(target$pane) && target$pane %in% c("back", "floor")
      fits <- vapply(resolved, function(t) t$axis == normal, logical(1))
      if (!keyword && !all(fits)) {
        stop("A layer on axes ", paste(spec$axes, collapse = " and "), " can only be drawn on a plane of constant ",
             c("x", "y", "z")[normal], ".", call. = FALSE)
      }
      resolved <- resolved[fits]
    }
    targets <- c(targets, resolved)
  }
  if (!length(targets)) stop("No pane perpendicular to the ", c("x", "y", "z")[normal], " axis matches onto.", call. = FALSE)
  targets
}

# Normalised layer data on one target. A three-dimensional layer keeps its
# positions and loses the target coordinate; a planar layer returns to x and y
# at z = 0, to be placed on the target after emission.
project_data_3d <- function(data, target, axes) {
  if (!is.null(axes)) {
    data <- rename_families_3d(data, axes, c("x", "y"))
    data$z <- rep(0, nrow(data))
    return(data)
  }
  axis <- c("x", "y", "z")[target$axis]
  for (column in intersect(position_family_3d(axis), names(data))) {
    data[[column]] <- ifelse(is.na(data[[column]]), NA_real_, target$value)
  }
  if (all(c("nx", "ny", "nz") %in% names(data))) {
    inward <- numeric(3); inward[target$axis] <- if (target$value == 1) -1 else 1
    data[c("nx", "ny", "nz")] <- as.list(inward)
  }
  data
}

# Moves an emitted planar packet from the z = 0 plane onto its target.
place_planar_packet_3d <- function(packet, axes, target) {
  slots <- c(match(axes, c("x", "y", "z")), target$axis)
  vertices <- packet$vertices
  placed <- list(vertices$x, vertices$y, rep(target$value, length(vertices$x)))
  packet$vertices <- stats::setNames(placed[order(slots)], c("x", "y", "z"))
  normals <- c("normal_x", "normal_y", "normal_z")
  if (all(normals %in% names(packet$styles))) packet$styles[normals] <- packet$styles[normals][order(slots)]
  packet
}

# Depth-sorted primitives on a pane move to the pane's partition: rear (0) or
# facing the camera (2), classified where each primitive lies on the pane.
pane_partition_packet_3d <- function(packet, pane, panel_params) {
  camera <- panel_params$camera
  if (!isTRUE(camera$clip_data)) return(packet)
  frame <- frame_of_3d(panel_params)
  basis <- frame_basis_3d(camera, emit_viewport_3d(panel_params))
  vertices <- do.call(cbind, packet$vertices)
  for (kind in c("faces", "polylines", "markers", "text")) {
    tab <- packet[[kind]]
    depth_sorted <- which(tab$partition == 1L)
    if (!length(depth_sorted)) next
    points <- vertices[packet$corners$vertex[tab$offset[depth_sorted]], , drop = FALSE]
    tab$partition[depth_sorted] <- facing_3d(pane_normals_3d(frame, pane, points, camera), frame$map(points), camera, basis)
    packet[[kind]] <- tab
  }
  packet
}

# Flattening collapses faces perpendicular to the target and repeats corners
# along edges perpendicular to it. Repeated consecutive corners are dropped;
# faces left without area are removed. Newell's area vector of the ring is
# unchanged by dropping a corner that coincides with its successor.
clean_flat_faces_3d <- function(packet, eps = 1e-9) {
  faces <- packet$faces
  n <- length(faces$offset)
  if (!n) return(packet)
  face <- rep.int(seq_len(n), faces$count)
  position <- sequence(faces$count)
  corner <- faces$offset[face] + position - 1L
  following <- faces$offset[face] + position %% faces$count[face]
  xyz <- do.call(cbind, packet$vertices)
  p <- xyz[packet$corners$vertex[corner], , drop = FALSE]
  q <- xyz[packet$corners$vertex[following], , drop = FALSE]
  distinct <- rowSums(abs(q - p)) > eps
  area <- sqrt(rowSums(rowsum(cross_rows_3d(p, q), face, reorder = TRUE)^2))
  kept <- tabulate(face[distinct], n)
  good <- kept >= 3L & area > eps
  changed <- good & kept < faces$count
  if (any(changed)) {
    rows <- distinct & changed[face]
    start <- length(packet$corners$vertex)
    packet$corners <- lapply(packet$corners, function(column) c(column, column[corner[rows]]))
    faces$offset[changed] <- as.integer(start + 1L + c(0L, cumsum(kept[changed])[-sum(changed)]))
    faces$count[changed] <- as.integer(kept[changed])
  }
  packet$faces <- lapply(faces, function(column) column[good])
  packet
}

emit_projected_3d <- function(entry, data, params, panel_params) {
  spec <- params$.project
  packets <- lapply(projection_targets_3d(spec, panel_params), function(target) {
    packet <- entry$emit(project_data_3d(data, target, spec$axes), params, panel_params)
    if (inherits(packet, "grob")) stop("position_project_3d() requires a layer that draws scene geometry.", call. = FALSE)
    validate_scene_packet(packet)
    if (!is.null(spec$axes)) packet <- place_planar_packet_3d(packet, spec$axes, target)
    packet <- clean_flat_faces_3d(packet)
    if (!is.null(target$pane)) packet <- pane_partition_packet_3d(packet, target$pane, panel_params)
    packet
  })
  merge_scene_packets(packets, panel_params$panel_id)
}

# The plane argument of contour and density layers projects them onto the z
# floor or a horizontal plane.
plane_position_3d <- function(plane, position) {
  if (identical(plane, "surface")) return(position)
  if (!(identical(position, "identity") || inherits(position, "PositionIdentity"))) {
    stop("plane positions the layer; supply plane or position, not both.", call. = FALSE)
  }
  position_project_3d(if (identical(plane, "floor")) "zmin" else list(axis = "z", at = plane))
}
