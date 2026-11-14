cube_edges_3d <- function() {
  corners <- as.matrix(expand.grid(x = 0:1, y = 0:1, z = 0:1))
  edges <- do.call(rbind, lapply(seq_len(3L), function(axis) {
    starts <- which(corners[, axis] == 0)
    cbind(axis = axis, edge = seq_along(starts), a = starts, b = starts + 2L^(axis - 1L))
  }))
  list(corners = corners, edges = edges)
}

text_metrics_3d <- function(labels, element, angle = 0) {
  if (!visible_element_3d(element) || !length(labels)) return(matrix(0, length(labels), 2L))
  metrics <- t(vapply(seq_along(labels), function(i) {
    g <- grid::textGrob(labels[i], gp = text_gp_3d(element))
    c(grid::convertWidth(grid::grobWidth(g), "mm", TRUE), grid::convertHeight(grid::grobHeight(g), "mm", TRUE))
  }, numeric(2)))
  rotated_extent_3d(metrics, angle)
}

# Width and height of the bounding rectangle of each n x 2 text box turned
# by `angle` degrees (one angle or one per row).
rotated_extent_3d <- function(size, angle) {
  if (!is.matrix(size)) size <- matrix(size, ncol = 2L)
  a <- rep_len(angle, nrow(size)) * pi / 180
  cbind(abs(cos(a)) * size[, 1L] + abs(sin(a)) * size[, 2L],
        abs(sin(a)) * size[, 1L] + abs(cos(a)) * size[, 2L])
}

# Angle in degrees of a page direction, turned by a half turn where needed so
# that text along it reads upright.
upright_angle_3d <- function(dx, dy) {
  angle <- atan2(dy, dx) * 180 / pi
  angle <- ifelse(angle > 90, angle - 180, ifelse(angle < -90, angle + 180, angle))
  angle[!is.finite(angle)] <- 0
  angle
}

element_margin_mm_3d <- function(element) {
  margin <- element$margin
  if (is.null(margin)) return(0)
  max(grid::convertUnit(margin, "mm", valueOnly = TRUE))
}

measure_guides_3d <- function(params, theme, viewport) {
  guides <- params$axis_guides
  metrics <- lapply(guides, function(guide) {
    axis <- guide$axis
    local <- guide$theme
    text <- element_3d(paste0("axis.text.", axis), local)
    title <- element_3d(paste0("axis.title.", axis), local)
    angle <- if (inherits(guide$params$angle, "waiver")) 0 else guide$params$angle
    label <- text_metrics_3d(guide$labels, text)
    title_size <- text_metrics_3d(guide$title, title)
    tick <- element_3d(paste0("axis.ticks.", axis), local)
    tick_length <- if (visible_element_3d(tick)) abs(grid::convertUnit(element_3d(paste0("axis.ticks.length.", axis), local), "mm", valueOnly = TRUE)) else 0
    for (panel_guide in params$panel_guides) {
      if (is.function(panel_guide$axis_space_3d)) tick_length <- tick_length + panel_guide$axis_space_3d(axis)
    }
    minor_length <- if (isTRUE(guide$params$minor.ticks)) abs(grid::convertUnit(element_3d(paste0("axis.minor.ticks.length.", axis), local) %or3d% grid::unit(1, "pt"), "mm", valueOnly = TRUE)) else 0
    gap <- if (visible_element_3d(text)) element_margin_mm_3d(text) + 0.8 else 0
    title_gap <- if (visible_element_3d(title)) element_margin_mm_3d(title) + 1.2 else 0
    label_extent <- if (length(label)) max(sqrt(rowSums(label^2))) else 0
    title_extent <- if (visible_element_3d(title) && length(title_size)) {
      offset <- max(tick_length, minor_length) + gap + guide$params$n.dodge * label_extent + title_gap + title_size[1L, 2L] / 2
      sqrt((title_size[1L, 1L] / 2)^2 + offset^2) + title_size[1L, 2L] / 2
    } else 0
    list(text = text, title = title, angle = angle, label = label, title_size = title_size,
      tick_length = max(tick_length, minor_length), gap = gap, title_gap = title_gap,
      label_extent = label_extent, title_extent = title_extent,
      extent = max(max(tick_length, minor_length) + gap + guide$params$n.dodge * label_extent, title_extent))
  })
  active <- names(guides)[vapply(guides, guide_on_box_3d, logical(1), params)]
  extent <- if (length(active)) max(vapply(metrics[active], `[[`, numeric(1), "extent")) else 0
  margin <- element_3d("panel.margin.3d", theme) %or3d% ggplot2::margin(8, 8, 8, 8, unit = "pt")
  margin <- grid::convertUnit(margin, "mm", valueOnly = TRUE)
  # A common reserve on all sides keeps magnification independent of edge selection.
  reserve <- if (identical(params$coord_params$frame, "view")) margin else extent + margin
  width <- viewport$width_mm - reserve[2L] - reserve[4L]
  height <- viewport$height_mm - reserve[1L] - reserve[3L]
  if (!is.finite(width) || !is.finite(height) || min(width, height) <= 0) stop("3D panel too small for measured axis labels and titles; enlarge the device or reduce text size.", call. = FALSE)
  list(metrics = metrics, reserve = reserve,
    viewport = new_scene_viewport(width, height, viewport$dpi),
    origin = c(reserve[4L], reserve[3L]))
}

frame_basis_3d <- function(camera, viewport) {
  scale <- camera$aspect / max(camera$aspect)
  extent <- pmax(abs(camera$target * scale), abs((1 - camera$target) * scale))
  radius <- sqrt(sum(extent^2))
  ratio <- viewport$width_mm / viewport$height_mm
  az <- (camera$azimuth %% 360) * pi / 180
  el <- camera$elevation * pi / 180
  roll <- (camera$roll %% 360) * pi / 180
  # Angles are defined about the vertical axis; a cyclic permutation carries
  # the z-up frame onto the chosen vertical axis.
  axes <- (0:2 + (camera$up %or3d% 3L)) %% 3L + 1L
  right <- up <- forward <- numeric(3)
  right[axes] <- c(-sin(az), cos(az), 0)
  up[axes] <- c(-sin(el) * cos(az), -sin(el) * sin(az), cos(el))
  forward[axes] <- -c(cos(el) * cos(az), cos(el) * sin(az), sin(el))
  rolled_right <- cos(roll) * right - sin(roll) * up
  rolled_up <- sin(roll) * right + cos(roll) * up
  shear <- camera$shear %or3d% c(0, 0)
  basis <- cbind(rolled_right + shear[1L] * forward, rolled_up + shear[2L] * forward)
  ray <- forward - shear[1L] * rolled_right - shear[2L] * rolled_up
  perspective <- camera$projection == "perspective"
  distance <- camera$distance
  if (perspective && is.na(distance)) {
    half_angle <- atan(tan(camera$fov * pi / 360) * min(1, ratio))
    distance <- radius / sin(half_angle) + 64 * .Machine$double.eps * max(1, radius)
  }
  base <- if (perspective) 1 / tan(camera$fov * pi / 360) else min(1, ratio) / radius
  list(scale = scale, extent = extent, radius = radius, ratio = ratio, basis = basis,
    right = rolled_right, up = rolled_up, forward = forward, ray = ray, standard = all(shear == 0) && identical(as.integer(camera$up %or3d% 3L), 3L),
    perspective = perspective, distance = distance, base = base)
}

frame_eye_coordinates_3d <- function(xyz, camera, basis) {
  delta <- sweep(sweep(xyz, 2L, camera$target), 2L, basis$scale, "*")
  xy <- delta %*% basis$basis
  distance <- if (basis$perspective) basis$distance else 2 * basis$radius + 1
  cbind(x = xy[, 1L], y = xy[, 2L], depth = distance + drop(delta %*% basis$forward))
}

frame_in_front_3d <- function(frame, camera, basis) {
  eye <- frame_eye_coordinates_3d(rbind(frame$map(cube_edges_3d()$corners), frame$bounds()), camera, basis)
  all(eye[, 3L] > 0)
}

frame_coordinates_3d <- function(xyz, camera, basis) {
  coordinates <- frame_eye_coordinates_3d(xyz, camera, basis)
  if (any(coordinates[, 3L] <= 0)) stop("The projected box crosses the eye plane; use a greater camera distance.", call. = FALSE)
  if (basis$perspective) coordinates[, 1:2] <- coordinates[, 1:2] / coordinates[, 3L]
  coordinates
}

# A polyline whose points all lie on one sight ray has no projected stroke
# length; the test cross-multiplies by depth, so points behind the eye count.
sight_ray_line_3d <- function(xyz, camera, basis) {
  eye <- frame_eye_coordinates_3d(xyz, camera, basis)
  if (!basis$perspective) return(max(abs(sweep(eye[, 1:2, drop = FALSE], 2L, eye[1L, 1:2]))) <= 64 * .Machine$double.eps)
  spread <- abs(eye[, 1:2, drop = FALSE] * eye[1L, 3L] - outer(eye[, 3L], eye[1L, 1:2]))
  max(spread) <= 64 * .Machine$double.eps * max(1, abs(eye))^2
}

frame_positions_3d <- function(xyz, camera, basis) {
  coordinates <- frame_coordinates_3d(xyz, camera, basis)
  coordinates[, 1L] <- 0.5 + coordinates[, 1L] * basis$base * camera$zoom / (2 * basis$ratio)
  coordinates[, 2L] <- 0.5 + coordinates[, 2L] * basis$base * camera$zoom / 2
  coordinates
}

# Screen positions of homogeneous box fractions under the camera pre-transform,
# divided only after pre; rows at or behind infinity, behind a perspective
# eye or left unmapped get Inf.
screen_positions_homogeneous_3d <- function(xyzw, camera, basis) {
  if (ncol(xyzw) == 3L) xyzw <- cbind(xyzw, 1)
  pre <- camera$pre %or3d% as.double(diag(4))
  if (!identical(as.double(pre), as.double(diag(4)))) xyzw <- xyzw %*% t(matrix(pre, 4L, 4L))
  out <- matrix(Inf, nrow(xyzw), 2L)
  finite <- which(rowSums(is.na(xyzw)) == 0 & xyzw[, 4L] > 0)
  if (!length(finite)) return(out)
  xyz <- xyzw[finite, 1:3, drop = FALSE] / xyzw[finite, 4L]
  if (basis$perspective) {
    front <- frame_eye_coordinates_3d(xyz, camera, basis)[, 3L] > 0
    finite <- finite[front]; xyz <- xyz[front, , drop = FALSE]
  }
  if (length(finite)) out[finite, ] <- frame_positions_3d(xyz, camera, basis)[, 1:2, drop = FALSE]
  out
}

frame_camera_3d <- function(camera, viewport, frame, bounds = cube_edges_3d()$corners) {
  if (frame == "sphere") return(camera)
  geometry <- frame_basis_3d(camera, viewport)
  scale <- geometry$scale; extent <- geometry$extent; radius <- geometry$radius
  ratio <- geometry$ratio; basis <- geometry$basis; forward <- geometry$forward
  perspective <- geometry$perspective; distance <- geometry$distance
  # The azimuth-invariant support assumes a z-up camera without shear.
  if (frame == "view" || !geometry$standard) {
    xy <- frame_coordinates_3d(bounds, camera, geometry)
    support <- apply(abs(xy[, 1:2]), 2L, max)
  } else {
    # The enclosing vertical cylinder has azimuth-invariant projected support.
    radial <- sqrt(sum(extent[1:2]^2))
    if (!perspective) support <- radial * sqrt(colSums(basis[1:2, , drop = FALSE]^2)) + extent[3L] * abs(basis[3L, ])
    else {
      support <- vapply(seq_len(2L), function(axis) {
        a <- basis[1:2, axis]; b <- forward[1:2]
        z <- c(-camera$target[3L], 1 - camera$target[3L]) * scale[3L]
        d <- distance + z * forward[3L]; c <- z * basis[3L, axis]
        if (any(d <= radial * sqrt(sum(b^2)))) stop("The azimuth frame crosses the eye plane; use a greater camera distance.", call. = FALSE)
        aa <- d^2 - radial^2 * sum(b^2)
        bb <- -2 * c * d + 2 * radial^2 * sum(a * b)
        cc <- c^2 - radial^2 * sum(a^2)
        root <- sqrt(pmax(0, bb^2 - 4 * aa * cc))
        max(abs(c((-bb - root) / (2 * aa), (-bb + root) / (2 * aa))))
      }, numeric(1))
    }
  }
  base <- geometry$base
  camera$zoom <- camera$zoom / (base * max(support[1L] / ratio, support[2L]))
  camera$zoom <- camera$zoom * (1 - 1 / min(viewport$width_mm, viewport$height_mm) * 25.4 / viewport$dpi)
  camera
}

furniture_partitions_3d <- function(camera, viewport, frame = FrameBox) {
  basis <- frame_basis_3d(camera, viewport)
  panes <- frame$panes()
  partitions <- vapply(panes, function(pane) {
    centre <- matrix(colMeans(pane$xyz), 1L)
    facing_3d(pane_normals_3d(frame, pane, centre, camera), frame$map(centre), camera, basis)
  }, integer(1))
  stats::setNames(partitions, vapply(panes, `[[`, character(1), "name"))
}

panel_furniture_3d <- function(panel_params, theme, viewport, frame_group = list()) {
  frame <- frame_of_3d(panel_params)
  bounds_of <- function(params) frame_of_3d(params)$bounds()
  fit <- measure_guides_3d(panel_params, theme, viewport)
  if (length(frame_group)) {
    fits <- lapply(frame_group, function(panel) measure_guides_3d(panel$params, panel$theme, viewport))
    fit$reserve <- Reduce(pmax, lapply(fits, `[[`, "reserve"))
    fit$origin <- fit$reserve[c(4L, 3L)]
    fit$viewport <- new_scene_viewport(viewport$width_mm - sum(fit$reserve[c(2L, 4L)]),
      viewport$height_mm - sum(fit$reserve[c(1L, 3L)]), viewport$dpi)
  }
  mode <- panel_params$coord_params$frame %or3d% "sphere"
  camera <- frame_camera_3d(panel_params$camera, fit$viewport, mode, bounds_of(panel_params))
  if (length(frame_group)) {
    magnifications <- vapply(frame_group, function(panel) {
      fitted <- frame_camera_3d(panel$params$camera, fit$viewport, mode, bounds_of(panel$params))
      fitted$zoom / panel$params$camera$zoom
    }, numeric(1))
    camera$zoom <- panel_params$camera$zoom * min(magnifications)
  }
  if (mode == "view") {
    groups <- if (length(frame_group)) frame_group else list(list(params = panel_params, theme = theme))
    group_fits <- lapply(groups, function(panel) measure_guides_3d(panel$params, panel$theme, viewport))
    for (iteration in seq_len(16L)) {
      reserves <- Map(function(panel, measured) view_guide_reserve_3d(panel$params, measured, fit$viewport), groups, group_fits)
      reserve <- Reduce(pmax, reserves)
      difference <- max(abs(reserve - fit$reserve))
      fit$reserve <- if (difference < 0.01) pmax(reserve, fit$reserve) else (reserve + fit$reserve) / 2
      fit$origin <- fit$reserve[c(4L, 3L)]
      width <- viewport$width_mm - sum(fit$reserve[c(2L, 4L)])
      height <- viewport$height_mm - sum(fit$reserve[c(1L, 3L)])
      if (min(width, height) <= 0) stop("3D panel too small for measured axis labels and titles; enlarge the device or reduce text size.", call. = FALSE)
      fit$viewport <- new_scene_viewport(width, height, viewport$dpi)
      if (difference < 0.01) break
    }
    magnifications <- vapply(groups, function(panel) {
      fitted <- frame_camera_3d(panel$params$camera, fit$viewport, mode, bounds_of(panel$params))
      fitted$zoom / panel$params$camera$zoom
    }, numeric(1))
    camera$zoom <- panel_params$camera$zoom * min(magnifications)
  }
  cube <- cube_edges_3d()
  basis <- frame_basis_3d(camera, fit$viewport)
  chunks <- list(frame$map(cube$corners)); n_vertices <- 8L
  primitives <- list(); kinds <- character(); materials <- list(); anchor_rows <- integer(); tags <- list()
  # A tag records which panes a primitive belongs to and a scale-space point on
  # it, so a viewer can re-decide visibility for another camera. Scene
  # primitives (zero planes, origin axes) lie inside the box and depth-sort
  # with the data.
  add <- function(xyz, kind, element = NULL, partition = 0L, anchor = FALSE, tag = NULL, text = NULL, scene = FALSE) {
    if (kind == "polylines" && sight_ray_line_3d(xyz, camera, basis)) return(0L)
    rows <- n_vertices + seq_len(nrow(xyz))
    chunks[[length(chunks) + 1L]] <<- xyz
    n_vertices <<- n_vertices + nrow(xyz)
    primitives[[length(primitives) + 1L]] <<- rows
    kinds <<- c(kinds, kind)
    materials[[length(materials) + 1L]] <<- list(element = element, partition = as.integer(partition), anchor = anchor,
      text = text, scene = scene)
    tags[length(primitives)] <<- list(tag)
    if (anchor) anchor_rows <<- c(anchor_rows, length(primitives))
    length(primitives)
  }
  # Empty overlay text carries projection queries through the one panel compilation.
  corner_queries <- vapply(seq_len(8L), function(i) add(chunks[[1L]][i, , drop = FALSE], "text", partition = 2L, anchor = TRUE), integer(1))
  guide_queries <- lapply(panel_params$axis_guides, function(guide) {
    lapply(frame$axis_candidates(guide), function(candidate) {
      values <- c(guide$position, guide$minor)
      if (!length(values)) return(integer())
      model <- frame$map(axis_candidate_points_3d(candidate, values))
      vapply(seq_along(values), function(j) add(model[j, , drop = FALSE], "text", partition = 2L, anchor = TRUE), integer(1))
    })
  })
  cfg <- panel_params$coord_params
  all_panes <- frame$panes()
  pane_names <- vapply(all_panes, `[[`, character(1), "name")
  partitions <- furniture_partitions_3d(camera, fit$viewport, frame)
  explicit <- intersect(setdiff(cfg$panes, c("back", "floor", "front")), pane_names)
  back <- if ("back" %in% cfg$panes) frame$back_panes(partitions) else character()
  panes <- unique(c(explicit, back, if ("floor" %in% cfg$panes) frame$floor_panes()))
  front <- if ("front" %in% cfg$panes) setdiff(pane_names, panes) else character()
  if (frame$linear) front <- front[partitions[front] == 2L]
  rear_only <- stats::setNames(!(panes %in% explicit) & vapply(panes, frame$rear_only, logical(1)), panes)
  pane_facing <- function(pane, points) facing_3d(pane_normals_3d(frame, pane, points, camera), frame$map(points), camera, basis)
  # A stroke on a curved pane splits into runs of constant visibility class.
  add_runs <- function(path, element, class, tag) {
    runs <- rle(ifelse(is.na(class), -1L, class))
    end <- cumsum(runs$lengths); begin <- end - runs$lengths + 1L
    for (r in seq_along(runs$values)) {
      if (runs$values[r] < 0L) next
      rows <- begin[r]:(end[r] + 1L)
      tag$at <- path$scale[rows[ceiling(length(rows) / 2)], ]
      add(path$model[rows, , drop = FALSE], "polylines", element, runs$values[r], tag = tag)
    }
  }
  segment_midpoints <- function(path) {
    n <- nrow(path$scale)
    (path$scale[-1L, , drop = FALSE] + path$scale[-n, , drop = FALSE]) / 2
  }
  # Fill of a scale-space pane; classify gives the partition of points on it,
  # NA where the pane is not drawn.
  fill_pane <- function(pane, element, classify, tagged = TRUE, scene = FALSE) {
    if (!visible_element_3d(element) || is.na(element$fill %or3d% NA) || isFALSE(pane$fill)) return(invisible())
    pane_tag <- function(at) if (tagged) list(type = "pane", panes = pane$name, at = unname(at))
    if (frame$linear) {
      centre <- colMeans(pane$xyz)
      class <- classify(matrix(centre, 1L))
      if (!is.na(class)) add(frame$map(pane$xyz), "faces", element, class, tag = pane_tag(centre), scene = scene)
      return(invisible())
    }
    polygon <- new_scene_packet(vertices = list(x = pane$xyz[, 1L], y = pane$xyz[, 2L], z = pane$xyz[, 3L]),
      corners = list(vertex = seq_len(nrow(pane$xyz)), source_row = rep.int(0L, nrow(pane$xyz))),
      faces = list(offset = 1L, count = nrow(pane$xyz)), styles = data.frame(style_id = 1L))
    munched <- munch_packet_3d(frame, polygon, clip = FALSE)
    tri <- matrix(munched$corners$vertex, nrow = 3L)
    if (!ncol(tri)) return(invisible())
    model <- scene_vertices_xyz_3d(munched$vertices); scale <- attr(munched, "scale")
    centres <- (scale[tri[1L, ], , drop = FALSE] + scale[tri[2L, ], , drop = FALSE] + scale[tri[3L, ], , drop = FALSE]) / 3
    class <- classify(centres)
    for (k in which(!is.na(class))) add(model[tri[, k], , drop = FALSE], "faces", element, class[k], tag = pane_tag(centres[k, ]), scene = scene)
    invisible()
  }
  # The level line of one grid family on a pane.
  pane_line <- function(pane, family, value, element, classify) {
    line <- pane_isoline_3d(pane$xyz, family, value)
    if (is.null(line)) return(invisible())
    if (frame$linear) {
      at <- colMeans(line)
      add(frame$map(line), "polylines", element, classify(matrix(at, 1L)), tag = list(type = "pane", panes = pane$name, at = unname(at)))
    } else {
      path <- polyline_model_3d(frame, line)
      add_runs(path, element, classify(segment_midpoints(path)), list(type = "pane", panes = pane$name, at = NULL))
    }
    invisible()
  }
  for (pane_name in panes) {
    pane <- all_panes[[match(pane_name, pane_names)]]
    classify <- function(points) {
      if (!camera$clip_data) return(rep(1L, nrow(points)))
      if (frame$linear) return(rep(unname(partitions[pane_name]), nrow(points)))
      class <- pane_facing(pane, points)
      if (rear_only[[pane_name]]) class[class != 0L] <- NA_integer_
      class
    }
    fill_pane(pane, element_3d(paste0("panel.pane.", pane$element), theme), classify)
    plane <- pane_grid_plane_3d(pane)
    for (family in pane$families) {
      view <- panel_params[[family$scale]]
      for (level in c("minor", "major")) {
        values <- if (level == "major") view$break_positions() else view$break_positions_minor()
        values <- values[is.finite(values) & values >= 0 & values <= 1]
        element <- element_3d(paste0("panel.grid.", level, ".", family$element, ".", plane), theme)
        if (!visible_element_3d(element)) next
        for (value in values) pane_line(pane, family, value, element, classify)
      }
      element <- element_3d(paste0("panel.zero.line.", family$element), theme)
      if (visible_element_3d(element)) {
        zero <- range_position_3d(panel_params, family$scale, 0)
        if (zero >= 0 && zero <= 1) pane_line(pane, family, zero, element, classify)
      }
    }
  }
  for (pane_name in front) {
    pane <- all_panes[[match(pane_name, pane_names)]]
    classify <- function(points) {
      facing <- if (frame$linear) rep(unname(partitions[pane_name]), nrow(points)) else pane_facing(pane, points)
      ifelse(facing == 2L, if (camera$clip_data) 2L else 1L, NA_integer_)
    }
    fill_pane(pane, element_3d("panel.pane.front", theme), classify, tagged = FALSE)
  }
  for (k in seq_len(3L)) {
    element <- element_3d(paste0("panel.zero.", c("x", "y", "z")[k]), theme)
    if (!visible_element_3d(element)) next
    value <- range_position_3d(panel_params, c("x", "y", "z")[k], 0)
    section <- if (value >= 0 && value <= 1) frame$section(k, value)
    if (is.null(section)) next
    # Pieces cut from the plane by data carry no outline; the boundary is one ring.
    fill_pane(section, list(fill = element$fill), function(points) rep(1L, nrow(points)), tagged = FALSE, scene = TRUE)
    if (!is.na(element$colour %or3d% NA)) {
      ring <- section$xyz[c(seq_len(nrow(section$xyz)), 1L), , drop = FALSE]
      add(if (frame$linear) frame$map(ring) else polyline_model_3d(frame, ring)$model, "polylines", element, 1L, scene = TRUE)
    }
  }
  origin_guides <- Filter(function(guide) guide_at_origin_3d(guide, panel_params), panel_params$axis_guides)
  project <- function(xyz) {
    points <- frame_positions_3d(xyz, camera, basis)
    unname(cbind(points[, 1:2, drop = FALSE] * rep(c(fit$viewport$width_mm, fit$viewport$height_mm), each = nrow(points)), points[, 3L]))
  }
  ctx <- typography_context_3d(frame, camera, fit$viewport, project)
  occupied <- matrix(numeric(), 0L, 4L)
  box_guides <- if (frame_in_front_3d(frame, camera, basis)) names(panel_params$axis_guides) else character()
  for (name in box_guides) {
    guide <- panel_params$axis_guides[[name]]
    if (!guide_on_box_3d(guide, panel_params)) next
    layout <- axis_layout_3d(frame, guide, project, project(frame$bounds()))
    if (is.null(layout)) next
    m <- fit$metrics[[name]]
    typography <- guide_typography_3d(ctx, guide, m, layout)
    if (identical(guide$params$ticks, "in_plane")) {
      for (kind in c("major", "minor")) {
        ticks <- typography[[kind]]
        element <- element_3d(paste0("axis.", if (kind == "minor") "minor.", "ticks.", guide$axis), guide$theme)
        if (is.null(ticks) || !visible_element_3d(element)) next
        for (i in seq_len(nrow(ticks$model_from))) add(rbind(ticks$model_from[i, ], ticks$model_to[i, ]), "polylines", element, 1L, scene = TRUE)
      }
    }
    labels <- typography$labels
    if (!is.null(labels)) {
      thinned <- thin_axis_labels_3d(labels$location, labels$size, layout$tangent, occupied, fit$viewport,
        gap = if (isTRUE(guide$params$check.overlap)) 1 else 0.5, thin = thin_labels_of_3d(guide))
      occupied <- thinned$occupied
      if (labels$kind == "path") for (i in which(thinned$keep)) {
        for (item in plane_text_primitives_3d(ctx, guide$labels[[i]], m$text, labels$centres[i, ], labels$axes[[i]])) {
          add(item$xyz, "faces", item$element, 1L, scene = TRUE)
        }
      }
    }
    title <- typography$title
    if (!is.null(title) && title$kind == "path") {
      for (item in plane_text_primitives_3d(ctx, guide$title, m$title, title$centre, title$axes)) {
        add(item$xyz, "faces", item$element, 1L, scene = TRUE)
      }
    }
  }
  if (length(origin_guides)) {
    origin <- origin_position_3d(panel_params)
    for (name in names(origin_guides)) {
      items <- origin_axis_primitives_3d(ctx, origin_guides[[name]], fit$metrics[[name]], origin,
        line = !identical(cfg$axis_lines, "none"), crossing = length(origin_guides) > 1L)
      for (item in items) add(item$xyz, item$kind, item$element, 1L, text = item$text, scene = TRUE)
    }
  }
  if (cfg$box != "none") {
    back_element <- element_3d("panel.box.back", theme); front_element <- element_3d("panel.box.front", theme)
    for (edge in frame$edges()) {
      if (frame$linear) {
        rear <- all(partitions[edge$incident] == 0L)
        if (!rear && cfg$box != "all") next
        element <- if (rear) back_element else front_element
        if (!visible_element_3d(element)) next
        partition <- if (!camera$clip_data) 1L else if (rear) 0L else 2L
        add(frame$map(edge$xyz), "polylines", element, partition, tag = list(type = "edge", panes = edge$incident, at = colMeans(edge$xyz)))
        next
      }
      incident <- all_panes[match(edge$incident, pane_names)]
      path <- polyline_model_3d(frame, edge$xyz)
      mid <- segment_midpoints(path)
      rear <- Reduce(`&`, lapply(incident, function(pane) pane_facing(pane, mid) == 0L))
      for (side in c(TRUE, FALSE)) {
        if (!side && cfg$box != "all") next
        element <- if (side) back_element else front_element
        if (!visible_element_3d(element)) next
        class <- ifelse(rear == side, if (!camera$clip_data) 1L else if (side) 0L else 2L, NA_integer_)
        add_runs(path, element, class, list(type = "edge", panes = edge$incident))
      }
    }
  }
  styles <- furniture_styles_3d(kinds, materials)
  vertices <- do.call(rbind, chunks)
  assemble <- function(selected) furniture_packet_3d(vertices, primitives, kinds, materials, styles, panel_params$panel_id, selected)
  packet <- assemble(seq_along(primitives))
  # The raster backend draws furniture outside its depth buffer, so scene
  # primitives travel to it as their own packet.
  scene_rows <- which(vapply(materials, function(m) isTRUE(m$scene), logical(1)))
  split <- if (length(scene_rows)) list(fixed = assemble(setdiff(seq_along(primitives), scene_rows)), scene = assemble(scene_rows))
  tags[length(primitives) + 1L] <- list(NULL)
  list(packet = packet, split = split, camera = camera, overlay = list(frame = frame, corners = corner_queries, guides = guide_queries,
    params = panel_params, fit = fit), viewport = fit$viewport, tags = tags[seq_along(primitives)])
}

# Style rows of furniture primitives, one per primitive, with the opacity of
# the colour that paints each kind as attribute.
furniture_styles_3d <- function(kinds, materials) {
  n <- length(kinds)
  styles <- data.frame(style_id = seq_len(n), colour = rep(NA_character_, n), fill = rep(NA_character_, n),
    linewidth = rep(0, n), linetype = rep("solid", n), label = rep("", n), fontsize = rep(0, n),
    family = rep("", n), fontface = rep(1, n), .anchor = rep(FALSE, n))
  opacity <- rep(1, n)
  for (i in seq_len(n)) {
    m <- materials[[i]]; el <- m$element
    styles$.anchor[i] <- m$anchor
    if (is.null(el)) next
    styles$colour[i] <- el$colour %or3d% NA_character_
    styles$fill[i] <- el$fill %or3d% NA_character_
    styles$linewidth[i] <- el$linewidth %or3d% 0
    styles$linetype[i] <- as.character(el$linetype %or3d% 1)
    if (!is.null(m$text)) {
      styles$label[i] <- m$text$label
      styles$fontsize[i] <- el$size %or3d% 11
      styles$family[i] <- el$family %or3d% ""
      face <- el$face %or3d% 1
      styles$fontface[i] <- if (is.numeric(face)) face else match(face, c("plain", "bold", "italic", "bold.italic"), nomatch = 1L)
    }
    chosen <- if (kinds[i] == "faces") styles$fill[i] else styles$colour[i]
    rgba <- grDevices::col2rgb(chosen, alpha = TRUE)
    opacity[i] <- rgba[4L, 1L] / 255
    for (key in c("colour", "fill")) if (!is.na(styles[[key]][i])) {
      rgb <- grDevices::col2rgb(styles[[key]][i])
      styles[[key]][i] <- grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
    }
  }
  attr(styles, "opacity") <- opacity
  styles
}

# Layer of the scene furniture, distinct from every plot layer, so that the
# pieces of a zero plane cut by data form seam groups of their own.
scene_furniture_layer_3d <- .Machine$integer.max

# A scene packet of the selected furniture primitives. Primitive, style and
# source row identifiers stay those of the whole furniture.
furniture_packet_3d <- function(vertices, primitives, kinds, materials, styles, panel_id, selected) {
  opacity <- attr(styles, "opacity"); attr(styles, "opacity") <- NULL
  counts <- lengths(primitives)[selected]
  offsets <- cumsum(c(1L, head(counts, -1L)))
  text_defaults <- c(hjust = 0.5, vjust = 0.5, angle = 0)
  tables <- lapply(c("faces", "polylines", "text"), function(kind) {
    j <- which(kinds[selected] == kind); i <- selected[j]
    tab <- list(primitive_id = i, offset = offsets[j], count = counts[j], style_id = i,
      layer_id = as.integer(ifelse(vapply(materials[i], function(m) isTRUE(m$scene), logical(1)), scene_furniture_layer_3d, 0L)),
      source_row = as.integer(i), partition = vapply(materials[i], `[[`, integer(1), "partition"), opacity = opacity[i])
    if (kind == "polylines") tab$width_mm <- styles$linewidth[i]
    if (kind == "text") for (key in names(text_defaults)) {
      tab[[key]] <- vapply(materials[i], function(m) as.double(m$text[[key]] %or3d% text_defaults[[key]]), numeric(1))
    }
    tab
  }); names(tables) <- c("faces", "polylines", "text")
  geometry <- lapply(as.data.frame(vertices), as.double); names(geometry) <- c("x", "y", "z")
  packet <- do.call(new_scene_packet, c(list(vertices = geometry,
    corners = list(vertex = as.integer(unlist(primitives[selected])), source_row = rep.int(0L, sum(counts))), styles = styles,
    panel_id = panel_id), tables))
  packet
}

# Normalised panel position of a transformed value on one axis.
range_position_3d <- function(panel_params, axis, value) {
  range <- panel_params$ranges[[axis]]
  (value - range[1L]) / diff(range)
}

# The origin of coord_3d() in normalised scale space, moved into the box.
origin_position_3d <- function(panel_params) {
  origin <- panel_params$coord_params$origin %or3d% c(x = 0, y = 0, z = 0)
  u <- vapply(c("x", "y", "z"), function(axis) {
    view <- panel_params[[axis]]
    value <- if (isTRUE(view$is_discrete())) origin[[axis]] else view$get_transformation()$transform(origin[[axis]])
    range_position_3d(panel_params, axis, value)
  }, numeric(1))
  u[is.nan(u)] <- 0
  pmin(pmax(u, 0), 1)
}

# Model and screen derivatives of the frame map along a scale-space direction,
# by differences kept inside the unit cube.
frame_derivative_3d <- function(frame, points, direction, project, step = 1e-4) {
  d <- matrix(direction, nrow(points), 3L, byrow = TRUE)
  plus <- pmin(pmax(points + step * d, 0), 1)
  minus <- pmin(pmax(points - step * d, 0), 1)
  span <- rowSums((plus - minus) * d) / sum(direction^2)
  a <- frame$map(plus); b <- frame$map(minus)
  list(model = (a - b) / span, screen = (project(a) - project(b)) / span)
}

label_text_3d <- function(label) if (is.language(label)) deparse1(label) else as.character(label)

# Justification that places a label of the given size (mm) beyond its anchor
# in the screen direction `screen`, its nearest point `offset` mm away. The
# label is turned by `angle` degrees, so the direction is read in its own
# frame. The anchor stays on the axis, inside the data box that clips the scene.
origin_text_3d <- function(label, screen, size, offset, angle) {
  screen <- screen / sqrt(sum(screen^2))
  a <- angle * pi / 180
  local <- c(cos(a) * screen[1L] + sin(a) * screen[2L], -sin(a) * screen[1L] + cos(a) * screen[2L])
  side <- local / max(abs(local))
  size <- pmax(size, 1e-6)
  list(label = label_text_3d(label), hjust = (1 - side[1L]) / 2 - offset * local[1L] / size[1L],
    vjust = (1 - side[2L]) / 2 - offset * local[2L] / size[2L], angle = angle)
}

# Typography in three dimensions --------------------------------------------

# Projection context of axis typography: the frame, project() from model
# points to page millimetres and eye distance, the aspect scale, the camera
# basis, the eye distance of the camera target, and units(), aspect-scaled
# model units per millimetre at the target, computed once when first needed.
typography_context_3d <- function(frame, camera, viewport, project) {
  units <- NULL
  list(frame = frame, project = project, camera = camera, basis = frame_basis_3d(camera, viewport),
    scale = camera$aspect / max(camera$aspect),
    reference = project(matrix(camera$target, 1L))[1L, 3L],
    units = function() {
      if (is.null(units)) units <<- model_units_per_mm_3d(camera, viewport)
      units
    })
}

# Size factor of text and ticks at eye distances `depth`: the ratio of the
# target's distance to theirs, raised to the strength, under perspective.
depth_factor_3d <- function(ctx, depth, strength) {
  if (!isTRUE(strength > 0) || !ctx$basis$perspective) return(rep(1, length(depth)))
  (ctx$reference / depth)^strength
}

# Unit aspect-scaled model directions of a scale-space direction at points,
# through the frame's Jacobian.
frame_directions_3d <- function(frame, points, direction, scale) {
  d <- sweep(frame_derivative_3d(frame, points, direction, identity)$model, 2L, scale, "*")
  d / sqrt(rowSums(d^2))
}

# Model points of text-local millimetres (x, y) placed at a model centre, with
# u the reading direction and v up, both unit aspect-scaled vectors.
plane_points_3d <- function(ctx, xy, centre, u, v) {
  offset <- (outer(xy[, 1L], u) + outer(xy[, 2L], v)) * ctx$units()
  sweep(sweep(offset, 2L, ctx$scale, "/"), 2L, centre, "+")
}

# Aspect-scaled direction from model points toward the eye, one row each;
# the reversed sight ray under parallel projection.
toward_camera_3d <- function(basis, camera, points) {
  if (!basis$perspective) return(matrix(-basis$ray, nrow(points), 3L, byrow = TRUE))
  position <- sweep(points, 2L, basis$scale, "*")
  -sweep(sweep(position, 2L, camera$target * basis$scale), 2L, basis$forward * basis$distance, "+")
}

# Reading direction for text lying in the plane spanned by the orthogonal
# unit vectors a and b at the model point p. Among `choices` ("a", "b", "-a",
# "-b") the reading direction u is the one that runs most nearly left to
# right on the page with v = n x u most nearly up, n being the plane normal
# turned toward the camera, so the text is never mirrored. Returns u as
# coefficients on (a, b) and the sign s of n on a x b.
reading_axes_3d <- function(ctx, p, a, b, choices) {
  n <- cross_rows_3d(matrix(a, 1L), matrix(b, 1L))[1L, ]
  s <- if (sum(n * toward_camera_3d(ctx$basis, ctx$camera, matrix(p, 1L))) < 0) -1 else 1
  coefficients <- list(a = c(1, 0), b = c(0, 1), "-a" = c(-1, 0), "-b" = c(0, -1))
  page <- function(direction) {
    ends <- ctx$project(rbind(p, p + 1e-3 * direction / ctx$scale))
    d <- ends[2L, 1:2] - ends[1L, 1:2]
    d / sqrt(sum(d^2))
  }
  scores <- vapply(choices, function(choice) {
    axes <- reading_choice_3d(list(u = coefficients[[choice]], s = s), a, b)
    score <- page(axes$u)[1L] + page(axes$v)[2L]
    if (is.finite(score)) score else -Inf
  }, numeric(1))
  list(u = coefficients[[choices[which.max(scores)]]], s = s)
}

# Text axes of a reading choice in the plane of a and b: u, v = s n x u, and
# their coefficients on a and b.
reading_choice_3d <- function(choice, a, b) {
  u <- choice$u; s <- choice$s
  v <- s * c(-u[2L], u[1L])
  list(u = u[1L] * a + u[2L] * b, v = v[1L] * a + v[2L] * b, u_a = u[1L], u_b = u[2L], v_a = v[1L], v_b = v[2L])
}

# Width and height in millimetres of a label's text box.
text_outline_size_3d <- function(label, element) {
  text_outline_3d(label, element$size, element$family %or3d% "", element$face %or3d% "plain")$size
}

scaled_text_element_3d <- function(element, factor) {
  if (factor != 1) element$size <- element$size * factor
  element
}

plane_text_primitives_3d <- function(ctx, label, element, centre, axes) {
  mesh <- text_mesh_3d(label, element$size, element$family %or3d% "", element$face %or3d% "plain")
  xyz <- plane_points_3d(ctx, mesh$vertices[, 1:2, drop = FALSE], centre, axes$u, axes$v)
  unlist(lapply(mesh$faces, function(faces) lapply(seq_len(nrow(faces)), function(i) {
    list(xyz = xyz[faces[i, ], , drop = FALSE], element = list(fill = element$colour))
  })), recursive = FALSE)
}

# Scale-space axes, other than the axis `k`, that span the requested pane
# with it: all for "auto", the one normal to the vertical axis for "floor",
# the partner named by "xy", "xz" or "yz".
pane_partners_3d <- function(k, plane, up) {
  others <- setdiff(seq_len(3L), k)
  axis <- c("x", "y", "z")[k]
  if (identical(plane, "auto")) return(others)
  if (identical(plane, "floor")) {
    if (k == up) stop("The ", axis, " axis is vertical and does not lie in the floor; choose a plane containing it.", call. = FALSE)
    return(setdiff(others, up))
  }
  spanned <- match(strsplit(plane, "", fixed = TRUE)[[1L]], c("x", "y", "z"))
  if (!k %in% spanned) stop("plane = '", plane, "' does not contain the ", axis, " axis.", call. = FALSE)
  setdiff(spanned, k)
}

# Scene primitives of an axis through the origin: the line; ticks along the
# coordinate direction drawn most nearly perpendicular to the axis on screen,
# toward the bottom (the left for a vertical axis) unless that leaves the box,
# within the guide's pane when one is named; tick labels beyond the ticks; the
# title past the upper end, or past the end nearer the camera. Labels and
# title in the pane are glyph faces lying in it. ctx holds the
# frame and the projection to millimetres in the scene viewport.
origin_axis_primitives_3d <- function(ctx, guide, metrics, origin, line = TRUE, crossing = FALSE) {
  frame <- ctx$frame; project <- ctx$project; params <- guide$params
  orientation <- params$label.orientation %or3d% "screen"
  in_plane <- identical(params$ticks, "in_plane")
  k <- match(guide$axis, c("x", "y", "z"))
  path <- frame$origin_axis(k, origin)
  out <- list()
  push <- function(xyz, kind, element, text = NULL) out[[length(out) + 1L]] <<- list(xyz = xyz, kind = kind, element = element, text = text)
  if (line) {
    element <- element_3d(paste0("axis.line.origin.", guide$axis), guide$theme)
    ends <- rbind(path$start, path$end)
    if (visible_element_3d(element)) push(if (frame$linear) frame$map(ends) else polyline_model_3d(frame, ends)$model, "polylines", element)
  }
  along <- path$end - path$start
  middle <- axis_candidate_points_3d(path, 0.5)
  tangent <- frame_derivative_3d(frame, middle, along, project)$screen[1L, 1:2]
  if (!all(is.finite(tangent)) || sqrt(sum(tangent^2)) < 1e-9) return(out)
  normal <- c(-tangent[2L], tangent[1L]) / sqrt(sum(tangent^2))
  if (normal[2L] > 1e-9 || (abs(normal[2L]) <= 1e-9 && normal[1L] > 0)) normal <- -normal
  options <- expand.grid(axis = pane_partners_3d(k, params$plane %or3d% "auto", frame$up), sign = c(1, -1))
  options$score <- mapply(function(j, s) {
    screen <- frame_derivative_3d(frame, middle, replace(numeric(3), j, s), project)$screen[1L, 1:2]
    size <- sqrt(sum(screen^2))
    if (!is.finite(size) || size < 1e-9) NA_real_ else sum(screen * normal) / size
  }, options$axis, options$sign)
  inside <- ifelse(options$sign > 0, origin[options$axis] < 1, origin[options$axis] > 0)
  options <- options[!is.na(options$score) & abs(options$score) > 1e-9 & inside, , drop = FALSE]
  if (!nrow(options)) return(out)
  best <- options[which.max(options$score), ]
  direction <- replace(numeric(3), best$axis, best$sign)
  strength <- params$depth.scale %or3d% 0
  # Base points on the axis, unit page directions toward `toward`, model
  # offsets per millimetre (on the page, or in the pane), and depth factors.
  outward <- function(values, toward, pane = FALSE) {
    points <- axis_candidate_points_3d(path, values)
    d <- frame_derivative_3d(frame, points, toward, project)
    screen <- d$screen[, 1:2, drop = FALSE]
    size <- sqrt(rowSums(screen^2))
    base <- frame$map(points)
    per_mm <- if (pane) sweep(frame_directions_3d(ctx$frame, points, toward, ctx$scale) * ctx$units(), 2L, ctx$scale, "/") else d$model / size
    list(points = points, base = base, per_mm = per_mm, screen = screen / size,
      factor = if (pane) rep(1, length(values)) else depth_factor_3d(ctx, project(base)[, 3L], strength))
  }
  ticks <- function(values, element, length) {
    if (!length(values) || !visible_element_3d(element) || length <= 0) return(invisible())
    o <- outward(values, direction, in_plane)
    tip <- o$base + o$per_mm * length * o$factor
    for (i in seq_along(values)) push(rbind(o$base[i, ], tip[i, ]), "polylines", element)
  }
  major <- element_3d(paste0("axis.ticks.", guide$axis), guide$theme)
  ticks(guide$position, major, metrics$tick_length)
  if (isTRUE(params$minor.ticks)) {
    ticks(guide$minor, element_3d(paste0("axis.minor.ticks.", guide$axis), guide$theme) %or3d% major, metrics$tick_length / 2)
  }
  edge_angle <- upright_angle_3d(tangent[1L], tangent[2L])
  # Glyph faces of text lying in the pane, centred at the model point
  # `centre`, read along axes$u with axes$v up.
  plane_text <- function(label, element, centre, axes) {
    for (item in plane_text_primitives_3d(ctx, label, element, centre, axes)) push(item$xyz, "faces", item$element)
  }
  labelled <- seq_along(guide$position)
  if (crossing) labelled <- labelled[abs(guide$position[labelled] - origin[[k]]) > 1e-9]
  if (length(labelled) && visible_element_3d(metrics$text)) {
    o <- outward(guide$position[labelled], direction, in_plane)
    if (orientation == "plane") {
      a <- frame_directions_3d(ctx$frame, o$points, along, ctx$scale); b <- frame_directions_3d(ctx$frame, o$points, direction, ctx$scale)
      choice <- reading_axes_3d(ctx, o$base[1L, ], a[1L, ], b[1L, ], c("a", "b", "-a", "-b"))
      for (j in seq_along(labelled)) {
        axes <- reading_choice_3d(choice, a[j, ], b[j, ])
        label <- guide$labels[[labelled[j]]]
        size <- text_outline_size_3d(label, metrics$text)
        depth <- abs(axes$u_b) * size[1L] + abs(axes$v_b) * size[2L]
        centre <- o$base[j, ] + b[j, ] * ctx$units() * (metrics$tick_length + metrics$gap + depth / 2) / ctx$scale
        plane_text(label, metrics$text, centre, axes)
      }
    } else {
      angle <- if (orientation == "edge") edge_angle else metrics$angle
      for (j in seq_along(labelled)) {
        anchor <- o$base[j, , drop = FALSE]
        offset <- metrics$gap
        if (in_plane) anchor <- anchor + o$per_mm[j, , drop = FALSE] * metrics$tick_length else offset <- offset + metrics$tick_length * o$factor[j]
        push(anchor, "text", scaled_text_element_3d(metrics$text, o$factor[j]), origin_text_3d(guide$labels[[labelled[j]]], o$screen[j, ],
          metrics$label[labelled[j], ] * o$factor[j], offset, angle))
      }
    }
  }
  if (length(guide$title) && visible_element_3d(metrics$title)) {
    near <- identical(params$title.position, "near")
    end <- 0.5
    if (near) {
      depths <- project(frame$map(axis_candidate_points_3d(path, c(0, 1))))[, 3L]
      end <- if (depths[1L] < depths[2L]) 0 else 1
    }
    toward <- if (!near) direction else if (end == 1) along else -along
    o <- outward(end, toward)
    if (orientation == "plane") {
      a <- frame_directions_3d(ctx$frame, o$points, along, ctx$scale); b <- frame_directions_3d(ctx$frame, o$points, direction, ctx$scale)
      axes <- reading_choice_3d(reading_axes_3d(ctx, o$base[1L, ], a[1L, ], b[1L, ], c("a", "-a")), a[1L, ], b[1L, ])
      size <- text_outline_size_3d(guide$title, metrics$title)
      along_depth <- abs(axes$u_a) * size[1L] + abs(axes$v_a) * size[2L]
      across_depth <- abs(axes$u_b) * size[1L] + abs(axes$v_b) * size[2L]
      band <- if (length(labelled)) max(vapply(guide$labels[labelled], function(label) {
        size <- text_outline_size_3d(label, metrics$text)
        abs(axes$u_b) * size[1L] + abs(axes$v_b) * size[2L]
      }, numeric(1))) else 0
      offset <- if (near) {
        -sign(end - 0.5) * a[1L, ] * (metrics$title_gap + along_depth / 2) + b[1L, ] * (metrics$title_gap + across_depth / 2)
      } else b[1L, ] * (metrics$tick_length + metrics$gap + band + metrics$title_gap + across_depth / 2)
      plane_text(guide$title, metrics$title, o$base[1L, ] + offset * ctx$units() / ctx$scale, axes)
    } else {
      gap <- metrics$title_gap
      if (!near) gap <- gap + metrics$tick_length + metrics$gap + metrics$label_extent
      push(o$base, "text", metrics$title, origin_text_3d(guide$title, o$screen[1L, ], metrics$title_size[1L, ], gap, edge_angle))
    }
  }
  out
}

# Intersection of a convex scale-space pane polygon with the level set
# coef . u + offset = value, as a segment ordered lexicographically.
pane_isoline_3d <- function(xyz, family, value) {
  f <- drop(xyz %*% family$coef) + family$offset - value
  n <- nrow(xyz)
  points <- xyz[f == 0, , drop = FALSE]
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    if (f[i] * f[j] < 0) {
      t <- f[i] / (f[i] - f[j])
      points <- rbind(points, xyz[i, ] + t * (xyz[j, ] - xyz[i, ]))
    }
  }
  axis <- which(family$coef != 0)
  if (length(axis) == 1L && nrow(points)) points[, axis] <- (value - family$offset) / family$coef[axis]
  points <- unique(points)
  if (nrow(points) < 2L) return(NULL)
  points <- points[do.call(order, unname(as.data.frame(points))), , drop = FALSE]
  if (nrow(points) > 2L) points <- points[c(1L, nrow(points)), , drop = FALSE]
  rownames(points) <- NULL
  points
}

axis_candidate_points_3d <- function(candidate, values) {
  start <- matrix(candidate$start, length(values), 3L, byrow = TRUE)
  start + values %o% (candidate$end - candidate$start)
}

select_axis_path_3d <- function(points, paths, position = "auto", secondary = FALSE, auto = "lower") {
  centre <- colMeans(points[, 1:2, drop = FALSE])
  candidates <- lapply(seq_along(paths), function(i) {
    path <- paths[[i]]; n <- nrow(path)
    a <- path[1L, 1:2]; b <- path[n, 1:2]
    d <- b - a; length <- sqrt(sum(d^2))
    if (!is.finite(length) || length < 1e-7) {
      if (n <= 2L) return(NULL)
      spread <- apply(path[, 1:2, drop = FALSE], 2L, function(v) diff(range(v)))
      if (!all(is.finite(spread)) || max(spread) < 1e-7) return(NULL)
      midpoint <- colMeans(path[, 1:2, drop = FALSE])
      return(c(edge = i, nx = 0, ny = -1, exterior = 0, length = max(spread),
        distance = centre[2L] - min(path[, 2L]), x = midpoint[1L], y = min(path[, 2L]), depth = mean(path[, 3L])))
    }
    normal <- c(-d[2L], d[1L]) / length
    midpoint <- (a + b) / 2
    if (sum(normal * (midpoint - centre)) < 0) normal <- -normal
    interior <- drop(sweep(points[, 1:2, drop = FALSE], 2L, a) %*% normal)
    exterior <- max(interior) <= 1e-6
    c(edge = i, nx = normal[1L], ny = normal[2L], exterior = exterior,
      length = length, distance = sum(normal * (midpoint - centre)),
      x = midpoint[1L], y = midpoint[2L], depth = mean(path[c(1L, n), 3L]))
  })
  valid <- which(!vapply(candidates, is.null, logical(1)))
  if (!length(valid)) return(NULL)
  scores <- do.call(rbind, candidates[valid])
  if (is.numeric(position) || position %in% as.character(1:4)) chosen <- match(as.integer(position), scores[, "edge"])
  else {
    # Automatic placement keeps the horizontal axes on the outer edges of the
    # floor, the lower of each pair of exterior edges on screen, and the z
    # axis on the vertical silhouette edge farthest from the centre.
    preferred <- if (identical(auto, "lower")) -scores[, "y"] else scores[, "distance"]
    score <- switch(position, top = scores[, "y"], bottom = -scores[, "y"], left = -scores[, "x"], right = scores[, "x"], preferred)
    if (secondary) {
      primary <- select_axis_path_3d(points, paths, position, FALSE, auto)
      score <- (scores[, "x"] - primary["x"])^2 + (scores[, "y"] - primary["y"])^2
    }
    chosen <- order(-scores[, "exterior"], -round(score, 8L), round(scores[, "depth"], 12L), scores[, "edge"])[1L]
  }
  if (is.na(chosen)) return(NULL)
  scores[chosen, ]
}

# Depth of each text box, turned by `angle` degrees, along the page direction
# `normal` (one direction or one per row): the length of its projection.
label_depths_3d <- function(label, normal, angle = 0) {
  if (!nrow(label)) return(numeric())
  if (!is.matrix(normal)) normal <- matrix(normal, nrow(label), 2L, byrow = TRUE)
  a <- rep_len(angle, nrow(label)) * pi / 180
  label[, 1L] * abs(cos(a) * normal[, 1L] + sin(a) * normal[, 2L]) +
    label[, 2L] * abs(-sin(a) * normal[, 1L] + cos(a) * normal[, 2L])
}

# Labels stand in n.dodge rows. Every row is as deep as the deepest label along
# the normal, and the inner edge of each label touches the inner edge of its row.
axis_label_rows_3d <- function(label, normal, metrics, dodge, angle = 0) {
  depth <- label_depths_3d(label, normal, angle)
  row_depth <- if (length(depth)) max(depth) else 0
  step <- row_depth + metrics$gap / 2
  list(depth = depth, step = step, band = if (length(depth)) dodge * step - metrics$gap / 2 else 0)
}

# Centres of labels of sizes `label`, turned by `angle`, beyond anchors
# `major` along `normal`; `start` is the distance of each label row from its
# anchor before the gap, the tick length for ticks on the page.
axis_label_positions_3d <- function(major, normal, metrics, dodge, label = metrics$label, angle = 0,
                                    start = metrics$tick_length) {
  rows <- axis_label_rows_3d(label, normal, metrics, dodge, angle)
  offset <- start + metrics$gap + rows$depth / 2 + ((seq_len(nrow(major)) - 1L) %% dodge) * rows$step
  if (is.matrix(normal)) major + offset * normal else major + outer(offset, normal)
}

# The title beside the edge from a to b, beyond `reach` millimetres along
# the normal (by default the tick, gap and label band), turned along the edge.
axis_title_geometry_3d <- function(a, b, normal, metrics, band, reach = metrics$tick_length + metrics$gap + band) {
  size <- metrics$title_size[1L, ]
  location <- (a + b) / 2 + normal * (reach + metrics$title_gap + size[2L] / 2)
  angle <- upright_angle_3d(b[1L] - a[1L], b[2L] - a[2L])
  text_box_3d(location, size, angle)
}

text_box_3d <- function(location, size, angle) {
  extent <- rotated_extent_3d(size, angle)[1L, ]
  list(location = location, angle = angle, bounds = c(location - extent / 2, location + extent / 2))
}

# Screen geometry of one axis guide: the chosen candidate, tick anchors,
# outward normals and tangents on the page, and title placement. project()
# maps model points to screen millimetres and depth; centre holds projected
# frame bounds.
axis_layout_3d <- function(frame, guide, project, centre) {
  candidates <- frame$axis_candidates(guide)
  if (!length(candidates)) return(NULL)
  paths <- lapply(candidates, function(candidate) {
    xyz <- rbind(candidate$start, candidate$end)
    project(if (frame$linear) frame$map(xyz) else polyline_model_3d(frame, xyz)$model)
  })
  chosen <- select_axis_path_3d(centre, paths, guide$params$position, guide$secondary, attr(candidates, "auto") %or3d% "lower")
  if (is.null(chosen)) return(NULL)
  candidate <- candidates[[chosen[["edge"]]]]; path <- paths[[chosen[["edge"]]]]
  at <- function(values) {
    if (!length(values)) return(matrix(numeric(), 0L, 3L))
    project(frame$map(axis_candidate_points_3d(candidate, values)))
  }
  straight <- nrow(path) == 2L
  normal <- unname(chosen[c("nx", "ny")])
  a <- path[1L, 1:2]; b <- path[nrow(path), 1:2]
  closed <- !straight && sqrt(sum((b - a)^2)) < 1e-7
  middle <- colMeans(centre[, 1:2, drop = FALSE])
  tangent <- if (straight) (b - a) / sqrt(sum((b - a)^2)) else NULL
  # Unit page tangents in the direction of increasing values.
  tangents_at <- function(values) {
    if (straight) return(matrix(tangent, length(values), 2L, byrow = TRUE))
    delta <- 1e-4
    d <- at(values + delta)[, 1:2, drop = FALSE] - at(values - delta)[, 1:2, drop = FALSE]
    d / sqrt(rowSums(d^2))
  }
  normals_at <- function(values) {
    if (straight || !length(values)) return(normal)
    t <- tangents_at(values)
    n <- cbind(-t[, 2L], t[, 1L])
    away <- sweep(at(values)[, 1:2, drop = FALSE], 2L, middle)
    flip <- rowSums(n * away) < 0
    n[flip, ] <- -n[flip, ]
    n
  }
  title <- candidate$title %or3d% list(type = "edge")
  title_args <- if (identical(title$type, "vertex")) {
    p <- project(frame$map(matrix(title$at, 1L)))[1L, 1:2]
    away <- p - middle
    list(a = p - c(1e-3, 0), b = p + c(1e-3, 0), normal = away / sqrt(sum(away^2)))
  } else if (closed) {
    low <- c(mean(range(path[, 1L])), min(path[, 2L], centre[, 2L]))
    list(a = low - c(1, 0), b = low + c(1, 0), normal = c(0, -1))
  } else if (!straight) {
    # The title sits beyond the side toward which the curve bows.
    offsets <- drop(sweep(path[, 1:2, drop = FALSE], 2L, a) %*% normal)
    side <- if (abs(max(offsets)) >= abs(min(offsets))) 1 else -1
    bulge <- max(0, side * offsets)
    list(a = a + side * normal * bulge, b = b + side * normal * bulge, normal = side * normal)
  } else list(a = a, b = b, normal = normal)
  list(candidate = candidate, index = chosen[["edge"]], path = path, straight = straight,
    a = a, b = b, normal = normal, at = at, normals_at = normals_at, tangents_at = tangents_at,
    tangent = tangent, title = title_args)
}

tick_ends_3d <- function(points, normals, length) {
  if (is.matrix(normals)) points + normals * length else points + outer(length * rep_len(1, nrow(points)), normals)
}

rows_of_3d <- function(normal, n) if (is.matrix(normal)) normal else matrix(rep_len(normal, 2L * n), n, 2L, byrow = TRUE)

# The scale-space direction entering the box inside the pane of a box axis
# that plane typography and in-plane ticks use: along a coordinate held at
# 0 (positive) or 1 (negative) on the axis edge. With plane = "auto" the pane
# in which the direction stands out furthest from the edge on the page.
guide_pane_direction_3d <- function(ctx, guide, layout) {
  k <- match(guide$axis, c("x", "y", "z"))
  candidate <- layout$candidate
  partners <- pane_partners_3d(k, guide$params$plane %or3d% "auto", ctx$frame$up)
  side <- vapply(partners, function(j) {
    if (abs(candidate$start[j] - candidate$end[j]) > 1e-12) return(0)
    -sign(candidate$start[j] - 0.5)
  }, numeric(1))
  partners <- partners[side != 0]; side <- side[side != 0]
  if (!length(partners)) stop("The ", guide$axis, " axis does not run along an edge of the requested pane; plane labels and in-plane ticks need one.", call. = FALSE)
  directions <- lapply(seq_along(partners), function(i) replace(numeric(3), partners[i], side[i]))
  if (length(directions) == 1L) return(directions[[1L]])
  middle <- axis_candidate_points_3d(candidate, 0.5)
  normal <- rows_of_3d(layout$normals_at(0.5), 1L)[1L, ]
  score <- vapply(directions, function(d) {
    page <- frame_derivative_3d(ctx$frame, middle, d, ctx$project)$screen[1L, 1:2]
    value <- abs(sum(page * normal))
    if (is.finite(value)) value else -Inf
  }, numeric(1))
  directions[[which.max(score)]]
}

# Text lying in a pane, as page paths: label outlines centred at model
# points, read along axes$u with axes$v up, projected.
plane_label_paths_3d <- function(ctx, outline, centre, axes) {
  if (!length(outline$x)) {
    p <- ctx$project(matrix(centre, 1L))[1L, 1:2]
    return(list(x = numeric(), y = numeric(), id = integer(), box = c(p, p)))
  }
  xy <- ctx$project(plane_points_3d(ctx, cbind(outline$x, outline$y), centre, axes$u, axes$v))[, 1:2, drop = FALSE]
  list(x = xy[, 1L], y = xy[, 2L], id = outline$id, box = c(apply(xy, 2L, min), apply(xy, 2L, max)))
}

# Page geometry of the ticks, tick labels and title of a box axis in its
# typography: ticks on the page or in the pane; labels upright, along the
# edge or lying in the pane; the title beside the middle or past the near
# end. Everything in page millimetres; labels carry their bounding boxes.
guide_typography_3d <- function(ctx, guide, m, layout) {
  params <- guide$params
  orientation <- params$label.orientation %or3d% "screen"
  in_plane <- identical(params$ticks, "in_plane")
  strength <- params$depth.scale %or3d% 0
  pane <- if (in_plane || orientation == "plane") guide_pane_direction_3d(ctx, guide, layout)
  along <- layout$candidate$end - layout$candidate$start
  tick_shown <- visible_element_3d(element_3d(paste0("axis.ticks.", guide$axis), guide$theme))
  # Ticks at scale values: page start and end points, with depth factors.
  ticks_at <- function(values, length) {
    at <- layout$at(values)
    from <- at[, 1:2, drop = FALSE]
    if (in_plane) {
      points <- axis_candidate_points_3d(layout$candidate, values)
      base <- ctx$frame$map(points)
      tip <- base + sweep(frame_directions_3d(ctx$frame, points, pane, ctx$scale) * ctx$units() * length, 2L, ctx$scale, "/")
      return(list(from = from, to = ctx$project(tip)[, 1:2, drop = FALSE], model_from = base, model_to = tip, factor = rep(1, length(values))))
    }
    factor <- depth_factor_3d(ctx, at[, 3L], strength)
    list(from = from, to = tick_ends_3d(from, rows_of_3d(layout$normals_at(values), length(values)), length * factor), factor = factor)
  }
  n <- length(guide$position)
  major <- if (n) ticks_at(guide$position, m$tick_length)
  minor <- if (isTRUE(params$minor.ticks) && length(guide$minor)) ticks_at(guide$minor, m$tick_length / 2)
  normal <- rows_of_3d(layout$normals_at(guide$position), n)
  reach <- if (n && tick_shown) max(0, rowSums((major$to - major$from) * normal)) else 0
  band <- 0; labels <- NULL; plane_frame <- NULL
  # Unit model axes along the edge (a) and into the pane (b) at scale values.
  pane_frame <- function(values) {
    points <- axis_candidate_points_3d(layout$candidate, values)
    list(base = ctx$frame$map(points), a = frame_directions_3d(ctx$frame, points, along, ctx$scale), b = frame_directions_3d(ctx$frame, points, pane, ctx$scale))
  }
  start_mm <- if (tick_shown) m$tick_length else 0
  if (n && visible_element_3d(m$text)) {
    if (orientation == "plane") {
      f <- pane_frame(guide$position)
      middle <- ceiling(n / 2)
      choice <- reading_axes_3d(ctx, f$base[middle, ], f$a[middle, ], f$b[middle, ], c("a", "b", "-a", "-b"))
      outlines <- lapply(guide$labels, function(label) text_outline_3d(label, m$text$size, m$text$family %or3d% "", m$text$face %or3d% "plain"))
      size <- do.call(rbind, lapply(outlines, `[[`, "size"))
      depth <- abs(choice$u[2L]) * size[, 1L] + abs(choice$u[1L]) * size[, 2L]
      step <- max(depth) + m$gap / 2
      offset <- start_mm + m$gap + depth / 2 + ((seq_len(n) - 1L) %% params$n.dodge) * step
      band <- params$n.dodge * step - m$gap / 2
      axes <- lapply(seq_len(n), function(i) reading_choice_3d(choice, f$a[i, ], f$b[i, ]))
      centres <- f$base + sweep(f$b * ctx$units() * offset, 2L, ctx$scale, "/")
      paths <- lapply(seq_len(n), function(i) plane_label_paths_3d(ctx, outlines[[i]], centres[i, ], axes[[i]]))
      boxes <- do.call(rbind, lapply(paths, `[[`, "box"))
      labels <- list(kind = "path", paths = paths, centres = centres, axes = axes, location = (boxes[, 1:2, drop = FALSE] + boxes[, 3:4, drop = FALSE]) / 2,
        size = boxes[, 3:4, drop = FALSE] - boxes[, 1:2, drop = FALSE])
      plane_frame <- list(choice = choice)
    } else {
      angle <- if (orientation == "edge") {
        t <- layout$tangents_at(guide$position)
        upright_angle_3d(t[, 1L], t[, 2L])
      } else rep(m$angle, n)
      factor <- major$factor
      size <- m$label * factor
      if (in_plane) {
        stick <- major$to - major$from
        length <- sqrt(rowSums(stick^2))
        direction <- stick / length
        flat <- !is.finite(length) | length < 1e-6 | !tick_shown
        direction[flat, ] <- normal[flat, ]
        anchor <- if (tick_shown) major$to else major$from
        location <- axis_label_positions_3d(anchor, direction, m, params$n.dodge, size, angle, start = 0)
      } else {
        location <- axis_label_positions_3d(major$from, normal, m, params$n.dodge, size, angle, start = start_mm * factor)
      }
      band <- axis_label_rows_3d(size, normal, m, params$n.dodge, angle)$band
      labels <- list(kind = "text", location = location, angle = angle, factor = factor, size = rotated_extent_3d(size, angle))
    }
    reach <- max(reach, rowSums((labels$location - major$from) * normal) + label_depths_3d(labels$size, normal) / 2)
  }
  if (orientation != "plane" && !in_plane && (is.null(labels) || all(labels$factor == 1))) reach <- m$tick_length + m$gap + band
  title <- NULL
  if (length(guide$title) && visible_element_3d(m$title)) {
    near <- identical(params$title.position, "near")
    end <- if (near) {
      depths <- layout$at(c(0, 1))[, 3L]
      if (depths[1L] < depths[2L]) 0 else 1
    }
    if (orientation == "plane") {
      value <- if (near) end else 0.5
      f <- pane_frame(value)
      choice <- reading_axes_3d(ctx, f$base[1L, ], f$a[1L, ], f$b[1L, ], c("a", "-a"))
      axes <- reading_choice_3d(choice, f$a[1L, ], f$b[1L, ])
      outline <- text_outline_3d(guide$title, m$title$size, m$title$family %or3d% "", m$title$face %or3d% "plain")
      centre <- if (near) {
        depth <- abs(axes$u_a) * outline$size[1L] + abs(axes$v_a) * outline$size[2L]
        f$base[1L, ] - sign(end - 0.5) * f$a[1L, ] * ctx$units() * (m$title_gap + depth / 2) / ctx$scale +
          f$b[1L, ] * ctx$units() * (m$title_gap + outline$size[2L] / 2) / ctx$scale
      } else {
        depth <- abs(axes$u_b) * outline$size[1L] + abs(axes$v_b) * outline$size[2L]
        label_band <- if (is.null(plane_frame)) 0 else band
        f$base[1L, ] + f$b[1L, ] * ctx$units() * (start_mm + m$gap + label_band + m$title_gap + depth / 2) / ctx$scale
      }
      path <- plane_label_paths_3d(ctx, outline, centre, axes)
      title <- list(kind = "path", path = path, centre = centre, axes = axes, bounds = path$box)
    } else if (near) {
      anchor <- layout$at(end)[1L, 1:2]
      direction <- layout$tangents_at(end)[1L, ] * sign(end - 0.5)
      size <- m$title_size[1L, ]
      angle <- if (orientation == "edge") upright_angle_3d(direction[1L], direction[2L]) else 0
      depth <- label_depths_3d(matrix(size, 1L), direction, angle)
      title <- c(list(kind = "text"), text_box_3d(anchor + direction * (m$title_gap + depth / 2), size, angle))
    } else {
      title <- c(list(kind = "text"), axis_title_geometry_3d(layout$title$a, layout$title$b, layout$title$normal, m, band, reach))
    }
  }
  list(major = major, minor = minor, labels = labels, title = title)
}

view_guide_reserve_3d <- function(params, measured, viewport) {
  frame <- frame_of_3d(params)
  camera <- params$camera; camera$zoom <- 1
  camera <- frame_camera_3d(camera, viewport, "view", frame$bounds())
  basis <- frame_basis_3d(camera, viewport)
  project <- function(xyz) {
    xy <- frame_positions_3d(xyz, camera, basis)
    xy[, 1:2] <- sweep(xy[, 1:2, drop = FALSE], 2L, c(viewport$width_mm, viewport$height_mm), "*")
    unname(xy)
  }
  ctx <- typography_context_3d(frame, camera, viewport, project)
  centre <- project(frame$bounds())
  boxes <- matrix(c(0, 0, viewport$width_mm, viewport$height_mm), 1L)
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide_on_box_3d(guide, params)) next
    layout <- axis_layout_3d(frame, guide, project, centre)
    if (is.null(layout)) next
    typography <- guide_typography_3d(ctx, guide, measured$metrics[[name]], layout)
    for (ticks in list(typography$major, typography$minor)) {
      if (!is.null(ticks)) boxes <- rbind(boxes, cbind(pmin(ticks$from, ticks$to), pmax(ticks$from, ticks$to)))
    }
    labels <- typography$labels
    if (!is.null(labels)) boxes <- rbind(boxes, cbind(labels$location - labels$size / 2, labels$location + labels$size / 2))
    if (!is.null(typography$title)) boxes <- rbind(boxes, typography$title$bounds)
  }
  measured$reserve + c(max(0, max(boxes[, 4L]) - viewport$height_mm),
    max(0, max(boxes[, 3L]) - viewport$width_mm), max(0, -min(boxes[, 2L])), max(0, -min(boxes[, 1L])))
}

guide_visible_3d <- function(guide, params) (guide$part %or3d% guide$axis) %in% params$coord_params$axes

# A primary guide at position "auto" follows coord_3d(axis_lines = ).
guide_at_origin_3d <- function(guide, params) {
  position <- guide$params$position
  guide_visible_3d(guide, params) && (identical(position, "origin") ||
    (identical(position, "auto") && !isTRUE(guide$secondary) && identical(params$coord_params$axis_lines, "origin")))
}

guide_on_box_3d <- function(guide, params) guide_visible_3d(guide, params) && !guide_at_origin_3d(guide, params)

# Labels are visited in ggplot2's check.overlap priority along the axis (first,
# last, then recursive midpoints); a label is kept when its rectangle lies on
# the device and clears every rectangle kept before it by gap. For the overlap
# test each rectangle grows by `margin` times its own width and height, so two
# labels need room for (1 + margin) times their measured extent: text drawn
# later with a substituted font (a web font in an SVG, a viewer without the
# measured face) runs wider than the metrics it was laid out with. Without
# thinning (discrete axes) every finite label is kept, as in ggplot2 with
# check.overlap = FALSE.
thin_axis_labels_3d <- function(locations, size, tangent, occupied, viewport, gap = 0.5, thin = TRUE,
                                margin = axis_label_margin_3d) {
  boxes <- cbind(locations - size / 2, locations + size / 2)
  along <- if (is.null(tangent)) seq_len(nrow(locations)) else drop(locations %*% tangent)
  keep <- rep(FALSE, nrow(locations))
  ranked <- order(along, seq_along(along))
  for (i in ranked[axis_label_priority_3d(length(ranked))]) {
    box <- boxes[i, ]
    if (any(!is.finite(box))) next
    if (thin) {
      if (any(box[1:2] < 0) || box[3L] > viewport$width_mm || box[4L] > viewport$height_mm) next
      if (nrow(occupied)) {
        grown <- grow_boxes_3d(occupied, margin / 2)
        mine <- grow_boxes_3d(matrix(box, 1L), margin / 2)
        if (any(grown[, 1L] < mine[3L] + gap & grown[, 3L] > mine[1L] - gap &
            grown[, 2L] < mine[4L] + gap & grown[, 4L] > mine[2L] - gap)) next
      }
    }
    keep[i] <- TRUE
    occupied <- rbind(occupied, box)
  }
  list(keep = keep, boxes = boxes, occupied = occupied)
}

# Font substitution allowance of axis label thinning, as a fraction of a label's
# measured width and height.
axis_label_margin_3d <- 0.2

# Rectangles (x0, y0, x1, y1) grown on every side by `fraction` of their size.
grow_boxes_3d <- function(boxes, fraction) {
  pad <- (boxes[, 3:4, drop = FALSE] - boxes[, 1:2, drop = FALSE]) * fraction
  boxes + cbind(-pad, pad)
}

# Visiting order of n labels sorted along an axis: both ends, then the midpoint
# of each remaining run, recursively (ggplot2's axis_label_priority()).
axis_label_priority_3d <- function(n) {
  if (n <= 0L) return(integer())
  between <- function(lo, hi) {
    if (hi - lo + 1L <= 2L) return(integer())
    mid <- lo - 1L + (hi - lo + 2L) %/% 2L
    c(mid, between(lo, mid), between(mid, hi))
  }
  unique(c(1L, n, between(1L, n)))
}

# Discrete axes keep every label unless check.overlap asks for thinning.
thin_labels_of_3d <- function(guide) !isTRUE(guide$discrete) || isTRUE(guide$params$check.overlap)

# Model points to millimetres in the outer panel viewport, and depth.
furniture_projector_3d <- function(furniture) {
  fit <- furniture$overlay$fit
  basis <- frame_basis_3d(furniture$camera, fit$viewport)
  function(xyz) {
    points <- frame_positions_3d(xyz, furniture$camera, basis)
    cbind(points[, 1L] * fit$viewport$width_mm + fit$origin[1L],
          points[, 2L] * fit$viewport$height_mm + fit$origin[2L], points[, 3L])
  }
}

furniture_overlay_3d <- function(furniture, scene, theme) {
  meta <- furniture$overlay; fit <- meta$fit; params <- meta$params
  frame <- meta$frame %or3d% FrameBox
  panel_guides <- lapply(names(params$panel_guides), function(name) {
    grob <- params$panel_guides[[name]]$draw_panel_3d(furniture, theme)
    grob$name <- paste0("panel-guide-3d-", name)
    grob
  })
  # Axes run along the box edges on the image; an eye inside the box sees no
  # outline for them to follow, so they are left out.
  if (!frame_in_front_3d(frame, furniture$camera, frame_basis_3d(furniture$camera, fit$viewport))) {
    return(grid::gTree(children = do.call(grid::gList, panel_guides), bounds = list(), label_layout = list(),
      corners_mm = matrix(NA_real_, 8L, 3L), name = "furniture-3d"))
  }
  project <- furniture_projector_3d(furniture)
  corners <- project(frame$map(cube_edges_3d()$corners))
  if (any(!is.finite(corners)) && (length(params$coord_params$axes) || params$coord_params$box == "all")) {
    stop("3D axis and front-frame placement requires unclipped cube projection queries for this camera; schema 1 does not retain these anchors outside the scene rectangle.", call. = FALSE)
  }
  ctx <- typography_context_3d(frame, furniture$camera, fit$viewport, project)
  centre <- project(frame$bounds())
  page <- new_scene_viewport(fit$viewport$width_mm + sum(fit$reserve[c(2L, 4L)]),
    fit$viewport$height_mm + sum(fit$reserve[c(1L, 3L)]), fit$viewport$dpi)
  children <- panel_guides; bounds <- list(); occupied <- matrix(numeric(), 0, 4L); label_layout <- list()
  add <- function(g) children[[length(children) + 1L]] <<- g
  line <- function(a, b, element) {
    if (visible_element_3d(element)) add(grid::segmentsGrob(a[, 1L], a[, 2L], b[, 1L], b[, 2L], default.units = "mm", gp = line_gp_3d(element)))
  }
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide_on_box_3d(guide, params)) next
    m <- fit$metrics[[name]]
    layout <- axis_layout_3d(frame, guide, project, centre)
    if (is.null(layout)) next
    typography <- guide_typography_3d(ctx, guide, m, layout)
    n <- length(guide$position)
    a <- layout$a; b <- layout$b
    lo <- if (guide$params$cap %in% c("lower", "both") && n) min(guide$position) else 0
    hi <- if (guide$params$cap %in% c("upper", "both") && n) max(guide$position) else 1
    axis_element <- element_3d(paste0("axis.line.", guide$axis), guide$theme)
    if (identical(params$coord_params$axis_lines, "none")) NULL
    else if (layout$straight) line(matrix(a + (b - a) * lo, 1), matrix(a + (b - a) * hi, 1), axis_element)
    else if (visible_element_3d(axis_element)) {
      curve <- layout$at(seq(lo, hi, length.out = 129L))
      add(grid::linesGrob(curve[, 1L], curve[, 2L], default.units = "mm", gp = line_gp_3d(axis_element)))
    }
    tick_element <- element_3d(paste0("axis.ticks.", guide$axis), guide$theme)
    if (!identical(guide$params$ticks, "in_plane")) {
      if (!is.null(typography$major)) line(typography$major$from, typography$major$to, tick_element)
      if (!is.null(typography$minor)) line(typography$minor$from, typography$minor$to, element_3d(paste0("axis.minor.ticks.", guide$axis), guide$theme) %or3d% tick_element)
    }
    labels <- typography$labels
    if (!is.null(labels)) {
      thinned <- thin_axis_labels_3d(labels$location, labels$size, layout$tangent, occupied, page,
        gap = if (isTRUE(guide$params$check.overlap)) 1 else 0.5, thin = thin_labels_of_3d(guide))
      keep <- thinned$keep; occupied <- thinned$occupied
      thinned$rows <- (seq_len(n) - 1L) %% guide$params$n.dodge
      label_layout[[name]] <- thinned
      bounds <- c(bounds, lapply(which(keep), function(i) thinned$boxes[i, ]))
      if (any(keep)) {
        if (labels$kind == "text") {
          gp <- text_gp_3d(m$text); gp$fontsize <- m$text$size * labels$factor[keep]
          add(grid::textGrob(guide$labels[keep], labels$location[keep, 1L], labels$location[keep, 2L], default.units = "mm",
            rot = labels$angle[keep], gp = gp))
        }
      }
    }
    title <- typography$title
    if (!is.null(title)) {
      bounds[[length(bounds) + 1L]] <- title$bounds
      if (title$kind == "text") add(grid::textGrob(guide$title, title$location[1L], title$location[2L], default.units = "mm", rot = title$angle, gp = text_gp_3d(m$title)))
    }
  }
  grid::gTree(children = do.call(grid::gList, children), bounds = bounds, label_layout = label_layout,
    corners_mm = corners, name = "furniture-3d")
}
