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
  a <- angle * pi / 180
  cbind(abs(cos(a)) * metrics[, 1L] + abs(sin(a)) * metrics[, 2L],
        abs(sin(a)) * metrics[, 1L] + abs(cos(a)) * metrics[, 2L])
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
    label <- text_metrics_3d(guide$labels, text, angle)
    title_size <- text_metrics_3d(guide$title, title)
    tick <- element_3d(paste0("axis.ticks.", axis), local)
    tick_length <- if (visible_element_3d(tick)) abs(grid::convertUnit(element_3d(paste0("axis.ticks.length.", axis), local), "mm", valueOnly = TRUE)) else 0
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
  active <- names(guides)[vapply(guides, guide_visible_3d, logical(1), params)]
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
    forward = forward, ray = ray, standard = all(shear == 0) && identical(as.integer(camera$up %or3d% 3L), 3L),
    perspective = perspective, distance = distance, base = base)
}

frame_coordinates_3d <- function(xyz, camera, basis) {
  delta <- sweep(sweep(xyz, 2L, camera$target), 2L, basis$scale, "*")
  xy <- delta %*% basis$basis
  distance <- if (basis$perspective) basis$distance else 2 * basis$radius + 1
  depth <- distance + drop(delta %*% basis$forward)
  if (any(depth <= 0)) stop("The projected box crosses the eye plane; use a greater camera distance.", call. = FALSE)
  if (basis$perspective) xy <- xy / depth
  cbind(x = xy[, 1L], y = xy[, 2L], depth = depth)
}

frame_positions_3d <- function(xyz, camera, basis) {
  coordinates <- frame_coordinates_3d(xyz, camera, basis)
  coordinates[, 1L] <- 0.5 + coordinates[, 1L] * basis$base * camera$zoom / (2 * basis$ratio)
  coordinates[, 2L] <- 0.5 + coordinates[, 2L] * basis$base * camera$zoom / 2
  coordinates
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
  # it, so a viewer can re-decide visibility for another camera.
  add <- function(xyz, kind, element = NULL, partition = 0L, anchor = FALSE, tag = NULL) {
    if (kind == "polylines") {
      projected <- frame_positions_3d(xyz, camera, basis)[, 1:2, drop = FALSE]
      # A line parallel to the sight ray has no projected stroke length.
      if (max(abs(sweep(projected, 2L, projected[1L, ]))) <= 64 * .Machine$double.eps) return(0L)
    }
    rows <- n_vertices + seq_len(nrow(xyz))
    chunks[[length(chunks) + 1L]] <<- xyz
    n_vertices <<- n_vertices + nrow(xyz)
    primitives[[length(primitives) + 1L]] <<- rows
    kinds <<- c(kinds, kind)
    materials[[length(materials) + 1L]] <<- list(element = element, partition = partition, anchor = anchor)
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
  explicit <- intersect(setdiff(cfg$panes, c("back", "floor")), pane_names)
  back <- if ("back" %in% cfg$panes) frame$back_panes(partitions) else character()
  panes <- unique(c(explicit, back, if ("floor" %in% cfg$panes) frame$floor_panes()))
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
  for (pane_name in panes) {
    pane <- all_panes[[match(pane_name, pane_names)]]
    classify <- function(points) {
      if (!camera$clip_data) return(rep(1L, nrow(points)))
      if (frame$linear) return(rep(unname(partitions[pane_name]), nrow(points)))
      class <- pane_facing(pane, points)
      if (rear_only[[pane_name]]) class[class != 0L] <- NA_integer_
      class
    }
    element <- element_3d(paste0("panel.pane.", pane$element), theme)
    pane_tag <- function(at) list(type = "pane", panes = pane_name, at = unname(at))
    if (visible_element_3d(element) && !is.na(element$fill) && !isFALSE(pane$fill)) {
      if (frame$linear) add(frame$map(pane$xyz), "faces", element, classify(matrix(colMeans(pane$xyz), 1L)), tag = pane_tag(colMeans(pane$xyz)))
      else {
        polygon <- new_scene_packet(vertices = list(x = pane$xyz[, 1L], y = pane$xyz[, 2L], z = pane$xyz[, 3L]),
          corners = list(vertex = seq_len(nrow(pane$xyz)), source_row = rep.int(0L, nrow(pane$xyz))),
          faces = list(offset = 1L, count = nrow(pane$xyz)), styles = data.frame(style_id = 1L))
        munched <- munch_packet_3d(frame, polygon, clip = FALSE)
        tri <- matrix(munched$corners$vertex, nrow = 3L)
        if (ncol(tri)) {
          model <- do.call(cbind, munched$vertices); scale <- attr(munched, "scale")
          centres <- (scale[tri[1L, ], , drop = FALSE] + scale[tri[2L, ], , drop = FALSE] + scale[tri[3L, ], , drop = FALSE]) / 3
          class <- classify(centres)
          for (k in which(!is.na(class))) add(model[tri[, k], , drop = FALSE], "faces", element, class[k], tag = pane_tag(centres[k, ]))
        }
      }
    }
    for (family in pane$families) for (level in c("minor", "major")) {
      view <- panel_params[[family$scale]]
      values <- if (level == "major") view$break_positions() else view$break_positions_minor()
      values <- values[is.finite(values) & values >= 0 & values <= 1]
      element <- element_3d(paste0("panel.grid.", level, ".", family$element), theme)
      if (!visible_element_3d(element)) next
      for (value in values) {
        line <- pane_isoline_3d(pane$xyz, family, value)
        if (is.null(line)) next
        if (frame$linear) add(frame$map(line), "polylines", element, classify(matrix(colMeans(line), 1L)), tag = pane_tag(colMeans(line)))
        else {
          path <- polyline_model_3d(frame, line)
          add_runs(path, element, classify(segment_midpoints(path)), pane_tag(NULL))
        }
      }
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
  styles <- data.frame(style_id = seq_along(primitives), colour = NA_character_, fill = NA_character_,
    linewidth = 0, linetype = "solid", label = "", fontsize = 0, .anchor = FALSE)
  opacity <- rep(1, nrow(styles))
  for (i in seq_along(materials)) {
    m <- materials[[i]]; el <- m$element
    styles$.anchor[i] <- m$anchor
    if (!is.null(el)) {
      styles$colour[i] <- el$colour %or3d% NA_character_
      styles$fill[i] <- el$fill %or3d% NA_character_
      styles$linewidth[i] <- el$linewidth %or3d% 0
      styles$linetype[i] <- as.character(el$linetype %or3d% 1)
      chosen <- if (kinds[i] == "faces") styles$fill[i] else styles$colour[i]
      rgba <- grDevices::col2rgb(chosen, alpha = TRUE)
      opacity[i] <- rgba[4L, 1L] / 255
      for (key in c("colour", "fill")) if (!is.na(styles[[key]][i])) {
        rgb <- grDevices::col2rgb(styles[[key]][i])
        styles[[key]][i] <- grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
      }
    }
  }
  counts <- lengths(primitives); offsets <- cumsum(c(1L, head(counts, -1L)))
  tables <- lapply(c("faces", "polylines", "text"), function(kind) {
    i <- which(kinds == kind)
    tab <- list(primitive_id = i, offset = offsets[i], count = counts[i], style_id = i,
      layer_id = rep.int(0L, length(i)), source_row = as.integer(i),
      partition = vapply(materials[i], `[[`, integer(1), "partition"), opacity = opacity[i])
    if (kind == "polylines") tab$width_mm <- styles$linewidth[i]
    tab
  }); names(tables) <- c("faces", "polylines", "text")
  vertices <- do.call(rbind, chunks)
  geometry <- lapply(as.data.frame(vertices), as.double); names(geometry) <- c("x", "y", "z")
  packet <- do.call(new_scene_packet, c(list(vertices = geometry,
    corners = list(vertex = as.integer(unlist(primitives)), source_row = rep.int(0L, sum(counts))), styles = styles,
    panel_id = panel_params$panel_id), tables))
  names(packet$vertices) <- c("x", "y", "z")
  tags[length(primitives) + 1L] <- list(NULL)
  list(packet = packet, camera = camera, overlay = list(frame = frame, corners = corner_queries, guides = guide_queries,
    params = panel_params, fit = fit), viewport = fit$viewport, tags = tags[seq_along(primitives)])
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

axis_label_positions_3d <- function(major, normal, metrics, dodge) {
  if (is.matrix(normal)) {
    support <- rowSums(metrics$label * abs(normal)) / 2
    offset <- metrics$tick_length + metrics$gap + support +
      ((seq_len(nrow(major)) - 1L) %% dodge) * metrics$label_extent
    return(major + offset * normal)
  }
  support <- drop(metrics$label %*% abs(normal)) / 2
  offset <- metrics$tick_length + metrics$gap + support +
    ((seq_len(nrow(major)) - 1L) %% dodge) * metrics$label_extent
  major + outer(offset, normal)
}

axis_title_geometry_3d <- function(a, b, normal, metrics, dodge) {
  size <- metrics$title_size[1L, ]
  offset <- metrics$tick_length + metrics$gap + dodge * metrics$label_extent + metrics$title_gap + size[2L] / 2
  location <- (a + b) / 2 + normal * offset
  angle <- atan2(b[2L] - a[2L], b[1L] - a[1L]) * 180 / pi
  if (angle > 90) angle <- angle - 180
  if (angle < -90) angle <- angle + 180
  theta <- angle * pi / 180
  extent <- c(abs(cos(theta)) * size[1L] + abs(sin(theta)) * size[2L],
    abs(sin(theta)) * size[1L] + abs(cos(theta)) * size[2L])
  list(location = location, angle = angle, bounds = c(location - extent / 2, location + extent / 2))
}

# Screen geometry of one axis guide: the chosen candidate, tick anchors and
# outward normals, and title placement. project() maps model points to screen
# millimetres and depth; centre holds projected frame bounds.
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
  normals_at <- function(values) {
    if (straight || !length(values)) return(normal)
    delta <- 1e-4
    tangent <- at(values + delta)[, 1:2, drop = FALSE] - at(values - delta)[, 1:2, drop = FALSE]
    n <- cbind(-tangent[, 2L], tangent[, 1L]) / sqrt(rowSums(tangent^2))
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
    a = a, b = b, normal = normal, at = at, normals_at = normals_at,
    tangent = if (straight) (b - a) / sqrt(sum((b - a)^2)) else NULL, title = title_args)
}

tick_ends_3d <- function(points, normals, length) {
  if (is.matrix(normals)) points + normals * length else sweep(points, 2L, normals * length, "+")
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
  centre <- project(frame$bounds())
  boxes <- matrix(c(0, 0, viewport$width_mm, viewport$height_mm), 1L)
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide_visible_3d(guide, params)) next
    m <- measured$metrics[[name]]
    layout <- axis_layout_3d(frame, guide, project, centre)
    if (is.null(layout)) next
    if (length(guide$position)) {
      major <- layout$at(guide$position)[, 1:2, drop = FALSE]
      normal <- layout$normals_at(guide$position)
      tip <- tick_ends_3d(major, normal, m$tick_length)
      boxes <- rbind(boxes, cbind(pmin(major, tip), pmax(major, tip)))
      if (visible_element_3d(m$text)) {
        labels <- axis_label_positions_3d(major, normal, m, guide$params$n.dodge)
        boxes <- rbind(boxes, cbind(labels - m$label / 2, labels + m$label / 2))
      }
    }
    if (length(guide$title) && visible_element_3d(m$title)) {
      boxes <- rbind(boxes, axis_title_geometry_3d(layout$title$a, layout$title$b, layout$title$normal, m, guide$params$n.dodge)$bounds)
    }
  }
  measured$reserve + c(max(0, max(boxes[, 4L]) - viewport$height_mm),
    max(0, max(boxes[, 3L]) - viewport$width_mm), max(0, -min(boxes[, 2L])), max(0, -min(boxes[, 1L])))
}

guide_visible_3d <- function(guide, params) (guide$part %or3d% guide$axis) %in% params$coord_params$axes

thin_axis_labels_3d <- function(locations, size, tangent, rows, occupied, viewport, gap = 0.5) {
  if (is.null(tangent)) {
    intervals <- cbind(seq_len(nrow(locations)), seq_len(nrow(locations)))
  } else {
    along <- drop(locations %*% tangent)
    half <- drop(size %*% abs(tangent)) / 2
    intervals <- cbind(along - half - gap / 2, along + half + gap / 2)
  }
  boxes <- cbind(locations - size / 2, locations + size / 2)
  keep <- rep(FALSE, nrow(locations))
  for (row in unique(rows)) {
    candidates <- which(rows == row)
    candidates <- candidates[order(intervals[candidates, 2L], candidates)]
    end <- -Inf
    for (i in candidates) {
      box <- boxes[i, ]
      if (intervals[i, 1L] < end || any(!is.finite(box)) || any(box[1:2] < 0) ||
          box[3L] > viewport$width_mm || box[4L] > viewport$height_mm) next
      if (nrow(occupied) && any(occupied[, 1L] < box[3L] + gap & occupied[, 3L] > box[1L] - gap &
          occupied[, 2L] < box[4L] + gap & occupied[, 4L] > box[2L] - gap)) next
      keep[i] <- TRUE; end <- intervals[i, 2L]
      occupied <- rbind(occupied, box)
    }
  }
  list(keep = keep, boxes = boxes, intervals = intervals, rows = rows, occupied = occupied)
}

furniture_overlay_3d <- function(furniture, scene, theme) {
  meta <- furniture$overlay; fit <- meta$fit; params <- meta$params
  frame <- meta$frame %or3d% FrameBox
  basis <- frame_basis_3d(furniture$camera, fit$viewport)
  project <- function(xyz) {
    points <- frame_positions_3d(xyz, furniture$camera, basis)
    cbind(points[, 1L] * fit$viewport$width_mm + fit$origin[1L],
          points[, 2L] * fit$viewport$height_mm + fit$origin[2L], points[, 3L])
  }
  corners <- project(frame$map(cube_edges_3d()$corners))
  if (any(!is.finite(corners)) && (length(params$coord_params$axes) || params$coord_params$box == "all")) {
    stop("3D axis and front-frame placement requires unclipped cube projection queries for this camera; schema 1 does not retain these anchors outside the scene rectangle.", call. = FALSE)
  }
  centre <- project(frame$bounds())
  children <- list(); bounds <- list(); occupied <- matrix(numeric(), 0, 4L); label_layout <- list()
  add <- function(g) children[[length(children) + 1L]] <<- g
  line <- function(a, b, element) {
    if (visible_element_3d(element)) add(grid::segmentsGrob(a[, 1L], a[, 2L], b[, 1L], b[, 2L], default.units = "mm", gp = line_gp_3d(element)))
  }
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide_visible_3d(guide, params)) next
    m <- fit$metrics[[name]]
    layout <- axis_layout_3d(frame, guide, project, centre)
    if (is.null(layout)) next
    n <- length(guide$position)
    major <- layout$at(guide$position)[, 1:2, drop = FALSE]
    normal <- layout$normals_at(guide$position)
    a <- layout$a; b <- layout$b
    lo <- if (guide$params$cap %in% c("lower", "both") && n) min(guide$position) else 0
    hi <- if (guide$params$cap %in% c("upper", "both") && n) max(guide$position) else 1
    axis_element <- element_3d(paste0("axis.line.", guide$axis), guide$theme)
    if (layout$straight) line(matrix(a + (b - a) * lo, 1), matrix(a + (b - a) * hi, 1), axis_element)
    else if (visible_element_3d(axis_element)) {
      curve <- layout$at(seq(lo, hi, length.out = 129L))
      add(grid::linesGrob(curve[, 1L], curve[, 2L], default.units = "mm", gp = line_gp_3d(axis_element)))
    }
    if (n) line(major, tick_ends_3d(major, normal, m$tick_length), element_3d(paste0("axis.ticks.", guide$axis), guide$theme))
    if (isTRUE(guide$params$minor.ticks) && length(guide$minor)) {
      minor <- layout$at(guide$minor)[, 1:2, drop = FALSE]
      line(minor, tick_ends_3d(minor, layout$normals_at(guide$minor), m$tick_length / 2), element_3d(paste0("axis.minor.ticks.", guide$axis), guide$theme) %or3d% element_3d(paste0("axis.ticks.", guide$axis), guide$theme))
    }
    if (n && visible_element_3d(m$text)) {
      locations <- axis_label_positions_3d(major, normal, m, guide$params$n.dodge)
      thinned <- thin_axis_labels_3d(locations, m$label, layout$tangent,
        (seq_len(n) - 1L) %% guide$params$n.dodge, occupied,
        new_scene_viewport(fit$viewport$width_mm + sum(fit$reserve[c(2L, 4L)]),
          fit$viewport$height_mm + sum(fit$reserve[c(1L, 3L)]), fit$viewport$dpi),
        gap = if (isTRUE(guide$params$check.overlap)) 1 else 0.5)
      keep <- thinned$keep; occupied <- thinned$occupied
      label_layout[[name]] <- thinned
      bounds <- c(bounds, lapply(which(keep), function(i) thinned$boxes[i, ]))
      if (any(keep)) add(grid::textGrob(guide$labels[keep], locations[keep, 1L], locations[keep, 2L], default.units = "mm", rot = m$angle, gp = text_gp_3d(m$text)))
    }
    if (length(guide$title) && visible_element_3d(m$title)) {
      title <- axis_title_geometry_3d(layout$title$a, layout$title$b, layout$title$normal, m, guide$params$n.dodge)
      bounds[[length(bounds) + 1L]] <- title$bounds
      add(grid::textGrob(guide$title, title$location[1L], title$location[2L], default.units = "mm", rot = title$angle, gp = text_gp_3d(m$title)))
    }
  }
  grid::gTree(children = do.call(grid::gList, children), bounds = bounds, label_layout = label_layout,
    corners_mm = corners, name = "furniture-3d")
}
