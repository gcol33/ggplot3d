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
  active <- names(guides)[vapply(guides, function(g) g$axis %in% params$coord_params$axes, logical(1))]
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
  right <- c(-sin(az), cos(az), 0)
  up <- c(-sin(el) * cos(az), -sin(el) * sin(az), cos(el))
  basis <- cbind(cos(roll) * right - sin(roll) * up, sin(roll) * right + cos(roll) * up)
  forward <- -c(cos(el) * cos(az), cos(el) * sin(az), sin(el))
  perspective <- camera$projection == "perspective"
  distance <- camera$distance
  if (perspective && is.na(distance)) {
    half_angle <- atan(tan(camera$fov * pi / 360) * min(1, ratio))
    distance <- radius / sin(half_angle) + 64 * .Machine$double.eps * max(1, radius)
  }
  base <- if (perspective) 1 / tan(camera$fov * pi / 360) else min(1, ratio) / radius
  list(scale = scale, extent = extent, radius = radius, ratio = ratio, basis = basis,
    forward = forward, perspective = perspective, distance = distance, base = base)
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

frame_camera_3d <- function(camera, viewport, frame) {
  if (frame == "sphere") return(camera)
  geometry <- frame_basis_3d(camera, viewport)
  scale <- geometry$scale; extent <- geometry$extent; radius <- geometry$radius
  ratio <- geometry$ratio; basis <- geometry$basis; forward <- geometry$forward
  perspective <- geometry$perspective; distance <- geometry$distance
  if (frame == "view") {
    xy <- frame_coordinates_3d(cube_edges_3d()$corners, camera, geometry)
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

furniture_partitions_3d <- function(camera, viewport) {
  basis <- frame_basis_3d(camera, viewport)
  panes <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
  axis <- rep(seq_len(3L), each = 2L)
  side <- rep(0:1, 3L)
  toward <- -basis$forward[axis]
  if (basis$perspective) {
    toward <- toward * basis$distance - (side - camera$target[axis]) * basis$scale[axis]
  }
  facing <- (2 * side - 1) * toward
  tolerance <- 64 * .Machine$double.eps * max(1, abs(toward))
  stats::setNames(ifelse(facing > tolerance, 2L, 0L), panes)
}

panel_furniture_3d <- function(panel_params, theme, viewport, frame_group = list()) {
  fit <- measure_guides_3d(panel_params, theme, viewport)
  if (length(frame_group)) {
    fits <- lapply(frame_group, function(panel) measure_guides_3d(panel$params, panel$theme, viewport))
    fit$reserve <- Reduce(pmax, lapply(fits, `[[`, "reserve"))
    fit$origin <- fit$reserve[c(4L, 3L)]
    fit$viewport <- new_scene_viewport(viewport$width_mm - sum(fit$reserve[c(2L, 4L)]),
      viewport$height_mm - sum(fit$reserve[c(1L, 3L)]), viewport$dpi)
  }
  frame <- panel_params$coord_params$frame %or3d% "sphere"
  camera <- frame_camera_3d(panel_params$camera, fit$viewport, frame)
  if (length(frame_group)) {
    magnifications <- vapply(frame_group, function(panel) {
      fitted <- frame_camera_3d(panel$params$camera, fit$viewport, frame)
      fitted$zoom / panel$params$camera$zoom
    }, numeric(1))
    camera$zoom <- panel_params$camera$zoom * min(magnifications)
  }
  if (frame == "view") {
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
      fitted <- frame_camera_3d(panel$params$camera, fit$viewport, frame)
      fitted$zoom / panel$params$camera$zoom
    }, numeric(1))
    camera$zoom <- panel_params$camera$zoom * min(magnifications)
  }
  cube <- cube_edges_3d()
  basis <- frame_basis_3d(camera, fit$viewport)
  vertices <- cube$corners
  primitives <- list(); kinds <- character(); materials <- list(); anchor_rows <- integer()
  add <- function(xyz, kind, element = NULL, partition = 0L, anchor = FALSE) {
    if (kind == "polylines") {
      projected <- frame_positions_3d(xyz, camera, basis)[, 1:2, drop = FALSE]
      # A line parallel to the sight ray has no projected stroke length.
      if (max(abs(sweep(projected, 2L, projected[1L, ]))) <= 64 * .Machine$double.eps) return(0L)
    }
    rows <- nrow(vertices) + seq_len(nrow(xyz))
    vertices <<- rbind(vertices, xyz)
    primitives[[length(primitives) + 1L]] <<- rows
    kinds <<- c(kinds, kind)
    materials[[length(materials) + 1L]] <<- list(element = element, partition = partition, anchor = anchor)
    if (anchor) anchor_rows <<- c(anchor_rows, length(primitives))
    length(primitives)
  }
  # Empty overlay text carries projection queries through the one panel compilation.
  corner_queries <- vapply(seq_len(8L), function(i) add(cube$corners[i, , drop = FALSE], "text", partition = 2L, anchor = TRUE), integer(1))
  guide_queries <- lapply(panel_params$axis_guides, function(guide) {
    axis <- match(guide$axis, c("x", "y", "z"))
    edges <- cube$edges[cube$edges[, "axis"] == axis, , drop = FALSE]
    lapply(seq_len(4L), function(i) {
      values <- c(guide$position, guide$minor)
      if (!length(values)) return(integer())
      base <- cube$corners[rep(edges[i, "a"], length(values)), , drop = FALSE]
      base[, axis] <- values
      vapply(seq_along(values), function(j) add(base[j, , drop = FALSE], "text", partition = 2L, anchor = TRUE), integer(1))
    })
  })
  cfg <- panel_params$coord_params
  partitions <- furniture_partitions_3d(camera, fit$viewport)
  rear <- vapply(c("x", "y"), function(axis) {
    candidates <- paste0(axis, c("min", "max"))
    candidates[which.min(partitions[candidates])]
  }, character(1))
  rear_edges <- vapply(seq_len(nrow(cube$edges)), function(i) {
    xyz <- cube$corners[cube$edges[i, c("a", "b")], , drop = FALSE]
    fixed <- which(xyz[1L, ] == xyz[2L, ])
    incident <- paste0(c("x", "y", "z")[fixed], ifelse(xyz[1L, fixed] == 0, "min", "max"))
    all(partitions[incident] == 0L)
  }, logical(1))
  panes <- unique(c(setdiff(cfg$panes, c("back", "floor")), if ("back" %in% cfg$panes) rear, if ("floor" %in% cfg$panes) "zmin"))
  for (pane in panes) {
    normal <- match(substr(pane, 1, 1), c("x", "y", "z"))
    side <- as.integer(substr(pane, 2, 4) == "max")
    other <- setdiff(seq_len(3L), normal)
    xyz <- matrix(0, 4L, 3L); xyz[, normal] <- side
    xyz[, other] <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
    element <- element_3d(paste0("panel.pane.", c("x", "y", "z")[normal]), theme)
    partition <- if (camera$clip_data) unname(partitions[pane]) else 1L
    if (visible_element_3d(element) && !is.na(element$fill)) add(xyz, "faces", element, partition)
    for (axis in other) for (level in c("minor", "major")) {
      view <- panel_params[[c("x", "y", "z")[axis]]]
      values <- if (level == "major") view$break_positions() else view$break_positions_minor()
      values <- values[is.finite(values) & values >= 0 & values <= 1]
      element <- element_3d(paste0("panel.grid.", level, ".", c("x", "y", "z")[axis]), theme)
      if (!visible_element_3d(element)) next
      for (value in values) {
        line <- xyz[1:2, , drop = FALSE]
        line[, normal] <- side; line[, axis] <- value
        line[, setdiff(other, axis)] <- c(0, 1)
        add(line, "polylines", element, partition)
      }
    }
  }
  if (cfg$box != "none") {
    for (i in seq_len(nrow(cube$edges))) {
      if (!rear_edges[i] && cfg$box != "all") next
      element <- element_3d(if (rear_edges[i]) "panel.box.back" else "panel.box.front", theme)
      if (!visible_element_3d(element)) next
      e <- cube$edges[i, ]; xyz <- cube$corners[e[c("a", "b")], , drop = FALSE]
      partition <- if (!camera$clip_data) 1L else if (rear_edges[i]) 0L else 2L
      add(xyz, "polylines", element, partition)
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
  geometry <- lapply(as.data.frame(vertices), as.double); names(geometry) <- c("x", "y", "z")
  packet <- do.call(new_scene_packet, c(list(vertices = geometry,
    corners = list(vertex = as.integer(unlist(primitives)), source_row = rep.int(0L, sum(counts))), styles = styles,
    panel_id = panel_params$panel_id), tables))
  names(packet$vertices) <- c("x", "y", "z")
  list(packet = packet, camera = camera, overlay = list(cube = cube, rear_edges = rear_edges, corners = corner_queries, guides = guide_queries,
    params = panel_params, fit = fit), viewport = fit$viewport)
}

select_axis_edge_3d <- function(points, edges, position = "auto", secondary = FALSE) {
  candidates <- lapply(seq_len(nrow(edges)), function(i) {
    e <- edges[i, ]; a <- points[e["a"], 1:2]; b <- points[e["b"], 1:2]
    d <- b - a; length <- sqrt(sum(d^2))
    if (!is.finite(length) || length < 1e-7) return(NULL)
    normal <- c(-d[2L], d[1L]) / length
    midpoint <- (a + b) / 2
    if (sum(normal * (midpoint - colMeans(points[, 1:2]))) < 0) normal <- -normal
    interior <- drop(sweep(points[, 1:2, drop = FALSE], 2L, a) %*% normal)
    exterior <- max(interior) <= 1e-6
    c(edge = i, nx = normal[1L], ny = normal[2L], exterior = exterior,
      length = length, distance = sum(normal * (midpoint - colMeans(points[, 1:2]))),
      x = midpoint[1L], y = midpoint[2L], depth = mean(points[e[c("a", "b")], 3L]))
  })
  valid <- which(!vapply(candidates, is.null, logical(1)))
  if (!length(valid)) return(NULL)
  scores <- do.call(rbind, candidates[valid])
  if (is.numeric(position) || position %in% as.character(1:4)) chosen <- match(as.integer(position), scores[, "edge"])
  else {
    score <- switch(position, top = scores[, "y"], bottom = -scores[, "y"], left = -scores[, "x"], right = scores[, "x"], scores[, "distance"])
    if (secondary) {
      primary <- select_axis_edge_3d(points, edges, position, FALSE)
      score <- (scores[, "x"] - primary["x"])^2 + (scores[, "y"] - primary["y"])^2
    }
    chosen <- order(-scores[, "exterior"], -round(score, 8L), round(scores[, "depth"], 12L), scores[, "edge"])[1L]
  }
  if (is.na(chosen)) return(NULL)
  scores[chosen, ]
}

axis_label_positions_3d <- function(major, normal, metrics, dodge) {
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

view_guide_reserve_3d <- function(params, measured, viewport) {
  camera <- params$camera; camera$zoom <- 1
  camera <- frame_camera_3d(camera, viewport, "view")
  basis <- frame_basis_3d(camera, viewport)
  project <- function(xyz) {
    xy <- frame_positions_3d(xyz, camera, basis)
    xy[, 1:2] <- sweep(xy[, 1:2, drop = FALSE], 2L, c(viewport$width_mm, viewport$height_mm), "*")
    unname(xy)
  }
  cube <- cube_edges_3d(); corners <- project(cube$corners)
  boxes <- matrix(c(0, 0, viewport$width_mm, viewport$height_mm), 1L)
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide$axis %in% params$coord_params$axes) next
    m <- measured$metrics[[name]]
    axis <- match(guide$axis, c("x", "y", "z"))
    edges <- cube$edges[cube$edges[, "axis"] == axis, , drop = FALSE]
    chosen <- select_axis_edge_3d(corners, edges, guide$params$position, guide$secondary)
    if (is.null(chosen)) next
    edge <- edges[chosen["edge"], ]; normal <- unname(chosen[c("nx", "ny")])
    a <- corners[edge["a"], 1:2]; b <- corners[edge["b"], 1:2]
    if (length(guide$position)) {
      xyz <- cube$corners[rep(edge["a"], length(guide$position)), , drop = FALSE]
      xyz[, axis] <- guide$position
      major <- project(xyz)[, 1:2, drop = FALSE]
      tip <- sweep(major, 2L, normal * m$tick_length, "+")
      boxes <- rbind(boxes, cbind(pmin(major, tip), pmax(major, tip)))
      if (visible_element_3d(m$text)) {
        labels <- axis_label_positions_3d(major, normal, m, guide$params$n.dodge)
        boxes <- rbind(boxes, cbind(labels - m$label / 2, labels + m$label / 2))
      }
    }
    if (length(guide$title) && visible_element_3d(m$title)) {
      boxes <- rbind(boxes, axis_title_geometry_3d(a, b, normal, m, guide$params$n.dodge)$bounds)
    }
  }
  measured$reserve + c(max(0, max(boxes[, 4L]) - viewport$height_mm),
    max(0, max(boxes[, 3L]) - viewport$width_mm), max(0, -min(boxes[, 2L])), max(0, -min(boxes[, 1L])))
}

thin_axis_labels_3d <- function(locations, size, tangent, rows, occupied, viewport, gap = 0.5) {
  along <- drop(locations %*% tangent)
  half <- drop(size %*% abs(tangent)) / 2
  intervals <- cbind(along - half - gap / 2, along + half + gap / 2)
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
  query <- function(ids) {
    packet <- furniture$packet
    rows <- match(ids, packet$text$style_id)
    v <- packet$corners$vertex[packet$text$offset[rows]]
    xyz <- do.call(cbind, lapply(packet$vertices, `[`, v))
    points <- frame_positions_3d(xyz, furniture$camera,
      frame_basis_3d(furniture$camera, fit$viewport))
    cbind(points[, 1L] * fit$viewport$width_mm + fit$origin[1L],
          points[, 2L] * fit$viewport$height_mm + fit$origin[2L], points[, 3L])
  }
  corners <- query(meta$corners)
  if (any(!is.finite(corners)) && (length(params$coord_params$axes) || params$coord_params$box == "all")) {
    stop("3D axis and front-frame placement requires unclipped cube projection queries for this camera; schema 1 does not retain these anchors outside the scene rectangle.", call. = FALSE)
  }
  children <- list(); bounds <- list(); occupied <- matrix(numeric(), 0, 4L); label_layout <- list()
  add <- function(g) children[[length(children) + 1L]] <<- g
  line <- function(a, b, element) {
    if (visible_element_3d(element)) add(grid::segmentsGrob(a[, 1L], a[, 2L], b[, 1L], b[, 2L], default.units = "mm", gp = line_gp_3d(element)))
  }
  for (name in names(params$axis_guides)) {
    guide <- params$axis_guides[[name]]
    if (!guide$axis %in% params$coord_params$axes) next
    m <- fit$metrics[[name]]; axis <- match(guide$axis, c("x", "y", "z"))
    edges <- meta$cube$edges[meta$cube$edges[, "axis"] == axis, , drop = FALSE]
    chosen <- select_axis_edge_3d(corners, edges, guide$params$position, guide$secondary)
    if (is.null(chosen)) next
    edge <- edges[chosen["edge"], ]; normal <- unname(chosen[c("nx", "ny")])
    anchors <- query(meta$guides[[name]][[chosen["edge"]]])
    n <- length(guide$position); major <- anchors[seq_len(n), 1:2, drop = FALSE]
    a <- corners[edge["a"], 1:2]; b <- corners[edge["b"], 1:2]
    lo <- if (guide$params$cap %in% c("lower", "both") && n) min(guide$position) else 0
    hi <- if (guide$params$cap %in% c("upper", "both") && n) max(guide$position) else 1
    line(matrix(a + (b - a) * lo, 1), matrix(a + (b - a) * hi, 1), element_3d(paste0("axis.line.", guide$axis), guide$theme))
    if (n) line(major, sweep(major, 2L, normal * m$tick_length, "+"), element_3d(paste0("axis.ticks.", guide$axis), guide$theme))
    if (isTRUE(guide$params$minor.ticks) && nrow(anchors) > n) {
      minor <- anchors[seq.int(n + 1L, nrow(anchors)), 1:2, drop = FALSE]
      line(minor, sweep(minor, 2L, normal * m$tick_length / 2, "+"), element_3d(paste0("axis.minor.ticks.", guide$axis), guide$theme) %or3d% element_3d(paste0("axis.ticks.", guide$axis), guide$theme))
    }
    if (n && visible_element_3d(m$text)) {
      locations <- axis_label_positions_3d(major, normal, m, guide$params$n.dodge)
      tangent <- (b - a) / sqrt(sum((b - a)^2))
      layout <- thin_axis_labels_3d(locations, m$label, tangent,
        (seq_len(n) - 1L) %% guide$params$n.dodge, occupied,
        new_scene_viewport(fit$viewport$width_mm + sum(fit$reserve[c(2L, 4L)]),
          fit$viewport$height_mm + sum(fit$reserve[c(1L, 3L)]), fit$viewport$dpi),
        gap = if (isTRUE(guide$params$check.overlap)) 1 else 0.5)
      keep <- layout$keep; occupied <- layout$occupied
      label_layout[[name]] <- layout
      bounds <- c(bounds, lapply(which(keep), function(i) layout$boxes[i, ]))
      if (any(keep)) add(grid::textGrob(guide$labels[keep], locations[keep, 1L], locations[keep, 2L], default.units = "mm", rot = m$angle, gp = text_gp_3d(m$text)))
    }
    if (length(guide$title) && visible_element_3d(m$title)) {
      title <- axis_title_geometry_3d(a, b, normal, m, guide$params$n.dodge)
      bounds[[length(bounds) + 1L]] <- title$bounds
      add(grid::textGrob(guide$title, title$location[1L], title$location[2L], default.units = "mm", rot = title$angle, gp = text_gp_3d(m$title)))
    }
  }
  grid::gTree(children = do.call(grid::gList, children), bounds = bounds, label_layout = label_layout,
    corners_mm = corners, name = "furniture-3d")
}
