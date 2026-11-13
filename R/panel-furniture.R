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
  reserve <- extent + margin
  width <- viewport$width_mm - reserve[2L] - reserve[4L]
  height <- viewport$height_mm - reserve[1L] - reserve[3L]
  if (!is.finite(width) || !is.finite(height) || min(width, height) <= 0) stop("3D panel too small for measured axis labels and titles; enlarge the device or reduce text size.", call. = FALSE)
  list(metrics = metrics, reserve = reserve,
    viewport = new_scene_viewport(width, height, viewport$dpi),
    origin = c(reserve[4L], reserve[3L]))
}

panel_furniture_3d <- function(panel_params, theme, viewport) {
  fit <- measure_guides_3d(panel_params, theme, viewport)
  cube <- cube_edges_3d()
  vertices <- cube$corners
  primitives <- list(); kinds <- character(); materials <- list(); anchor_rows <- integer()
  add <- function(xyz, kind, element = NULL, partition = 0L, anchor = FALSE) {
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
  # Camera-side signs determine rear walls; projection and edge choice use engine anchors.
  az <- panel_params$camera$azimuth * pi / 180
  rear <- c(if (cos(az) >= 0) "xmin" else "xmax", if (sin(az) >= 0) "ymin" else "ymax")
  rear_edges <- vapply(seq_len(nrow(cube$edges)), function(i) {
    xyz <- cube$corners[cube$edges[i, c("a", "b")], , drop = FALSE]
    any(vapply(rear, function(p) all(xyz[, match(substr(p, 1, 1), c("x", "y", "z"))] == as.integer(substr(p, 2, 4) == "max")), logical(1)))
  }, logical(1))
  panes <- unique(c(setdiff(cfg$panes, c("back", "floor")), if ("back" %in% cfg$panes) rear, if ("floor" %in% cfg$panes) "zmin"))
  for (pane in panes) {
    normal <- match(substr(pane, 1, 1), c("x", "y", "z"))
    side <- as.integer(substr(pane, 2, 4) == "max")
    other <- setdiff(seq_len(3L), normal)
    xyz <- matrix(0, 4L, 3L); xyz[, normal] <- side
    xyz[, other] <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
    element <- element_3d(paste0("panel.pane.", c("x", "y", "z")[normal]), theme)
    # Floor and explicit camera-side panes participate in occlusion.
    partition <- if (pane %in% rear && panel_params$camera$clip_data) 0L else 1L
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
    element <- element_3d("panel.box.back", theme)
    if (visible_element_3d(element)) for (i in seq_len(nrow(cube$edges))) {
      e <- cube$edges[i, ]; xyz <- cube$corners[e[c("a", "b")], , drop = FALSE]
      if (rear_edges[i]) add(xyz, "polylines", element)
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
      layer_id = rep.int(0L, length(i)), source_row = rep.int(0L, length(i)),
      partition = vapply(materials[i], `[[`, integer(1), "partition"), opacity = opacity[i])
    if (kind == "polylines") tab$width_mm <- styles$linewidth[i]
    tab
  }); names(tables) <- c("faces", "polylines", "text")
  geometry <- lapply(as.data.frame(vertices), as.double); names(geometry) <- c("x", "y", "z")
  packet <- do.call(new_scene_packet, c(list(vertices = geometry,
    corners = list(vertex = as.integer(unlist(primitives)), source_row = rep.int(0L, sum(counts))), styles = styles,
    panel_id = panel_params$panel_id), tables))
  names(packet$vertices) <- c("x", "y", "z")
  list(packet = packet, overlay = list(cube = cube, rear_edges = rear_edges, corners = corner_queries, guides = guide_queries,
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

furniture_overlay_3d <- function(furniture, scene, theme) {
  meta <- furniture$overlay; fit <- meta$fit; params <- meta$params
  # Furniture styles are unique query keys and occupy the first merged style range.
  query <- function(ids) {
    f <- scene$fragments
    rows <- match(ids, f$style_id)
    v <- f$offset[rows]
    cbind(scene$vertices$x_npc[v] * fit$viewport$width_mm + fit$origin[1L],
          scene$vertices$y_npc[v] * fit$viewport$height_mm + fit$origin[2L], scene$vertices$depth[v])
  }
  corners <- query(meta$corners)
  if (any(!is.finite(corners)) && (length(params$coord_params$axes) || params$coord_params$box == "all")) {
    stop("3D axis and front-frame placement requires unclipped cube projection queries for this camera; schema 1 does not retain these anchors outside the scene rectangle.", call. = FALSE)
  }
  children <- list(); bounds <- list(); occupied <- matrix(numeric(), 0, 4L)
  add <- function(g) children[[length(children) + 1L]] <<- g
  line <- function(a, b, element) {
    if (visible_element_3d(element)) add(grid::segmentsGrob(a[, 1L], a[, 2L], b[, 1L], b[, 2L], default.units = "mm", gp = line_gp_3d(element)))
  }
  if (params$coord_params$box == "all") {
    edges <- meta$cube$edges
    front <- !meta$rear_edges
    line(corners[edges[front, "a"], 1:2, drop = FALSE], corners[edges[front, "b"], 1:2, drop = FALSE], element_3d("panel.box.front", theme))
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
      support <- (abs(normal[1L]) * m$label[, 1L] + abs(normal[2L]) * m$label[, 2L]) / 2
      offset <- m$tick_length + m$gap + support + ((seq_len(n) - 1L) %% guide$params$n.dodge) * m$label_extent
      locations <- major + outer(offset, normal)
      keep <- rep(TRUE, n)
      for (i in seq_len(n)) {
        box <- c(locations[i, ] - m$label[i, ] / 2, locations[i, ] + m$label[i, ] / 2)
        overlap <- nrow(occupied) && any(occupied[, 1L] < box[3L] & occupied[, 3L] > box[1L] & occupied[, 2L] < box[4L] & occupied[, 4L] > box[2L])
        if (isTRUE(guide$params$check.overlap) && overlap) keep[i] <- FALSE
        else { occupied <- rbind(occupied, box); bounds[[length(bounds) + 1L]] <- box }
      }
      add(grid::textGrob(guide$labels[keep], locations[keep, 1L], locations[keep, 2L], default.units = "mm", rot = m$angle, gp = text_gp_3d(m$text)))
    }
    if (length(guide$title) && visible_element_3d(m$title)) {
      size <- m$title_size[1L, ]
      support <- size[2L] / 2
      offset <- m$tick_length + m$gap + guide$params$n.dodge * m$label_extent + m$title_gap + support
      location <- (a + b) / 2 + normal * offset
      angle <- atan2(b[2L] - a[2L], b[1L] - a[1L]) * 180 / pi
      if (angle > 90) angle <- angle - 180
      if (angle < -90) angle <- angle + 180
      theta <- angle * pi / 180
      extent <- c(abs(cos(theta)) * size[1L] + abs(sin(theta)) * size[2L], abs(sin(theta)) * size[1L] + abs(cos(theta)) * size[2L])
      box <- c(location - extent / 2, location + extent / 2)
      bounds[[length(bounds) + 1L]] <- box
      add(grid::textGrob(guide$title, location[1L], location[2L], default.units = "mm", rot = angle, gp = text_gp_3d(m$title)))
    }
  }
  grid::gTree(children = do.call(grid::gList, children), bounds = bounds, corners_mm = corners, name = "furniture-3d")
}
