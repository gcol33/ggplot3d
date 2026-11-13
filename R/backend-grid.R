grid_capabilities_3d <- function() {
  caps <- grDevices::dev.capabilities()
  list(vector = TRUE, clipping_paths = isTRUE(caps$clippingPaths),
       masks = length(caps$masks) > 0L && !all(is.na(caps$masks)),
       dash_phase = "physical segmented strokes", raster = FALSE)
}

scene_panel_grob <- function(packets, panel_params, theme, underlay = list(), overlay = list()) {
  grid::gTree(packets = packets, panel_params = panel_params, theme = theme,
    underlay = underlay, overlay = overlay, cl = "scene_panel_grob", name = "scene-panel-3d")
}

#' @importFrom grid makeContent
#' @export
makeContent.scene_panel_grob <- function(x) {
  backend <- x$panel_params$coord_params$backend
  if (!identical(backend$type, "vector")) stop("The grid backend supports vector output; raster output requires the frustumR backend adapter.", call. = FALSE)
  dpi <- mean(grDevices::dev.size("px") / grDevices::dev.size("in"))
  outer <- new_scene_viewport(grid::unit(1, "npc"), grid::unit(1, "npc"), dpi)
  furniture <- panel_furniture_3d(x$panel_params, x$theme, outer)
  packet <- merge_scene_packets(c(list(furniture$packet), x$packets), x$panel_params$panel_id)
  # Device text metrics are authoritative at the physical-size boundary.
  if (length(packet$text$style_id)) {
    for (i in seq_along(packet$text$style_id)) {
      row <- match(packet$text$style_id[i], packet$styles$style_id)
      if (isTRUE(packet$styles$.anchor[row])) next
      s <- packet$styles[row, , drop = FALSE]
      g <- grid::textGrob(text_label_3d(s), gp = material_gp_3d(s, 1, 4L))
      packet$text$width_mm[i] <- grid::convertWidth(grid::grobWidth(g), "mm", TRUE)
      packet$text$height_mm[i] <- grid::convertHeight(grid::grobHeight(g), "mm", TRUE)
    }
  }
  scene <- scene_compile(packet, x$panel_params$camera, furniture$viewport)
  rendered <- render_scene_grid(scene, furniture$viewport, light = x$panel_params$coord_params$light)
  fit <- furniture$overlay$fit
  rendered$vp <- grid::viewport(x = grid::unit(fit$origin[1L], "mm"), y = grid::unit(fit$origin[2L], "mm"),
    width = grid::unit(furniture$viewport$width_mm, "mm"), height = grid::unit(furniture$viewport$height_mm, "mm"),
    just = c("left", "bottom"), clip = if (x$panel_params$camera$clip) "on" else "off", name = "scene-inner-3d")
  decorations <- furniture_overlay_3d(furniture, scene, x$theme)
  background <- ggplot2::element_render(x$theme, "panel.background")
  border <- ggplot2::element_render(x$theme, "panel.border")
  children <- c(list(background), x$underlay, list(rendered, decorations, border), x$overlay)
  x$scene <- scene; x$furniture <- decorations; x$inner_viewport <- furniture$viewport; x$outer_viewport <- outer
  grid::setChildren(x, do.call(grid::gList, children))
}

style_value_3d <- function(styles, name, default) {
  value <- styles[[name]]
  if (is.null(value)) return(rep_len(default, nrow(styles)))
  if (is.list(value)) return(lapply(value, function(x) {
    if (length(x) == 1L && is.atomic(x) && is.na(x)) return(default[[1L]])
    x
  }))
  value[is.na(value)] <- default
  value
}

text_label_3d <- function(styles) {
  label <- style_value_3d(styles, "label", "")
  if (is.list(label)) label <- as.expression(lapply(label, function(x) if (is.expression(x)) x[[1L]] else x))
  label
}

material_gp_3d <- function(styles, opacity, kind, fill = NULL) {
  colour <- if (is.null(styles$colour)) rep(NA_character_, nrow(styles)) else styles$colour
  if (is.null(fill)) fill <- if (is.null(styles$fill)) rep(NA_character_, nrow(styles)) else styles$fill
  lty <- style_value_3d(styles, "linetype", 1)
  if (is.character(lty)) {
    number <- match(lty, as.character(0:6))
    lty[!is.na(number)] <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[number[!is.na(number)]]
  }
  grid::gpar(col = colour, fill = fill, alpha = opacity,
    lwd = style_value_3d(styles, if (kind == 3L) "stroke" else "linewidth", 0.5) * 72.27 / 25.4,
    lty = lty, fontsize = style_value_3d(styles, "fontsize", 11),
    fontfamily = style_value_3d(styles, "family", ""), fontface = style_value_3d(styles, "fontface", 1))
}

shade_fill_3d <- function(fill, fragments, rows, camera, light) {
  if (is.null(light) || !length(rows)) return(fill)
  normal <- cbind(fragments$normal_x[rows], fragments$normal_y[rows], fragments$normal_z[rows])
  direction <- light$direction
  if (light$space == "camera") direction <- camera$right * direction[1L] + camera$up * direction[2L] - camera$forward * direction[3L]
  if (light$space == "data") direction <- direction * camera$model_scale
  direction <- direction / sqrt(sum(direction^2))
  toward <- -camera$forward
  normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, ] <- -normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, , drop = FALSE]
  brightness <- light$ambient + light$diffuse * pmax(0, drop(normal %*% direction))
  rgb <- t(grDevices::col2rgb(fill)) / 255
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  linear <- if (light$mode == "multiply") linear * brightness else 1 - (1 - linear) * brightness
  rgb <- ifelse(linear <= 0.0031308, 12.92 * linear, 1.055 * linear^(1 / 2.4) - 0.055)
  result <- grDevices::rgb(pmin(1, pmax(0, rgb[, 1L])), pmin(1, pmax(0, rgb[, 2L])), pmin(1, pmax(0, rgb[, 3L])))
  result[is.na(fill)] <- NA_character_
  result
}

mask_path_3d <- function(scene, rows) {
  f <- scene$fragments
  rows <- rows[!duplicated(paste(f$mask_offset[rows], f$mask_count[rows], sep = ":"))]
  counts <- f$mask_count[rows]
  i <- rep(f$mask_offset[rows], counts) + sequence(counts) - 1L
  ring <- paste(rep(seq_along(rows), counts), scene$masks$ring[i], sep = ":")
  grid::pathGrob(scene$masks$x_npc[i], scene$masks$y_npc[i],
    id = match(ring, unique(ring)), rule = "evenodd")
}

dash_pattern_3d <- function(lty, width_mm) {
  named <- c(blank = "0", solid = "1", dashed = "44", dotted = "13", dotdash = "1343", longdash = "73", twodash = "2262")
  key <- as.character(lty)
  if (key %in% names(named)) key <- named[[key]]
  else if (key %in% as.character(0:6)) key <- unname(named[as.integer(key) + 1L])
  if (key == "0") return(numeric())
  if (key == "1") return(Inf)
  chars <- strsplit(toupper(key), "", fixed = TRUE)[[1L]]
  digits <- match(chars, c(as.character(0:9), LETTERS[1:6])) - 1L
  if (anyNA(digits) || length(digits) %% 2L || any(digits == 0)) stop("Invalid line dash pattern.", call. = FALSE)
  digits * if (width_mm > 0) width_mm * 72.27 / 96 else 25.4 / 96
}

dash_segments_3d <- function(x, y, start, pattern, viewport) {
  if (!length(pattern)) return(list(x = numeric(), y = numeric(), count = integer()))
  if (is.infinite(pattern[1L])) return(list(x = x, y = y, count = length(x)))
  distance <- sqrt(diff(x * viewport$width_mm)^2 + diff(y * viewport$height_mm)^2)
  cumulative <- c(0, cumsum(distance)); total <- sum(distance)
  if (total == 0) return(list(x = numeric(), y = numeric(), count = integer()))
  period <- sum(pattern); phase <- start %% period
  j <- which(cumsum(pattern) > phase)[1L]
  cursor <- 0; remaining <- cumsum(pattern)[j] - phase
  xx <- yy <- numeric(); counts <- integer()
  while (cursor < total) {
    end <- min(total, cursor + remaining)
    if (j %% 2L == 1L && end > cursor) {
      at <- c(cursor, cumulative[cumulative > cursor & cumulative < end], end)
      xx <- c(xx, stats::approx(cumulative, x, at, ties = "ordered")$y)
      yy <- c(yy, stats::approx(cumulative, y, at, ties = "ordered")$y)
      counts <- c(counts, length(at))
    }
    cursor <- end; j <- j %% length(pattern) + 1L; remaining <- pattern[j]
  }
  list(x = xx, y = yy, count = counts)
}

arrow_heads_3d <- function(scene, rows, styles, viewport) {
  f <- scene$fragments; heads <- list(); materials <- integer()
  arrow <- style_value_3d(styles[1L, , drop = FALSE], "arrow", list(NULL))[[1L]]
  if (is.null(arrow)) return(grid::nullGrob())
  length <- grid::convertWidth(arrow$length, "mm", TRUE)
  angle <- arrow$angle * pi / 180
  for (j in seq_along(rows)) {
    row <- rows[j]; indices <- seq.int(f$offset[row], length.out = f$count[row])
    xy <- cbind(scene$vertices$x_npc[indices] * viewport$width_mm, scene$vertices$y_npc[indices] * viewport$height_mm)
    for (start in c(TRUE, FALSE)) {
      if (!(if (start) f$arrow_start[row] else f$arrow_end[row])) next
      pair <- if (start) c(1L, 2L) else c(nrow(xy), nrow(xy) - 1L)
      tip <- xy[pair[1L], ]; direction <- tip - xy[pair[2L], ]
      magnitude <- sqrt(sum(direction^2)); if (magnitude == 0) next
      direction <- direction / magnitude
      normal <- c(-direction[2L], direction[1L])
      base <- tip - direction * length * cos(angle)
      heads[[length(heads) + 1L]] <- rbind(base + normal * length * sin(angle), tip, base - normal * length * sin(angle))
      materials <- c(materials, j)
    }
  }
  if (!length(heads)) return(grid::nullGrob())
  xy <- do.call(rbind, heads); gp <- material_gp_3d(styles[materials, , drop = FALSE], f$opacity[rows[materials]], 2L)
  gp$lty <- 1
  id <- rep(seq_along(heads), each = 3L)
  if (arrow$type == 2L) {
    gp$fill <- style_value_3d(styles[materials, , drop = FALSE], "arrow_fill", "black")
    grid::polygonGrob(xy[, 1L], xy[, 2L], default.units = "mm", id = id, gp = gp)
  } else grid::polylineGrob(xy[, 1L], xy[, 2L], default.units = "mm", id = id, gp = gp)
}

opaque_silhouette_3d <- function(scene, rows) {
  f <- scene$fragments; counts <- f$count[rows]
  indices <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  x <- scene$vertices$x_npc[indices]; y <- scene$vertices$y_npc[indices]
  starts <- cumsum(c(1L, head(counts, -1L))); ends <- cumsum(counts)
  next_vertex <- seq_along(x) + 1L; next_vertex[ends] <- starts
  area <- as.vector(rowsum(x * y[next_vertex] - y * x[next_vertex], rep(seq_along(rows), counts), reorder = FALSE))
  reverse <- rep(area < 0, counts)
  a <- ifelse(reverse, next_vertex, seq_along(x)); b <- ifelse(reverse, seq_along(x), next_vertex)
  # Keys identify coincident coverage edges; the drawn coordinates retain full precision.
  key <- paste(sprintf("%.17g", x), sprintf("%.17g", y), sep = ":")
  vertex <- match(key, unique(key))
  va <- vertex[a]; vb <- vertex[b]
  group <- match(paste(pmin(va, vb), pmax(va, vb), sep = ":"), unique(paste(pmin(va, vb), pmax(va, vb), sep = ":")))
  winding <- as.vector(rowsum(ifelse(va < vb, 1L, -1L), group, reorder = FALSE))
  keep <- winding[group] != 0 & va != vb
  a <- a[keep]; b <- b[keep]; va <- va[keep]; vb <- vb[keep]
  outgoing <- split(seq_along(a), va)
  used <- rep(FALSE, length(a)); path <- integer(length(a)); ring_counts <- integer(length(a))
  n <- 0L; nr <- 0L
  for (first in seq_along(a)) {
    if (used[first]) next
    before <- n; current <- first
    repeat {
      if (used[current]) break
      used[current] <- TRUE; n <- n + 1L; path[n] <- a[current]
      candidates <- outgoing[[as.character(vb[current])]]
      candidates <- candidates[!used[candidates]]
      if (!length(candidates)) break
      current <- candidates[1L]
    }
    nr <- nr + 1L; ring_counts[nr] <- n - before
  }
  indices <- path[seq_len(n)]
  grid::pathGrob(x[indices], y[indices], id.lengths = ring_counts[seq_len(nr)], rule = "winding")
}

render_scene_grid <- function(scene, viewport, light = NULL) {
  f <- scene$fragments; order <- scene$draw_order
  styles <- scene$styles[match(f$style_id, scene$styles$style_id), , drop = FALSE]
  anchors <- if (is.null(styles$.anchor)) rep(FALSE, nrow(styles)) else !is.na(styles$.anchor) & styles$.anchor
  order <- order[!anchors[order]]
  caps <- grid_capabilities_3d()
  if (any(f$mask_count[order] > 0L) && !caps$clipping_paths) stop("The vector device does not support clipping paths required by scene fragment masks.", call. = FALSE)
  if (!length(order)) return(grid::gTree(children = grid::gList(), fragment_runs = list(), capabilities = caps))
  # Properties that grid cannot vectorise terminate a compatible run.
  key <- as.character(f$kind)
  for (i in which(f$kind == 2L)) {
    source <- f$source_index[i]; tab <- scene$source$polylines
    arrow <- style_value_3d(styles[i, , drop = FALSE], "arrow", list(NULL))[[1L]]
    arrow_key <- if (is.null(arrow)) "" else paste(c(arrow$angle, as.character(arrow$length), arrow$type), collapse = ",")
    key[i] <- paste(key[i], tab$cap[source], tab$join[source], tab$mitre_limit[source],
      f$arrow_start[i], f$arrow_end[i], arrow_key, sep = ":")
  }
  for (i in which(f$kind == 4L)) key[i] <- paste(key[i], scene$source$text$angle[f$source_index[i]], sep = ":")
  # Adjacent pieces of one glyph share one paint operation over their disjoint masks.
  masked <- which(f$mask_count > 0L)
  key[masked] <- paste0(key[masked], ":mask:", f$mask_offset[masked], ":", f$mask_count[masked])
  glyphs <- which(f$mask_count > 0L & f$kind %in% 3:4)
  key[glyphs] <- paste(f$kind[glyphs], "glyph", f$parent_primitive_id[glyphs], f$style_id[glyphs], f$source_index[glyphs], sep = ":")
  face <- f$kind == 1L
  seam <- face & f$layer_id > 0L & f$opacity == 1 & f$mask_count == 0L & is.na(styles$colour) & !is.na(styles$fill)
  key[face] <- paste(key[face], seam[face], ifelse(seam[face], f$layer_id[face], 0L), sep = ":")
  silhouettes <- lapply(split(which(seam), f$layer_id[seam]), function(rows) opaque_silhouette_3d(scene, rows))
  run_id <- cumsum(c(TRUE, key[order][-1L] != head(key[order], -1L)))
  runs <- unname(split(order, run_id))
  grobs <- lapply(seq_along(runs), function(run) {
    masks <- runs[[run]]
    rows <- if (f$kind[masks[1L]] %in% 3:4 && f$mask_count[masks[1L]] > 0L) masks[1L] else masks
    kind <- f$kind[rows[1L]]; s <- styles[rows, , drop = FALSE]
    counts <- f$count[rows]; starts <- cumsum(c(1L, head(counts, -1L)))
    indices <- rep(f$offset[rows], counts) + sequence(counts) - 1L
    x <- scene$vertices$x_npc[indices]; y <- scene$vertices$y_npc[indices]
    gp <- material_gp_3d(s, f$opacity[rows], kind)
    clip <- NULL
    if (f$mask_count[rows[1L]] > 0L) clip <- mask_path_3d(scene, masks)
    if (kind == 1L) {
      fill <- s$fill
      shade <- style_value_3d(s, "shade", FALSE)
      if (any(shade)) fill[shade] <- shade_fill_3d(fill[shade], f, rows[shade], scene$camera, light)
      gp$fill <- fill
      if (all(seam[rows])) {
        if (!caps$clipping_paths) stop("Seam-free opaque vector meshes require a device supporting clipping paths.", call. = FALSE)
        # A layer silhouette closes cracks across draw runs and retains exterior coverage.
        clip <- silhouettes[[as.character(f$layer_id[rows[1L]])]]
        gp$col <- fill; gp$lwd <- rep(2 * 96 / viewport$dpi, length(rows)); gp$lty <- 1
      }
      g <- grid::polygonGrob(x, y, id = rep(seq_along(counts), counts), gp = gp)
    } else if (kind == 2L) {
      tab <- scene$source$polylines; source <- f$source_index[rows]
      paths <- lapply(seq_along(rows), function(j) {
        i <- seq.int(starts[j], length.out = counts[j])
        lty <- style_value_3d(s[j, , drop = FALSE], "linetype", 1)
        if (f$hidden[rows[j]]) lty <- "dashed"
        dash_segments_3d(x[i], y[i], f$path_start_mm[rows[j]], dash_pattern_3d(lty, tab$width_mm[source[j]]), viewport)
      })
      npaths <- vapply(paths, function(p) length(p$count), integer(1))
      gp <- material_gp_3d(s[rep(seq_along(rows), npaths), , drop = FALSE], rep(f$opacity[rows], npaths), kind)
      gp$lty <- 1; gp$lwd <- rep(tab$width_mm[source] * 72.27 / 25.4, npaths)
      gp$lineend <- c("butt", "round", "square")[tab$cap[source[1L]] + 1L]
      gp$linejoin <- c("round", "mitre", "bevel")[tab$join[source[1L]] + 1L]
      gp$linemitre <- tab$mitre_limit[source[1L]]
      g <- if (!sum(npaths)) grid::nullGrob() else grid::polylineGrob(
        unlist(lapply(paths, `[[`, "x")), unlist(lapply(paths, `[[`, "y")),
        id = rep(seq_len(sum(npaths)), unlist(lapply(paths, `[[`, "count"))), gp = gp)
      heads <- arrow_heads_3d(scene, rows, s, viewport)
      if (!inherits(heads, "null")) g <- grid::grobTree(g, heads)
    } else if (kind == 3L) {
      g <- grid::pointsGrob(x, y, pch = style_value_3d(s, "shape", 19), size = grid::unit(style_value_3d(s, "size", 1.5), "mm"), gp = gp)
    } else {
      tab <- scene$source$text; source <- f$source_index[rows]
      label <- text_label_3d(s)
      g <- grid::textGrob(label, x, y, hjust = tab$hjust[source], vjust = tab$vjust[source], rot = tab$angle[source[1L]], gp = gp)
    }
    if (!is.null(clip)) g$vp <- grid::viewport(clip = clip)
    g$name <- paste0("scene-run-", run)
    g
  })
  grid::gTree(children = do.call(grid::gList, grobs), fragment_runs = runs, capabilities = caps, name = "scene-vector-3d")
}
