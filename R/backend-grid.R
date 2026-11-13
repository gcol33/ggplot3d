grid_capabilities_3d <- function() {
  caps <- grDevices::dev.capabilities()
  list(vector = TRUE, clipping_paths = isTRUE(caps$clippingPaths),
       masks = length(caps$masks) > 0L && !all(is.na(caps$masks)),
       dash_phase = "physical segmented strokes", raster = FALSE)
}

scene_panel_grob <- function(packets, panel_params, theme, underlay = list(), overlay = list()) {
  registry <- panel_params$coord_params$frame_registry
  if (is.null(registry)) registry <- new.env(parent = emptyenv())
  if (panel_params$panel_id == 1L || is.null(registry$group)) registry$group <- new.env(parent = emptyenv())
  group <- registry$group
  group[[as.character(panel_params$panel_id)]] <- list(params = panel_params, theme = theme)
  grid::gTree(packets = packets, panel_params = panel_params, theme = theme,
    underlay = underlay, overlay = overlay, frame_group = group, cl = "scene_panel_grob", name = "scene-panel-3d")
}

#' @importFrom grid makeContent
#' @export
makeContent.scene_panel_grob <- function(x) {
  backend <- x$panel_params$coord_params$backend
  dpi <- if (identical(backend$type, "raster")) backend$dpi else mean(grDevices::dev.size("px") / grDevices::dev.size("in"))
  outer <- new_scene_viewport(grid::unit(1, "npc"), grid::unit(1, "npc"), dpi)
  furniture <- panel_furniture_3d(x$panel_params, x$theme, outer, as.list(x$frame_group))
  if (identical(backend$type, "raster")) {
    result <- render_raster_panel_3d(x, furniture, backend)
    scene <- result$scene
    rendered <- result$grob
  } else {
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
    scene <- scene_compile(packet, furniture$camera, furniture$viewport)
    rendered <- render_scene_grid(scene, furniture$viewport, light = x$panel_params$coord_params$light)
  }
  fit <- furniture$overlay$fit
  rendered$vp <- grid::viewport(x = grid::unit(fit$origin[1L], "mm"), y = grid::unit(fit$origin[2L], "mm"),
    width = grid::unit(furniture$viewport$width_mm, "mm"), height = grid::unit(furniture$viewport$height_mm, "mm"),
    just = c("left", "bottom"), clip = if (x$panel_params$camera$clip) "on" else "off", name = "scene-inner-3d")
  decorations <- furniture_overlay_3d(furniture, scene, x$theme)
  background <- ggplot2::element_render(x$theme, "panel.background")
  border <- ggplot2::element_render(x$theme, "panel.border", fill = NA)
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
  metrics <- paint_metrics_3d(styles)
  grid::gpar(col = colour, fill = fill, alpha = opacity,
    lwd = if (kind == 3L) metrics$point_lwd else metrics$line_lwd,
    lty = lty, fontsize = if (kind == 3L) metrics$fontsize else style_value_3d(styles, "fontsize", 11),
    fontfamily = style_value_3d(styles, "family", ""), fontface = style_value_3d(styles, "fontface", 1))
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
  digits * if (width_mm > 0) width_mm else 25.4 / 96
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
  f <- scene$fragments
  arrow <- style_value_3d(styles[1L, , drop = FALSE], "arrow", list(NULL))[[1L]]
  if (is.null(arrow)) return(grid::nullGrob())
  materials <- rep(seq_along(rows), each = 2L)
  start <- rep(c(TRUE, FALSE), length(rows))
  keep <- ifelse(start, f$arrow_start[rows[materials]], f$arrow_end[rows[materials]])
  materials <- materials[keep]; start <- start[keep]
  if (!length(materials)) return(grid::nullGrob())
  source <- rows[materials]
  tip <- f$offset[source] + ifelse(start, 0L, f$count[source] - 1L)
  adjacent <- tip + ifelse(start, 1L, -1L)
  x <- scene$vertices$x_npc[tip] * viewport$width_mm
  y <- scene$vertices$y_npc[tip] * viewport$height_mm
  dx <- x - scene$vertices$x_npc[adjacent] * viewport$width_mm
  dy <- y - scene$vertices$y_npc[adjacent] * viewport$height_mm
  magnitude <- sqrt(dx^2 + dy^2)
  keep <- magnitude > 0
  if (!any(keep)) return(grid::nullGrob())
  x <- x[keep]; y <- y[keep]; dx <- dx[keep] / magnitude[keep]; dy <- dy[keep] / magnitude[keep]
  materials <- materials[keep]
  length <- grid::convertWidth(arrow$length, "mm", TRUE)
  angle <- arrow$angle * pi / 180
  bx <- x - dx * length * cos(angle); by <- y - dy * length * cos(angle)
  xx <- as.vector(t(cbind(bx - dy * length * sin(angle), x, bx + dy * length * sin(angle))))
  yy <- as.vector(t(cbind(by + dx * length * sin(angle), y, by - dx * length * sin(angle))))
  gp <- material_gp_3d(styles[materials, , drop = FALSE], f$opacity[rows[materials]], 2L)
  gp$lty <- 1
  id <- rep(seq_along(materials), each = 3L)
  if (arrow$type == 2L) {
    gp$fill <- style_value_3d(styles[materials, , drop = FALSE], "arrow_fill", "black")
    grid::polygonGrob(xx, yy, default.units = "mm", id = id, gp = gp)
  } else grid::polylineGrob(xx, yy, default.units = "mm", id = id, gp = gp)
}

render_scene_grid <- function(scene, viewport, light = NULL) {
  f <- scene$fragments; order <- scene$draw_order
  styles <- scene$styles[match(f$style_id, scene$styles$style_id), , drop = FALSE]
  if (is.null(styles$colour)) styles$colour <- rep(NA_character_, nrow(styles))
  if (is.null(styles$fill)) styles$fill <- rep(NA_character_, nrow(styles))
  anchors <- if (is.null(styles$.anchor)) rep(FALSE, nrow(styles)) else !is.na(styles$.anchor) & styles$.anchor
  order <- order[!anchors[order]]
  caps <- grid_capabilities_3d()
  if (any(f$mask_count[order] > 0L) && !caps$clipping_paths) stop("The vector device does not support clipping paths required by scene fragment masks.", call. = FALSE)
  if (!length(order)) return(grid::gTree(children = grid::gList(), fragment_runs = list(), capabilities = caps))
  # Properties that grid cannot vectorise terminate a compatible run.
  key <- as.character(f$kind)
  lines <- which(f$kind == 2L)
  if (length(lines)) {
    source <- f$source_index[lines]; tab <- scene$source$polylines
    arrows <- style_value_3d(scene$styles, "arrow", list(NULL))
    arrow_key <- vapply(arrows, function(arrow) {
      if (is.null(arrow)) "" else paste(c(arrow$angle, as.character(arrow$length), arrow$type), collapse = ",")
    }, character(1))
    key[lines] <- paste(key[lines], tab$cap[source], tab$join[source], tab$mitre_limit[source],
      f$arrow_start[lines], f$arrow_end[lines], arrow_key[match(f$style_id[lines], scene$styles$style_id)], sep = ":")
    lty <- style_value_3d(styles, "linetype", 1)
    key[lines] <- paste(key[lines], lty[lines] %in% c("solid", "1") & !f$hidden[lines], sep = ":")
  }
  text <- which(f$kind == 4L)
  key[text] <- paste(key[text], scene$source$text$angle[f$source_index[text]], sep = ":")
  # Adjacent pieces of one glyph share one paint operation over their disjoint masks.
  masked <- which(f$mask_count > 0L)
  key[masked] <- paste0(key[masked], ":mask:", f$mask_offset[masked], ":", f$mask_count[masked])
  glyphs <- which(f$mask_count > 0L & f$kind %in% 2:4)
  key[glyphs] <- paste(f$kind[glyphs], "glyph", f$parent_primitive_id[glyphs], f$style_id[glyphs], f$source_index[glyphs], sep = ":")
  masked_lines <- intersect(glyphs, lines)
  key[masked_lines] <- paste(key[masked_lines], f$path_start_mm[masked_lines], f$path_end_mm[masked_lines], f$count[masked_lines], f$hidden[masked_lines], sep = ":")
  face <- f$kind == 1L
  seam <- face & f$layer_id > 0L & f$opacity == 1 & f$mask_count == 0L & is.na(styles$colour) & !is.na(styles$fill)
  key[face] <- paste(key[face], seam[face], sep = ":")
  run_id <- cumsum(c(TRUE, key[order][-1L] != head(key[order], -1L)))
  runs <- unname(split(order, run_id))
  grobs <- lapply(seq_along(runs), function(run) {
    masks <- runs[[run]]
    rows <- if (f$kind[masks[1L]] %in% 2:4 && f$mask_count[masks[1L]] > 0L) masks[1L] else masks
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
        # Fill-colour outlines cover shared-edge antialiasing within one device pixel.
        gp$col <- fill; gp$lwd <- 2 * 96 / viewport$dpi; gp$lty <- 1; gp$linejoin <- "round"
      }
      g <- grid::polygonGrob(x, y, id = rep(seq_along(counts), counts), gp = gp)
    } else if (kind == 2L) {
      tab <- scene$source$polylines; source <- f$source_index[rows]
      lty <- style_value_3d(s, "linetype", 1)
      if (all(lty %in% c("solid", "1") & !f$hidden[rows])) {
        path_x <- x; path_y <- y; path_counts <- counts; npaths <- rep.int(1L, length(rows))
      } else {
        paths <- lapply(seq_along(rows), function(j) {
          i <- seq.int(starts[j], length.out = counts[j])
          pattern <- if (f$hidden[rows[j]] && tab$hidden[source[j]] == 1L) "dashed" else lty[j]
          dash_segments_3d(x[i], y[i], f$path_start_mm[rows[j]], dash_pattern_3d(pattern, tab$width_mm[source[j]]), viewport)
        })
        npaths <- vapply(paths, function(p) length(p$count), integer(1))
        path_x <- unlist(lapply(paths, `[[`, "x")); path_y <- unlist(lapply(paths, `[[`, "y"))
        path_counts <- unlist(lapply(paths, `[[`, "count"))
        gp <- material_gp_3d(s[rep(seq_along(rows), npaths), , drop = FALSE], rep(f$opacity[rows], npaths), kind)
      }
      gp$lty <- 1; gp$lwd <- rep(tab$width_mm[source] * 96 / 25.4, npaths)
      gp$lineend <- c("butt", "round", "square")[tab$cap[source[1L]] + 1L]
      gp$linejoin <- c("round", "mitre", "bevel")[tab$join[source[1L]] + 1L]
      gp$linemitre <- tab$mitre_limit[source[1L]]
      g <- if (!sum(npaths)) grid::nullGrob() else grid::polylineGrob(
        path_x, path_y, default.units = "npc", id = rep(seq_len(sum(npaths)), path_counts), gp = gp)
      heads <- arrow_heads_3d(scene, masks, styles[masks, , drop = FALSE], viewport)
      if (!inherits(heads, "null")) {
        if (!is.null(clip)) { g$vp <- grid::viewport(clip = clip); clip <- NULL }
        g <- grid::grobTree(g, heads)
      }
    } else if (kind == 3L) {
      g <- grid::pointsGrob(x, y, default.units = "npc", pch = style_value_3d(s, "shape", 19), gp = gp)
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
