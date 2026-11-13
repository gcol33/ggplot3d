svg_number_3d <- function(x) formatC(x, digits = 12L, format = "fg", flag = "#")
svg_attributes_3d <- function(values) {
  if (!length(values)) return("")
  paste0(" ", names(values), '="', knit_escape_3d(as.character(values)), '"', collapse = "")
}
svg_element_3d <- function(tag, attributes = list(), content = NULL) {
  paste0("<", tag, svg_attributes_3d(attributes), if (is.null(content)) "/>" else paste0(">", paste(content, collapse = "\n"), "</", tag, ">"))
}
svg_path_3d <- function(x, y, id = rep(1L, length(x)), close = TRUE) {
  paste(vapply(split(seq_along(x), factor(id, unique(id))), function(i) {
    paste0("M", paste(paste(svg_number_3d(x[i]), svg_number_3d(y[i]), sep = ","), collapse = " L"), if (close) " Z" else "")
  }, character(1)), collapse = " ")
}
svg_grid_path_3d <- function(path, viewport, fill = path$gp$fill) {
  groups <- if (length(fill) > 1L) split(seq_along(path$x), path$pathId) else list(seq_along(path$x))
  rule <- if (identical(path$rule, "evenodd")) "evenodd" else "nonzero"
  paste(vapply(seq_along(groups), function(i) {
    rows <- groups[[i]]
    svg_element_3d("path", list(d = svg_path_3d(as.numeric(path$x)[rows] * viewport$width_mm,
      (1 - as.numeric(path$y)[rows]) * viewport$height_mm, path$id[rows]), fill = rep_len(fill, length(groups))[i],
      `fill-rule` = rule, `clip-rule` = rule, stroke = "none"))
  }, character(1)), collapse = "\n")
}

# A fully laid-out panel keeps its static furniture on either side of the
# native scene. The surrounding gtable supplies strips, titles and legends.
svg_chrome_part_3d <- function(grob, after = FALSE) {
  if (inherits(grob, "scene_panel_grob") && !is.null(grob$scene)) {
    at <- which(vapply(grob$children, function(g) identical(g$name, "scene-vector-3d"), logical(1)))
    scene_assert(length(at) == 1L, "A native vector panel needs one compiled scene")
    rows <- if (after) seq_along(grob$children) > at else seq_along(grob$children) < at
    grob$children <- do.call(grid::gList, as.list(grob$children[rows]))
    grob$childrenOrder <- names(grob$children)
    class(grob) <- setdiff(class(grob), c("scene_panel_grob", "forcedgrob"))
    return(grob)
  }
  if (inherits(grob, "gTree") && length(grob$children)) {
    grob$children <- do.call(grid::gList, lapply(grob$children, svg_chrome_part_3d, after = after))
    grob$childrenOrder <- names(grob$children)
  } else if (inherits(grob, "gtable")) {
    grob$grobs <- lapply(grob$grobs, svg_chrome_part_3d, after = after)
  } else if (after) return(grid::nullGrob())
  grob
}

svg_panels_3d <- function(grob) {
  if (inherits(grob, "scene_panel_grob") && !is.null(grob$scene)) return(list(grob))
  children <- if (inherits(grob, "gTree") && length(grob$children)) grob$children else if (inherits(grob, "gtable")) grob$grobs else list()
  unlist(lapply(children, svg_panels_3d), recursive = FALSE)
}

svg_chrome_3d <- function(table, width, height, prefix, after = FALSE) {
  svg <- svglite::svgstring(width = width / 96, height = height / 96, bg = "transparent", standalone = TRUE)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  grid::grid.newpage(); grid::grid.draw(svg_chrome_part_3d(table, after))
  grDevices::dev.off(device)
  doc <- xml2::read_xml(as.character(svg()))
  root <- xml2::xml_root(doc)
  xml2::xml_set_attr(root, "width", svg_number_3d(width / 96 * 25.4))
  xml2::xml_set_attr(root, "height", svg_number_3d(height / 96 * 25.4))
  tagged <- xml2::xml_find_all(root, ".//*[@id]")
  ids <- xml2::xml_attr(tagged, "id")
  xml2::xml_set_attr(tagged, "id", paste0(prefix, ids))
  for (node in xml2::xml_find_all(root, ".//*")) {
    attrs <- xml2::xml_attrs(node)
    for (name in names(attrs)) {
      value <- attrs[[name]]
      for (id in ids) {
        value <- gsub(paste0("url(#", id, ")"), paste0("url(#", prefix, id, ")"), value, fixed = TRUE)
        if (value == paste0("#", id)) value <- paste0("#", prefix, id)
      }
      if (!identical(value, attrs[[name]])) xml2::xml_set_attr(node, name, value)
    }
  }
  as.character(root, options = c("format", "no_declaration"))
}

svg_marker_3d <- function(x, y, style) {
  shape <- style_value_3d(style, "shape", 19)[1L]
  metrics <- paint_metrics_3d(style)
  r <- metrics$body_radius_mm[1L]
  colour <- style_value_3d(style, "colour", "black")[1L]
  fill <- if (is.numeric(shape) && shape >= 21 && shape <= 25) style_value_3d(style, "fill", NA_character_)[1L]
    else if (is.numeric(shape) && shape >= 15 && shape <= 20) colour else NA_character_
  attributes <- list(fill = if (is.na(fill)) "none" else fill, stroke = if (is.na(colour) || shape %in% 15:18) "none" else colour,
    `stroke-width` = svg_number_3d(2 * metrics$border_mm[1L]), `stroke-linejoin` = "miter")
  body <- marker_body_3d(shape, r)
  circle <- function() svg_element_3d("circle", c(list(cx = svg_number_3d(x), cy = svg_number_3d(y), r = svg_number_3d(r)), attributes))
  polygon <- function(points) svg_element_3d("path", c(list(d = svg_path_3d(x + points[, 1L], y - points[, 2L])), attributes))
  if (!is.null(body)) return(polygon(body))
  if (is.character(shape) || (is.numeric(shape) && (shape < 0 || shape > 25))) {
    label <- if (is.character(shape)) shape else intToUtf8(abs(shape))
    return(svg_element_3d("text", list(x = svg_number_3d(x), y = svg_number_3d(y), fill = colour,
      `font-size` = svg_number_3d(metrics$fontsize[1L] * 25.4 / 72), `text-anchor` = "middle", `dominant-baseline` = "central"), knit_escape_3d(label)))
  }
  if (shape %in% c(1, 16, 19, 20, 21)) return(circle())
  attributes$fill <- "none"
  plus <- cbind(c(-1, 1, 0, 0), c(0, 0, -1, 1)) * r
  cross <- cbind(c(-1, 1, -1, 1), c(-1, 1, 1, -1)) * r / sqrt(2)
  strokes <- function(points) svg_element_3d("path", c(list(d = svg_path_3d(x + points[, 1L], y - points[, 2L],
    rep(seq_len(nrow(points) / 2L), each = 2L), FALSE)), attributes))
  pieces <- list()
  if (shape %in% c(7, 12, 14)) pieces <- c(pieces, list(polygon(marker_body_3d(0, r))))
  if (shape == 9) pieces <- c(pieces, list(polygon(marker_body_3d(5, r))))
  if (shape %in% c(10, 13)) pieces <- c(pieces, list(circle()))
  if (shape %in% c(3, 8, 9, 10, 12)) pieces <- c(pieces, list(strokes(plus)))
  if (shape %in% c(4, 7, 8, 13)) pieces <- c(pieces, list(strokes(cross)))
  if (shape %in% c(11, 14)) pieces <- c(pieces, list(polygon(marker_body_3d(6, r))))
  if (shape == 11) pieces <- c(pieces, list(polygon(marker_body_3d(2, r))))
  paste(pieces, collapse = "\n")
}

svg_primitive_3d <- function(scene, row, viewport, fill, opacity, masks, layers, prefix, interactive) {
  f <- scene$fragments; s <- scene$styles[f$style_id[row], , drop = FALSE]
  indices <- f$offset[row] + seq_len(f$count[row]) - 1L
  x <- scene$vertices$x_npc[indices] * viewport$width_mm
  y <- (1 - scene$vertices$y_npc[indices]) * viewport$height_mm
  colour <- style_value_3d(s, "colour", NA_character_)[1L]
  content <- character()
  kind <- f$kind[row]
  if (kind == 1L) {
    content <- svg_element_3d("path", list(d = svg_path_3d(x, y), fill = fill, stroke = "none"))
  } else if (kind == 2L) {
    tab <- scene$source$polylines; source <- f$source_index[row]
    lty <- if (f$hidden[row] && tab$hidden[source] == 1L) "dashed" else style_value_3d(s, "linetype", 1)[1L]
    paths <- dash_segments_3d(x / viewport$width_mm, 1 - y / viewport$height_mm,
      f$path_start_mm[row], dash_pattern_3d(lty, tab$width_mm[source]), viewport)
    if (length(paths$count)) content <- svg_element_3d("path", list(d = svg_path_3d(paths$x * viewport$width_mm,
      (1 - paths$y) * viewport$height_mm, rep(seq_along(paths$count), paths$count), FALSE),
      fill = "none", stroke = colour, `stroke-width` = svg_number_3d(tab$width_mm[source]),
      `stroke-linecap` = c("butt", "round", "square")[tab$cap[source] + 1L],
      `stroke-linejoin` = c("round", "miter", "bevel")[tab$join[source] + 1L], `stroke-miterlimit` = tab$mitre_limit[source]))
    heads <- arrow_heads_3d(scene, row, s, viewport)
    if (!inherits(heads, "null")) content <- c(content, svg_element_3d("path", list(
      d = svg_path_3d(as.numeric(heads$x), viewport$height_mm - as.numeric(heads$y), close = inherits(heads, "polygon")),
      fill = if (inherits(heads, "polygon")) style_value_3d(s, "arrow_fill", colour)[1L] else "none",
      stroke = colour, `stroke-width` = svg_number_3d(tab$width_mm[source]))))
  } else if (kind == 3L) content <- svg_marker_3d(x[1L], y[1L], s)
  else {
    tab <- scene$source$text; source <- f$source_index[row]
    label <- text_label_3d(s)
    extent <- c(tab$width_mm[source], tab$height_mm[source])
    text <- svg_element_3d("text", list(x = svg_number_3d(x + (.5 - tab$hjust[source]) * extent[1L]),
      y = svg_number_3d(y - (.5 - tab$vjust[source]) * extent[2L]), fill = colour,
      `font-family` = style_value_3d(s, "family", "sans")[1L],
      `font-size` = svg_number_3d(style_value_3d(s, "fontsize", 11)[1L] * 25.4 / 72.27),
      `text-anchor` = "middle", `dominant-baseline` = "central"),
      knit_escape_3d(if (is.expression(label)) view_labels_3d(as.list(label)) else label))
    box <- character()
    if (is.finite(style_value_3d(s, "label_padding", NA_real_)[1L])) {
      border <- style_value_3d(s, "border_colour", NA_character_)[1L]
      box <- svg_element_3d("rect", list(x = svg_number_3d(x - tab$hjust[source] * extent[1L]),
        y = svg_number_3d(y - (1 - tab$vjust[source]) * extent[2L]), width = svg_number_3d(extent[1L]), height = svg_number_3d(extent[2L]),
        rx = svg_number_3d(style_value_3d(s, "label_r", 0)[1L]), fill = style_value_3d(s, "fill", "white")[1L],
        stroke = if (is.na(border)) "none" else border, `stroke-width` = svg_number_3d(paint_metrics_3d(s)$width_mm[1L])))
    }
    content <- svg_element_3d("g", list(transform = sprintf("rotate(%s %s %s)",
      svg_number_3d(-tab$angle[source]), svg_number_3d(x), svg_number_3d(y))), c(box, text))
  }
  layer <- f$layer_id[row]; source <- f$source_row[row]
  information <- Filter(function(x) x$layer == layer, layers)
  tooltip <- if (length(information)) information[[1L]]$tooltip[match(source, information[[1L]]$rows)] else NULL
  if (!length(tooltip) || is.na(tooltip)) tooltip <- paste("Layer", layer, "row", source)
  attrs <- list(class = if (layer > 0) "gg3d-primitive" else "gg3d-furniture",
    id = paste0(prefix, "f", row), `data-layer` = layer, `data-row` = source,
    `data-primitive` = f$parent_primitive_id[row], `data-kind` = kind, opacity = svg_number_3d(opacity))
  if (layer == 0) attrs$`pointer-events` <- "none"
  if (interactive && layer > 0) attrs$tabindex <- "0"
  if (!is.null(masks)) attrs$`clip-path` <- paste0("url(#", masks, ")")
  svg_element_3d("g", attrs, c(if (interactive && layer > 0) svg_element_3d("title", content = knit_escape_3d(tooltip)), content))
}

svg_scene_3d <- function(panel, layers, prefix, interactive) {
  scene <- panel$scene; f <- scene$fragments; viewport <- panel$inner_viewport
  vector <- panel$children[[which(vapply(panel$children, function(g) identical(g$name, "scene-vector-3d"), logical(1)))]]
  runs <- unlist(lapply(vector$fragment_runs, function(rows) unname(split(rows,
    cumsum(c(TRUE, diff(f$layer_id[rows]) != 0L))))), recursive = FALSE)
  fills <- style_value_3d(scene$styles[f$style_id, , drop = FALSE], "fill", NA_character_)
  shaded <- which(style_value_3d(scene$styles[f$style_id, , drop = FALSE], "shade", FALSE) %in% TRUE & f$kind == 1L)
  if (length(shaded)) fills[shaded] <- shade_fill_3d(fills[shaded], f, shaded, scene$camera,
    panel$panel_params$coord_params$light, scene$styles[f$style_id[shaded], , drop = FALSE])
  definitions <- character(); commands <- character()
  for (run in seq_along(runs)) {
    rows <- runs[[run]]; first <- rows[1L]; mask <- NULL
    if (f$mask_count[first] > 0) {
      mask <- paste0(prefix, "clip", run)
      path <- mask_path_3d(scene, rows)
      definitions <- c(definitions, svg_element_3d("clipPath", list(id = mask, clipPathUnits = "userSpaceOnUse"),
        svg_grid_path_3d(path, viewport, "black")))
      if (f$kind[first] %in% 2:4) rows <- first
    }
    seam <- length(rows) > 1L && all(f$kind[rows] == 1L & f$layer_id[rows] > 0 & f$mask_count[rows] == 0)
    underlay <- character(); opacity <- f$opacity[rows]
    outer <- list(`data-layer` = paste(unique(f$layer_id[rows]), collapse = " "), class = "gg3d-layer")
    if (seam) {
      counts <- f$count[rows]; indices <- rep(f$offset[rows], counts) + sequence(counts) - 1L
      paths <- seam_underlay_3d(scene$vertices$x_npc[indices], scene$vertices$y_npc[indices], rep(seq_along(rows), counts), fills[rows])
      if (all(opacity == 1)) underlay <- vapply(paths, svg_grid_path_3d, character(1), viewport = viewport)
      else {
        id <- paste0(prefix, "seam", run)
        definitions <- c(definitions, svg_element_3d("mask", list(id = id, maskUnits = "userSpaceOnUse",
          x = "-100%", y = "-100%", width = "300%", height = "300%", style = "mask-type:alpha"), svg_grid_path_3d(paths[[1L]], viewport, "white")))
        outer$mask <- paste0("url(#", id, ")"); outer$opacity <- svg_number_3d(opacity[1L]); opacity[] <- 1
        underlay <- svg_element_3d("rect", list(x = "-100%", y = "-100%", width = "300%", height = "300%", fill = paths[[1L]]$gp$fill))
      }
    }
    primitives <- vapply(seq_along(rows), function(i) svg_primitive_3d(scene, rows[i], viewport,
      fills[rows[i]], opacity[i], mask, layers, prefix, interactive), character(1))
    commands <- c(commands, svg_element_3d("g", outer, c(underlay, primitives)))
  }
  if (isTRUE(panel$panel_params$camera$clip)) {
    clip <- paste0(prefix, "panel")
    definitions <- c(definitions, svg_element_3d("clipPath", list(id = clip), svg_element_3d("rect",
      list(x = 0, y = 0, width = svg_number_3d(viewport$width_mm), height = svg_number_3d(viewport$height_mm)))))
    commands <- svg_element_3d("g", list(`clip-path` = paste0("url(#", clip, ")")), commands)
  }
  list(defs = definitions, content = commands, origin = as.numeric(c(vector$vp$x, vector$vp$y)))
}

native_svg_3d <- function(plot, width, height, interactive) {
  rlang::check_installed(c("svglite", "xml2"))
  motion_plot_coord_3d(plot);motion_dimensions_3d(width, height, 96)
  coord <- plot@coordinates
  plot@coordinates <- ggplot2::ggproto(NULL, coord)
  plot@coordinates$config$backend <- backend_3d("vector")
  device <- export_device_3d(width, height, 96)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  table <- grid::grid.force(ggplot2::ggplot_gtable(built))
  panels <- svg_panels_3d(table)
  scene_assert(length(panels) > 0, "plot has no compiled vector panels")
  information <- view_layers_3d(built, NULL, TRUE)
  W <- width / 96 * 25.4; H <- height / 96 * 25.4
  definitions <- commands <- character()
  for (i in seq_along(panels)) {
    panel <- panels[[i]]
    scene <- svg_scene_3d(panel, view_panel_layers_3d(information, panel$panel_params$panel_id), paste0("p", i, "-"), interactive)
    origin <- panel$device_origin_in * 25.4 + scene$origin
    definitions <- c(definitions, scene$defs)
    commands <- c(commands, svg_element_3d("g", list(`data-panel` = i,
      transform = sprintf("translate(%s %s)", svg_number_3d(origin[1L]), svg_number_3d(H - origin[2L] - panel$inner_viewport$height_mm))), scene$content))
  }
  grDevices::dev.off(device)
  css <- if (interactive) ".gg3d-primitive { outline:none; } .gg3d-primitive.gg3d-hover { filter:brightness(1.3); } .gg3d-primitive:focus { filter:brightness(1.3); }" else ""
  script <- if (interactive) paste0("(function(root){function hover(e,on){var mark=e.target.closest('.gg3d-primitive');if(!mark)return;",
    "root.querySelectorAll('.gg3d-primitive').forEach(function(p){if(p.dataset.layer===mark.dataset.layer&&p.dataset.row===mark.dataset.row)p.classList.toggle('gg3d-hover',on);});}",
    "root.addEventListener('pointerover',function(e){hover(e,true);});root.addEventListener('pointerout',function(e){hover(e,false);});})(document.currentScript.ownerSVGElement);")
  svg_element_3d("svg", list(xmlns = "http://www.w3.org/2000/svg", `xmlns:xlink` = "http://www.w3.org/1999/xlink", width = paste0(svg_number_3d(W), "mm"),
    height = paste0(svg_number_3d(H), "mm"), viewBox = paste(0, 0, svg_number_3d(W), svg_number_3d(H)), role = "img", `aria-labelledby` = "gg3d-title"),
    c(svg_element_3d("title", list(id = "gg3d-title"), knit_escape_3d(alt_text_3d(plot))),
      svg_element_3d("defs", content = definitions), svg_element_3d("style", content = css),
      svg_chrome_3d(table, width, height, "under-"), commands,
      svg_chrome_3d(table, width, height, "over-", TRUE), if (interactive) svg_element_3d("script", content = paste0("<![CDATA[", script, "]]>"))))
}

#' Save an SVG with source rows and hover text
#'
#' Writes compiled, depth-ordered fragments as SVG geometry. Each fragment
#' carries data-layer, data-row and data-primitive attributes. Consecutive
#' runs of each layer share a group so that interleaved layers retain their
#' visibility order. Scene masks become clipping paths, and surface seams
#' share compound-path underlays. Static plot furniture is embedded as SVG.
#' @param plot A ggplot with [coord_3d()].
#' @param file Output SVG path.
#' @param interactive Whether to include tooltips, keyboard focus and linked
#'   hover highlighting for fragments of the same source row.
#' @param width,height Plot dimensions in pixels at 96 pixels per inch.
#' @return `file`, invisibly.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp, tooltip = rownames(mtcars))) +
#'   geom_point_3d() + coord_3d()
#' if (requireNamespace("svglite", quietly = TRUE) && requireNamespace("xml2", quietly = TRUE)) {
#'   save_svg_3d(p, tempfile(fileext = ".svg"))
#' }
#' @export
save_svg_3d <- function(plot, file, interactive = TRUE, width = 640L, height = 640L) {
  scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && dir.exists(dirname(file)), "file must be an output path in an existing directory")
  interactive <- check_flag_3d(interactive, "interactive")
  writeLines(native_svg_3d(plot, width, height, interactive), file, useBytes = TRUE)
  invisible(file)
}
