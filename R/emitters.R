.emitters_3d <- new.env(parent = emptyenv())

#' Register a geometry emitter
#' @param geom A ggplot2 geom prototype.
#' @param emit Function accepting data, params and panel_params.
#' @param aesthetics Additional supported aesthetics.
#' @param setup_data Optional geometry setup function.
#' @param stat_adapter Optional stat prototype.
#' @param position_check Optional position validation function.
#' @param replace Whether to replace an existing registration.
#' @export
register_geom_3d <- function(geom, emit, aesthetics = character(), setup_data = NULL,
                             stat_adapter = NULL, position_check = NULL, replace = FALSE) {
  stopifnot(inherits(geom, "Geom"), is.function(emit))
  name <- class(geom)[1L]
  if (exists(name, .emitters_3d, inherits = FALSE) && !replace) stop("Emitter already registered for ", name, ".", call. = FALSE)
  .emitters_3d[[name]] <- list(name = name, emit = emit, aesthetics = aesthetics,
                              setup_data = setup_data, stat_adapter = stat_adapter, position_check = position_check)
  invisible(NULL)
}

find_emitter_3d <- function(geom) {
  if (!is.null(geom$scene_entry)) return(geom$scene_entry)
  for (name in class(geom)) {
    entry <- .emitters_3d[[name]]
    if (!is.null(entry)) return(entry)
  }
  NULL
}

scene_geom_3d <- function(parent, entry) {
  ggplot2::ggproto(NULL, parent,
    scene_entry = entry,
    required_aes = if (entry$name == "GeomSegment") c("x", "y") else parent$required_aes,
    optional_aes = union(parent$optional_aes, c(z_aesthetics, entry$aesthetics)),
    extra_params = union(parent$extra_params, c(".emitter", ".layer_id", "occlusion", "shade", "hidden", "method", "max_edge", "wireframe")),
    handle_na = function(data, params) data,
    setup_data = function(data, params) {
      if (entry$name == "GeomSegment") {
        for (axis in c("x", "y", "z")) if (is.null(data[[paste0(axis, "end")]])) data[[paste0(axis, "end")]] <- data[[axis]]
      }
      if (!is.null(entry$setup_data)) data <- entry$setup_data(data, params)
      else if (entry$name != "GeomSegment") data <- parent$setup_data(data, params)
      data
    },
    draw_panel = function(data, panel_params, coord, .emitter = entry$name, .layer_id = 1L,
                           na.rm = FALSE, occlusion = "depth", shade = FALSE, hidden = "hide",
                           method = "grid", max_edge = Inf, wireframe = FALSE,
                           lineend = "butt", linejoin = "round", linemitre = 10,
                           arrow = NULL, arrow.fill = NULL, parse = FALSE, check_overlap = FALSE,
                           size.unit = "mm", rule = "evenodd") {
      if (!inherits(coord, "Coord3D")) stop("3D layers require coord_3d().", call. = FALSE)
      params <- list(.emitter = .emitter, na.rm = na.rm, occlusion = occlusion,
                     shade = shade, hidden = hidden, method = method, max_edge = max_edge,
                     wireframe = wireframe, lineend = lineend, linejoin = linejoin,
                     linemitre = linemitre, arrow = arrow, arrow.fill = arrow.fill,
                     parse = parse, check_overlap = check_overlap, size.unit = size.unit, rule = rule)
      scene_layer_grob(emit_scene_3d(data, params, panel_params, .layer_id))
    }
  )
}

emit_scene_3d <- function(data, params, panel_params, layer_id) {
  entry <- .emitters_3d[[params$.emitter]]
  if (is.null(entry)) stop("Unknown 3D emitter: ", params$.emitter, call. = FALSE)
  if (is.null(data$.source)) data$.source <- seq_len(nrow(data))
  data$.layer <- rep.int(as.integer(layer_id), nrow(data))
  params$occlusion <- match.arg(params$occlusion %or3d% "depth", c("depth", "overlay"))
  params$hidden <- match.arg(params$hidden %or3d% "hide", c("hide", "dashed", "show"))
  if (entry$name %in% c("GeomPoint", "GeomText") && params$hidden != "hide") stop("Hidden-line policies require stroke geometry.", call. = FALSE)
  packet <- entry$emit(normalise_positions_3d(data, panel_params), params, panel_params)
  validate_scene_packet(packet)
  packet
}

`%or3d%` <- function(x, y) if (is.null(x)) y else x

styles_3d <- function(data, rows, kinds, params) {
  defaults <- list(colour = "black", fill = NA_character_, alpha = 1, linewidth = 0.5,
                   size = 1.5, shape = 19, linetype = 1, label = "", family = "", fontface = 1,
                   hjust = 0.5, vjust = 0.5, angle = 0, stroke = 0.5)
  styles <- data.frame(style_id = seq_along(rows))
  for (name in names(defaults)) styles[[name]] <- if (name %in% names(data)) data[[name]][rows] else rep(defaults[[name]], length(rows))
  styles$alpha[is.na(styles$alpha)] <- 1
  chosen <- ifelse(kinds == "faces", styles$fill, styles$colour)
  chosen[kinds == "markers" & styles$shape %in% 21:25 & !is.na(styles$fill)] <- styles$fill[kinds == "markers" & styles$shape %in% 21:25 & !is.na(styles$fill)]
  rgba <- grDevices::col2rgb(chosen, alpha = TRUE)
  opacity <- as.double(styles$alpha * rgba[4L, ] / 255)
  for (name in c("colour", "fill")) {
    rgb <- grDevices::col2rgb(styles[[name]], alpha = TRUE)
    styles[[name]] <- ifelse(is.na(styles[[name]]), NA_character_, grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255))
  }
  styles$alpha <- 1
  styles$shade <- isTRUE(params$shade)
  styles$arrow <- I(rep(list(params$arrow), length(rows)))
  styles$arrow_fill <- params$arrow.fill %or3d% styles$colour
  styles$fontsize <- styles$size * switch(params$size.unit %or3d% "mm", mm = 72.27 / 25.4, pt = 1, cm = 72.27 / 2.54, "in" = 72.27, pc = 12, stop("Unsupported text size unit.", call. = FALSE))
  if (isTRUE(params$parse)) styles$label <- I(lapply(as.character(styles$label), function(x) parse(text = x)))
  list(styles = styles, opacity = opacity)
}

packet_from_primitives_3d <- function(data, primitives, kinds, params, panel_id) {
  if (!length(primitives)) return(new_scene_packet(panel_id = panel_id))
  rows <- vapply(primitives, `[`, integer(1), 1L)
  material <- styles_3d(data, rows, kinds, params)
  counts <- lengths(primitives)
  offsets <- cumsum(c(1L, counts[-length(counts)]))
  used <- sort(unique(unlist(primitives, use.names = FALSE)))
  vertices <- lapply(data[used, c("x", "y", "z"), drop = FALSE], as.double)
  corners <- list(vertex = match(unlist(primitives, use.names = FALSE), used),
                  source_row = as.integer(data$.source[unlist(primitives, use.names = FALSE)]))
  tables <- list()
  for (kind in unique(kinds)) {
    i <- which(kinds == kind)
    tables[[kind]] <- list(primitive_id = i, layer_id = data$.layer[rows[i]],
      source_row = as.integer(data$.source[rows[i]]), style_id = i, offset = offsets[i], count = counts[i],
      partition = rep.int(if (identical(params$occlusion, "overlay")) 2L else 1L, length(i)), opacity = material$opacity[i])
    if (kind == "polylines") {
      tables[[kind]]$width_mm <- material$styles$linewidth[i]
      tables[[kind]]$cap <- rep.int(match(params$lineend %or3d% "butt", c("butt", "round", "square")) - 1L, length(i))
      tables[[kind]]$join <- rep.int(match(params$linejoin %or3d% "round", c("round", "mitre", "bevel")) - 1L, length(i))
      tables[[kind]]$mitre_limit <- rep(params$linemitre %or3d% 10, length(i))
      tables[[kind]]$hidden <- rep.int(match(params$hidden %or3d% "hide", c("hide", "dashed", "show")) - 1L, length(i))
      arrow <- params$arrow
      tables[[kind]]$arrow_start <- rep(!is.null(arrow) && arrow$ends %in% c(1L, 3L), length(i))
      tables[[kind]]$arrow_end <- rep(!is.null(arrow) && arrow$ends %in% c(2L, 3L), length(i))
    }
    if (kind == "markers") tables[[kind]]$footprint_id <- seq_along(i)
    if (kind == "text") {
      for (key in c("hjust", "vjust", "angle")) tables[[kind]][[key]] <- material$styles[[key]][i]
      metrics <- lapply(i, function(j) grid::textGrob(material$styles$label[j], gp = grid::gpar(fontsize = material$styles$fontsize[j], fontfamily = material$styles$family[j], fontface = material$styles$fontface[j])))
      tables[[kind]]$width_mm <- vapply(metrics, function(g) grid::convertWidth(grid::grobWidth(g), "mm", TRUE), numeric(1))
      tables[[kind]]$height_mm <- vapply(metrics, function(g) grid::convertHeight(grid::grobHeight(g), "mm", TRUE), numeric(1))
    }
  }
  if ("markers" %in% kinds) {
    i <- which(kinds == "markers")
    radius <- material$styles$size[i] / 2 + material$styles$stroke[i] / 2
    tables$footprints <- list(kind = rep.int(1L, length(i)), offset = rep.int(1L, length(i)), count = rep.int(0L, length(i)),
                              radius_x_mm = radius, radius_y_mm = radius, angle = rep(0, length(i)))
  }
  do.call(new_scene_packet, c(list(vertices = vertices, corners = corners), tables,
                             list(styles = material$styles, panel_id = panel_id)))
}

valid_rows_3d <- function(data, params, columns = c("x", "y", "z")) {
  valid <- Reduce(`&`, lapply(data[columns], is.finite))
  if (any(!valid) && !isTRUE(params$na.rm)) warning("Removed ", sum(!valid), " rows with missing 3D positions.", call. = FALSE)
  valid
}

emit_points_3d <- function(data, params, panel_params) {
  if (is.character(data$shape)) {
    identity_coord <- ggplot2::ggproto(NULL, ggplot2::Coord, transform = function(data, panel_params) data)
    data$shape <- ggplot2::GeomPoint$draw_panel(data, panel_params, identity_coord)$pch
  }
  rows <- which(valid_rows_3d(data, params))
  packet_from_primitives_3d(data, as.list(rows), rep("markers", length(rows)), params, panel_params$panel_id)
}

emit_text_3d <- function(data, params, panel_params) {
  rows <- which(valid_rows_3d(data, params) & !is.na(data$label))
  packet_from_primitives_3d(data, as.list(rows), rep("text", length(rows)), params, panel_params$panel_id)
}

emit_paths_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params)
  primitives <- list()
  fields <- intersect(c("colour", "linewidth", "alpha", "linetype"), names(data))
  materials <- if (length(fields)) do.call(paste, c(data[fields], sep = "\r")) else rep("", nrow(data))
  for (group in unique(data$group)) {
    rows <- which(data$group == group)
    run <- integer()
    for (j in seq_len(max(0L, length(rows) - 1L))) {
      pair <- rows[c(j, j + 1L)]
      connected <- all(valid[pair])
      same <- length(run) && connected && materials[pair[1L]] == materials[run[1L]]
      if (length(run) && !same) {
        primitives[[length(primitives) + 1L]] <- run
        run <- integer()
      }
      if (connected) run <- if (length(run)) c(run, pair[2L]) else pair
    }
    if (length(run)) primitives[[length(primitives) + 1L]] <- run
  }
  packet_from_primitives_3d(data, primitives, rep("polylines", length(primitives)), params, panel_params$panel_id)
}

emit_segments_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend"))
  data <- data[valid, , drop = FALSE]
  ends <- data
  ends[c("x", "y", "z")] <- data[c("xend", "yend", "zend")]
  all <- rbind(data, ends)
  n <- nrow(data)
  primitives <- lapply(seq_len(n), function(i) c(i, i + n))
  packet_from_primitives_3d(all, primitives, rep("polylines", n), params, panel_params$panel_id)
}

merge_scene_packets <- function(packets, panel_id) {
  if (!length(packets)) return(new_scene_packet(panel_id = panel_id))
  lapply(packets, validate_scene_packet)
  result <- new_scene_packet(panel_id = panel_id)
  primitive_next <- 0L
  for (packet in packets) {
    shift <- c(vertex = length(result$vertices$x), corner = length(result$corners$vertex),
               footprint = length(result$footprints$kind), footprint_vertex = length(result$footprint_vertices$x_mm),
               style = nrow(result$styles))
    packet$corners$vertex <- packet$corners$vertex + shift[["vertex"]]
    packet$footprints$offset <- ifelse(packet$footprints$count == 0L, 1L, packet$footprints$offset + shift[["footprint_vertex"]])
    packet$footprints$offset <- as.integer(packet$footprints$offset)
    ids <- unlist(lapply(packet[c("faces", "polylines", "markers", "text")], `[[`, "primitive_id"), use.names = FALSE)
    for (kind in c("faces", "polylines", "markers", "text")) {
      tab <- packet[[kind]]
      tab$primitive_id <- as.integer(match(tab$primitive_id, ids) + primitive_next)
      tab$offset <- as.integer(tab$offset + shift[["corner"]])
      tab$style_id <- as.integer(match(tab$style_id, packet$styles$style_id) + shift[["style"]])
      if (kind == "markers") tab$footprint_id <- as.integer(tab$footprint_id + shift[["footprint"]])
      packet[[kind]] <- tab
    }
    primitive_next <- primitive_next + length(ids)
    for (kind in names(scene_schemas())) {
      for (column in names(result[[kind]])) result[[kind]][[column]] <- c(result[[kind]][[column]], packet[[kind]][[column]])
    }
    packet$styles$style_id <- as.integer(seq_len(nrow(packet$styles)) + shift[["style"]])
    if (!nrow(result$styles)) result$styles <- packet$styles
    else if (nrow(packet$styles)) {
      columns <- union(names(result$styles), names(packet$styles))
      for (column in setdiff(columns, names(result$styles))) result$styles[[column]] <- NA
      for (column in setdiff(columns, names(packet$styles))) packet$styles[[column]] <- NA
      result$styles <- rbind(result$styles[columns], packet$styles[columns])
    }
  }
  result$style_count <- as.integer(nrow(result$styles))
  validate_scene_packet(result)
  result
}

collect_scene_layers <- function(panel, panel_params, theme) {
  result <- list(packets = list(), underlay = list(), overlay = list())
  for (grob in panel) {
    if (is.null(grob) || inherits(grob, "zeroGrob") || inherits(grob, "null")) next
    if (inherits(grob, "scene_layer_grob")) result$packets[[length(result$packets) + 1L]] <- grob$packet
    else if (inherits(grob, "scene_underlay_grob")) result$underlay[[length(result$underlay) + 1L]] <- grob
    else if (inherits(grob, "scene_overlay_grob")) result$overlay[[length(result$overlay) + 1L]] <- grob
    else stop("Unsupported nonempty panel grob: ", class(grob)[1L], ". Register a 3D emitter or explicit decoration adapter.", call. = FALSE)
  }
  result
}

register_geom_3d(ggplot2::GeomPoint, emit_points_3d)
register_geom_3d(ggplot2::GeomPath, emit_paths_3d)
register_geom_3d(ggplot2::GeomLine, emit_paths_3d)
register_geom_3d(ggplot2::GeomSegment, emit_segments_3d)
register_geom_3d(ggplot2::GeomText, emit_text_3d)
