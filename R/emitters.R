.emitters_3d <- new.env(parent = emptyenv())

solid_defaults_3d <- function() ggplot2::aes(colour = NA, fill = "grey70", linewidth = 0.3, linetype = 1, alpha = 1)

# Smallest positive gap between distinct finite values: the default cell size of
# a lattice of centres.
lattice_spacing_3d <- function(v, default = 1) {
  d <- diff(sort(unique(v[is.finite(v)])))
  d <- d[d > 0]
  if (length(d)) min(d) else default
}

paint_metrics_3d <- function(styles) {
  point_lwd <- style_value_3d(styles, "stroke", 0.5) * 96 / 25.4 / 2
  fontsize <- style_value_3d(styles, "size", 1.5) * 72.27 / 25.4 + point_lwd
  line_lwd <- style_value_3d(styles, "linewidth", 0.5) * 72.27 / 25.4
  shape <- style_value_3d(styles, "shape", 19)
  radius <- fontsize * 25.4 / 72 * ifelse(shape == 20, .25, .375)
  border <- point_lwd * 25.4 / 192 * !(shape %in% 15:18)
  if (!is.null(styles$colour)) border[is.na(styles$colour)] <- 0
  list(point_lwd = point_lwd, fontsize = fontsize, line_lwd = line_lwd,
       width_mm = line_lwd * 25.4 / 96,
       radius_mm = radius + border, body_radius_mm = radius, border_mm = border)
}

# Body outline in mm of a square, diamond or triangle point shape (pch 0, 2,
# 5, 6, 15, 17, 18, 22 to 25) of body radius r; NULL for round shapes.
marker_body_3d <- function(shape, r) {
  if (shape %in% c(0,15,22)) {
    r <- r * if (shape == 22) sqrt(pi / 4) else 1
    return(cbind(c(-1,1,1,-1), c(-1,-1,1,1)) * r)
  }
  if (shape %in% c(5,18,23)) {
    r <- r * if (shape == 23) sqrt(pi / 2) else if (shape == 5) sqrt(2) else 1
    return(cbind(c(0,1,0,-1),c(-1,0,1,0)) * r)
  }
  if (!shape %in% c(2,6,17,24,25)) return(NULL)
  r <- r * sqrt(4 * pi / (3 * sqrt(3)))
  xy <- cbind(c(0,-sqrt(3)/2,sqrt(3)/2),c(1,-.5,-.5)) * r
  if (shape %in% c(6,25)) xy[,2L] <- -xy[,2L]
  xy
}

marker_footprints_3d <- function(styles) {
  metrics <- paint_metrics_3d(styles)
  shapes <- style_value_3d(styles, "shape", 19)
  keys <- paste(shapes, metrics$body_radius_mm, metrics$border_mm, sep = ":")
  chosen <- which(!duplicated(keys)); index <- match(keys, keys[chosen])
  shapes <- shapes[chosen]
  radii <- metrics$radius_mm[chosen]
  footprint <- list(kind = rep.int(1L, length(chosen)), offset = rep.int(1L, length(chosen)),
    count = rep.int(0L, length(chosen)), radius_x_mm = radii, radius_y_mm = radii, angle = rep(0, length(chosen)))
  vertices <- list(x_mm = numeric(), y_mm = numeric(), ring = integer())
  for (i in which(shapes %in% c(0,2,5,6,15,17,18,22:25))) {
    shape <- shapes[i]; border <- metrics$border_mm[chosen[i]]
    xy <- marker_body_3d(shape, metrics$body_radius_mm[chosen[i]])
    centre <- colMeans(xy)
    area <- sum(xy[,1L] * xy[c(2:nrow(xy),1L),2L] - xy[,2L] * xy[c(2:nrow(xy),1L),1L])
    if (area < 0) xy <- xy[nrow(xy):1L,,drop=FALSE]
    next_xy <- xy[c(2:nrow(xy),1L),,drop=FALSE]
    delta <- next_xy-xy; lengths <- sqrt(rowSums(delta^2))
    normals <- cbind(delta[,2L],-delta[,1L])/lengths
    previous <- normals[c(nrow(xy),seq_len(nrow(xy)-1L)),,drop=FALSE]
    offsets <- (normals + previous) / (1 + rowSums(normals * previous))
    outer <- xy + border * offsets
    rings <- list(outer)
    if (shape %in% c(0,2,5,6)) {
      inner <- xy - border * offsets
      if (all(rowSums((inner-centre) * (xy-centre)) > 0)) rings[[2L]] <- inner
    }
    polygon <- do.call(rbind,rings)
    footprint$kind[i] <- 2L; footprint$offset[i] <- length(vertices$x_mm)+1L; footprint$count[i] <- nrow(polygon)
    vertices$x_mm <- c(vertices$x_mm,polygon[,1L]); vertices$y_mm <- c(vertices$y_mm,polygon[,2L])
    vertices$ring <- c(vertices$ring,rep(seq_along(rings),vapply(rings,nrow,integer(1))))
  }
  list(index=index,footprints=footprint,vertices=vertices)
}

# Kinds of adjustment an emitter accepts: "nudge", "jitter", "dodge",
# "stack" (stack and fill) and "planar" (ggplot2's own x/y adjustments).
# Identity and position_project_3d() are accepted by every geometry.
position_kinds_3d <- c("nudge", "jitter", "dodge", "stack", "planar")

# Columns holding a geometry's interval along one axis: lower (NA when the
# interval starts at zero), upper, and the anchor position (NA when absent).
extent_3d <- function(lower = NA_character_, upper, centre = NA_character_) {
  stopifnot(is.character(upper), length(upper) == 1L, !is.na(upper),
            length(lower) == 1L, length(centre) == 1L)
  c(lower = as.character(lower), upper = upper, centre = as.character(centre))
}

#' Register a geometry emitter
#' @param space Optional function of data, params and panel_params returning a
#'   space specification. Layer-owned canonical geometry then shares coordinate
#'   munching and remunches against the final camera and viewport.
#' @param geom A ggplot2 geom prototype.
#' @param emit Function accepting data, params and panel_params.
#' @param aesthetics Additional supported aesthetics.
#' @param setup_data Optional geometry setup function.
#' @param stat_adapter Optional stat prototype.
#' @param position_check Optional extra position validation, a function of
#'   the position and the panel's scales that signals an error.
#' @param positions Position adjustments the geometry accepts besides
#'   identity: any of `"nudge"` ([position_nudge_3d()]), `"jitter"`
#'   ([position_jitter_3d()]), `"dodge"` ([position_dodge_3d()]), `"stack"`
#'   ([position_stack_3d()], [position_fill_3d()]) and `"planar"` (ggplot2's
#'   own nudge, jitter and dodge, which move x and y only).
#' @param extents Named list, by axis `"x"`, `"y"`, `"z"`, of the columns
#'   holding the geometry's interval along that axis, as character vectors
#'   `c(lower = , upper = , centre = )`; `lower` or `centre` may be `NA`.
#'   Stacking and dodging read and rewrite these columns. An axis not listed
#'   takes its own coordinate as both the interval end and the anchor.
#' @param replace Whether to replace an existing registration.
#' @param prepare_data,prepare_aesthetics Optional preparation functions applied
#'   to every layer drawing the geom inside [coord_3d()]: `prepare_data(data)`
#'   receives the layer data, `prepare_aesthetics(out, data, plot, params)` the
#'   evaluated aesthetics with the layer data, plot and layer parameters.
#' @param parameters Named list of further drawing parameters the geom accepts,
#'   with their defaults. They reach the emitter in `params`.
#' @param default_z Height of a layer without a z mapping. The default 0
#'   trains the z scale; `-Inf` places the layer on the floor without training it.
#' @param tween How [tween_3d()] pairs the layer's rows between two states.
#'   `"row"`: each row is one mark (points, text, columns), paired by group
#'   and order. `"path"` and `"polygon"`: the rows of a group form an open or
#'   closed line, resampled by arc length when the two states differ in
#'   length. `"group"` (default): the rows of a group form one shape (a
#'   surface, a mesh) that morphs row by row when its row count is unchanged
#'   and cross-fades otherwise.
#' @return NULL invisibly. Registers the emitter for subsequent layers.
#' @details
#' The emitter receives scale-transformed positions normalised to the panel box.
#' It returns a scene packet from [new_scene_packet()]. Vertices hold x, y, z;
#' corners index vertices; primitive offsets index corners. Style IDs index the
#' style table. Source rows and layer IDs preserve input identity.
#' @examples
#' GeomTriangle3D <- ggplot2::ggproto("GeomTriangle3D", ggplot2::Geom,
#'   required_aes = c("x", "y", "z"),
#'   default_aes = ggplot2::aes(fill = "steelblue", alpha = 1),
#'   draw_key = ggplot2::draw_key_polygon)
#' emit_triangle <- function(data, params, panel_params) {
#'   stopifnot(nrow(data) == 3L)
#'   new_scene_packet(
#'     vertices = list(x = as.double(data$x), y = as.double(data$y),
#'                     z = as.double(data$z)),
#'     corners = list(vertex = 1:3, source_row = as.integer(data$.source)),
#'     faces = list(offset = 1L, count = 3L, style_id = 1L,
#'       source_row = as.integer(data$.source[1]),
#'       layer_id = as.integer(data$.layer[1]), opacity = data$alpha[1]),
#'     styles = data.frame(style_id = 1L, fill = data$fill[1],
#'                         colour = NA_character_, shade = FALSE),
#'     panel_id = panel_params$panel_id)
#' }
#' register_geom_3d(GeomTriangle3D, emit_triangle, replace = TRUE)
#' triangle <- ggplot2::layer(geom = GeomTriangle3D, stat = "identity",
#'   position = "identity", params = list())
#' d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 1))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   layer_3d(triangle) + coord_3d()
#' @export
register_geom_3d <- function(geom, emit, aesthetics = character(), setup_data = NULL,
                             stat_adapter = NULL, position_check = NULL, replace = FALSE,
                             prepare_data = NULL, prepare_aesthetics = NULL, parameters = list(),
                             default_z = 0, positions = "nudge", extents = list(),
                             tween = c("group", "row", "path", "polygon"), space = NULL) {
  tween <- match.arg(tween)
  stopifnot(inherits(geom, "Geom"), is.function(emit), is.list(parameters),
            is.null(space) || is.function(space),
            is.character(positions), all(positions %in% position_kinds_3d),
            is.list(extents), all(names(extents) %in% c("x", "y", "z")),
            all(vapply(extents, function(e) is.character(e) && setequal(names(e), c("lower", "upper", "centre")), logical(1))),
            !length(parameters) || !is.null(names(parameters)),
            is.numeric(default_z), length(default_z) == 1L, !is.na(default_z))
  name <- class(geom)[1L]
  if (exists(name, .emitters_3d, inherits = FALSE) && !replace) stop("Emitter already registered for ", name, ".", call. = FALSE)
  .emitters_3d[[name]] <- list(name = name, geom = geom, emit = emit, aesthetics = aesthetics,
                              setup_data = setup_data, stat_adapter = stat_adapter, position_check = position_check,
                              prepare_data = prepare_data, prepare_aesthetics = prepare_aesthetics,
                              parameters = parameters, default_z = default_z,
                              positions = positions, extents = extents, tween = tween, space = space)
  invisible(NULL)
}

# Drawing parameters of the geoms that draw triangulated surfaces.
surface_parameters_3d <- list(outline = NULL, alpha_mode = "face", shading = "flat", material = NULL)

shading_mode_3d <- function(shading) {
  if (!is.character(shading) || length(shading) != 1L || !(shading %in% c("flat", "smooth")))
    stop("shading must be \"flat\" or \"smooth\".", call. = FALSE)
  shading
}

# Per-corner attributes of a smooth-shaded layer: the unit vertex normals of
# face corners and, where the fill varies within a face, the encoded sRGB fill
# channels of every face corner. Other corners carry NA.
smooth_corners_3d <- function(data, primitives, kinds, counts) {
  owner <- rep.int(seq_along(counts), counts)
  face <- kinds[owner] == "faces"
  out <- list()
  if (all(c("nx", "ny", "nz") %in% names(data))) {
    normal <- matrix(NA_real_, length(primitives), 3L)
    n <- cbind(data$nx, data$ny, data$nz)[primitives[face], , drop = FALSE]
    size <- sqrt(rowSums(n^2))
    good <- is.finite(size) & size > 0
    n[good, ] <- n[good, , drop = FALSE] / size[good]
    n[!good, ] <- NA_real_
    normal[face, ] <- n
    out[c("normal_x", "normal_y", "normal_z")] <- lapply(1:3, function(k) normal[, k])
  }
  fill <- data$fill
  if (!is.null(fill) && any(face)) {
    key <- match(fill[primitives], unique(fill[primitives]))
    varies <- tapply(key[face], owner[face], function(k) any(k != k[1L]))
    if (any(varies)) {
      rgb <- matrix(NA_real_, length(primitives), 3L)
      painted <- face & !is.na(fill[primitives])
      rgb[painted, ] <- t(colour_channels_3d(fill[primitives[painted]])) / 255
      out[c("red", "green", "blue")] <- lapply(1:3, function(k) rgb[, k])
    }
  }
  out
}

# Drawing parameters every 3D geom accepts, with their defaults.
scene_draw_formals_3d <- function() {
  alist(data = , panel_params = , coord = , .emitter = , .layer_id = 1L,
    na.rm = FALSE, occlusion = "depth", shade = FALSE, hidden = "hide",
    method = "grid", max_edge = Inf, wireframe = FALSE,
    lineend = "butt", linejoin = "round", linemitre = 10,
    arrow = NULL, arrow.fill = NULL, parse = FALSE, check_overlap = FALSE,
    size.unit = "mm", rule = "evenodd", base = 0, cap = "both",
    width = NULL, depth = NULL, face = "zmin", .project = NULL,
    render = "marker", edl = 1, raster = FALSE, clip_planes = NULL)
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
    optional_aes = union(parent$optional_aes, c(z_aesthetics, interactive_aesthetics_3d, entry$aesthetics, ".tween_group",
      if (entry$name == "GeomSegment") c("xend", "yend"))),
    extra_params = union(parent$extra_params, c(".emitter", ".layer_id", ".project", ".key_context", "occlusion", "shade", "hidden", "method", "max_edge", "wireframe", "render", "edl", "raster", "clip_planes")),
    handle_na = function(data, params) data,
    setup_data = function(data, params) {
      if (entry$name == "GeomSegment") {
        for (axis in c("x", "y", "z")) if (is.null(data[[paste0(axis, "end")]])) data[[paste0(axis, "end")]] <- data[[axis]]
      }
      if (!is.null(entry$setup_data)) data <- entry$setup_data(data, params)
      else if (entry$name != "GeomSegment") data <- parent$setup_data(data, params)
      data
    },
    draw_panel = scene_draw_panel_3d(entry)
  )
}

# The formals of draw_panel are the parameters ggplot2 passes to the geom: the
# common set and the parameters registered with the emitter.
scene_draw_panel_3d <- function(entry) {
  draw <- function() {
    params <- as.list(environment(), all.names = TRUE)
    if (!inherits(params$coord, "Coord3D")) stop("3D layers require coord_3d().", call. = FALSE)
    drawn <- emit_scene_3d(params$data, params[setdiff(names(params), c("data", "panel_params", "coord", ".layer_id"))],
      params$panel_params, params$.layer_id)
    if (inherits(drawn, "grob")) return(drawn)
    # A packet resolved at draw time stays in the panel while still empty.
    if (isTRUE(attr(drawn, "draw_time"))) grid::gTree(packet = drawn, cl = "scene_layer_grob") else scene_layer_grob(drawn)
  }
  arguments <- scene_draw_formals_3d()
  arguments$.emitter <- entry$name
  arguments[names(entry$parameters)] <- entry$parameters
  formals(draw) <- arguments
  draw
}

# An emitter returns a scene packet, or an annotation grob drawn over the scene.
emit_scene_3d <- function(data, params, panel_params, layer_id) {
  check_3d_build(!is.null(panel_params$camera) && !is.null(panel_params$ranges$z))
  entry <- .emitters_3d[[params$.emitter]]
  if (is.null(entry)) stop("Unknown 3D emitter: ", params$.emitter, call. = FALSE)
  if (is.function(entry$space)) {
    panel_params$space_context <- bind_space_3d(entry$space(data, params, panel_params), panel_params)
  }
  recipe <- list(data = data, params = params, panel_params = panel_params, layer_id = layer_id)
  if (is.null(data$.source)) data$.source <- seq_len(nrow(data))
  if (!is.null(data$.origin)) data$.source <- data$.origin
  data$.layer <- rep.int(as.integer(layer_id), nrow(data))
  params$occlusion <- match.arg(params$occlusion %or3d% "depth", c("depth", "overlay"))
  params$hidden <- match.arg(params$hidden %or3d% "hide", c("hide", "dashed", "show"))
  if (entry$name %in% c("GeomPoint", "GeomText") && params$hidden != "hide") stop("Hidden-line policies require stroke geometry.", call. = FALSE)
  params$render <- match.arg(params$render %or3d% "marker", c("marker", "splat"))
  if (params$render == "splat" && entry$name != "GeomPoint") stop("render = \"splat\" requires a point layer.", call. = FALSE)
  edl <- params$edl %or3d% 1
  if (!is.numeric(edl) || length(edl) != 1L || !is.finite(edl) || edl < 0) stop("edl must be a nonnegative number.", call. = FALSE)
  raster <- params$raster %or3d% FALSE
  if (!is.logical(raster) || length(raster) != 1L || is.na(raster)) stop("raster must be TRUE or FALSE.", call. = FALSE)
  data <- normalise_positions_3d(data, panel_params)
  params$.space_context <- panel_params$space_context
  params$.space_munch <- panel_params$space_munch
  params$clip_planes <- layer_clip_planes_3d(params$clip_planes, panel_params)
  packet <- if (is.null(params$.project)) entry$emit(data, params, panel_params) else emit_projected_3d(entry, data, params, panel_params)
  if (inherits(packet, "grob")) return(packet)
  if (!is.null(panel_params$space_context) && !isTRUE(attr(packet, "space_mapped")) && !isTRUE(attr(packet, "model_space"))) {
    packet <- space_packet_3d(packet, panel_params$space_context)
  }
  unstyled <- packet
  if (!is.null(panel_params$space_context)) packet <- panel_params$space_context$space$style_packet(packet, panel_params$space_context)
  camera_free <- isTRUE(panel_params$space_context$linear_action) && isTRUE(attr(packet, "space_mapped"))
  if (camera_free) attr(packet, "space_unstyled") <- unstyled
  validate_scene_packet(packet)
  if (raster && nrow(packet$styles)) packet$styles$raster <- TRUE
  frame <- frame_of_3d(panel_params)
  # Packets already placed in model space, such as rigid glyphs, skip the
  # frame; outside the identity frame they are cut by their anchors instead.
  model_space <- isTRUE(attr(packet, "model_space"))
  if (length(params$clip_planes) && (!model_space || isTRUE(frame$identity))) {
    packet <- clip_packet_planes_3d(packet, params$clip_planes)
    attr(packet, "parent") <- NULL
  }
  if ((!is.null(panel_params$space_context) && !camera_free) || isTRUE(attr(packet, "draw_time"))) attr(packet, "draw_recipe") <- recipe
  if (model_space) return(packet)
  frame_packet_3d(frame, packet, panel_params$camera$clip_data)
}

`%or3d%` <- function(x, y) if (is.null(x)) y else x

alpha_mode_3d <- function(mode) {
  if (!is.character(mode) || length(mode) != 1L || !(mode %in% c("face", "surface")))
    stop("alpha_mode must be \"face\" or \"surface\".", call. = FALSE)
  mode
}

# Sheet of each face: the connected component of the faces that share a vertex
# position. The label is the smallest face index of the component, reached by
# taking the minimum over shared vertices and then following labels to their
# own label until nothing changes.
face_sheets_3d <- function(x, y, z, face) {
  o <- order(x, y, z, method = "radix")
  vertex <- integer(length(x))
  vertex[o] <- cumsum(c(TRUE, diff(x[o]) != 0 | diff(y[o]) != 0 | diff(z[o]) != 0))
  label <- seq_len(max(face))
  repeat {
    corner <- label[face]
    o <- order(vertex, corner, method = "radix"); first <- o[!duplicated(vertex[o])]
    smallest <- integer(max(vertex)); smallest[vertex[first]] <- corner[first]
    via <- smallest[vertex]
    o <- order(face, via, method = "radix"); first <- o[!duplicated(face[o])]
    updated <- label; updated[face[first]] <- pmin(label[face[first]], via[first])
    repeat {
      jumped <- updated[updated]
      if (identical(jumped, updated)) break
      updated <- jumped
    }
    if (identical(updated, label)) return(label)
    label <- updated
  }
}

styles_3d <- function(data, rows, kinds, params, normals = NULL, sheets = NULL) {
  defaults <- list(colour = "black", fill = NA_character_, alpha = 1, linewidth = 0.5,
                   size = 1.5, shape = 19, linetype = 1, label = "", family = "", fontface = 1,
                   hjust = 0.5, vjust = 0.5, angle = 0, stroke = 0.5)
  styles <- data.frame(style_id = seq_along(rows))
  for (name in names(defaults)) styles[[name]] <- if (name %in% names(data)) data[[name]][rows] else rep(defaults[[name]], length(rows))
  # Label boxes behind text: padding and corner radius in mm, border colour.
  for (name in intersect(c("label_padding", "label_r", "border_colour"), names(data))) styles[[name]] <- data[[name]][rows]
  styles$colour[kinds == "faces"] <- NA_character_
  styles$alpha[is.na(styles$alpha)] <- 1
  if (!is.null(normals)) styles[c("normal_x", "normal_y", "normal_z")] <- as.data.frame(normals)
  if (!is.null(params$alpha_mode)) styles$alpha_mode <- rep(alpha_mode_3d(params$alpha_mode), length(rows))
  if (!is.null(sheets)) styles$sheet <- sheets
  keys <- lapply(styles[setdiff(names(styles), "style_id")], function(x) match(x, unique(x)))
  order <- do.call(order, c(keys, list(method = "radix")))
  a <- head(order, -1L); b <- tail(order, -1L)
  changed <- rep(FALSE, length(a))
  for (key in keys) changed <- changed | key[a] != key[b]
  first <- c(TRUE, changed)
  group <- integer(length(rows)); group[order] <- cumsum(first)
  representatives <- sort(order[first], method = "radix")
  lookup <- integer(length(representatives))
  lookup[group[representatives]] <- seq_along(representatives)
  index <- lookup[group]
  styles <- styles[representatives, , drop = FALSE]
  styles$style_id <- seq_len(nrow(styles))
  chosen <- ifelse(kinds == "faces", styles$fill[index], styles$colour[index])
  filled <- kinds == "markers" & styles$shape[index] %in% 21:25 & !is.na(styles$fill[index])
  chosen[filled] <- styles$fill[index[filled]]
  rgba <- colour_channels_3d(chosen, alpha = TRUE)
  opacity <- as.double(styles$alpha[index] * rgba[4L, ] / 255)
  for (name in intersect(c("colour", "fill", "border_colour"), names(styles))) {
    palette <- unique(styles[[name]])
    rgb <- grDevices::col2rgb(palette, alpha = TRUE)
    hex <- ifelse(is.na(palette), NA_character_, grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255))
    styles[[name]] <- hex[match(styles[[name]], palette)]
  }
  styles$alpha <- 1
  styles$shade <- isTRUE(params$shade)
  if (identical(params$render, "splat")) {
    styles$render <- "splat"
    styles$edl <- as.double(params$edl %or3d% 1)
  }
  styles$arrow <- I(rep(list(params$arrow), nrow(styles)))
  styles$arrow_fill <- params$arrow.fill %or3d% styles$colour
  styles$fontsize <- styles$size * text_size_pt_3d(params$size.unit)
  if (isTRUE(params$parse)) styles$label <- I(lapply(as.character(styles$label), function(x) parse(text = x)))
  list(styles = styles, opacity = opacity, index = index)
}

vertex_identity_3d <- function(data, columns) {
  if (!nrow(data)) return(integer())
  values <- data[columns]
  order <- do.call(order, c(values, list(method = "radix")))
  id <- integer(nrow(data));id[order] <- cumsum(!duplicated(values[order, , drop = FALSE]))
  id
}

packet_from_primitives_3d <- function(data, primitives, kinds, params, panel_id, counts) {
  if (!length(primitives)) return(new_scene_packet(panel_id = panel_id))
  space_mapped <- !is.null(params$.space_context)
  if (space_mapped) {
    context <- list(space_context = params$.space_context)
    munched <- params$.space_munch(data, primitives, kinds, counts, context)
    data <- munched$data; primitives <- munched$primitives
    kinds <- munched$kinds; counts <- munched$counts
    params$.munch_start <- munched$start; params$.munch_end <- munched$end
    params$.vertex_key <- NULL
    if(!length(primitives)) return(new_scene_packet(panel_id=panel_id))
  }
  offsets <- cumsum(c(1L, counts[-length(counts)]))
  rows <- primitives[offsets]
  normals <- NULL
  if (all(c("nx", "ny", "nz") %in% names(data))) {
    # Each face takes the mean of its unit vertex normals, renormalised.
    normals <- matrix(NA_real_, length(rows), 3L)
    owner <- rep.int(seq_along(counts), counts)
    corner <- kinds[owner] == "faces"
    if (any(corner)) {
      vertex_normals <- cbind(data$nx, data$ny, data$nz)[primitives[corner], , drop = FALSE]
      vertex_normals <- vertex_normals / sqrt(rowSums(vertex_normals^2))
      face <- unique(owner[corner])
      mean <- rowsum(vertex_normals, owner[corner], reorder = FALSE) / counts[face]
      length <- sqrt(rowSums(mean^2))
      ok <- is.finite(length) & length > 0
      normals[face[ok], ] <- mean[ok, , drop = FALSE] / length[ok]
    }
  }
  sheets <- NULL
  if (identical(params$alpha_mode, "surface")) {
    owner <- rep.int(seq_along(counts), counts)
    corner <- kinds[owner] == "faces"
    sheets <- integer(length(rows))
    if (any(corner)) {
      p <- primitives[corner]
      sheets[unique(owner[corner])] <- face_sheets_3d(data$x[p], data$y[p], data$z[p], match(owner[corner], unique(owner[corner])))
    }
  }
  material <- styles_3d(data, rows, kinds, params, normals, sheets)
  if (!is.null(params$material)) material$styles$material_id <- 1L
  used <- which(tabulate(primitives, nrow(data)) > 0L)
  vertices <- lapply(data[used, c("x", "y", "z"), drop = FALSE], as.double)
  if (!is.null(data$.w)) vertices$w <- as.double(data$.w[used])
  corners <- list(vertex = match(primitives, used),
                  source_row = as.integer(data$.source[primitives]))
  if (identical(params$shading, "smooth")) corners <- c(corners, smooth_corners_3d(data, primitives, kinds, counts))
  if (!is.null(params$.vertex_key)) {
    key <- params$.vertex_key[used]
    representatives <- !duplicated(key)
    vertices <- lapply(data[used[representatives], c("x", "y", "z"), drop = FALSE], as.double)
    if (!is.null(data$.w)) vertices$w <- as.double(data$.w[used[representatives]])
    corners$vertex <- match(params$.vertex_key[primitives], key[representatives])
  }
  tables <- list()
  for (kind in unique(kinds)) {
    i <- which(kinds == kind)
    style <- material$index[i]
    tables[[kind]] <- list(primitive_id = i, layer_id = data$.layer[rows[i]],
      source_row = as.integer(data$.source[rows[i]]), style_id = style, offset = offsets[i], count = counts[i],
      partition = rep.int(if (identical(params$occlusion, "overlay")) 2L else 1L, length(i)), opacity = material$opacity[i])
    if (kind == "polylines") {
      tables[[kind]]$width_mm <- paint_metrics_3d(material$styles)$width_mm[style]
      tables[[kind]]$cap <- rep.int(match(params$lineend %or3d% "butt", c("butt", "round", "square")) - 1L, length(i))
      tables[[kind]]$join <- rep.int(match(params$linejoin %or3d% "round", c("round", "mitre", "bevel")) - 1L, length(i))
      tables[[kind]]$mitre_limit <- rep(params$linemitre %or3d% 10, length(i))
      tables[[kind]]$hidden <- rep.int(match(params$hidden %or3d% "hide", c("hide", "dashed", "show")) - 1L, length(i))
      arrow <- params$arrow
      tables[[kind]]$arrow_start <- rep(!is.null(arrow) && arrow$ends %in% c(1L, 3L), length(i))
      tables[[kind]]$arrow_end <- rep(!is.null(arrow) && arrow$ends %in% c(2L, 3L), length(i))
      if (space_mapped) {
        tables[[kind]]$arrow_start <- tables[[kind]]$arrow_start & params$.munch_start[i]
        tables[[kind]]$arrow_end <- tables[[kind]]$arrow_end & params$.munch_end[i]
      }
    }
    if (kind == "text") {
      for (key in c("hjust", "vjust", "angle")) tables[[kind]][[key]] <- material$styles[[key]][style]
      measured <- unique(style)
      extent <- text_extent_mm_3d(material$styles[measured, , drop = FALSE])
      tables[[kind]]$width_mm <- extent[match(style, measured), 1L]
      tables[[kind]]$height_mm <- extent[match(style, measured), 2L]
    }
  }
  if ("markers" %in% kinds) {
    i <- which(kinds == "markers")
    style <- material$index[i]
    footprints <- marker_footprints_3d(material$styles)
    tables$markers$footprint_id <- footprints$index[style]
    tables$footprints <- footprints$footprints
    tables$footprint_vertices <- footprints$vertices
  }
  packet <- do.call(new_scene_packet, c(list(vertices = vertices, corners = corners), tables,
                             list(styles = material$styles, panel_id = panel_id,
                                  materials = material_table_3d(params$material))))
  if (space_mapped) {
    attr(packet, "space_mapped") <- TRUE
    attr(packet, "model_space") <- TRUE
  }
  packet
}

valid_rows_3d <- function(data, params, columns = c("x", "y", "z")) {
  valid <- Reduce(`&`, lapply(data[columns], is.finite))
  if (any(!valid) && !isTRUE(params$na.rm)) warning("Removed ", sum(!valid), " rows with missing 3D positions.", call. = FALSE)
  valid
}

nondegenerate_triangles_3d <- function(data, faces) {
  a <- as.matrix(data[faces[, 2L], c("x", "y", "z")]) - as.matrix(data[faces[, 1L], c("x", "y", "z")])
  b <- as.matrix(data[faces[, 3L], c("x", "y", "z")]) - as.matrix(data[faces[, 1L], c("x", "y", "z")])
  normal <- a[, c(2L, 3L, 1L), drop = FALSE] * b[, c(3L, 1L, 2L), drop = FALSE] -
    a[, c(3L, 1L, 2L), drop = FALSE] * b[, c(2L, 3L, 1L), drop = FALSE]
  sqrt(rowSums(normal^2)) > 256 * .Machine$double.eps * pmax(rowSums(a^2), rowSums(b^2))
}

unique_edges_3d <- function(edges, vertex, mesh = rep.int(1L, length(vertex))) {
  a <- edges[, 1L]; b <- edges[, 2L]
  edges[!duplicated(data.frame(mesh = mesh[a], a = pmin(vertex[a], vertex[b]),
                              b = pmax(vertex[a], vertex[b]))), , drop = FALSE]
}

packet_from_faces_edges_3d <- function(data, faces, edges, params, panel_params) {
  if (is.matrix(faces)) faces <- list(faces)
  if (isTRUE(params$wireframe)) {
    faces <- list()
    if (all(is.na(data$colour))) data$colour <- "grey30"
  }
  faces <- lapply(faces, function(f) f[!is.na(data$fill[f[, 1L]]), , drop = FALSE])
  edges <- edges[!is.na(data$colour[edges[, 1L]]), , drop = FALSE]
  counts <- unlist(lapply(faces, function(f) rep.int(ncol(f), nrow(f))), use.names = FALSE)
  packet_from_primitives_3d(data, c(unlist(lapply(faces, function(f) as.vector(t(f))), use.names = FALSE), as.vector(t(edges))),
    c(rep("faces", length(counts)), rep("polylines", nrow(edges))), params, panel_params$panel_id,
    c(counts, rep.int(2L, nrow(edges))))
}

emit_points_3d <- function(data, params, panel_params) {
  if (is.character(data$shape)) {
    identity_coord <- ggplot2::ggproto(NULL, ggplot2::Coord, transform = function(data, panel_params) data)
    data$shape <- ggplot2::GeomPoint$draw_panel(data, panel_params, identity_coord)$pch
  }
  rows <- which(valid_rows_3d(data, params))
  packet_from_primitives_3d(data, rows, rep("markers", length(rows)), params, panel_params$panel_id, rep.int(1L, length(rows)))
}

emit_text_3d <- function(data, params, panel_params) {
  rows <- which(valid_rows_3d(data, params) & !is.na(data$label))
  packet_from_primitives_3d(data, rows, rep("text", length(rows)), params, panel_params$panel_id, rep.int(1L, length(rows)))
}

emit_paths_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params)
  fields <- intersect(c("colour", "linewidth", "alpha", "linetype"), names(data))
  rows <- order(match(data$group, unique(data$group)), seq_len(nrow(data)))
  a <- head(rows, -1L); b <- tail(rows, -1L)
  good <- data$group[a] == data$group[b] & valid[a] & valid[b]
  a <- a[good]; b <- b[good]
  same <- rep(TRUE, max(0L, length(a) - 1L))
  for (field in fields) {
    ids <- match(data[[field]], unique(data[[field]]))
    same <- same & ids[head(a, -1L)] == ids[tail(a, -1L)]
  }
  start <- c(TRUE, head(b, -1L) != tail(a, -1L) | !same)[seq_along(a)]
  counts <- diff(c(which(start), length(a) + 1L)) + 1L
  counts <- counts[seq_len(sum(start))]
  positions <- seq_along(a) + cumsum(start)
  primitives <- integer(length(a) + sum(start))
  primitives[positions] <- b
  primitives[positions[start] - 1L] <- a[start]
  packet_from_primitives_3d(data, primitives, rep("polylines", length(counts)), params, panel_params$panel_id, counts)
}

emit_segments_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend"))
  data <- data[valid, , drop = FALSE]
  ends <- data
  ends[c("x", "y", "z")] <- data[c("xend", "yend", "zend")]
  all <- rbind(data, ends)
  n <- nrow(data)
  primitives <- as.vector(rbind(seq_len(n), seq_len(n) + n))
  packet_from_primitives_3d(all, primitives, rep("polylines", n), params, panel_params$panel_id, rep.int(2L, n))
}

merge_scene_packets <- function(packets, panel_id) {
  packets <- unname(packets)
  if (!length(packets)) return(new_scene_packet(panel_id = panel_id))
  lapply(packets, validate_scene_packet)
  schema <- scene_schemas()
  sizes <- vapply(packets, function(p) c(vertex = length(p$vertices$x),
    corner = length(p$corners$vertex), footprint = length(p$footprints$kind),
    footprint_vertex = length(p$footprint_vertices$x_mm), style = nrow(p$styles),
    primitive = sum(vapply(p[c("faces", "polylines", "markers", "text", "quadrics")],
                           function(t) length(t$primitive_id), integer(1)))), numeric(6))
  totals <- rowSums(sizes)
  scene_assert(all(totals <= .Machine$integer.max), "merged packet exceeds integer index capacity")
  shifts <- sizes
  for (key in rownames(sizes)) shifts[key, ] <- c(0, head(cumsum(sizes[key, ]), -1L))
  material_count <- vapply(packets, function(p) nrow(p$materials), integer(1))
  material_shift <- c(0L, head(cumsum(material_count), -1L))
  for (j in seq_along(packets)) {
    packet <- packets[[j]]
    shift <- shifts[, j]
    packet$corners$vertex <- as.integer(packet$corners$vertex + shift[["vertex"]])
    packet$footprints$offset <- as.integer(ifelse(packet$footprints$count == 0L, 1L,
      packet$footprints$offset + shift[["footprint_vertex"]]))
    next_id <- shift[["primitive"]]
    for (kind in c("faces", "polylines", "markers", "text")) {
      tab <- packet[[kind]]
      tab$primitive_id <- as.integer(seq_along(tab$primitive_id) + next_id)
      next_id <- next_id + length(tab$primitive_id)
      tab$offset <- as.integer(tab$offset + shift[["corner"]])
      tab$style_id <- as.integer(tab$style_id + shift[["style"]])
      if (kind == "markers") tab$footprint_id <- as.integer(tab$footprint_id + shift[["footprint"]])
      packet[[kind]] <- tab
    }
    quadric <- packet$quadrics
    quadric$primitive_id <- as.integer(seq_along(quadric$primitive_id) + next_id)
    quadric$style_id <- as.integer(quadric$style_id + shift[["style"]])
    packet$quadrics <- quadric
    packet$styles$style_id <- as.integer(packet$styles$style_id + shift[["style"]])
    if (!is.null(packet$styles$material_id)) packet$styles$material_id <- as.integer(packet$styles$material_id + material_shift[j])
    packets[[j]] <- packet
  }
  # An optional column supplied by some packets is NA (not supplied) in the others.
  optional <- scene_optional_schemas()
  tables <- lapply(names(schema), function(kind) {
    extra <- names(optional[[kind]])
    extra <- extra[extra %in% unlist(lapply(packets, function(p) names(p[[kind]])))]
    columns <- lapply(c(names(schema[[kind]]), extra), function(column)
      unlist(lapply(packets, function(p) p[[kind]][[column]] %or3d%
        rep(NA_real_, length(p[[kind]][[1L]]))), use.names = FALSE))
    names(columns) <- c(names(schema[[kind]]), extra)
    columns
  })
  names(tables) <- names(schema)
  styles <- lapply(packets, `[[`, "styles")
  columns <- unique(unlist(lapply(styles, names), use.names = FALSE))
  styles <- lapply(styles, function(s) {
    for (column in setdiff(columns, names(s))) s[[column]] <- rep(NA, nrow(s))
    s[columns]
  })
  material <- lapply(columns, function(column) {
    values <- lapply(styles, `[[`, column)
    value <- do.call(c, values)
    if (any(vapply(values, inherits, logical(1), "AsIs"))) value <- I(value)
    value
  })
  names(material) <- columns
  material <- structure(material, class = "data.frame", row.names = .set_row_names(as.integer(totals[["style"]])))
  result <- c(list(schema_version = scene_schema_version, panel_id = as.integer(panel_id),
                   style_count = as.integer(totals[["style"]])), tables,
              list(styles = material, materials = merge_materials_3d(packets)))
  class(result) <- "ggplot3d_scene_packet"
  validate_scene_packet(result)
  result
}

merge_materials_3d <- function(packets) {
  tables <- Filter(nrow, lapply(packets, `[[`, "materials"))
  if (!length(tables)) return(data.frame(material_id = integer()))
  out <- do.call(rbind, tables)
  out$material_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

collect_scene_layers <- function(panel, panel_params, theme) {
  result <- list(packets = list(), underlay = list(), overlay = list())
  extra <- frame_of_3d(panel_params)$scene_packets
  if (is.function(extra)) result$packets <- extra(panel_params, theme)
  for (grob in panel) {
    if (is.null(grob) || inherits(grob, "zeroGrob") || inherits(grob, "null")) next
    if (inherits(grob, "scene_layer_grob")) result$packets[[length(result$packets) + 1L]] <- grob$packet
    else if (inherits(grob, "scene_underlay_grob")) result$underlay[[length(result$underlay) + 1L]] <- grob
    else if (inherits(grob, "scene_overlay_grob")) result$overlay[[length(result$overlay) + 1L]] <- grob
    else stop("Unsupported nonempty panel grob: ", class(grob)[1L], ". Register a 3D emitter or explicit decoration adapter.", call. = FALSE)
  }
  result
}

# Marks placed by one coordinate per row accept every adjustment.
mark_positions_3d <- c("nudge", "jitter", "dodge", "stack", "planar")
register_geom_3d(ggplot2::GeomPoint, emit_points_3d, positions = mark_positions_3d, tween = "row")
register_geom_3d(ggplot2::GeomPath, emit_paths_3d, positions = mark_positions_3d, tween = "path")
register_geom_3d(ggplot2::GeomLine, emit_paths_3d, positions = mark_positions_3d, tween = "path")
register_geom_3d(ggplot2::GeomSegment, emit_segments_3d, positions = c("nudge", "jitter", "dodge"), tween = "row")
register_geom_3d(ggplot2::GeomText, emit_text_3d, positions = mark_positions_3d, tween = "row")
# geom_blank() and expand_limits() train scales and draw nothing.
register_geom_3d(ggplot2::GeomBlank, function(data, params, panel_params) {
  new_scene_packet(vertices = list(x = numeric(), y = numeric(), z = numeric()))
}, positions = mark_positions_3d)
