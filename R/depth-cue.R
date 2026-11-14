#' Planar cast shadows
#'
#' Casts the shadows of a panel's plot layers onto panes of the panel box,
#' for the `shadow` argument of [coord_3d()]. Every filled face, stroke and
#' point is projected along the light direction onto the plane of each chosen
#' pane (Blinn 1988) and cut to the pane. The shadows of one layer are painted
#' as one translucent film, the union of their pieces filled once with the
#' nonzero winding rule, so overlapping pieces do not darken twice. The film
#' lies in the pane's plane and is drawn over the pane fill and under its grid
#' lines.
#'
#' Strokes cast ribbons of their drawn width and points cast discs of their
#' drawn radius, both measured where the caster lies. Text casts no shadow,
#' and neither do ray-traced layers. A pane receives shadows only when the
#' light reaches it from inside the box, and only while it faces away from
#' the camera, as panes drawn behind the data do. The shadows are vector
#' geometry in every backend: the raster and hybrid backends draw them with
#' the panel furniture.
#' @param on Panes that receive shadows: `"floor"`, `"back"`, or pane names
#'   `"xmin"` to `"zmax"`.
#' @param alpha Opacity of the shadow film, greater than 0 and at most 1.
#' @param colour Shadow colour.
#' @param light A [light_3d()] configuration whose direction casts the
#'   shadows. `NULL` uses the light of [coord_3d()], or the default
#'   [light_3d()] when that is `NULL` too.
#' @references Blinn, J. (1988). Me and my (fake) shadow. IEEE Computer
#'   Graphics and Applications 8(1), 82-86.
#' @return A shadow configuration for [coord_3d()].
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 25), y = seq(-1, 1, length.out = 25))
#' d$z <- exp(-2 * (d$x^2 + d$y^2))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "steelblue", colour = NA) +
#'   coord_3d(shadow = shadow_3d(light = light_3d(c(1, 1, 3), space = "data")),
#'     zlim = c(-1, 1))
#' @export
shadow_3d <- function(on = "floor", alpha = 0.3, colour = "black", light = NULL) {
  panes <- c("back", "floor", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
  if (!is.character(on) || !length(on) || anyNA(on) || !all(on %in% panes))
    stop("on must name panes: ", paste(panes, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha > 1)
    stop("alpha must be a number greater than 0 and at most 1.", call. = FALSE)
  check_colour_3d(colour, "colour")
  if (!is.null(light) && !(is.list(light) && all(c("direction", "space") %in% names(light))))
    stop("light must be NULL or a light_3d() configuration.", call. = FALSE)
  structure(list(on = unique(on), alpha = alpha, colour = colour, light = light), class = "ggplot3d_shadow")
}

#' @export
print.ggplot3d_shadow <- function(x, ...) {
  cat("<shadow_3d> on ", paste(x$on, collapse = ", "), ", ", x$colour, " at alpha ", format(x$alpha), "\n", sep = "")
  invisible(x)
}

#' Depth cues: fog, line width and eye-dome lighting
#'
#' Configures the `depth_cue` argument of [coord_3d()]. Fog moves the colours
#' of the plot layers toward a fog colour with their depth from the camera,
#' in linear light, so far data recede. The same weight can thin strokes with
#' depth, as `scale_depth` does in ggcube. Eye-dome lighting (Boucheny 2009)
#' darkens rasterised pixels that lie behind their neighbours, which outlines
#' silhouettes and depth steps from the depth buffer alone.
#'
#' The fog weight of a primitive is
#' \deqn{w = \min(1, \max(0, (s - near) / (far - near))),}
#' where \eqn{s} is the camera depth of the primitive's centre as a fraction
#' of the depth extent of the panel box: 0 at the box corner nearest the
#' camera, 1 at the farthest. Fill, stroke, text and label border colours move
#' toward the fog colour by `amount * w`, and with `linewidth` a stroke's width
#' is multiplied by `1 - linewidth * w`. Panel furniture keeps its colours.
#' The vector, raster and hybrid backends evaluate the weight once per
#' primitive, so tessellated surfaces fade smoothly; [view_3d()] evaluates the
#' same formula per pixel.
#'
#' Eye-dome lighting acts on the raster backend, the rasterised layers of the
#' hybrid backend, and [view_3d()]. A pixel darkens by
#' `exp(-40 * edl * r)`, where `r` averages over its eight neighbours how far
#' each lies in front of it in log2 of the depth normalised to the camera's
#' near and far planes, as for `geom_point_3d(render = "splat")`.
#' @param fog Fog colour. `NULL` uses the fill of the theme element
#'   `panel.background` (white when it is blank); `NA` leaves colours
#'   unchanged.
#' @param near,far Box depth fractions at which the fog weight starts to grow
#'   from 0 and reaches 1; `near < far`.
#' @param amount Share of the fog colour at full weight, from 0 to 1.
#' @param linewidth `FALSE`, `TRUE` (0.5), or a number from 0 to 1: the share
#'   by which strokes thin at full weight.
#' @param edl `FALSE`, `TRUE` (1), or a nonnegative eye-dome lighting
#'   strength.
#' @references Boucheny, C. (2009). Visualisation scientifique de grands
#'   volumes de donnees: pour une approche perceptive. PhD thesis, Universite
#'   Joseph Fourier, Grenoble.
#'
#'   Kitware. Eye-Dome Lighting: a non-photorealistic shading technique.
#'   <https://www.kitware.com/eye-dome-lighting-a-non-photorealistic-shading-technique/>
#' @return A depth cue configuration for [coord_3d()].
#' @examples
#' d <- expand.grid(x = 1:6, y = 1:6, z = 1:6)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 2)
#' p + coord_3d(depth_cue = depth_cue_3d())
#' p + coord_3d(depth_cue = depth_cue_3d(fog = "grey90", amount = 0.8, linewidth = TRUE))
#' v <- expand.grid(x = seq(-1, 1, length.out = 30), y = seq(-1, 1, length.out = 30))
#' v$z <- sin(3 * v$x) * cos(3 * v$y)
#' ggplot2::ggplot(v, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey80", colour = NA, shade = FALSE) +
#'   coord_3d(backend = backend_3d("raster", dpi = 72), light = NULL,
#'     depth_cue = depth_cue_3d(fog = NA, edl = 2))
#' @export
depth_cue_3d <- function(fog = NULL, near = 0, far = 1, amount = 0.6, linewidth = FALSE, edl = FALSE) {
  if (!is.null(fog) && !(length(fog) == 1L && is.na(fog))) check_colour_3d(fog, "fog")
  if (!is.numeric(near) || !is.numeric(far) || length(near) != 1L || length(far) != 1L ||
      !is.finite(near) || !is.finite(far) || near >= far)
    stop("near and far must be finite numbers with near < far.", call. = FALSE)
  if (!is.numeric(amount) || length(amount) != 1L || !is.finite(amount) || amount < 0 || amount > 1)
    stop("amount must be a number from 0 to 1.", call. = FALSE)
  linewidth <- flag_strength_3d(linewidth, 0.5, "linewidth", 1)
  edl <- flag_strength_3d(edl, 1, "edl", Inf)
  structure(list(fog = if (is.null(fog)) NULL else if (is.na(fog)) NA_character_ else fog,
    near = as.double(near), far = as.double(far), amount = as.double(amount),
    linewidth = linewidth, edl = edl), class = "ggplot3d_depth_cue")
}

#' @export
print.ggplot3d_depth_cue <- function(x, ...) {
  fog <- if (is.null(x$fog)) "panel background" else if (is.na(x$fog)) "none" else x$fog
  cat("<depth_cue_3d> fog ", fog, " (amount ", format(x$amount), ", near ", format(x$near), ", far ", format(x$far),
      "), linewidth ", format(x$linewidth), ", edl ", format(x$edl), "\n", sep = "")
  invisible(x)
}

check_colour_3d <- function(colour, name) {
  valid <- length(colour) == 1L && !is.na(colour) &&
    !inherits(tryCatch(grDevices::col2rgb(colour), error = function(e) e), "error")
  if (!valid) stop(name, " must be one colour.", call. = FALSE)
  invisible(colour)
}

# A logical switch or a strength: FALSE is 0, TRUE is `on`, numbers lie in
# [0, upper].
flag_strength_3d <- function(value, on, name, upper) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(if (value) on else 0)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0 || value > upper)
    stop(name, " must be TRUE, FALSE or a number from 0 to ", upper, ".", call. = FALSE)
  as.double(value)
}

# Fog weight at box depth fraction s.
depth_cue_weight_3d <- function(s, cue) pmin(1, pmax(0, (s - cue$near) / (cue$far - cue$near)))

# The fog colour of a depth cue in a theme, NA for none.
depth_cue_fog_3d <- function(cue, theme) {
  if (!is.null(cue$fog)) return(cue$fog)
  background <- element_3d("panel.background", theme)
  fill <- if (visible_element_3d(background)) background$fill %or3d% NA else NA
  if (is.na(fill)) "white" else fill
}

# Colours moved toward `fog` by `mix` (one weight per colour) in linear
# light, with their alpha kept.
fog_colours_3d <- function(colour, mix, fog) {
  missing <- is.na(colour)
  if (all(missing)) return(colour)
  rgba <- colour_channels_3d(ifelse(missing, "black", colour), alpha = TRUE) / 255
  target <- srgb_decode_3d(grDevices::col2rgb(fog)[, 1L] / 255)
  linear <- srgb_decode_3d(rgba[1:3, , drop = FALSE]) * rep(1 - mix, each = 3L) + target * rep(mix, each = 3L)
  rgb <- matrix(pmin(1, pmax(0, srgb_encode_3d(linear))), nrow = 3L)
  out <- grDevices::rgb(rgb[1L, ], rgb[2L, ], rgb[3L, ], rgba[4L, ])
  out[missing] <- NA_character_
  out
}

# Camera-depth fraction of packet points within the depth extent of the
# panel box: 0 at the nearest box point, 1 at the farthest.
box_depth_fraction_3d <- function(camera, viewport, frame) {
  basis <- frame_basis_3d(camera, viewport)
  box <- frame_eye_coordinates_3d(frame$bounds(), camera, basis)[, 3L]
  low <- min(box); span <- max(box) - low
  function(xyz) (frame_eye_coordinates_3d(xyz, camera, basis)[, 3L] - low) / span
}

# Centre of every primitive, one matrix per kind (faces, polylines, markers,
# text, quadrics), in packet coordinates after the homogeneous matrix `pre`:
# the mean of its homogeneous corners (the placed local centre of a
# quadric), NA where that mean lies at or beyond infinity.
primitive_centres_3d <- function(packet, pre = diag(4)) {
  positions <- scene_vertex_matrix_3d(packet$vertices)
  identity <- identical(as.double(pre), as.double(diag(4)))
  if (!identity) positions <- positions %*% t(matrix(as.double(pre), 4L, 4L))
  lapply(primitive_kinds_3d, function(kind) {
    tab <- packet[[kind]]
    n <- length(tab$primitive_id)
    if (!n) return(matrix(numeric(), 0L, 3L))
    if (kind == "quadrics") {
      axial <- ifelse(tab$kind == 1L, 0, 0.5)
      mean <- cbind(tab$transform_13 + axial * tab$transform_9, tab$transform_14 + axial * tab$transform_10,
        tab$transform_15 + axial * tab$transform_11, 1)
      if (!identity) mean <- mean %*% t(matrix(as.double(pre), 4L, 4L))
    } else {
      group <- rep.int(seq_len(n), tab$count)
      corner <- tab$offset[group] + sequence(tab$count) - 1L
      mean <- rowsum(positions[packet$corners$vertex[corner], , drop = FALSE], group, reorder = TRUE)
    }
    centre <- mean[, 1:3, drop = FALSE] / mean[, 4L]
    centre[!(mean[, 4L] > 0), ] <- NA_real_
    centre
  })
}

# Style rows of every primitive, kinds in table order.
primitive_styles_3d <- function(packet) {
  rows <- unlist(lapply(packet[primitive_kinds_3d], function(tab)
    match(tab$style_id, packet$styles$style_id)), use.names = FALSE)
  packet$styles[rows, , drop = FALSE]
}

# The packet with one new style row per primitive, from `styles` (as
# returned by primitive_styles_3d()); `sizes` counts the primitives of each
# kind.
restyle_primitives_3d <- function(packet, styles, sizes) {
  styles$style_id <- nrow(packet$styles) + seq_len(nrow(styles))
  packet$styles <- rbind(packet$styles, styles)
  packet$style_count <- as.integer(nrow(packet$styles))
  index <- 0L
  kinds <- primitive_kinds_3d
  for (i in seq_along(kinds)) if (sizes[i]) {
    packet[[kinds[i]]]$style_id <- styles$style_id[index + seq_len(sizes[i])]
    index <- index + sizes[i]
  }
  packet
}

# Solid polylines split into one primitive per segment, sharing corners, so a
# depth cue varies along a path. Arrow heads stay at the ends of the path.
# Opaque pieces take round caps, which cover the joints between pieces.
split_polylines_3d <- function(packet) {
  tab <- packet$polylines
  n <- length(tab$offset)
  if (!n) return(packet)
  styles <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
  solid <- as.character(style_value_3d(styles, "linetype", 1)) %in% c("solid", "1") & tab$count > 2L
  if (!any(solid)) return(packet)
  pieces <- ifelse(solid, tab$count - 1L, 1L)
  row <- rep.int(seq_len(n), pieces)
  step <- sequence(pieces) - 1L
  split <- lapply(tab, `[`, row)
  split$offset <- as.integer(tab$offset[row] + step)
  split$count <- as.integer(ifelse(solid[row], 2L, tab$count[row]))
  split$arrow_start <- tab$arrow_start[row] & step == 0L
  split$arrow_end <- tab$arrow_end[row] & step == pieces[row] - 1L
  opaque <- solid & tab$opacity >= 1 & colour_channels_3d(ifelse(is.na(styles$colour), "black", styles$colour), alpha = TRUE)[4L, ] == 255
  split$cap[opaque[row]] <- 1L
  packet$polylines <- split
  renumber_primitives_3d(packet)
}

# Primitives whose centre lies at infinity take the full weight.
depth_cue_packet_3d <- function(packet, cue, fog, depth_of, pre = diag(4)) {
  packet <- split_polylines_3d(packet)
  centres <- primitive_centres_3d(packet, pre)
  sizes <- vapply(centres, nrow, integer(1))
  if (!sum(sizes)) return(packet)
  weight <- depth_cue_weight_3d(depth_of(do.call(rbind, centres)), cue)
  weight[is.na(weight)] <- 1
  styles <- primitive_styles_3d(packet)
  if (!is.na(fog) && cue$amount > 0) {
    for (name in intersect(c("colour", "fill", "border_colour", "arrow_fill"), names(styles)))
      if (is.character(styles[[name]])) styles[[name]] <- fog_colours_3d(styles[[name]], cue$amount * weight, fog)
  }
  if (cue$linewidth > 0 && sizes[2L]) {
    rows <- sizes[1L] + seq_len(sizes[2L])
    factor <- 1 - cue$linewidth * weight[rows]
    packet$polylines$width_mm <- packet$polylines$width_mm * factor
    if (is.numeric(styles$linewidth)) styles$linewidth[rows] <- style_value_3d(styles[rows, , drop = FALSE], "linewidth", 0.5) * factor
  }
  restyle_primitives_3d(packet, styles, sizes)
}

# Plot layer packets with the panel's depth cue applied to their styles.
depth_cue_packets_3d <- function(packets, panel_params, camera, viewport, theme) {
  cue <- panel_params$coord_params$depth_cue
  if (is.null(cue)) return(packets)
  fog <- depth_cue_fog_3d(cue, theme)
  if ((is.na(fog) || cue$amount == 0) && cue$linewidth == 0) return(packets)
  depth_of <- box_depth_fraction_3d(camera, viewport, frame_of_3d(panel_params))
  lapply(packets, depth_cue_packet_3d, cue = cue, fog = fog, depth_of = depth_of, pre = camera$pre %or3d% diag(4))
}

# Eye-dome lighting strength of a panel.
depth_cue_edl_3d <- function(panel_params) panel_params$coord_params$depth_cue$edl %or3d% 0

# Settings the viewer needs to evaluate the depth cue itself.
view_depth_cue_3d <- function(cue, theme) {
  if (is.null(cue)) return(NULL)
  fog <- depth_cue_fog_3d(cue, theme)
  list(fog = if (is.na(fog)) c(0, 0, 0, 0) else view_rgba_3d(fog), amount = if (is.na(fog)) 0 else cue$amount,
    near = cue$near, far = cue$far, linewidth = cue$linewidth, edl = cue$edl)
}

# Planar shadows ----------------------------------------------------------------

# Furniture with the panel's shadows appended to its packets.
shadow_furniture_3d <- function(furniture, packets, panel_params) {
  spec <- panel_params$coord_params$shadow
  if (is.null(spec)) return(furniture)
  shadow <- shadow_packet_3d(packets, spec, panel_params, furniture$camera, furniture$viewport)
  if (is.null(shadow)) return(furniture)
  furniture$packet <- append_scene_packet_3d(furniture$packet, shadow)
  if (!is.null(furniture$split)) furniture$split$fixed <- append_scene_packet_3d(furniture$split$fixed, shadow)
  furniture
}

# `base` followed by `extra`, keeping the primitive ids of `base` (furniture
# queries refer to them) and numbering the extra primitives after them.
append_scene_packet_3d <- function(base, extra) {
  kinds <- primitive_kinds_3d
  ids <- lapply(kinds, function(kind) base[[kind]]$primitive_id)
  merged <- merge_scene_packets(list(base, extra), base$panel_id)
  top <- max(c(0L, unlist(ids)))
  for (i in seq_along(kinds)) {
    n <- length(ids[[i]]); m <- length(merged[[kinds[i]]]$primitive_id)
    added <- m - n
    merged[[kinds[i]]]$primitive_id <- as.integer(c(ids[[i]], top + seq_len(added)))
    top <- top + added
  }
  merged
}

# Millimetres on the page per model unit at packet points, along the image
# plane.
page_scale_3d <- function(xyz, camera, basis, viewport) {
  depth <- if (basis$perspective) frame_eye_coordinates_3d(xyz, camera, basis)[, 3L] else 1
  basis$base * camera$zoom * viewport$height_mm / 2 / depth
}

# The shadow film of every plot layer on every lit pane, or NULL.
shadow_packet_3d <- function(packets, spec, panel_params, camera, viewport) {
  frame <- frame_of_3d(panel_params)
  if (!isTRUE(frame$identity) || !is.null(panel_params$space_context))
    stop("shadow_3d() needs the box frame of a Euclidean coord_3d() panel.", call. = FALSE)
  basis <- frame_basis_3d(camera, viewport)
  light <- spec$light %or3d% panel_params$coord_params$light %or3d% light_3d()
  towards <- light_vector_3d(list(right = basis$right, up = basis$up, forward = basis$forward,
    model_scale = basis$scale), light) / basis$scale
  partitions <- furniture_partitions_3d(camera, viewport, frame)
  casters <- Filter(function(p) any(unlist(lapply(p[c("faces", "polylines", "markers")], `[[`, "layer_id")) > 0L), packets)
  parts <- list()
  for (plane in resolve_panes_3d(spec$on, panel_params)) {
    along <- towards[plane$axis]
    if (if (plane$value == 0) along <= 1e-9 else along >= -1e-9) next
    partition <- if (isTRUE(camera$clip_data)) unname(partitions[plane$pane$name]) else 1L
    if (identical(partition, 2L)) next
    for (packet in casters) {
      part <- shadow_part_3d(packet, plane, towards, partition, spec, camera, basis, viewport)
      if (!is.null(part)) parts[[length(parts) + 1L]] <- part
    }
  }
  if (!length(parts)) return(NULL)
  merge_scene_packets(parts, panel_params$panel_id)
}

# Shadow of one layer packet on one pane plane: projected filled faces,
# stroke ribbons and point discs as one sheet of convex faces cut to the pane.
shadow_part_3d <- function(packet, plane, towards, partition, spec, camera, basis, viewport) {
  k <- plane$axis; scale <- basis$scale
  packet <- scene_finite_packet_3d(packet)
  xyz <- scene_vertices_xyz_3d(packet$vertices)
  onto <- function(p) {
    p <- p - outer((p[, k] - plane$value) / towards[k], towards)
    p[, k] <- plane$value
    p
  }
  corners_of <- function(tab, rows) {
    group <- rep.int(seq_along(rows), tab$count[rows])
    list(group = group, vertex = packet$corners$vertex[tab$offset[rows][group] + sequence(tab$count[rows]) - 1L])
  }
  styles_of <- function(tab) packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
  points <- list(); counts <- integer()
  add <- function(p, count) { points[[length(points) + 1L]] <<- p; counts <<- c(counts, count) }
  faces <- packet$faces
  filled <- which(!is.na(style_value_3d(styles_of(faces), "fill", NA_character_)) & faces$opacity > 0)
  if (length(filled)) {
    ring <- corners_of(faces, filled)
    add(onto(xyz[ring$vertex, , drop = FALSE]), faces$count[filled])
  }
  normal <- numeric(3); normal[k] <- 1
  lines <- packet$polylines
  drawn <- which(!is.na(style_value_3d(styles_of(lines), "colour", NA_character_)) & lines$opacity > 0 & lines$width_mm > 0)
  if (length(drawn)) {
    path <- corners_of(lines, drawn)
    caster <- xyz[path$vertex, , drop = FALSE]
    shadow <- onto(caster) * rep(scale, each = nrow(caster))
    n <- nrow(shadow)
    segment <- which(path$group[-1L] == path$group[-n])
    a <- shadow[segment, , drop = FALSE]; b <- shadow[segment + 1L, , drop = FALSE]
    across <- cross_rows_3d(matrix(normal, nrow(a), 3L, byrow = TRUE), b - a)
    length <- sqrt(rowSums(across^2))
    keep <- length > 0
    if (any(keep)) {
      segment <- segment[keep]; a <- a[keep, , drop = FALSE]; b <- b[keep, , drop = FALSE]
      mid <- (caster[segment, , drop = FALSE] + caster[segment + 1L, , drop = FALSE]) / 2
      half <- lines$width_mm[drawn][path$group[segment]] / 2 / page_scale_3d(mid, camera, basis, viewport)
      offset <- across[keep, , drop = FALSE] / length[keep] * half
      quad <- rbind(a - offset, b - offset, b + offset, a + offset)
      quad <- quad[as.vector(t(matrix(seq_len(nrow(quad)), ncol = 4L))), , drop = FALSE]
      add(quad / rep(scale, each = nrow(quad)), rep.int(4L, length(segment)))
    }
  }
  markers <- packet$markers
  if (length(markers$offset)) {
    s <- styles_of(markers)
    visible <- which((!is.na(style_value_3d(s, "colour", NA_character_)) | !is.na(style_value_3d(s, "fill", NA_character_))) &
      markers$opacity > 0)
    if (length(visible)) {
      centre <- xyz[packet$corners$vertex[markers$offset[visible]], , drop = FALSE]
      footprint <- markers$footprint_id[visible]
      radius_mm <- pmax(packet$footprints$radius_x_mm[footprint], packet$footprints$radius_y_mm[footprint])
      fallback <- paint_metrics_3d(s[visible, , drop = FALSE])$radius_mm
      radius_mm[!(radius_mm > 0)] <- fallback[!(radius_mm > 0)]
      radius <- radius_mm / page_scale_3d(centre, camera, basis, viewport)
      axes <- setdiff(1:3, k)
      angle <- seq(0, 2 * pi, length.out = 17L)[-17L]
      disc <- onto(centre)[rep(seq_along(visible), each = 16L), , drop = FALSE]
      r <- rep(radius, each = 16L)
      disc[, axes[1L]] <- disc[, axes[1L]] + r * rep(cos(angle), length(visible)) / scale[axes[1L]]
      disc[, axes[2L]] <- disc[, axes[2L]] + r * rep(sin(angle), length(visible)) / scale[axes[2L]]
      add(disc, rep.int(16L, length(visible)))
    }
  }
  if (!length(counts)) return(NULL)
  geometry <- do.call(rbind, points)
  layer <- min(unlist(lapply(packet[c("faces", "polylines", "markers")], `[[`, "layer_id")))
  shadow <- new_scene_packet(vertices = list(x = geometry[, 1L], y = geometry[, 2L], z = geometry[, 3L]),
    corners = list(vertex = seq_len(nrow(geometry)), source_row = rep.int(0L, nrow(geometry))),
    faces = list(offset = as.integer(cumsum(c(1L, utils::head(counts, -1L)))), count = as.integer(counts),
      layer_id = rep.int(0L, length(counts)), partition = rep.int(as.integer(partition), length(counts)),
      opacity = rep(spec$alpha, length(counts))),
    styles = data.frame(style_id = 1L, fill = spec$colour, colour = NA_character_, alpha_mode = "surface",
      sheet = as.integer(layer), stringsAsFactors = FALSE),
    panel_id = packet$panel_id)
  shadow <- clean_flat_faces_3d(shadow)
  box <- unit_box_planes_3d()[c(2L * setdiff(1:3, k) - 1L, 2L * setdiff(1:3, k))]
  shadow <- clip_packet_planes_3d(shadow, box)
  attr(shadow, "parent") <- NULL
  if (!length(shadow$faces$offset)) NULL else shadow
}
