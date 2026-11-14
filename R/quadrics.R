utils::globalVariables(c("quadric_outline_cpp", "quadric_cast_cpp", "quadric_fragment_parts_cpp", "quadric_mesh_cpp"))

# Analytic quadrics: unit spheres, cylinders and cones placed by affine maps
# into the panel box (Sigg, Weyrich, Botsch and Gross 2006). The engine orders
# them through a refined polyhedral stand-in and returns its visible pieces as
# fragments of kind 5; the vector backend draws the exact outline inside
# them, the raster backend and the viewer cast each sample's ray against the
# quadric, and exporters and shadows take an inscribed mesh.

primitive_kinds_3d <- c("faces", "polylines", "markers", "text", "quadrics")

# Consecutive primitive ids across every primitive table, in table order.
renumber_primitives_3d <- function(packet) {
  next_id <- 0L
  for (kind in primitive_kinds_3d) {
    n <- length(packet[[kind]]$primitive_id)
    packet[[kind]]$primitive_id <- as.integer(next_id + seq_len(n))
    next_id <- next_id + n
  }
  packet
}

# Points around a quadric's circles in the inscribed meshes of exporters,
# shadows, clipped quadrics and viewer quadrics whose image is unbounded.
quadric_segments_3d <- function() {
  n <- getOption("ggplot3d.quadric_segments", 32L)
  scene_assert(is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 3 && n == trunc(n),
               "option ggplot3d.quadric_segments must be a whole number of at least 3")
  as.integer(n)
}

# Transform columns of quadric rows from the 3 x 3 linear parts (box units per
# local unit, one per row) and the box positions of the local origins.
quadric_transforms_3d <- function(linear, origin) {
  n <- dim(linear)[3L]
  m <- matrix(0, n, 16L)
  for (j in 1:3) for (i in 1:3) m[, (j - 1L) * 4L + i] <- linear[i, j, ]
  m[, 13:15] <- origin
  m[, 16L] <- 1
  stats::setNames(lapply(seq_len(16L), function(k) as.double(m[, k])), quadric_transform_columns)
}

# 3 x 3 x n linear parts of quadric rows.
quadric_linear_3d <- function(q, rows = seq_along(q$kind)) {
  out <- array(0, c(3L, 3L, length(rows)))
  for (j in 1:3) for (i in 1:3) out[i, j, ] <- q[[quadric_transform_columns[(j - 1L) * 4L + i]]][rows]
  out
}

# A packet of quadric rows for `data[rows, ]`: `kind`, transform columns,
# radius_ratio and caps, filled with each row's fill.
packet_from_quadrics_3d <- function(data, rows, kind, transform, params, panel_params,
                                    radius_ratio = 1, cap_0 = 1L, cap_1 = 1L) {
  if (!length(rows)) return(new_scene_packet(panel_id = panel_params$panel_id))
  material <- styles_3d(data, rows, rep("faces", length(rows)), params)
  quadrics <- c(list(layer_id = data$.layer[rows], source_row = as.integer(data$.source[rows]),
    style_id = material$index, partition = rep.int(if (identical(params$occlusion, "overlay")) 2L else 1L, length(rows)),
    opacity = material$opacity, kind = rep_len(as.integer(kind), length(rows))), transform,
    list(radius_ratio = rep_len(as.double(radius_ratio), length(rows)),
      cap_0 = rep_len(as.integer(cap_0), length(rows)), cap_1 = rep_len(as.integer(cap_1), length(rows))))
  new_scene_packet(quadrics = quadrics, styles = material$styles, panel_id = panel_params$panel_id)
}

# Places quadrics and returns a packet in model space, as
# emit_glyph_instances_3d() places glyphs: `maps` (3 x 3 x n) take the local
# primitive to offsets about `origin` (normalised positions, n x 3) in data
# units, model units or millimetres by `space`. `fixed` (3 x 3 x n, box units)
# adds columns that keep their length in every space, such as the axis of a
# cylinder between two data points. `centre` gives the origins already mapped
# by the frame where the caller has them; `shift` (n x 3, in the units of
# `space`) moves each origin. Millimetre rows are resolved at draw time.
emit_quadric_instances_3d <- function(data, origin, maps, space, kind, params, panel_params,
                                      fixed = NULL, radius_ratio = 1, cap_0 = 1L, cap_1 = 1L, centre = NULL,
                                      shift = NULL) {
  n <- nrow(data)
  if (is.null(fixed)) fixed <- array(0, c(3L, 3L, n))
  if (is.null(shift)) shift <- matrix(0, n, 3L)
  frame <- frame_of_3d(panel_params)
  if (is.null(centre)) centre <- frame$map(origin)
  scale <- model_scale_3d(panel_params)
  linear <- fixed
  translation <- centre
  data_rows <- which(space == "data")
  if (length(data_rows)) {
    jacobian <- if (isTRUE(frame$identity)) NULL else frame_jacobian_3d(frame, origin[data_rows, , drop = FALSE])
    span <- vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
    for (i in seq_along(data_rows)) {
      j <- data_rows[i]
      m <- cbind(maps[, , j], shift[j, ]) / span
      if (!is.null(jacobian)) m <- jacobian[, , i] %*% m
      linear[, , j] <- linear[, , j] + m[, 1:3]
      translation[j, ] <- translation[j, ] + m[, 4L]
    }
  }
  model_rows <- which(space == "model")
  for (j in model_rows) {
    linear[, , j] <- linear[, , j] + maps[, , j] / scale
    translation[j, ] <- translation[j, ] + shift[j, ] / scale
  }
  mm_rows <- which(space == "mm")
  for (j in mm_rows) {
    linear[, , j] <- linear[, , j] + maps[, , j] * glyph_placeholder_mm_3d / scale
    translation[j, ] <- translation[j, ] + shift[j, ] * glyph_placeholder_mm_3d / scale
  }
  packet <- packet_from_quadrics_3d(data, seq_len(n), kind, quadric_transforms_3d(linear, translation), params,
    panel_params, radius_ratio, cap_0, cap_1)
  attr(packet, "model_space") <- TRUE
  if (length(mm_rows)) attr(packet, "quadric_mm") <- list(rows = mm_rows,
    unit = maps[, , mm_rows, drop = FALSE], fixed = fixed[, , mm_rows, drop = FALSE],
    centre = centre[mm_rows, , drop = FALSE], shift = shift[mm_rows, , drop = FALSE])
  packet
}

# Millimetre quadrics at `k` model units per millimetre in a box of model
# scale `scale`.
resolve_quadric_mm_3d <- function(packet, k, scale) {
  deferred <- attr(packet, "quadric_mm")
  q <- packet$quadrics
  linear <- quadric_linear_3d(q, deferred$rows)
  for (i in seq_along(deferred$rows)) linear[, , i] <- deferred$fixed[, , i] + deferred$unit[, , i] * k / scale
  translation <- deferred$centre + sweep(deferred$shift * k, 2L, scale, "/")
  for (j in 1:3) {
    for (i in 1:3) {
      column <- quadric_transform_columns[(j - 1L) * 4L + i]
      packet$quadrics[[column]][deferred$rows] <- as.double(linear[i, j, ])
    }
    packet$quadrics[[quadric_transform_columns[12L + j]]][deferred$rows] <- as.double(translation[, j])
  }
  attr(packet, "quadric_mm") <- NULL
  packet
}

# The packet with quadric `rows` replaced by inscribed meshes: faces carrying
# their quadric's metadata and per-corner normals.
quadric_faces_3d <- function(packet, rows = seq_along(packet$quadrics$kind), segments = quadric_segments_3d()) {
  if (!length(rows)) return(packet)
  q <- packet$quadrics
  part <- packet
  part$quadrics <- lapply(q, `[`, rows)
  mesh <- quadric_mesh_cpp(part[names(part) != "styles"], segments)
  source <- rows[mesh$quadric]
  nv <- length(packet$vertices$x); nc <- length(packet$corners$vertex)
  packet$vertices <- lapply(c(x = "x", y = "y", z = "z", w = "w"), function(axis)
    c(packet$vertices[[axis]], if (axis == "w") rep(1, length(mesh$x)) else mesh[[axis]]))
  corner_source <- rep.int(q$source_row[source], mesh$count)
  added <- list(vertex = nv + mesh$vertex, source_row = corner_source,
    normal_x = mesh$normal_x, normal_y = mesh$normal_y, normal_z = mesh$normal_z)
  columns <- union(names(packet$corners), names(added))
  corners <- lapply(stats::setNames(columns, columns), function(column)
    c(packet$corners[[column]] %or3d% rep(NA_real_, nc), added[[column]] %or3d% rep(NA_real_, length(mesh$vertex))))
  packet$corners <- scene_table(corners, scene_schemas()$corners, list(), "corners", scene_optional_schemas()$corners)
  faces <- packet$faces
  added <- list(primitive_id = rep(0L, length(source)), layer_id = q$layer_id[source], source_row = q$source_row[source],
    style_id = q$style_id[source], offset = as.integer(nc + cumsum(c(1L, utils::head(mesh$count, -1L)))),
    count = as.integer(mesh$count), role = rep(0L, length(source)), partition = q$partition[source],
    opacity = q$opacity[source])
  packet$faces <- lapply(stats::setNames(names(faces), names(faces)), function(column) c(faces[[column]], added[[column]]))
  subset_quadrics_3d(packet, setdiff(seq_along(q$kind), rows))
}

# The packet with only quadric rows `keep`, millimetre rows remapped.
subset_quadrics_3d <- function(packet, keep) {
  packet$quadrics <- lapply(packet$quadrics, `[`, keep)
  deferred <- attr(packet, "quadric_mm")
  if (!is.null(deferred)) {
    kept <- deferred$rows %in% keep
    deferred <- list(rows = match(deferred$rows[kept], keep), unit = deferred$unit[, , kept, drop = FALSE],
      fixed = deferred$fixed[, , kept, drop = FALSE], centre = deferred$centre[kept, , drop = FALSE],
      shift = deferred$shift[kept, , drop = FALSE])
    attr(packet, "quadric_mm") <- if (length(deferred$rows)) deferred else NULL
  }
  packet <- renumber_primitives_3d(packet)
  validate_scene_packet(packet)
  packet
}

# Quadrics against clip planes (normal . x + offset >= 0 keeps): kept whole
# when the solid lies inside every plane, dropped when outside one, and
# otherwise replaced by an inscribed mesh for the face clipper. Millimetre
# quadrics, sized only at draw time, are kept or dropped by their centre.
quadric_clip_planes_3d <- function(packet, planes) {
  q <- packet$quadrics
  n <- length(q$kind)
  if (!n || !length(planes)) return(packet)
  linear <- quadric_linear_3d(q)
  origin <- cbind(q$transform_13, q$transform_14, q$transform_15)
  deferred <- seq_len(n) %in% attr(packet, "quadric_mm")$rows
  inside <- rep(TRUE, n); outside <- rep(FALSE, n)
  for (plane in planes) {
    for (i in seq_len(n)) {
      at <- sum(plane$normal * origin[i, ]) + plane$offset
      if (deferred[i]) { lo <- hi <- at } else {
        g <- drop(crossprod(linear[, , i], plane$normal))
        if (q$kind[i] == 1L) {
          reach <- sqrt(sum(g^2)); lo <- at - reach; hi <- at + reach
        } else {
          radial <- sqrt(sum(g[1:2]^2))
          ends <- at + g[3L] * c(0, 1)
          radius <- c(1, if (q$kind[i] == 3L) q$radius_ratio[i] else 1)
          lo <- min(ends - radius * radial); hi <- max(ends + radius * radial)
        }
      }
      inside[i] <- inside[i] && lo >= 0
      outside[i] <- outside[i] || hi < 0
    }
  }
  packet <- quadric_faces_3d(packet, which(!inside & !outside))
  subset_quadrics_3d(packet, which(inside[inside | outside]))
}

# Vector drawing ---------------------------------------------------------------

# Chord error, in millimetres, of the sampled exact outlines.
quadric_outline_tolerance_mm_3d <- 0.01

# Part (0 curved surface, 1 and 2 the end disks) and side of each quadric
# fragment in `rows`, per quadric row the exact outline ring (npc; NULL where
# its image is unbounded) and the npc images of its axis ends, and a cache of
# the paint of each quadric part and side.
quadric_fragment_info_3d <- function(scene, rows) {
  f <- scene$fragments; v <- scene$vertices
  first <- f$offset[rows]
  parts <- quadric_fragment_parts_cpp(scene$source, scene$camera_input, scene$viewport, f$source_index[rows],
    cbind(f$normal_x[rows], f$normal_y[rows], f$normal_z[rows]), cbind(v$x[first], v$y[first], v$z[first]))
  quadric <- sort(unique(f$source_index[rows]))
  outline <- quadric_outline_cpp(scene$source, scene$camera_input, scene$viewport, quadric,
    quadric_outline_tolerance_mm_3d)
  rings <- lapply(seq_along(quadric), function(i) {
    k <- outline$ring == i
    if (!any(k)) NULL else list(x = outline$x_npc[k], y = outline$y_npc[k])
  })
  part <- back <- rep(NA, length(f$kind))
  part[rows] <- parts$part; back[rows] <- parts$back
  list(part = part, back = back, quadric = quadric, ring = rings, axis = outline$axis,
    paint = new.env(parent = emptyenv()))
}

# Colours of `fill` lit by the shading kernel at the model-space hits of
# quadric `q` (outward normals and positions, rows), with the quadric's
# material and opacity.
quadric_colours_3d <- function(scene, q, fill, hit, light, opacity) {
  style <- scene$styles[scene$source$quadrics$style_id[q], , drop = FALSE]
  n <- nrow(hit$normal)
  lit <- shade_linear_3d(linear_rgb_3d(rep(fill, n)), hit$normal, scene$camera, light, hit$position,
    rep(material_index_3d(style), n), scene$materials)
  scales::alpha(encode_rgb_3d(lit), opacity)
}

# Colour samples along a gradient: rays through npc points `xy` against
# quadric row `q`, missing hits taking their nearest neighbour's colour.
quadric_gradient_colours_3d <- function(scene, q, xy, cast, back, fill, light, opacity) {
  hit <- quadric_cast_cpp(scene$source, scene$camera_input, scene$viewport, q, xy[, 1L], xy[, 2L], cast, back)
  if (!any(hit$hit)) return(NULL)
  colours <- rep(NA_character_, nrow(xy))
  found <- list(normal = hit$normal[hit$hit, , drop = FALSE], position = hit$position[hit$hit, , drop = FALSE])
  colours[hit$hit] <- quadric_colours_3d(scene, q, fill, found, light, opacity)
  valid <- which(hit$hit)
  nearest <- valid[apply(abs(outer(seq_len(nrow(xy)), valid, "-")), 1L, which.min)]
  colours[nearest]
}

# A radial gradient for the lit image of a sphere: focus at the brightest
# probe, end circle shifted toward the focus by alpha times the focus offset
# and grown to touch the outline on the far side, so the outline on the lit
# side keeps a lighter colour. Stop colours are sampled from the
# kernel along the ray from the focus through the end circle's far point;
# alpha is chosen from a few values by least squares against the exact
# colours at the probes (mm screen points, colours as 3 x n sRGB).
quadric_radial_fit_3d <- function(scene, q, probe, exact, centre, radius, stops, back, fill, light, opacity, to_npc) {
  focus <- probe[which.max(colSums(exact)), ]
  away <- centre - focus
  u <- if (sum(away^2) > 1e-12) away / sqrt(sum(away^2)) else c(1, 0)
  best <- NULL
  for (alpha in c(0, 0.5, 1, 2, 4, 8, 16)) {
    end <- centre - alpha * away
    reach <- radius + sqrt(sum((end - centre)^2))
    d <- end - focus
    along <- outer(stops, d + reach * u) + matrix(focus, length(stops), 2L, byrow = TRUE)
    colours <- quadric_gradient_colours_3d(scene, q, to_npc(along), 1L, back, fill, light, opacity)
    if (is.null(colours)) next
    # Gradient parameter of each probe: the circle through it of the pencil
    # (focus + s d, s reach).
    rel <- sweep(probe, 2L, focus)
    b <- drop(rel %*% d); a <- sum(d^2) - reach^2; cc <- rowSums(rel^2)
    t <- pmin(1, pmax(0, (sqrt(pmax(0, b^2 - a * cc)) - b) / (-a)))
    rgb <- grDevices::col2rgb(colours)
    predicted <- apply(rgb, 1L, function(channel) stats::approx(stops, channel, t, rule = 2)$y)
    error <- sum((t(predicted) - exact)^2)
    if (is.null(best) || error < best$error)
      best <- list(error = error, colours = colours, focus = focus, centre = end, radius = reach)
  }
  best
}

# Paint of one part of a quadric seen from one side, lit by the shading
# kernel: an end disk takes the colour at the image of its centre; the curved
# surface of a sphere a radial gradient from its brightest visible point, of a
# cylinder or cone a linear gradient across the image of its axis, both
# sampled at exact hit points and normals. Gradient
# positions are millimetres from the outline's bounding box, against which
# grid resolves them.
quadric_paint_3d <- function(scene, q, part, back, ring, axis, style, light, opacity) {
  fill <- style$fill
  if (!isTRUE(style_value_3d(style, "shade", FALSE)) || is.null(light) || is.na(fill)) return(scales::alpha(fill, opacity))
  w <- scene$viewport$width_mm; h <- scene$viewport$height_mm
  if (part > 0L) {
    end <- if (part == 1L) axis[1:2] else axis[3:4]
    colour <- quadric_gradient_colours_3d(scene, q, matrix(end, 1L), part + 1L, back, fill, light, opacity)
    return(colour %or3d% scales::alpha(fill, opacity))
  }
  rx <- ring$x * w; ry <- ring$y * h
  lo <- c(min(rx), min(ry))
  stops <- seq(0, 1, length.out = 32L)
  to_npc <- function(p) cbind(p[, 1L] / w, p[, 2L] / h)
  if (scene$source$quadrics$kind[q] == 1L) {
    centre <- c(mean(range(rx)), mean(range(ry)))
    radius <- max(sqrt((rx - centre[1L])^2 + (ry - centre[2L])^2))
    probe <- as.matrix(expand.grid(seq(min(rx), max(rx), length.out = 15L), seq(min(ry), max(ry), length.out = 15L)))
    hit <- quadric_cast_cpp(scene$source, scene$camera_input, scene$viewport, q, probe[, 1L] / w, probe[, 2L] / h, 1L, back)
    if (!any(hit$hit)) return(scales::alpha(fill, opacity))
    probe <- probe[hit$hit, , drop = FALSE]
    exact <- grDevices::col2rgb(quadric_colours_3d(scene, q, fill, list(normal = hit$normal[hit$hit, , drop = FALSE],
      position = hit$position[hit$hit, , drop = FALSE]), light, 1))
    fit <- quadric_radial_fit_3d(scene, q, probe, exact, centre, radius, stops, back, fill, light, opacity, to_npc)
    if (is.null(fit)) return(scales::alpha(fill, opacity))
    return(grid::radialGradient(fit$colours, stops, cx1 = fit$focus[1L] - lo[1L], cy1 = fit$focus[2L] - lo[2L], r1 = 0,
      cx2 = fit$centre[1L] - lo[1L], cy2 = fit$centre[2L] - lo[2L], r2 = fit$radius, default.units = "mm"))
  }
  a0 <- axis[1:2] * c(w, h); a1 <- axis[3:4] * c(w, h)
  middle <- (a0 + a1) / 2; direction <- a1 - a0
  across <- if (sum(direction^2) > 1e-12) c(-direction[2L], direction[1L]) / sqrt(sum(direction^2)) else c(1, 0)
  reach <- range((rx - middle[1L]) * across[1L] + (ry - middle[2L]) * across[2L])
  along <- outer(reach[1L] + stops * diff(reach), across) + matrix(middle, length(stops), 2L, byrow = TRUE)
  colours <- quadric_gradient_colours_3d(scene, q, to_npc(along), 1L, back, fill, light, opacity)
  if (is.null(colours)) return(scales::alpha(fill, opacity))
  start <- along[1L, ] - lo; end <- along[length(stops), ] - lo
  grid::linearGradient(colours, stops, x1 = start[1L], y1 = start[2L], x2 = end[1L], y2 = end[2L], default.units = "mm")
}

# One run of quadric fragments (one quadric, part and side, consecutive in
# draw order): the exact outline, painted, and the union of the fragments
# `clip_rows` to clip it to. A quadric whose image is unbounded draws its
# stand-in facets.
quadric_run_grob_3d <- function(scene, rows, info, styles, light, clip_rows = rows) {
  f <- scene$fragments
  counts <- f$count[rows]
  indices <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  x <- scene$vertices$x_npc[indices]; y <- scene$vertices$y_npc[indices]
  id <- rep(seq_along(rows), counts)
  q <- f$source_index[rows[1L]]; k <- match(q, info$quadric)
  style <- styles[rows[1L], , drop = FALSE]; opacity <- f$opacity[rows[1L]]
  ring <- info$ring[[k]]
  if (is.null(ring)) {
    fill <- rep(style$fill, length(rows))
    if (isTRUE(style_value_3d(style, "shade", FALSE)))
      fill <- shade_fill_3d(fill, f, rows, scene$camera, light, styles[rows, , drop = FALSE], fragment_centres_3d(scene, rows), scene$materials)
    return(list(grob = grid::polygonGrob(x, y, id = id, gp = grid::gpar(fill = fill, col = NA, alpha = opacity)), clip = NULL))
  }
  key <- paste(q, info$part[rows[1L]], info$back[rows[1L]])
  paint <- info$paint[[key]]
  if (is.null(paint)) {
    paint <- quadric_paint_3d(scene, q, info$part[rows[1L]], info$back[rows[1L]], ring, info$axis[k, ], style, light, opacity)
    info$paint[[key]] <- paint
  }
  counts <- f$count[clip_rows]
  indices <- rep(f$offset[clip_rows], counts) + sequence(counts) - 1L
  list(grob = grid::polygonGrob(ring$x, ring$y, gp = grid::gpar(fill = paint, col = NA)),
    clip = grid::pathGrob(scene$vertices$x_npc[indices], scene$vertices$y_npc[indices],
      id = rep(seq_along(clip_rows), counts), rule = "winding"))
}
