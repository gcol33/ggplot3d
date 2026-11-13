axes_3d <- c("x", "y", "z")

# Axes along which every row may carry an interval: both bounds are present.
interval_axes_3d <- function(data) {
  axes_3d[vapply(axes_3d, function(a) all(paste0(a, c("min", "max")) %in% names(data)), logical(1))]
}

# Rows that carry an interval on one axis; rows with both bounds missing have
# none there, so each row may use any subset of the three axes.
interval_rows_3d <- function(data, axis) {
  !(is.na(data[[paste0(axis, "min")]]) & is.na(data[[paste0(axis, "max")]]))
}

cap_planes_3d <- c("auto", "xy", "xz", "yz", "cross")

check_cap_plane_3d <- function(plane) {
  if (!is.character(plane) || length(plane) != 1L || !plane %in% cap_planes_3d) {
    stop("cap.plane must be one of ", paste0('"', cap_planes_3d, '"', collapse = ", "), ".", call. = FALSE)
  }
  plane
}

# Axes along which the caps of a bar on `axis` extend. A bar lying in the plane
# is capped along the plane's other axis; a bar normal to the plane, or any bar
# with "cross", is capped along both perpendicular axes.
cap_axes_3d <- function(axis, plane) {
  plane <- check_cap_plane_3d(plane %or3d% "auto")
  if (plane == "auto") plane <- if (axis == "z") "xz" else "xy"
  if (plane == "cross") return(setdiff(axes_3d, axis))
  in_plane <- strsplit(plane, "", fixed = TRUE)[[1L]]
  if (axis %in% in_plane) setdiff(in_plane, axis) else in_plane
}

# Fills each missing centre from the middle of its interval.
interval_centres_3d <- function(data) {
  axes <- interval_axes_3d(data)
  if (!length(axes)) stop("Interval layers need xmin/xmax, ymin/ymax or zmin/zmax.", call. = FALSE)
  for (axis in axes_3d) {
    if (!axis %in% axes) {
      if (is.null(data[[axis]])) stop("Interval layers need ", axis, " or ", axis, "min and ", axis, "max.", call. = FALSE)
      next
    }
    mid <- (data[[paste0(axis, "min")]] + data[[paste0(axis, "max")]]) / 2
    centre <- data[[axis]] %or3d% mid
    missing <- !is.finite(centre) & is.finite(mid)
    centre[missing] <- mid[missing]
    data[[axis]] <- centre
  }
  data
}

# Geometries with caps store, along every axis a cap extends, the cap's lower
# and upper ends in the `*end` and `*intercept` columns of that axis. Both
# belong to the axis' position family, so they train the scale (as the
# xmin/xmax of ggplot2::geom_errorbar() do) and move with position
# adjustments. The width is `width` in transformed data units, else 0.9 times
# the spacing of the positions along that axis within the panel.
setup_interval_3d <- function(caps) {
  function(data, params) {
    data <- interval_centres_3d(data)
    if (!caps) return(data)
    size <- data$width %or3d% params$width
    if (!is.null(size) && (!is.numeric(size) || any(!is.finite(size) | size < 0))) stop("width must be a nonnegative number.", call. = FALSE)
    for (axis in unique(unlist(lapply(interval_axes_3d(data), cap_axes_3d, plane = params$cap.plane)))) {
      spacing <- vapply(split(data[[axis]], data$PANEL), lattice_spacing_3d, numeric(1)) * 0.9
      half <- rep_len(size %or3d% unname(spacing[as.character(data$PANEL)]), nrow(data)) / 2
      data[[paste0(axis, "end")]] <- data[[axis]] - half
      data[[paste0(axis, "intercept")]] <- data[[axis]] + half
    }
    data
  }
}

# Cap width along one axis, in the units the data are in.
cap_width_3d <- function(data, axis) {
  data[[paste0(axis, "intercept")]] - data[[paste0(axis, "end")]]
}

# Segment rows from `from` to `to`; axes not named keep each row's coordinate.
segment_rows_3d <- function(data, from, to) {
  for (axis in axes_3d) {
    end <- paste0(axis, "end")
    data[[end]] <- to[[axis]] %or3d% data[[axis]]
    data[[axis]] <- from[[axis]] %or3d% data[[axis]]
  }
  data
}

interval_bars_3d <- function(data) {
  bars <- lapply(interval_axes_3d(data), function(axis) {
    rows <- data[interval_rows_3d(data, axis), , drop = FALSE]
    segment_rows_3d(rows, stats::setNames(list(rows[[paste0(axis, "min")]]), axis),
      stats::setNames(list(rows[[paste0(axis, "max")]]), axis))
  })
  do.call(rbind, bars)
}

interval_caps_3d <- function(data, params) {
  caps <- list()
  for (axis in interval_axes_3d(data)) {
    rows <- data[interval_rows_3d(data, axis), , drop = FALSE]
    for (bound in paste0(axis, c("min", "max"))) for (cap in cap_axes_3d(axis, params$cap.plane)) {
      half <- cap_width_3d(rows, cap) / 2
      at <- stats::setNames(list(rows[[bound]]), axis)
      caps[[length(caps) + 1L]] <- segment_rows_3d(rows,
        c(at, stats::setNames(list(rows[[cap]] - half), cap)),
        c(at, stats::setNames(list(rows[[cap]] + half), cap)))
    }
  }
  do.call(rbind, caps)
}

merge_emitted_3d <- function(packets, panel_params) {
  packets <- Filter(Negate(is.null), packets)
  if (length(packets) == 1L) packets[[1L]] else merge_scene_packets(packets, panel_params$panel_id)
}

emit_linerange_3d <- function(data, params, panel_params) {
  emit_segments_3d(interval_bars_3d(data), params, panel_params)
}

emit_errorbar_3d <- function(data, params, panel_params) {
  emit_segments_3d(rbind(interval_bars_3d(data), interval_caps_3d(data, params)), params, panel_params)
}

# Markers drawn at each row's position; rows with a missing shape draw none.
emit_marks_3d <- function(data, params, panel_params, fatten = 1) {
  data <- data[!is.na(data$shape), , drop = FALSE]
  if (!nrow(data)) return(NULL)
  data$size <- data$size * fatten
  emit_points_3d(data, params, panel_params)
}

emit_pointrange_3d <- function(data, params, panel_params) {
  fatten <- params$fatten
  if (!is.numeric(fatten) || length(fatten) != 1L || !is.finite(fatten)) fatten <- 1
  merge_emitted_3d(list(emit_linerange_3d(data, params, panel_params),
    emit_marks_3d(data, params, panel_params, fatten)), panel_params)
}

# Vertices, quadrilateral faces and edges of the unit square (d = 2) or cube
# (d = 3); bit k of a vertex selects the lower or upper side along axis k.
cube_template_3d <- function(d) {
  bits <- as.matrix(expand.grid(rep(list(0:1), d)))
  pairs <- which(upper.tri(diag(nrow(bits))), arr.ind = TRUE)
  edges <- pairs[rowSums(abs(bits[pairs[, 1L], , drop = FALSE] - bits[pairs[, 2L], , drop = FALSE])) == 1L, , drop = FALSE]
  ring <- rbind(c(0L, 0L), c(1L, 0L), c(1L, 1L), c(0L, 1L))
  find <- function(fixed, values) {
    vapply(seq_len(4L), function(r) {
      target <- integer(d); target[fixed] <- values; target[-fixed] <- ring[r, ]
      which(colSums(t(bits) == target) == d)
    }, integer(1))
  }
  faces <- if (d == 1L) NULL else if (d == 2L) {
    matrix(vapply(seq_len(4L), function(r) which(colSums(t(bits) == ring[r, ]) == 2L), integer(1)), 1L)
  } else {
    do.call(rbind, lapply(seq_len(d), function(k) rbind(find(k, 0L), find(k, 1L))))
  }
  list(bits = bits, edges = unname(edges), faces = unname(faces))
}

# Points spanning the template cell: `axis` from its lower to upper bound, each
# cap axis across its width around the row's centre.
cell_vertices_3d <- function(rows, axis, caps, bits) {
  out <- rows
  out[[axis]] <- if (bits[1L]) rows[[paste0(axis, "max")]] else rows[[paste0(axis, "min")]]
  for (j in seq_along(caps)) {
    out[[caps[j]]] <- rows[[caps[j]]] + (bits[j + 1L] - 0.5) * cap_width_3d(rows, caps[j])
  }
  out
}

emit_crossbar_3d <- function(data, params, panel_params) {
  vertices <- list(); faces <- list(); edges <- list(); middles <- list()
  cell <- integer(); offset <- 0L
  for (axis in interval_axes_3d(data)) {
    rows <- data[interval_rows_3d(data, axis), , drop = FALSE]
    rows <- rows[valid_rows_3d(rows, params, c(axes_3d, paste0(axis, c("min", "max")))), , drop = FALSE]
    m <- nrow(rows)
    if (!m) next
    caps <- cap_axes_3d(axis, params$cap.plane)
    template <- cube_template_3d(length(caps) + 1L)
    for (v in seq_len(nrow(template$bits))) {
      vertices[[length(vertices) + 1L]] <- cell_vertices_3d(rows, axis, caps, template$bits[v, ])
    }
    index <- function(v) offset + (v - 1L) * m + seq_len(m)
    expand <- function(topology) do.call(rbind, lapply(seq_len(nrow(topology)), function(i)
      do.call(cbind, lapply(topology[i, ], index))))
    faces[[length(faces) + 1L]] <- expand(template$faces)
    edges[[length(edges) + 1L]] <- expand(template$edges)
    cell <- c(cell, rep(offset + seq_len(m), nrow(template$bits)))
    offset <- offset + m * nrow(template$bits)
    middle <- cube_template_3d(length(caps))
    for (e in seq_len(nrow(middle$edges))) {
      ends <- lapply(middle$edges[e, ], function(v) {
        at <- cell_vertices_3d(rows, axis, caps, c(0L, middle$bits[v, ]))
        at[[axis]] <- rows[[axis]]
        at[axes_3d]
      })
      middles[[length(middles) + 1L]] <- segment_rows_3d(rows, ends[[1L]], ends[[2L]])
    }
  }
  if (!length(vertices)) return(new_scene_packet(panel_id = panel_params$panel_id))
  box <- apply_gp_3d(do.call(rbind, vertices), params$box_gp)
  faces <- orient_faces_3d(box, do.call(rbind, faces), cell)
  fatten <- params$fatten
  if (!is.numeric(fatten) || length(fatten) != 1L || !is.finite(fatten)) fatten <- 2.5
  middle <- do.call(rbind, middles)
  middle$linewidth <- middle$linewidth * fatten
  middle$alpha <- NA
  merge_emitted_3d(list(packet_from_faces_edges_3d(box, faces, do.call(rbind, edges), params, panel_params),
    emit_segments_3d(apply_gp_3d(middle, params$middle_gp), params, panel_params)), panel_params)
}

# Stroke overrides in the form of ggplot2's crossbar middle_gp and box_gp.
apply_gp_3d <- function(data, gp) {
  for (name in intersect(c("colour", "linetype", "linewidth"), names(gp))) {
    if (!is.null(gp[[name]])) data[[name]] <- rep_len(gp[[name]], nrow(data))
  }
  data
}

# Drops zero-area quadrilaterals and turns the others to face away from the
# centre of their cell, so solid boxes have outward normals.
orient_faces_3d <- function(vertices, faces, cell) {
  p <- as.matrix(vertices[axes_3d])
  d1 <- p[faces[, 3L], , drop = FALSE] - p[faces[, 1L], , drop = FALSE]
  d2 <- p[faces[, 4L], , drop = FALSE] - p[faces[, 2L], , drop = FALSE]
  normal <- d1[, c(2L, 3L, 1L), drop = FALSE] * d2[, c(3L, 1L, 2L), drop = FALSE] -
    d1[, c(3L, 1L, 2L), drop = FALSE] * d2[, c(2L, 3L, 1L), drop = FALSE]
  area <- sqrt(rowSums(normal^2))
  keep <- is.finite(area) & area > 256 * .Machine$double.eps * pmax(rowSums(d1^2), rowSums(d2^2))
  centres <- rowsum(p, cell[seq_len(nrow(p))]) / as.vector(table(cell))
  centre <- centres[match(cell[faces[, 1L]], as.integer(rownames(centres))), , drop = FALSE]
  facing <- rowSums(normal * ((p[faces[, 1L], , drop = FALSE] + p[faces[, 3L], , drop = FALSE]) / 2 - centre))
  flip <- keep & facing < 0
  faces[flip, ] <- faces[flip, 4:1, drop = FALSE]
  faces[keep, , drop = FALSE]
}

draw_key_pointrange_3d <- function(data, params, size) {
  line <- ggplot2::draw_key_linerange(data, params, size)
  grob <- grid::grobTree(line, ggplot2::draw_key_point(data, params, size))
  attr(grob, "width") <- attr(line, "width")
  attr(grob, "height") <- attr(line, "height")
  grob
}

interval_aes_3d <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")

GeomLinerange3D <- ggplot2::ggproto("GeomLinerange3D", ggplot2::Geom,
  required_aes = character(), optional_aes = c("x", "y", "z", interval_aes_3d),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_linerange
)

GeomErrorbar3D <- ggplot2::ggproto("GeomErrorbar3D", GeomLinerange3D,
  draw_key = ggplot2::draw_key_path
)

GeomPointrange3D <- ggplot2::ggproto("GeomPointrange3D", GeomLinerange3D,
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
    size = 2, shape = 19, fill = NA, stroke = 0.5),
  draw_key = draw_key_pointrange_3d
)

GeomCrossbar3D <- ggplot2::ggproto("GeomCrossbar3D", GeomLinerange3D,
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA, fill = NA),
  draw_key = ggplot2::draw_key_crossbar
)

crossbar_gp_3d <- function(colour, linetype, linewidth) {
  list(colour = colour, linetype = linetype, linewidth = linewidth)
}

#' Intervals along x, y and z: error bars, line ranges, point ranges and crossbars
#'
#' Each row draws a bar along every axis for which it has both bounds:
#' `xmin`/`xmax`, `ymin`/`ymax`, `zmin`/`zmax`, in any combination. A row with
#' both bounds missing on an axis draws no bar there. `geom_errorbar_3d()` adds
#' caps at both ends, `geom_pointrange_3d()` a point at `x`, `y`, `z`, and
#' `geom_crossbar_3d()` a box spanning the interval with a line at the centre.
#' A missing centre is taken as the middle of the interval; a layer without z
#' lies on the floor of the box.
#'
#' Caps and crossbar boxes are perpendicular to their bar and lie in the plane
#' named by `cap.plane`. A bar lying in that plane is capped along the plane's
#' other axis. A bar normal to the plane, or any bar with `"cross"`, is capped
#' along both perpendicular axes: a cross for error bars and a cuboid for
#' crossbars, which read the same from every view. With `"auto"`, bars along z
#' are capped along x and bars along x or y within the floor plane.
#'
#' The registered ggplot2 layers [ggplot2::geom_errorbar()],
#' [ggplot2::geom_linerange()], [ggplot2::geom_pointrange()] and
#' [ggplot2::geom_crossbar()] draw their x and y intervals the same way inside
#' [coord_3d()], on the floor unless z is mapped; z intervals need the layers
#' documented here.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   layer arguments. Positions: [position_nudge_3d()], [position_jitter_3d()]
#'   and [position_dodge_3d()].
#' @param width Cap and box width in transformed data units of the axis it
#'   extends along. Defaults to 0.9 times the spacing of the positions along
#'   that axis.
#' @param cap.plane Plane of the caps: `"auto"`, `"xy"`, `"xz"`, `"yz"` or
#'   `"cross"`.
#' @param fatten Factor applied to `linewidth` for the crossbar's middle line.
#' @param middle.colour,middle.color,middle.linetype,middle.linewidth,box.colour,box.color,box.linetype,box.linewidth
#'   Stroke of the crossbar's middle line and box, overriding the layer's
#'   aesthetics.
#' @return A ggplot2 layer.
#' @seealso [stat_summary_3d()] computes intervals from data, [geom_stem_3d()]
#'   draws stems to a base value.
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:2)
#' d$z <- c(2, 3, 5, 4, 6, 5)
#' d$se <- c(0.4, 0.6, 0.5, 0.8, 0.3, 0.7)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se))
#' p + geom_errorbar_3d(width = 0.2) + geom_point_3d() + coord_3d()
#' p + geom_pointrange_3d(colour = "steelblue") + coord_3d()
#' p + geom_crossbar_3d(width = 0.3, fill = "grey85", cap.plane = "cross") + coord_3d()
#'
#' # Intervals on all three axes at once
#' e <- data.frame(x = c(1, 2), y = c(1, 3), z = c(2, 1))
#' ggplot2::ggplot(e, ggplot2::aes(x, y, z = z,
#'     xmin = x - 0.3, xmax = x + 0.3, ymin = y - 0.5, ymax = y + 0.5,
#'     zmin = z - 0.4, zmax = z + 0.4)) +
#'   geom_errorbar_3d(width = 0.15, cap.plane = "cross") + coord_3d()
#'
#' # Ranges on the floor, from x/y alone
#' ggplot2::ggplot(e, ggplot2::aes(x, y, ymin = y - 1, ymax = y + 1)) +
#'   geom_linerange_3d(linewidth = 1) + coord_3d()
#' @export
geom_errorbar_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             width = NULL, cap.plane = "auto",
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomErrorbar3D, mapping, data, stat, position,
    c(list(width = width, cap.plane = check_cap_plane_3d(cap.plane), na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

#' @rdname geom_errorbar_3d
#' @export
geom_linerange_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomLinerange3D, mapping, data, stat, position,
    c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_errorbar_3d
#' @export
geom_pointrange_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomPointrange3D, mapping, data, stat, position,
    c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_errorbar_3d
#' @export
geom_crossbar_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             width = NULL, cap.plane = "auto", fatten = 2.5,
                             middle.colour = NULL, middle.color = NULL, middle.linetype = NULL,
                             middle.linewidth = NULL, box.colour = NULL, box.color = NULL,
                             box.linetype = NULL, box.linewidth = NULL,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomCrossbar3D, mapping, data, stat, position,
    c(list(width = width, cap.plane = check_cap_plane_3d(cap.plane), fatten = fatten,
      middle_gp = crossbar_gp_3d(middle.color %or3d% middle.colour, middle.linetype, middle.linewidth),
      box_gp = crossbar_gp_3d(box.color %or3d% box.colour, box.linetype, box.linewidth),
      na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

interval_positions_3d <- c("nudge", "jitter", "dodge")
crossbar_parameters_3d <- list(cap.plane = "auto", fatten = 2.5, middle_gp = NULL, box_gp = NULL)

register_interval_3d <- function(geom, emit, parameters = list(cap.plane = "auto"), caps = FALSE) {
  register_geom_3d(geom, emit, aesthetics = interval_aes_3d, setup_data = setup_interval_3d(caps),
    parameters = parameters, default_z = -Inf, positions = interval_positions_3d, tween = "row")
}

register_interval_3d(GeomLinerange3D, emit_linerange_3d)
register_interval_3d(GeomErrorbar3D, emit_errorbar_3d, caps = TRUE)
register_interval_3d(GeomPointrange3D, emit_pointrange_3d)
register_interval_3d(GeomCrossbar3D, emit_crossbar_3d, crossbar_parameters_3d, caps = TRUE)
register_interval_3d(ggplot2::GeomLinerange, emit_linerange_3d)
register_interval_3d(ggplot2::GeomErrorbar, emit_errorbar_3d, caps = TRUE)
register_interval_3d(ggplot2::GeomPointrange, emit_pointrange_3d, list(cap.plane = "auto", fatten = 4))
register_interval_3d(ggplot2::GeomCrossbar, emit_crossbar_3d, crossbar_parameters_3d, caps = TRUE)

GeomStem3D <- ggplot2::ggproto("GeomStem3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
    size = 1.5, shape = 19, fill = NA, stroke = 0.5),
  draw_key = draw_key_pointrange_3d
)

prepare_stem_3d <- function(out, data, plot, params) {
  end <- paste0(params$axis %or3d% "z", "end")
  if (is.null(out[[end]])) out[[end]] <- rep_len(params$base %or3d% 0, nrow(out))
  out
}

#' Stems from a base value to each point
#'
#' Draws a line from each point to `base` along `axis`, with a marker at the
#' point, as `stem3` in MATLAB and `stem(orientation = )` in matplotlib's 3D
#' axes. Unlike [geom_dropline_3d()], which ends on a face of the box, a stem
#' ends at a data value, so positive and negative values point away from the
#' base in opposite directions.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   layer arguments. Mapping `zend` (or `xend`, `yend` for those axes) gives
#'   each stem its own base.
#' @param base Base value in original data units of `axis`.
#' @param axis Axis the stems run along: `"z"`, `"x"` or `"y"`.
#' @return A ggplot2 layer. Set `shape = NA` to draw the stems alone.
#' @examples
#' d <- expand.grid(x = 1:5, y = 1:3)
#' d$z <- sin(d$x) * d$y
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = z > 0)) +
#'   geom_stem_3d() + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_stem_3d(axis = "x", base = 0, shape = 21, fill = "white") + coord_3d()
#' @export
geom_stem_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                         base = 0, axis = "z", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  axis <- check_axis_3d(axis, axes_3d)
  if (!is.numeric(base) || length(base) != 1L || is.na(base)) stop("base must be a number.", call. = FALSE)
  make_layer_3d(GeomStem3D, mapping, data, stat, position,
    c(list(base = base, axis = axis, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

emit_stems_3d <- function(data, params, panel_params) {
  axis <- params$axis %or3d% "z"
  stems <- segment_rows_3d(data, list(), stats::setNames(list(data[[paste0(axis, "end")]]), axis))
  merge_emitted_3d(list(emit_segments_3d(stems, params, panel_params),
    emit_marks_3d(data, params, panel_params)), panel_params)
}

register_geom_3d(GeomStem3D, emit_stems_3d, aesthetics = c("xend", "yend", "zend"),
  prepare_aesthetics = prepare_stem_3d, parameters = list(axis = "z"), positions = interval_positions_3d)

# Summary of one numeric vector as a data frame with columns y, ymin and ymax
# (any subset), following ggplot2::stat_summary().
summary_fun_3d <- function(fun.data = NULL, fun = NULL, fun.max = NULL, fun.min = NULL, fun.args = list()) {
  as_fun <- function(f) if (is.null(f)) NULL else match.fun(f)
  if (!is.null(fun.data)) {
    summarise <- as_fun(fun.data)
    return(function(v) as.data.frame(do.call(summarise, c(list(v), fun.args))))
  }
  funs <- Filter(Negate(is.null), list(ymin = as_fun(fun.min), y = as_fun(fun), ymax = as_fun(fun.max)))
  if (length(funs)) {
    return(function(v) as.data.frame(lapply(funs, function(f) do.call(f, c(list(v), fun.args)))))
  }
  message("No summary function supplied, defaulting to `mean_se()`.")
  function(v) ggplot2::mean_se(v)
}

StatSummary3D <- ggplot2::ggproto("StatSummary3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  extra_params = c("na.rm", "fun.data", "fun", "fun.max", "fun.min", "fun.args"),
  setup_params = function(data, params) {
    params$fun <- summary_fun_3d(params$fun.data, params$fun, params$fun.max, params$fun.min,
      params$fun.args %or3d% list())
    params
  },
  compute_panel = function(data, scales, fun = NULL, axis = "z") {
    others <- setdiff(axes_3d, axis)
    cells <- interaction(data$group, data[[others[1L]]], data[[others[2L]]], drop = TRUE, lex.order = TRUE)
    pieces <- lapply(split(data, cells), function(d) {
      values <- d[[axis]]
      summary <- fun(values[is.finite(values)])
      names(summary) <- ifelse(names(summary) %in% c("y", "ymin", "ymax"), sub("^y", axis, names(summary)), names(summary))
      constant <- names(d)[vapply(d, function(column) length(unique(column)) == 1L, logical(1))]
      keep <- d[rep.int(1L, nrow(summary)), setdiff(constant, c(axis, names(summary))), drop = FALSE]
      cbind(keep, summary)
    })
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    out
  }
)

summary_geoms_3d <- function() {
  list(pointrange = GeomPointrange3D, errorbar = GeomErrorbar3D, linerange = GeomLinerange3D,
       crossbar = GeomCrossbar3D, point = ggplot2::GeomPoint, path = ggplot2::GeomPath,
       line = ggplot2::GeomLine)
}

#' Summarise one axis at each position on the other two
#'
#' For every group and every distinct pair of positions on the two other axes,
#' summarises the values along `axis` into a centre and an interval, as
#' [ggplot2::stat_summary()] does for y at each x. The summary function returns
#' columns `y`, `ymin` and `ymax` (as [ggplot2::mean_se()] and the Hmisc
#' wrappers such as [ggplot2::mean_cl_normal()] do); they become `z`, `zmin`
#' and `zmax`, or the matching columns of `axis`. Summaries are computed on
#' the transformed scale.
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param geom Geometry drawing the summary: `"pointrange"`, `"errorbar"`,
#'   `"linerange"`, `"crossbar"`, `"point"`, `"path"`, `"line"`, or a
#'   registered geom prototype.
#' @param fun.data Function, or its name, taking a numeric vector and returning
#'   a data frame with any of `y`, `ymin` and `ymax`. Defaults to
#'   [ggplot2::mean_se()] when no summary function is given.
#' @param fun,fun.min,fun.max Functions, or names, each returning one number:
#'   the centre, lower and upper bound. Used when `fun.data` is `NULL`.
#' @param fun.args List of further arguments to the summary functions.
#' @param axis Axis to summarise: `"z"`, `"x"` or `"y"`.
#' @return A ggplot2 layer.
#' @examples
#' set.seed(1)
#' d <- expand.grid(x = 1:3, y = 1:2, rep = 1:10)
#' d$z <- d$x + d$y + rnorm(nrow(d), sd = 0.5)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + stat_summary_3d() + coord_3d()
#' p + stat_summary_3d(geom = "errorbar", fun = median,
#'   fun.min = function(z) quantile(z, 0.25), fun.max = function(z) quantile(z, 0.75),
#'   width = 0.2) + coord_3d()
#' p + stat_summary_3d(geom = "crossbar", fun.data = ggplot2::mean_se,
#'   fun.args = list(mult = 2), width = 0.3, cap.plane = "cross", fill = "grey90") + coord_3d()
#' @export
stat_summary_3d <- function(mapping = NULL, data = NULL, geom = "pointrange", position = "identity", ...,
                            fun.data = NULL, fun = NULL, fun.max = NULL, fun.min = NULL,
                            fun.args = list(), axis = "z",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (is.character(geom)) {
    geoms <- summary_geoms_3d()
    if (length(geom) != 1L || !geom %in% names(geoms)) {
      stop("geom must be one of ", paste0('"', names(geoms), '"', collapse = ", "), " or a geom prototype.", call. = FALSE)
    }
    geom <- geoms[[geom]]
  }
  if (!inherits(geom, "Geom")) stop("geom must be a geometry name or prototype.", call. = FALSE)
  make_layer_3d(geom, mapping, data, StatSummary3D, position,
    c(list(fun.data = fun.data, fun = fun, fun.max = fun.max, fun.min = fun.min, fun.args = fun.args,
      axis = check_axis_3d(axis, axes_3d), na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}
