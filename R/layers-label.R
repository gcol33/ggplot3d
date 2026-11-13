utils::globalVariables("repel_labels_cpp")

GeomTextRepel3D <- ggplot2::ggproto("GeomTextRepel3D", ggplot2::GeomText)
GeomLabelRepel3D <- ggplot2::ggproto("GeomLabelRepel3D", ggplot2::GeomLabel)

#' Labels with boxes and labels that repel each other
#'
#' `geom_label_3d()` draws text on a rounded box at each 3D position, like
#' [ggplot2::geom_label()]. `geom_text_repel_3d()` and `geom_label_repel_3d()`
#' move each label away from the other labels and from the data points, and
#' join a moved label to its point with a leader line, as in the ggrepel
#' package's `geom_text_repel()` and `geom_label_repel()`.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param parse Whether to parse labels as plotmath expressions.
#' @param label.padding,label.r Padding around the text and corner radius of
#'   the box, as grid units or numbers of lines.
#' @param box.padding Space kept free around each label box, as a grid unit or
#'   a number of lines (default 0.25).
#' @param point.padding Space kept free around each data point, as a grid unit
#'   or a number of lines.
#' @param min.segment.length Leader lines shorter than this are not drawn, as a
#'   grid unit or a number of lines (default 0.5). `Inf` draws none, 0 draws
#'   every leader of a label that does not cover its point.
#' @param force Strength of the push between overlapping labels and between a
#'   label and a data point it covers (default 1).
#' @param force_pull Strength of the pull of each label back to its point
#'   (default 1).
#' @param max.iter Maximum number of solver iterations (default 10000).
#' @param max.time Maximum solver time in seconds per panel. The default `Inf`
#'   leaves the iteration count as the only limit, so a seeded layout is the
#'   same on every machine.
#' @param max.overlaps A label that overlaps more than this number of other
#'   labels and data points where it starts is not drawn. Defaults to the
#'   option `ggplot3d.max.overlaps`, or 10.
#' @param nudge_x,nudge_y Starting offset of each label from its point, in
#'   millimetres on the page (default 0).
#' @param xlim,ylim Range the labels stay within, as fractions of the panel
#'   width and height. `NA` stands for the panel edge.
#' @param direction Move labels in both directions (default), or only along
#'   `"x"` or `"y"` of the page.
#' @param seed Seed of the random starting directions. `NA` (default) draws
#'   one from R's random number generator at each draw.
#' @details
#' Labels are placed on the page when the panel is drawn, after projection, so
#' they never overlap whatever the camera angle. A force-directed solver in C++
#' moves the label boxes: overlapping boxes push each other apart along the
#' line between their centres, every data point of the layer pushes boxes off
#' its disc of radius `point.size` plus `point.padding`, and a spring pulls each
#' box back until it touches its own point. Each step keeps part of the
#' previous velocity, which damps oscillation. The solver stops once no box
#' overlaps and the boxes are at rest.
#'
#' Rows with an empty or missing label are not drawn but still push labels
#' away. The repelled labels and their leader lines are drawn over the scene,
#' not hidden by surfaces in front of their points. [animate_3d()] starts each
#' frame's labels from their offsets in the previous frame, matched by label
#' text, so labels keep their side of the point while the camera moves.
#' [view_3d()] does not show repelled labels.
#'
#' Extra aesthetics of the repel layers: `point.size` (size of the data points
#' to keep clear of, in the units of the `size` aesthetic of points, default
#' 1.5), `segment.colour`, `segment.size`, `segment.alpha` and
#' `segment.linetype` (leader colour, width in mm, opacity and line type;
#' default the label colour, 0.5, the label opacity and solid).
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = c(1, 1.1, 1.2, 2, 3), y = c(1, 1.1, 1, 2, 1),
#'                 z = c(1, 1.2, 1.1, 2, 3), name = c("alpha", "beta", "gamma", "delta", "epsilon"))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = name)) + geom_point_3d()
#' p + geom_label_3d(fill = "white") + coord_3d()
#' p + geom_text_repel_3d(seed = 1) + coord_3d()
#' p + geom_label_repel_3d(seed = 1, segment.colour = "grey40") + coord_3d()
#' @export
geom_label_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          parse = FALSE, label.padding = grid::unit(0.25, "lines"),
                          label.r = grid::unit(0.15, "lines"), na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomLabel, mapping, data, stat, position,
    c(list(parse = parse, label.padding = label.padding, label.r = label.r, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

#' @rdname geom_label_3d
#' @export
geom_text_repel_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                               parse = FALSE, box.padding = 0.25, point.padding = 1e-6,
                               min.segment.length = 0.5, force = 1, force_pull = 1,
                               max.time = Inf, max.iter = 10000,
                               max.overlaps = getOption("ggplot3d.max.overlaps", default = 10),
                               nudge_x = 0, nudge_y = 0, xlim = c(NA, NA), ylim = c(NA, NA),
                               direction = c("both", "y", "x"), seed = NA,
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  settings <- repel_settings_3d(box.padding, point.padding, min.segment.length, force, force_pull,
    max.time, max.iter, max.overlaps, nudge_x, nudge_y, xlim, ylim, match.arg(direction), seed)
  make_layer_3d(GeomTextRepel3D, mapping, data, stat, position,
    c(list(parse = parse, na.rm = na.rm), settings, list(...)), show.legend, inherit.aes)
}

#' @rdname geom_label_3d
#' @export
geom_label_repel_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                                parse = FALSE, box.padding = 0.25, label.padding = grid::unit(0.25, "lines"),
                                point.padding = 1e-6, label.r = grid::unit(0.15, "lines"),
                                min.segment.length = 0.5, force = 1, force_pull = 1,
                                max.time = Inf, max.iter = 10000,
                                max.overlaps = getOption("ggplot3d.max.overlaps", default = 10),
                                nudge_x = 0, nudge_y = 0, xlim = c(NA, NA), ylim = c(NA, NA),
                                direction = c("both", "y", "x"), seed = NA,
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  settings <- repel_settings_3d(box.padding, point.padding, min.segment.length, force, force_pull,
    max.time, max.iter, max.overlaps, nudge_x, nudge_y, xlim, ylim, match.arg(direction), seed)
  make_layer_3d(GeomLabelRepel3D, mapping, data, stat, position,
    c(list(parse = parse, label.padding = label.padding, label.r = label.r, na.rm = na.rm), settings, list(...)),
    show.legend, inherit.aes)
}

repel_settings_3d <- function(box.padding, point.padding, min.segment.length, force, force_pull,
                              max.time, max.iter, max.overlaps, nudge_x, nudge_y, xlim, ylim,
                              direction, seed) {
  is_length <- function(x) (grid::is.unit(x) || is.numeric(x)) && length(x) == 1L && !is.na(as.numeric(x)) && as.numeric(x) >= 0
  is_number <- function(x, low = 0) is.numeric(x) && length(x) == 1L && !is.na(x) && x >= low
  if (!is_length(box.padding) || !is_length(point.padding) || !is_length(min.segment.length))
    stop("box.padding, point.padding and min.segment.length must be one nonnegative length.", call. = FALSE)
  if (!is_number(force) || !is_number(force_pull) || !is_number(max.time) || !is_number(max.iter, 1) ||
      !is_number(max.overlaps))
    stop("force, force_pull, max.time, max.iter and max.overlaps must be one nonnegative number; max.iter at least 1.", call. = FALSE)
  if (!is.numeric(nudge_x) || !is.numeric(nudge_y) || length(nudge_x) != 1L || length(nudge_y) != 1L ||
      !is.finite(nudge_x) || !is.finite(nudge_y))
    stop("nudge_x and nudge_y must be one finite number of millimetres.", call. = FALSE)
  for (limit in list(xlim, ylim)) {
    if (length(limit) != 2L || !all(is.na(limit) | is.numeric(limit)))
      stop("xlim and ylim must be two panel fractions or NA.", call. = FALSE)
  }
  if (!(length(seed) == 1L && (is.na(seed) || (is.numeric(seed) && is.finite(seed)))))
    stop("seed must be one number or NA.", call. = FALSE)
  list(box.padding = box.padding, point.padding = point.padding, min.segment.length = min.segment.length,
    force = force, force_pull = force_pull, max.time = max.time, max.iter = max.iter,
    max.overlaps = max.overlaps, nudge_x = nudge_x, nudge_y = nudge_y,
    xlim = as.double(xlim), ylim = as.double(ylim), direction = direction, seed = as.double(seed))
}

repel_parameters_3d <- function() {
  repel_settings_3d(0.25, 1e-6, 0.5, 1, 1, Inf, 10000, 10, 0, 0, c(NA, NA), c(NA, NA), "both", NA)
}

repel_aesthetics_3d <- c("point.size", "segment.colour", "segment.size", "segment.alpha", "segment.linetype")

column_or_3d <- function(data, name, default) {
  value <- data[[name]]
  if (is.null(value)) return(rep(default, nrow(data)))
  if (length(default) == nrow(data)) value[is.na(value)] <- default[is.na(value)]
  else value[is.na(value)] <- default
  value
}

# The emitter keeps everything the placement needs: anchors in frame space,
# the label styles, point discs and leader styles. Placement happens at draw
# time, when the projection and the page size are known.
repel_emitter_3d <- function(boxed) {
  function(data, params, panel_params) {
    data <- data[valid_rows_3d(data, params), , drop = FALSE]
    if (isTRUE(panel_params$camera$clip_data)) {
      inside <- Reduce(`&`, lapply(data[c("x", "y", "z")], function(v) v >= -1e-9 & v <= 1 + 1e-9))
      data <- data[inside, , drop = FALSE]
    }
    if (boxed) data <- label_box_columns_3d(data, params)
    metrics <- text_rows_metrics_3d(data, params)
    to_mm <- function(value) label_unit_mm_3d(value, metrics$fontsize, metrics$lineheight)
    label <- data$label
    text <- if (is.null(label)) character(nrow(data)) else as.character(label)
    labelled <- which(!is.na(text) & nzchar(text))
    material <- if (length(labelled)) styles_3d(data, labelled, rep("text", length(labelled)), params) else
      list(styles = data.frame(style_id = integer()), index = integer(), opacity = numeric())
    point_size <- column_or_3d(data, "point.size", 1.5)
    point_size[!is.finite(point_size)] <- 0
    radius <- ifelse(point_size > 0, paint_metrics_3d(data.frame(size = point_size))$radius_mm, 0)
    colour <- column_or_3d(data, "segment.colour", data$colour %or3d% rep("black", nrow(data)))
    alpha <- column_or_3d(data, "alpha", 1)
    alpha <- column_or_3d(data, "segment.alpha", alpha)
    segment <- data.frame(colour = colour, alpha = alpha,
      lwd = column_or_3d(data, "segment.size", 0.5) * 72.27 / 25.4,
      linetype = column_or_3d(data, "segment.linetype", 1))[labelled, , drop = FALSE]
    keys <- text[labelled]
    keys <- paste(keys, stats::ave(seq_along(keys), keys, FUN = seq_along), sep = "\r")
    grid::gTree(anchors = frame_of_3d(panel_params)$map(as.matrix(data[c("x", "y", "z")])),
      radius = radius + to_mm(params$point.padding), labelled = labelled,
      styles = material$styles, style = material$index, opacity = material$opacity,
      box_padding = to_mm(params$box.padding)[labelled],
      min_segment = to_mm(params$min.segment.length)[labelled],
      segment = segment, arrow = params$arrow, arrow_fill = params$arrow.fill, keys = keys,
      settings = params[names(repel_parameters_3d())],
      state_key = paste(panel_params$panel_id, data$.layer[1L] %or3d% 0L),
      cl = c("scene_repel_grob", "scene_overlay_grob"))
  }
}

# Width and height of the axis-aligned box that holds a rotated rectangle.
rotated_extent_3d <- function(extent, angle) {
  radians <- angle * pi / 180
  cbind(abs(extent[, 1L] * cos(radians)) + abs(extent[, 2L] * sin(radians)),
        abs(extent[, 1L] * sin(radians)) + abs(extent[, 2L] * cos(radians)))
}

# Leader from the edge of a label box to the edge of its point's disc.
repel_leaders_3d <- function(anchor, centre, half, radius) {
  end <- cbind(pmin(pmax(anchor[, 1L], centre[, 1L] - half[, 1L]), centre[, 1L] + half[, 1L]),
               pmin(pmax(anchor[, 2L], centre[, 2L] - half[, 2L]), centre[, 2L] + half[, 2L]))
  delta <- end - anchor
  distance <- sqrt(rowSums(delta^2))
  start <- anchor + delta * ifelse(distance > 0, pmin(radius, distance) / distance, 0)
  list(x0 = end[, 1L], y0 = end[, 2L], x1 = start[, 1L], y1 = start[, 2L],
       length = pmax(distance - radius, 0))
}

repel_start_3d <- function(grob, keep, anchor, state) {
  settings <- grob$settings
  start <- sweep(anchor, 2L, c(settings$nudge_x, settings$nudge_y), `+`)
  previous <- if (is.null(state)) NULL else state[[grob$state_key]]
  if (is.null(previous)) return(list(start = start, warm = FALSE))
  match <- match(grob$keys[keep], previous$keys)
  found <- !is.na(match)
  start[found, ] <- anchor[found, , drop = FALSE] + previous$offset[match[found], , drop = FALSE]
  list(start = start, warm = all(found))
}

#' @export
place_overlay_3d.scene_repel_grob <- function(grob, context) {
  empty <- grid::gTree(children = grid::gList(), hidden = 0L, name = "scene-repel-3d")
  xy <- context$project(grob$anchors)[, 1:2, drop = FALSE]
  visible <- rowSums(is.finite(xy)) == 2L
  keep <- which(visible[grob$labelled])
  if (!length(keep)) return(empty)
  points <- which(visible)
  settings <- grob$settings
  styles <- grob$styles[grob$style[keep], , drop = FALSE]
  measured <- unique(grob$style[keep])
  extent <- text_extent_mm_3d(grob$styles[measured, , drop = FALSE])[match(grob$style[keep], measured), , drop = FALSE]
  angle <- as.numeric(style_value_3d(styles, "angle", 0))
  half_box <- rotated_extent_3d(extent, angle) / 2
  anchor <- xy[grob$labelled[keep], , drop = FALSE]
  start <- repel_start_3d(grob, keep, anchor, context$state)
  limits <- function(range, size) {
    range <- range * size
    c(if (is.na(range[1L])) 0 else range[1L], if (is.na(range[2L])) size else range[2L])
  }
  bounds <- c(limits(settings$xlim, context$width_mm), limits(settings$ylim, context$height_mm))
  seed <- if (is.na(settings$seed)) sample.int(.Machine$integer.max, 1L) else settings$seed
  fit <- repel_labels_cpp(xy[points, , drop = FALSE], grob$radius[points],
    match(grob$labelled[keep], points), start$start, half_box + grob$box_padding[keep],
    bounds, settings$force, settings$force_pull, as.integer(min(settings$max.iter, .Machine$integer.max)),
    settings$max.time, as.integer(min(settings$max.overlaps, .Machine$integer.max)),
    match(settings$direction, c("both", "x", "y")) - 1L, seed, !start$warm)
  centre <- cbind(fit$x, fit$y)
  if (!is.null(context$state)) {
    context$state[[grob$state_key]] <- list(keys = grob$keys[keep], offset = centre - anchor)
  }
  shown <- which(!fit$hidden)
  if (!length(shown)) { empty$hidden <- length(keep); return(empty) }
  leaders <- repel_leaders_3d(anchor[shown, , drop = FALSE], centre[shown, , drop = FALSE],
    half_box[shown, , drop = FALSE], grob$radius[grob$labelled[keep[shown]]])
  inside <- abs(anchor[shown, 1L] - centre[shown, 1L]) <= half_box[shown, 1L] &
    abs(anchor[shown, 2L] - centre[shown, 2L]) <= half_box[shown, 2L]
  drawn <- which(!inside & leaders$length > 0 & leaders$length >= grob$min_segment[keep[shown]])
  children <- list()
  if (length(drawn)) {
    segment <- grob$segment[keep[shown[drawn]], , drop = FALSE]
    fill <- grob$arrow_fill %or3d% segment$colour
    children$segments <- grid::segmentsGrob(leaders$x0[drawn], leaders$y0[drawn], leaders$x1[drawn], leaders$y1[drawn],
      default.units = "mm", arrow = grob$arrow, name = "scene-repel-leaders",
      gp = grid::gpar(col = segment$colour, fill = fill, alpha = segment$alpha, lwd = segment$lwd,
        lty = segment$linetype))
  }
  hjust <- as.numeric(style_value_3d(styles, "hjust", 0.5))[shown]
  vjust <- as.numeric(style_value_3d(styles, "vjust", 0.5))[shown]
  radians <- angle[shown] * pi / 180
  dx <- (hjust - 0.5) * extent[shown, 1L]; dy <- (vjust - 0.5) * extent[shown, 2L]
  x <- centre[shown, 1L] + dx * cos(radians) - dy * sin(radians)
  y <- centre[shown, 2L] + dx * sin(radians) + dy * cos(radians)
  children$labels <- text_grob_3d(styles[shown, , drop = FALSE], grid::unit(x, "mm"), grid::unit(y, "mm"),
    hjust, vjust, angle[shown], grob$opacity[keep[shown]])
  children$labels$name <- "scene-repel-labels"
  grid::gTree(children = do.call(grid::gList, unname(children)), hidden = length(keep) - length(shown),
    iterations = fit$iterations, name = "scene-repel-3d")
}

register_geom_3d(GeomTextRepel3D, repel_emitter_3d(FALSE), aesthetics = repel_aesthetics_3d,
  parameters = repel_parameters_3d(),
  position_check = function(position, scales) check_position_3d(position, scales))
register_geom_3d(GeomLabelRepel3D, repel_emitter_3d(TRUE), aesthetics = repel_aesthetics_3d,
  parameters = c(repel_parameters_3d(), list(label.padding = grid::unit(0.25, "lines"),
    label.r = grid::unit(0.15, "lines"), border.colour = NULL, text.colour = NULL)),
  position_check = function(position, scales) check_position_3d(position, scales))
