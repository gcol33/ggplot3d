distribution_cell_3d <- function(data) {
  if (length(unique(data$x)) != 1L || length(unique(data$y)) != 1L) {
    stop("Each distribution group needs one x/y cell; map group = interaction(x, y).", call. = FALSE)
  }
  data.frame(x = data$x[1L], y = data$y[1L])
}

StatBoxplot3D <- ggplot2::ggproto("StatBoxplot3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"), optional_aes = "weight",
  compute_group = function(data, scales, coef = 1.5, na.rm = FALSE) {
    cell <- distribution_cell_3d(data)
    values <- data; values$y <- data$z
    box <- ggplot2::StatBoxplot$compute_group(values, scales, coef = coef, na.rm = na.rm)
    out <- cell
    out$z <- box$middle; out$zmin <- box$lower; out$zmax <- box$upper
    out$zintercept <- box$ymin; out$zend <- box$ymax
    out$.kind <- "box"
    outside <- which(data$z < box$ymin | data$z > box$ymax)
    if (length(outside)) {
      points <- out[rep(1L, length(outside)), , drop = FALSE]
      for (name in z_aesthetics) points[[name]] <- data$z[outside]
      points$.kind <- "outlier"
      out <- rbind(out, points)
    }
    out
  }
)

GeomBoxplot3D <- ggplot2::ggproto("GeomBoxplot3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "zmin", "zmax", "zintercept", "zend"),
  default_aes = ggplot2::aes(colour = "grey20", fill = "white", alpha = NA,
    linewidth = 0.5, linetype = 1, shape = 19, size = 1.5, stroke = 0.5),
  draw_key = ggplot2::draw_key_boxplot,
  setup_data = function(data, params) setup_cell_bounds_3d(data, params)
)

emit_boxplots_3d <- function(data, params, panel_params) {
  boxes <- data[data$.kind == "box", , drop = FALSE]
  pieces <- list()
  if (nrow(boxes)) {
    crossbar <- boxes[setdiff(names(boxes), c("xmin", "xmax", "ymin", "ymax"))]
    crossbar$xend <- boxes$xmin; crossbar$xintercept <- boxes$xmax
    crossbar$yend <- boxes$ymin; crossbar$yintercept <- boxes$ymax
    settings <- utils::modifyList(params, list(cap.plane = "cross", fatten = params$fatten,
      middle_gp = NULL, box_gp = NULL))
    pieces[[1L]] <- emit_crossbar_3d(crossbar, settings, panel_params)
    whiskers <- rbind(segment_rows_3d(boxes, list(z = boxes$zintercept), list(z = boxes$zmin)),
      segment_rows_3d(boxes, list(z = boxes$zmax), list(z = boxes$zend)))
    caps <- rbind(segment_rows_3d(boxes, list(x = boxes$xmin, z = boxes$zintercept),
      list(x = boxes$xmax, z = boxes$zintercept)),
      segment_rows_3d(boxes, list(x = boxes$xmin, z = boxes$zend), list(x = boxes$xmax, z = boxes$zend)))
    pieces[[2L]] <- emit_segments_3d(rbind(whiskers, caps), params, panel_params)
    median <- boxes
    median$fill <- boxes$colour
    median$colour <- NA_character_
    pieces[[3L]] <- emit_rects_3d(median, utils::modifyList(params, list(shade = FALSE)), panel_params)
  }
  outliers <- data[data$.kind == "outlier", , drop = FALSE]
  if (nrow(outliers) && isTRUE(params$outliers)) pieces[[length(pieces) + 1L]] <- emit_points_3d(outliers, params, panel_params)
  merge_emitted_3d(pieces, panel_params)
}

register_geom_3d(GeomBoxplot3D, emit_boxplots_3d,
  parameters = list(width = NULL, depth = NULL, fatten = 2.5, outliers = TRUE),
  positions = c("nudge", "dodge"), tween = "row",
  extents = list(x = extent_3d("xmin", "xmax", "x"), y = extent_3d("ymin", "ymax", "y")))

#' Boxplots and violins standing on an x/y plane
#'
#' A distribution of z stands at each x/y cell. Map discrete x/y variables,
#' or set `group = interaction(x, y)` for numeric cells. Boxplots use
#' ggplot2's boxplot statistic on z, with a box prism, median face and perimeter,
#' whiskers and outliers. Violin walls mirror the z density along x;
#' revolved violins sweep the density around the vertical axis.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param width,depth Cell dimensions in transformed position units.
#' @param coef Whisker length in multiples of the interquartile range.
#' @param outliers Whether to draw outlier markers. All values train the z scale.
#' @param fatten Median perimeter linewidth multiplier.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = rep(c("a", "b"), each = 20), y = 0,
#'                 z = c(seq(-1, 1, length.out = 20), seq(0, 3, length.out = 20)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
#'   geom_boxplot_3d() + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
#'   geom_violin_3d(shape = "revolve", n = 24) + coord_3d()
#' @export
geom_boxplot_3d <- function(mapping = NULL, data = NULL, stat = StatBoxplot3D, position = "identity", ...,
                            width = NULL, depth = NULL, coef = 1.5, outliers = TRUE, fatten = 2.5,
                            shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  scene_assert(is.numeric(coef) && length(coef) == 1L && is.finite(coef) && coef >= 0, "coef must be nonnegative")
  make_layer_3d(GeomBoxplot3D, mapping, data, stat, position,
    c(list(width = width, depth = depth, coef = coef, outliers = check_flag_3d(outliers, "outliers"),
      fatten = fatten, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

StatViolin3D <- ggplot2::ggproto("StatViolin3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"), optional_aes = "weight",
  compute_group = function(data, scales, bw = "nrd0", adjust = 1, trim = TRUE, n = 80L, na.rm = FALSE) {
    cell <- distribution_cell_3d(data)
    values <- data; values$y <- data$z
    density <- ggplot2::StatYdensity$compute_group(values, scales,
      bw = bw, adjust = adjust, trim = trim, na.rm = na.rm, quantiles = NULL)
    if (!nrow(density)) return(data.frame())
    z <- seq(min(density$y), max(density$y), length.out = n)
    out <- cell[rep(1L, n), , drop = FALSE]
    out$z <- z
    out$density <- stats::approx(density$y, density$density, xout = z, ties = "ordered")$y
    out$scaled <- out$density / max(out$density)
    out$count <- out$density * nrow(data)
    out
  }
)

GeomViolin3D <- ggplot2::ggproto("GeomViolin3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "scaled"), default_aes = solid_defaults_3d(),
  draw_key = ggplot2::draw_key_polygon,
  setup_data = function(data, params) setup_cell_bounds_3d(data, params)
)

emit_violins_3d <- function(data, params, panel_params) {
  packets <- lapply(split(data, data$group), function(d) {
    d <- d[order(d$z), , drop = FALSE]
    rx <- (d$xmax - d$xmin) * d$scaled / 2
    if (params$shape == "wall") {
      left <- right <- d
      left$x <- d$x - rx; right$x <- d$x + rx
      return(emit_polygons_3d(rbind(left, right[nrow(d):1L, , drop = FALSE]), params, panel_params))
    }
    sides <- params$sides; n <- nrow(d)
    rows <- rep(seq_len(n), each = sides)
    theta <- rep(2 * pi * (seq_len(sides) - 1) / sides, n)
    mesh <- d[rows, , drop = FALSE]
    mesh$x <- mesh$x + rx[rows] * cos(theta)
    mesh$y <- mesh$y + (mesh$ymax - mesh$ymin) * mesh$scaled * sin(theta) / 2
    a <- sequence(rep(sides, n - 1L), from = (seq_len(n - 1L) - 1L) * sides + 1L)
    b <- (a - 1L) %/% sides * sides + a %% sides + 1L
    quads <- cbind(a, b, b + sides, a + sides)
    caps <- rbind(rev(seq_len(sides)), (n - 1L) * sides + seq_len(sides))
    edges <- if (isTRUE(params$wireframe)) rbind(cbind(a, b), cbind(a, a + sides)) else matrix(integer(), 0L, 2L)
    packet_from_faces_edges_3d(mesh, list(quads, caps), edges, params, panel_params)
  })
  merge_emitted_3d(packets, panel_params)
}

register_geom_3d(GeomViolin3D, emit_violins_3d,
  parameters = list(width = NULL, depth = NULL, shape = "wall", sides = 16L),
  positions = c("nudge", "dodge"),
  extents = list(x = extent_3d("xmin", "xmax", "x"), y = extent_3d("ymin", "ymax", "y")))

#' @rdname geom_boxplot_3d
#' @param shape Violin shape: `"wall"` or `"revolve"`.
#' @param bw,adjust,trim Density bandwidth, multiplier and trimming, as in [ggplot2::geom_violin()].
#' @param n Number of density samples.
#' @param sides Number of sides of a revolved violin.
#' @export
geom_violin_3d <- function(mapping = NULL, data = NULL, stat = StatViolin3D, position = "identity", ...,
                           shape = c("wall", "revolve"), width = NULL, depth = NULL,
                           bw = "nrd0", adjust = 1, trim = TRUE, n = 80L, sides = 16L,
                           shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  scene_assert(n >= 3 && n == as.integer(n) && sides >= 3 && sides == as.integer(sides), "n and sides must be integers of at least three")
  make_layer_3d(GeomViolin3D, mapping, data, stat, position,
    c(list(shape = match.arg(shape), width = width, depth = depth, bw = bw, adjust = adjust,
      trim = trim, n = as.integer(n), sides = as.integer(sides), shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

prepare_area_3d <- function(out, data, plot, params) {
  if (is.null(out$zmin)) out$zmin <- rep_len(params$base, nrow(out))
  if (is.null(out$zmax)) out$zmax <- out$z
  out$z <- out$zmin
  prepare_band_3d(out, data, plot, params)
}

GeomArea3D <- ggplot2::ggproto("GeomArea3D", GeomBand3D,
  required_aes = c("x", "y", "z", "zend"))

emit_areas_3d <- function(data, params, panel_params) {
  data <- data[order(match(data$group, unique(data$group)), data$x), , drop = FALSE]
  emit_bands_3d(data, params, panel_params)
}

register_geom_3d(GeomArea3D, emit_areas_3d,
  prepare_aesthetics = prepare_area_3d, parameters = list(base = 0),
  positions = c("nudge", "stack"), tween = "path",
  extents = list(x = extent_3d("x", "xend"), y = extent_3d("y", "yend"), z = extent_3d("z", "zend")))

#' Filled area walls and waterfall curtains
#'
#' Draws each group as a filled curtain from `base` to z, ordered by x.
#' The x coordinates shared by stacked groups form their lattice. Groups
#' with different x coordinates retain their own vertices.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param base Baseline in original z units, overridden by zmin.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = 0:8, y = 1:3)
#' d$z <- 1 + sin(d$x / 2 + d$y)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = y, fill = factor(y))) +
#'   geom_waterfall_3d() + coord_3d()
#' d <- expand.grid(x = 0:8, y = 0, part = c("a", "b"))
#' d$z <- 1 + sin(d$x / 2 + as.integer(d$part)) / 2
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = part, fill = part)) +
#'   geom_area_3d() + coord_3d()
#' @export
geom_area_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = position_stack_3d(), ...,
                         base = 0, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomArea3D, mapping, data, stat, position,
    c(list(base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_area_3d
#' @export
geom_waterfall_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                              base = 0, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_area_3d(mapping, data, stat, position, ..., base = base, shade = shade,
    na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}
