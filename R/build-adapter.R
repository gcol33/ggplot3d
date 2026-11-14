#' @importFrom ggplot2 ggplot_build update_ggplot
#' @export
ggplot_build.ggplot3d <- function(plot, ...) {
  parent_build <- utils::getS3method("ggplot_build", "ggplot2::ggplot", envir = asNamespace("ggplot2"))
  if (!inherits(plot@coordinates, "Coord3D")) return(parent_build(plot, ...))
  context <- new.env(parent = emptyenv())
  context$labels <- plot@labels
  context$mapping <- plot@mapping
  context$layer_mappings <- lapply(plot@layers, function(layer) {
    mapping <- if (isTRUE(layer$inherit.aes)) plot@mapping else ggplot2::aes()
    for (name in names(layer$mapping)) mapping[[name]] <- layer$mapping[[name]]
    for (name in names(layer$aes_params)) mapping[[name]] <- NULL
    mapping
  })
  frame <- plot@coordinates$frame %or3d% FrameBox
  for (layer in plot@layers) {
    for (axis in c("x", "y", "z", frame$parts)) {
      if (is.null(context$mapping[[axis]]) && !is.null(layer$mapping[[axis]])) context$mapping[[axis]] <- layer$mapping[[axis]]
    }
  }
  context$theme <- ggplot2::complete_theme(plot@theme)
  working <- plot
  working@coordinates <- ggplot2::ggproto(NULL, plot@coordinates)
  context$key_context <- new.env(parent = emptyenv())
  context$key_context$camera <- plot@coordinates$camera
  context$key_context$light <- plot@coordinates$config$light
  working@layers <- lapply(seq_along(plot@layers), function(i) adapt_layer_3d(plot@layers[[i]], i, frame,
    context$key_context, isTRUE(plot@coordinates$config$tween)))
  working@scales <- scales_3d(plot@scales, context)
  working@layout <- ggplot2::ggproto(NULL, Layout3D, context = context)
  built <- parent_build(working, ...)
  stereo_colour_warning_3d(plot@coordinates, context$scales)
  built
}

check_3d_build <- function(valid) {
  if (!isTRUE(valid)) stop("coord_3d() needs its 3D build pipeline. For gganimate transitions, use a regular 3D plot with animate_3d(time = , tween = tween_3d()).", call. = FALSE)
  invisible(NULL)
}

scales_3d <- function(parent, context) {
  ggplot2::ggproto("Scales3D", parent,
    context = context,
    clone = function(self) {
      result <- ggplot2::ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
      self$context$scales <- result
      result
    },
    add_defaults = function(self, data, env) {
      if (!self$has_scale("z")) self$add(infer_z_scale(data))
      ggplot2::ggproto_parent(parent, self)$add_defaults(data[setdiff(names(data), z_aesthetics)], env)
    },
    non_position_scales = function(self) {
      keep <- !vapply(self$scales, function(s) any(s$aesthetics %in% c("x", "y", z_aesthetics)), logical(1))
      ggplot2::ggproto(NULL, self, scales = self$scales[keep])
    },
    transform_df = function(self, df) {
      position <- unique(unlist(lapply(Filter(function(s) any(s$aesthetics %in% c("x", "y", "z")), self$scales), `[[`, "aesthetics")))
      columns <- intersect(position, names(df))
      # Columns holding only box-face sentinels (+/-Inf) stay out of the scale
      # transformation, which date and time scales reject for numbers.
      boundary <- columns[vapply(df[columns], function(v) is.numeric(v) && any(is.infinite(v)) && all(is.infinite(v) | is.na(v)), logical(1))]
      held <- df[boundary]
      df <- df[setdiff(names(df), boundary)]
      columns <- setdiff(columns, boundary)
      original <- df[columns]
      for (column in columns) {
        if (is.numeric(df[[column]])) df[[column]][is.infinite(df[[column]])] <- NA
      }
      result <- ggplot2::ggproto_parent(parent, self)$transform_df(df)
      for (column in columns) {
        value <- original[[column]]
        if (!is.numeric(value)) next
        sentinel <- is.infinite(value)
        invalid <- is.finite(value) & !is.finite(result[[column]]) & !is.na(result[[column]])
        if (any(invalid)) warning("Position transformation produced non-finite values in ", column, ".", call. = FALSE)
        result[[column]][invalid] <- NA_real_
        result[[column]][sentinel] <- value[sentinel]
      }
      result[names(held)] <- held
      result
    }
  )
}

adapt_layer_3d <- function(layer, layer_id, frame = FrameBox, key_context = NULL, tweening = FALSE) {
  entry <- find_emitter_3d(layer$geom)
  if (is.null(entry)) stop("No 3D emitter registered for ", class(layer$geom)[1L], ".", call. = FALSE)
  check_position_kind_3d(layer$position, entry)
  parent <- layer
  fixed <- layer$aes_params[intersect(names(layer$aes_params), c("x", "xmin", "xmax", "xend", "y", "ymin", "ymax", "yend", z_aesthetics))]
  hook_params <- c(layer$aes_params, layer$geom_params, layer$stat_params)
  base_stat <- entry$stat_adapter %or3d% layer$stat
  # The row index is internal; a stat that aggregates rows drops it silently.
  stat <- ggplot2::ggproto(NULL, base_stat, dropped_aes = union(base_stat$dropped_aes, c(".source", ".tween_group", interactive_aesthetics_3d, frame$parts)))
  result <- ggplot2::ggproto(NULL, layer,
    geom = scene_geom_3d(layer$geom, entry),
    position = bind_position_3d(layer$position, entry),
    stat = stat,
    compute_statistic = function(self, data, layout) {
      out <- ggplot2::ggproto_parent(parent, self)$compute_statistic(data, layout)
      # Contours lie at their numeric level; filled bands lie at their midpoint.
      level <- out$level_mid %or3d% out$level
      if (!"z" %in% names(out) && is.numeric(level)) out$z <- level
      out
    },
    aes_params = layer$aes_params[setdiff(names(layer$aes_params), names(fixed))],
    layer_data = function(self, plot_data) {
      data <- ggplot2::ggproto_parent(parent, self)$layer_data(plot_data)
      if (!is.null(entry$prepare_data)) data <- entry$prepare_data(data)
      if (any(c(".source", ".layer") %in% names(data))) stop("Input uses reserved .source or .layer columns.", call. = FALSE)
      data$.source <- seq_len(nrow(data))
      data
    },
    compute_aesthetics = function(self, data, plot) {
      # Tooltips and selection keys label rows: they neither form groups nor
      # train scales.
      mapping <- self$computed_mapping
      interactive <- intersect(names(mapping), interactive_aesthetics_3d)
      if (length(interactive)) {
        self$computed_mapping <- mapping[setdiff(names(mapping), interactive)]
        on.exit(self$computed_mapping <- mapping, add = TRUE)
      }
      out <- ggplot2::ggproto_parent(parent, self)$compute_aesthetics(data, plot)
      if (!is.null(entry$prepare_aesthetics)) out <- entry$prepare_aesthetics(out, data, plot, hook_params)
      if (is.function(plot@facet$positions_3d)) out <- plot@facet$positions_3d(out, data)
      for (name in interactive) out[[name]] <- rep_len(rlang::eval_tidy(mapping[[name]], data), nrow(out))
      for (name in names(fixed)) out[[name]] <- rep_len(fixed[[name]], nrow(out))
      out <- frame$setup_aesthetics(out)
      if (!"z" %in% names(out) && !"z" %in% names(self$computed_mapping) &&
          !"z" %in% names(self$stat$default_aes) && !"z" %in% self$geom$required_aes) out$z <- rep(entry$default_z %or3d% 0, nrow(out))
      out$.source <- if (nrow(out) == nrow(data)) data$.source else seq_len(nrow(out))
      out$.layer <- rep.int(as.integer(layer_id), nrow(out))
      if (tweening && is.null(out$.tween_group)) {
        discrete <- names(out)[vapply(out, function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))]
        out$.tween_group <- tween_keys_3d(out, setdiff(discrete, c("label", "PANEL")))
      }
      plot@scales$add_defaults(out[intersect(names(out), c("x", "y", z_aesthetics))], plot@plot_env)
      out
    },
    compute_position = function(self, data, layout) {
      for (panel in unique(data$PANEL)) {
        scales <- layout$get_scales(panel)
        check_position_scales_3d(self$position, scales)
        if (!is.null(entry$position_check)) entry$position_check(self$position, scales)
      }
      ggplot2::ggproto_parent(parent, self)$compute_position(data, layout)
    }
  )
  result$geom_params$.emitter <- entry$name
  result$geom_params$.layer_id <- as.integer(layer_id)
  result$geom_params$.key_context <- key_context
  if (inherits(layer$position, "PositionProject3D")) result$geom_params$.project <- list(onto = layer$position$onto, axes = layer$position$axes)
  result
}

S7::method(update_ggplot, list(S7::new_S3_class("Coord3D"), ggplot2::class_ggplot)) <- function(object, plot, ...) {
  if (!isTRUE(plot@coordinates$default)) message("Coordinate system already present. Adding coord_3d() replaces it.")
  plot@coordinates <- object
  class(plot) <- union("ggplot3d", class(plot))
  plot
}
