Layout3D <- ggplot2::ggproto("Layout3D", ggplot2::Layout,
  panel_scales_z = NULL,
  position_pass = 0L,
  provisional_z = FALSE,
  train_position = function(self, data, x_scale, y_scale) {
    ggplot2::ggproto_parent(ggplot2::Layout, self)$train_position(data, x_scale, y_scale)
    scales <- self$context$scales
    self$position_pass <- self$position_pass + 1L
    if (!scales$has_scale("z") && self$position_pass >= 2L) scales$add(scale_z_continuous())
    if (is.null(self$panel_scales_z) || (self$provisional_z && scales$has_scale("z"))) {
      prototype <- scales$get_scales("z") %or3d% scale_z_continuous()
      self$panel_scales_z <- lapply(seq_len(max(self$layout$SCALE_Z)), function(i) prototype$clone())
      self$provisional_z <- !scales$has_scale("z")
    }
    for (d in data) {
      ids <- self$layout$SCALE_Z[match(d$PANEL, self$layout$PANEL)]
      for (i in unique(ids)) {
        scale <- self$panel_scales_z[[i]]
        for (column in intersect(scale$aesthetics, names(d))) {
          values <- d[[column]][which(ids == i)]
          if (is.numeric(values)) values <- values[is.finite(values)]
          if (length(values)) scale$train(values)
        }
      }
    }
  },
  map_position = function(self, data) {
    data <- ggplot2::ggproto_parent(ggplot2::Layout, self)$map_position(data)
    lapply(data, function(d) {
      ids <- self$layout$SCALE_Z[match(d$PANEL, self$layout$PANEL)]
      for (column in intersect(z_aesthetics, names(d))) {
        pieces <- lapply(seq_along(self$panel_scales_z), function(i) self$panel_scales_z[[i]]$map(d[[column]][which(ids == i)]))
        if (!length(pieces)) next
        value <- pieces[[1]][rep(NA_integer_, nrow(d))]
        for (i in seq_along(pieces)) value[which(ids == i)] <- pieces[[i]]
        d[[column]] <- value
      }
      d
    })
  },
  reset_scales = function(self) {
    ggplot2::ggproto_parent(ggplot2::Layout, self)$reset_scales()
    if (self$facet$shrink) lapply(self$panel_scales_z, function(s) s$reset())
    invisible()
  },
  get_scales = function(self, i) {
    out <- ggplot2::ggproto_parent(ggplot2::Layout, self)$get_scales(i)
    row <- match(i, self$layout$PANEL)
    out$z <- self$panel_scales_z[[self$layout$SCALE_Z[row]]]
    out
  },
  setup_panel_params = function(self) {
    coord <- self$coord
    panels <- lapply(seq_len(nrow(self$layout)), function(i) {
      row <- self$layout[i, ]
      scales <- self$get_scales(row$PANEL)
      out <- coord$setup_panel_params(scales$x, scales$y, self$coord_params)
      out <- c(out, coord$axis_view(scales$z, "z"))
      out$panel_id <- as.integer(as.character(row$PANEL))
      out$scale_ids <- c(x = row$SCALE_X, y = row$SCALE_Y, z = row$SCALE_Z)
      out$ranges <- list(x = out$x.range, y = out$y.range, z = out$z.range)
      view <- if (is.null(row$VIEW)) NULL else self$facet_params$views[[row$VIEW]]
      out$camera <- if (is.null(view)) coord$panel_camera(i, nrow(self$layout)) else coord$view_camera(view)
      if (identical(coord$model_aspect, "data")) out$camera$aspect <- as.double(vapply(out$ranges, diff, numeric(1)))
      out$coord_params <- coord$config
      out$frame <- bind_frame_3d(coord$frame %or3d% FrameBox, out)
      out$axis_titles <- lapply(c("x", "y", "z"), function(axis) {
        name <- scales[[axis]]$name
        if (!inherits(name, "waiver")) return(name)
        # labs(z = NULL) stores an explicit NULL, which removes the title.
        if (axis %in% names(self$context$labels)) return(self$context$labels[[axis]])
        mapping <- self$context$mapping[[axis]]
        if (!is.null(mapping)) return(axis_mapping_label_3d(mapping))
        self$facet$params$axis_titles_3d[[axis]]
      })
      names(out$axis_titles) <- c("x", "y", "z")
      parts <- (coord$frame %or3d% FrameBox)$parts
      out$part_titles <- stats::setNames(lapply(parts, function(part) {
        label <- self$context$labels[[part]]
        if (!is.null(label)) return(label)
        mapping <- self$context$mapping[[part]]
        if (!is.null(mapping)) axis_mapping_label_3d(mapping) else part
      }), parts)
      out
    })
    self$panel_params <- self$facet$setup_panel_params(panels, coord)
    if (is.environment(self$context$key_context)) self$context$key_context$camera <- self$panel_params[[1L]]$camera
    invisible()
  },
  setup_panel_guides = function(self, guides, layers) {
    self$panel_params <- lapply(self$panel_params, function(p) {
      if (exists("setup_guides_3d", mode = "function")) return(setup_guides_3d(p, guides, self$context$theme,
        self$context$scales, self$context$layer_mappings))
      p$axis_guides <- lapply(p[c("x", "y", "z")], function(s) list(breaks = s$get_breaks(), labels = s$get_labels()))
      p
    })
    invisible()
  }
)

axis_mapping_label_3d <- function(mapping) {
  expression <- if (inherits(mapping, "quosure")) unclass(mapping)[[2L]] else mapping
  if (is.symbol(expression)) return(as.character(expression))
  if (is.character(expression) && length(expression) == 1L) return(expression)
  deparse1(expression)
}

axis_view_3d <- function(scale, limits, expand, axis) {
  helper <- ggplot2::coord_cartesian(xlim = limits, expand = expand)
  helper_params <- helper$setup_params(list())
  view <- helper$setup_panel_params(scale, ggplot2::scale_y_continuous(), helper_params)
  fields <- c(axis, paste0(axis, ".sec"), paste0(axis, ".range"))
  range <- view[[paste0(axis, ".range")]]
  if (length(range) == 2L && all(is.finite(range)) && diff(range) == 0) {
    provisional <- scale$get_transformation()$inverse(range[1L] + c(-0.5, 0.5))
    helper <- ggplot2::coord_cartesian(xlim = provisional, expand = expand)
    view <- helper$setup_panel_params(scale, ggplot2::scale_y_continuous(), helper$setup_params(list()))
  }
  view[fields]
}
