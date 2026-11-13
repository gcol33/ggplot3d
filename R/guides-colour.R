#' Colourbars on position axes and lit legend keys
#'
#' `guide_colourbar_3d()` draws a colourbar beside the selected box edge of a
#' position guide. The axis supplies its ticks, labels and title. The colour
#' aesthetic and that position must map the same expression in every layer
#' using the colour scale. Scale transformations may differ: colours are
#' evaluated at the position axis's original data values.
#'
#' `guide_legend_3d()` draws keys of shaded layers as a small facet lit by the
#' plot's light and first camera. `normal` specifies the facet's model-space
#' normal. `shading.range = TRUE` adds the light's ambient-to-diffuse colour
#' range below each facet. Unshaded layers retain their own key geometry.
#' @param along Position axis beside which the bar is drawn, one of `"x"`,
#'   `"y"`, `"z"`. A visible box position guide is required.
#' @param width,gap Physical grid units for the colourbar width and its gap
#'   from the edge. Tick lengths include this space.
#' @param nbin Number of colour intervals along the bar.
#' @param lit Whether shaded layers use lit facet keys.
#' @param normal Finite nonzero model-space facet normal, length three.
#' @param shading.range Whether to display the colour's lighting range.
#' @param ... Arguments passed to [ggplot2::guide_legend()].
#' @return A ggplot2 guide.
#' @references Karline Soetaert, plot3D, `colkey` colour keys.
#' @examples
#' d <- expand.grid(x = 1:4, y = 1:4)
#' d$z <- with(d, x + y)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z))
#' p + geom_surface_3d() +
#'   ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d()) + coord_3d()
#' p + geom_col_3d(ggplot2::aes(fill = factor(x))) +
#'   ggplot2::guides(fill = guide_legend_3d(shading.range = TRUE)) + coord_3d()
#' @export
guide_colourbar_3d <- function(along = "z", width = grid::unit(2, "mm"),
                                gap = grid::unit(0.5, "mm"), nbin = 128L) {
  along <- match.arg(along, c("x", "y", "z"))
  for (value in list(width, gap)) {
    if (!grid::is.unit(value) || length(value) != 1L ||
        !grid::unitType(value) %in% c("mm", "cm", "inches", "points", "bigpts", "picas") ||
        !is.finite(as.numeric(value)) || as.numeric(value) < 0) stop("width and gap must be nonnegative physical grid units.", call. = FALSE)
  }
  if (as.numeric(width) == 0) stop("width must be positive.", call. = FALSE)
  if (!is.numeric(nbin) || length(nbin) != 1L || !is.finite(nbin) || nbin < 2 || nbin != trunc(nbin)) stop("nbin must be an integer of at least two.", call. = FALSE)
  ggplot2::new_guide(along = along, width = width, gap = gap, nbin = as.integer(nbin),
    available_aes = c("fill", "colour"), super = GuideColourbar3D)
}

mapping_expression_3d <- function(mapping) {
  if (inherits(mapping, "quosure") || inherits(mapping, "formula")) unclass(mapping)[[2L]] else mapping
}

GuideColourbar3D <- ggplot2::ggproto("GuideColourbar3D", ggplot2::Guide,
  params = c(ggplot2::Guide$params, list(along = "z", width = grid::unit(2, "mm"), gap = grid::unit(0.5, "mm"), nbin = 128L)),
  train = function(...) NULL,
  setup_panel_3d = function(self, panel_params, scale, aesthetic, mappings) {
    along <- self$params$along
    if (scale$is_discrete() || panel_params[[along]]$is_discrete()) stop("An axis colourbar needs continuous colour and position scales.", call. = FALSE)
    relevant <- Filter(function(m) !is.null(m[[aesthetic]]), mappings)
    if (!length(relevant) || any(!vapply(relevant, function(m) {
      !is.null(m[[along]]) && identical(mapping_expression_3d(m[[aesthetic]]), mapping_expression_3d(m[[along]]))
    }, logical(1)))) stop("The colour aesthetic and ", along, " must map the same expression in every layer using the scale.", call. = FALSE)
    axis <- panel_params$axis_guides[[along]]
    if (is.null(axis) || !guide_on_box_3d(axis, panel_params)) stop("An axis colourbar needs a visible box guide for ", along, ".", call. = FALSE)
    ggplot2::ggproto(NULL, self, scale = scale)
  },
  axis_space_3d = function(self, axis) {
    if (axis != self$params$along) return(0)
    grid::convertUnit(self$params$width + self$params$gap, "mm", valueOnly = TRUE)
  },
  draw_panel_3d = function(self, furniture, theme) {
    params <- furniture$overlay$params; along <- self$params$along
    guide <- params$axis_guides[[along]]
    project <- furniture_projector_3d(furniture)
    frame <- furniture$overlay$frame
    layout <- axis_layout_3d(frame, guide, project, project(frame$bounds()))
    if (is.null(layout)) return(grid::nullGrob())
    limits <- params[[along]]$scale$get_limits()
    limits <- (limits - params$ranges[[along]][1L]) / diff(params$ranges[[along]])
    values <- seq(max(0, min(limits)), min(1, max(limits)), length.out = self$params$nbin + 1L)
    points <- layout$at(values)[, 1:2, drop = FALSE]
    normals <- rows_of_3d(layout$normals_at(values), length(values))
    gap <- grid::convertUnit(self$params$gap, "mm", valueOnly = TRUE)
    width <- grid::convertUnit(self$params$width, "mm", valueOnly = TRUE)
    inner <- points + normals * gap; outer <- inner + normals * width
    i <- seq_len(length(values) - 1L); j <- i + 1L
    x <- as.vector(t(cbind(inner[i, 1L], inner[j, 1L], outer[j, 1L], outer[i, 1L])))
    y <- as.vector(t(cbind(inner[i, 2L], inner[j, 2L], outer[j, 2L], outer[i, 2L])))
    middle <- (values[i] + values[j]) / 2
    raw <- params[[along]]$get_transformation()$inverse(params$ranges[[along]][1L] + middle * diff(params$ranges[[along]]))
    colours <- self$scale$map(self$scale$transform(raw))
    if (layout$straight) {
      delta <- points[nrow(points), ] - points[1L, ]
      stops <- drop(sweep((points[i, , drop = FALSE] + points[j, , drop = FALSE]) / 2, 2L, points[1L, ]) %*% delta) / sum(delta^2)
      low <- apply(rbind(inner, outer), 2L, min)
      span <- apply(rbind(inner, outer), 2L, max) - low
      ends <- sweep(sweep((inner[c(1L, nrow(inner)), ] + outer[c(1L, nrow(outer)), ]) / 2, 2L, low), 2L, span, "/")
      gradient <- grid::linearGradient(c(colours[1L], colours, tail(colours, 1L)), stops = c(0, stops, 1),
        x1 = ends[1L, 1L], y1 = ends[1L, 2L], x2 = ends[2L, 1L], y2 = ends[2L, 2L])
      shape <- grid::polygonGrob(c(inner[1L, 1L], tail(inner[, 1L], 1L), tail(outer[, 1L], 1L), outer[1L, 1L]),
        c(inner[1L, 2L], tail(inner[, 2L], 1L), tail(outer[, 2L], 1L), outer[1L, 2L]),
        default.units = "mm", gp = grid::gpar(fill = gradient, col = NA))
    } else shape <- grid::polygonGrob(x, y, id = rep(i, each = 4L), default.units = "mm", gp = grid::gpar(fill = colours, col = NA))
    grid::gTree(children = grid::gList(shape),
      values = raw, colours = colours, axis_positions = points, axis_index = layout$index, name = "axis-colourbar-3d")
  }
)

#' @rdname guide_colourbar_3d
#' @export
guide_legend_3d <- function(..., lit = TRUE, normal = c(0, 0, 1), shading.range = FALSE) {
  for (flag in list(lit, shading.range)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("lit and shading.range must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(normal) || length(normal) != 3L || any(!is.finite(normal)) || sum(normal^2) <= 0) stop("normal must be a finite nonzero vector of length three.", call. = FALSE)
  base <- ggplot2::guide_legend(...)
  ggplot2::ggproto(NULL, GuideLegend3D, params = c(base$params, list(lit = lit,
    normal = normal / sqrt(sum(normal^2)), shading.range = shading.range)))
}

lit_key_3d <- function(normal, context, range, original) {
  force(normal); force(context); force(range); force(original)
  if (is.null(context)) stop("Lit legend keys require coord_3d().", call. = FALSE)
  camera <- scene_compile(new_scene_packet(), context$camera, new_scene_viewport(100, 100))$camera
  light <- context$light
  function(data, params, size) {
    if (!isTRUE(params$shade) || is.na(data$fill %or3d% NA)) return(original(data, params, size))
    fragments <- list(normal_x = normal[1L], normal_y = normal[2L], normal_z = normal[3L])
    fill <- shade_fill_3d(data$fill, fragments, 1L, camera, light)
    children <- list(grid::polygonGrob(c(.1, .5, .9, .5), c(.45, .85, .55, .15),
      gp = grid::gpar(fill = fill, col = data$colour %or3d% NA,
        alpha = if (is.na(data$alpha %or3d% NA)) 1 else data$alpha,
        lwd = (data$linewidth %or3d% 0.3) * 72.27 / 25.4)))
    colours <- character()
    if (range && !is.null(light)) {
      colours <- apply_brightness_3d(rep(data$fill, 32L), seq(light$ambient, light$ambient + light$diffuse, length.out = 32L), light$mode)
      children[[2L]] <- grid::rectGrob((seq_len(32L) - 0.5) / 32, .04, width = 1 / 32, height = .08,
        gp = grid::gpar(fill = colours, col = NA, alpha = if (is.na(data$alpha %or3d% NA)) 1 else data$alpha))
    }
    grid::gTree(children = do.call(grid::gList, children), base_fill = data$fill, lit_fill = fill,
      shading_colours = colours, name = "lit-legend-key-3d")
  }
}

GuideLegend3D <- ggplot2::ggproto("GuideLegend3D", ggplot2::GuideLegend,
  params = c(ggplot2::GuideLegend$params, list(lit = TRUE, normal = c(0, 0, 1), shading.range = FALSE)),
  hashables = c(ggplot2::GuideLegend$hashables, alist(lit, normal, shading.range)),
  get_layer_key = function(self, params, layers, data, theme = NULL) {
    params <- ggplot2::ggproto_parent(ggplot2::GuideLegend, self)$get_layer_key(params, layers, data, theme)
    if (isTRUE(params$lit)) params$decor <- lapply(params$decor, function(decor) {
      decor$draw_key <- lit_key_3d(params$normal, decor$params$.key_context, params$shading.range, decor$draw_key)
      decor
    })
    params
  }
)
