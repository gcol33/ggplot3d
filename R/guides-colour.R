#' Colour guides that show the scene's shading
#'
#' `guide_colourbar_3d()` with `along` an axis draws a colourbar beside the
#' selected box edge of that position guide. The axis supplies its ticks,
#' labels and title. The colour aesthetic and that position must map the same
#' expression in every layer using the colour scale. Scale transformations may
#' differ: colours are evaluated at the position axis's original data values.
#' With `along = NULL` it is an ordinary legend-box colourbar
#' ([ggplot2::guide_colourbar()]).
#'
#' `guide_coloursteps_3d()` and `guide_bins_3d()` are
#' [ggplot2::guide_coloursteps()] and [ggplot2::guide_bins()] with shading;
#' `guide_legend_3d()` and `guide_bins_3d()` draw keys of shaded layers as a
#' small facet lit by the plot's light and first camera. `normal` specifies the
#' facet's model-space normal. Unshaded layers retain their own key geometry.
#'
#' `shading.range` shows what the light does to each colour. The colourbars
#' and colour steps become a two-dimensional swatch, value along the bar and
#' light across it; legend and bin keys get a strip below the facet. The
#' colours come from the same shading kernel as the faces, for a surface whose
#' normal makes each angle along the range with every light, without the
#' specular term, using the material of the first layer with one. `TRUE`
#' spans 90 to 0 degrees for the `"multiply"` and `"tint"` modes of
#' [lights_3d()] (from ambient to full light) and 180 to 0 degrees for the
#' signed modes; two angles in degrees, `c(from, to)`, set the span, and
#' reversing them reverses the strip. With several facet panels, guides use
#' the plot's light.
#' @param along Position axis beside which the bar is drawn, one of `"x"`,
#'   `"y"`, `"z"`, or NULL for a legend-box colourbar.
#' @param width,gap Physical grid units for the axis colourbar width and its
#'   gap from the edge. Tick lengths include this space.
#' @param nbin Number of colour intervals along the bar.
#' @param lit Whether shaded layers use lit facet keys.
#' @param normal Finite nonzero model-space facet normal, length three.
#' @param shading.range `FALSE`, `TRUE`, or two angles in degrees in \[0,
#'   180\], the span of light shown.
#' @param ... Arguments passed to [ggplot2::guide_colourbar()],
#'   [ggplot2::guide_coloursteps()], [ggplot2::guide_bins()] or
#'   [ggplot2::guide_legend()].
#' @return A ggplot2 guide.
#' @references Karline Soetaert, plot3D, `colkey` colour keys.
#' @examples
#' d <- expand.grid(x = 1:4, y = 1:4)
#' d$z <- with(d, x + y)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z))
#' p + geom_surface_3d() +
#'   ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d()) + coord_3d()
#' p + geom_surface_3d() +
#'   ggplot2::scale_fill_distiller(palette = "Spectral",
#'     guide = guide_colourbar_3d(along = NULL, shading.range = TRUE)) +
#'   coord_3d(light = light_3d(mode = "lightness"))
#' p + geom_surface_3d() +
#'   ggplot2::scale_fill_viridis_b(guide = guide_coloursteps_3d(shading.range = TRUE)) + coord_3d()
#' p + geom_col_3d(ggplot2::aes(fill = factor(x))) +
#'   ggplot2::guides(fill = guide_legend_3d(shading.range = TRUE)) + coord_3d()
#' @export
guide_colourbar_3d <- function(along = "z", width = grid::unit(2, "mm"),
                                gap = grid::unit(0.5, "mm"), nbin = 128L, shading.range = FALSE, ...) {
  shading.range <- shading_range_spec_3d(shading.range)
  if (is.null(along)) {
    base <- ggplot2::guide_colourbar(nbin = nbin, ...)
    return(ggplot2::ggproto(NULL, GuideColourbarShaded3D,
      params = c(base$params, list(shading.range = shading.range, .key_context = NULL, .material = NULL))))
  }
  along <- match.arg(along, c("x", "y", "z"))
  for (value in list(width, gap)) {
    if (!grid::is.unit(value) || length(value) != 1L ||
        !grid::unitType(value) %in% c("mm", "cm", "inches", "points", "bigpts", "picas") ||
        !is.finite(as.numeric(value)) || as.numeric(value) < 0) stop("width and gap must be nonnegative physical grid units.", call. = FALSE)
  }
  if (as.numeric(width) == 0) stop("width must be positive.", call. = FALSE)
  if (!is.numeric(nbin) || length(nbin) != 1L || !is.finite(nbin) || nbin < 2 || nbin != trunc(nbin)) stop("nbin must be an integer of at least two.", call. = FALSE)
  ggplot2::new_guide(along = along, width = width, gap = gap, nbin = as.integer(nbin), shading.range = shading.range,
    available_aes = c("fill", "colour"), super = GuideColourbar3D)
}

# A shading.range argument as NULL (none) or the angles in degrees of the span
# (NA for the default of the light's mode).
shading_range_spec_3d <- function(range) {
  if (isFALSE(range)) return(NULL)
  if (isTRUE(range)) return(c(NA_real_, NA_real_))
  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range)) || any(range < 0 | range > 180) || range[1L] == range[2L])
    stop("shading.range must be TRUE, FALSE or two distinct angles in [0, 180] degrees.", call. = FALSE)
  as.double(range)
}

# Light values (cosines) of n steps over a shading range for a light.
shading_values_3d <- function(range, light, n) {
  if (anyNA(range)) range <- if (signed_transfer_3d(light$mode)) c(180, 0) else c(90, 0)
  cospi(seq(range[1L], range[2L], length.out = n) / 180)
}

# The camera of legend keys: the camera of a key context, resolved.
key_camera_3d <- function(camera) scene_compile(new_scene_packet(), camera, new_scene_viewport(100, 100))$camera

# Colours lit as a surface meeting every light at light value x, without the
# specular term: the swatch sampler of the shading kernel. One row of the
# result per colour, one column per light value.
shade_swatch_3d <- function(colour, x, light, camera, material = NULL) {
  n <- length(colour); m <- length(x)
  if (!n || !m) return(matrix(character(), n, m))
  base <- linear_rgb_3d(rep(colour, m))
  lit <- shade_swatch_cpp(base, rep(as.double(x), each = n), rep(if (is.null(material)) 0L else 1L, n * m),
    kernel_materials_3d(material_table_3d(material)), resolved_light_3d(light, camera))
  out <- encode_rgb_3d(lit)
  alpha <- grDevices::col2rgb(colour, alpha = TRUE)[4L, ]
  translucent <- rep(alpha < 255, m)
  if (any(translucent)) out[translucent] <- paste0(substr(out[translucent], 1L, 7L), sprintf("%02X", rep(alpha, m)[translucent]))
  out[rep(is.na(colour), m)] <- NA_character_
  matrix(out, n, m)
}

# The key context and material of the first adapted layer of a guide.
guide_key_context_3d <- function(layers) {
  for (layer in layers) {
    params <- layer$computed_geom_params %or3d% layer$geom_params
    if (!is.null(params$.key_context)) return(list(context = params$.key_context, material = params$material))
  }
  list(context = NULL, material = NULL)
}

# Steps across a shaded bar.
shading_steps_3d <- 24L

GuideColourbarShaded3D <- ggplot2::ggproto("GuideColourbarShaded3D", ggplot2::GuideColourbar,
  params = c(ggplot2::GuideColourbar$params, list(shading.range = NULL, .key_context = NULL, .material = NULL)),
  get_layer_key = function(self, params, layers, data = NULL, theme = NULL) {
    found <- guide_key_context_3d(layers)
    params$.key_context <- found$context
    params$.material <- found$material
    params
  },
  build_decor = function(self, decor, grobs, elements, params) {
    light <- params$.key_context$light
    if (is.null(params$shading.range) || is.null(light)) return(ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$build_decor(decor, grobs, elements, params))
    x <- shading_values_3d(params$shading.range, light, shading_steps_3d)
    colours <- shade_swatch_3d(decor$colour, x, light, key_camera_3d(params$.key_context$camera), params$.material)
    image <- if (params$direction == "horizontal") t(colours[, rev(seq_along(x)), drop = FALSE]) else
      colours[rev(seq_len(nrow(colours))), , drop = FALSE]
    bar <- grid::rasterGrob(image = image, width = 1, height = 1, default.units = "npc", interpolate = TRUE,
      gp = grid::gpar(col = NA), name = "shaded-colourbar-3d")
    bar$shading_colours <- colours
    list(bar = bar, frame = ggplot2::element_grob(elements$frame, fill = NA), ticks = grobs$ticks)
  }
)

#' @rdname guide_colourbar_3d
#' @export
guide_coloursteps_3d <- function(..., shading.range = TRUE) {
  base <- ggplot2::guide_coloursteps(...)
  ggplot2::ggproto(NULL, GuideColoursteps3D,
    params = c(base$params, list(shading.range = shading_range_spec_3d(shading.range), .key_context = NULL, .material = NULL)))
}

GuideColoursteps3D <- ggplot2::ggproto("GuideColoursteps3D", ggplot2::GuideColoursteps,
  params = c(ggplot2::GuideColoursteps$params, list(shading.range = NULL, .key_context = NULL, .material = NULL)),
  get_layer_key = GuideColourbarShaded3D$get_layer_key,
  build_decor = function(self, decor, grobs, elements, params) {
    light <- params$.key_context$light
    if (is.null(params$shading.range) || is.null(light)) return(ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$build_decor(decor, grobs, elements, params))
    x <- shading_values_3d(params$shading.range, light, shading_steps_3d)
    colours <- shade_swatch_3d(decor$colour, x, light, key_camera_3d(params$.key_context$camera), params$.material)
    m <- length(x); k <- rep(seq_len(m), each = nrow(decor))
    size <- abs(decor$max - decor$min); just <- as.numeric(decor$min > decor$max)
    across <- (k - 1) / m
    bar <- if (params$direction == "vertical") {
      grid::rectGrob(x = across, y = rep(decor$min, m), width = 1 / m, height = rep(size, m), vjust = rep(just, m), hjust = 0,
        gp = grid::gpar(col = NA, fill = as.vector(colours)), name = "shaded-coloursteps-3d")
    } else {
      grid::rectGrob(x = rep(decor$min, m), y = across, height = 1 / m, width = rep(size, m), hjust = rep(just, m), vjust = 0,
        gp = grid::gpar(col = NA, fill = as.vector(colours)), name = "shaded-coloursteps-3d")
    }
    bar$shading_colours <- colours
    list(bar = bar, frame = ggplot2::element_grob(elements$frame, fill = NA), ticks = grobs$ticks)
  }
)

mapping_expression_3d <- function(mapping) {
  if (inherits(mapping, "quosure") || inherits(mapping, "formula")) unclass(mapping)[[2L]] else mapping
}

GuideColourbar3D <- ggplot2::ggproto("GuideColourbar3D", ggplot2::Guide,
  params = c(ggplot2::Guide$params, list(along = "z", width = grid::unit(2, "mm"), gap = grid::unit(0.5, "mm"), nbin = 128L,
    shading.range = NULL)),
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
    light <- params$coord_params$light
    if (!is.null(self$params$shading.range) && !is.null(light)) {
      light <- panel_lights_3d(light, params)
      x_light <- shading_values_3d(self$params$shading.range, light, shading_steps_3d)
      shaded <- shade_swatch_3d(colours, x_light, light, key_camera_3d(params$camera), NULL)
      m <- length(x_light); cells <- expand.grid(i = i, k = seq_len(m))
      a <- (cells$k - 1) / m; b <- cells$k / m
      inner_i <- inner[cells$i, , drop = FALSE] + normals[cells$i, , drop = FALSE] * width * a
      inner_j <- inner[cells$i + 1L, , drop = FALSE] + normals[cells$i + 1L, , drop = FALSE] * width * a
      outer_j <- inner[cells$i + 1L, , drop = FALSE] + normals[cells$i + 1L, , drop = FALSE] * width * b
      outer_i <- inner[cells$i, , drop = FALSE] + normals[cells$i, , drop = FALSE] * width * b
      x <- as.vector(t(cbind(inner_i[, 1L], inner_j[, 1L], outer_j[, 1L], outer_i[, 1L])))
      y <- as.vector(t(cbind(inner_i[, 2L], inner_j[, 2L], outer_j[, 2L], outer_i[, 2L])))
      shape <- grid::polygonGrob(x, y, id = rep(seq_len(nrow(cells)), each = 4L), default.units = "mm",
        gp = grid::gpar(fill = shaded[cbind(cells$i, cells$k)], col = shaded[cbind(cells$i, cells$k)], lwd = 0.25))
      return(grid::gTree(children = grid::gList(shape), values = raw, colours = colours, shading_colours = shaded,
        axis_positions = points, axis_index = layout$index, name = "axis-colourbar-3d"))
    }
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
  lit_guide_3d(ggplot2::guide_legend(...), GuideLegend3D, lit, normal, shading.range)
}

#' @rdname guide_colourbar_3d
#' @export
guide_bins_3d <- function(..., lit = TRUE, normal = c(0, 0, 1), shading.range = FALSE) {
  lit_guide_3d(ggplot2::guide_bins(...), GuideBins3D, lit, normal, shading.range)
}

lit_guide_3d <- function(base, super, lit, normal, shading.range) {
  if (!is.logical(lit) || length(lit) != 1L || is.na(lit)) stop("lit must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(normal) || length(normal) != 3L || any(!is.finite(normal)) || sum(normal^2) <= 0) stop("normal must be a finite nonzero vector of length three.", call. = FALSE)
  ggplot2::ggproto(NULL, super, params = c(base$params, list(lit = lit,
    normal = normal / sqrt(sum(normal^2)), shading.range = shading_range_spec_3d(shading.range))))
}

lit_key_3d <- function(normal, context, range, original) {
  force(normal); force(context); force(range); force(original)
  if (is.null(context)) stop("Lit legend keys require coord_3d().", call. = FALSE)
  camera <- key_camera_3d(context$camera)
  light <- context$light
  function(data, params, size) {
    if (!isTRUE(params$shade) || is.na(data$fill %or3d% NA)) return(original(data, params, size))
    fragments <- list(normal_x = normal[1L], normal_y = normal[2L], normal_z = normal[3L])
    material <- params$material
    styles <- if (!is.null(material)) data.frame(material_id = 1L)
    fill <- shade_fill_3d(data$fill, fragments, 1L, camera, light, styles, materials = material_table_3d(material))
    alpha <- if (is.na(data$alpha %or3d% NA)) 1 else data$alpha
    children <- list(grid::polygonGrob(c(.1, .5, .9, .5), c(.45, .85, .55, .15),
      gp = grid::gpar(fill = fill, col = data$colour %or3d% NA, alpha = alpha,
        lwd = (data$linewidth %or3d% 0.3) * 72.27 / 25.4)))
    colours <- character()
    if (!is.null(range) && !is.null(light)) {
      colours <- drop(shade_swatch_3d(data$fill, shading_values_3d(range, light, 32L), light, camera, material))
      children[[2L]] <- grid::rectGrob((seq_len(32L) - 0.5) / 32, .04, width = 1 / 32, height = .08,
        gp = grid::gpar(fill = colours, col = NA, alpha = alpha))
    }
    grid::gTree(children = do.call(grid::gList, children), base_fill = data$fill, lit_fill = fill,
      shading_colours = colours, name = "lit-legend-key-3d")
  }
}

lit_layer_key_3d <- function(parent) {
  force(parent)
  function(self, params, layers, data, theme = NULL) {
    params <- ggplot2::ggproto_parent(parent, self)$get_layer_key(params, layers, data, theme)
    if (isTRUE(params$lit)) params$decor <- lapply(params$decor, function(decor) {
      decor$draw_key <- lit_key_3d(params$normal, decor$params$.key_context, params$shading.range, decor$draw_key)
      decor
    })
    params
  }
}

GuideLegend3D <- ggplot2::ggproto("GuideLegend3D", ggplot2::GuideLegend,
  params = c(ggplot2::GuideLegend$params, list(lit = TRUE, normal = c(0, 0, 1), shading.range = NULL)),
  hashables = c(ggplot2::GuideLegend$hashables, alist(lit, normal, shading.range)),
  get_layer_key = lit_layer_key_3d(ggplot2::GuideLegend)
)

GuideBins3D <- ggplot2::ggproto("GuideBins3D", ggplot2::GuideBins,
  params = c(ggplot2::GuideBins$params, list(lit = TRUE, normal = c(0, 0, 1), shading.range = NULL)),
  hashables = c(ggplot2::GuideBins$hashables %or3d% ggplot2::GuideLegend$hashables, alist(lit, normal, shading.range)),
  get_layer_key = lit_layer_key_3d(ggplot2::GuideBins)
)
