utils::globalVariables("hatch_lines_cpp")

#' Hatching of surfaces and meshes
#'
#' `hatch_3d()` draws a surface or mesh layer's tone as hatching, passed as
#' the layer's `hatch` argument. With `space = "model"` the strokes are the
#' intersections of the triangles with parallel planes: `angle` turns the
#' planes' normal in the panel's x-y plane from the x axis, so `angle = 0`
#' gives lines of constant x (the iso-parameter lines of a grid surface) and
#' `angle = 90` lines of constant y; `cross = TRUE` adds the perpendicular
#' family. The strokes are lines of the scene, hidden by nearer surfaces like
#' any other line, so in vector output they are exact; with a white fill, or
#' with `backend_3d("plotter")`, the figure is drawn with lines only.
#'
#' With `tone = TRUE` the density follows the shading: every face is lit (as
#' a white face of the layer's material) and its darkness, one minus the
#' encoded grey, selects how many lines it keeps, from every fourth line in
#' light tones to every line, then the cross family, in dark tones. The lines
#' kept at one tone contain those kept at every lighter tone, the nesting of
#' the tonal art maps of Praun et al. (2001), so strokes run on unbroken
#' where the tone changes. Without a light every line of the first family is
#' drawn.
#'
#' With `space = "screen"` the faces get a hatch pattern in device space
#' instead (a [grid::pattern()] fill per tone band, as in Murrell's vector
#' pattern fills): lines `spacing` mm apart, twice as far in light faces,
#' crossed in dark ones. It needs a device with pattern fills (ragg, svg,
#' cairo_pdf, pdf) and is drawn by the vector backend.
#' @param angle Angle in degrees of the first family.
#' @param spacing Distance between lines: a fraction of the panel box side
#'   for `space = "model"`, mm for `"screen"`. NULL takes 0.015 and 1.2.
#' @param cross Whether the dark tones add the perpendicular family.
#' @param tone Whether line density follows the shading.
#' @param space `"model"` or `"screen"`.
#' @param colour,linewidth,alpha Stroke of the lines.
#' @return A hatching specification for the `hatch` argument of
#'   [geom_surface_3d()], [geom_mesh_3d()] and the surface stats.
#' @references Praun, Hoppe, Webb, Finkelstein (2001). Real-time hatching.
#'   SIGGRAPH 2001, 581-586. \doi{10.1145/383259.383328}
#'
#'   Hertzmann and Zorin (2000). Illustrating smooth surfaces. SIGGRAPH 2000,
#'   517-526. \doi{10.1145/344779.345074}
#' @examples
#' d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
#' d$z <- with(d, exp(-(x^2 + y^2)))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_surface_3d(fill = "white", colour = NA, hatch = hatch_3d(angle = 0)) + coord_3d()
#' p + geom_surface_3d(fill = "white", colour = NA, hatch = hatch_3d(space = "screen")) + coord_3d()
#' @export
hatch_3d <- function(angle = 45, spacing = NULL, cross = TRUE, tone = TRUE, space = c("model", "screen"),
                     colour = "black", linewidth = 0.25, alpha = 1) {
  space <- match.arg(space)
  spacing <- spacing %or3d% if (space == "model") 0.015 else 1.2
  angle <- mesh_number_3d(angle, "angle", is.finite, "finite")
  spacing <- mesh_number_3d(spacing, "spacing", function(x) is.finite(x) && x > 0, "positive")
  for (flag in list(cross, tone)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("cross and tone must be TRUE or FALSE.", call. = FALSE)
  linewidth <- mesh_number_3d(linewidth, "linewidth", function(x) is.finite(x) && x >= 0, "nonnegative")
  alpha <- mesh_number_3d(alpha, "alpha", function(x) is.finite(x) && x >= 0 && x <= 1, "between 0 and 1")
  structure(list(angle = angle, spacing = spacing, cross = cross, tone = tone, space = space,
    colour = check_light_colour_3d(colour, "colour"), linewidth = linewidth, alpha = alpha), class = "ggplot3d_hatch")
}

hatch_argument_3d <- function(hatch) {
  if (!is.null(hatch) && !inherits(hatch, "ggplot3d_hatch")) stop("hatch must be created by hatch_3d().", call. = FALSE)
  hatch
}

# Darkness thresholds of the hatch ranks: the first family keeps lines of
# rank below the number of thresholds a face's darkness exceeds, the cross
# family likewise with its own thresholds.
hatch_thresholds_3d <- list(c(0.08, 0.3, 0.5), c(0.62, 0.74, 0.86))

# Darkness of faces lit as white faces of a material: one minus the encoded
# grey of their luminance. normal and position are model-space rows.
hatch_darkness_3d <- function(normal, position, camera, light, material) {
  white <- matrix(1, nrow(normal), 3L)
  lit <- shade_linear_3d(white, normal, camera, light, position, rep(if (is.null(material)) 0L else 1L, nrow(normal)),
    material_table_3d(material))
  1 - srgb_encode_3d(pmin(1, pmax(0, drop(lit %*% c(0.2126, 0.7152, 0.0722)))))
}

# Ranks drawn per face for each family from the darkness of the faces.
hatch_levels_3d <- function(darkness, cross) {
  first <- rowSums(outer(darkness, hatch_thresholds_3d[[1L]], ">"))
  if (!cross) return(matrix(as.integer(first), ncol = 1L))
  cbind(first, rowSums(outer(darkness, hatch_thresholds_3d[[2L]], ">")))
}

# Adds the model-space hatching strokes of params$hatch to a packet of
# triangles, as outline_merge_3d() does for feature lines.
hatch_merge_3d <- function(packet, data, faces, params, panel_params) {
  spec <- hatch_argument_3d(params$hatch)
  if (is.null(spec) || spec$space != "model" || !nrow(faces)) return(packet)
  key <- if (!is.null(data$.vertex)) paste(data$.mesh %or3d% 1L, data$group %or3d% 1L, data$.vertex)
    else paste(data$group %or3d% 1L, data$.source)
  ids <- match(key, unique(key)); rows <- match(seq_len(max(ids)), ids)
  xyz <- matrix(as.double(as.matrix(data[rows, c("x", "y", "z")])), ncol = 3L)
  frame <- frame_of_3d(panel_params)
  model <- if (isTRUE(frame$identity)) xyz else frame$map(xyz)
  triangles <- matrix(ids[faces], ncol = 3L)
  angle <- spec$angle * pi / 180
  directions <- rbind(c(cos(angle), sin(angle), 0), if (spec$cross) c(-sin(angle), cos(angle), 0))
  light <- panel_lights_3d(panel_params$coord_params$light, panel_params)
  if (spec$tone && !is.null(light)) {
    camera <- key_camera_3d(panel_params$camera %or3d% new_camera_3d())
    scaled <- sweep(model, 2L, camera$model_scale, "*")
    a <- scaled[triangles[, 1L], , drop = FALSE]; b <- scaled[triangles[, 2L], , drop = FALSE]; c <- scaled[triangles[, 3L], , drop = FALSE]
    normal <- cross_rows_3d(b - a, c - a)
    length <- sqrt(rowSums(normal^2)); normal <- normal / ifelse(length > 0, length, 1)
    levels <- hatch_levels_3d(hatch_darkness_3d(normal, (a + b + c) / 3, camera, light, params$material), spec$cross)
  } else levels <- cbind(rep(3L, nrow(triangles)), if (spec$cross) rep(if (spec$tone) 0L else 3L, nrow(triangles)))
  lines <- hatch_lines_cpp(model, triangles - 1L, directions, spec$spacing, matrix(as.integer(levels), nrow(triangles)))
  n <- length(lines$x)
  if (!n) return(packet)
  source <- faces[lines$face + 1L, 1L]
  xyz_out <- cbind(lines$x, lines$y, lines$z)
  if (!isTRUE(frame$identity)) xyz_out <- frame$inverse(xyz_out)
  strokes <- data.frame(x = xyz_out[, 1L], y = xyz_out[, 2L], z = xyz_out[, 3L], group = lines$line,
    colour = spec$colour, linewidth = spec$linewidth, linetype = 1, alpha = spec$alpha,
    .source = data$.source[source], .layer = data$.layer[source])
  counts <- tabulate(lines$line)
  style <- utils::modifyList(params, list(hidden = "hide", lineend = "butt", linejoin = "round", shade = FALSE, arrow = NULL, hatch = NULL))
  hatching <- packet_from_primitives_3d(strokes, seq_len(n), rep("polylines", length(counts)), style,
    panel_params$panel_id, counts)
  merge_scene_packets(list(packet, hatching), panel_params$panel_id)
}

# Derived strokes of a packet of triangles: hatching and feature lines.
surface_strokes_3d <- function(packet, data, faces, params, panel_params) {
  outline_merge_3d(hatch_merge_3d(packet, data, faces, params, panel_params), data, faces, params, panel_params)
}

# Screen-space hatch bands of faces from their darkness: 0 none, 1 lines
# 2 * spacing apart, 2 lines spacing apart, 3 crossed at 2 * spacing,
# 4 crossed at spacing.
hatch_bands_3d <- function(darkness, cross) {
  band <- rowSums(outer(darkness, c(0.08, 0.3, 0.55, 0.75), ">"))
  if (!cross) band <- pmin(band, 2L)
  band
}

# A device-space pattern of parallel lines `spacing` mm apart at `angle`
# degrees.
hatch_pattern_3d <- function(angle, spacing, colour, linewidth, alpha) {
  a <- (angle %% 180) * pi / 180
  tile <- function(a) {
    s <- abs(sin(a)); c <- abs(cos(a))
    if (s < 1e-6) return(list(w = spacing, h = spacing, x = c(0, 1), y = c(0.5, 0.5)))
    if (c < 1e-6) return(list(w = spacing, h = spacing, x = c(0.5, 0.5), y = c(0, 1)))
    list(w = spacing / s, h = spacing / c, x = c(0, 1), y = if (sin(a) * cos(a) > 0) c(0, 1) else c(1, 0))
  }
  t <- tile(a)
  w <- t$w; h <- t$h
  gp <- grid::gpar(col = colour, lwd = linewidth * 72.27 / 25.4, alpha = alpha, lineend = "square")
  grob <- grid::polylineGrob(t$x * w, t$y * h, default.units = "mm", gp = gp)
  grid::pattern(grob, x = 0, y = 0,
    width = grid::unit(w, "mm"), height = grid::unit(h, "mm"), just = c("left", "bottom"), extend = "repeat")
}

# Screen-space hatching of the face fragments `rows` (vertex positions x, y
# in npc, face id per vertex): one pattern-filled polygon grob per tone band,
# or NULL when no face is hatched.
hatch_face_grob_3d <- function(scene, rows, styles, light, x, y, id) {
  angle <- styles$hatch_angle[rows]
  if (is.null(angle) || all(is.na(angle))) return(NULL)
  s <- styles[rows[which(!is.na(angle))[1L]], , drop = FALSE]
  darkness <- rep(0.4, length(rows))
  if (isTRUE(s$hatch_tone) && !is.null(light)) {
    white <- styles; white$fill <- "#FFFFFF"
    darkness <- 1 - grey_level_3d(grDevices::col2rgb(face_fill_3d(scene, rows, white, light)) / 255)
  }
  band <- hatch_bands_3d(darkness, isTRUE(s$hatch_cross))
  if (!isTRUE(s$hatch_tone)) band[] <- if (isTRUE(s$hatch_cross)) 4L else 2L
  band[is.na(angle)] <- 0L
  grobs <- unlist(lapply(sort(unique(band[band > 0L])), function(b) {
    take <- band[id] == b
    lapply(s$hatch_angle + if (b >= 3L) c(0, 90) else 0, function(angle) {
      pattern <- hatch_pattern_3d(angle, s$hatch_spacing * if (b %in% c(1L, 3L)) 2 else 1,
        s$hatch_colour, s$hatch_linewidth, s$hatch_alpha)
      g <- grid::polygonGrob(x[take], y[take], id = match(id[take], unique(id[take])), gp = grid::gpar(fill = pattern, col = NA))
      g$hatch_band <- b
      g
    })
  }), recursive = FALSE)
  if (!length(grobs)) return(NULL)
  grid::gTree(children = do.call(grid::gList, grobs), name = "hatch-3d", bands = band)
}
