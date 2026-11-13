#' Stereo output for 3D panels
#'
#' `stereo_3d()` configures [coord_3d()] to draw every panel for two eyes, as
#' a side-by-side stereo pair or as one anaglyph to view through coloured
#' glasses, in the manner of the stereo displays of tourr
#' (`display_stereo()`, `anaglyph()`) and ggthreed (`stat_anaglyph()`).
#'
#' The two eye cameras turn about the camera target by half of `separation`
#' each way, about the vertical axis of the image. For a level camera without
#' roll this is an azimuth offset of `separation / 2` per eye. Both eyes look
#' at the target, so the target lies in the page plane: geometry in front of
#' it appears in front of the page, geometry behind it behind the page. In an
#' orthographic view the two images differ only horizontally. Both eyes share
#' one magnification and one axis-label reserve, so the images have the same
#' scale.
#'
#' `"pair"` places the left-eye image on the left, for parallel (wall-eyed)
#' free viewing and stereoscopes; `"cross"` places it on the right, for
#' cross-eyed viewing. Each panel then holds the two images side by side and
#' is twice as wide as high.
#'
#' `"anaglyph"` draws both images into one panel. Each image is reduced to its
#' luminance and seen through its filter colour: the panel is the product of
#' the left image tinted toward the complement of the left filter and the
#' right image tinted toward the complement of the right filter, so with red
#' and cyan filters the red channel holds the left image and the green and
#' blue channels the right image. Paint seen identically by both eyes, such as
#' the panel background, keeps its grey level. Colour and fill scales then
#' carry only luminance; [coord_3d()] warns when the plot has one. With the
#' vector and hybrid backends the two images are drawn as monochrome passes
#' composited with a multiply blend (grid group compositing, R 4.2 or later)
#' when the graphics device supports it, such as the ragg, Cairo, svg and pdf
#' devices. Otherwise, and always with the raster backend, both images are
#' rendered offscreen at the backend's `dpi` with ragg and composited per
#' colour channel into one image; on a white background this matches the
#' blended result.
#'
#' [view_3d()] shows the plot from the central camera, without stereo.
#' @param mode `"pair"`, `"cross"` or `"anaglyph"`.
#' @param separation Angle between the two eye directions in degrees, greater
#'   than 0 and less than 90.
#' @param colours Two filter colours, the left eye's first, as in the colour
#'   glasses used to view an anaglyph.
#' @return A stereo configuration for the `stereo` argument of [coord_3d()].
#' @seealso [coord_3d()], [facet_views()], whose views each become a stereo
#'   pair.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 25), y = seq(-1, 1, length.out = 25))
#' d$z <- exp(-3 * (d$x^2 + d$y^2))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey85", colour = "grey30", linewidth = 0.2)
#' p + coord_3d(stereo = stereo_3d("pair"))
#' p + coord_3d(stereo = stereo_3d("cross", separation = 5))
#' p + coord_3d(stereo = stereo_3d("anaglyph", colours = c("red", "cyan")))
#' @export
stereo_3d <- function(mode = c("pair", "cross", "anaglyph"), separation = 3, colours = c("red", "cyan")) {
  mode <- match.arg(mode)
  if (!(is.numeric(separation) && length(separation) == 1L && is.finite(separation) && separation > 0 && separation < 90))
    stop("separation must be one angle in degrees between 0 and 90.", call. = FALSE)
  if (length(colours) != 2L || anyNA(colours))
    stop("colours must be two filter colours, the left eye's first.", call. = FALSE)
  filters <- tryCatch(grDevices::col2rgb(colours) / 255,
    error = function(e) stop("colours must be two valid colours.", call. = FALSE))
  colnames(filters) <- c("left", "right")
  structure(list(mode = mode, separation = as.double(separation), colours = colours, filters = filters),
    class = "ggplot3d_stereo")
}

#' @export
print.ggplot3d_stereo <- function(x, ...) {
  cat(sprintf("<stereo_3d> %s, separation %g degrees", x$mode, x$separation))
  if (x$mode == "anaglyph") cat(sprintf(", filters %s (left) and %s (right)", x$colours[1L], x$colours[2L]))
  cat("\n")
  invisible(x)
}

stereo_side_by_side_3d <- function(stereo) !is.null(stereo) && stereo$mode %in% c("pair", "cross")

# Signed eye offsets in degrees; a positive offset moves the eye to the
# camera's right.
stereo_offsets_3d <- function(stereo) c(left = -stereo$separation / 2, right = stereo$separation / 2)

# Camera turned about its target by `offset` degrees about the image's
# vertical axis: the eye moves toward the image's right for a positive
# offset, the image's up direction stays, and the azimuth, elevation and roll
# that give the turned view are recovered from its forward and right vectors.
stereo_camera_3d <- function(camera, offset) {
  theta <- offset * pi / 180
  az <- camera$azimuth * pi / 180; el <- camera$elevation * pi / 180; roll <- camera$roll * pi / 180
  level_right <- function(az) c(-sin(az), cos(az), 0)
  level_up <- function(az, el) c(-sin(el) * cos(az), -sin(el) * sin(az), cos(el))
  forward <- -c(cos(el) * cos(az), cos(el) * sin(az), sin(el))
  right <- cos(roll) * level_right(az) - sin(roll) * level_up(az, el)
  turned_forward <- cos(theta) * forward - sin(theta) * right
  turned_right <- cos(theta) * right + sin(theta) * forward
  el2 <- asin(max(-1, min(1, -turned_forward[3L])))
  az2 <- if (cos(el2) > 1e-12) atan2(-turned_forward[2L], -turned_forward[1L]) else az
  roll2 <- atan2(-sum(turned_right * level_up(az2, el2)), sum(turned_right * level_right(az2)))
  wrap <- function(angle) (angle + pi) %% (2 * pi) - pi
  camera$azimuth <- camera$azimuth + wrap(az2 - az) * 180 / pi
  camera$elevation <- el2 * 180 / pi
  camera$roll <- camera$roll + wrap(roll2 - roll) * 180 / pi
  camera
}

# A panel grob of one eye: the panel's own grob with the eye camera, and the
# frame group of all panels seen by both eyes, so every eye image shares one
# magnification.
stereo_eye_grobs_3d <- function(x, stereo) {
  offsets <- stereo_offsets_3d(stereo)
  group <- as.list(x$frame_group)
  if (!length(group)) group <- list(list(params = x$panel_params, theme = x$theme))
  eye_group <- unlist(lapply(names(offsets), function(eye) {
    entries <- lapply(group, function(entry) {
      entry$params$camera <- stereo_camera_3d(entry$params$camera, offsets[[eye]])
      entry
    })
    stats::setNames(entries, paste(names(group), eye, sep = "-"))
  }), recursive = FALSE)
  eyes <- lapply(names(offsets), function(eye) {
    g <- x
    g$eye <- eye
    g$eye_group <- eye_group
    g$panel_params$camera <- stereo_camera_3d(x$panel_params$camera, offsets[[eye]])
    g$name <- paste0("scene-eye-", eye, "-3d")
    g
  })
  stats::setNames(eyes, names(offsets))
}

stereo_panel_children_3d <- function(x, stereo) {
  eyes <- stereo_eye_grobs_3d(x, stereo)
  side <- function(g, left) {
    g$vp <- grid::viewport(x = if (left) 0 else 0.5, width = 0.5, just = c("left", "centre"),
      name = paste0(g$name, "-vp"))
    g
  }
  switch(stereo$mode,
    pair = list(side(eyes$left, TRUE), side(eyes$right, FALSE)),
    cross = list(side(eyes$right, TRUE), side(eyes$left, FALSE)),
    anaglyph = list(anaglyph_grob_3d(eyes, stereo, x$panel_params$coord_params$backend)))
}

anaglyph_blend_available_3d <- function() {
  getRversion() >= "4.2.0" && "multiply" %in% grDevices::dev.capabilities()$compositing
}

anaglyph_grob_3d <- function(eyes, stereo, backend) {
  if (!identical(backend$type, "raster") && anaglyph_blend_available_3d()) {
    eyes$left$ink <- stereo$filters[, "left"]
    eyes$right$ink <- stereo$filters[, "right"]
    # The right image is isolated in its own group, so it multiplies the left
    # image once as a whole instead of shape by shape.
    group <- getExportedValue("grid", "groupGrob")
    right <- group(eyes$right, coords = FALSE, name = "scene-anaglyph-right-3d")
    return(group(right, op = "multiply", dst = eyes$left, coords = FALSE, name = "scene-anaglyph-3d"))
  }
  anaglyph_raster_3d(eyes, stereo$filters, backend$dpi)
}

# Per-channel composite of the two eye images rendered offscreen at `dpi` and
# the size of the current viewport. Each eye's paint, over white, is reduced
# to its grey level and seen through its filter; the product of both is the
# composite over white, returned with the union of both coverages.
anaglyph_raster_3d <- function(eyes, filters, dpi) {
  motion_require_3d("ragg")
  size <- c(grid::convertWidth(grid::unit(1, "npc"), "in", TRUE),
            grid::convertHeight(grid::unit(1, "npc"), "in", TRUE))
  pixels <- pmax(1L, as.integer(ceiling(size * dpi)))
  images <- lapply(eyes, capture_grob_rgba_3d, pixels = pixels, dpi = dpi)
  product <- 1; transparency <- 1
  for (eye in c("left", "right")) {
    rgba <- images[[eye]]
    over_white <- 1 - rgba[4L, ] * (1 - grey_level_3d(rgba[1:3, , drop = FALSE]))
    product <- product * (1 - outer(filters[, eye], 1 - over_white))
    transparency <- transparency * (1 - rgba[4L, ])
  }
  alpha <- 1 - transparency
  colour <- 1 - sweep(1 - product, 2L, pmax(alpha, .Machine$double.eps), "/")
  colour[] <- pmin(1, pmax(0, colour))
  raster <- matrix(grDevices::rgb(colour[1L, ], colour[2L, ], colour[3L, ], alpha),
    nrow = pixels[2L], ncol = pixels[1L])
  grid::rasterGrob(raster, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
    interpolate = FALSE, name = "scene-anaglyph-3d")
}

# RGBA (4 x pixels, in [0, 1], column-major over the image rows) of a grob
# drawn alone on a transparent offscreen device.
capture_grob_rgba_3d <- function(grob, pixels, dpi) {
  current <- grDevices::dev.cur()
  capture <- ragg::agg_capture(width = pixels[1L], height = pixels[2L], units = "px", res = dpi,
    background = "transparent")
  device <- grDevices::dev.cur()
  on.exit({
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device)
    if (current %in% grDevices::dev.list()) grDevices::dev.set(current)
  }, add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  image <- capture(native = FALSE)
  grDevices::col2rgb(as.vector(image), alpha = TRUE) / 255
}

# Colours seen through a filter: the grey level of each colour tinted from
# white toward the filter's complement, alpha unchanged.
ink_colours_3d <- function(colour, filter) {
  if (!length(colour) || !(is.character(colour) || is.factor(colour))) return(colour)
  out <- if (is.factor(colour)) as.character(colour) else colour
  keep <- !is.na(out) & !(out %in% "transparent")
  if (!any(keep)) return(out)
  rgba <- colour_channels_3d(out[keep], alpha = TRUE) / 255
  ink <- 1 - outer(filter, 1 - grey_level_3d(rgba[1:3, , drop = FALSE]))
  out[keep] <- grDevices::rgb(ink[1L, ], ink[2L, ], ink[3L, ], rgba[4L, ])
  out
}

# nativeRaster pixels (packed non-premultiplied RGBA) seen through a filter.
ink_native_raster_3d <- function(raster, filter) {
  values <- as.double(raster)
  values[values < 0] <- values[values < 0] + 2^32
  channel <- function(k) (values %/% 256^k) %% 256
  rgba <- rbind(channel(0), channel(1), channel(2)) / 255
  ink <- round(255 * (1 - outer(filter, 1 - grey_level_3d(rgba))))
  packed <- ink[1L, ] + 256 * ink[2L, ] + 65536 * ink[3L, ] + 16777216 * channel(3)
  packed[packed >= 2^31] <- packed[packed >= 2^31] - 2^32
  raster[] <- as.integer(packed)
  raster
}

ink_gpar_3d <- function(gp, filter) {
  for (field in intersect(c("col", "fill"), names(gp))) {
    value <- gp[[field]]
    if (inherits(value, "GridPattern")) {
      if (!is.null(value$colours)) value$colours <- ink_colours_3d(value$colours, filter)
      gp[[field]] <- value
    } else if (is.list(value)) {
      gp[[field]] <- lapply(value, function(v) if (inherits(v, "GridPattern") && !is.null(v$colours)) {
        v$colours <- ink_colours_3d(v$colours, filter); v
      } else ink_colours_3d(v, filter))
    } else gp[[field]] <- ink_colours_3d(value, filter)
  }
  gp
}

# A grob and its children with every stroke, fill, text and raster colour
# seen through a filter.
ink_grob_3d <- function(grob, filter) {
  if (!is.null(grob$gp)) grob$gp <- ink_gpar_3d(grob$gp, filter)
  if (inherits(grob, "rastergrob")) {
    grob$raster <- if (inherits(grob$raster, "nativeRaster")) ink_native_raster_3d(grob$raster, filter)
      else ink_colours_3d(grob$raster, filter)
  }
  if (inherits(grob, "gTree") && length(grob$children)) {
    children <- lapply(as.list(grob$children)[grob$childrenOrder], ink_grob_3d, filter = filter)
    grob <- grid::setChildren(grob, do.call(grid::gList, children))
  }
  grob
}

stereo_colour_warning_3d <- function(coord, scales) {
  stereo <- coord$config$stereo
  if (is.null(stereo) || stereo$mode != "anaglyph" || is.null(scales)) return(invisible())
  aesthetics <- unlist(lapply(scales$non_position_scales()$scales, `[[`, "aesthetics"))
  if (any(aesthetics %in% c("colour", "fill")))
    warning("Anaglyph stereo shows colour and fill scales by luminance only.", call. = FALSE)
  invisible()
}

# The plot drawn from the central camera, without stereo.
mono_plot_3d <- function(plot) {
  coord <- plot$coordinates
  if (is.null(coord$config$stereo)) return(plot)
  config <- coord$config
  config["stereo"] <- list(NULL)
  plot$coordinates <- ggplot2::ggproto(NULL, coord, config = config)
  plot
}
