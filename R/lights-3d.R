light_kinds_3d <- c(directional = 1L, point = 2L, head = 3L, hemisphere = 4L)

check_light_colour_3d <- function(colour, name) {
  if (length(colour) != 1L || is.na(colour) || inherits(try(grDevices::col2rgb(colour), silent = TRUE), "try-error"))
    stop(name, " must be one colour.", call. = FALSE)
  colour
}

check_light_number_3d <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) stop(name, " must be a nonnegative number.", call. = FALSE)
  as.double(x)
}

check_light_vector_3d <- function(x, name, nonzero = TRUE) {
  if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x)) || (nonzero && sum(x^2) == 0))
    stop(name, " must be three finite numbers", if (nonzero) ", not all zero", ".", call. = FALSE)
  as.double(x)
}

new_light_3d <- function(kind, vector = c(0, 0, 1), space = "camera", colour = "white", intensity = 1,
                         falloff = 0, ground = "black") {
  structure(list(kind = kind, vector = vector, space = space, colour = check_light_colour_3d(colour, "colour"),
    intensity = check_light_number_3d(intensity, "intensity"), falloff = falloff,
    ground = check_light_colour_3d(ground, "ground")), class = "ggplot3d_light")
}

#' Lights for a 3D scene
#'
#' Lights combine in [lights_3d()], which [coord_3d()] takes as `light`, with
#' an ambient term. Each face colour is lit by one shading kernel shared by
#' the vector and raster backends and the [view_3d()] viewer: the
#' Blinn-Phong model (Phong 1975, Blinn 1977) in linear light, with the
#' ambient, diffuse and specular strengths of the face's [material_3d()].
#' Lights follow the light objects of MATLAB (`light`, `camlight headlight`),
#' rgl (`light3d()`), plotly (`lightposition`) and three.js
#' (`DirectionalLight`, `PointLight`, `HemisphereLight`).
#'
#' `light_directional()` shines along a fixed direction from infinitely far
#' away. `light_point()` sits at a position and its contribution falls off as
#' `intensity / d^falloff`, with `d` the distance in panel units (the box
#' side is one, stretched by the camera aspect) and at least 0.001;
#' `falloff = 2` is the inverse-square law, `falloff = 0` a constant. The
#' falloff never depends on the other geometry in the panel.
#' `light_head()` is a directional light that shines from the eye, like
#' MATLAB's `camlight headlight`. `light_hemisphere()` adds an ambient term
#' that blends from `ground` to `sky` as the normal turns from `-direction` to
#' `direction` (three.js `HemisphereLight`).
#'
#' In `space = "camera"`, x points right, y up and z toward the viewer, and
#' positions are relative to the camera target in panel units. In
#' `space = "data"`, directions are in the normalised panel axes and
#' positions in data units.
#' @param direction Direction from a surface toward the light, or the sky
#'   direction of a hemisphere light.
#' @param position Position of a point light.
#' @param space `"camera"` or `"data"`.
#' @param colour,sky,ground Light colours.
#' @param intensity Nonnegative multiplier of the light colour.
#' @param falloff Nonnegative exponent of the distance falloff.
#' @return A light for [lights_3d()].
#' @references Phong (1975). Illumination for computer generated pictures.
#'   Communications of the ACM 18, 311-317. \doi{10.1145/360825.360839}
#'
#'   Blinn (1977). Models of light reflection for computer synthesized
#'   pictures. SIGGRAPH '77, 192-198. \doi{10.1145/563858.563893}
#' @examples
#' lights_3d(light_directional(c(1, 1, 2)), light_point(c(0, 0, 3), falloff = 2))
#' @export
light_directional <- function(direction = c(-1, 1, 2), space = "camera", colour = "white", intensity = 0.65) {
  direction <- check_light_vector_3d(direction, "direction")
  new_light_3d(light_kinds_3d[["directional"]], direction / sqrt(sum(direction^2)),
    match.arg(space, c("camera", "data")), colour, intensity)
}

#' @rdname light_directional
#' @export
light_point <- function(position, space = "data", colour = "white", intensity = 1, falloff = 2) {
  new_light_3d(light_kinds_3d[["point"]], check_light_vector_3d(position, "position", nonzero = FALSE),
    match.arg(space, c("camera", "data")), colour, intensity, check_light_number_3d(falloff, "falloff"))
}

#' @rdname light_directional
#' @export
light_head <- function(colour = "white", intensity = 0.65) {
  new_light_3d(light_kinds_3d[["head"]], colour = colour, intensity = intensity)
}

#' @rdname light_directional
#' @export
light_hemisphere <- function(sky = "white", ground = "grey30", direction = c(0, 0, 1), space = "data", intensity = 0.35) {
  direction <- check_light_vector_3d(direction, "direction")
  new_light_3d(light_kinds_3d[["hemisphere"]], direction / sqrt(sum(direction^2)),
    match.arg(space, c("camera", "data")), sky, intensity, ground = ground)
}

#' Combine lights for a 3D scene
#'
#' `mode` sets how the light changes a face colour. `"multiply"` scales the
#' colour by the ambient and diffuse light and `"tint"` scales its distance
#' from white; both darken every channel, so a shaded fill no longer matches
#' its legend key. The signed modes keep the colour readable against its key
#' (Szafir et al. 2016; Kovesi 2015). Each light gives a level in \[-1, 1\]
#' from the angle between the normal and the light: 0 at `neutral` degrees,
#' 1 facing the light and -1 facing away, linear in the cosine on each side
#' (ggcube's `ramp_smooth()`). The levels are averaged over the lights,
#' weighted by their luminance, and scaled by the material's diffuse
#' strength. A level of 0 leaves the colour unchanged.
#'
#' - `"lightness"` moves the OKLab lightness (Ottosson 2020) toward black by
#'   up to the fraction `abs(range[1])` in shadow and toward white by up to
#'   `range[2]` in light, and keeps the hue; where the moved colour leaves the
#'   sRGB gamut, its chroma is reduced until it fits (ggcube's
#'   `tone_lightness()`).
#' - `"tone"` moves the colour in OKLab toward `shadow` or `highlight`, up to
#'   the fractions `abs(range)`.
#' - `"overlay"` and `"soft"` are matplotlib's `LightSource` blend modes on the
#'   encoded colour, with an intensity of `0.5 + level * abs(range) / 2`.
#'
#' Specular highlights of [material_3d()] are added on top in every mode.
#' Lightness-monotone palettes such as viridis lose the most under shading;
#' isoluminant or diverging palettes combine best with relief shading.
#' [guide_colourbar_3d()], [guide_coloursteps_3d()], [guide_bins_3d()] and
#' [guide_legend_3d()] show the shaded colours of a key.
#' @param ... Lights made with [light_directional()], [light_point()],
#'   [light_head()] or [light_hemisphere()].
#' @param ambient Nonnegative strength of the uniform ambient light.
#' @param mode Colour operation: `"multiply"`, `"tint"`, `"lightness"`,
#'   `"tone"`, `"overlay"` or `"soft"`.
#' @param range Moves at full shadow and full highlight, `c(shadow, highlight)`
#'   with `-1 <= shadow <= 0 <= highlight <= 1`: fractions of the way to black
#'   and white in OKLab lightness for `"lightness"`, to the tone colours for
#'   `"tone"`, blend strengths for `"overlay"` and `"soft"`. NULL takes
#'   `c(-0.45, 0.3)` for `"lightness"` and `c(-0.6, 0.6)` otherwise.
#' @param neutral Angle in degrees, in \[0, 180), between the normal and the
#'   light at which a signed mode leaves the colour unchanged.
#' @param shadow,highlight Tone colours of `"tone"`.
#' @return A lighting configuration for [coord_3d()]. The backends that take
#'   one directional light (the path tracer and the raymarch backend) use the
#'   first directional light.
#' @references Szafir, Sarikaya, Gleicher (2016). Lightness constancy in
#'   surface visualization. IEEE TVCG 22, 2107-2121.
#'   \doi{10.1109/TVCG.2015.2500240}
#'
#'   Kovesi (2015). Good colour maps: how to design them.
#'   \url{https://arxiv.org/abs/1509.03700}
#'
#'   Ottosson (2020). A perceptual color space for image processing.
#'   \url{https://bottosson.github.io/posts/oklab/}
#' @examples
#' lights_3d(light_head(), light_hemisphere("lightblue", "brown"), ambient = 0.1)
#' d <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
#' d$z <- with(d, exp(-(x^2 + y^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
#'   geom_surface_3d(shading = "smooth", colour = NA) +
#'   ggplot2::scale_fill_distiller(palette = "Spectral") +
#'   coord_3d(light = lights_3d(light_directional(), mode = "lightness"))
#' @export
lights_3d <- function(..., ambient = 0.35, mode = "multiply", range = NULL, neutral = 45,
                      shadow = "#1d2b53", highlight = "#fff1d6") {
  lights <- list(...)
  if (!all(vapply(lights, inherits, logical(1), "ggplot3d_light"))) stop("lights_3d() takes lights made with light_*().", call. = FALSE)
  ambient <- check_light_number_3d(ambient, "ambient")
  mode <- match.arg(mode, light_transfers_3d)
  range <- range %or3d% if (mode == "lightness") c(-0.45, 0.3) else c(-0.6, 0.6)
  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range)) || range[1L] > 0 || range[2L] < 0 || any(abs(range) > 1))
    stop("range must be c(shadow, highlight) with -1 <= shadow <= 0 <= highlight <= 1.", call. = FALSE)
  if (!is.numeric(neutral) || length(neutral) != 1L || !is.finite(neutral) || neutral < 0 || neutral >= 180)
    stop("neutral must be one angle in [0, 180) degrees.", call. = FALSE)
  primary <- Filter(function(l) l$kind == light_kinds_3d[["directional"]], lights)
  primary <- if (length(primary)) primary[[1L]] else NULL
  structure(list(lights = unname(lights), ambient = ambient, mode = mode, range = as.double(range),
    neutral = as.double(neutral), shadow = check_light_colour_3d(shadow, "shadow"),
    highlight = check_light_colour_3d(highlight, "highlight"),
    direction = if (is.null(primary)) c(0, 0, 1) else primary$vector,
    space = if (is.null(primary)) "camera" else primary$space,
    diffuse = if (is.null(primary)) 0 else primary$intensity), class = "ggplot3d_lights")
}

# Colour transfers of lights_3d(), in the order of the kernel's transfer ids
# (src/shade/shade.h), starting at 0.
light_transfers_3d <- c("multiply", "tint", "lightness", "tone", "overlay", "soft")

# Whether a transfer moves colours by a signed level instead of scaling them.
signed_transfer_3d <- function(mode) !mode %in% c("multiply", "tint")

# A lighting argument as a lights configuration: NULL, one light, or lights.
as_lights_3d <- function(light) {
  if (is.null(light) || inherits(light, "ggplot3d_lights")) return(light)
  if (inherits(light, "ggplot3d_light")) return(lights_3d(light))
  stop("light must be NULL, a light or lights_3d().", call. = FALSE)
}

#' Surface materials
#'
#' A material sets how strongly a face reflects the ambient, diffuse and
#' specular light of [lights_3d()]. The presets follow MATLAB's `material`:
#' `"matte"` (MATLAB `dull`), `"shiny"` and `"metal"`, with the ambient and
#' diffuse strengths taken relative to `dull` so that `"matte"` reproduces
#' plain Lambert shading. The specular highlight is Blinn's (1977) halfway
#' term `specular * max(0, n . h)^shininess`; its colour moves from white to
#' the face colour as `specular_colour` goes from 1 to 0 (MATLAB's
#' `SpecularColorReflectance`). `back` colours the back side of faces, the
#' side a face's normal points away from; NULL lights both sides alike.
#'
#' `style` selects an illustrative response, computed by the same shading
#' kernel in the vector and raster backends and the viewer:
#'
#' - `"toon"` quantises each light's diffuse response into `bands` equal
#'   steps of the cosine, or at the angles `breaks` (degrees from the light,
#'   increasing, at most seven; a normal exactly at a break, within 1e-6 in
#'   cosine, falls in the darker band), as ggcube's `ramp_steps()`. With the
#'   signed modes of [lights_3d()] the steps divide the signed level instead.
#'   `specular_steps = TRUE` (the default for toon) turns the highlight into a
#'   flat spot where Blinn's term reaches one half.
#' - `"gooch"` is the cool-to-warm shading of Gooch et al. (1998): the colour
#'   blends from `cool + alpha * fill` to `warm + beta * fill` as
#'   `(1 + n . l) / 2` goes from 0 to 1, averaged over the lights; the
#'   defaults are the paper's `k_blue = (0, 0, 0.4)` and
#'   `k_yellow = (0.4, 0.4, 0)` in linear light with `alpha = 0.2`,
#'   `beta = 0.6`.
#' - `"normals"` replaces the fill by the unit normal encoded as
#'   `(n + 1) / 2` in sRGB (three.js `MeshNormalMaterial`), in the panel's
#'   axes (`normal_space = "model"`), the camera's right, up and toward axes
#'   (`"camera"`), or a frame whose third axis points at the first
#'   contributing light (`"light"`). It takes no light and no specular term,
#'   and the layer's fill legend is dropped.
#'
#' Styles need a light: with `coord_3d(light = NULL)` faces keep their fill.
#' In the path tracer, illustrative surfaces return the shared kernel's
#' colour at the hit point and terminate the path, including indirect hits.
#' @param type Preset: `"matte"`, `"shiny"` or `"metal"`.
#' @param ambient,diffuse,specular Nonnegative strengths; NULL keeps the preset.
#' @param shininess Positive specular exponent; NULL keeps the preset.
#' @param specular_colour Specular colour reflectance in \[0, 1\]; NULL keeps
#'   the preset.
#' @param back NULL or one colour for the back side of faces.
#' @param style `"standard"`, `"toon"`, `"gooch"` or `"normals"`.
#' @param bands Number of toon bands, at least two.
#' @param breaks NULL or increasing toon band breaks in degrees in (0, 180).
#' @param specular_steps Whether the highlight is a flat spot; NULL takes
#'   `TRUE` for toon and `FALSE` otherwise.
#' @param cool,warm Gooch colours.
#' @param alpha,beta Gooch weights of the fill in the cool and warm colours.
#' @param normal_space Frame of the normal colours: `"model"`, `"camera"` or
#'   `"light"`.
#' @return A material for the `material` argument of the surface layers.
#' @references Gooch, Gooch, Shirley, Cohen (1998). A non-photorealistic
#'   lighting model for automatic technical illustration. SIGGRAPH '98,
#'   447-452. \doi{10.1145/280814.280950}
#' @examples
#' material_3d("shiny")
#' material_3d("metal", back = "grey40")
#' material_3d(style = "toon", breaks = c(40, 75))
#' d <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
#' d$z <- with(d, x^2 - y^2)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_surface_3d(shading = "smooth", material = material_3d("shiny")) + coord_3d()
#' p + geom_surface_3d(fill = "grey60", colour = NA, material = material_3d(style = "gooch")) + coord_3d()
#' p + geom_surface_3d(fill = "tomato", colour = NA, material = material_3d(style = "toon", bands = 4)) + coord_3d()
#' p + geom_surface_3d(colour = NA, shading = "smooth", material = material_3d(style = "normals")) + coord_3d()
#' @export
material_3d <- function(type = "matte", ambient = NULL, diffuse = NULL, specular = NULL, shininess = NULL,
                        specular_colour = NULL, back = NULL, style = "standard", bands = 3L, breaks = NULL,
                        specular_steps = NULL, cool = "#0000AA", warm = "#AAAA00", alpha = 0.2, beta = 0.6,
                        normal_space = "model") {
  presets <- list(matte = c(1, 1, 0, 10, 1), shiny = c(1, 0.75, 0.9, 20, 1), metal = c(1, 0.375, 1, 25, 0.5))
  value <- presets[[match.arg(type, names(presets))]]
  names(value) <- c("ambient", "diffuse", "specular", "shininess", "specular_colour")
  given <- list(ambient = ambient, diffuse = diffuse, specular = specular, shininess = shininess, specular_colour = specular_colour)
  for (name in names(given)) if (!is.null(given[[name]])) value[[name]] <- check_light_number_3d(given[[name]], name)
  if (value[["shininess"]] <= 0) stop("shininess must be positive.", call. = FALSE)
  if (value[["specular_colour"]] > 1) stop("specular_colour must lie in [0, 1].", call. = FALSE)
  if (!is.null(back)) back <- check_light_colour_3d(back, "back")
  style <- match.arg(style, material_styles_3d)
  if (!is.numeric(bands) || length(bands) != 1L || !is.finite(bands) || bands < 2 || bands != round(bands))
    stop("bands must be a whole number of at least two.", call. = FALSE)
  if (!is.null(breaks) && (!is.numeric(breaks) || !length(breaks) || length(breaks) > 7L || any(!is.finite(breaks)) ||
      any(breaks <= 0 | breaks >= 180) || any(diff(breaks) <= 0)))
    stop("breaks must be at most seven increasing angles in (0, 180) degrees.", call. = FALSE)
  specular_steps <- specular_steps %or3d% (style == "toon")
  if (!is.logical(specular_steps) || length(specular_steps) != 1L || is.na(specular_steps)) stop("specular_steps must be TRUE or FALSE.", call. = FALSE)
  alpha <- check_light_number_3d(alpha, "alpha"); beta <- check_light_number_3d(beta, "beta")
  structure(c(as.list(value), list(back = back %or3d% NA_character_, style = style, bands = as.integer(bands),
    breaks = if (is.null(breaks)) numeric() else as.double(breaks), specular_steps = specular_steps,
    cool = check_light_colour_3d(cool, "cool"), warm = check_light_colour_3d(warm, "warm"), alpha = alpha, beta = beta,
    normal_space = match.arg(normal_space, c("model", "camera", "light")))), class = "ggplot3d_material")
}

# Material styles in the order of the kernel's style ids (src/shade/shade.h),
# starting at 0.
material_styles_3d <- c("standard", "toon", "gooch", "normals")

# The materials table of a packet from a material (or NULL for none).
material_table_3d <- function(material) {
  if (is.null(material)) return(data.frame(material_id = integer()))
  if (!inherits(material, "ggplot3d_material")) stop("material must be made with material_3d().", call. = FALSE)
  out <- data.frame(material_id = 1L, ambient = material$ambient, diffuse = material$diffuse, specular = material$specular,
    shininess = material$shininess, specular_colour = material$specular_colour, back = material$back,
    style = material$style %or3d% "standard", bands = material$bands %or3d% 3L, specular_steps = material$specular_steps %or3d% FALSE,
    cool = material$cool %or3d% "#0000AA", warm = material$warm %or3d% "#AAAA00", alpha = material$alpha %or3d% 0.2,
    beta = material$beta %or3d% 0.6, normal_space = material$normal_space %or3d% "model")
  out$breaks <- I(list(material$breaks %or3d% numeric()))
  out
}

# A layer's show.legend with the fill legend dropped for normal colouring,
# which replaces the fill.
material_legend_3d <- function(material, show.legend) {
  if (is.null(material) || !identical(material$style, "normals")) return(show.legend)
  if (length(show.legend) == 1L && is.null(names(show.legend))) {
    if (isTRUE(show.legend)) return(c(fill = FALSE, colour = TRUE))
    if (isFALSE(show.legend)) return(FALSE)
    return(c(fill = FALSE))
  }
  show.legend[["fill"]] <- FALSE
  show.legend
}

# Lights with data-space point positions placed in the panel box (model
# coordinates through the panel frame), so that only the camera remains to
# resolve them.
panel_lights_3d <- function(light, panel_params) {
  light <- as_lights_3d(light)
  if (is.null(light)) return(NULL)
  light$occlusion <- NULL
  light$ao <- panel_params$coord_params$depth_cue$ao
  light$lights <- lapply(light$lights, function(l) {
    if (l$kind != light_kinds_3d[["point"]] || l$space != "data") return(l)
    ranges <- panel_params$ranges %or3d% list(x = c(0, 1), y = c(0, 1), z = c(0, 1))
    box <- vapply(1:3, function(k) (l$vector[k] - ranges[[k]][1L]) / diff(ranges[[k]]), numeric(1))
    frame <- frame_of_3d(panel_params)
    if (!isTRUE(frame$identity)) box <- drop(frame$map(matrix(box, 1L)))
    l$vector <- box; l$space <- "box"
    l
  })
  light
}

# Lights as the shading kernel reads them (src/shade/read.h): vectors in
# aspect-scaled model space, colours in linear light times intensity.
resolved_light_3d <- function(light, camera) {
  light <- as_lights_3d(light)
  scale <- camera$model_scale
  camera_axes <- function(v) camera$right * v[1L] + camera$up * v[2L] - camera$forward * v[3L]
  vectors <- lapply(light$lights, function(l) {
    v <- l$vector
    if (l$kind == light_kinds_3d[["point"]]) {
      target <- camera$eye + camera$distance * camera$ray
      if (l$space == "camera") return(target + camera_axes(v))
      return(v * scale)
    }
    v <- if (l$space == "camera") camera_axes(v) else v * scale
    v / sqrt(sum(v^2))
  })
  colour <- function(name) t(vapply(light$lights, function(l) {
    srgb_decode_3d(grDevices::col2rgb(l[[name]])[, 1L] / 255) * l$intensity
  }, numeric(3)))
  n <- length(light$lights)
  c(list(kind = vapply(light$lights, `[[`, integer(1), "kind"),
    vector = if (n) do.call(rbind, vectors) else matrix(numeric(), 0L, 3L),
    colour = if (n) colour("colour") else matrix(numeric(), 0L, 3L),
    ground = if (n) colour("ground") else matrix(numeric(), 0L, 3L),
    falloff = vapply(light$lights, `[[`, numeric(1), "falloff"),
    ambient = rep(light$ambient, 3L), frame = rbind(as.double(scale %or3d% c(1, 1, 1)), as.double(camera$right %or3d% c(1, 0, 0)),
      as.double(camera$up %or3d% c(0, 1, 0)), -as.double(camera$forward %or3d% c(0, 0, -1)))), transfer_settings_3d(light))
}

# The colour transfer of lights as the kernel reads it: transfer id, range,
# neutral cosine and linear tone colours.
transfer_settings_3d <- function(light) {
  linear <- function(colour) unname(srgb_decode_3d(grDevices::col2rgb(colour)[, 1L] / 255))
  list(transfer = match(light$mode, light_transfers_3d) - 1L, range = light$range %or3d% c(0, 0),
    neutral = cospi((light$neutral %or3d% 45) / 180), shadow = linear(light$shadow %or3d% "black"),
    highlight = linear(light$highlight %or3d% "white"))
}

material_argument_3d <- function(material) {
  if (!is.null(material) && !inherits(material, "ggplot3d_material")) stop("material must be made with material_3d().", call. = FALSE)
  material
}
