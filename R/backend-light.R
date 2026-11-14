utils::globalVariables(c("shade_cpp", "shade_swatch_cpp", "shade_transfer_cpp", "shade_glsl_cpp"))

colour_channels_3d <- function(colour, alpha = FALSE) {
  palette <- unique(colour)
  grDevices::col2rgb(palette, alpha = alpha)[, match(colour, palette), drop = FALSE]
}

# sRGB transfer function (IEC 61966-2-1) between encoded values and linear
# light, both in [0, 1].
srgb_decode_3d <- function(value) ifelse(value <= 0.04045, value / 12.92, ((value + 0.055) / 1.055)^2.4)
srgb_encode_3d <- function(value) ifelse(value <= 0.0031308, 12.92 * value, 1.055 * value^(1 / 2.4) - 0.055)

# Encoded grey of the same relative luminance (Rec. 709 primaries) as each
# column of a 3 x n matrix of encoded sRGB values.
grey_level_3d <- function(rgb) {
  luminance <- colSums(srgb_decode_3d(rgb) * c(0.2126, 0.7152, 0.0722))
  pmin(1, pmax(0, srgb_encode_3d(luminance)))
}

# Linear-light rows (n x 3) of colours, and encoded colours of such rows.
linear_rgb_3d <- function(colour) srgb_decode_3d(t(colour_channels_3d(colour)) / 255)
encode_rgb_3d <- function(linear) {
  rgb <- srgb_encode_3d(pmin(pmax(linear, 0), 1))
  grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L])
}

# Unit model-space direction toward the first directional light of a
# lighting configuration (see lights_3d()), for a camera with right, up and
# forward axes and the model scale of the panel box.
light_vector_3d <- function(camera, light) {
  direction <- light$direction
  if (light$space == "camera") direction <- camera$right * direction[1L] + camera$up * direction[2L] - camera$forward * direction[3L]
  if (light$space == "data") direction <- direction * camera$model_scale
  direction / sqrt(sum(direction^2))
}

# Unit direction from the scene toward a parallel viewer.
view_toward_3d <- function(camera) -(camera$ray %or3d% camera$forward)

# Whether a resolved camera projects in perspective: its projection matrix
# divides by depth (last row (0, 0, 1, 0)).
camera_perspective_3d <- function(camera) {
  if (!is.null(camera$perspective)) return(isTRUE(camera$perspective))
  !is.null(camera$projection) && camera$projection[4L, 4L] == 0
}

# The viewing direction as the shading kernel reads it.
kernel_view_3d <- function(camera) {
  list(perspective = camera_perspective_3d(camera), eye = as.double(camera$eye %or3d% c(0, 0, 0)),
       toward = view_toward_3d(camera))
}

# The materials table of a packet as the shading kernel reads it, back
# colours in linear light (NA for none).
kernel_materials_3d <- function(materials) {
  if (is.null(materials) || !nrow(materials)) {
    return(list(ambient = numeric(), diffuse = numeric(), specular = numeric(), shininess = numeric(),
                specular_colour = numeric(), back = matrix(numeric(), 0L, 3L)))
  }
  back <- matrix(NA_real_, nrow(materials), 3L)
  given <- !is.na(materials$back)
  if (any(given)) back[given, ] <- linear_rgb_3d(materials$back[given])
  list(ambient = as.double(materials$ambient), diffuse = as.double(materials$diffuse),
       specular = as.double(materials$specular), shininess = as.double(materials$shininess),
       specular_colour = as.double(materials$specular_colour), back = back)
}

# Kernel material index of each style row: its material_id, 0 for none.
material_index_3d <- function(styles) {
  id <- styles$material_id
  if (is.null(id)) return(integer(NROW(styles)))
  id <- as.integer(id)
  id[is.na(id)] <- 0L
  id
}

# Unit model-space normals from normals in normalised panel coordinates: the
# aspect scaling maps a normal by its inverse transpose.
model_normals_3d <- function(normal, camera) {
  normal <- sweep(normal, 2L, camera$model_scale, "/")
  normal / sqrt(rowSums(normal^2))
}

# Linear-light colours (rows) lit by the shading kernel at unit model-space
# normals and model-space positions, with kernel material indices into the
# packet's materials table.
shade_linear_3d <- function(linear, normal, camera, light, position = NULL, material = NULL, materials = NULL) {
  n <- nrow(linear)
  if (!n) return(linear)
  if (is.null(position)) position <- matrix(camera$eye + camera$distance * camera$ray, n, 3L, byrow = TRUE)
  position[!is.finite(position)] <- 0
  shade_cpp(normal, position, linear, as.integer(material %or3d% integer(n)), kernel_materials_3d(materials),
            resolved_light_3d(light, camera), kernel_view_3d(camera))
}

# Model-space centroids of compiled fragments `rows`.
fragment_centres_3d <- function(scene, rows) {
  f <- scene$fragments
  counts <- f$count[rows]
  index <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  xyz <- cbind(scene$vertices$x[index], scene$vertices$y[index], scene$vertices$z[index])
  sweep(rowsum(xyz, rep(seq_along(rows), counts), reorder = FALSE) / counts, 2L, scene$camera$model_scale, "*")
}

shade_fill_3d <- function(fill, fragments, rows, camera, light, styles = NULL, position = NULL, materials = NULL) {
  if (is.null(light) || !length(rows)) return(fill)
  normal <- face_shading_normals_3d(fragments, rows, camera, styles)
  result <- encode_rgb_3d(shade_linear_3d(linear_rgb_3d(fill), normal, camera, light, position,
    if (!is.null(styles)) material_index_3d(styles), materials))
  result[is.na(fill)] <- NA_character_
  result
}

# Linear-light colours (one per row) scaled by brightness: multiplied, or
# their distance from white scaled.
brighten_linear_3d <- function(linear, brightness, mode) {
  shade_transfer_cpp(linear, rep_len(as.double(brightness), nrow(linear)), mode == "multiply")
}

apply_brightness_3d <- function(fill, brightness, mode) {
  result <- encode_rgb_3d(brighten_linear_3d(linear_rgb_3d(fill), brightness, mode))
  result[is.na(fill)] <- NA_character_
  result
}
