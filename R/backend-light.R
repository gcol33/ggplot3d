utils::globalVariables(c("shade_cpp", "shade_transfer_cpp"))

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

# Unit model-space direction toward the light, for a camera with right, up
# and forward axes and the model scale of the panel box.
light_vector_3d <- function(camera, light) {
  direction <- light$direction
  if (light$space == "camera") direction <- camera$right * direction[1L] + camera$up * direction[2L] - camera$forward * direction[3L]
  if (light$space == "data") direction <- direction * camera$model_scale
  direction / sqrt(sum(direction^2))
}

# The light as the shading kernel reads it (src/shade/read.h).
resolved_light_3d <- function(light, camera) {
  list(direction = light_vector_3d(camera, light), ambient = light$ambient,
       diffuse = light$diffuse, mode = light$mode)
}

# Unit direction from the scene toward a parallel viewer.
view_toward_3d <- function(camera) -(camera$ray %or3d% camera$forward)

# Unit model-space normals from normals in normalised panel coordinates: the
# aspect scaling maps a normal by its inverse transpose.
model_normals_3d <- function(normal, camera) {
  normal <- sweep(normal, 2L, camera$model_scale, "/")
  normal / sqrt(rowSums(normal^2))
}

# Linear-light colours (rows) lit by the shading kernel at unit model-space
# normals, seen from `toward` (one row, or one per colour).
shade_linear_3d <- function(linear, normal, camera, light, toward = view_toward_3d(camera)) {
  if (!nrow(linear)) return(linear)
  shade_cpp(normal, matrix(toward, ncol = 3L), linear, resolved_light_3d(light, camera))
}

shade_fill_3d <- function(fill, fragments, rows, camera, light, styles = NULL) {
  if (is.null(light) || !length(rows)) return(fill)
  normal <- face_shading_normals_3d(fragments, rows, camera, styles)
  result <- encode_rgb_3d(shade_linear_3d(linear_rgb_3d(fill), normal, camera, light))
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
