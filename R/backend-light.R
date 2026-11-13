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

shade_fill_3d <- function(fill, fragments, rows, camera, light, styles = NULL) {
  if (is.null(light) || !length(rows)) return(fill)
  normal <- cbind(fragments$normal_x[rows], fragments$normal_y[rows], fragments$normal_z[rows])
  if (all(c("normal_x", "normal_y", "normal_z") %in% names(styles))) {
    supplied <- as.matrix(styles[c("normal_x", "normal_y", "normal_z")])
    supplied <- sweep(supplied, 2L, camera$model_scale, "/")
    lengths <- sqrt(rowSums(supplied^2))
    valid <- is.finite(lengths) & lengths > 0
    normal[valid, ] <- supplied[valid, , drop = FALSE] / lengths[valid]
  }
  direction <- light$direction
  if (light$space == "camera") direction <- camera$right * direction[1L] + camera$up * direction[2L] - camera$forward * direction[3L]
  if (light$space == "data") direction <- direction * camera$model_scale
  direction <- direction / sqrt(sum(direction^2))
  toward <- -(camera$ray %or3d% camera$forward)
  normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, ] <- -normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, , drop = FALSE]
  brightness <- light$ambient + light$diffuse * pmax(0, drop(normal %*% direction))
  rgb <- t(colour_channels_3d(fill)) / 255
  linear <- srgb_decode_3d(rgb)
  linear <- if (light$mode == "multiply") linear * brightness else 1 - (1 - linear) * brightness
  rgb <- srgb_encode_3d(linear)
  result <- grDevices::rgb(pmin(1, pmax(0, rgb[, 1L])), pmin(1, pmax(0, rgb[, 2L])), pmin(1, pmax(0, rgb[, 3L])))
  result[is.na(fill)] <- NA_character_
  result
}

