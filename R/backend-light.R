colour_channels_3d <- function(colour, alpha = FALSE) {
  palette <- unique(colour)
  grDevices::col2rgb(palette, alpha = alpha)[, match(colour, palette), drop = FALSE]
}

shade_fill_3d <- function(fill, fragments, rows, camera, light) {
  if (is.null(light) || !length(rows)) return(fill)
  normal <- cbind(fragments$normal_x[rows], fragments$normal_y[rows], fragments$normal_z[rows])
  direction <- light$direction
  if (light$space == "camera") direction <- camera$right * direction[1L] + camera$up * direction[2L] - camera$forward * direction[3L]
  if (light$space == "data") direction <- direction * camera$model_scale
  direction <- direction / sqrt(sum(direction^2))
  toward <- -camera$forward
  normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, ] <- -normal[rowSums(sweep(normal, 2L, toward, "*")) < 0, , drop = FALSE]
  brightness <- light$ambient + light$diffuse * pmax(0, drop(normal %*% direction))
  rgb <- t(colour_channels_3d(fill)) / 255
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  linear <- if (light$mode == "multiply") linear * brightness else 1 - (1 - linear) * brightness
  rgb <- ifelse(linear <= 0.0031308, 12.92 * linear, 1.055 * linear^(1 / 2.4) - 0.055)
  result <- grDevices::rgb(pmin(1, pmax(0, rgb[, 1L])), pmin(1, pmax(0, rgb[, 2L])), pmin(1, pmax(0, rgb[, 3L])))
  result[is.na(fill)] <- NA_character_
  result
}

