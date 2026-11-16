utils::globalVariables("raymarch_hdr_cpp")

#' @rdname light_directional
#' @param image Linear RGB array with dimensions height by width by 3, or a
#'   Radiance RGBE `.hdr` or sRGB `.png` path. PNG input needs package png.
#' @param rotation Environment rotation about the up axis, in degrees.
#' @export
light_environment <- function(image, intensity = 1, rotation = 0, space = "data") {
  if (is.character(image) && length(image) == 1L && !is.na(image)) {
    if (!file.exists(image)) stop("Environment image does not exist: ", image, call. = FALSE)
    extension <- tolower(tools::file_ext(image))
    image <- switch(extension,
      hdr = raymarch_hdr_cpp(normalizePath(image, winslash = "/")),
      png = {
        if (!requireNamespace("png", quietly = TRUE)) stop("PNG environments require package 'png'.", call. = FALSE)
        pixels <- png::readPNG(image)
        if (length(dim(pixels)) == 3L && dim(pixels)[3L] <= 2L) pixels <- pixels[, , 1L]
        if (length(dim(pixels)) == 2L) pixels <- array(rep(pixels, 3L), c(dim(pixels), 3L))
        if (length(dim(pixels)) == 3L && dim(pixels)[3L] >= 3L) pixels <- pixels[, , 1:3, drop = FALSE]
        srgb_decode_3d(pixels)
      }, stop("Environment image must be a .hdr or .png file.", call. = FALSE))
  }
  if (!is.numeric(image) || length(dim(image)) != 3L || dim(image)[3L] != 3L ||
      any(dim(image)[1:2] < 1L) || any(!is.finite(image) | image < 0))
    stop("image must be a finite nonnegative height x width x 3 linear RGB array.", call. = FALSE)
  if (!is.numeric(rotation) || length(rotation) != 1L || !is.finite(rotation))
    stop("rotation must be one finite angle in degrees.", call. = FALSE)
  light <- new_light_3d(light_kinds_3d[["environment"]], space = match.arg(space, c("data", "camera")), intensity = intensity)
  light$image <- image; light$rotation <- rotation
  light
}

environment_mean_3d <- function(light, linear = TRUE) {
  h <- dim(light$image)[1L]
  weight <- cospi((0:(h - 1L)) / h) - cospi((1:h) / h)
  mean <- vapply(1:3, function(k) sum(light$image[, , k] * weight) / (sum(weight) * dim(light$image)[2L]), numeric(1)) * light$intensity
  if (isFALSE(linear)) srgb_encode_3d(mean) else mean
}

resolved_environment_3d <- function(light, camera) {
  angle <- light$rotation * pi / 180
  frame <- if (light$space == "camera") cbind(camera$right, -camera$forward, camera$up) else diag(3)
  frame <- frame %*% matrix(c(cos(angle), sin(angle), 0, -sin(angle), cos(angle), 0, 0, 0, 1), 3L)
  list(image = light$image * light$intensity, frame = frame)
}
