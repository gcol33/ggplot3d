utils::globalVariables(c("raymarch_rays_cpp", "raymarch_volume_cpp"))

raymarch_surface_handlers_3d <- list(
  sdf = function(spec, rays, limit) raymarch_trace_cpp(spec$tape, rays$origin, rays$direction,
    unlist(spec$ranges, use.names = FALSE), spec$tolerance, spec$max_steps, raster_threads_3d()),
  volume = function(spec, rays, limit) raymarch_volume_cpp(list(spec), rays$origin, rays$direction, limit, raster_threads_3d())
)

raymarch_paint_3d <- function(result, colour, alpha, pixels) {
  if (!length(pixels)) return(result)
  index <- arrayInd(pixels, c(result$height, result$width))
  native <- (index[, 1L] - 1L) * result$width + index[, 2L]
  packed <- as.double(result$image[native])
  packed[is.na(packed)] <- 2^31
  packed[packed < 0] <- packed[packed < 0] + 2^32
  rgba <- vapply(0:3, function(k) (packed %/% 256^k) %% 256 / 255, numeric(length(packed)))
  rgba <- matrix(rgba, ncol = 4L)
  a <- alpha + (1 - alpha) * rgba[, 4L]
  rgb <- colour + (1 - alpha) * rgba[, 4L] * srgb_decode_3d(rgba[, 1:3, drop = FALSE])
  rgb <- srgb_encode_3d(rgb / pmax(a, .Machine$double.eps))
  rgba <- round(pmin(pmax(cbind(rgb, a), 0), 1) * 255)
  packed <- drop(rgba %*% 256^(0:3))
  packed[packed >= 2^31] <- packed[packed >= 2^31] - 2^32
  result$image[native] <- as.integer(packed)
  result
}

raymarch_rays_3d <- function(camera, viewport, result, ranges) {
  rays <- raymarch_rays_cpp(camera, viewport, as.integer(result$width), as.integer(result$height), result$extent)
  lo <- vapply(ranges, `[`, numeric(1), 1L)
  span <- vapply(ranges, diff, numeric(1))
  rays$origin <- sweep(sweep(rays$origin, 2L, span, "*"), 2L, lo, "+")
  rays$direction <- sweep(rays$direction, 2L, span, "*")
  rays$length <- sqrt(rowSums(rays$direction^2))
  rays$direction <- rays$direction / rays$length
  rays
}

