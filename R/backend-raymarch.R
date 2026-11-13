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

# Ray samples and raster centre samples share camera-space depth and top-down rows.
raymarch_scene_3d <- function(specs, packet, camera, viewport, backend, light) {
  result <- raster_scene_3d(packet, camera, viewport, backend$antialias, light, buffers = TRUE, blend = backend$blend)
  if (any(packet$faces$opacity < 1) || any(packet$polylines$opacity < 1) || any(packet$markers$opacity < 1))
    stop("Mixed ray-marched scenes require opaque raster geometry.", call. = FALSE)
  depth <- result$buffers$depth
  absorption <- vapply(specs, function(s) identical(s$kind, "volume") && s$mode == 0L, logical(1))
  for (j in which(!absorption)) {
    spec <- specs[[j]]
    rays <- raymarch_rays_3d(camera, viewport, result, spec$ranges)
    limit <- (pmin(rays$far, ifelse(is.na(depth), Inf, depth)) - rays$near) * rays$length
    kind <- spec$kind %or3d% "sdf"
    hit <- raymarch_surface_handlers_3d[[kind]](spec, rays, as.vector(limit))
    z <- rays$near + hit$distance / rays$length
    take <- which(is.finite(z) & z <= rays$far & (is.na(depth) | z < depth))
    if (!length(take)) next
    span <- vapply(spec$ranges, diff, numeric(1))
    normal <- sweep(hit$normal, 2L, span / result$camera$model_scale, "*")
    normal <- normal / pmax(sqrt(rowSums(normal^2)), .Machine$double.eps)
    fragments <- list(normal_x = normal[, 1L], normal_y = normal[, 2L], normal_z = normal[, 3L])
    fill <- if (is.null(hit$colour)) rep(spec$fill, length(z)) else {
      rgb <- srgb_encode_3d(hit$colour)
      grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L])
    }
    lighting <- if (identical(spec$mode, 1L)) NULL else light
    rgb <- t(colour_channels_3d(shade_fill_3d(fill[take], fragments, take, result$camera, lighting))) / 255
    result <- raymarch_paint_3d(result, srgb_decode_3d(rgb), rep(1, length(take)), take)
    depth[take] <- z[take]
    result$buffers$opaque_depth[take] <- z[take]
    result$buffers$kind[take] <- 1L
    result$buffers$row[take] <- j
    for (k in 1:3) result$buffers$normal[[k]][take] <- normal[take, k]
  }
  if (any(absorption)) {
    volumes <- specs[absorption]
    rays <- raymarch_rays_3d(camera, viewport, result, volumes[[1L]]$ranges)
    limit <- (pmin(rays$far, ifelse(is.na(depth), Inf, depth)) - rays$near) * rays$length
    medium <- raymarch_volume_cpp(volumes, rays$origin, rays$direction, as.vector(limit), raster_threads_3d())
    take <- which(medium$alpha > 0)
    result <- raymarch_paint_3d(result, medium$colour[take, , drop = FALSE], medium$alpha[take], take)
    z <- rays$near + medium$distance / rays$length
    depth[take] <- pmin(z[take], ifelse(is.na(depth[take]), Inf, depth[take]))
  }
  result$buffers$depth <- depth
  result
}

render_raymarch_panel_3d <- function(x, furniture, backend) {
  specs <- Filter(Negate(is.null), lapply(x$packets, attr, "raymarch"))
  packet <- merge_scene_packets(x$packets, x$panel_params$panel_id)
  result <- raymarch_scene_3d(specs, packet, furniture$camera, furniture$viewport, backend,
    x$panel_params$coord_params$light)
  scene <- scene_compile(furniture$packet, furniture$camera, furniture$viewport)
  part <- function(partition, name) {
    selected <- scene
    selected$draw_order <- scene$draw_order[scene$fragments$partition[scene$draw_order] %in% partition]
    g <- render_scene_grid(selected, furniture$viewport); g$name <- name; g
  }
  children <- c(list(part(c(0L, 1L), "raymarch-underlay-3d"), raster_image_grob_3d(result)),
    raster_text_3d(result, packet), list(part(2L, "raymarch-overlay-3d")))
  list(scene = scene, grob = grid::gTree(children = do.call(grid::gList, children),
    raster_result = result, name = "scene-raymarch-3d"))
}
