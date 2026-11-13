utils::globalVariables("raymarch_rays_cpp")

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
  for (j in seq_along(specs)) {
    spec <- specs[[j]]
    rays <- raymarch_rays_3d(camera, viewport, result, spec$ranges)
    hit <- raymarch_trace_cpp(spec$tape, rays$origin, rays$direction,
      unlist(spec$ranges, use.names = FALSE), spec$tolerance, spec$max_steps, raster_threads_3d())
    z <- rays$near + hit$distance / rays$length
    take <- which(is.finite(z) & z <= rays$far & (is.na(depth) | z < depth))
    if (!length(take)) next
    span <- vapply(spec$ranges, diff, numeric(1))
    normal <- sweep(hit$normal, 2L, span / result$camera$model_scale, "*")
    normal <- normal / pmax(sqrt(rowSums(normal^2)), .Machine$double.eps)
    fragments <- list(normal_x = normal[, 1L], normal_y = normal[, 2L], normal_z = normal[, 3L])
    fill <- rep(spec$fill, length(z))
    rgba <- colour_channels_3d(shade_fill_3d(fill[take], fragments, take, result$camera, light), alpha = TRUE)
    packed <- colSums(rgba * 256^(0:3))
    packed[packed >= 2^31] <- packed[packed >= 2^31] - 2^32
    pixel <- arrayInd(take, dim(depth))
    result$image[(pixel[, 1L] - 1L) * result$width + pixel[, 2L]] <- as.integer(packed)
    depth[take] <- z[take]
    result$buffers$opaque_depth[take] <- z[take]
    result$buffers$kind[take] <- 1L
    result$buffers$row[take] <- j
    for (k in 1:3) result$buffers$normal[[k]][take] <- normal[take, k]
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
