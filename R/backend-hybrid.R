utils::globalVariables("raster_occlusion_cpp")

# Visibility of the primitives of a vector packet against a raster result, on
# the raster's pixel grid. A primitive is behind the image where it lies
# behind the coverage depth, the depth at which the image's accumulated alpha
# reaches half its final value: drawn under the image it is then misplaced
# by less of the image's coverage than drawn over it. Over opaque paint this
# is the opaque depth. Every primitive is widened by
# one pixel on each side for the test (strokes, marker footprints, and faces
# through an outline), so the pixels its antialiased edge touches are
# classified with it.
raster_occlusion_3d <- function(packet, camera, viewport, result) {
  pixel <- max(diff(result$extent[c(1L, 3L)]) * viewport$width_mm / result$width,
               diff(result$extent[c(2L, 4L)]) * viewport$height_mm / result$height)
  packet$polylines$width_mm <- packet$polylines$width_mm + 2 * pixel
  packet$polylines$mask_width_mm <- packet$polylines$mask_width_mm + 2 * pixel
  ellipse <- packet$footprints$kind == 1L
  for (axis in c("radius_x_mm", "radius_y_mm"))
    packet$footprints[[axis]][ellipse] <- packet$footprints[[axis]][ellipse] + pixel
  prepared <- raster_prepare_cpp(scene_engine_packet_3d(packet), camera, viewport)
  colours <- raster_colours_3d(packet, prepared, NULL)
  faces <- colours[[1L]]
  if (ncol(faces)) {
    outline <- attr(faces, "outline"); outline[4L, ] <- 1
    attr(faces, "outline") <- outline
    attr(faces, "width_mm") <- attr(faces, "width_mm") + 2 * pixel
    colours[[1L]] <- faces
  }
  dashes <- rep(list(Inf), length(packet$polylines$width_mm))
  raster_occlusion_cpp(prepared$pointer, colours, dashes, result$extent,
    as.integer(result$width), as.integer(result$height), as.integer(result$antialias), result$buffers$coverage_depth)
}

# Hybrid panels rasterise the layers drawn with `raster = TRUE` and the traced
# layers, and compile the rest, with the panel furniture, into vector
# fragments. Vector panels place their traced layers the same way. A vector primitive
# lies in front of the raster image, behind it, or partly behind; the image is
# drawn between two vector passes. The behind pass holds the primitives behind
# the image and the hidden part of the partial ones, so the image's
# antialiased edges composite over them; the front pass holds the rest. A
# partial primitive is clipped to its hidden pixels in the first pass and to
# their complement in the second, so it is painted once.
render_hybrid_panel_3d <- function(x, furniture, backend) {
  panel <- x$panel_params$panel_id
  camera <- furniture$camera; viewport <- furniture$viewport
  light <- x$panel_params$coord_params$light
  traced <- vapply(x$packets, function(p) !is.null(attr(p, "raymarch")), logical(1))
  styled <- identical(backend$type, "hybrid") &
    vapply(x$packets, function(p) any(p$styles$raster %in% TRUE), logical(1))
  rasterised <- traced | styled
  specs <- raster_traced_specs_3d(x$packets[rasterised])
  raster_packet <- merge_scene_packets(x$packets[rasterised], panel)
  partitions <- unlist(lapply(primitive_kinds_3d, function(k) raster_packet[[k]]$partition))
  scene_assert(all(partitions == 1L), "Rasterised layers of a hybrid panel need occlusion = \"depth\".")
  vector_packet <- device_text_metrics_3d(merge_scene_packets(c(list(furniture$packet), x$packets[!rasterised]), panel))
  vector_marker_limit_3d(vector_packet)
  scene <- scene_compile(vector_packet, camera, viewport,
                         stable_partition = isTRUE(x$panel_params$coord_params$stable_partition))
  result <- raster_scene_3d(raster_packet, camera, viewport, backend$antialias, light,
    buffers = TRUE, blend = backend$blend, traced = specs, edl = depth_cue_edl_3d(x$panel_params))
  occlusion <- raster_occlusion_3d(vector_packet, camera, viewport, result)
  f <- scene$fragments
  kinds <- primitive_kinds_3d
  status <- integer(length(f$kind))
  for (k in seq_along(kinds)) {
    i <- which(f$kind == k)
    status[i] <- occlusion$status[[kinds[k]]][f$source_index[i]]
  }
  rings <- occlusion$rings
  owner <- paste(rings$kind, rings$row)
  keys <- unique(owner)
  region_id <- match(paste(f$kind, f$source_index), keys, nomatch = 0L)
  # Rings wind counter-clockwise around hidden pixels and clockwise around
  # holes, so the nonzero winding rule, which clipping paths use, covers the
  # hidden pixels; a clockwise frame around the canvas turns that into the
  # complement.
  extent <- result$extent
  frame <- list(x = extent[c(1L, 1L, 3L, 3L)], y = extent[c(2L, 4L, 4L, 2L)])
  region <- function(key, complement) {
    j <- which(owner == key)
    x <- rings$x[j]; y <- rings$y[j]; id <- match(rings$ring[j], unique(rings$ring[j]))
    if (complement) { x <- c(frame$x, x); y <- c(frame$y, y); id <- c(rep(max(id) + 1L, 4L), id) }
    grid::pathGrob(x, y, id = id, rule = "winding", gp = grid::gpar(fill = "black", col = NA))
  }
  pass <- function(keep, complement, name) {
    selected <- scene
    selected$draw_order <- scene$draw_order[status[scene$draw_order] %in% keep]
    g <- render_scene_grid(selected, viewport, light = light,
      regions = list(id = region_id, grobs = lapply(keys, region, complement = complement)))
    g$name <- name
    g
  }
  behind <- pass(c(1L, 2L), FALSE, "scene-hybrid-behind-3d")
  front <- pass(c(0L, 2L), TRUE, "scene-hybrid-front-3d")
  children <- c(list(behind), raster_layer_children_3d(result, raster_packet), list(front))
  layers <- unlist(lapply(x$packets[rasterised], function(p)
    attr(p, "raymarch")$layer_id %or3d% unique(unlist(lapply(p[primitive_kinds_3d], `[[`, "layer_id")))))
  capabilities <- utils::modifyList(grid_capabilities_3d(), list(raster = TRUE, image_layers = sort(unique(as.integer(layers)))))
  grob <- grid::gTree(children = do.call(grid::gList, children), raster_result = result,
    occlusion = occlusion, capabilities = capabilities, name = "scene-hybrid-3d")
  list(scene = scene, grob = grob)
}
