scene_schemas <- function() {
  common <- c(primitive_id = "integer", layer_id = "integer",
              source_row = "integer", style_id = "integer", offset = "integer",
              count = "integer", role = "integer", partition = "integer",
              opacity = "double")
  list(
    vertices = c(x = "double", y = "double", z = "double"),
    corners = c(vertex = "integer", source_row = "integer"),
    faces = common,
    polylines = c(common, width_mm = "double", mitre_limit = "double",
                  cap = "integer", join = "integer", hidden = "integer",
                  arrow_start = "logical", arrow_end = "logical"),
    markers = c(common, footprint_id = "integer"),
    text = c(common, width_mm = "double", height_mm = "double",
             hjust = "double", vjust = "double", angle = "double"),
    footprints = c(kind = "integer", offset = "integer", count = "integer",
                   radius_x_mm = "double", radius_y_mm = "double",
                   angle = "double"),
    footprint_vertices = c(x_mm = "double", y_mm = "double", ring = "integer")
  )
}

scene_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

scene_table <- function(x, schema, defaults, name) {
  scene_assert(is.list(x) && !anyDuplicated(names(x)) &&
                 all(names(x) %in% names(schema)), paste(name, "has unknown columns"))
  n <- if (length(x)) length(x[[1L]]) else 0L
  scene_assert(all(lengths(x) == n), paste(name, "has unequal column lengths"))
  out <- lapply(names(schema), function(column) {
    value <- x[[column]]
    if (is.null(value)) {
      value <- defaults[[column]]
      if (is.null(value)) {
        scene_assert(n == 0L, paste(name, "requires", column))
        value <- vector(schema[[column]], 0L)
      } else value <- rep_len(value, n)
    }
    scene_assert(is.null(dim(value)) && !is.object(value),
                 paste(name, column, "must be an unclassed vector"))
    type <- schema[[column]]
    if (type == "integer") {
      scene_assert(is.numeric(value) && all(is.finite(value)) &&
                     all(value == trunc(value)) &&
                     all(abs(value) <= .Machine$integer.max),
                   paste(name, column, "must contain finite integers"))
      value <- as.integer(value)
    } else if (type == "double") {
      scene_assert(is.numeric(value) && all(is.finite(value)),
                   paste(name, column, "must contain finite numbers"))
      value <- as.double(value)
    } else {
      scene_assert(is.logical(value) && !anyNA(value),
                   paste(name, column, "must be logical without NA"))
    }
    value
  })
  names(out) <- names(schema)
  out
}

new_scene_packet <- function(vertices = list(), corners = list(),
                             faces = list(), polylines = list(),
                             markers = list(), text = list(),
                             footprints = list(), footprint_vertices = list(),
                             styles = data.frame(style_id = integer()),
                             panel_id = 1L) {
  schema <- scene_schemas()
  tables <- list(vertices = vertices, corners = corners, faces = faces,
                 polylines = polylines, markers = markers, text = text,
                 footprints = footprints, footprint_vertices = footprint_vertices)
  next_id <- 1L
  for (name in names(tables)) {
    n <- if (length(tables[[name]])) length(tables[[name]][[1L]]) else 0L
    kind <- match(name, c("faces", "polylines", "markers", "text"))
    defaults <- list(source_row = seq_len(n))
    if (!is.na(kind)) {
      defaults <- c(defaults, list(
        primitive_id = next_id + seq_len(n) - 1L, layer_id = 1L,
        style_id = 1L, count = c(3L, 2L, 1L, 1L)[kind],
        role = kind - 1L, partition = 1L, opacity = 1,
        width_mm = 0, height_mm = 0, mitre_limit = 10,
        cap = 0L, join = 0L, hidden = 0L,
        arrow_start = FALSE, arrow_end = FALSE,
        hjust = 0.5, vjust = 0.5, angle = 0))
      next_id <- next_id + n
    }
    tables[[name]] <- scene_table(tables[[name]], schema[[name]], defaults, name)
  }
  scene_assert(is.numeric(panel_id) && length(panel_id) == 1L &&
                 is.finite(panel_id) && panel_id >= 1 &&
                 panel_id <= .Machine$integer.max && panel_id == trunc(panel_id),
               "panel_id must be a positive integer")
  scene_assert(is.data.frame(styles), "styles must be a data frame")
  packet <- c(list(schema_version = 1L, panel_id = as.integer(panel_id),
                   style_count = nrow(styles)), tables, list(styles = styles))
  class(packet) <- "ggplot3d_scene_packet"
  validate_scene_packet(packet)
  packet
}

validate_scene_packet <- function(packet) {
  schemas <- scene_schemas()
  expected <- c("schema_version", "panel_id", "style_count", names(schemas), "styles")
  scene_assert(is.list(packet) && identical(names(packet), expected),
               "packet has an invalid schema")
  for (key in c("schema_version", "panel_id", "style_count")) {
    scene_assert(is.integer(packet[[key]]) && length(packet[[key]]) == 1L &&
                   !anyNA(packet[[key]]), paste(key, "must be an integer scalar"))
  }
  scene_assert(packet$schema_version == 1L, "unsupported scene schema version")
  scene_assert(packet$panel_id >= 1L, "panel_id must be positive")
  scene_assert(is.data.frame(packet$styles) &&
                 identical(packet$styles$style_id, seq_len(nrow(packet$styles))) &&
                 packet$style_count == nrow(packet$styles),
               "styles must have contiguous integer style_id keys starting at 1")
  for (name in names(schemas)) {
    tab <- packet[[name]]
    spec <- schemas[[name]]
    scene_assert(is.list(tab) && identical(names(tab), names(spec)),
                 paste(name, "has an invalid schema"))
    scene_assert(length(unique(lengths(tab))) == 1L,
                 paste(name, "has unequal column lengths"))
    for (column in names(spec)) {
      scene_assert(identical(typeof(tab[[column]]), spec[[column]]) &&
                     is.null(dim(tab[[column]])) && !is.object(tab[[column]]) &&
                     all(is.finite(tab[[column]])),
                   paste(name, column, "has invalid type or non-finite values"))
    }
  }
  nv <- length(packet$vertices$x)
  nc <- length(packet$corners$vertex)
  scene_assert(all(packet$corners$vertex >= 1L & packet$corners$vertex <= nv) &&
                 all(packet$corners$source_row >= 0L), "invalid corner references")
  ids <- integer()
  for (name in c("faces", "polylines", "markers", "text")) {
    tab <- packet[[name]]
    ids <- c(ids, tab$primitive_id)
    minimum <- switch(name, faces = 3L, polylines = 2L, 1L)
    scene_assert(all(tab$count >= minimum) &&
                   all(tab$offset >= 1L & as.double(tab$offset) + tab$count - 1 <= nc),
                 paste(name, "has invalid corner ranges"))
    if (name %in% c("markers", "text")) {
      scene_assert(all(tab$count == 1L), paste(name, "requires one anchor"))
    }
    scene_assert(all(tab$layer_id >= 0L & tab$source_row >= 0L) &&
                   all(tab$style_id >= 1L & tab$style_id <= packet$style_count) &&
                   all(tab$role %in% 0:3) && all(tab$partition %in% 0:2) &&
                   all(tab$opacity >= 0 & tab$opacity <= 1),
                 paste(name, "has invalid metadata"))
  }
  scene_assert(all(ids >= 1L) && !anyDuplicated(ids),
               "primitive_id must be positive and unique across the panel")
  line <- packet$polylines
  scene_assert(all(line$width_mm >= 0 & line$mitre_limit >= 1) &&
                 all(line$cap %in% 0:2) && all(line$join %in% 0:2) &&
                 all(line$hidden %in% 0:2), "invalid polyline footprint or policy")
  fp <- packet$footprints
  fv <- packet$footprint_vertices
  scene_assert(all(fp$kind %in% 1:2) &&
                 all(fp$radius_x_mm >= 0 & fp$radius_y_mm >= 0) &&
                 all(fv$ring >= 1L), "invalid footprint geometry")
  polygon <- fp$kind == 2L
  scene_assert(all(fp$count[!polygon] == 0L & fp$offset[!polygon] == 1L) &&
                 all(fp$count[polygon] >= 3L) &&
                 all(fp$offset[polygon] >= 1L &
                       as.double(fp$offset[polygon]) + fp$count[polygon] - 1 <= length(fv$ring)),
               "invalid footprint ranges")
  for (i in which(polygon)) {
    runs <- rle(fv$ring[seq.int(fp$offset[i], length.out = fp$count[i])])
    scene_assert(all(runs$lengths >= 3L) && !anyDuplicated(runs$values),
                 "footprint rings must be contiguous with at least three vertices")
  }
  scene_assert(all(packet$markers$footprint_id >= 1L &
                   packet$markers$footprint_id <= length(fp$kind)),
               "invalid marker footprint_id")
  scene_assert(all(packet$text$width_mm >= 0 & packet$text$height_mm >= 0),
               "text measurements must be nonnegative")
  invisible(packet)
}

new_camera_3d <- function(projection = "orthographic", azimuth = -60,
                          elevation = 30, roll = 0, distance = NULL,
                          fov = 35, zoom = 1, target = c(0.5, 0.5, 0.5),
                          aspect = c(1, 1, 1), near = NULL, far = NULL,
                          clip_data = TRUE, clip = TRUE) {
  camera <- list(projection = projection, azimuth = as.double(azimuth),
                 elevation = as.double(elevation), roll = as.double(roll),
                 distance = if (is.null(distance)) {
                   if (identical(projection, "orthographic")) Inf else NA_real_
                 } else as.double(distance),
                 fov = as.double(fov), zoom = as.double(zoom),
                 target = as.double(target), aspect = as.double(aspect),
                 near = if (is.null(near)) NA_real_ else as.double(near),
                 far = if (is.null(far)) NA_real_ else as.double(far),
                 clip_data = clip_data, clip = clip)
  validate_camera_3d(camera)
  structure(camera, class = "ggplot3d_camera")
}

validate_camera_3d <- function(camera) {
  scene_assert(identical(names(camera), c("projection", "azimuth", "elevation",
    "roll", "distance", "fov", "zoom", "target", "aspect", "near", "far",
    "clip_data", "clip")), "invalid camera schema")
  scene_assert(length(camera$projection) == 1L &&
                 camera$projection %in% c("orthographic", "perspective"),
               "invalid camera projection")
  for (key in c("azimuth", "elevation", "roll", "fov", "zoom")) {
    scene_assert(is.double(camera[[key]]) && length(camera[[key]]) == 1L &&
                   is.finite(camera[[key]]), paste("invalid camera", key))
  }
  scene_assert(abs(camera$elevation) <= 90 && camera$fov > 0 &&
                 camera$fov < 180 && camera$zoom > 0, "invalid camera angles or zoom")
  for (key in c("target", "aspect")) {
    scene_assert(is.double(camera[[key]]) && length(camera[[key]]) == 3L &&
                   all(is.finite(camera[[key]])), paste("invalid camera", key))
  }
  scene_assert(all(camera$aspect > 0), "camera aspect must be positive")
  for (key in c("distance", "near", "far")) {
    value <- camera[[key]]
    scene_assert(is.double(value) && length(value) == 1L && !is.nan(value) &&
                   (is.na(value) || value > 0), paste("invalid camera", key))
  }
  if (camera$projection == "orthographic") {
    scene_assert(identical(camera$distance, Inf), "orthographic distance must be Inf")
  } else scene_assert(is.na(camera$distance) || is.finite(camera$distance),
                      "perspective distance must be finite")
  for (key in c("near", "far")) {
    scene_assert(is.na(camera[[key]]) || is.finite(camera[[key]]),
                 paste("camera", key, "must be finite"))
  }
  if (!is.na(camera$near) && !is.na(camera$far)) {
    scene_assert(camera$near < camera$far, "camera near must be less than far")
  }
  for (key in c("clip_data", "clip")) {
    scene_assert(is.logical(camera[[key]]) && length(camera[[key]]) == 1L &&
                   !anyNA(camera[[key]]), paste("invalid camera", key))
  }
  invisible(camera)
}

new_scene_viewport <- function(width_mm, height_mm, dpi = 144) {
  if (grid::is.unit(width_mm)) width_mm <- grid::convertWidth(width_mm, "mm", TRUE)
  if (grid::is.unit(height_mm)) height_mm <- grid::convertHeight(height_mm, "mm", TRUE)
  viewport <- list(width_mm = as.double(width_mm), height_mm = as.double(height_mm),
                   dpi = as.double(dpi))
  scene_assert(all(lengths(viewport) == 1L) &&
                 all(is.finite(unlist(viewport))) && all(unlist(viewport) > 0),
               "viewport dimensions and dpi must be positive and finite")
  viewport
}

scene_layer_grob <- function(packet) {
  validate_scene_packet(packet)
  n <- sum(vapply(packet[c("faces", "polylines", "markers", "text")],
                  function(x) length(x$primitive_id), integer(1)))
  if (n == 0L) return(ggplot2::zeroGrob())
  grid::gTree(packet = packet, cl = "scene_layer_grob")
}

scene_compile <- function(packet, camera = new_camera_3d(), viewport) {
  validate_scene_packet(packet)
  validate_camera_3d(camera)
  scene_assert(identical(names(viewport), c("width_mm", "height_mm", "dpi")),
               "invalid viewport schema")
  viewport <- do.call(new_scene_viewport, viewport)
  result <- scene_compile_cpp(packet[names(packet) != "styles"], camera, viewport)
  result$styles <- packet$styles
  result$source <- packet[names(packet) != "styles"]
  class(result) <- "ggplot3d_compiled_scene"
  result
}
