register_exporter_3d("glb", function(frames, file, options, fps) {
  write_gltf_3d(frames, file, options, fps, binary = TRUE)
}, animation = TRUE)

register_exporter_3d("gltf", function(frames, file, options, fps) {
  write_gltf_3d(frames, file, options, fps, binary = FALSE)
}, animation = TRUE)

# Writes captured scenes as one glTF 2.0 asset. One frame gives a still
# scene. Several frames give an animation at `fps`: the camera node carries
# translation and rotation tracks, and a node whose geometry differs between
# frames holds one child per distinct geometry, shown in its frames by a
# step track on scale.
write_gltf_3d <- function(frames, file, options, fps = NULL, binary = TRUE) {
  up <- frames[[1L]]$up
  entries <- list()
  for (i in seq_along(frames)) for (node in frames[[i]]$nodes) {
    prims <- export_mesh_3d(node, options)
    if (!length(prims)) next
    key <- paste(node$parent %or3d% "", node$name, sep = "/")
    entry <- entries[[key]] %or3d% list(name = node$name, parent = node$parent, variants = list())
    found <- 0L
    for (v in seq_along(entry$variants)) {
      if (identical(entry$variants[[v]]$prims, prims)) {
        found <- v
        break
      }
    }
    if (found) entry$variants[[found]]$frames <- c(entry$variants[[found]]$frames, i)
    else entry$variants[[length(entry$variants) + 1L]] <- list(prims = prims, frames = i)
    entries[[key]] <- entry
  }
  b <- gltf_builder_3d()
  n_frames <- length(frames)
  if (n_frames > 1L) b$times <- gltf_accessor_3d(b, (seq_len(n_frames) - 1) / fps, "SCALAR", bounds = TRUE)
  visibility <- function(node, shown) {
    if (n_frames == 1L || length(shown) == n_frames) return(node)
    on <- seq_len(n_frames) %in% shown
    gltf_channel_3d(b, length(b$nodes), "scale", cbind(on, on, on) * 1, "STEP")
    if (!on[1L]) node$scale <- c(0, 0, 0)
    node
  }
  add_node <- function(node) {
    b$nodes[[length(b$nodes) + 1L]] <- node
    length(b$nodes) - 1L
  }
  roots <- integer(); furniture <- integer()
  for (entry in entries) {
    if (length(entry$variants) == 1L) {
      variant <- entry$variants[[1L]]
      index <- add_node(visibility(list(name = entry$name, mesh = gltf_mesh_3d(b, variant$prims, entry$name, up)), variant$frames))
    } else {
      children <- vapply(entry$variants, function(variant) {
        name <- sprintf("%s_frame%d", entry$name, variant$frames[1L])
        add_node(visibility(list(name = name, mesh = gltf_mesh_3d(b, variant$prims, name, up)), variant$frames))
      }, integer(1))
      index <- add_node(list(name = entry$name, children = I(children)))
    }
    if (identical(entry$parent, "furniture")) furniture <- c(furniture, index) else roots <- c(roots, index)
  }
  if (length(furniture)) roots <- c(roots, add_node(list(name = "furniture", children = I(furniture))))
  roots <- c(roots, gltf_camera_node_3d(b, lapply(frames, `[[`, "camera"), up))
  scene <- list(asset = list(version = "2.0",
      generator = paste("ggplot3d", as.character(utils::packageVersion("ggplot3d")))),
    scene = 0L, scenes = list(list(nodes = I(roots))), nodes = b$nodes, cameras = b$cameras,
    meshes = b$meshes, materials = b$materials, accessors = b$accessors, bufferViews = b$views,
    buffers = list(list(byteLength = b$length)))
  if (length(b$channels)) scene$animations <- list(list(name = "ggplot3d", samplers = b$samplers, channels = b$channels))
  if (length(b$extensions)) scene$extensionsUsed <- I(b$extensions)
  scene <- scene[lengths(scene) > 0L]
  bin <- if (length(b$chunks)) do.call(c, b$chunks) else raw()
  if (binary) {
    gltf_write_glb_3d(json_3d(scene), bin, file)
  } else {
    buffer <- sub("\\.gltf$", ".bin", file, ignore.case = TRUE)
    scene$buffers[[1L]]$uri <- basename(buffer)
    writeBin(bin, buffer)
    writeLines(json_3d(scene), file, useBytes = TRUE)
  }
  invisible(file)
}

gltf_builder_3d <- function() {
  b <- new.env(parent = emptyenv())
  b$chunks <- list(); b$length <- 0
  b$views <- list(); b$accessors <- list(); b$meshes <- list(); b$materials <- list()
  b$material_index <- list(); b$nodes <- list(); b$cameras <- list()
  b$samplers <- list(); b$channels <- list(); b$extensions <- character(); b$times <- NULL
  b
}

# Appends values to the binary buffer as float32 (doubles) or uint32
# (integers) in its own buffer view and returns the accessor index. Rows of a
# matrix are elements; `bounds` records the float32 component range.
gltf_accessor_3d <- function(b, values, type, target = NULL, bounds = FALSE) {
  integer <- is.integer(values)
  flat <- if (is.matrix(values)) as.vector(t(values)) else as.vector(values)
  bytes <- writeBin(if (integer) as.integer(flat) else as.double(flat), raw(), size = 4L, endian = "little")
  view <- list(buffer = 0L, byteOffset = b$length, byteLength = length(bytes))
  if (!is.null(target)) view$target <- as.integer(target)
  b$chunks[[length(b$chunks) + 1L]] <- bytes
  b$length <- b$length + length(bytes)
  b$views[[length(b$views) + 1L]] <- view
  width <- c(SCALAR = 1L, VEC3 = 3L, VEC4 = 4L)[[type]]
  accessor <- list(bufferView = length(b$views) - 1L, componentType = if (integer) 5125L else 5126L,
    count = length(flat) %/% width, type = type)
  if (bounds) {
    stored <- matrix(readBin(bytes, "double", n = length(flat), size = 4L, endian = "little"), ncol = width, byrow = TRUE)
    accessor$min <- I(apply(stored, 2L, min)); accessor$max <- I(apply(stored, 2L, max))
  }
  b$accessors[[length(b$accessors) + 1L]] <- accessor
  length(b$accessors) - 1L
}

gltf_material_3d <- function(b, lit, blend) {
  key <- paste(lit, blend)
  if (!is.null(b$material_index[[key]])) return(b$material_index[[key]])
  material <- list(name = paste0(if (lit) "lit" else "unlit", if (blend) "_translucent" else ""),
    pbrMetallicRoughness = list(baseColorFactor = c(1, 1, 1, 1), metallicFactor = 0, roughnessFactor = 1),
    alphaMode = if (blend) "BLEND" else "OPAQUE", doubleSided = TRUE)
  if (!lit) {
    material$extensions <- list(KHR_materials_unlit = json_object_3d())
    b$extensions <- union(b$extensions, "KHR_materials_unlit")
  }
  b$materials[[length(b$materials) + 1L]] <- material
  b$material_index[[key]] <- length(b$materials) - 1L
  b$material_index[[key]]
}

srgb_to_linear_3d <- function(x) ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)

gltf_mesh_3d <- function(b, prims, name, up) {
  primitives <- lapply(prims, function(p) {
    attributes <- list(POSITION = gltf_accessor_3d(b, export_axes_3d(p$position, up, "y"), "VEC3", 34962L, bounds = TRUE))
    if (!is.null(p$normal)) attributes$NORMAL <- gltf_accessor_3d(b, export_axes_3d(p$normal, up, "y"), "VEC3", 34962L)
    attributes$COLOR_0 <- gltf_accessor_3d(b, cbind(srgb_to_linear_3d(p$colour[, 1:3, drop = FALSE]), p$colour[, 4L]), "VEC4", 34962L)
    list(attributes = attributes, indices = gltf_accessor_3d(b, as.integer(p$index) - 1L, "SCALAR", 34963L),
      material = gltf_material_3d(b, p$lit, any(p$colour[, 4L] < 1)),
      mode = c(points = 0L, lines = 1L, triangles = 4L)[[p$mode]])
  })
  b$meshes[[length(b$meshes) + 1L]] <- list(name = name, primitives = primitives)
  length(b$meshes) - 1L
}

gltf_channel_3d <- function(b, node, path, values, interpolation, pointer = NULL) {
  type <- if (is.matrix(values)) c("", "", "VEC3", "VEC4")[ncol(values)] else "SCALAR"
  b$samplers[[length(b$samplers) + 1L]] <- list(input = b$times,
    output = gltf_accessor_3d(b, values, type), interpolation = interpolation)
  target <- if (is.null(pointer)) list(node = as.integer(node), path = path) else
    list(path = "pointer", extensions = list(KHR_animation_pointer = list(pointer = pointer)))
  if (!is.null(pointer)) b$extensions <- union(b$extensions, "KHR_animation_pointer")
  b$channels[[length(b$channels) + 1L]] <- list(sampler = length(b$samplers) - 1L, target = target)
}

# The camera looks along its local -z with +y up; its node turns the local
# axes onto the camera's right, up and backward directions.
gltf_camera_pose_3d <- function(camera, up) {
  basis <- export_axes_3d(rbind(camera$right, camera$up, -camera$forward), up, "y")
  list(translation = drop(export_axes_3d(matrix(camera$eye, 1L), up, "y")),
    rotation = quaternion_3d(t(basis)))
}

quaternion_3d <- function(m) {
  trace <- m[1L, 1L] + m[2L, 2L] + m[3L, 3L]
  q <- if (trace > 0) {
    s <- sqrt(trace + 1) * 2
    c((m[3L, 2L] - m[2L, 3L]) / s, (m[1L, 3L] - m[3L, 1L]) / s, (m[2L, 1L] - m[1L, 2L]) / s, s / 4)
  } else if (m[1L, 1L] > m[2L, 2L] && m[1L, 1L] > m[3L, 3L]) {
    s <- sqrt(1 + m[1L, 1L] - m[2L, 2L] - m[3L, 3L]) * 2
    c(s / 4, (m[1L, 2L] + m[2L, 1L]) / s, (m[1L, 3L] + m[3L, 1L]) / s, (m[3L, 2L] - m[2L, 3L]) / s)
  } else if (m[2L, 2L] > m[3L, 3L]) {
    s <- sqrt(1 + m[2L, 2L] - m[1L, 1L] - m[3L, 3L]) * 2
    c((m[1L, 2L] + m[2L, 1L]) / s, s / 4, (m[2L, 3L] + m[3L, 2L]) / s, (m[1L, 3L] - m[3L, 1L]) / s)
  } else {
    s <- sqrt(1 + m[3L, 3L] - m[1L, 1L] - m[2L, 2L]) * 2
    c((m[1L, 3L] + m[3L, 1L]) / s, (m[2L, 3L] + m[3L, 2L]) / s, s / 4, (m[2L, 1L] - m[1L, 2L]) / s)
  }
  q / sqrt(sum(q^2))
}

gltf_projection_3d <- function(camera) {
  if (camera$projection == "perspective") {
    list(type = "perspective", perspective = list(aspectRatio = camera$aspect, yfov = camera$yfov,
      znear = camera$near, zfar = camera$far))
  } else {
    list(type = "orthographic", orthographic = list(xmag = camera$ymag * camera$aspect, ymag = camera$ymag,
      znear = camera$near, zfar = camera$far))
  }
}

gltf_camera_node_3d <- function(b, cameras, up) {
  first <- cameras[[1L]]
  scene_assert(all(vapply(cameras, `[[`, character(1), "projection") == first$projection),
    "glTF animation requires one projection type; export each projection separately")
  b$cameras <- list(gltf_projection_3d(first))
  poses <- lapply(cameras, gltf_camera_pose_3d, up)
  translation <- do.call(rbind, lapply(poses, `[[`, "translation"))
  rotation <- do.call(rbind, lapply(poses, `[[`, "rotation"))
  for (i in seq_len(nrow(rotation))[-1L]) {
    if (sum(rotation[i, ] * rotation[i - 1L, ]) < 0) rotation[i, ] <- -rotation[i, ]
  }
  node <- length(b$nodes)
  b$nodes[[node + 1L]] <- list(name = "camera", camera = 0L, translation = translation[1L, ], rotation = rotation[1L, ])
  if (length(cameras) > 1L) {
    varies <- function(x) max(abs(sweep(as.matrix(x), 2L, as.matrix(x)[1L, ]))) > 1e-9
    if (varies(translation)) gltf_channel_3d(b, node, "translation", translation, "LINEAR")
    if (varies(rotation)) gltf_channel_3d(b, node, "rotation", rotation, "LINEAR")
    properties <- if (first$projection == "perspective") list(yfov = function(camera) camera$yfov) else
      list(ymag = function(camera) camera$ymag, xmag = function(camera) camera$ymag * camera$aspect)
    properties <- c(properties, list(znear = function(camera) camera$near, zfar = function(camera) camera$far))
    for (name in names(properties)) {
      values <- vapply(cameras, properties[[name]], numeric(1))
      if (varies(values)) gltf_channel_3d(b, node, NULL, values, "LINEAR",
        pointer = paste0("/cameras/0/", first$projection, "/", name))
    }
  }
  node
}

# GLB container: a 12-byte header, then a JSON chunk padded with spaces and a
# binary chunk padded with zeros, both to four bytes.
gltf_write_glb_3d <- function(json, bin, file) {
  json <- charToRaw(enc2utf8(json))
  json <- c(json, rep(as.raw(0x20), (4L - length(json) %% 4L) %% 4L))
  bin <- c(bin, raw((4L - length(bin) %% 4L) %% 4L))
  word <- function(x) writeBin(as.integer(x), raw(), size = 4L, endian = "little")
  total <- 12L + 8L + length(json) + if (length(bin)) 8L + length(bin) else 0L
  out <- c(word(c(0x46546C67, 2L, total)), word(c(length(json), 0x4E4F534A)), json,
    if (length(bin)) c(word(c(length(bin), 0x004E4942)), bin))
  writeBin(out, file)
}

json_object_3d <- function() structure(list(), class = "json_object_3d")

# Minimal JSON encoder: named lists are objects, unnamed lists arrays, and
# atomic vectors scalars when of length one unless wrapped in I().
json_3d <- function(x) {
  if (inherits(x, "json_object_3d")) return("{}")
  if (is.null(x)) return("null")
  if (is.list(x)) {
    values <- vapply(x, json_3d, character(1), USE.NAMES = FALSE)
    if (!is.null(names(x))) return(paste0("{", paste0(json_string_3d(names(x)), ":", values, collapse = ","), "}"))
    return(paste0("[", paste(values, collapse = ","), "]"))
  }
  values <- if (is.character(x)) json_string_3d(x) else if (is.logical(x)) ifelse(x, "true", "false") else {
    scene_assert(all(is.finite(x)), "JSON numbers must be finite")
    if (is.integer(x)) as.character(x) else
      ifelse(x == round(x) & abs(x) < 1e15, sprintf("%.0f", x), trimws(formatC(x, digits = 9L, format = "g")))
  }
  if (length(x) != 1L || inherits(x, "AsIs")) paste0("[", paste(values, collapse = ","), "]") else values
}

json_string_3d <- function(x) {
  x <- gsub("\\", "\\\\", enc2utf8(x), fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  paste0("\"", x, "\"")
}
