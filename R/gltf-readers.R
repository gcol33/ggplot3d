# A JSON text parsed into R values: objects become named lists, arrays of
# scalars vectors, other arrays unnamed lists, null NULL.
json_parse_3d <- function(text) {
  pattern <- "\"(?:[^\"\\\\]|\\\\.)*\"|-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?|true|false|null|[][{}:,]"
  if (grepl("[^[:space:]]", gsub(pattern, "", text, perl = TRUE))) stop("Invalid JSON.", call. = FALSE)
  tokens <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  at <- 0L
  expect <- function(token) {
    at <<- at + 1L
    if (at > length(tokens) || tokens[at] != token) stop("Invalid JSON: expected '", token, "'.", call. = FALSE)
  }
  value <- function() {
    at <<- at + 1L
    if (at > length(tokens)) stop("Invalid JSON: truncated.", call. = FALSE)
    token <- tokens[at]
    if (token == "{") {
      out <- structure(list(), names = character())
      if (tokens[at + 1L] == "}") { at <<- at + 1L; return(out) }
      repeat {
        at <<- at + 1L
        if (!startsWith(tokens[at], "\"")) stop("Invalid JSON: object keys must be strings.", call. = FALSE)
        key <- json_unescape_3d(tokens[at])
        expect(":")
        out[key] <- list(value())
        at <<- at + 1L
        if (tokens[at] == "}") return(out)
        if (tokens[at] != ",") stop("Invalid JSON: expected ',' or '}'.", call. = FALSE)
      }
    }
    if (token == "[") {
      out <- list()
      if (tokens[at + 1L] == "]") { at <<- at + 1L; return(out) }
      repeat {
        out[length(out) + 1L] <- list(value())
        at <<- at + 1L
        if (tokens[at] == "]") break
        if (tokens[at] != ",") stop("Invalid JSON: expected ',' or ']'.", call. = FALSE)
      }
      scalar <- vapply(out, function(x) is.atomic(x) && length(x) == 1L && is.null(names(x)), logical(1))
      if (all(scalar) && length(unique(vapply(out, typeof, ""))) == 1L) return(unlist(out))
      return(out)
    }
    if (token == "true") return(TRUE)
    if (token == "false") return(FALSE)
    if (token == "null") return(NULL)
    if (startsWith(token, "\"")) return(json_unescape_3d(token))
    if (token %in% c("}", "]", ":", ",")) stop("Invalid JSON: unexpected '", token, "'.", call. = FALSE)
    as.numeric(token)
  }
  out <- value()
  if (at != length(tokens)) stop("Invalid JSON: trailing content.", call. = FALSE)
  out
}

json_unescape_3d <- function(token) {
  s <- substr(token, 2L, nchar(token) - 1L)
  if (!grepl("\\", s, fixed = TRUE)) return(s)
  parts <- regmatches(s, gregexpr("\\\\(u[0-9a-fA-F]{4}|.)", s), invert = NA)[[1L]]
  escape <- substring(parts[c(FALSE, TRUE)], 2L)
  simple <- c("\"" = "\"", "\\" = "\\", "/" = "/", b = "\b", f = "\f", n = "\n", r = "\r", t = "\t")
  unicode <- nchar(escape) == 5L
  out <- unname(simple[escape])
  out[unicode] <- vapply(strtoi(substring(escape[unicode], 2L), 16L), intToUtf8, character(1))
  if (anyNA(out)) stop("Invalid JSON string escape.", call. = FALSE)
  parts[c(FALSE, TRUE)] <- out
  paste(parts, collapse = "")
}

# Byte width, signedness and normalisation divisor of glTF component types.
gltf_components_3d <- list(`5120` = list(1L, TRUE, 127), `5121` = list(1L, FALSE, 255), `5122` = list(2L, TRUE, 32767),
  `5123` = list(2L, FALSE, 65535), `5125` = list(4L, FALSE, 4294967295), `5126` = list(4L, NA, NA))
gltf_widths_3d <- c(SCALAR = 1L, VEC2 = 2L, VEC3 = 3L, VEC4 = 4L, MAT2 = 4L, MAT3 = 9L, MAT4 = 16L)

gltf_decode_3d <- function(bytes, component, n) {
  spec <- gltf_components_3d[[as.character(component)]]
  if (is.null(spec)) stop("Unknown glTF component type ", component, ".", call. = FALSE)
  if (component == 5126) return(readBin(bytes, "double", n = n, size = 4L, endian = "little"))
  if (component == 5125) {
    value <- readBin(bytes, "integer", n = n, size = 4L, endian = "little")
    return(ifelse(value < 0, value + 2^32, value))
  }
  readBin(bytes, "integer", n = n, size = spec[[1L]], signed = spec[[2L]], endian = "little")
}

# The raw document of a glTF or GLB file: the parsed JSON, its buffers and
# the directory that relative URIs resolve against.
gltf_document_3d <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  bin <- NULL; json <- NULL
  if (length(raw) >= 12L && identical(rawToChar(raw[1:4]), "glTF")) {
    word <- function(at) readBin(raw[at + 1:4], "integer", size = 4L, endian = "little")
    at <- 12L
    while (at + 8L <= length(raw)) {
      size <- word(at); type <- word(at + 4L)
      body <- raw[at + 8L + seq_len(size)]
      if (type == 0x4E4F534AL) json <- rawToChar(body) else if (type == 0x004E4942L) bin <- body
      at <- at + 8L + size
    }
    if (is.null(json)) stop("The GLB file has no JSON chunk.", call. = FALSE)
  } else json <- rawToChar(raw[raw != as.raw(0L)])
  doc <- json_parse_3d(sub("^\ufeff", "", json))
  version <- doc$asset$version %or3d% ""
  if (!startsWith(version, "2")) stop("Only glTF 2.0 files are supported.", call. = FALSE)
  doc$.buffers <- lapply(doc$buffers, function(b) {
    if (is.null(b$uri)) {
      if (is.null(bin)) stop("A glTF buffer without a URI needs a GLB binary chunk.", call. = FALSE)
      return(bin)
    }
    gltf_uri_bytes_3d(b$uri, dirname(path))
  })
  doc
}

gltf_uri_bytes_3d <- function(uri, directory) {
  if (startsWith(uri, "data:")) {
    if (!grepl(";base64,", uri, fixed = TRUE)) stop("glTF data URIs must be base64 encoded.", call. = FALSE)
    return(base64_decode_3d(sub("^data:[^,]*,", "", uri)))
  }
  file <- file.path(directory, utils::URLdecode(uri))
  if (!file.exists(file)) stop("glTF resource not found: ", file, call. = FALSE)
  readBin(file, "raw", file.size(file))
}

gltf_view_bytes_3d <- function(doc, view) {
  v <- doc$bufferViews[[view + 1L]]
  doc$.buffers[[v$buffer + 1L]][(v$byteOffset %or3d% 0) + seq_len(v$byteLength)]
}

# Accessor `index` as a matrix with one row per element, normalised integer
# components mapped to [0, 1] or [-1, 1] and sparse substitutions applied.
gltf_read_accessor_3d <- function(doc, index) {
  a <- doc$accessors[[index + 1L]]
  k <- gltf_widths_3d[[a$type]]; component <- a$componentType; count <- a$count
  size <- gltf_components_3d[[as.character(component)]][[1L]]
  packed <- function(view, offset, type, n) {
    v <- doc$bufferViews[[view + 1L]]
    start <- (v$byteOffset %or3d% 0) + (offset %or3d% 0)
    gltf_decode_3d(doc$.buffers[[v$buffer + 1L]][start + seq_len(n * gltf_components_3d[[as.character(type)]][[1L]])], type, n)
  }
  values <- if (is.null(a$bufferView)) numeric(count * k) else {
    v <- doc$bufferViews[[a$bufferView + 1L]]
    stride <- v$byteStride %or3d% (k * size)
    if (stride == k * size) packed(a$bufferView, a$byteOffset, component, count * k) else {
      start <- (v$byteOffset %or3d% 0) + (a$byteOffset %or3d% 0)
      bytes <- doc$.buffers[[v$buffer + 1L]][start + rep((seq_len(count) - 1) * stride, each = k * size) + seq_len(k * size)]
      gltf_decode_3d(bytes, component, count * k)
    }
  }
  if (!is.null(a$sparse)) {
    s <- a$sparse
    rows <- packed(s$indices$bufferView, s$indices$byteOffset, s$indices$componentType, s$count) + 1
    m <- matrix(values, k)
    m[, rows] <- matrix(packed(s$values$bufferView, s$values$byteOffset, component, s$count * k), k)
    values <- as.vector(m)
  }
  if (isTRUE(a$normalized)) {
    spec <- gltf_components_3d[[as.character(component)]]
    values <- if (spec[[2L]]) pmax(values / spec[[3L]], -1) else values / spec[[3L]]
  }
  matrix(values, ncol = k, byrow = TRUE)
}

# Local transform of a node: its matrix, or translation * rotation * scale.
gltf_node_matrix_3d <- function(node) {
  if (!is.null(node$matrix)) return(matrix(as.double(node$matrix), 4L))
  t <- node$translation %or3d% c(0, 0, 0); q <- node$rotation %or3d% c(0, 0, 0, 1); s <- node$scale %or3d% c(1, 1, 1)
  x <- q[1L]; y <- q[2L]; z <- q[3L]; w <- q[4L]
  r <- matrix(c(1 - 2 * (y^2 + z^2), 2 * (x * y + z * w), 2 * (x * z - y * w),
                2 * (x * y - z * w), 1 - 2 * (x^2 + z^2), 2 * (y * z + x * w),
                2 * (x * z + y * w), 2 * (y * z - x * w), 1 - 2 * (x^2 + y^2)), 3L)
  m <- diag(4)
  m[1:3, 1:3] <- r %*% diag(as.double(s), 3L)
  m[1:3, 4L] <- t
  m
}

# Mesh instances of the default scene: mesh index and world matrix.
gltf_instances_3d <- function(doc) {
  nodes <- doc$nodes
  if (!length(nodes)) return(lapply(seq_along(doc$meshes) - 1L, function(i) list(mesh = i, matrix = diag(4))))
  children <- unlist(lapply(nodes, `[[`, "children"))
  scene <- if (length(doc$scenes)) doc$scenes[[(doc$scene %or3d% 0) + 1L]] else NULL
  roots <- if (!is.null(scene)) scene$nodes else setdiff(seq_along(nodes) - 1L, children)
  out <- list()
  visit <- function(i, parent, depth) {
    if (depth > length(nodes)) stop("The glTF node hierarchy has a cycle.", call. = FALSE)
    node <- nodes[[i + 1L]]
    m <- parent %*% gltf_node_matrix_3d(node)
    if (!is.null(node$mesh)) out[[length(out) + 1L]] <<- list(mesh = node$mesh, matrix = m)
    for (child in node$children) visit(child, m, depth + 1L)
  }
  for (root in roots) visit(root, diag(4), 0L)
  out
}

# sRGB hex colours of linear RGB(A) rows.
linear_colours_3d <- function(rgba) {
  rgb <- srgb_encode_3d(pmin(pmax(rgba[, 1:3, drop = FALSE], 0), 1))
  channel_colours_3d(rgb[, 1L], rgb[, 2L], rgb[, 3L], if (ncol(rgba) == 4L) pmin(pmax(rgba[, 4L], 0), 1), max = 1)
}

gltf_image_3d <- function(doc, texture) {
  source <- doc$textures[[texture$index + 1L]]$source
  if (is.null(source)) stop("The glTF texture has no image source.", call. = FALSE)
  image <- doc$images[[source + 1L]]
  bytes <- if (!is.null(image$bufferView)) gltf_view_bytes_3d(doc, image$bufferView) else gltf_uri_bytes_3d(image$uri, doc$.directory)
  texture_native_3d(bytes)
}

# Geometry of every primitive of the default scene, merged: vertices, faces
# (1-based index vectors), vertex and face attributes, per-corner texture
# coordinates, per-face image indices with their images, and the points of
# point primitives.
gltf_scene_3d <- function(path) {
  doc <- gltf_document_3d(path)
  doc$.directory <- dirname(path)
  vertices <- list(); faces <- list(); colours <- list(); face_colours <- list(); face_material <- list()
  us <- list(); vs <- list(); face_image <- list(); points <- list()
  image_keys <- character(); images <- list(); offset <- 0L
  for (instance in gltf_instances_3d(doc)) {
    for (p in doc$meshes[[instance$mesh + 1L]]$primitives) {
      attributes <- p$attributes
      if (is.null(attributes$POSITION)) next
      position <- gltf_read_accessor_3d(doc, attributes$POSITION)
      world <- (cbind(position, 1) %*% t(instance$matrix))[, 1:3, drop = FALSE]
      material <- if (is.null(p$material)) NULL else doc$materials[[p$material + 1L]]
      pbr <- material$pbrMetallicRoughness
      factor <- as.double(pbr$baseColorFactor %or3d% c(1, 1, 1, 1))
      colour <- if (is.null(attributes$COLOR_0)) NULL else {
        c0 <- gltf_read_accessor_3d(doc, attributes$COLOR_0)
        if (ncol(c0) == 3L) c0 <- cbind(c0, 1)
        linear_colours_3d(sweep(c0, 2L, factor, `*`))
      }
      mode <- p$mode %or3d% 4
      index <- if (is.null(p$indices)) seq_len(nrow(position)) else gltf_read_accessor_3d(doc, p$indices)[, 1L] + 1
      if (mode == 0) {
        d <- data.frame(x = world[index, 1L], y = world[index, 2L], z = world[index, 3L])
        if (!is.null(colour)) d$colour <- colour[index]
        points[[length(points) + 1L]] <- d
        next
      }
      if (!mode %in% 4:6) next
      n <- length(index)
      tri <- switch(as.character(mode),
        `4` = matrix(index[seq_len(n - n %% 3L)], ncol = 3L, byrow = TRUE),
        `5` = if (n < 3L) matrix(0, 0L, 3L) else {
          i <- seq_len(n - 2L); odd <- i %% 2L == 1L
          cbind(ifelse(odd, index[i], index[i + 1L]), ifelse(odd, index[i + 1L], index[i]), index[i + 2L])
        },
        `6` = if (n < 3L) matrix(0, 0L, 3L) else cbind(index[1L], index[2:(n - 1L)], index[3:n]))
      tri <- tri[tri[, 1L] != tri[, 2L] & tri[, 2L] != tri[, 3L] & tri[, 1L] != tri[, 3L], , drop = FALSE]
      if (det(instance$matrix[1:3, 1:3]) < 0) tri <- tri[, c(1L, 3L, 2L), drop = FALSE]
      if (!nrow(tri)) next
      vertices[[length(vertices) + 1L]] <- world
      colours[[length(colours) + 1L]] <- colour %or3d% rep(NA_character_, nrow(world))
      faces[[length(faces) + 1L]] <- tri + offset
      face_colours[[length(face_colours) + 1L]] <- rep(if (is.null(colour)) linear_colours_3d(matrix(factor, 1L)) else NA_character_, nrow(tri))
      face_material[[length(face_material) + 1L]] <- rep(material$name %or3d% NA_character_, nrow(tri))
      texture <- pbr$baseColorTexture
      uv <- if (is.null(texture)) NULL else attributes[[paste0("TEXCOORD_", texture$texCoord %or3d% 0)]]
      image <- 0L
      if (!is.null(uv)) {
        key <- as.character(texture$index)
        if (!key %in% image_keys) { image_keys <- c(image_keys, key); images[[length(images) + 1L]] <- gltf_image_3d(doc, texture) }
        image <- match(key, image_keys)
        coords <- gltf_read_accessor_3d(doc, uv)
        corner <- as.vector(t(tri))
        us[[length(us) + 1L]] <- coords[corner, 1L]
        # glTF texture coordinates run down from the top of the image.
        vs[[length(vs) + 1L]] <- 1 - coords[corner, 2L]
      } else {
        us[[length(us) + 1L]] <- rep(NA_real_, 3L * nrow(tri)); vs[[length(vs) + 1L]] <- rep(NA_real_, 3L * nrow(tri))
      }
      face_image[[length(face_image) + 1L]] <- rep(image, nrow(tri))
      offset <- offset + nrow(world)
    }
  }
  out <- list(points = if (length(points)) do.call(rbind, points) else NULL)
  if (!length(faces)) return(c(out, list(faces = list())))
  xyz <- do.call(rbind, vertices)
  tri <- do.call(rbind, faces)
  colour <- unlist(colours); face_colour <- unlist(face_colours); material <- unlist(face_material)
  face_data <- data.frame(.face = seq_len(nrow(tri)))
  if (any(!is.na(material))) face_data$material <- material
  if (any(!is.na(face_colour))) face_data$face_colour <- face_colour
  c(out, list(vertices = data.frame(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L]),
    faces = unname(split(as.integer(t(tri)), rep(seq_len(nrow(tri)), each = 3L))),
    vertex_data = if (any(!is.na(colour))) data.frame(.vertex = seq_along(colour), vertex_colour = colour) else NULL,
    face_data = if (ncol(face_data) > 1L) face_data else NULL,
    u = unlist(us), v = unlist(vs), image = unlist(face_image), images = images))
}
