#' Read a triangle or polygon mesh file
#'
#' Reads Wavefront OBJ, STL, PLY and glTF 2.0 (`.gltf`, `.glb`) files into a
#' [mesh_3d()] corner table. The reader is chosen by file extension from a
#' registry of native parsers. Scans from phone and tablet LiDAR apps
#' (Polycam, Scaniverse, 3d Scanner App) export these formats; point clouds
#' without faces are read by [read_points_3d()].
#'
#' * OBJ (ASCII): `v` vertices (with optional red, green and blue vertex colours), `vt`
#'   texture coordinates and `f` faces of any size, with `v/vt/vn`
#'   references, negative (relative) indices and `\` line continuation;
#'   `o` and `g` names become the face attribute `group`, `usemtl` names the
#'   face attribute `material`. Materials from `mtllib` files give the face
#'   attribute `face_colour` (from `Kd` and `d`) and the texture image
#'   (`map_Kd`). Other statements are ignored.
#' * STL (ASCII or binary): every facet is a triangle; vertices with exactly
#'   equal coordinates are welded. A file whose size is 84 bytes plus 50 bytes
#'   per triangle counted in its header is read as binary, whatever its first
#'   bytes say.
#' * PLY (ASCII, binary little- or big-endian): header-driven elements and
#'   properties of every scalar type; `vertex` x, y, z and `face`
#'   `vertex_indices` (or `vertex_index`). Vertex `red`, `green`, `blue` (and
#'   `alpha`) become the vertex attribute `vertex_colour`, face colours the
#'   face attribute `face_colour`; other scalar vertex properties are kept as
#'   vertex attributes. Face `texcoord` lists (six numbers per triangle) give
#'   texture coordinates; the image named by a `comment TextureFile` header
#'   line is the texture.
#' * glTF 2.0 (JSON `.gltf` with external or embedded buffers, binary
#'   `.glb`): triangle, strip and fan primitives of every mesh in the default
#'   scene, placed by their node transforms. `COLOR_0` (times the base colour
#'   factor) becomes `vertex_colour`, the base colour factor alone
#'   `face_colour`, the material name `material`; `TEXCOORD_n` and the base
#'   colour texture give the texture. glTF colours are linear and are encoded
#'   to sRGB.
#'
#' A textured mesh carries its image as the attribute `"texture"` (a
#' [texture_3d()] with extent `c(0, 1, 0, 1)`) and its texture coordinates as
#' `.texture_u` and `.texture_v` corner columns; [geom_mesh_3d()] drapes it
#' when the mesh is the layer's data. Several images are packed into one
#' atlas, one above the other, and their texture coordinates are clamped to
#' the unit square. PNG textures need png, JPEG textures jpeg.
#'
#' glTF positions are y-up by specification, and most scanning apps export
#' y-up OBJ and PLY files as well; draw them with `coord_3d(up = "y")`.
#' @param path Path to a `.obj`, `.stl`, `.ply`, `.gltf` or `.glb` file.
#' @return A keyed corner data frame of class ggplot3d_mesh.
#' @references Wavefront OBJ and MTL: <https://paulbourke.net/dataformats/obj/>,
#'   <https://paulbourke.net/dataformats/mtl/>.
#'   PLY (Turk): <https://paulbourke.net/dataformats/ply/>.
#'   STL: <https://en.wikipedia.org/wiki/STL_(file_format)>.
#'   glTF 2.0 (Khronos Group): <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html>.
#' @seealso [read_points_3d()] for point clouds, [geom_mesh_3d()].
#' @export
#' @examples
#' path <- tempfile(fileext = ".obj")
#' writeLines(c("v 0 0 0", "v 1 0 0", "v 1 1 0", "v 0 1 0", "f 1 2 3 4"), path)
#' m <- read_mesh_3d(path)
#' ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
read_mesh_3d <- function(path) {
  reader_dispatch_3d(path, mesh_readers_3d, "mesh")
}

reader_dispatch_3d <- function(path, readers, what) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) stop("path must name an existing file.", call. = FALSE)
  extension <- if (grepl(".", basename(path), fixed = TRUE)) tolower(sub(".*\\.", "", basename(path))) else ""
  reader <- readers[[extension]]
  if (is.null(reader)) stop("No ", what, " reader for '.", extension, "' files; supported: ",
    paste0(".", names(readers), collapse = ", "), ".", call. = FALSE)
  reader(path)
}

mesh_faces_3d <- function(faces) {
  sizes <- lengths(faces)
  if (any(sizes < 3L)) stop("Mesh faces need at least three vertices.", call. = FALSE)
  data.frame(.face = rep(seq_along(faces), sizes), .corner = sequence(sizes), .vertex = unlist(faces, use.names = FALSE))
}

# A mesh from vertex positions and faces (a list of 1-based vertex index
# vectors), with optional vertex and face attribute tables, per-corner
# texture coordinates (`u`, `v` in corner order, NA for none) and per-face
# texture images (`image`: 0 for none, else an index into `images`, a list of
# nativeRasters).
mesh_from_faces_3d <- function(vertices, faces, vertex_data = NULL, face_data = NULL, u = NULL, v = NULL,
                             image = NULL, images = list()) {
  corners <- mesh_faces_3d(faces)
  texture <- NULL
  if (length(images) && any(image > 0L)) {
    atlas <- texture_atlas_3d(images)
    corner_image <- rep(image, lengths(faces))
    use <- corner_image > 0L & is.finite(u) & is.finite(v)
    uv <- atlas$map(u[use], v[use], corner_image[use])
    corners$.texture_u <- NA_real_; corners$.texture_v <- NA_real_
    corners$.texture_u[use] <- uv$u; corners$.texture_v[use] <- uv$v
    texture <- texture_3d(atlas$image, extent = c(0, 1, 0, 1))
  }
  mesh <- mesh_3d(vertices, corners, vertex_data = vertex_data, face_data = face_data)
  attr(mesh, "texture") <- texture
  mesh
}

# Images stacked top to bottom into one nativeRaster, with the map from an
# image's texture coordinates (v up from its bottom edge) to the atlas's.
# With several images the coordinates are clamped to the unit square.
texture_atlas_3d <- function(images) {
  widths <- vapply(images, ncol, integer(1)); heights <- vapply(images, nrow, integer(1))
  if (length(images) == 1L) return(list(image = images[[1L]], map = function(u, v, k) list(u = u, v = v)))
  width <- max(widths); height <- sum(heights); top <- c(0L, cumsum(heights))[seq_along(images)]
  # A nativeRaster stores pixels row by row, so a width x height matrix view
  # indexes it by column, then row.
  atlas <- matrix(0L, width, height)
  for (k in seq_along(images)) atlas[seq_len(widths[k]), top[k] + seq_len(heights[k])] <- matrix(unclass(images[[k]]), widths[k])
  clamp <- function(x) pmin(pmax(x, 0), 1)
  list(image = structure(as.vector(atlas), dim = c(height, width), class = "nativeRaster", channels = 4L),
    map = function(u, v, k) list(u = clamp(u) * widths[k] / width,
      v = (height - top[k] - heights[k] + clamp(v) * heights[k]) / height))
}

# sRGB hex colours from channel vectors in [0, 1] or [0, 255] (judged by the
# maximum), with alpha when given.
channel_colours_3d <- function(red, green, blue, alpha = NULL, max = NULL) {
  max <- max %or3d% if (max(c(red, green, blue, alpha), 0, na.rm = TRUE) <= 1) 1 else 255
  if (is.null(alpha)) grDevices::rgb(red, green, blue, maxColorValue = max)
  else grDevices::rgb(red, green, blue, alpha, maxColorValue = max)
}

# Statements of a line-oriented text format (OBJ, MTL): comments removed,
# backslash continuations joined, split into keyword and the rest.
text_statements_3d <- function(path) {
  lines <- sub("#.*$", "", readLines(path, warn = FALSE))
  continued <- grepl("\\\\[[:space:]]*$", lines)
  if (any(continued)) {
    statement <- cumsum(c(TRUE, !continued[-length(continued)]))
    lines <- vapply(split(sub("\\\\[[:space:]]*$", "", lines), statement), paste, character(1), collapse = " ")
  }
  lines <- trimws(lines)
  keyword <- sub("[[:space:]].*$", "", lines)
  list(keyword = keyword, rest = gsub("[[:space:]]+", " ", trimws(substring(lines, nchar(keyword) + 1L))))
}

# Rows of whitespace-separated numbers: the values and the count per row.
number_rows_3d <- function(text) {
  tokens <- strsplit(text, " ", fixed = TRUE)
  list(values = suppressWarnings(as.double(unlist(tokens, use.names = FALSE))), sizes = lengths(tokens),
    start = cumsum(c(0L, lengths(tokens)))[seq_along(tokens)])
}

read_obj_3d <- function(path) {
  s <- text_statements_3d(path)
  v <- number_rows_3d(s$rest[s$keyword == "v"])
  if (!length(v$sizes)) stop("The OBJ file has no vertices.", call. = FALSE)
  xyz <- vapply(1:3, function(k) v$values[v$start + k], numeric(length(v$start)))
  if (!is.matrix(xyz)) xyz <- matrix(xyz, 1L)
  if (any(v$sizes < 3L) || anyNA(xyz)) stop("OBJ vertices need three numeric coordinates.", call. = FALSE)
  vertex_data <- NULL
  if (all(v$sizes == 6L | v$sizes == 7L)) {
    first <- v$start + v$sizes - 2L
    rgb <- vapply(0:2, function(k) v$values[first + k], numeric(length(first)))
    if (!is.matrix(rgb)) rgb <- matrix(rgb, 1L)
    if (!anyNA(rgb)) vertex_data <- data.frame(.vertex = seq_len(nrow(xyz)),
      vertex_colour = channel_colours_3d(rgb[, 1L], rgb[, 2L], rgb[, 3L]))
  }
  vt <- number_rows_3d(s$rest[s$keyword == "vt"])
  uv <- cbind(vt$values[vt$start + 1L], ifelse(vt$sizes >= 2L, vt$values[vt$start + 2L], 0))
  at <- which(s$keyword == "f")
  if (!length(at)) stop("The OBJ file has no faces.", call. = FALSE)
  tokens <- strsplit(s$rest[at], " ", fixed = TRUE)
  sizes <- lengths(tokens); token <- unlist(tokens, use.names = FALSE)
  if (any(sizes < 3L)) stop("Mesh faces need at least three vertices.", call. = FALSE)
  relative <- function(index, kind, count, what) {
    seen <- rep(cumsum(s$keyword == kind)[at], sizes)
    index <- ifelse(!is.na(index) & index < 0L, seen + 1L + index, index)
    if (any(!is.na(index) & (index < 1L | index > count))) stop("OBJ face indices reference missing ", what, ".", call. = FALSE)
    index
  }
  vertex <- suppressWarnings(as.integer(sub("/.*$", "", token)))
  if (anyNA(vertex) || any(vertex == 0L)) stop("OBJ face indices must be nonzero integers.", call. = FALSE)
  vertex <- relative(vertex, "v", nrow(xyz), "vertices")
  texel <- suppressWarnings(as.integer(ifelse(grepl("/", token, fixed = TRUE), sub("^[^/]*/([^/]*).*$", "\\1", token), NA)))
  texel <- relative(texel, "vt", nrow(uv), "texture coordinates")
  # The name set by the last g/o (or usemtl) statement before each face.
  current <- function(keys) {
    marks <- which(s$keyword %in% keys)
    index <- findInterval(at, marks)
    out <- rep(NA_character_, length(at)); out[index > 0L] <- s$rest[marks[index]]
    out
  }
  group <- current(c("g", "o")); material <- current("usemtl")
  faces <- split(vertex, rep(seq_along(at), sizes))
  names(faces) <- NULL
  face_data <- data.frame(.face = seq_along(at))
  if (any(!is.na(group))) face_data$group <- group
  if (any(!is.na(material))) face_data$material <- material
  libraries <- unlist(strsplit(s$rest[s$keyword == "mtllib"], " ", fixed = TRUE))
  mtl <- obj_materials_3d(file.path(dirname(path), libraries))
  image <- integer(length(at)); images <- list()
  if (length(mtl) && any(!is.na(material))) {
    entry <- mtl[match(material, names(mtl))]
    colour <- vapply(entry, function(m) if (is.null(m) || is.null(m$kd)) NA_character_ else
      channel_colours_3d(m$kd[1L], m$kd[2L], m$kd[3L], m$d, max = 1), character(1))
    if (any(!is.na(colour))) face_data$face_colour <- colour
    maps <- vapply(entry, function(m) if (is.null(m) || is.null(m$map)) NA_character_ else m$map, character(1))
    files <- unique(maps[!is.na(maps)])
    if (length(files)) {
      images <- lapply(files, texture_native_3d)
      image <- match(maps, files, nomatch = 0L)
    }
  }
  if (ncol(face_data) == 1L) face_data <- NULL
  mesh_from_faces_3d(data.frame(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L]), faces, vertex_data, face_data,
    u = uv[texel, 1L], v = uv[texel, 2L], image = image, images = images)
}

# Materials of MTL files by name: diffuse colour Kd, dissolve d (or 1 - Tr)
# and the diffuse texture map_Kd resolved against the file's directory.
obj_materials_3d <- function(files) {
  missing <- files[!file.exists(files)]
  if (length(missing)) warning("Material library not found: ", paste(basename(missing), collapse = ", "), ".", call. = FALSE)
  out <- list()
  for (file in files[file.exists(files)]) {
    s <- text_statements_3d(file)
    name <- NULL
    for (i in seq_along(s$keyword)) {
      key <- s$keyword[i]; rest <- s$rest[i]
      if (key == "newmtl") { name <- rest; out[[name]] <- list(kd = NULL, d = 1, map = NULL); next }
      if (is.null(name)) next
      last <- sub("^.* ", "", rest)
      if (key == "Kd") out[[name]]$kd <- as.double(strsplit(rest, " ", fixed = TRUE)[[1L]][1:3])
      else if (key == "d") out[[name]]$d <- as.double(last)
      else if (key == "Tr") out[[name]]$d <- 1 - as.double(last)
      else if (key == "map_Kd") out[[name]]$map <- texture_file_3d(rest, dirname(file))
    }
  }
  out
}

# The image file of a texture statement: its last word, or the whole
# statement for a name with spaces, relative to `directory`.
texture_file_3d <- function(statement, directory) {
  statement <- gsub("\\", "/", statement, fixed = TRUE)
  candidates <- file.path(directory, c(sub("^.* ", "", statement), statement))
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Texture image not found: ", candidates[1L], call. = FALSE)
  found[1L]
}

read_stl_3d <- function(path) {
  size <- file.size(path)
  con <- file(path, "rb"); on.exit(close(con))
  header <- readBin(con, "raw", 84L)
  count <- if (length(header) == 84L) readBin(header[81:84], "integer", size = 4L, endian = "little") else -1L
  if (length(header) == 84L && count >= 0L && size == 84 + 50 * count) {
    body <- readBin(con, "raw", 50L * count)
    record <- matrix(body, nrow = 50L)
    # Bytes 13-48 of each 50-byte record hold the three vertices as float32.
    floats <- readBin(as.vector(record[13:48, , drop = FALSE]), "double", size = 4L, n = 9L * count, endian = "little")
    xyz <- matrix(floats, ncol = 3L, byrow = TRUE)
  } else {
    words <- scan(path, what = character(), quiet = TRUE)
    at <- which(words == "vertex")
    if (!length(at) || length(at) %% 3L) stop("The ASCII STL file must hold whole triangles.", call. = FALSE)
    xyz <- cbind(as.double(words[at + 1L]), as.double(words[at + 2L]), as.double(words[at + 3L]))
    if (anyNA(xyz)) stop("STL vertices need three numeric coordinates.", call. = FALSE)
  }
  key <- paste(xyz[, 1L], xyz[, 2L], xyz[, 3L], sep = "\r")
  unique_key <- unique(key)
  index <- match(key, unique_key)
  first <- match(unique_key, key)
  mesh_3d(data.frame(x = xyz[first, 1L], y = xyz[first, 2L], z = xyz[first, 3L]), matrix(index, ncol = 3L, byrow = TRUE))
}

ply_types_3d <- list(
  char = list("integer", 1L, TRUE), int8 = list("integer", 1L, TRUE),
  uchar = list("integer", 1L, FALSE), uint8 = list("integer", 1L, FALSE),
  short = list("integer", 2L, TRUE), int16 = list("integer", 2L, TRUE),
  ushort = list("integer", 2L, FALSE), uint16 = list("integer", 2L, FALSE),
  int = list("integer", 4L, TRUE), int32 = list("integer", 4L, TRUE),
  uint = list("integer", 4L, FALSE), uint32 = list("integer", 4L, FALSE),
  float = list("double", 4L, TRUE), float32 = list("double", 4L, TRUE),
  double = list("double", 8L, TRUE), float64 = list("double", 8L, TRUE))

ply_decode_3d <- function(bytes, type, endian) {
  spec <- ply_types_3d[[type]]
  n <- length(bytes) %/% spec[[2L]]
  if (spec[[1L]] == "integer" && spec[[2L]] == 4L && !spec[[3L]]) {
    value <- readBin(bytes, "integer", n = n, size = 4L, endian = endian)
    return(ifelse(value < 0, value + 2^32, value))
  }
  readBin(bytes, spec[[1L]], n = n, size = spec[[2L]], signed = spec[[2L]] >= 4L || spec[[3L]], endian = endian)
}

# The elements of a PLY file: per element a named list of its properties,
# numeric vectors for scalars and lists of numeric vectors for lists, plus
# the header comments.
ply_elements_3d <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  stop_at <- grepRaw("end_header\r?\n", raw)
  if (!length(stop_at) || !identical(rawToChar(raw[1:3]), "ply")) stop("Not a PLY file.", call. = FALSE)
  header_end <- stop_at + length(grepRaw("end_header\r?\n", raw, value = TRUE)) - 1L
  header <- strsplit(trimws(strsplit(rawToChar(raw[seq_len(header_end)]), "\r?\n")[[1L]]), "[[:space:]]+")
  format <- NULL; elements <- list(); comments <- character()
  for (line in header) {
    if (line[1L] == "format") format <- line[2L]
    else if (line[1L] %in% c("comment", "obj_info")) comments <- c(comments, paste(line[-1L], collapse = " "))
    else if (line[1L] == "element") elements[[length(elements) + 1L]] <- list(name = line[2L], count = as.numeric(line[3L]), properties = list())
    else if (line[1L] == "property") {
      e <- length(elements)
      property <- if (line[2L] == "list") list(name = line[5L], count = line[3L], type = line[4L]) else list(name = line[3L], type = line[2L])
      if (!all(c(property$type, property$count) %in% names(ply_types_3d))) stop("Unknown PLY property type.", call. = FALSE)
      elements[[e]]$properties[[length(elements[[e]]$properties) + 1L]] <- property
    }
  }
  if (is.null(format) || !format %in% c("ascii", "binary_little_endian", "binary_big_endian")) stop("Unknown PLY format.", call. = FALSE)
  body <- raw[-seq_len(header_end)]
  ascii <- format == "ascii"
  endian <- if (format == "binary_big_endian") "big" else "little"
  tokens <- if (ascii) as.double(scan(text = rawToChar(body), what = character(), quiet = TRUE)) else NULL
  # Cells of a value: tokens for ASCII, bytes for binary.
  cells <- function(type) if (ascii) 1L else ply_types_3d[[type]][[2L]]
  read <- function(at, n, type) if (ascii) tokens[at + seq_len(n)] else ply_decode_3d(body[at + seq_len(n * cells(type))], type, endian)
  at <- 0
  values <- list()
  for (element in elements) {
    p <- element$properties; m <- element$count
    property_names <- vapply(p, `[[`, "", "name")
    is_list <- vapply(p, function(q) !is.null(q$count), logical(1))
    # Rows of equal layout (all scalar, or the same length of every list as
    # in the first row) are read as one block.
    first <- at; list_length <- integer(length(p))
    for (k in seq_along(p)) {
      if (is_list[k]) { list_length[k] <- read(first, 1L, p[[k]]$count); first <- first + cells(p[[k]]$count) + list_length[k] * cells(p[[k]]$type) }
      else first <- first + cells(p[[k]]$type)
    }
    width <- first - at
    offsets <- c(0, cumsum(vapply(seq_along(p), function(k) if (is_list[k]) cells(p[[k]]$count) + list_length[k] * cells(p[[k]]$type) else cells(p[[k]]$type), numeric(1))))
    block <- m > 0 && is.finite(width) && at + width * m <= (if (ascii) length(tokens) else length(body))
    if (block && any(is_list)) for (k in which(is_list)) {
      counts <- if (ascii) tokens[at + offsets[k] + 1 + width * (seq_len(m) - 1)]
        else ply_decode_3d(as.vector(matrix(body[at + seq_len(width * m)], width)[offsets[k] + seq_len(cells(p[[k]]$count)), , drop = FALSE]), p[[k]]$count, endian)
      if (any(counts != list_length[k])) { block <- FALSE; break }
    }
    if (block) {
      record <- if (ascii) matrix(tokens[at + seq_len(width * m)], width) else matrix(body[at + seq_len(width * m)], width)
      column <- function(start, n, type) {
        part <- record[start + seq_len(n), , drop = FALSE]
        if (ascii) as.vector(part) else ply_decode_3d(as.vector(part), type, endian)
      }
      out <- lapply(seq_along(p), function(k) {
        if (!is_list[k]) return(column(offsets[k], cells(p[[k]]$type), p[[k]]$type))
        flat <- column(offsets[k] + cells(p[[k]]$count), list_length[k] * cells(p[[k]]$type), p[[k]]$type)
        if (!list_length[k]) return(rep(list(numeric()), m))
        unname(split(flat, rep(seq_len(m), each = list_length[k])))
      })
      at <- at + width * m
    } else {
      out <- lapply(p, function(q) if (is.null(q$count)) numeric(m) else vector("list", m))
      for (row in seq_len(m)) for (k in seq_along(p)) {
        if (!is_list[k]) { out[[k]][row] <- read(at, 1L, p[[k]]$type); at <- at + cells(p[[k]]$type); next }
        n <- read(at, 1L, p[[k]]$count); at <- at + cells(p[[k]]$count)
        out[[k]][[row]] <- read(at, n, p[[k]]$type); at <- at + n * cells(p[[k]]$type)
      }
    }
    values[[element$name]] <- stats::setNames(out, property_names)
  }
  attr(values, "comments") <- comments
  values
}

# Vertex positions, colours and other scalar properties of a PLY vertex
# element as a data frame and a colour vector (or NULL).
ply_vertices_3d <- function(vertex) {
  if (is.null(vertex) || !all(c("x", "y", "z") %in% names(vertex))) stop("PLY vertices need x, y and z properties.", call. = FALSE)
  scalars <- Filter(function(x) is.numeric(x) && !is.list(x), vertex)
  list(data = as.data.frame(scalars[setdiff(names(scalars), c("red", "green", "blue", "alpha"))]),
    colour = ply_colour_3d(vertex))
}

ply_colour_3d <- function(d) {
  if (!all(c("red", "green", "blue") %in% names(d))) return(NULL)
  channel_colours_3d(d$red, d$green, d$blue, d$alpha)
}

read_ply_3d <- function(path) {
  values <- ply_elements_3d(path)
  vertex <- ply_vertices_3d(values$vertex); face <- values$face
  if (is.null(face)) stop("The PLY file has no faces; read point clouds with read_points_3d().", call. = FALSE)
  indices <- face$vertex_indices %or3d% face$vertex_index
  if (is.null(indices)) stop("PLY faces need a vertex_indices list.", call. = FALSE)
  vertex_data <- if (!is.null(vertex$colour)) data.frame(.vertex = seq_len(nrow(vertex$data)), vertex_colour = vertex$colour) else NULL
  face_colour <- ply_colour_3d(face)
  face_data <- if (!is.null(face_colour)) data.frame(.face = seq_along(indices), face_colour = face_colour) else NULL
  faces <- lapply(indices, function(i) as.integer(i) + 1L)
  u <- v <- NULL; image <- integer(length(faces)); images <- list()
  texture <- sub("^TextureFile ", "", grep("^TextureFile ", attr(values, "comments"), value = TRUE))
  if (!is.null(face$texcoord) && length(texture)) {
    coords <- face$texcoord
    if (any(lengths(coords) != 2L * lengths(faces))) stop("PLY face texcoord lists need two numbers per corner.", call. = FALSE)
    flat <- unlist(coords, use.names = FALSE)
    u <- flat[c(TRUE, FALSE)]; v <- flat[c(FALSE, TRUE)]
    images <- list(texture_native_3d(texture_file_3d(texture[1L], dirname(path))))
    image <- rep(1L, length(faces))
  }
  mesh_from_faces_3d(vertex$data, faces, vertex_data, face_data, u = u, v = v, image = image, images = images)
}

read_gltf_mesh_3d <- function(path) {
  scene <- gltf_scene_3d(path)
  if (!length(scene$faces)) stop("The glTF file has no triangle primitives; read point primitives with read_points_3d().", call. = FALSE)
  mesh_from_faces_3d(scene$vertices, scene$faces, scene$vertex_data, scene$face_data,
    u = scene$u, v = scene$v, image = scene$image, images = scene$images)
}

mesh_readers_3d <- list(obj = read_obj_3d, stl = read_stl_3d, ply = read_ply_3d, gltf = read_gltf_mesh_3d, glb = read_gltf_mesh_3d)
