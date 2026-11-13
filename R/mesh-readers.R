#' Read a triangle or polygon mesh file
#'
#' Reads Wavefront OBJ, STL and PLY files into a [mesh_3d()] corner table. The
#' reader is chosen by file extension from a registry of native parsers.
#'
#' * OBJ (ASCII): `v` vertices and `f` faces of any size, with `v/vt/vn`
#'   references, negative (relative) indices and `\` line continuation;
#'   `o` and `g` names become the face attribute `group`. Other statements are
#'   ignored.
#' * STL (ASCII or binary): every facet is a triangle; vertices with exactly
#'   equal coordinates are welded. A file whose size is 84 bytes plus 50 bytes
#'   per triangle counted in its header is read as binary, whatever its first
#'   bytes say.
#' * PLY (ASCII, binary little- or big-endian): header-driven elements and
#'   properties of every scalar type; `vertex` x, y, z and `face`
#'   `vertex_indices` (or `vertex_index`). Vertex `red`, `green`, `blue` (and
#'   `alpha`) become the vertex attribute `vertex_colour`, face colours the
#'   face attribute `face_colour`; other scalar vertex properties are kept as
#'   vertex attributes.
#' @param path Path to a `.obj`, `.stl` or `.ply` file.
#' @return A keyed corner data frame of class ggplot3d_mesh.
#' @references Wavefront OBJ: <https://paulbourke.net/dataformats/obj/>.
#'   PLY (Turk): <https://paulbourke.net/dataformats/ply/>.
#'   STL: <https://en.wikipedia.org/wiki/STL_(file_format)>.
#' @export
#' @examples
#' path <- tempfile(fileext = ".obj")
#' writeLines(c("v 0 0 0", "v 1 0 0", "v 1 1 0", "v 0 1 1", "f 1 2 3 4"), path)
#' m <- read_mesh_3d(path)
#' ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
read_mesh_3d <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) stop("path must name an existing file.", call. = FALSE)
  extension <- if (grepl(".", basename(path), fixed = TRUE)) tolower(sub(".*\\.", "", basename(path))) else ""
  reader <- mesh_readers_3d[[extension]]
  if (is.null(reader)) stop("No mesh reader for '.", extension, "' files; supported: ",
    paste0(".", names(mesh_readers_3d), collapse = ", "), ".", call. = FALSE)
  reader(path)
}

mesh_faces_3d <- function(faces) {
  sizes <- lengths(faces)
  if (any(sizes < 3L)) stop("Mesh faces need at least three vertices.", call. = FALSE)
  data.frame(.face = rep(seq_along(faces), sizes), .corner = sequence(sizes), .vertex = unlist(faces, use.names = FALSE))
}

read_obj_3d <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- sub("#.*$", "", lines)
  # A trailing backslash continues a statement on the next line.
  joined <- character(); carry <- ""
  for (line in lines) {
    if (grepl("\\\\\\s*$", line)) { carry <- paste(carry, sub("\\\\\\s*$", "", line)); next }
    joined <- c(joined, paste(carry, line)); carry <- ""
  }
  tokens <- strsplit(trimws(joined), "[[:space:]]+")
  keyword <- vapply(tokens, function(t) if (length(t)) t[1L] else "", character(1))
  vertex_rows <- tokens[keyword == "v"]
  if (!length(vertex_rows)) stop("The OBJ file has no vertices.", call. = FALSE)
  xyz <- t(vapply(vertex_rows, function(t) as.double(t[2:4]), numeric(3)))
  if (anyNA(xyz)) stop("OBJ vertices need three numeric coordinates.", call. = FALSE)
  seen <- cumsum(keyword == "v")
  faces <- list(); groups <- character(); group <- NA_character_
  for (i in which(keyword %in% c("f", "g", "o"))) {
    if (keyword[i] != "f") { group <- paste(tokens[[i]][-1L], collapse = " "); next }
    index <- as.integer(sub("/.*$", "", tokens[[i]][-1L]))
    if (anyNA(index) || any(index == 0L)) stop("OBJ face indices must be nonzero integers.", call. = FALSE)
    index <- ifelse(index < 0L, seen[i] + 1L + index, index)
    if (any(index < 1L | index > nrow(xyz))) stop("OBJ face indices reference missing vertices.", call. = FALSE)
    faces[[length(faces) + 1L]] <- index; groups <- c(groups, group)
  }
  if (!length(faces)) stop("The OBJ file has no faces.", call. = FALSE)
  face_data <- if (any(!is.na(groups))) data.frame(.face = seq_along(faces), group = groups) else NULL
  mesh_3d(data.frame(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L]), mesh_faces_3d(faces), face_data = face_data)
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

read_ply_3d <- function(path) {
  size <- file.size(path)
  raw <- readBin(path, "raw", size)
  stop_at <- grepRaw("end_header\r?\n", raw)
  if (!length(stop_at) || !identical(rawToChar(raw[1:3]), "ply")) stop("Not a PLY file.", call. = FALSE)
  header_end <- stop_at + length(grepRaw("end_header\r?\n", raw, value = TRUE)) - 1L
  header <- strsplit(trimws(strsplit(rawToChar(raw[seq_len(header_end)]), "\r?\n")[[1L]]), "[[:space:]]+")
  format <- NULL; elements <- list()
  for (line in header) {
    if (line[1L] == "format") format <- line[2L]
    else if (line[1L] == "element") elements[[length(elements) + 1L]] <- list(name = line[2L], count = as.numeric(line[3L]), properties = list())
    else if (line[1L] == "property") {
      e <- length(elements)
      property <- if (line[2L] == "list") list(name = line[5L], count = line[3L], type = line[4L]) else list(name = line[3L], type = line[2L])
      if (!all(c(property$type, property$count) %in% names(ply_types_3d))) stop("Unknown PLY property type.", call. = FALSE)
      elements[[e]]$properties[[length(elements[[e]]$properties) + 1L]] <- property
    }
  }
  if (!format %in% c("ascii", "binary_little_endian", "binary_big_endian")) stop("Unknown PLY format.", call. = FALSE)
  body <- raw[-seq_len(header_end)]
  values <- list()
  if (format == "ascii") {
    tokens <- as.double(scan(text = rawToChar(body), what = character(), quiet = TRUE))
    at <- 0L
    for (element in elements) {
      out <- lapply(element$properties, function(p) if (is.null(p$count)) numeric(element$count) else vector("list", element$count))
      for (row in seq_len(element$count)) for (k in seq_along(element$properties)) {
        p <- element$properties[[k]]
        if (is.null(p$count)) { at <- at + 1L; out[[k]][row] <- tokens[at] }
        else { n <- tokens[at + 1L]; out[[k]][[row]] <- tokens[at + 1L + seq_len(n)]; at <- at + 1L + n }
      }
      values[[element$name]] <- stats::setNames(out, vapply(element$properties, `[[`, "", "name"))
    }
  } else {
    endian <- if (format == "binary_little_endian") "little" else "big"
    at <- 0L
    for (element in elements) {
      scalar <- vapply(element$properties, function(p) is.null(p$count), logical(1))
      widths <- vapply(element$properties, function(p) if (is.null(p$count)) ply_types_3d[[p$type]][[2L]] else NA_integer_, integer(1))
      if (all(scalar)) {
        record <- matrix(body[at + seq_len(sum(widths) * element$count)], nrow = sum(widths))
        offsets <- c(0L, cumsum(widths))
        out <- lapply(seq_along(widths), function(k)
          ply_decode_3d(as.vector(record[offsets[k] + seq_len(widths[k]), , drop = FALSE]), element$properties[[k]]$type, endian))
        at <- at + sum(widths) * element$count
      } else {
        out <- lapply(element$properties, function(p) if (is.null(p$count)) numeric(element$count) else vector("list", element$count))
        for (row in seq_len(element$count)) for (k in seq_along(element$properties)) {
          p <- element$properties[[k]]
          if (is.null(p$count)) {
            w <- widths[k]; out[[k]][row] <- ply_decode_3d(body[at + seq_len(w)], p$type, endian); at <- at + w
          } else {
            cw <- ply_types_3d[[p$count]][[2L]]; n <- ply_decode_3d(body[at + seq_len(cw)], p$count, endian); at <- at + cw
            iw <- ply_types_3d[[p$type]][[2L]]; out[[k]][[row]] <- ply_decode_3d(body[at + seq_len(n * iw)], p$type, endian); at <- at + n * iw
          }
        }
      }
      values[[element$name]] <- stats::setNames(out, vapply(element$properties, `[[`, "", "name"))
    }
  }
  vertex <- values$vertex; face <- values$face
  if (is.null(vertex) || !all(c("x", "y", "z") %in% names(vertex))) stop("PLY vertices need x, y and z properties.", call. = FALSE)
  if (is.null(face)) stop("The PLY file has no faces.", call. = FALSE)
  indices <- face$vertex_indices %or3d% face$vertex_index
  if (is.null(indices)) stop("PLY faces need a vertex_indices list.", call. = FALSE)
  colour <- function(d) {
    if (!all(c("red", "green", "blue") %in% names(d))) return(NULL)
    scale <- if (max(unlist(d[c("red", "green", "blue")])) <= 1) 1 else 255
    if (is.null(d$alpha)) grDevices::rgb(d$red, d$green, d$blue, maxColorValue = scale)
    else grDevices::rgb(d$red, d$green, d$blue, d$alpha, maxColorValue = scale)
  }
  scalars <- Filter(function(x) is.numeric(x) && !is.list(x), vertex)
  vertices <- as.data.frame(scalars[setdiff(names(scalars), c("red", "green", "blue", "alpha"))])
  vertex_colour <- colour(vertex)
  vertex_data <- if (!is.null(vertex_colour)) data.frame(.vertex = seq_len(nrow(vertices)), vertex_colour = vertex_colour) else NULL
  face_colour <- colour(face)
  face_data <- if (!is.null(face_colour)) data.frame(.face = seq_along(indices), face_colour = face_colour) else NULL
  mesh_3d(vertices, mesh_faces_3d(lapply(indices, function(i) as.integer(i) + 1L)), vertex_data = vertex_data, face_data = face_data)
}

mesh_readers_3d <- list(obj = read_obj_3d, stl = read_stl_3d, ply = read_ply_3d)
