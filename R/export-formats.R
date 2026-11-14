register_exporter_3d("ply", function(frames, file, options, fps) write_ply_3d(frames[[1L]], file, options))
register_exporter_3d("obj", function(frames, file, options, fps) write_obj_3d(frames[[1L]], file, options))
register_exporter_3d("stl", function(frames, file, options, fps) write_stl_3d(frames[[1L]], file, options), triangles = TRUE, printing = TRUE)

float_bytes_3d <- function(x) writeBin(as.double(x), raw(), size = 4L, endian = "little")
int_bytes_3d <- function(x) writeBin(as.integer(x), raw(), size = 4L, endian = "little")

# Binary little-endian PLY: float positions and normals, uchar sRGB colours
# with alpha, triangle faces and, for line primitives, edge elements.
write_ply_3d <- function(scene, file, options) {
  mesh <- export_merged_3d(scene, options, "z")
  n <- nrow(mesh$position); n_faces <- ncol(mesh$triangles); n_edges <- ncol(mesh$lines)
  header <- c("ply", "format binary_little_endian 1.0", "comment ggplot3d export",
    paste("element vertex", n), paste("property float", c("x", "y", "z", "nx", "ny", "nz")),
    paste("property uchar", c("red", "green", "blue", "alpha")),
    if (n_faces) c(paste("element face", n_faces), "property list uchar int vertex_indices"),
    if (n_edges) c(paste("element edge", n_edges), "property int vertex1", "property int vertex2"),
    "end_header")
  vertices <- rbind(matrix(float_bytes_3d(t(cbind(mesh$position, mesh$normal))), 24L),
    matrix(as.raw(round(t(mesh$colour) * 255)), 4L))
  faces <- if (n_faces) rbind(as.raw(3L), matrix(int_bytes_3d(mesh$triangles - 1L), 12L))
  edges <- if (n_edges) int_bytes_3d(mesh$lines - 1L)
  writeBin(c(charToRaw(paste0(paste(header, collapse = "\n"), "\n")), as.vector(vertices),
    as.vector(faces), edges), file)
  invisible(file)
}

# Binary STL: an 80-byte header, the triangle count and one 50-byte record per
# triangle with its facet normal.
write_stl_3d <- function(scene, file, options) {
  mesh <- export_merged_3d(scene, options, "z")
  tri <- mesh$triangles
  a <- mesh$position[tri[1L, ], , drop = FALSE]; b <- mesh$position[tri[2L, ], , drop = FALSE]
  c <- mesh$position[tri[3L, ], , drop = FALSE]
  normal <- export_unit_rows_3d(cross_rows_3d(b - a, c - a))
  header <- charToRaw("binary STL written by ggplot3d")
  records <- rbind(matrix(float_bytes_3d(rbind(t(normal), t(a), t(b), t(c))), 48L), matrix(raw(2L * ncol(tri)), 2L))
  writeBin(c(header, raw(80L - length(header)), int_bytes_3d(ncol(tri)), as.vector(records)), file)
  invisible(file)
}

# Wavefront OBJ with +y up: one object per node, vertex colours after each
# position, normals for triangle vertices and an MTL file with one material
# per distinct sRGB colour and alpha.
write_obj_3d <- function(scene, file, options) {
  library_file <- sub("\\.obj$", ".mtl", file, ignore.case = TRUE)
  out <- c("# ggplot3d export", paste("mtllib", basename(library_file)))
  n_vertices <- 0L; n_normals <- 0L; materials <- character()
  number <- function(x) trimws(formatC(x, digits = 7L, format = "g"))
  for (node in scene$nodes) {
    prims <- export_mesh_3d(node, options)
    if (!length(prims)) next
    out <- c(out, paste("o", gsub("[[:space:]]", "_", paste(c(node$parent, node$name), collapse = "_"))))
    for (p in prims) {
      position <- export_axes_3d(p$position, scene$up, "y")
      out <- c(out, paste("v", number(position[, 1L]), number(position[, 2L]), number(position[, 3L]),
        number(p$colour[, 1L]), number(p$colour[, 2L]), number(p$colour[, 3L])))
      key <- sprintf("colour_%02x%02x%02x%02x", round(p$colour[, 1L] * 255), round(p$colour[, 2L] * 255),
        round(p$colour[, 3L] * 255), round(p$colour[, 4L] * 255))
      materials <- c(materials, setdiff(unique(key), materials))
      index <- p$index
      element <- key[index[1L, ]]
      order <- order(element, method = "radix")
      index <- index[, order, drop = FALSE]; element <- element[order]
      corners <- matrix(index + n_vertices, nrow(index))
      tokens <- corners
      if (p$mode == "triangles") {
        normal <- export_axes_3d(p$normal, scene$up, "y")
        out <- c(out, paste("vn", number(normal[, 1L]), number(normal[, 2L]), number(normal[, 3L])))
        tokens <- matrix(paste0(corners, "//", index + n_normals), nrow(index))
        n_normals <- n_normals + nrow(normal)
      }
      body <- do.call(paste, c(list(c(triangles = "f", lines = "l", points = "p")[[p$mode]]),
        lapply(seq_len(nrow(tokens)), function(r) tokens[r, ])))
      start <- c(TRUE, element[-1L] != element[-length(element)])
      lines <- character(length(body) + sum(start))
      at <- seq_along(body) + cumsum(start)
      lines[at] <- body
      lines[which(start) + cumsum(start)[start] - 1L] <- paste("usemtl", element[start])
      out <- c(out, lines)
      n_vertices <- n_vertices + nrow(position)
    }
  }
  writeLines(out, file, useBytes = TRUE)
  rgba <- strtoi(substring(rep(materials, each = 4L), c(8L, 10L, 12L, 14L), c(9L, 11L, 13L, 15L)), 16L) / 255
  rgba <- matrix(rgba, ncol = 4L, byrow = TRUE)
  library <- unlist(lapply(seq_along(materials), function(i) {
    c(paste("newmtl", materials[i]), paste("Kd", number(rgba[i, 1L]), number(rgba[i, 2L]), number(rgba[i, 3L])),
      "Ka 0 0 0", "Ks 0 0 0", paste("d", number(rgba[i, 4L])), "illum 1", "")
  }))
  writeLines(c("# ggplot3d materials", "", library), library_file, useBytes = TRUE)
  invisible(file)
}
