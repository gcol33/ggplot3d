print_python_3d <- function(python) {
  scene_assert(nzchar(python), "Printing needs Python with manifold3d and numpy; set python in print_3d()")
  error <- tempfile();on.exit(unlink(error))
  out <- suppressWarnings(system2(python, c("-c", shQuote("import sys; print(sys.executable)")), stdout = TRUE, stderr = error))
  scene_assert(is.null(attr(out, "status")) && length(out) == 1L && file.exists(out),
    paste("Cannot resolve the Python executable. Set python in print_3d() to the interpreter containing manifold3d and numpy.",
      paste(c(out, readLines(error, warn = FALSE)), collapse = " ")))
  out
}

print_input_3d <- function(file, meshes, texts) {
  connection <- file(file, "wb");on.exit(close(connection))
  uint <- function(x) writeBin(as.integer(x), connection, size = 4L, endian = "little")
  doubles <- function(x) writeBin(as.double(x), connection, size = 8L, endian = "little")
  name <- function(x) {raw <- charToRaw(enc2utf8(x));uint(length(raw));writeBin(raw, connection)}
  writeBin(charToRaw("GGPRINT1"), connection);uint(length(meshes));uint(length(texts))
  for (mesh in meshes) {
    name(mesh$name);uint(mesh$role);uint(nrow(mesh$position));uint(nrow(mesh$triangles))
    doubles(t(mesh$position));uint(t(mesh$triangles - 1L));doubles(t(mesh$colour))
  }
  for (text in texts) {
    name(text$name);uint(text$mode);doubles(c(text$depth, text$dilation));doubles(text$colour)
    doubles(t(text$transform));uint(length(text$contours))
    for (contour in text$contours) {uint(nrow(contour));doubles(t(as.matrix(contour)))}
  }
}

print_output_3d <- function(file) {
  connection <- file(file, "rb");on.exit(close(connection))
  uint <- function(n) readBin(connection, "integer", n = n, size = 4L, endian = "little")
  scene_assert(identical(rawToChar(readBin(connection, "raw", 8L)), "GGUNION1"), "Invalid Manifold result stream")
  counts <- uint(3L);nv <- counts[1L];nt <- counts[2L];np <- counts[3L]
  scene_assert(all(counts > 0L) && np >= 7L, "Manifold returned invalid geometry dimensions")
  properties <- matrix(readBin(connection, "double", n = nv * np, size = 8L, endian = "little"), ncol = np, byrow = TRUE)
  triangles <- matrix(uint(nt * 3L), ncol = 3L, byrow = TRUE) + 1L
  scene_assert(nrow(properties) == nv && nrow(triangles) == nt && all(is.finite(properties)) &&
    all(triangles >= 1L & triangles <= nv), "Manifold returned an incomplete mesh")
  list(position = properties[, 1:3, drop = FALSE], colour = properties[, 4:7, drop = FALSE], triangles = triangles)
}

print_union_3d <- function(meshes, texts, python) {
  python <- print_python_3d(python)
  source <- tempfile(fileext = ".mesh");output <- tempfile(fileext = ".mesh");log <- tempfile()
  on.exit(unlink(c(source, output, log)))
  print_input_3d(source, meshes, texts)
  bridge <- system.file("python", "print_union.py", package = "ggplot3d")
  scene_assert(nzchar(bridge), "The printing bridge is missing from the package")
  status <- suppressWarnings(system2(python, shQuote(c(bridge, source, output)), stdout = log, stderr = log))
  scene_assert(identical(status, 0L) && file.exists(output),
    paste("Printing union failed:", paste(readLines(log, warn = FALSE), collapse = "\n")))
  print_output_3d(output)
}
