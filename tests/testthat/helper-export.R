export_surface_plot <- function(..., n = 6L, coord = coord_3d()) {
  d <- expand.grid(x = seq(-1, 1, length.out = n), y = seq(-1, 1, length.out = n))
  d$z <- d$x * exp(-d$x^2 - d$y^2)
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(...) + coord
}

read_glb <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  word <- function(at) readBin(raw[at + 0:3], "integer", size = 4L, endian = "little")
  json_length <- word(13L)
  json <- jsonlite::fromJSON(rawToChar(raw[20L + seq_len(json_length)]), simplifyVector = FALSE)
  bin_at <- 20L + json_length
  bin <- if (bin_at < length(raw)) raw[bin_at + 8L + seq_len(word(bin_at + 1L))] else raw()
  list(header = c(word(1L), word(5L), word(9L)), json = json, bin = bin)
}

glb_accessor <- function(glb, index) {
  accessor <- glb$json$accessors[[index + 1L]]
  view <- glb$json$bufferViews[[accessor$bufferView + 1L]]
  width <- c(SCALAR = 1L, VEC3 = 3L, VEC4 = 4L)[[accessor$type]]
  bytes <- glb$bin[view$byteOffset + seq_len(view$byteLength)]
  values <- if (accessor$componentType == 5125L) readBin(bytes, "integer", n = accessor$count * width, size = 4L, endian = "little")
    else readBin(bytes, "double", n = accessor$count * width, size = 4L, endian = "little")
  if (width == 1L) values else matrix(values, ncol = width, byrow = TRUE)
}

glb_node <- function(glb, name) {
  Filter(function(node) identical(node$name, name), glb$json$nodes)[[1L]]
}

quaternion_rotate <- function(q, v) {
  u <- q[1:3]; w <- q[4]
  cross <- function(a, b) c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
  v + 2 * w * cross(u, v) + 2 * cross(u, cross(u, v))
}
