scan_png <- function(path, colours) {
  rgb <- grDevices::col2rgb(colours) / 255
  image <- array(0, c(1L, length(colours), 3L))
  for (k in 1:3) image[1L, , k] <- rgb[k, ]
  png::writePNG(image, path)
  path
}

test_that("JSON parser reads nested values, escapes and scalar arrays", {
  x <- json_parse_3d('{"a": [1, 2.5, -3e2], "b": {"c": "q\\"\\n\\u00e9", "d": [true, null, {"e": []}]}, "f": false}')
  expect_identical(x$a, c(1, 2.5, -300))
  expect_identical(x$b$c, "q\"\né")
  expect_true(x$b$d[[1L]])
  expect_null(x$b$d[[2L]])
  expect_identical(x$b$d[[3L]]$e, list())
  expect_false(x$f)
  expect_error(json_parse_3d('{"a": 1,}'), "Invalid JSON")
  expect_error(json_parse_3d('[1, 2] 3'), "trailing")
  expect_error(json_parse_3d('{"a": @}'), "Invalid JSON")
})

test_that("OBJ reads texture coordinates, MTL colours and texture images", {
  skip_if_not_installed("png")
  dir <- tempfile(); dir.create(dir)
  scan_png(file.path(dir, "red.png"), c("red", "darkred"))
  scan_png(file.path(dir, "blue.png"), c("blue", "navy", "skyblue"))
  writeLines(c("newmtl a", "Kd 1 0.5 0", "map_Kd red.png", "newmtl b", "Kd 0 0 1", "d 0.5", "map_Kd -s 1 1 1 blue.png",
    "newmtl c", "Kd 0.2 0.2 0.2"), file.path(dir, "scan.mtl"))
  writeLines(c("mtllib scan.mtl", "v 0 0 0 1 0 0", "v 1 0 0 0 1 0", "v 1 1 0 0 0 1", "v 0 1 0 1 1 1",
    "vt 0 0", "vt 1 0", "vt 1 1", "vt 0 1", "usemtl a", "f 1/1 2/2 3/3", "usemtl b", "f 1/1/1 3/3/1 4/4/1",
    "usemtl c", "f -4 -3 -1"), file.path(dir, "scan.obj"))
  m <- read_mesh_3d(file.path(dir, "scan.obj"))
  expect_identical(unique(m$material), c("a", "b", "c"))
  expect_identical(unique(m$face_colour), c("#FF8000FF", "#0000FF80", "#333333FF"))
  expect_identical(m$vertex_colour[m$.vertex == 2L][1L], "#00FF00")
  tex <- attr(m, "texture")
  expect_s3_class(tex, "ggplot3d_texture")
  expect_identical(dim(tex$image), c(2L, 3L))
  expect_equal(tex$extent, c(0, 1, 0, 1))
  rows <- order(m$.face, m$.corner)
  # Image a fills the top row of the 3 x 2 atlas over two thirds of its width.
  expect_equal(m$.texture_u[rows][1:3], c(0, 2 / 3, 2 / 3))
  expect_equal(m$.texture_v[rows][1:3], c(0.5, 0.5, 1))
  expect_equal(m$.texture_v[rows][4:6], c(0, 0.5, 0.5))
  expect_true(all(is.na(m$.texture_u[rows][7:9])))
  s <- stats_scene(ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d())
  expect_identical(unique(s$packet$styles$texture_id), 1L)
  writeLines(c("v 0 0 0", "f 1 2 3"), file.path(dir, "bad.obj"))
  expect_error(read_mesh_3d(file.path(dir, "bad.obj")), "missing vertices")
  writeLines(c("mtllib none.mtl", "v 0 0 0", "v 1 0 0", "v 0 1 0", "usemtl a", "f 1 2 3"), file.path(dir, "lost.obj"))
  expect_warning(lost <- read_mesh_3d(file.path(dir, "lost.obj")), "not found")
  expect_null(attr(lost, "texture"))
})

test_that("PLY point clouds keep every vertex property and meshes point to them", {
  path <- tempfile(fileext = ".ply")
  writeLines(c("ply", "format ascii 1.0", "element vertex 3", "property float x", "property float y", "property float z",
    "property float nx", "property float opacity", "property uchar red", "property uchar green", "property uchar blue",
    "end_header", "0 0 0 1 0.5 255 0 0", "1 0 0 0 0.25 0 255 0", "0 1 2 0 1 0 0 255"), path)
  pts <- read_points_3d(path)
  expect_identical(names(pts), c("x", "y", "z", "nx", "opacity", "colour"))
  expect_equal(pts$z, c(0, 0, 2))
  expect_identical(pts$colour, c("#FF0000", "#00FF00", "#0000FF"))
  expect_error(read_mesh_3d(path), "read_points_3d")
  binary <- tempfile(fileext = ".ply")
  con <- file(binary, "wb")
  writeLines(c("ply", "format binary_big_endian 1.0", "element vertex 2", "property double x", "property double y",
    "property double z", "end_header"), con, sep = "\n")
  writeBin(c(1, 2, 3, 4, 5, 6), con, size = 8L, endian = "big")
  close(con)
  expect_equal(as.matrix(read_points_3d(binary)), rbind(c(x = 1, y = 2, z = 3), c(4, 5, 6)))
})

test_that("PLY faces of mixed and equal sizes read alike in ASCII and binary", {
  header <- function(format, faces) c("ply", paste("format", format, "1.0"), "element vertex 5", "property float x",
    "property float y", "property float z", paste("element face", faces), "property uchar intensity",
    "property list uchar int vertex_indices", "end_header")
  xyz <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 1, 0), c(2, 0, 0))
  faces <- list(c(0, 1, 2, 3), c(1, 4, 2))
  ascii <- tempfile(fileext = ".ply")
  writeLines(c(header("ascii", 2), apply(xyz, 1, paste, collapse = " "),
    vapply(faces, function(f) paste(7, length(f), paste(f, collapse = " ")), "")), ascii)
  binary <- tempfile(fileext = ".ply")
  con <- file(binary, "wb")
  writeLines(header("binary_little_endian", 2), con, sep = "\n")
  writeBin(as.vector(t(xyz)), con, size = 4L, endian = "little")
  for (f in faces) { writeBin(as.raw(c(7, length(f))), con); writeBin(as.integer(f), con, size = 4L, endian = "little") }
  close(con)
  a <- read_mesh_3d(ascii); b <- read_mesh_3d(binary)
  expect_identical(as.vector(table(a$.face)), c(4L, 3L))
  expect_equal(as.data.frame(a)[c(".face", ".vertex", "x", "y")], as.data.frame(b)[c(".face", ".vertex", "x", "y")])
})

test_that("PLY texcoord lists and TextureFile comments texture the mesh", {
  skip_if_not_installed("png")
  dir <- tempfile(); dir.create(dir)
  scan_png(file.path(dir, "atlas.png"), c("red", "blue"))
  writeLines(c("ply", "format ascii 1.0", "comment TextureFile atlas.png", "element vertex 3", "property float x",
    "property float y", "property float z", "element face 1", "property list uchar int vertex_indices",
    "property list uchar float texcoord", "end_header", "0 0 0", "1 0 0", "0 1 0", "3 0 1 2 6 0 0 1 0 0 1"),
    file.path(dir, "t.ply"))
  m <- read_mesh_3d(file.path(dir, "t.ply"))
  expect_equal(m$.texture_u[order(m$.corner)], c(0, 1, 0))
  expect_equal(m$.texture_v[order(m$.corner)], c(0, 0, 1))
  expect_identical(dim(attr(m, "texture")$image), c(1L, 2L))
})

las_test_file <- function(format, minor, n = 3L) {
  record <- c(`3` = 34L, `7` = 36L)[[as.character(format)]]
  size <- if (minor >= 4L) 375L else 227L
  header <- raw(size)
  put <- function(at, value, bytes, what = "integer") {
    header[at + seq_len(bytes)] <<- writeBin(if (what == "integer") as.integer(value) else as.double(value), raw(), size = bytes, endian = "little")
  }
  header[1:4] <- charToRaw("LASF"); header[25] <- as.raw(1L); header[26] <- as.raw(minor)
  put(94L, size, 2L); put(96L, size, 4L); header[105] <- as.raw(format); put(105L, record, 2L)
  put(107L, if (minor >= 4L) 0L else n, 4L)
  for (k in 0:2) { put(131L + 8L * k, 0.01, 8L, "double"); put(155L + 8L * k, c(100, 200, 0)[k + 1L], 8L, "double") }
  if (minor >= 4L) put(247L, n, 4L)
  points <- lapply(seq_len(n), function(i) {
    r <- raw(record)
    w <- function(at, value, bytes, what = "integer") r[at + seq_len(bytes)] <<- writeBin(if (what == "integer") as.integer(value) else as.double(value), raw(), size = bytes, endian = "little")
    w(0L, 100L * i, 4L); w(4L, -50L * i, 4L); w(8L, 7L * i, 4L); w(12L, 1000L + i, 2L)
    if (format == 3L) {
      r[15] <- as.raw(1L + 2L * 8L); r[16] <- as.raw(2L); r[17] <- as.raw(251L); r[18] <- as.raw(9L); w(18L, 42L, 2L)
      w(20L, 1.5 * i, 8L, "double"); w(28L, 65535L, 2L); w(30L, 0L, 2L); w(32L, 32896L, 2L)
    } else {
      r[15] <- as.raw(2L + 3L * 16L); r[17] <- as.raw(6L); r[18] <- as.raw(4L); w(18L, 1000L, 2L); w(20L, 43L, 2L)
      w(22L, 2.5 * i, 8L, "double"); w(30L, 255L, 2L); w(32L, 128L, 2L); w(34L, 0L, 2L)
    }
    r
  })
  path <- tempfile(fileext = ".las")
  writeBin(c(header, unlist(points)), path)
  path
}

test_that("LAS 1.2 and 1.4 point records decode with scale, offset and attributes", {
  a <- read_points_3d(las_test_file(3L, 2L))
  expect_equal(a$x, 100 + c(1, 2, 3)); expect_equal(a$y, 200 - c(0.5, 1, 1.5)); expect_equal(a$z, 0.07 * 1:3)
  expect_identical(a$intensity, 1001:1003)
  expect_identical(unique(a$return_number), 1L); expect_identical(unique(a$number_of_returns), 2L)
  expect_identical(unique(a$classification), 2L); expect_identical(unique(a$scan_angle_rank), -5L)
  expect_identical(unique(a$point_source_id), 42L)
  expect_equal(a$gpstime, 1.5 * 1:3)
  expect_identical(unique(a$colour), "#FF0080")
  b <- read_points_3d(las_test_file(7L, 4L, n = 2L))
  expect_identical(nrow(b), 2L)
  expect_identical(unique(b$return_number), 2L); expect_identical(unique(b$number_of_returns), 3L)
  expect_identical(unique(b$classification), 6L); expect_equal(unique(b$scan_angle), 6)
  expect_identical(unique(b$colour), "#FF8000")
  laz <- las_test_file(3L, 2L); bytes <- readBin(laz, "raw", file.size(laz)); bytes[105] <- as.raw(131L); writeBin(bytes, laz)
  expect_error(read_points_3d(laz), "lidR")
})

test_that("XYZ and PTS columns map to coordinates, intensity, colours and normals", {
  path <- tempfile(fileext = ".pts")
  writeLines(c("2", "1 2 3 10 255 0 0", "4 5 6 20 0 0 255"), path)
  p <- read_points_3d(path)
  expect_identical(names(p), c("x", "y", "z", "intensity", "colour"))
  expect_identical(p$colour, c("#FF0000", "#0000FF"))
  path <- tempfile(fileext = ".xyz")
  writeLines(c("1,2,3,0.6,0,0.8", "4;5;6;0;1;0"), path)
  expect_identical(names(read_points_3d(path)), c("x", "y", "z", "nx", "ny", "nz"))
  writeLines(c("1 2", "3 4"), path)
  expect_error(read_points_3d(path), "at least three")
  expect_error(read_points_3d(tempfile(fileext = ".e57")), "existing file")
  e57 <- tempfile(fileext = ".e57"); writeLines("x", e57)
  expect_error(read_points_3d(e57), "No point cloud reader")
})

gltf_test_document <- function(glb = FALSE) {
  png_bytes <- {
    path <- scan_png(tempfile(fileext = ".png"), c("red", "blue")); readBin(path, "raw", file.size(path))
  }
  pad <- function(x) c(x, raw((4L - length(x) %% 4L) %% 4L))
  f32 <- function(x) writeBin(as.double(x), raw(), size = 4L, endian = "little")
  parts <- list(
    position = f32(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0)),
    index = pad(writeBin(0:3, raw(), size = 2L, endian = "little")),
    colour = as.raw(c(255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 255, 255)),
    uv = f32(c(0, 1, 1, 1, 0, 0, 1, 0)),
    points = f32(c(5, 5, 5, 6, 6, 6)),
    image = pad(png_bytes))
  offsets <- cumsum(c(0L, lengths(parts)))[seq_along(parts)]
  bin <- unlist(parts, use.names = FALSE)
  views <- vapply(seq_along(parts), function(i) sprintf('{"buffer": 0, "byteOffset": %d, "byteLength": %d}', offsets[i],
    if (names(parts)[i] == "index") 8L else if (names(parts)[i] == "image") length(png_bytes) else length(parts[[i]])), "")
  buffer <- if (glb) sprintf('{"byteLength": %d}', length(bin)) else
    sprintf('{"byteLength": %d, "uri": "data:application/octet-stream;base64,%s"}', length(bin), base64_3d(bin))
  json <- paste0('{"asset": {"version": "2.0"}, "scene": 0, "scenes": [{"nodes": [0, 1]}],',
    '"nodes": [{"mesh": 0, "translation": [1, 0, 0], "rotation": [0, 0, 0.7071067811865476, 0.7071067811865476]}, {"mesh": 1}],',
    '"meshes": [{"primitives": [{"attributes": {"POSITION": 0, "COLOR_0": 2, "TEXCOORD_0": 3}, "indices": 1, "mode": 5, "material": 0}]},',
    '{"primitives": [{"attributes": {"POSITION": 4}, "mode": 0}]}],',
    '"materials": [{"name": "scan", "pbrMetallicRoughness": {"baseColorTexture": {"index": 0}}}],',
    '"textures": [{"source": 0}], "images": [{"bufferView": 5, "mimeType": "image/png"}],',
    '"accessors": [{"bufferView": 0, "componentType": 5126, "count": 4, "type": "VEC3"},',
    '{"bufferView": 1, "componentType": 5123, "count": 4, "type": "SCALAR"},',
    '{"bufferView": 2, "componentType": 5121, "normalized": true, "count": 4, "type": "VEC4"},',
    '{"bufferView": 3, "componentType": 5126, "count": 4, "type": "VEC2"},',
    '{"bufferView": 4, "componentType": 5126, "count": 2, "type": "VEC3"}],',
    '"bufferViews": [', paste(views, collapse = ", "), '], "buffers": [', buffer, ']}')
  if (!glb) {
    path <- tempfile(fileext = ".gltf"); writeLines(json, path); return(path)
  }
  chunk <- function(bytes, type, fill) {
    bytes <- c(bytes, rep(as.raw(fill), (4L - length(bytes) %% 4L) %% 4L))
    c(writeBin(length(bytes), raw(), size = 4L, endian = "little"), writeBin(type, raw(), size = 4L, endian = "little"), bytes)
  }
  body <- c(chunk(charToRaw(json), 0x4E4F534AL, 0x20), chunk(bin, 0x004E4942L, 0))
  path <- tempfile(fileext = ".glb")
  writeBin(c(charToRaw("glTF"), writeBin(c(2L, 12L + length(body)), raw(), size = 4L, endian = "little"), body), path)
  path
}

test_that("glTF and GLB read strips, node transforms, colours, textures and points", {
  skip_if_not_installed("png")
  for (glb in c(FALSE, TRUE)) {
    path <- gltf_test_document(glb)
    m <- read_mesh_3d(path)
    expect_identical(length(unique(m$.face)), 2L)
    # The node turns the quad a quarter turn about z and moves it by x = 1.
    corners <- unique(round(as.matrix(m[c("x", "y", "z")]), 12))
    expect_equal(corners[order(corners[, 1L], corners[, 2L]), ], rbind(c(0, 0, 0), c(0, 1, 0), c(1, 0, 0), c(1, 1, 0)),
      ignore_attr = TRUE)
    expect_identical(m$vertex_colour[m$.vertex == 1L][1L], "#FF0000FF")
    expect_identical(unique(m$material), "scan")
    expect_identical(dim(attr(m, "texture")$image), c(1L, 2L))
    expect_equal(m$.texture_v[m$.vertex == 1L][1L], 0)
    pts <- read_points_3d(path)
    expect_equal(as.matrix(pts), rbind(c(x = 5, y = 5, z = 5), c(6, 6, 6)))
  }
})

test_that("GLB files written by export_3d() read back", {
  skip_on_cran()
  p <- export_surface_plot(fill = "#336699", colour = NA, coord = coord_3d(light = NULL))
  file <- tempfile(fileext = ".glb")
  export_3d(p, file)
  m <- read_mesh_3d(file)
  expect_gte(length(unique(m$.face)), 50L)
  colour <- grDevices::col2rgb(unique(c(m$vertex_colour, m$face_colour)[!is.na(c(m$vertex_colour, m$face_colour))]))
  expect_lte(max(abs(colour - grDevices::col2rgb("#336699"))), 1)
})
