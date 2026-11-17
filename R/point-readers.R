#' Read a point cloud file
#'
#' Reads LiDAR and photogrammetry point clouds into a data frame with one row
#' per point, ready for `geom_point()` with `aes(z = z)` in [coord_3d()], or
#' for surface reconstruction with [stat_implicit_fit_3d()] and
#' [stat_alpha_shape_3d()]. The reader is chosen by file extension from a
#' registry of native parsers.
#'
#' * PLY (ASCII, binary little- or big-endian): the `vertex` element; faces,
#'   if any, are ignored. `red`, `green`, `blue` (and `alpha`) become
#'   `colour`; every other scalar property is kept as a column, so normals
#'   (`nx`, `ny`, `nz`) and the parameters of 3D Gaussian splatting files
#'   (`f_dc_0`, `opacity`, `scale_0`, `rot_0`, ...) come through by name.
#' * LAS 1.0 to 1.4, point formats 0 to 10: coordinates with the header's
#'   scale and offset applied, `intensity`, `return_number`,
#'   `number_of_returns`, `classification`, `scan_angle_rank` (formats 0 to 5,
#'   degrees) or `scan_angle` (formats 6 to 10, degrees), `user_data`,
#'   `point_source_id`, `gpstime` where the format records it, and `colour`
#'   and `nir` where it records them. Names follow lidR's columns in snake
#'   case. Compressed LAZ files are read by lidR (`lidR::readLAS()`, then
#'   [ggplot2::fortify()]).
#' * XYZ and PTS text: whitespace, comma or semicolon separated columns, one
#'   point per line; a PTS file may start with the point count. Three
#'   columns are `x`, `y`, `z`; four add `intensity`; six add `colour` when
#'   the last three are integers in 0 to 255 and normals `nx`, `ny`, `nz`
#'   otherwise; seven add `intensity` and `colour`. Further columns are kept as
#'   `v4`, `v5`, and so on.
#' * glTF 2.0 (`.gltf`, `.glb`): the points of point primitives (`mode` 0),
#'   placed by their node transforms, with `COLOR_0` as `colour`.
#'
#' Scanning apps on phones and tablets with a LiDAR sensor (iPhone and iPad
#' Pro models) export PLY, LAS, XYZ or glTF point clouds. Most of them write
#' y-up coordinates; draw those with `coord_3d(up = "y")`.
#' @param path Path to a `.ply`, `.las`, `.xyz`, `.pts`, `.txt`, `.gltf` or
#'   `.glb` file.
#' @return A data frame with columns `x`, `y`, `z` and the point attributes.
#' @references LAS Specification 1.4 (ASPRS), maintained at
#'   <https://github.com/ASPRSorg/LAS>.
#'   PLY (Turk): <https://paulbourke.net/dataformats/ply/>.
#'   Kerbl, Kopanas, Leimkuehler, Drettakis (2023). 3D Gaussian splatting for
#'   real-time radiance field rendering. ACM Transactions on Graphics 42(4),
#'   139. \doi{10.1145/3592433}
#' @seealso [read_mesh_3d()] for meshes.
#' @export
#' @examples
#' path <- tempfile(fileext = ".xyz")
#' set.seed(1)
#' u <- runif(400, 0, 2 * pi); v <- acos(runif(400, -1, 1))
#' writeLines(paste(cos(u) * sin(v), sin(u) * sin(v), cos(v)), path)
#' pts <- read_points_3d(path)
#' ggplot2::ggplot(pts, ggplot2::aes(x, y, z = z, colour = z)) +
#'   ggplot2::geom_point() +
#'   coord_3d()
read_points_3d <- function(path) {
  points <- reader_dispatch_3d(path, point_readers_3d, "point cloud")
  rownames(points) <- NULL
  points
}

read_ply_points_3d <- function(path) {
  vertex <- ply_vertices_3d(ply_elements_3d(path)$vertex)
  points <- vertex$data
  if (!is.null(vertex$colour)) points$colour <- vertex$colour
  points
}

# Byte offsets of the LAS point fields after X, Y, Z and intensity, per
# point data format, and the minimum record length.
las_formats_3d <- list(
  list(gps = NA, rgb = NA, nir = NA, size = 20L), list(gps = 20L, rgb = NA, nir = NA, size = 28L),
  list(gps = NA, rgb = 20L, nir = NA, size = 26L), list(gps = 20L, rgb = 28L, nir = NA, size = 34L),
  list(gps = 20L, rgb = NA, nir = NA, size = 57L), list(gps = 20L, rgb = 28L, nir = NA, size = 63L),
  list(gps = 22L, rgb = NA, nir = NA, size = 30L), list(gps = 22L, rgb = 30L, nir = NA, size = 36L),
  list(gps = 22L, rgb = 30L, nir = 36L, size = 38L), list(gps = 22L, rgb = NA, nir = NA, size = 59L),
  list(gps = 22L, rgb = 30L, nir = 36L, size = 67L))

read_las_3d <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  header <- readBin(con, "raw", 375L)
  if (length(header) < 227L || !identical(rawToChar(header[1:4]), "LASF")) stop("Not a LAS file.", call. = FALSE)
  unsigned <- function(at, size) {
    if (size < 4L) return(readBin(header[at + seq_len(size)], "integer", size = size, signed = FALSE, endian = "little"))
    value <- readBin(header[at + 1:4], "integer", size = 4L, endian = "little")
    if (value < 0) value + 2^32 else value
  }
  real <- function(at) readBin(header[at + 1:8], "double", size = 8L, endian = "little")
  minor <- unsigned(25L, 1L); start <- unsigned(96L, 4L); format <- unsigned(104L, 1L); record <- unsigned(105L, 2L)
  count <- unsigned(107L, 4L)
  if (format >= 64L) stop("Compressed LAZ points are read by lidR: lidR::readLAS(path), then ggplot2::fortify().", call. = FALSE)
  if (format > 10L) stop("Unknown LAS point data format ", format, ".", call. = FALSE)
  if (minor >= 4L && length(header) >= 255L && count == 0) count <- unsigned(247L, 4L) + 2^32 * unsigned(251L, 4L)
  spec <- las_formats_3d[[format + 1L]]
  if (record < spec$size) stop("The LAS point records are shorter than format ", format, " needs.", call. = FALSE)
  seek(con, start)
  body <- readBin(con, "raw", record * count)
  if (length(body) < record * count) stop("The LAS file is truncated.", call. = FALSE)
  m <- matrix(body, nrow = record)
  field <- function(at, size, type = "integer", signed = TRUE)
    readBin(as.vector(m[at + seq_len(size), , drop = FALSE]), type, n = count, size = size, signed = signed, endian = "little")
  scale <- c(real(131L), real(139L), real(147L)); shift <- c(real(155L), real(163L), real(171L))
  out <- data.frame(x = field(0L, 4L) * scale[1L] + shift[1L], y = field(4L, 4L) * scale[2L] + shift[2L],
    z = field(8L, 4L) * scale[3L] + shift[3L], intensity = field(12L, 2L, signed = FALSE))
  flags <- field(14L, 1L, signed = FALSE)
  if (format <= 5L) {
    out$return_number <- flags %% 8L; out$number_of_returns <- (flags %/% 8L) %% 8L
    out$classification <- field(15L, 1L, signed = FALSE) %% 32L
    out$scan_angle_rank <- field(16L, 1L)
    out$user_data <- field(17L, 1L, signed = FALSE)
    out$point_source_id <- field(18L, 2L, signed = FALSE)
  } else {
    out$return_number <- flags %% 16L; out$number_of_returns <- flags %/% 16L
    out$classification <- field(16L, 1L, signed = FALSE)
    out$user_data <- field(17L, 1L, signed = FALSE)
    out$scan_angle <- field(18L, 2L) * 0.006
    out$point_source_id <- field(20L, 2L, signed = FALSE)
  }
  if (!is.na(spec$gps)) out$gpstime <- field(spec$gps, 8L, "double")
  if (!is.na(spec$rgb)) {
    rgb <- vapply(0:2, function(k) field(spec$rgb + 2L * k, 2L, signed = FALSE), numeric(count))
    if (!is.matrix(rgb)) rgb <- matrix(rgb, 1L)
    out$colour <- channel_colours_3d(rgb[, 1L], rgb[, 2L], rgb[, 3L], max = if (max(rgb, 0) > 255) 65535 else 255)
  }
  if (!is.na(spec$nir)) out$nir <- field(spec$nir, 2L, signed = FALSE)
  out
}

read_xyz_3d <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#") & !startsWith(lines, "//")]
  if (length(lines) && grepl("^[0-9]+$", lines[1L])) lines <- lines[-1L]
  if (!length(lines)) stop("The point file has no points.", call. = FALSE)
  rows <- number_rows_3d(gsub("[[:space:],;]+", " ", lines))
  k <- rows$sizes[1L]
  if (any(rows$sizes != k) || k < 3L) stop("Every line of the point file needs the same number of columns, at least three.", call. = FALSE)
  values <- matrix(rows$values, ncol = k, byrow = TRUE)
  if (anyNA(values)) stop("The point file has non-numeric values.", call. = FALSE)
  out <- data.frame(x = values[, 1L], y = values[, 2L], z = values[, 3L])
  is_rgb <- function(cols) all(values[, cols] == round(values[, cols])) && all(values[, cols] >= 0 & values[, cols] <= 255)
  used <- 3L
  if (k == 4L || k >= 7L) { out$intensity <- values[, 4L]; used <- 4L }
  if (k == 6L || k >= 7L) {
    cols <- used + 1:3
    if (k == 6L && !is_rgb(cols)) { out$nx <- values[, 4L]; out$ny <- values[, 5L]; out$nz <- values[, 6L] }
    else out$colour <- channel_colours_3d(values[, cols[1L]], values[, cols[2L]], values[, cols[3L]], max = 255)
    used <- used + 3L
  }
  for (j in seq_len(k - used) + used) out[[paste0("v", j)]] <- values[, j]
  out
}

read_gltf_points_3d <- function(path) {
  points <- gltf_scene_3d(path)$points
  if (is.null(points)) stop("The glTF file has no point primitives; read meshes with read_mesh_3d().", call. = FALSE)
  points
}

point_readers_3d <- list(ply = read_ply_points_3d, las = read_las_3d, xyz = read_xyz_3d, pts = read_xyz_3d,
  txt = read_xyz_3d, gltf = read_gltf_points_3d, glb = read_gltf_points_3d)
