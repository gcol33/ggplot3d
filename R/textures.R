#' Images for textured faces
#'
#' Wraps an image so that surface layers can drape it over their faces and
#' [annotation_raster_3d()] can lay it on a plane. Texture coordinates `u`
#' (left to right) and `v` (bottom to top) run from 0 to 1 across the image,
#' as in OpenGL and rgl; points outside the image take the face's fill, or
#' nothing when the fill is `NA`. The image is painted over the fill, so a
#' transparent image region shows the fill.
#'
#' The raster backend and the [view_3d()] viewer sample the image per
#' sample, perspective-correctly, in linear light: bilinearly where the image
#' is magnified and from a mipmap (Williams 1983) where it is minified. The
#' vector backend draws the image once per face triangle through an affine
#' group transformation clipped to the triangle (R >= 4.2 groups, Murrell
#' 2021). Under an orthographic camera every triangle maps the image affinely,
#' which is exact; under perspective, triangles are split until the affine
#' map lies within half a device pixel of the projective one. Raster devices
#' resample the image with their own filter, in encoded sRGB. svglite writes
#' transformation matrices with two significant digits, which can misplace
#' the image by about 1% of its size; ragg, pdf(), cairo_pdf() and svg() keep
#' it in place.
#' [view_3d()] and glTF export ([export_3d()]) carry the image and the
#' texture coordinates.
#' @param image A `nativeRaster` (such as `png::readPNG(native = TRUE)`), a
#'   raster or matrix of colours, a numeric matrix of grey levels or a numeric
#'   array of grey, grey and alpha, RGB or RGBA channels in \[0, 1\] (such as
#'   `png::readPNG()`), or the path of a PNG file (needs png). Row one is the
#'   top of the image.
#' @param interpolate Whether to interpolate between pixels (bilinear and
#'   mipmapped sampling) or to show them as sharp squares, as
#'   [grid::rasterGrob()] does.
#' @param extent `NULL`, or four numbers `c(umin, umax, vmin, vmax)`: the
#'   range of the layer's texture coordinates that the image covers. A surface
#'   layer without explicit `u` and `v` uses its x and y positions (its
#'   parameters for [stat_parametric_3d()], its texture coordinates for an
#'   rgl mesh), so an extent in data units places an orthophoto over a
#'   terrain. `NULL` stretches the image over the range of the layer's
#'   coordinates.
#' @return A `ggplot3d_texture` object.
#' @references Williams (1983). Pyramidal parametrics. SIGGRAPH Computer
#'   Graphics 17(3), 1-11. \doi{10.1145/964967.801126}
#'
#'   Murrell (2021). Groups, compositing operators, and affine
#'   transformations in R graphics. Technical report, University of Auckland.
#' @seealso [annotation_raster_3d()], [geom_raster_3d()] and the `texture`
#'   argument of [geom_surface_3d()] and [geom_mesh_3d()].
#' @examples
#' checker <- matrix(c("black", "white"), 8, 8)
#' checker[] <- ifelse((row(checker) + col(checker)) %% 2 == 0, "black", "white")
#' tex <- texture_3d(checker, interpolate = FALSE)
#' tex
#' d <- expand.grid(x = seq(-1, 1, length.out = 21), y = seq(-1, 1, length.out = 21))
#' d$z <- with(d, exp(-2 * (x^2 + y^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(texture = tex, shade = FALSE) +
#'   coord_3d()
#' @export
texture_3d <- function(image, interpolate = TRUE, extent = NULL) {
  if (inherits(image, "ggplot3d_texture")) {
    texture <- image
    if (missing(interpolate)) interpolate <- image$interpolate
    if (missing(extent)) extent <- image$extent
  } else {
    texture <- list(image = texture_native_3d(image))
    texture$opaque <- all(texture_alpha_3d(texture$image) == 255L)
  }
  if (!is.logical(interpolate) || length(interpolate) != 1L || is.na(interpolate)) stop("interpolate must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(extent) && (!is.numeric(extent) || length(extent) != 4L || any(!is.finite(extent)) ||
      extent[1L] == extent[2L] || extent[3L] == extent[4L])) {
    stop("extent must be four finite numbers c(umin, umax, vmin, vmax) with distinct ends.", call. = FALSE)
  }
  texture$interpolate <- interpolate
  texture$extent <- if (is.null(extent)) NULL else as.double(extent)
  class(texture) <- "ggplot3d_texture"
  texture
}

#' @export
print.ggplot3d_texture <- function(x, ...) {
  size <- dim(x$image)
  cat("<ggplot3d texture> ", size[2L], " x ", size[1L], " pixels", if (x$opaque) ", opaque" else ", with transparency",
    if (x$interpolate) ", interpolated" else ", sharp pixels", "\n", sep = "")
  if (!is.null(x$extent)) cat("extent: u ", x$extent[1L], " to ", x$extent[2L], ", v ", x$extent[3L], " to ", x$extent[4L], "\n", sep = "")
  invisible(x)
}

# A texture argument of a layer: NULL, a texture or an image texture_3d() reads.
texture_argument_3d <- function(texture) {
  if (is.null(texture)) return(NULL)
  texture_3d(texture)
}

# An image as a nativeRaster: an integer matrix of packed 8-bit encoded sRGB
# channels R | G << 8 | B << 16 | A << 24, stored row by row from the top.
texture_native_3d <- function(image) {
  if (inherits(image, "nativeRaster")) {
    if (length(dim(image)) != 2L || any(dim(image) < 1L)) stop("A texture image needs at least one pixel.", call. = FALSE)
    return(image)
  }
  if (is.character(image) && length(image) == 1L && is.null(dim(image))) {
    rlang::check_installed("png", reason = "to read a PNG texture.")
    if (!file.exists(image)) stop("Texture file not found: ", image, call. = FALSE)
    return(png::readPNG(image, native = TRUE))
  }
  if (inherits(image, "raster") || (is.character(image) && length(dim(image)) == 2L)) {
    colours <- as.matrix(image)
    rgba <- matrix(as.vector(colour_channels_3d(t(colours), alpha = TRUE)), 4L)
    return(texture_pack_3d(rgba, nrow(colours), ncol(colours)))
  }
  if (is.numeric(image) && length(dim(image)) %in% 2:3) {
    channels <- if (length(dim(image)) == 2L) 1L else dim(image)[3L]
    if (!channels %in% 1:4) stop("A texture array needs one to four channels.", call. = FALSE)
    if (any(!is.finite(image)) || any(image < 0 | image > 1)) stop("Texture channels must lie in [0, 1].", call. = FALSE)
    h <- dim(image)[1L]; w <- dim(image)[2L]
    plane <- function(k) as.vector(t(matrix(image[seq_len(h * w) + (k - 1L) * h * w], h, w)))
    grey <- channels <= 2L
    rgb <- if (grey) rep(list(plane(1L)), 3L) else lapply(1:3, plane)
    alpha <- if (channels %in% c(2L, 4L)) plane(channels) else rep(1, h * w)
    rgba <- round(255 * rbind(rgb[[1L]], rgb[[2L]], rgb[[3L]], alpha))
    return(texture_pack_3d(rgba, h, w))
  }
  stop("A texture image must be a nativeRaster, a raster or colour matrix, a numeric matrix or array in [0, 1], or a PNG file path.", call. = FALSE)
}

# 4 x n matrix of 0-255 channels (pixels row by row from the top) packed into
# a nativeRaster. The pattern of a packed pixel with alpha >= 128 lies above
# the integer range and is wrapped to its two's-complement bits.
texture_pack_3d <- function(rgba, height, width) {
  if (height < 1L || width < 1L) stop("A texture image needs at least one pixel.", call. = FALSE)
  packed <- rgba[1L, ] + 256 * rgba[2L, ] + 65536 * rgba[3L, ] + 16777216 * rgba[4L, ]
  packed <- ifelse(packed >= 2^31, packed - 2^32, packed)
  structure(suppressWarnings(as.integer(packed)), dim = c(as.integer(height), as.integer(width)),
    class = "nativeRaster", channels = 4L)
}

# 4 x n matrix of the 0-255 channels of a nativeRaster, pixels row by row.
texture_channels_3d <- function(native) {
  bits <- unclass(native)
  value <- as.double(bits)
  value[is.na(bits)] <- -2^31
  value <- ifelse(value < 0, value + 2^32, value)
  rbind(value %% 256, (value %/% 256) %% 256, (value %/% 65536) %% 256, value %/% 16777216)
}

texture_alpha_3d <- function(native) as.integer(texture_channels_3d(native)[4L, ])

# Uncompressed-filter PNG (colour type 6, 8-bit RGBA) of a nativeRaster.
texture_png_3d <- function(native) {
  h <- nrow(native); w <- ncol(native)
  rows <- matrix(as.raw(texture_channels_3d(native)), 4L * w, h)
  body <- as.vector(rbind(as.raw(0L), rows))
  header <- c(png_uint32_3d(w), png_uint32_3d(h), as.raw(c(8L, 6L, 0L, 0L, 0L)))
  c(as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)), png_chunk_3d("IHDR", header),
    png_chunk_3d("IDAT", memCompress(body, "gzip")), png_chunk_3d("IEND", raw()))
}

# The panel's textures: the distinct texture_id values of `styles` in
# increasing order, the texture of each, and per style row its index into
# them (0 for none).
panel_textures_3d <- function(styles) {
  id <- styles$texture_id
  if (is.null(id) || is.null(styles$texture) || all(is.na(id))) return(list(textures = list(), index = integer(nrow(styles))))
  ids <- sort(unique(id[!is.na(id)]))
  textures <- lapply(ids, function(k) styles$texture[[match(k, id)]])
  index <- match(id, ids)
  index[is.na(index)] <- 0L
  list(textures = textures, index = index)
}

# Opacity just below one: the engine and the backends treat a face below full
# opacity as one that does not hide what lies behind it, and the difference
# is far below 8-bit colour resolution. Textured faces that may show through
# (a transparent image or no fill under it) carry it.
translucent_opacity_3d <- 1 - 2^-24

# Texture coordinates of a textured layer's rows, before positions are
# normalised: kept where a stat or mesh supplied .texture_u/.texture_v, else
# the u/v aesthetics, else the x and y positions in transformed scale units.
texture_coordinates_3d <- function(data) {
  if (all(c(".texture_u", ".texture_v") %in% names(data))) return(data)
  source <- if (all(c("u", "v") %in% names(data))) c("u", "v") else c("x", "y")
  data$.texture_u <- as.double(data[[source[1L]]])
  data$.texture_v <- as.double(data[[source[2L]]])
  data
}

# Texture coordinates of the corners of `primitives`: .texture_u/.texture_v
# mapped from the texture extent (default their range over the face corners)
# to [0, 1]. Corners of other primitives carry NA.
texture_corners_3d <- function(data, primitives, kinds, counts, texture) {
  owner <- rep.int(seq_along(counts), counts)
  face <- kinds[owner] == "faces"
  cu <- as.double(data$.texture_u[primitives] %or3d% rep(NA_real_, length(primitives)))
  cv <- as.double(data$.texture_v[primitives] %or3d% rep(NA_real_, length(primitives)))
  extent <- texture$extent
  if (is.null(extent)) {
    span <- function(x) {
      r <- suppressWarnings(range(x[face & is.finite(x)]))
      if (!all(is.finite(r))) r <- c(0, 1)
      if (r[1L] == r[2L]) r <- r + c(-0.5, 0.5)
      r
    }
    extent <- c(span(cu), span(cv))
  }
  u <- (cu - extent[1L]) / (extent[2L] - extent[1L])
  v <- (cv - extent[3L]) / (extent[4L] - extent[3L])
  u[!face | !is.finite(u)] <- NA_real_
  v[!face | !is.finite(v)] <- NA_real_
  list(u = u, v = v)
}

# Styles and face opacities of a textured layer: every style names texture
# 1; a face without a fill keeps the layer alpha as its opacity. A face that
# may show through (a transparent image, or no fill where a corner lies
# outside the image) carries at most translucent_opacity_3d.
texture_styles_3d <- function(material, data, rows, kinds, counts, texture, uv) {
  material$styles$texture_id <- rep(1L, nrow(material$styles))
  material$styles$texture <- I(rep(list(texture), nrow(material$styles)))
  face <- which(kinds == "faces")
  if (length(face)) {
    fill <- material$styles$fill[material$index[face]]
    alpha <- if (is.null(data$alpha)) rep(1, length(face)) else data$alpha[rows[face]]
    alpha[is.na(alpha)] <- 1
    bare <- is.na(fill)
    material$opacity[face[bare]] <- alpha[bare]
    owner <- rep.int(seq_along(counts), counts)
    outside <- !is.finite(uv$u) | !is.finite(uv$v) | uv$u < 0 | uv$u > 1 | uv$v < 0 | uv$v > 1
    leaves <- as.vector(rowsum(as.integer(outside), owner, reorder = TRUE))[face] > 0L
    through <- !texture$opaque | (bare & leaves)
    material$opacity[face[through]] <- pmin(material$opacity[face[through]], translucent_opacity_3d)
  }
  material
}
