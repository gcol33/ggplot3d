# Text as geometry: glyph outlines from the font file, laid out by the text
# shaper of 'systemfonts', in millimetres of a text-local plane. The outlines
# are drawn as projected paths (axis typography in a pane) or cut into scene
# faces (text lying in a plane of the data).

.outline_cache_3d <- new.env(parent = emptyenv())

# Size, in points, at which glyphs are outlined and shaped; outlines scale
# linearly to the requested size.
outline_reference_pt_3d <- 100

# Millimetres per point of a font size: grid sizes text in big points.
outline_mm_per_pt_3d <- 25.4 / 72

require_text_outline_3d <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("Install the 'systemfonts' package to draw text in a plane.", call. = FALSE)
  }
}

outline_font_3d <- function(family, face) {
  faces <- c("plain", "bold", "italic", "bold.italic")
  face <- if (is.numeric(face)) faces[face] else as.character(face)
  if (length(face) != 1L || is.na(face) || !face %in% faces) face <- "plain"
  family <- if (is.null(family) || is.na(family)) "" else as.character(family)
  font <- systemfonts::match_fonts(family, italic = face %in% c("italic", "bold.italic"),
    weight = if (face %in% c("bold", "bold.italic")) "bold" else "normal")
  list(family = family, face = face, path = font$path[[1L]], index = font$index[[1L]])
}

# Contours of one glyph at the reference size, in points: a list of n x 2
# matrices without repeated consecutive or closing points.
glyph_contours_3d <- function(font, glyph) {
  key <- paste("contours", font$path, font$index, glyph)
  cached <- .outline_cache_3d[[key]]
  if (!is.null(cached)) return(cached)
  outline <- systemfonts::glyph_outline(glyph, font$path, font$index, size = outline_reference_pt_3d, tolerance = 0.5)
  contours <- lapply(split(outline[c("x", "y")], outline$contour), function(c) {
    xy <- unname(as.matrix(c))
    n <- nrow(xy)
    keep <- c(TRUE, rowSums(abs(xy[-1L, , drop = FALSE] - xy[-n, , drop = FALSE])) > 0)
    xy <- xy[keep, , drop = FALSE]
    if (nrow(xy) > 1L && all(xy[1L, ] == xy[nrow(xy), ])) xy <- xy[-nrow(xy), , drop = FALSE]
    xy
  })
  contours <- unname(Filter(function(xy) nrow(xy) >= 3L && abs(ring_area_3d(xy, seq_len(nrow(xy)))) > 1e-9, contours))
  .outline_cache_3d[[key]] <- contours
  contours
}

# Faces covering one glyph at the reference size: vertices (n x 2, points)
# and index matrices of triangles and quadrilaterals winding counterclockwise.
glyph_faces_3d <- function(font, glyph) {
  key <- paste("faces", font$path, font$index, glyph)
  cached <- .outline_cache_3d[[key]]
  if (!is.null(cached)) return(cached)
  result <- winding_trapezoids_3d(glyph_contours_3d(font, glyph))
  .outline_cache_3d[[key]] <- result
  result
}

# Trapezoids covering the region of nonzero winding number of closed
# contours, the fill rule of TrueType and PostScript outlines, which holds
# for nested holes and overlapping contours alike. The plane is cut into
# horizontal slabs at every vertex height and every height where two edges
# cross; inside a slab no edges cross, so the edges met along a horizontal
# line keep their order and the winding number is constant between
# neighbours. Each span of nonzero winding is one trapezoid, a triangle where
# it closes to a point.
winding_trapezoids_3d <- function(contours) {
  empty <- list(vertices = matrix(numeric(), 0L, 2L), faces = list(matrix(integer(), 0L, 3L), matrix(integer(), 0L, 4L)))
  if (!length(contours)) return(empty)
  a <- do.call(rbind, contours)
  b <- do.call(rbind, lapply(contours, function(xy) xy[c(seq.int(2L, nrow(xy)), 1L), , drop = FALSE]))
  keep <- a[, 2L] != b[, 2L]
  a <- a[keep, , drop = FALSE]; b <- b[keep, , drop = FALSE]
  if (!nrow(a)) return(empty)
  winding <- sign(b[, 2L] - a[, 2L])
  low <- pmin(a[, 2L], b[, 2L]); high <- pmax(a[, 2L], b[, 2L])
  heights <- c(a[, 2L], b[, 2L])
  n <- nrow(a)
  if (n > 1L) {
    pairs <- which(upper.tri(matrix(TRUE, n, n)), arr.ind = TRUE)
    i <- pairs[, 1L]; j <- pairs[, 2L]
    pairs <- pairs[low[i] < high[j] & low[j] < high[i], , drop = FALSE]
    i <- pairs[, 1L]; j <- pairs[, 2L]
    r <- b[i, , drop = FALSE] - a[i, , drop = FALSE]; q <- b[j, , drop = FALSE] - a[j, , drop = FALSE]
    w <- a[j, , drop = FALSE] - a[i, , drop = FALSE]
    denominator <- r[, 1L] * q[, 2L] - r[, 2L] * q[, 1L]
    t <- (w[, 1L] * q[, 2L] - w[, 2L] * q[, 1L]) / denominator
    u <- (w[, 1L] * r[, 2L] - w[, 2L] * r[, 1L]) / denominator
    crossing <- denominator != 0 & t > 0 & t < 1 & u > 0 & u < 1
    heights <- c(heights, a[i[crossing], 2L] + t[crossing] * r[crossing, 2L])
  }
  heights <- sort(unique(heights))
  span <- diff(range(heights))
  heights <- heights[c(TRUE, diff(heights) > 1e-9 * span)]
  x_at <- function(rows, y) a[rows, 1L] + (y - a[rows, 2L]) * (b[rows, 1L] - a[rows, 1L]) / (b[rows, 2L] - a[rows, 2L])
  pieces <- vector("list", length(heights) - 1L)
  for (s in seq_len(length(heights) - 1L)) {
    middle <- (heights[s] + heights[s + 1L]) / 2
    active <- which(low < middle & high > middle)
    if (length(active) < 2L) next
    active <- active[order(x_at(active, middle))]
    inside <- cumsum(winding[active])[-length(active)] != 0
    if (any(inside)) pieces[[s]] <- cbind(left = active[-length(active)][inside], right = active[-1L][inside], slab = s)
  }
  spans <- do.call(rbind, pieces)
  if (is.null(spans)) return(empty)
  # Spans between the same two edges in consecutive slabs join into one.
  spans <- spans[order(spans[, "left"], spans[, "right"], spans[, "slab"]), , drop = FALSE]
  m <- nrow(spans)
  first <- c(TRUE, spans[-1L, "left"] != spans[-m, "left"] | spans[-1L, "right"] != spans[-m, "right"] |
    spans[-1L, "slab"] != spans[-m, "slab"] + 1L)
  run <- cumsum(first)
  y0 <- heights[spans[first, "slab"]]
  y1 <- heights[tapply(spans[, "slab"], run, max) + 1L]
  left <- spans[first, "left"]; right <- spans[first, "right"]
  spans <- cbind(x_at(left, y0), x_at(right, y0), x_at(right, y1), x_at(left, y1), y0, y1)
  tolerance <- 1e-9 * span
  bottom <- spans[, 2L] - spans[, 1L] > tolerance; top <- spans[, 3L] - spans[, 4L] > tolerance
  spans <- spans[bottom | top, , drop = FALSE]; bottom <- bottom[bottom | top]; top <- top[bottom | top]
  corners <- lapply(seq_len(nrow(spans)), function(k) {
    p <- rbind(c(spans[k, 1L], spans[k, 5L]), c(spans[k, 2L], spans[k, 5L]), c(spans[k, 3L], spans[k, 6L]), c(spans[k, 4L], spans[k, 6L]))
    if (!bottom[k]) p[-2L, , drop = FALSE] else if (!top[k]) p[-4L, , drop = FALSE] else p
  })
  counts <- vapply(corners, nrow, integer(1))
  start <- cumsum(c(0L, head(counts, -1L)))
  index <- function(size) {
    rows <- which(counts == size)
    matrix(start[rows] + rep(seq_len(size), each = length(rows)), ncol = size)
  }
  list(vertices = do.call(rbind, corners), faces = list(index(3L), index(4L)))
}

# Height of a capital above the baseline, in points at the reference size;
# grid measures the height of a line of text the same way.
cap_height_3d <- function(font) {
  key <- paste("cap", font$path, font$index)
  cached <- .outline_cache_3d[[key]]
  if (!is.null(cached)) return(cached)
  glyph <- systemfonts::shape_string("M", path = font$path, index = font$index, size = outline_reference_pt_3d)$shape$index
  contours <- glyph_contours_3d(font, glyph)
  height <- if (length(contours)) max(vapply(contours, function(xy) max(xy[, 2L]), numeric(1))) else 0.7 * outline_reference_pt_3d
  .outline_cache_3d[[key]] <- height
  height
}

# Shaped glyphs of a label with their pen positions, and the text box of
# grid's justification: from the last baseline to the cap height of the first
# line, across the advance width. Lengths in points at the reference size.
shape_label_3d <- function(label, font) {
  shaped <- systemfonts::shape_string(label, path = font$path, index = font$index, size = outline_reference_pt_3d)
  glyphs <- shaped$shape[!shaped$shape$glyph %in% c("\n", "\r", "\t"), , drop = FALSE]
  bottom <- if (nrow(shaped$shape)) min(shaped$shape$y_offset) else 0
  list(glyphs = glyphs, box = c(0, bottom, shaped$metrics$width[1L], cap_height_3d(font)))
}

check_outline_label_3d <- function(label) {
  if (is.language(label) || is.expression(label)) {
    stop("Text in a plane needs character labels; plotmath expressions are drawn on screen only.", call. = FALSE)
  }
  as.character(label)
}

# Justified text-local placement in millimetres: a function of reference
# points giving millimetres, and the box width and height in millimetres.
outline_placement_3d <- function(shape, fontsize, hjust, vjust) {
  mm <- fontsize * outline_mm_per_pt_3d / outline_reference_pt_3d
  box <- shape$box
  origin <- c(box[1L] + hjust * (box[3L] - box[1L]), box[2L] + vjust * (box[4L] - box[2L]))
  list(place = function(xy, offset) cbind(xy[, 1L] + offset[1L] - origin[1L], xy[, 2L] + offset[2L] - origin[2L]) * mm,
    size = c(box[3L] - box[1L], box[4L] - box[2L]) * mm)
}

# Outline of a label as closed contours in millimetres: x, y and a contour id,
# with the text box size. `fontsize` in points.
text_outline_3d <- function(label, fontsize, family = "", face = "plain", hjust = 0.5, vjust = 0.5) {
  require_text_outline_3d()
  label <- check_outline_label_3d(label)
  font <- outline_font_3d(family, face)
  shape <- shape_label_3d(label, font)
  placement <- outline_placement_3d(shape, fontsize, hjust, vjust)
  pieces <- list(); id <- 0L
  for (g in seq_len(nrow(shape$glyphs))) {
    offset <- c(shape$glyphs$x_offset[g], shape$glyphs$y_offset[g])
    for (contour in glyph_contours_3d(font, shape$glyphs$index[g])) {
      id <- id + 1L
      pieces[[id]] <- cbind(placement$place(contour, offset), id)
    }
  }
  xy <- if (length(pieces)) do.call(rbind, pieces) else matrix(numeric(), 0L, 3L)
  list(x = xy[, 1L], y = xy[, 2L], id = as.integer(xy[, 3L]), size = placement$size)
}

# A label as a planar mesh in millimetres, in the form of a glyph instancing
# unit: vertices (z = 0), faces grouped by corner count winding
# counterclockwise seen from +z, no edges.
text_mesh_3d <- function(label, fontsize, family = "", face = "plain", hjust = 0.5, vjust = 0.5) {
  require_text_outline_3d()
  label <- check_outline_label_3d(label)
  font <- outline_font_3d(family, face)
  shape <- shape_label_3d(label, font)
  placement <- outline_placement_3d(shape, fontsize, hjust, vjust)
  vertices <- list(); triangles <- list(); quads <- list(); n <- 0L
  for (g in seq_len(nrow(shape$glyphs))) {
    mesh <- glyph_faces_3d(font, shape$glyphs$index[g])
    if (!nrow(mesh$vertices)) next
    vertices[[length(vertices) + 1L]] <- placement$place(mesh$vertices, c(shape$glyphs$x_offset[g], shape$glyphs$y_offset[g]))
    triangles[[length(triangles) + 1L]] <- mesh$faces[[1L]] + n
    quads[[length(quads) + 1L]] <- mesh$faces[[2L]] + n
    n <- n + nrow(mesh$vertices)
  }
  xy <- if (length(vertices)) do.call(rbind, vertices) else matrix(numeric(), 0L, 2L)
  faces <- list(do.call(rbind, c(list(matrix(integer(), 0L, 3L)), triangles)), do.call(rbind, c(list(matrix(integer(), 0L, 4L)), quads)))
  list(vertices = cbind(xy, 0), faces = faces[vapply(faces, nrow, integer(1)) > 0L],
    edges = matrix(integer(), 0L, 2L), sphere = FALSE, size = placement$size)
}
