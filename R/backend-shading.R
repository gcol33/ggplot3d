# Smooth shading of compiled faces: colours at fragment vertices from the
# interpolated corner attributes, and their vector rendering.

corner_attribute_columns_3d <- c("normal_x", "normal_y", "normal_z", "red", "green", "blue")

# Corner colours are encoded sRGB channels and the faces they paint are
# interpolated in linear light (Gouraud over the decoded colours), so a point
# added on a face by clipping, munching or splitting takes the linear-light
# interpolation of the corner colours: the split face paints as the whole one.
# Other corner attributes interpolate as stored.
corner_colour_columns_3d <- c("red", "green", "blue")

corner_linear_3d <- function(corners, name) {
  if (name %in% corner_colour_columns_3d) srgb_decode_3d(corners[[name]]) else corners[[name]]
}

corner_encoded_3d <- function(name, value) {
  if (name %in% corner_colour_columns_3d) srgb_encode_3d(pmin(pmax(value, 0), 1)) else value
}

# Corner attributes at the compiled vertices of a scene, colours in linear light.
scene_linear_attributes_3d <- function(scene, columns = corner_attribute_columns_3d) {
  corners <- scene$source$corners
  for (name in intersect(columns, names(corners))) corners[[name]] <- corner_linear_3d(corners, name)
  scene$source$corners <- corners
  scene_vertex_attributes_3d(scene, columns)
}

# Encoded-sRGB tolerance of vector smooth shading.
shading_tolerance_3d <- function() {
  tolerance <- getOption("ggplot3d.shading_tolerance", 2 / 255)
  scene_assert(is.numeric(tolerance) && length(tolerance) == 1L && is.finite(tolerance) && tolerance > 0,
               "option ggplot3d.shading_tolerance must be a positive number")
  tolerance
}

# Unit model-space normals of face fragments `rows`: the style normal where a
# layer supplies one, else the fragment's face normal.
face_shading_normals_3d <- function(fragments, rows, camera, styles = NULL) {
  normal <- cbind(fragments$normal_x[rows], fragments$normal_y[rows], fragments$normal_z[rows])
  if (all(c("normal_x", "normal_y", "normal_z") %in% names(styles))) {
    supplied <- as.matrix(styles[c("normal_x", "normal_y", "normal_z")])
    intrinsic <- styles$intrinsic_normal %in% TRUE
    supplied <- sweep(supplied, 2L, camera$model_scale, "/")
    if (length(intrinsic)) supplied[intrinsic, ] <- as.matrix(styles[intrinsic, c("normal_x", "normal_y", "normal_z")])
    lengths <- sqrt(rowSums(supplied^2))
    valid <- is.finite(lengths) & lengths > 0
    normal[valid, ] <- supplied[valid, , drop = FALSE] / lengths[valid]
  }
  normal
}

# Face fragments whose colour varies over the fragment: some vertex carries a
# corner colour, or a corner normal on a lit face.
smooth_fragments_3d <- function(scene, attributes, styles, light) {
  f <- scene$fragments
  smooth <- logical(length(f$kind))
  if (is.null(attributes)) return(smooth)
  faces <- which(f$kind == 1L)
  if (!length(faces)) return(smooth)
  counts <- f$count[faces]
  index <- rep(f$offset[faces], counts) + sequence(counts) - 1L
  owner <- rep(seq_along(faces), counts)
  has <- function(columns) {
    columns <- intersect(columns, colnames(attributes))
    if (length(columns) < 3L) return(logical(length(faces)))
    ok <- rowSums(is.finite(attributes[index, columns, drop = FALSE])) == 3L
    as.vector(rowsum(as.integer(ok), owner)) > 0L
  }
  lit <- !is.null(light) & style_value_3d(styles[faces, , drop = FALSE], "shade", FALSE) %in% TRUE
  smooth[faces] <- has(c("red", "green", "blue")) | (lit & has(c("normal_x", "normal_y", "normal_z")))
  smooth
}

# Linear-light colours at the vertices of face fragments `rows` (one row per
# vertex, fragments in order) from the linear vertex attributes of
# scene_linear_attributes_3d(): the corner colour where interpolated, else the
# style fill, lit at the interpolated corner normal where one exists, else at
# the face normal. Gouraud (1971) shading over the clipped fragment.
fragment_vertex_linear_3d <- function(scene, rows, styles, attributes, light) {
  f <- scene$fragments
  counts <- f$count[rows]
  index <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  owner <- rep(seq_along(rows), counts)
  linear <- linear_rgb_3d(styles$fill)[owner, , drop = FALSE]
  columns <- colnames(attributes)
  if (all(c("red", "green", "blue") %in% columns)) {
    rgb <- attributes[index, c("red", "green", "blue"), drop = FALSE]
    ok <- rowSums(is.finite(rgb)) == 3L
    linear[ok, ] <- rgb[ok, , drop = FALSE]
  }
  lit <- !is.null(light) & (style_value_3d(styles, "shade", FALSE) %in% TRUE)[owner]
  if (any(lit)) {
    normal <- face_shading_normals_3d(f, rows, scene$camera, styles)[owner, , drop = FALSE]
    if (all(c("normal_x", "normal_y", "normal_z") %in% columns)) {
      corner <- attributes[index, c("normal_x", "normal_y", "normal_z"), drop = FALSE]
      ok <- rowSums(is.finite(corner)) == 3L & rowSums(corner^2) > 0
      normal[ok, ] <- model_normals_3d(corner[ok, , drop = FALSE], scene$camera)
    }
    position <- sweep(cbind(scene$vertices$x[index], scene$vertices$y[index], scene$vertices$z[index]), 2L,
      scene$camera$model_scale, "*")
    linear[lit, ] <- shade_linear_3d(linear[lit, , drop = FALSE], normal[lit, , drop = FALSE], scene$camera, light,
      position[lit, , drop = FALSE], material_index_3d(styles)[owner][lit], scene$materials)
  }
  list(linear = pmin(pmax(linear, 0), 1), owner = owner)
}

# How the vector backend paints a run of smooth faces whose colour a single
# linear gradient cannot follow: "auto" draws the run as one image clipped
# to the faces' outline when any triangle's colour varies over two screen
# directions by more than the tolerance, "subdivide" splits such triangles
# into flat pieces.
smooth_vector_mode_3d <- function() {
  mode <- getOption("ggplot3d.smooth_vector", "auto")
  scene_assert(is.character(mode) && length(mode) == 1L && mode %in% c("auto", "subdivide"),
               "option ggplot3d.smooth_vector must be \"auto\" or \"subdivide\"")
  mode
}

# Resolution in dots per inch of the images of smooth face runs: the device
# resolution, at least the option ggplot3d.smooth_dpi (default 150) so that
# images in vector files stay sharp when enlarged.
smooth_image_dpi_3d <- function(viewport) {
  dpi <- getOption("ggplot3d.smooth_dpi", 150)
  scene_assert(is.numeric(dpi) && length(dpi) == 1L && is.finite(dpi) && dpi > 0,
               "option ggplot3d.smooth_dpi must be a positive number")
  max(viewport$dpi, dpi)
}

# Fan triangulation of faces with per-vertex linear colours: for each
# triangle of a face whose colour varies, its face, corners P and colours L
# (lists of three matrices), which of its edges lie on the face outline,
# whether one linear gradient paints it (the colour differences of its
# corners are parallel and not all zero, as for a lit face of one colour,
# and the device draws gradients) and the largest difference of its encoded
# corner colours. x_mm, y_mm are vertex positions in mm, id numbers faces
# 1..n with consecutive vertices.
smooth_fan_3d <- function(x_mm, y_mm, id, linear, gradients) {
  counts <- tabulate(id); start <- cumsum(c(1L, head(counts, -1L)))
  deviation <- rowSums(abs(linear - linear[start[id], , drop = FALSE]))
  flat_face <- as.vector(rowsum(deviation, id, reorder = TRUE)) <= 1e-12
  varied <- which(!flat_face)
  n <- counts[varied] - 2L
  k <- sequence(n)
  face <- rep(varied, n); a <- rep(start[varied], n); b <- a + k; c <- b + 1L
  L <- list(linear[a, , drop = FALSE], linear[b, , drop = FALSE], linear[c, , drop = FALSE])
  P <- list(cbind(x_mm[a], y_mm[a]), cbind(x_mm[b], y_mm[b]), cbind(x_mm[c], y_mm[c]))
  d1 <- L[[2L]] - L[[1L]]; d2 <- L[[3L]] - L[[1L]]
  parallel <- sqrt(rowSums(cross_rows_3d(d1, d2)^2)) <= 1e-9 * pmax(sqrt(rowSums(d1^2) * rowSums(d2^2)), 1e-300)
  area <- (P[[2L]][, 1L] - P[[1L]][, 1L]) * (P[[3L]][, 2L] - P[[1L]][, 2L]) -
    (P[[3L]][, 1L] - P[[1L]][, 1L]) * (P[[2L]][, 2L] - P[[1L]][, 2L])
  scale <- pmax(rowSums((P[[2L]] - P[[1L]])^2), rowSums((P[[3L]] - P[[1L]])^2))
  varies <- rowSums(d1^2) + rowSums(d2^2) > 0
  list(flat_face = flat_face, counts = counts, start = start, face = face, P = P, L = L,
       outer = cbind(k == 1L, rep(TRUE, length(k)), k == rep(n, n)),
       gradient = gradients & varies & parallel & abs(area) > 1e-8 * scale,
       range = colour_range_3d(L))
}

# Largest per-channel difference of the encoded corner colours of triangles.
colour_range_3d <- function(L) {
  if (!nrow(L[[1L]])) return(numeric())
  E <- lapply(L, srgb_encode_3d)
  apply(pmax(abs(E[[1L]] - E[[2L]]), abs(E[[2L]] - E[[3L]]), abs(E[[1L]] - E[[3L]])), 1L, max)
}

# Colour halfway between the smallest and largest encoded corner colours of
# triangles with linear corner colours L, per channel.
midrange_rgb_3d <- function(L) {
  E <- lapply(L, function(l) srgb_encode_3d(pmin(pmax(l, 0), 1)))
  mid <- (pmax(E[[1L]], E[[2L]], E[[3L]]) + pmin(E[[1L]], E[[2L]], E[[3L]])) / 2
  grDevices::rgb(mid[, 1L], mid[, 2L], mid[, 3L])
}

# Whether a run of smooth faces is painted as an image (smooth_vector_mode_3d()).
smooth_image_needed_3d <- function(x_mm, y_mm, id, linear, gradients, tolerance = shading_tolerance_3d()) {
  if (smooth_vector_mode_3d() != "auto") return(FALSE)
  fan <- smooth_fan_3d(x_mm, y_mm, id, linear, gradients)
  any(!fan$gradient & fan$range > tolerance)
}

# Vector paint of faces with per-vertex linear colours. Each face is
# fan-triangulated (smooth_fan_3d()). A triangle that one linear gradient
# paints becomes that gradient, with stops along the sRGB curve until the
# device's encoded interpolation is within the tolerance. Other triangles are
# split into four until the encoded corner colours of each piece differ by
# at most the tolerance, or its halves would be narrower than `pixel_mm`, at
# most `max_level` times (4^max_level pieces per triangle), and each
# piece is filled with the midrange of its encoded corner colours: the colour
# is affine in linear light over a piece and the encoding is monotone, so
# each channel takes its extremes at corners and every point of the piece
# lies within half the corner range of that colour. The pieces overlap by
# half of `pixel_mm` inside the face (subdivide_colour_triangles_3d()), so
# each piece covers the antialiased edge of the one before instead of
# letting the background through. A face of one colour stays one polygon. Returns shapes (x_mm,
# y_mm, id) in face order with their face, flat encoded colour, whether they
# are gradients and the gradient patterns (NULL for flat shapes).
smooth_face_paint_3d <- function(x_mm, y_mm, id, linear, gradients, tolerance = shading_tolerance_3d(), pixel_mm = 0,
                                 max_level = 4L) {
  fan <- smooth_fan_3d(x_mm, y_mm, id, linear, gradients)
  counts <- fan$counts; start <- fan$start; flat_face <- fan$flat_face
  shapes <- list(x = list(), y = list(), count = list(), face = list(), flat = list(), gradient = list(), paint = list())
  add <- function(x, y, count, face, flat, gradient, paint) {
    values <- list(x = x, y = y, count = count, face = face, flat = flat, gradient = gradient, paint = paint)
    for (name in names(values)) shapes[[name]][[length(shapes[[name]]) + 1L]] <<- values[[name]]
  }
  plain <- which(flat_face)
  if (length(plain)) {
    keep <- flat_face[id]
    add(x_mm[keep], y_mm[keep], counts[plain], plain, encode_rgb_3d(linear[start[plain], , drop = FALSE]),
      rep(FALSE, length(plain)), vector("list", length(plain)))
  }
  P <- fan$P; L <- fan$L; face <- fan$face
  gradient <- which(fan$gradient)
  if (length(gradient)) {
    made <- lapply(gradient, function(j) triangle_gradient_3d(lapply(P, function(p) p[j, ]), lapply(L, function(l) l[j, ]), tolerance))
    add(as.vector(rbind(P[[1L]][gradient, 1L], P[[2L]][gradient, 1L], P[[3L]][gradient, 1L])),
      as.vector(rbind(P[[1L]][gradient, 2L], P[[2L]][gradient, 2L], P[[3L]][gradient, 2L])),
      rep.int(3L, length(gradient)), face[gradient], vapply(made, function(m) m$flat, character(1)),
      rep(TRUE, length(gradient)), lapply(made, function(m) m$paint))
  }
  split <- setdiff(seq_along(face), gradient)
  if (length(split)) {
    pieces <- subdivide_colour_triangles_3d(lapply(P, function(p) p[split, , drop = FALSE]),
      lapply(L, function(l) l[split, , drop = FALSE]), tolerance, fan$outer[split, , drop = FALSE], pixel_mm, max_level)
    m <- length(pieces$owner)
    add(as.vector(rbind(pieces$P[[1L]][, 1L], pieces$P[[2L]][, 1L], pieces$P[[3L]][, 1L])),
      as.vector(rbind(pieces$P[[1L]][, 2L], pieces$P[[2L]][, 2L], pieces$P[[3L]][, 2L])),
      rep.int(3L, m), face[split][pieces$owner], midrange_rgb_3d(pieces$L),
      rep(FALSE, m), vector("list", m))
  }
  count <- unlist(shapes$count, use.names = FALSE)
  face <- unlist(shapes$face, use.names = FALSE)
  first <- cumsum(c(1L, head(count, -1L)))
  order <- order(face, method = "radix")
  vertex <- rep(first[order], count[order]) + sequence(count[order]) - 1L
  list(x_mm = unlist(shapes$x, use.names = FALSE)[vertex], y_mm = unlist(shapes$y, use.names = FALSE)[vertex],
       id = rep(seq_along(order), count[order]), face = face[order],
       flat = unlist(shapes$flat, use.names = FALSE)[order],
       gradient = unlist(shapes$gradient, use.names = FALSE)[order],
       paint = unlist(shapes$paint, recursive = FALSE)[order])
}

# Image of a run of smooth faces at the colours of smooth_faces_raster_cpp(),
# clipped to the union of the faces, for runs whose colour no linear
# gradient follows. Rasterising Gouraud shading in vector output follows
# matplotlib's Agg path and stands in for the mesh shadings (PDF shading
# types 4 to 7, used by Asymptote, pgfplots and matplotlib's PDF backend)
# that R graphics cannot emit. x, y are vertex positions in npc of the
# viewport, id numbers faces 1..n with consecutive vertices, opacity is per
# face. `outer` is a clip region that the image's own clipping path would
# replace; it is applied as an alpha mask instead.
smooth_image_grob_3d <- function(x, y, id, linear, opacity, viewport, outer = NULL, caps = grid_capabilities_3d()) {
  if (!caps$clipping_paths) stop("The vector device does not support clipping paths required by smooth shading.", call. = FALSE)
  pixel <- 25.4 / smooth_image_dpi_3d(viewport)
  x_mm <- x * viewport$width_mm; y_mm <- y * viewport$height_mm
  reach <- 2
  lo <- c(floor(min(x_mm) / pixel) - reach, floor(min(y_mm) / pixel) - reach) * pixel
  size <- pmax(1L, as.integer(ceiling((c(max(x_mm), max(y_mm)) - lo) / pixel) + reach))
  image <- smooth_faces_raster_cpp((x_mm - lo[1L]) / pixel, (y_mm - lo[2L]) / pixel, as.integer(id), linear,
    as.numeric(opacity), size[1L], size[2L], reach)
  ring <- counter_clockwise_3d(x, y, id)
  outline <- grid::pathGrob(ring$x, ring$y, id = id, rule = "winding")
  vp <- if (is.null(outer)) grid::viewport(clip = outline) else {
    if (is.null(caps$mask_type)) stop("The vector device does not support the masks required by clipped smooth shading.", call. = FALSE)
    outer$gp <- grid::gpar(fill = "white", col = NA, alpha = 1)
    grid::viewport(clip = outline, mask = grid::as.mask(outer, type = caps$mask_type))
  }
  grid::rasterGrob(image, x = lo[1L] / viewport$width_mm, y = lo[2L] / viewport$height_mm,
    width = size[1L] * pixel / viewport$width_mm, height = size[2L] * pixel / viewport$height_mm,
    just = c("left", "bottom"), interpolate = TRUE, vp = vp)
}

# Grob of painted smooth shapes (smooth_face_paint_3d()) of faces with styles
# and opacity: consecutive gradient shapes and consecutive flat shapes are
# drawn as one polygon grob each, in face order, over a seam underlay when
# `seam` is TRUE.
smooth_face_grob_3d <- function(painted, styles, opacity, viewport, seam, caps) {
  segment <- cumsum(c(TRUE, diff(painted$gradient) != 0L))
  grobs <- lapply(split(seq_along(painted$face), segment), function(k) {
    keep <- painted$id %in% k
    x <- painted$x_mm[keep] / viewport$width_mm; y <- painted$y_mm[keep] / viewport$height_mm
    id <- match(painted$id[keep], k)
    face <- painted$face[k]
    gp <- material_gp_3d(styles[face, , drop = FALSE], opacity[face], 1L)
    gp$col <- NA
    gp$fill <- if (painted$gradient[k[1L]]) painted$paint[k] else painted$flat[k]
    if (seam) seam_group_grob_3d(x, y, id, painted$flat[k], gp, opacity[face[1L]], caps) else
      grid::polygonGrob(x, y, id = id, gp = gp)
  })
  if (length(grobs) == 1L) grobs[[1L]] else grid::gTree(children = do.call(grid::gList, unname(grobs)))
}

# One linear gradient for a triangle whose linear colour is L1 + t U with t
# affine in screen space, in npc of the triangle's bounding box (grid fills
# each shape relative to its own box with group = FALSE).
triangle_gradient_3d <- function(P, L, tolerance) {
  d <- list(L[[2L]] - L[[1L]], L[[3L]] - L[[1L]])
  U <- d[[which.max(c(sum(d[[1L]]^2), sum(d[[2L]]^2)))]]
  t <- c(0, sum(d[[1L]] * U), sum(d[[2L]] * U)) / sum(U^2)
  edges <- rbind(P[[2L]] - P[[1L]], P[[3L]] - P[[1L]])
  g <- solve(edges, t[2:3])
  points <- rbind(P[[1L]], P[[2L]], P[[3L]])
  lo <- apply(points, 2L, min); hi <- apply(points, 2L, max)
  span <- hi - lo
  ends <- rbind(P[[1L]] + g * min(t) / sum(g^2), P[[1L]] + g * max(t) / sum(g^2))
  npc <- sweep(sweep(ends, 2L, lo), 2L, ifelse(span > 0, span, 1), "/")
  stops <- gradient_stops_3d(function(s) sweep(matrix(s, ncol = 1L) %*% U, 2L, L[[1L]] + min(t) * U, "+"),
    max(t) - min(t), tolerance)
  list(paint = grid::linearGradient(encode_rgb_3d(stops$colour), stops$at,
    x1 = npc[1L, 1L], y1 = npc[1L, 2L], x2 = npc[2L, 1L], y2 = npc[2L, 2L], group = FALSE),
    flat = encode_rgb_3d(matrix((L[[1L]] + L[[2L]] + L[[3L]]) / 3, 1L)))
}

# Uniform stops on [0, length] of the linear colour path colour(s), doubled
# until the device's linear interpolation of the encoded stops lies within the
# tolerance of the encoded path at every midpoint.
gradient_stops_3d <- function(colour, length, tolerance) {
  encoded <- function(s) srgb_encode_3d(pmin(pmax(colour(s), 0), 1))
  k <- 2L
  repeat {
    at <- seq(0, length, length.out = k)
    mid <- (head(at, -1L) + tail(at, -1L)) / 2
    stops <- encoded(at)
    error <- max(abs(encoded(mid) - (head(stops, -1L) + tail(stops, -1L)) / 2))
    if (error <= tolerance || k >= 257L) break
    k <- 2L * k - 1L
  }
  list(at = at / length, colour = colour(at))
}

# Triangles P (three n x 2 corner matrices) with linear corner colours L,
# split into four at edge midpoints until every piece's encoded corner
# colours differ by at most the tolerance in each channel, or its longest
# edge is shorter than twice `pixel_mm`; at most `max_level` levels. With
# `pixel_mm` > 0 every piece grows by half of it across its edges, and
# corners that leave their triangle across an edge marked in `outer` (n x 3;
# edges P1P2, P2P3, P3P1 on the face outline) are moved back onto it, so the
# pieces overlap inside the face and keep its outline. Returns the pieces and
# the triangle each comes from.
subdivide_colour_triangles_3d <- function(P, L, tolerance, outer = matrix(TRUE, nrow(P[[1L]]), 3L), pixel_mm = 0,
                                          max_level = 8L) {
  owner <- seq_len(nrow(P[[1L]]))
  original <- P
  done <- list(P = list(list(), list(), list()), L = list(list(), list(), list()), owner = list())
  keep <- function(rows) {
    for (k in 1:3) {
      done$P[[k]][[length(done$P[[k]]) + 1L]] <<- P[[k]][rows, , drop = FALSE]
      done$L[[k]][[length(done$L[[k]]) + 1L]] <<- L[[k]][rows, , drop = FALSE]
    }
    done$owner[[length(done$owner) + 1L]] <<- owner[rows]
  }
  for (level in 0:max_level) {
    edge <- sqrt(pmax(rowSums((P[[1L]] - P[[2L]])^2), rowSums((P[[2L]] - P[[3L]])^2), rowSums((P[[3L]] - P[[1L]])^2)))
    fine <- colour_range_3d(L) <= tolerance | edge < 2 * pixel_mm | level == max_level
    keep(which(fine))
    rows <- which(!fine)
    if (!length(rows)) break
    p <- lapply(P, function(x) x[rows, , drop = FALSE]); l <- lapply(L, function(x) x[rows, , drop = FALSE])
    mp <- list((p[[1L]] + p[[2L]]) / 2, (p[[2L]] + p[[3L]]) / 2, (p[[3L]] + p[[1L]]) / 2)
    ml <- list((l[[1L]] + l[[2L]]) / 2, (l[[2L]] + l[[3L]]) / 2, (l[[3L]] + l[[1L]]) / 2)
    P <- list(rbind(p[[1L]], mp[[1L]], mp[[3L]], mp[[1L]]), rbind(mp[[1L]], p[[2L]], mp[[2L]], mp[[2L]]),
              rbind(mp[[3L]], mp[[2L]], p[[3L]], mp[[3L]]))
    L <- list(rbind(l[[1L]], ml[[1L]], ml[[3L]], ml[[1L]]), rbind(ml[[1L]], l[[2L]], ml[[2L]], ml[[2L]]),
              rbind(ml[[3L]], ml[[2L]], l[[3L]], ml[[3L]]))
    owner <- rep(owner[rows], 4L)
  }
  P <- lapply(done$P, function(x) do.call(rbind, x))
  owner <- unlist(done$owner, use.names = FALSE)
  if (pixel_mm > 0) {
    corners <- lapply(original, function(x) x[owner, , drop = FALSE])
    P <- lapply(offset_triangles_3d(P, pixel_mm / 2), clamp_to_outline_3d, corners, outer[owner, , drop = FALSE])
  }
  list(P = P, L = lapply(done$L, function(x) do.call(rbind, x)), owner = owner)
}

# Triangles P grown by `distance` across each edge: each corner moves to the
# intersection of the moved lines of its two edges.
offset_triangles_3d <- function(P, distance) {
  edges <- list(c(1L, 2L), c(2L, 3L), c(3L, 1L))
  area <- (P[[2L]][, 1L] - P[[1L]][, 1L]) * (P[[3L]][, 2L] - P[[1L]][, 2L]) -
    (P[[3L]][, 1L] - P[[1L]][, 1L]) * (P[[2L]][, 2L] - P[[1L]][, 2L])
  lines <- lapply(edges, function(e) {
    a <- P[[e[1L]]]; d <- P[[e[2L]]] - a; len <- sqrt(rowSums(d^2))
    normal <- cbind(d[, 2L], -d[, 1L]) * sign(area) / ifelse(len > 0, len, 1)
    list(normal = normal, offset = rowSums(normal * a) + distance)
  })
  corner <- function(e1, e2) {
    n1 <- lines[[e1]]$normal; n2 <- lines[[e2]]$normal
    det <- n1[, 1L] * n2[, 2L] - n1[, 2L] * n2[, 1L]
    cbind(lines[[e1]]$offset * n2[, 2L] - lines[[e2]]$offset * n1[, 2L],
          n1[, 1L] * lines[[e2]]$offset - n2[, 1L] * lines[[e1]]$offset) / det
  }
  moved <- list(corner(3L, 1L), corner(1L, 2L), corner(2L, 3L))
  ok <- abs(area) > 0 & is.finite(rowSums(do.call(cbind, moved)))
  for (k in 1:3) P[[k]][ok, ] <- moved[[k]][ok, , drop = FALSE]
  P
}

# Points X (n x 2) moved back into triangles `corners` across the edges
# marked in `outer` (P1P2, P2P3, P3P1): the barycentric weight opposite such
# an edge is raised to zero and the weights renormalised.
clamp_to_outline_3d <- function(X, corners, outer) {
  A <- corners[[1L]]; B <- corners[[2L]]; C <- corners[[3L]]
  area <- (B[, 1L] - A[, 1L]) * (C[, 2L] - A[, 2L]) - (C[, 1L] - A[, 1L]) * (B[, 2L] - A[, 2L])
  weight <- function(U, V) ((U[, 1L] - X[, 1L]) * (V[, 2L] - X[, 2L]) - (V[, 1L] - X[, 1L]) * (U[, 2L] - X[, 2L])) / area
  w <- cbind(weight(B, C), weight(C, A), weight(A, B))
  bounded <- outer[, c(2L, 3L, 1L), drop = FALSE]
  w[bounded & w < 0] <- 0
  w <- w / rowSums(w)
  ok <- abs(area) > 0 & is.finite(rowSums(w))
  X[ok, ] <- (w[, 1L] * A + w[, 2L] * B + w[, 3L] * C)[ok, , drop = FALSE]
  X
}
