utils::globalVariables("grid_union_paths_cpp")

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
  if (is.null(attributes) && is.null(light$occlusion)) return(smooth)
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
  smooth[faces] <- has(c("red", "green", "blue")) |
    (lit & (has(c("normal_x", "normal_y", "normal_z")) | !is.null(light$occlusion)))
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
  base <- linear
  normal <- position <- matrix(0, length(index), 3L)
  lit_face <- !is.null(light) & style_value_3d(styles, "shade", FALSE) %in% TRUE
  lit <- lit_face[owner]
  accessibility <- NULL
  if (any(lit)) {
    normal <- face_shading_normals_3d(f, rows, scene$camera, styles)[owner, , drop = FALSE]
    if (all(c("normal_x", "normal_y", "normal_z") %in% columns)) {
      corner <- attributes[index, c("normal_x", "normal_y", "normal_z"), drop = FALSE]
      ok <- rowSums(is.finite(corner)) == 3L & rowSums(corner^2) > 0
      normal[ok, ] <- model_normals_3d(corner[ok, , drop = FALSE], scene$camera)
    }
    position <- sweep(cbind(scene$vertices$x[index], scene$vertices$y[index], scene$vertices$z[index]), 2L,
      scene$camera$model_scale, "*")
    if (!is.null(light$occlusion)) {
      accessibility <- matrix(1, length(index), 2L)
      accessibility[lit, 1L] <- ambient_occlusion_cpp(light$occlusion, position[lit, , drop = FALSE], normal[lit, , drop = FALSE])
      accessibility[lit, 2L] <- ambient_occlusion_cpp(light$occlusion, position[lit, , drop = FALSE], -normal[lit, , drop = FALSE])
    }
    linear[lit, ] <- shade_linear_3d(linear[lit, , drop = FALSE], normal[lit, , drop = FALSE], scene$camera, light,
      position[lit, , drop = FALSE], material_index_3d(styles)[owner][lit], scene$materials)
  }
  list(linear = pmin(pmax(linear, 0), 1), owner = owner,
    shading = list(base = base, normal = normal, position = position, accessibility = accessibility,
      depth = if (camera_perspective_3d(scene$camera)) scene$vertices$depth[index] else rep(1, length(index)),
      lit = lit_face, material = material_index_3d(styles), materials = kernel_materials_3d(scene$materials),
      lights = if (any(lit)) resolved_light_3d(light, scene$camera), view = kernel_view_3d(scene$camera, light)))
}

# How the vector backend paints smooth faces: "auto" draws adjacent varied
# triangles as one image clipped to their union, with per-sample lighting.
# A single triangle whose colour varies along one direction uses a gradient.
# "subdivide" splits triangles into flat pieces.
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
  (max(id) > 1L && any(abs(sweep(linear, 2L, linear[1L, ])) > 1e-12)) ||
    length(fan$face) > 1L || any(!fan$gradient & fan$range > tolerance)
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

# Image of a run of smooth faces, clipped to their union. Corner colours,
# normals and positions interpolate perspective-correctly at each sample,
# which is lit by the shared shading kernel. Rasterising smooth shading follows
# matplotlib's Agg path and stands in for the mesh shadings (PDF shading
# types 4 to 7, used by Asymptote, pgfplots and matplotlib's PDF backend)
# that R graphics cannot emit. x, y are vertex positions in npc of the
# viewport, id numbers faces 1..n with consecutive vertices, opacity is per
# face. `outer` is a clip region that the image's own clipping path would
# replace; it is applied as an alpha mask instead, or, where masks cannot hold
# scene-sized content, intersected with the image outline into one clipping
# path.
smooth_image_grob_3d <- function(x, y, id, linear, opacity, viewport, outer = NULL, caps = grid_capabilities_3d(), shading = NULL) {
  if (!caps$clipping_paths) stop("The vector device does not support clipping paths required by smooth shading.", call. = FALSE)
  pixel <- 25.4 / smooth_image_dpi_3d(viewport)
  x_mm <- x * viewport$width_mm; y_mm <- y * viewport$height_mm
  reach <- 2
  lo <- c(floor(min(x_mm) / pixel) - reach, floor(min(y_mm) / pixel) - reach) * pixel
  size <- pmax(1L, as.integer(ceiling((c(max(x_mm), max(y_mm)) - lo) / pixel) + reach))
  image <- smooth_faces_raster_cpp((x_mm - lo[1L]) / pixel, (y_mm - lo[2L]) / pixel, as.integer(id), linear,
    as.numeric(opacity), size[1L], size[2L], reach, shading)
  ring <- grid_union_paths_cpp(x, y, as.integer(id))
  outline <- grid::pathGrob(ring$x, ring$y, id = ring$id, rule = "winding")
  vp <- if (is.null(outer)) grid::viewport(clip = outline) else if (isFALSE(caps$large_definitions)) {
    outline <- intersect_clips_3d(outline, outer)
    if (is.null(outline)) return(grid::nullGrob())
    grid::viewport(clip = outline)
  } else {
    if (is.null(caps$mask_type)) stop("The vector device does not support the masks required by clipped smooth shading.", call. = FALSE)
    outer$gp <- grid::gpar(fill = "white", col = NA, alpha = 1)
    grid::viewport(clip = outline, mask = grid::as.mask(outer, type = caps$mask_type))
  }
  grid::rasterGrob(image, x = lo[1L] / viewport$width_mm, y = lo[2L] / viewport$height_mm,
    width = size[1L] * pixel / viewport$width_mm, height = size[2L] * pixel / viewport$height_mm,
    just = c("left", "bottom"), interpolate = TRUE, vp = vp)
}

# Selected faces of a smooth run, with the same per-sample paint data.
smooth_image_faces_3d <- function(x, y, id, vertex, opacity, viewport, outer, caps, k = seq_along(opacity)) {
  i <- id %in% k
  shading <- vertex$shading
  for (name in c("base", "normal", "position")) shading[[name]] <- shading[[name]][i, , drop = FALSE]
  shading$depth <- shading$depth[i]
  if (!is.null(shading$accessibility)) shading$accessibility <- shading$accessibility[i, , drop = FALSE]
  for (name in c("lit", "material")) shading[[name]] <- shading[[name]][k]
  smooth_image_grob_3d(x[i], y[i], match(id[i], k), vertex$linear[i, , drop = FALSE], opacity[k],
    viewport, outer, caps, shading)
}

# Grob of painted smooth shapes (smooth_face_paint_3d()) of faces with styles
# and opacity: consecutive gradient shapes and consecutive flat shapes are
# drawn as one polygon grob each, in face order, over a seam underlay when
# `seam` is TRUE.
smooth_face_grob_3d <- function(painted, styles, opacity, viewport, seam, caps) {
  underlay <- grid::gList()
  if (seam && length(painted$face) > 1L && all(opacity[painted$face] == 1)) {
    underlay <- seam_underlay_3d(painted$x_mm / viewport$width_mm, painted$y_mm / viewport$height_mm,
      painted$id, painted$flat)
    seam <- FALSE
  }
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
  grobs <- c(as.list(underlay), grobs)
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

# A packet whose faces with vertex-lit materials (material_3d(lighting =
# "vertex")) carry their lit colours as corner colours and are no longer
# shaded, so that every backend interpolates the lit corner colours in linear
# light: Gouraud (1971) shading. Each corner is lit by the shading kernel at
# its corner normal, or at the face normal where it has none, and at its
# model-space position. `camera` is resolved and `pre` is the camera's
# homogeneous pre-transform. Textured faces keep per-sample shading.
vertex_lit_packet_3d <- function(packet, camera, light, pre = diag(4)) {
  materials <- packet$materials
  if (is.null(light) || is.null(materials$lighting) || !any(materials$lighting %in% "vertex")) return(packet)
  styles <- packet$styles
  index <- material_index_3d(styles)
  vertex <- index > 0L & style_value_3d(styles, "shade", FALSE) %in% TRUE & panel_textures_3d(styles)$index == 0L
  vertex[vertex] <- materials$lighting[index[vertex]] %in% "vertex"
  faces <- packet$faces
  owner_style <- match(faces$style_id, styles$style_id)
  take <- which(vertex[owner_style] & faces$count >= 3L)
  if (!length(take)) return(packet)
  fill <- style_value_3d(styles, "fill", NA_character_)[owner_style[take]]
  take <- take[!is.na(fill)]
  fill <- fill[!is.na(fill)]
  if (!length(take)) return(packet)
  counts <- faces$count[take]
  corner <- rep(faces$offset[take], counts) + sequence(counts) - 1L
  owner <- rep(seq_along(take), counts)
  pre <- matrix(as.double(pre %or3d% diag(4)), 4L, 4L)
  xyzw <- scene_vertex_matrix_3d(packet$vertices)[packet$corners$vertex[corner], , drop = FALSE] %*% t(pre)
  position <- sweep(xyzw[, 1:3, drop = FALSE] / xyzw[, 4L], 2L, camera$model_scale, "*")
  edge <- function(k) position[corner_shift_3d(owner, k), , drop = FALSE]
  a <- position; b <- edge(1L)
  newell <- rowsum(cbind((a[, 2L] - b[, 2L]) * (a[, 3L] + b[, 3L]), (a[, 3L] - b[, 3L]) * (a[, 1L] + b[, 1L]),
    (a[, 1L] - b[, 1L]) * (a[, 2L] + b[, 2L])), owner, reorder = FALSE)
  newell <- unit_rows_3d(newell)
  face_normal <- face_shading_normals_3d(list(normal_x = newell[, 1L], normal_y = newell[, 2L], normal_z = newell[, 3L]),
    seq_along(take), camera, styles[owner_style[take], , drop = FALSE])
  normal <- face_normal[owner, , drop = FALSE]
  corners <- packet$corners
  if (all(c("normal_x", "normal_y", "normal_z") %in% names(corners))) {
    supplied <- cbind(corners$normal_x[corner], corners$normal_y[corner], corners$normal_z[corner])
    ok <- rowSums(is.finite(supplied)) == 3L & rowSums(supplied^2) > 0
    if (any(ok)) {
      supplied <- supplied[ok, , drop = FALSE] %*% solve(pre[1:3, 1:3])
      normal[ok, ] <- model_normals_3d(supplied, camera)
    }
  }
  base <- linear_rgb_3d(fill)[owner, , drop = FALSE]
  if (all(corner_colour_columns_3d %in% names(corners))) {
    rgb <- cbind(corners$red[corner], corners$green[corner], corners$blue[corner])
    ok <- rowSums(is.finite(rgb)) == 3L
    base[ok, ] <- srgb_decode_3d(rgb[ok, , drop = FALSE])
  }
  lit <- shade_linear_3d(base, normal, camera, light, position, index[owner_style[take]][owner], materials)
  encoded <- srgb_encode_3d(pmin(pmax(lit, 0), 1))
  n <- length(corners$vertex)
  for (k in seq_along(corner_colour_columns_3d)) {
    name <- corner_colour_columns_3d[k]
    if (is.null(corners[[name]])) corners[[name]] <- rep(NA_real_, n)
    corners[[name]][corner] <- encoded[, k]
  }
  columns <- scene_table_columns(corners, scene_schemas()$corners, scene_optional_schemas()$corners, "corners")
  packet$corners <- corners[names(columns)]
  packet$styles$shade[vertex] <- FALSE
  if (isFALSE(light$linear)) {
    border <- style_value_3d(styles, "colour", NA_character_)[owner_style[take]]
    packet <- encoded_corner_faces_3d(packet, take[is.na(border)], camera$model_scale)
  }
  packet
}

# Faces `rows` of a packet with corner colours, split on a barycentric grid
# so that the backends' linear-light interpolation of corner colours follows
# the interpolation of the encoded colours, as the fixed-function OpenGL
# pipeline interpolates the colours it lights on encoded values. Each face is
# fanned into triangles, the shared edges are flipped toward the diagonal
# whose corner colours differ least (flip_colour_edges_3d()), and every
# triangle is cut into level^2 triangles,
# with one level for all faces (so that shared edges split alike) set by the
# largest encoded colour step along a triangle (one level per 0.1) and kept
# within `budget` triangles.
encoded_corner_faces_3d <- function(packet, rows, camera_scale = c(1, 1, 1), budget = 40000L) {
  faces <- packet$faces
  corners <- packet$corners
  if (!length(rows) || !all(corner_colour_columns_3d %in% names(corners))) return(packet)
  counts <- faces$count[rows]
  rgb <- cbind(corners$red, corners$green, corners$blue)
  fan <- counts - 2L
  face <- rep(rows, fan)
  step <- sequence(fan)
  tri <- cbind(faces$offset[face], faces$offset[face] + step, faces$offset[face] + step + 1L)
  at <- scene_vertex_matrix_3d(packet$vertices)[corners$vertex, , drop = FALSE]
  place <- paste(signif(at[, 1L] / at[, 4L], 12), signif(at[, 2L] / at[, 4L], 12), signif(at[, 3L] / at[, 4L], 12))
  tri <- flip_colour_edges_3d(tri, match(place, unique(place)), rgb, faces$layer_id[face],
    sweep(at[, 1:3, drop = FALSE] / at[, 4L], 2L, camera_scale, "*"))
  jump <- function(a, b) apply(abs(rgb[tri[, a], , drop = FALSE] - rgb[tri[, b], , drop = FALSE]), 1L, max)
  steepest <- max(jump(1L, 2L), jump(2L, 3L), jump(1L, 3L), 0, na.rm = TRUE)
  level <- min(ceiling(steepest / 0.1), floor(sqrt(budget / length(face))), 6L)
  if (level < 2L) return(packet)
  grid <- expand.grid(i = 0:level, j = 0:level)
  grid <- grid[grid$i + grid$j <= level, ]
  weight <- cbind(level - grid$i - grid$j, grid$i, grid$j) / level
  at <- function(i, j) match(paste(i, j), paste(grid$i, grid$j))
  small <- list()
  for (i in 0:(level - 1L)) for (j in 0:(level - 1L - i)) {
    small[[length(small) + 1L]] <- c(at(i, j), at(i + 1L, j), at(i, j + 1L))
    if (i + j < level - 1L) small[[length(small) + 1L]] <- c(at(i + 1L, j), at(i + 1L, j + 1L), at(i, j + 1L))
  }
  small <- do.call(rbind, small)
  m <- nrow(weight); k <- nrow(small); n <- nrow(tri)
  xyzw <- scene_vertex_matrix_3d(packet$vertices)
  mix <- function(values) {
    out <- matrix(0, n * m, ncol(values))
    for (c in 1:3) out <- out + weight[rep(seq_len(m), n), c] * values[tri[rep(seq_len(n), each = m), c], , drop = FALSE]
    out
  }
  point <- mix(xyzw[corners$vertex, , drop = FALSE])
  colour <- mix(rgb)
  base <- length(packet$vertices$x)
  packet$vertices <- lapply(stats::setNames(seq_along(packet$vertices), names(packet$vertices)),
    function(c) c(packet$vertices[[c]], point[, c]))
  sub <- as.vector(t(small[rep(seq_len(k), n), , drop = FALSE] + rep((seq_len(n) - 1L) * m, each = k)))
  parent <- rep(face, each = k)
  keep <- setdiff(seq_along(faces$count), rows)
  kept_corners <- unlist(lapply(keep, function(f) faces$offset[f] + seq_len(faces$count[f]) - 1L))
  new_corners <- lapply(stats::setNames(names(corners), names(corners)), function(name) {
    value <- switch(name, vertex = as.integer(base + sub), source_row = rep(corners$source_row[faces$offset[parent]], each = 3L),
      red = colour[sub, 1L], green = colour[sub, 2L], blue = colour[sub, 3L], corners[[name]][rep(NA_integer_, length(sub))])
    c(corners[[name]][kept_corners], value)
  })
  new_faces <- slice_rows_3d(as.data.frame(faces, stringsAsFactors = FALSE), c(keep, parent))
  new_faces$count <- c(faces$count[keep], rep(3L, length(parent)))
  new_faces$offset <- as.integer(cumsum(c(1L, new_faces$count))[seq_along(new_faces$count)])
  packet$faces <- as.list(new_faces)
  packet$corners <- new_corners
  renumber_primitives_3d(packet)
}

# Index of the corner k places after each corner within its face, for corners
# grouped by consecutive owners.
corner_shift_3d <- function(owner, k) {
  start <- match(owner, owner)
  count <- tabulate(owner)[owner]
  start + (seq_along(owner) - start + k) %% count
}

# Triangles (rows of corner indices, counter-clockwise; `vertex` numbers the
# distinct corner positions) with each edge shared by two triangles of one
# group (layer) flipped to the other diagonal of their
# quadrilateral when the colours at its ends differ less than at the ends of
# the shared edge: one pass of the data-dependent triangulation of Dyn,
# Levin and Rippa (1990) with the largest channel difference as cost, so that
# an interpolated highlight runs along the diagonal it lies on instead of
# zigzagging across it. Each triangle takes part in at most one flip, and a
# flip is made only where the two triangles meet along the edge from
# opposite sides and their model-space planes (`position`, one row per
# corner) differ by less than `crease` degrees, so that the flip leaves the
# surface's shape nearly unchanged.
flip_colour_edges_3d <- function(tri, vertex, rgb, group, position, crease = 10) {
  n <- nrow(tri)
  if (n < 2L) return(tri)
  v <- matrix(vertex[tri], n)
  edge <- data.frame(t = rep(seq_len(n), 3L), e = rep(0:2, each = n))
  a <- v[cbind(edge$t, edge$e + 1L)]; b <- v[cbind(edge$t, (edge$e + 1L) %% 3L + 1L)]
  edge$key <- paste(group[edge$t], pmin(a, b), pmax(a, b))
  edge$forward <- a < b
  shared <- edge[edge$key %in% edge$key[duplicated(edge$key)], ]
  shared <- shared[order(shared$key, !shared$forward), ]
  if (!nrow(shared)) return(tri)
  first <- shared[c(TRUE, FALSE), ]; second <- shared[c(FALSE, TRUE), ]
  ok <- first$key == second$key & first$forward != second$forward & first$t != second$t
  first <- first[ok, ]; second <- second[ok, ]
  cost <- function(p, q) apply(abs(rgb[p, , drop = FALSE] - rgb[q, , drop = FALSE]), 1L, max)
  # Corners of each pair: the shared edge runs a -> b in the first triangle,
  # c and d are the corners opposite it in the first and second triangle.
  ca <- tri[cbind(first$t, first$e + 1L)]; cb <- tri[cbind(first$t, (first$e + 1L) %% 3L + 1L)]
  cc <- tri[cbind(first$t, (first$e + 2L) %% 3L + 1L)]; cd <- tri[cbind(second$t, (second$e + 2L) %% 3L + 1L)]
  gain <- cost(ca, cb) - cost(cc, cd)
  plane <- function(p, q, r) {
    u <- position[q, , drop = FALSE] - position[p, , drop = FALSE]; w <- position[r, , drop = FALSE] - position[p, , drop = FALSE]
    unit_rows_3d(cbind(u[, 2L] * w[, 3L] - u[, 3L] * w[, 2L], u[, 3L] * w[, 1L] - u[, 1L] * w[, 3L], u[, 1L] * w[, 2L] - u[, 2L] * w[, 1L]))
  }
  flat <- rowSums(plane(ca, cb, cc) * plane(cb, ca, cd)) >= cospi(crease / 180)
  gain[!flat] <- 0
  used <- logical(n)
  for (i in order(-gain)) {
    if (gain[i] <= 1e-9) break
    s <- first$t[i]; u <- second$t[i]
    if (used[s] || used[u]) next
    used[s] <- used[u] <- TRUE
    tri[s, ] <- c(cc[i], ca[i], cd[i])
    tri[u, ] <- c(cd[i], cb[i], cc[i])
  }
  tri
}
