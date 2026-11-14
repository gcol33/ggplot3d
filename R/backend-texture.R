# Vector paint of textured faces. The image is defined once as a group (a
# raster square at the viewport origin, as large as the viewport's shorter
# side, since raster devices hold a group as an image at device resolution)
# and used once per face
# triangle under an affine transformation that takes its texture coordinates
# to the triangle's screen corners, clipped to the triangle (Murrell 2021,
# groups and affine transformations in R graphics; R >= 4.2). Faces are
# fan-triangulated. A texture coordinate is affine in screen space over a
# triangle under orthographic projection, so one transformation per triangle
# is exact; under perspective it is a ratio of affine functions, and each
# triangle is split at its edge midpoints (true texture coordinates there,
# from the corners' inverse depths) until the affine map moves the texture
# at every edge midpoint by at most half a device pixel, at most eight
# levels. The clip triangles of an opaque run are pushed out by half a
# pixel so that antialiasing leaves no seam between them.

texture_device_ok_3d <- function() {
  getRversion() >= "4.2.0" && isTRUE(grDevices::dev.capabilities()$transformations) &&
    isTRUE(grDevices::dev.capabilities()$clippingPaths)
}

# Triangles P (three n x 2 corner matrices in mm) with texture coordinates
# UV (three n x 2) and inverse depths Q (three n-vectors), split until the
# affine texture map of every piece is within `tolerance` mm of the
# projective one at its edge midpoints. `owner` labels each triangle and
# follows its pieces.
subdivide_texture_triangles_3d <- function(P, UV, Q, tolerance, owner = seq_len(nrow(P[[1L]]))) {
  done <- list(P = list(list(), list(), list()), UV = list(list(), list(), list()), owner = list())
  keep <- function(rows) {
    for (k in 1:3) {
      done$P[[k]][[length(done$P[[k]]) + 1L]] <<- P[[k]][rows, , drop = FALSE]
      done$UV[[k]][[length(done$UV[[k]]) + 1L]] <<- UV[[k]][rows, , drop = FALSE]
    }
    done$owner[[length(done$owner) + 1L]] <<- owner[rows]
  }
  edges <- list(c(1L, 2L), c(2L, 3L), c(3L, 1L))
  for (level in 0:8) {
    A <- texture_affine_3d(P, UV)
    error <- matrix(0, nrow(P[[1L]]), 3L)
    middle <- vector("list", 3L)
    for (e in 1:3) {
      i <- edges[[e]][1L]; j <- edges[[e]][2L]
      q <- Q[[i]] + Q[[j]]
      true <- (UV[[i]] * Q[[i]] + UV[[j]] * Q[[j]]) / q
      d <- true - (UV[[i]] + UV[[j]]) / 2
      error[, e] <- sqrt((d[, 1L] * A[, 1L] + d[, 2L] * A[, 3L])^2 + (d[, 1L] * A[, 2L] + d[, 2L] * A[, 4L])^2)
      middle[[e]] <- list(P = (P[[i]] + P[[j]]) / 2, UV = true, Q = q / 2)
    }
    fine <- apply(error, 1L, max) <= tolerance | !is.finite(rowSums(error)) | level == 8L
    keep(which(fine))
    rows <- which(!fine)
    if (!length(rows)) break
    pick <- function(x, field) if (is.null(field)) lapply(x, function(m) if (is.matrix(m)) m[rows, , drop = FALSE] else m[rows]) else
      lapply(x, function(m) if (is.matrix(m[[field]])) m[[field]][rows, , drop = FALSE] else m[[field]][rows])
    split <- function(corner, mid) {
      list(rbind(corner[[1L]], mid[[1L]], mid[[3L]], mid[[1L]]), rbind(mid[[1L]], corner[[2L]], mid[[2L]], mid[[2L]]),
           rbind(mid[[3L]], mid[[2L]], corner[[3L]], mid[[3L]]))
    }
    splitq <- function(corner, mid) {
      list(c(corner[[1L]], mid[[1L]], mid[[3L]], mid[[1L]]), c(mid[[1L]], corner[[2L]], mid[[2L]], mid[[2L]]),
           c(mid[[3L]], mid[[2L]], corner[[3L]], mid[[3L]]))
    }
    P <- split(pick(P, NULL), pick(middle, "P"))
    UV <- split(pick(UV, NULL), pick(middle, "UV"))
    Q <- splitq(pick(Q, NULL), pick(middle, "Q"))
    owner <- rep(owner[rows], 4L)
  }
  list(P = lapply(done$P, function(x) do.call(rbind, x)), UV = lapply(done$UV, function(x) do.call(rbind, x)),
       owner = unlist(done$owner, use.names = FALSE))
}

# Row-vector linear part (a11, a12, a21, a22) of the affine map taking
# texture coordinate differences to screen differences on each triangle:
# (du, dv) %*% matrix(c(a11, a21, a12, a22), 2) = (dx, dy).
texture_affine_3d <- function(P, UV) {
  e1 <- UV[[2L]] - UV[[1L]]; e2 <- UV[[3L]] - UV[[1L]]
  f1 <- P[[2L]] - P[[1L]]; f2 <- P[[3L]] - P[[1L]]
  det <- e1[, 1L] * e2[, 2L] - e1[, 2L] * e2[, 1L]
  det[abs(det) <= 1e-14] <- NA_real_
  cbind((e2[, 2L] * f1[, 1L] - e1[, 2L] * f2[, 1L]) / det, (e2[, 2L] * f1[, 2L] - e1[, 2L] * f2[, 2L]) / det,
        (e1[, 1L] * f2[, 1L] - e2[, 1L] * f1[, 1L]) / det, (e1[, 1L] * f2[, 2L] - e2[, 1L] * f1[, 2L]) / det)
}

# Triangles P with every edge pushed outward by `distance` mm: each corner
# moves to the meet of its two offset edge lines, or stays where they meet
# farther than four times the distance away (a sliver).
dilate_triangles_3d <- function(P, distance) {
  if (distance <= 0) return(P)
  edges <- list(c(1L, 2L), c(2L, 3L), c(3L, 1L))
  area <- (P[[2L]][, 1L] - P[[1L]][, 1L]) * (P[[3L]][, 2L] - P[[1L]][, 2L]) -
    (P[[3L]][, 1L] - P[[1L]][, 1L]) * (P[[2L]][, 2L] - P[[1L]][, 2L])
  orientation <- ifelse(area >= 0, 1, -1)
  # Outward unit normal and offset of each edge line n . p = c.
  lines <- lapply(edges, function(e) {
    a <- P[[e[1L]]]; b <- P[[e[2L]]]
    d <- b - a; len <- sqrt(rowSums(d^2)); len[len == 0] <- 1
    normal <- cbind(d[, 2L], -d[, 1L]) / len * orientation
    list(normal = normal, c = rowSums(normal * a) + distance)
  })
  meet <- function(l1, l2, fallback) {
    det <- l1$normal[, 1L] * l2$normal[, 2L] - l1$normal[, 2L] * l2$normal[, 1L]
    x <- (l1$c * l2$normal[, 2L] - l2$c * l1$normal[, 2L]) / det
    y <- (l1$normal[, 1L] * l2$c - l2$normal[, 1L] * l1$c) / det
    out <- cbind(x, y)
    bad <- !is.finite(det) | abs(det) < 1e-6 | !is.finite(rowSums(out)) | rowSums((out - fallback)^2) > (4 * distance)^2
    out[bad, ] <- fallback[bad, ]
    out
  }
  list(meet(lines[[3L]], lines[[1L]], P[[1L]]), meet(lines[[1L]], lines[[2L]], P[[2L]]), meet(lines[[2L]], lines[[3L]], P[[3L]]))
}

# Group transformation of one triangle: texture coordinates UV (3 x 2) of a
# group image `side` inches square to target corners T (3 x 2, inches from
# the viewport origin), as grid's row-vector 3 x 3 matrices in device or inch
# units.
texture_use_transform_3d <- function(UV, T, side) {
  force(UV); force(T); force(side)
  function(group, device) {
    scale <- grid::deviceDim(grid::unit(1, "in"), grid::unit(1, "in"), valueOnly = TRUE, device = device)
    s <- c(scale$w, scale$h)
    source <- cbind(sweep(UV * side, 2L, s, "*"), 1)
    target <- sweep(T, 2L, s, "*")
    M <- rbind(cbind(solve(source, target), c(0, 0, 1)))
    grid::defnTranslate(group, inverse = TRUE, device = device) %*% M %*% grid::useTranslate(device = device)
  }
}

# Grob of a run of textured face fragments `rows` sharing one texture, face
# by face in draw order: its fill (where set), the texture over each of its
# triangles, then its darkening where lit. The image group is defined before,
# outside any compositing group that uses it. `uv` holds
# the texture coordinates at the scene's output vertices. Lit faces are
# darkened by their shading of white (per vertex where corner normals exist,
# see fragment_vertex_linear_3d()): black at alpha 1 - g over a colour c
# leaves c g, the product the raster backend forms, taken on encoded values,
# which approximates it in linear light since the sRGB curve is close to a
# power law; g is the mean of the shading's channels, so coloured light
# tints nothing and highlights do not brighten. Where the image has
# transparent texels and no fill lies under them, the darkening is drawn
# atop the painted faces inside a group so that they stay transparent.
textured_faces_grob_3d <- function(scene, rows, styles, opacity, uv, texture, viewport, caps, name,
                                   attributes = NULL, light = NULL) {
  if (!texture_device_ok_3d()) stop("Textured faces in the vector backend need R >= 4.2 and a device with affine transformations and clipping paths (ragg, png(type = \"cairo\"), pdf(), cairo_pdf(), svg()); use coord_3d(backend = backend_3d(\"raster\")) otherwise.", call. = FALSE)
  f <- scene$fragments
  counts <- f$count[rows]
  index <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  id <- rep(seq_along(rows), counts)
  x <- scene$vertices$x_npc[index] * viewport$width_mm; y <- scene$vertices$y_npc[index] * viewport$height_mm
  fill <- styles$fill
  filled <- !is.na(fill)
  n <- counts - 2L
  start <- cumsum(c(1L, head(counts, -1L)))
  a <- rep(start, n); b <- a + sequence(n); c <- b + 1L
  corner <- list(a, b, c)
  P <- lapply(corner, function(k) cbind(x[k], y[k]))
  UV <- lapply(corner, function(k) uv[index[k], , drop = FALSE])
  perspective <- camera_perspective_3d(scene$camera)
  Q <- lapply(corner, function(k) if (perspective) 1 / scene$vertices$depth[index[k]] else rep(1, length(k)))
  owner <- rep(seq_along(rows), n)
  ok <- Reduce(`&`, lapply(c(P, UV, Q), function(m) if (is.matrix(m)) is.finite(rowSums(m)) else is.finite(m)))
  pixel <- 25.4 / viewport$dpi
  pieces <- subdivide_texture_triangles_3d(lapply(P, function(m) m[ok, , drop = FALSE]),
    lapply(UV, function(m) m[ok, , drop = FALSE]), lapply(Q, function(m) m[ok]), pixel / 2, owner[ok])
  usable <- if (length(pieces$owner)) is.finite(rowSums(texture_affine_3d(pieces$P, pieces$UV))) else logical()
  P <- lapply(pieces$P, function(m) m[usable, , drop = FALSE]); UV <- lapply(pieces$UV, function(m) m[usable, , drop = FALSE])
  owner <- pieces$owner[usable]
  opaque <- texture$opaque && all(opacity >= translucent_opacity_3d)
  clip <- if (length(owner)) dilate_triangles_3d(P, if (opaque) pixel / 2 else 0) else P
  group <- paste0(name, "-texture")
  side <- min(viewport$width_mm, viewport$height_mm) / 25.4
  definition <- getExportedValue("grid", "defineGrob")(
    grid::rasterGrob(texture$image, x = 0, y = 0, width = grid::unit(side, "in"), height = grid::unit(side, "in"),
      just = c(0, 0), interpolate = texture$interpolate), name = group,
    vp = grid::viewport(x = 0, y = 0, width = 1, height = 1, just = c(0, 0)))
  use <- function(k) {
    region <- grid::polygonGrob(c(clip[[1L]][k, 1L], clip[[2L]][k, 1L], clip[[3L]][k, 1L]) / viewport$width_mm,
      c(clip[[1L]][k, 2L], clip[[2L]][k, 2L], clip[[3L]][k, 2L]) / viewport$height_mm)
    corners <- rbind(UV[[1L]][k, ], UV[[2L]][k, ], UV[[3L]][k, ])
    target <- rbind(P[[1L]][k, ], P[[2L]][k, ], P[[3L]][k, ]) / 25.4
    getExportedValue("grid", "useGrob")(group, transform = texture_use_transform_3d(corners, target, side),
      vp = grid::viewport(x = 0, y = 0, width = 1, height = 1, just = c(0, 0), clip = region))
  }
  lit <- !is.null(light) & style_value_3d(styles, "shade", FALSE) %in% TRUE
  shade <- NULL
  if (any(lit)) {
    white <- styles
    white$fill <- "#FFFFFF"
    vertex <- fragment_vertex_linear_3d(scene, rows, white, attributes, light)
    shade <- smooth_face_paint_3d(x, y, id, vertex$linear, caps$gradients)
    shade$flat <- darkening_3d(shade$flat)
    shade$paint <- lapply(shade$paint, function(p) if (is.null(p)) p else { p$colours <- darkening_3d(p$colours); p })
  }
  atop <- !(texture$opaque || all(filled)) && "atop" %in% grDevices::dev.capabilities()$compositing
  triangles <- split(seq_along(owner), factor(owner, levels = seq_along(rows)))
  faces <- lapply(seq_along(rows), function(j) {
    parts <- list()
    if (filled[j]) parts[[1L]] <- grid::polygonGrob(x[id == j] / viewport$width_mm, y[id == j] / viewport$height_mm,
      gp = grid::gpar(fill = fill[j], col = NA))
    parts <- c(parts, lapply(triangles[[j]], use))
    painted <- grid::gTree(children = do.call(grid::gList, parts))
    if (is.null(shade) || !lit[j]) return(painted)
    k <- which(shade$face == j)
    keep <- shade$id %in% k
    darkening <- smooth_face_grob_3d(list(x_mm = shade$x_mm[keep], y_mm = shade$y_mm[keep], id = match(shade$id[keep], k),
      face = rep(1L, length(k)), flat = shade$flat[k], gradient = shade$gradient[k], paint = shade$paint[k]),
      white[j, , drop = FALSE], 1, viewport, FALSE, caps)
    if (atop) getExportedValue("grid", "groupGrob")(darkening, op = "atop", dst = painted) else
      if (texture$opaque || filled[j]) grid::gTree(children = grid::gList(painted, darkening)) else painted
  })
  painted <- grid::gTree(children = do.call(grid::gList, faces))
  alpha <- opacity[1L]
  if (alpha < translucent_opacity_3d && isTRUE(caps$group_over)) {
    mask <- grid::rectGrob(gp = grid::gpar(fill = grDevices::rgb(0, 0, 0, alpha), col = NA))
    painted <- grid::grobTree(getExportedValue("grid", "groupGrob")(painted), vp = grid::viewport(mask = mask))
  }
  grid::gTree(children = grid::gList(definition, painted))
}

# Black with the alpha that darkens a colour as multiplying by `colour`'s
# mean encoded channel would.
darkening_3d <- function(colour) {
  grey <- colMeans(grDevices::col2rgb(colour)) / 255
  grDevices::rgb(0, 0, 0, 1 - grey)
}
