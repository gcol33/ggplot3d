#' Export a panel as a 3D model file
#'
#' Write the scene of one panel to a file that 3D software opens: glTF binary
#' (`.glb`) or glTF with a separate buffer (`.gltf` and `.bin`) for Blender
#' and the web, PLY, OBJ with an MTL material file, or binary STL. Faces are
#' triangulated, paths become tubes or line primitives, and points become
#' small solids or point primitives, all in model coordinates: the panel box
#' scaled by the aspect of [coord_3d()], with its longest side `size` long.
#' @param plot A ggplot with [coord_3d()].
#' @param file Output path. Its extension selects the format: `.glb`,
#'   `.gltf`, `.ply`, `.obj`, `.stl` or `.3mf`.
#' @param panel Panel index in layout order, default 1.
#' @param furniture Whether to include the panes, grid lines and box edges,
#'   default FALSE.
#' @param lines `"tube"` (default) writes each path as a closed tube with the
#'   drawn line width as diameter; `"line"` writes line primitives.
#' @param points `"mesh"` (default) writes each point as a solid of its drawn
#'   size: a cube for square shapes, a tetrahedron for triangles, an
#'   octahedron for diamonds and a sphere for the other shapes; `"point"`
#'   writes point primitives.
#' @param size Length of the longest side of the box in file units, default 1.
#' @param width,height Device size in pixels used to lay out the plot,
#'   default 640 each. Tubes and solids keep the size relative to the box that
#'   lines and points have in a plot of this size.
#' @param dpi Device resolution, default 96 dots per inch.
#' @param print Optional [print_3d()] specification for closed millimetre solids
#'   in STL or 3MF output.
#' @return `file`, invisibly.
#' @details
#' glTF output follows the glTF 2.0 specification and is the route into
#' Blender, whose importer reads its meshes, materials, cameras and
#' animation. The vertical axis of the plot becomes +y. Each layer is one
#' named node (`layer1_point`, `layer2_surface`, ...), and the furniture is a
#' node `furniture` with the children `panes`, `grid` and `box`, so layers can
#' be selected, hidden and restyled after import. Mapped colours are vertex
#' colours (`COLOR_0`, linear light), so a colour scale needs no material per
#' colour. Faces drawn with lighting ([light_3d()] and `shade`) use one lit
#' material whose base colour multiplies the vertex colours; all other
#' geometry uses the `KHR_materials_unlit` extension. Translucent geometry
#' uses alpha blending and every material is double sided. A node `camera`
#' holds the view of the panel, so the file opens on the plot's view; an
#' oblique projection is written as an orthographic camera along the sight
#' direction. [animate_3d()] writes animated glTF files.
#'
#' PLY output is binary with vertex normals and 8-bit sRGB vertex colours;
#' line primitives are `edge` elements. OBJ output writes one object per node,
#' sRGB vertex colours after each vertex position, and one MTL material per
#' distinct colour; its vertical axis is +y, as OBJ importers expect. PLY and
#' STL keep the vertical axis of the plot as +z. STL holds triangles only, so
#' it requires `lines = "tube"` and `points = "mesh"`.
#'
#' Ordinary model exports omit text and write dashed lines solid. Printing
#' exports include extruded text according to [print_3d()].
#' @seealso [animate_3d()] for animated glTF files.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 15), y = seq(-1, 1, length.out = 15))
#' d$z <- d$x * exp(-d$x^2 - d$y^2)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(ggplot2::aes(fill = z)) + coord_3d()
#' glb <- export_3d(p, tempfile(fileext = ".glb"))
#' file.size(glb)
#' export_3d(p, tempfile(fileext = ".stl"), size = 50)
#' @export
export_3d <- function(plot, file, panel = 1L, furniture = FALSE, lines = c("tube", "line"),
                      points = c("mesh", "point"), size = 1, width = 640L, height = 640L, dpi = 96, print = NULL) {
  writer <- export_writer_3d(file)
  if (!is.null(print)) {
    scene_assert(inherits(print, "ggplot3d_print"), "print must be a print_3d() specification")
    scene_assert(writer$printing, "This file format does not support printing solids")
    options <- export_options_3d(FALSE, "tube", "mesh", 1)
    scene <- print_scene_3d(export_scene_3d(plot, panel, options, width, height, dpi), print)
    writer$write(list(scene), file, options, fps = NULL)
    return(invisible(file))
  }
  scene_assert(!writer$print_only, "This file format requires print = print_3d()")
  options <- export_options_3d(furniture, match.arg(lines), match.arg(points), size)
  scene_assert(!writer$triangles || (options$lines == "tube" && options$points == "mesh"),
    sprintf("%s files hold triangles only; use lines = \"tube\" and points = \"mesh\"", toupper(writer$extension)))
  scene <- export_scene_3d(plot, panel, options, width, height, dpi)
  writer$write(list(scene), file, options, fps = NULL)
  invisible(file)
}

.exporters_3d <- new.env(parent = emptyenv())

# A format writer receives the captured scenes (one per animation frame), the
# path and the export options. `animation` marks writers that store more than
# one frame; `triangles` marks formats without line or point primitives.
register_exporter_3d <- function(extension, write, animation = FALSE, triangles = FALSE, printing = FALSE, print_only = FALSE) {
  .exporters_3d[[extension]] <- list(extension = extension, write = write,
    animation = animation, triangles = triangles, printing = printing, print_only = print_only)
  if (animation) motion_outputs_3d[[extension]] <- list(device = "pdf", kind = "scene",
    packages = character(), write = write)
  invisible(extension)
}

export_writer_3d <- function(file, animation = FALSE) {
  scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file),
    "file must be a single output path")
  extension <- tolower(tools::file_ext(file))
  known <- export_extensions_3d(animation)
  scene_assert(extension %in% known,
    paste0("file must end in ", paste0(".", known, collapse = ", ")))
  scene_assert(dir.exists(dirname(file)), "The output directory does not exist")
  .exporters_3d[[extension]]
}

export_extensions_3d <- function(animation = FALSE) {
  known <- sort(ls(.exporters_3d))
  if (animation) known <- known[vapply(known, function(e) .exporters_3d[[e]]$animation, logical(1))]
  known
}

export_options_3d <- function(furniture, lines, points, size) {
  scene_assert(is.logical(furniture) && length(furniture) == 1L && !is.na(furniture),
    "furniture must be TRUE or FALSE")
  scene_assert(is.numeric(size) && length(size) == 1L && is.finite(size) && size > 0,
    "size must be a positive finite number")
  list(furniture = furniture, lines = lines, points = points, size = as.double(size))
}

export_scene_3d <- function(plot, panel, options, width, height, dpi) {
  motion_plot_coord_3d(plot)
  motion_dimensions_3d(width, height, dpi)
  scene_assert(is.numeric(panel) && length(panel) == 1L && is.finite(panel) &&
    panel >= 1 && panel == trunc(panel), "panel must be a positive integer")
  device <- export_device_3d(width, height, dpi)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  scene <- export_capture_3d(ggplot2::ggplotGrob(plot), panel, options, export_layer_names_3d(plot))
  scene$layers <- data.frame(name = export_layer_names_3d(plot), text = vapply(plot$layers,
    function(layer) inherits(layer$geom, c("GeomText", "GeomLabel")), logical(1)))
  scene
}

# A device without output fixes the physical panel size and text metrics.
export_device_3d <- function(width, height, dpi) {
  grDevices::pdf(NULL, width = width / dpi, height = height / dpi)
  grDevices::dev.cur()
}

export_layer_names_3d <- function(plot) {
  vapply(seq_along(plot$layers), function(i) {
    geom <- tolower(sub("3D$", "", sub("^Geom", "", class(plot$layers[[i]]$geom)[1L])))
    sprintf("layer%d_%s", i, geom)
  }, character(1))
}

# Lays out a built plot without drawing it and returns the scene of one panel:
# its nodes in model coordinates, the camera and the vertical axis. `mm` fixes
# the millimetres per model unit for tube and solid sizes; NULL takes it from
# this layout.
export_capture_3d <- function(grob, panel, options, layer_names, mm = NULL) {
  panels <- layout_scene_panels_3d(grob)
  scene_assert(length(panels) > 0L, "plot has no 3D scene panels")
  scene_assert(panel <= length(panels), "panel is outside the plot layout")
  selected <- panels[[panel]]
  params <- selected$panel_params
  furniture <- selected$layout_furniture
  resolved <- scene_compile(new_scene_packet(panel_id = params$panel_id),
    furniture$camera, furniture$viewport)$camera
  camera <- export_camera_3d(resolved, furniture$camera, furniture$viewport, options$size)
  mm <- mm %or3d% camera$mm_per_unit
  scale <- resolved$model_scale * options$size
  light <- params$coord_params$light
  packets <- resolve_draw_packets_3d(selected$packets, furniture$camera, furniture$viewport)
  packet <- merge_scene_packets(resolve_glyph_packets_3d(packets, furniture$camera, furniture$viewport), params$panel_id)
  layer_key <- function(table) {
    known <- table$layer_id >= 1L & table$layer_id <= length(layer_names)
    ifelse(known, layer_names[pmax(1L, pmin(length(layer_names), table$layer_id))],
      paste0("layer", table$layer_id))
  }
  nodes <- export_nodes_3d(packet, scale, light, mm, layer_key)
  if (options$furniture) {
    tags <- furniture$tags
    furniture_key <- function(table) {
      type <- vapply(table$primitive_id, function(i) tags[[i]]$type %or3d% "", character(1))
      if (identical(attr(table, "kind"), "faces")) return(rep("panes", length(type)))
      ifelse(type == "edge", "box", "grid")
    }
    nodes <- c(nodes, export_nodes_3d(furniture$packet, scale, NULL, mm, furniture_key, parent = "furniture"))
  }
  list(nodes = nodes, camera = camera, up = as.integer(params$camera$up), mm = mm, viewport = furniture$viewport,
    box = scale, furniture_layout = furniture)
}

# The glTF-style camera of a resolved scene camera: eye and orthonormal basis
# in model units times `size`, half height of an orthographic view, vertical
# field of view of a perspective one, and millimetres per model unit at the
# target. An oblique view keeps its target and looks along the forward axis.
export_camera_3d <- function(resolved, camera, viewport, size) {
  sy <- resolved$projection[2L, 2L]
  perspective <- identical(camera$projection, "perspective")
  target <- resolved$eye + resolved$distance * resolved$ray
  eye <- target - resolved$distance * resolved$forward
  list(projection = if (perspective) "perspective" else "orthographic",
    eye = eye * size, target = target * size, right = resolved$right, up = resolved$up,
    forward = resolved$forward, aspect = viewport$width_mm / viewport$height_mm,
    ymag = size / sy, yfov = 2 * atan(1 / sy), near = resolved$near * size, far = resolved$far * size,
    mm_per_unit = 0.5 * sy * viewport$height_mm / (if (perspective) resolved$distance else 1) / size)
}

export_rgba_3d <- function(colour, opacity) {
  out <- matrix(NA_real_, length(colour), 4L)
  known <- !is.na(colour)
  if (any(known)) {
    out[known, ] <- t(colour_channels_3d(colour[known], alpha = TRUE)) / 255
    out[known, 4L] <- out[known, 4L] * opacity[known]
  }
  out
}

# Groups the primitives of a packet into named nodes. Each node holds a
# triangle soup (three rows per triangle), paths (consecutive rows sharing a
# path id), markers and text anchors, with sRGB colours and alpha in [0, 1].
export_nodes_3d <- function(packet, scale, light, mm, key, parent = NULL) {
  v <- packet$vertices
  model <- cbind(v$x * scale[1L], v$y * scale[2L], v$z * scale[3L])
  styles <- packet$styles
  vertex <- packet$corners$vertex
  table_key <- function(kind) {
    table <- packet[[kind]]
    attr(table, "kind") <- kind
    key(table)
  }
  parts <- list(triangles = NULL, paths = NULL, markers = NULL, text = NULL)
  f <- packet$faces
  path_rows <- list()
  if (length(f$offset)) {
    s <- styles[f$style_id, , drop = FALSE]
    keys <- table_key("faces")
    fill <- export_rgba_3d(style_value_3d(s, "fill", NA_character_), f$opacity)
    lit <- !is.null(light) & style_value_3d(s, "shade", FALSE) %in% TRUE
    n <- f$count - 2L
    face <- rep(seq_along(n), n); j <- sequence(n)
    keep <- !is.na(fill[face, 1L]) & fill[face, 4L] > 0
    face <- face[keep]; j <- j[keep]
    if (length(face)) {
      corners <- rbind(f$offset[face], f$offset[face] + j, f$offset[face] + j + 1L)
      parts$triangles <- list(position = model[vertex[as.vector(corners)], , drop = FALSE],
        identity = vertex[as.vector(corners)],
        colour = fill[face, , drop = FALSE], lit = lit[face], key = keys[face])
    }
    stroke <- export_rgba_3d(style_value_3d(s, "colour", NA_character_), f$opacity)
    width <- paint_metrics_3d(s)$width_mm
    blank <- as.character(style_value_3d(s, "linetype", 1)) %in% c("blank", "0")
    outlined <- which(!is.na(stroke[, 1L]) & stroke[, 4L] > 0 & width > 0 & !blank)
    if (length(outlined)) {
      edge_face <- rep(outlined, f$count[outlined])
      k <- sequence(f$count[outlined]) - 1L
      a <- vertex[f$offset[edge_face] + k]
      b <- vertex[f$offset[edge_face] + (k + 1L) %% f$count[edge_face]]
      id <- paste(keys[edge_face], pmin(a, b), pmax(a, b), width[edge_face],
        do.call(paste, c(as.data.frame(stroke[edge_face, , drop = FALSE]), sep = ",")))
      unique_edge <- !duplicated(id) & a != b
      edge_face <- edge_face[unique_edge]
      path_rows$outlines <- list(vertex = as.vector(rbind(a[unique_edge], b[unique_edge])),
        path = rep(seq_along(edge_face), each = 2L), colour = stroke[edge_face, , drop = FALSE],
        width_mm = width[edge_face], key = keys[edge_face])
    }
  }
  l <- packet$polylines
  if (length(l$offset)) {
    s <- styles[l$style_id, , drop = FALSE]
    stroke <- export_rgba_3d(style_value_3d(s, "colour", NA_character_), l$opacity)
    blank <- as.character(style_value_3d(s, "linetype", 1)) %in% c("blank", "0")
    drawn <- which(!is.na(stroke[, 1L]) & stroke[, 4L] > 0 & l$width_mm > 0 & !blank)
    if (length(drawn)) {
      path_rows$polylines <- list(
        vertex = vertex[rep(l$offset[drawn], l$count[drawn]) + sequence(l$count[drawn]) - 1L],
        path = rep(seq_along(drawn), l$count[drawn]), colour = stroke[drawn, , drop = FALSE],
        width_mm = l$width_mm[drawn], key = table_key("polylines")[drawn])
    }
  }
  if (length(path_rows)) {
    shift <- cumsum(c(0L, vapply(path_rows, function(p) length(p$width_mm), integer(1))))
    path <- unlist(Map(function(p, s) p$path + s, path_rows, head(shift, -1L)), use.names = FALSE)
    width <- unlist(lapply(path_rows, `[[`, "width_mm"), use.names = FALSE)
    parts$paths <- list(position = model[unlist(lapply(path_rows, `[[`, "vertex"), use.names = FALSE), , drop = FALSE],
      path = path, colour = do.call(rbind, lapply(path_rows, `[[`, "colour")),
      width_mm = width, radius = width / 2 / mm, key = unlist(lapply(path_rows, `[[`, "key"), use.names = FALSE))
  }
  m <- packet$markers
  if (length(m$offset)) {
    s <- styles[m$style_id, , drop = FALSE]
    shape <- style_value_3d(s, "shape", 19)
    filled <- shape %in% 21:25 & !is.na(style_value_3d(s, "fill", NA_character_))
    colour <- ifelse(filled, style_value_3d(s, "fill", NA_character_), style_value_3d(s, "colour", NA_character_))
    rgba <- export_rgba_3d(colour, m$opacity)
    radius_mm <- packet$footprints$radius_x_mm[m$footprint_id]
    drawn <- which(!is.na(rgba[, 1L]) & rgba[, 4L] > 0 & radius_mm > 0)
    if (length(drawn)) {
      parts$markers <- list(position = model[vertex[m$offset[drawn]], , drop = FALSE],
        colour = rgba[drawn, , drop = FALSE], radius_mm = radius_mm[drawn], radius = radius_mm[drawn] / mm,
        shape = if (is.numeric(shape)) shape[drawn] else rep(19, length(drawn)), key = table_key("markers")[drawn])
    }
  }
  tx <- packet$text
  if (length(tx$offset)) {
    s <- styles[tx$style_id, , drop = FALSE]
    label <- view_labels_3d(as.list(style_value_3d(s, "label", "")))
    rgba <- export_rgba_3d(style_value_3d(s, "colour", NA_character_), tx$opacity)
    drawn <- which(nzchar(label) & !is.na(rgba[, 1L]) & rgba[, 4L] > 0)
    if (length(drawn)) {
      parts$text <- list(position = model[vertex[tx$offset[drawn]], , drop = FALSE], label = label[drawn],
        colour = rgba[drawn, , drop = FALSE],
        fontsize = style_value_3d(s, "fontsize", style_value_3d(s, "size", 3.88) * 72.27 / 25.4)[drawn],
        family = style_value_3d(s, "family", "")[drawn], fontface = style_value_3d(s, "fontface", 1)[drawn],
        hjust = tx$hjust[drawn], vjust = tx$vjust[drawn], angle = tx$angle[drawn],
        key = table_key("text")[drawn])
    }
  }
  keys <- unique(unlist(lapply(parts, `[[`, "key"), use.names = FALSE))
  lapply(keys, function(name) {
    node <- list(name = name, parent = parent)
    tri <- parts$triangles
    if (!is.null(tri) && any(tri$key == name)) {
      i <- which(tri$key == name)
      node$triangles <- list(position = tri$position[as.vector(rbind(3L * i - 2L, 3L * i - 1L, 3L * i)), , drop = FALSE],
        identity = tri$identity[as.vector(rbind(3L * i - 2L, 3L * i - 1L, 3L * i))],
        colour = tri$colour[i, , drop = FALSE], lit = tri$lit[i])
    }
    p <- parts$paths
    if (!is.null(p) && any(p$key == name)) {
      i <- which(p$key == name)
      rows <- which(p$path %in% i)
      node$paths <- list(position = p$position[rows, , drop = FALSE], path = match(p$path[rows], i),
        colour = p$colour[i, , drop = FALSE], width_mm = p$width_mm[i], radius = p$radius[i])
    }
    for (kind in c("markers", "text")) {
      x <- parts[[kind]]
      if (is.null(x) || !any(x$key == name)) next
      i <- which(x$key == name)
      node[[kind]] <- lapply(x[setdiff(names(x), "key")], function(column) {
        if (is.matrix(column)) column[i, , drop = FALSE] else column[i]
      })
    }
    node
  })
}

# Rotates model coordinates so the plot's vertical axis becomes +z ("z") or
# +y ("y"). Both are proper rotations, so handedness and winding hold.
export_axes_3d <- function(xyz, up, frame = c("z", "y")) {
  frame <- match.arg(frame)
  standard <- xyz[, (0:2 + up) %% 3L + 1L, drop = FALSE]
  if (frame == "z") return(standard)
  cbind(standard[, 1L], standard[, 3L], -standard[, 2L])
}

# Meshes one node into primitives: list(mode, position, normal, colour, index,
# lit). Triangle primitives are split by lighting and translucency, so each
# maps to one material.
export_mesh_3d <- function(node, options) {
  soups <- list()
  if (!is.null(node$triangles)) {
    t <- node$triangles
    soups$faces <- export_soup_3d(t$position, matrix(seq_len(nrow(t$position)), 3L),
      t$colour[rep(seq_len(nrow(t$colour)), each = 3L), , drop = FALSE], t$lit)
  }
  if (!is.null(node$paths) && options$lines == "tube") {
    soups$tubes <- export_tubes_3d(node$paths)
  }
  if (!is.null(node$markers) && options$points == "mesh") {
    soups$solids <- export_solids_3d(node$markers)
  }
  prims <- list()
  soups <- Filter(Negate(is.null), soups)
  if (length(soups)) {
    all <- export_join_soups_3d(soups)
    alpha <- matrix(all$colour[all$index, 4L], 3L)
    blend <- colSums(alpha < 1) > 0
    for (lit in c(TRUE, FALSE)) for (translucent in c(FALSE, TRUE)) {
      keep <- which(all$lit == lit & blend == translucent)
      if (!length(keep)) next
      prims[[length(prims) + 1L]] <- c(list(mode = "triangles", lit = lit),
        export_compact_3d(all, all$index[, keep, drop = FALSE]))
    }
  }
  if (!is.null(node$paths) && options$lines == "line") {
    p <- node$paths
    n <- nrow(p$position)
    joined <- which(p$path[-1L] == p$path[-n])
    colour <- p$colour[p$path, , drop = FALSE]
    if (length(joined)) prims[[length(prims) + 1L]] <- list(mode = "lines", lit = FALSE,
      position = p$position, normal = NULL, colour = colour, index = rbind(joined, joined + 1L))
  }
  if (!is.null(node$markers) && options$points == "point") {
    m <- node$markers
    prims[[length(prims) + 1L]] <- list(mode = "points", lit = FALSE, position = m$position,
      normal = NULL, colour = m$colour, index = matrix(seq_len(nrow(m$position)), 1L))
  }
  prims
}

export_compact_3d <- function(mesh, index) {
  used <- sort(unique(as.vector(index)))
  list(position = mesh$position[used, , drop = FALSE], normal = mesh$normal[used, , drop = FALSE],
    colour = mesh$colour[used, , drop = FALSE], index = matrix(match(index, used), nrow(index)))
}

# Flat-shaded triangle soup: three unshared vertices per triangle carrying the
# face normal. `index` is a 3 x n matrix of rows of `position`; degenerate
# triangles are dropped.
export_soup_3d <- function(position, index, colour, lit) {
  a <- position[index[1L, ], , drop = FALSE]; b <- position[index[2L, ], , drop = FALSE]
  c <- position[index[3L, ], , drop = FALSE]
  normal <- cross_rows_3d(b - a, c - a)
  length <- sqrt(rowSums(normal^2))
  keep <- which(is.finite(length) & length > 0)
  if (!length(keep)) return(NULL)
  normal <- normal[keep, , drop = FALSE] / length[keep]
  rows <- as.vector(index[, keep, drop = FALSE])
  n <- length(keep)
  list(position = position[rows, , drop = FALSE], normal = normal[rep(seq_len(n), each = 3L), , drop = FALSE],
    colour = colour[rows, , drop = FALSE], index = matrix(seq_len(3L * n), 3L), lit = rep_len(lit, length(index[1L, ]))[keep])
}

export_join_soups_3d <- function(soups) {
  soups <- Filter(Negate(is.null), soups)
  if (!length(soups)) return(NULL)
  shift <- cumsum(c(0L, vapply(soups, function(s) nrow(s$position), integer(1))))
  index <- do.call(cbind, Map(function(s, k) s$index + k, soups, head(shift, -1L)))
  storage.mode(index) <- "integer"
  list(position = do.call(rbind, lapply(soups, `[[`, "position")),
    normal = do.call(rbind, lapply(soups, `[[`, "normal")), colour = do.call(rbind, lapply(soups, `[[`, "colour")),
    index = index, lit = unlist(lapply(soups, function(s) s$lit %or3d% rep(FALSE, ncol(s$index))), use.names = FALSE))
}

# Closed tubes share the rotation-minimising sweep and cap geometry.
export_tubes_3d <- function(paths, sides = 8L) {
  pieces <- lapply(seq_along(paths$radius), function(i) {
    xyz <- paths$position[paths$path == i, , drop = FALSE]
    if (nrow(xyz) < 2L) return(NULL)
    same <- c(FALSE, rowSums(abs(diff(xyz))) == 0)
    xyz <- xyz[!same, , drop = FALSE]
    if (nrow(xyz) < 2L) return(NULL)
    closed <- nrow(xyz) >= 4L && all(xyz[1L, ] == xyz[nrow(xyz), ])
    data <- curve_points_3d(stats::setNames(as.data.frame(xyz), c("x", "y", "z")), closed)
    surface <- sweep_surface_3d(data, rep(paths$radius[i], nrow(data)), rep(0, nrow(data)),
      sweep_profile_3d(NULL, sides, NULL), closed, "flat", NULL)
    export_soup_3d(as.matrix(surface[c("x", "y", "z")]), matrix(seq_len(nrow(surface)), 3L),
      paths$colour[rep(i, nrow(surface)), , drop = FALSE], FALSE)
  })
  export_join_soups_3d(pieces)
}

export_unit_rows_3d <- function(x) {
  length <- sqrt(rowSums(x^2))
  length[!(length > 0)] <- 1
  x / length
}

# A unit vector perpendicular to each row, built from the coordinate axis
# least aligned with it.
export_perpendicular_3d <- function(tangent) {
  axis <- max.col(-abs(tangent), ties.method = "first")
  e <- diag(3L)[axis, , drop = FALSE]
  export_unit_rows_3d(e - rowSums(e * tangent) * tangent)
}

# Unit solids for marker shapes as flat-shaded soups, each fitting the unit
# sphere: cube (squares), tetrahedron (triangles), octahedron (diamonds) and
# a once-subdivided icosahedron (all other shapes).
export_solid_templates_3d <- function() {
  cube_v <- as.matrix(expand.grid(c(-1, 1), c(-1, 1), c(-1, 1))) / sqrt(3)
  cube_f <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  cube_t <- rbind(cube_f[, c(1, 2, 3)], cube_f[, c(1, 3, 4)])
  tetra_v <- rbind(c(1, 1, 1), c(1, -1, -1), c(-1, 1, -1), c(-1, -1, 1)) / sqrt(3)
  tetra_t <- rbind(c(1, 2, 3), c(1, 4, 2), c(1, 3, 4), c(2, 4, 3))
  octa_v <- rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0), c(0, 0, 1), c(0, 0, -1))
  octa_t <- rbind(c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5), c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6))
  phi <- (1 + sqrt(5)) / 2
  ico_v <- rbind(c(-1, phi, 0), c(1, phi, 0), c(-1, -phi, 0), c(1, -phi, 0), c(0, -1, phi), c(0, 1, phi),
    c(0, -1, -phi), c(0, 1, -phi), c(phi, 0, -1), c(phi, 0, 1), c(-phi, 0, -1), c(-phi, 0, 1))
  ico_t <- rbind(c(1, 12, 6), c(1, 6, 2), c(1, 2, 8), c(1, 8, 11), c(1, 11, 12), c(2, 6, 10), c(6, 12, 5),
    c(12, 11, 3), c(11, 8, 7), c(8, 2, 9), c(4, 10, 5), c(4, 5, 3), c(4, 3, 7), c(4, 7, 9), c(4, 9, 10),
    c(5, 10, 6), c(3, 5, 12), c(7, 3, 11), c(9, 7, 8), c(10, 9, 2))
  a <- ico_v[ico_t[, 1L], ]; b <- ico_v[ico_t[, 2L], ]; c <- ico_v[ico_t[, 3L], ]
  ab <- (a + b) / 2; bc <- (b + c) / 2; ca <- (c + a) / 2
  k <- nrow(ico_t)
  sphere <- rbind(a, b, c, ab, bc, ca)
  sphere <- sphere / sqrt(rowSums(sphere^2))
  at <- function(block) (block - 1L) * k + seq_len(k)
  sphere_t <- rbind(cbind(at(1), at(4), at(6)), cbind(at(4), at(2), at(5)),
    cbind(at(6), at(5), at(3)), cbind(at(4), at(5), at(6)))
  build <- function(v, faces) {
    v <- v / sqrt(max(rowSums(v^2)))
    export_soup_3d(v, t(faces), matrix(1, nrow(v), 4L), FALSE)
  }
  list(cube = build(cube_v, cube_t), tetrahedron = build(tetra_v, tetra_t),
    octahedron = build(octa_v, octa_t), sphere = build(sphere, sphere_t))
}

export_solid_shape_3d <- function(shape) {
  ifelse(shape %in% c(0, 15, 22), "cube", ifelse(shape %in% c(2, 6, 17, 24, 25), "tetrahedron",
    ifelse(shape %in% c(5, 18, 23), "octahedron", "sphere")))
}

export_solids_3d <- function(markers) {
  templates <- export_solid_templates_3d()
  kind <- export_solid_shape_3d(markers$shape)
  pieces <- lapply(unique(kind), function(name) {
    template <- templates[[name]]
    rows <- which(kind == name)
    nv <- nrow(template$position)
    instance <- rep(rows, each = nv)
    list(position = markers$position[instance, , drop = FALSE] +
      template$position[rep(seq_len(nv), length(rows)), , drop = FALSE] * markers$radius[instance],
      normal = template$normal[rep(seq_len(nv), length(rows)), , drop = FALSE],
      colour = markers$colour[instance, , drop = FALSE],
      index = template$index[, rep(seq_len(ncol(template$index)), length(rows)), drop = FALSE] +
        rep((seq_along(rows) - 1L) * nv, each = length(template$index)))
  })
  export_join_soups_3d(pieces)
}

# All primitives of a scene merged into one indexed mesh in the z-up frame,
# with the triangle, segment and point index matrices kept apart.
export_merged_3d <- function(scene, options, frame = "z") {
  meshes <- unlist(lapply(scene$nodes, export_mesh_3d, options), recursive = FALSE)
  out <- list(position = matrix(0, 0L, 3L), normal = matrix(0, 0L, 3L), colour = matrix(0, 0L, 4L),
    triangles = matrix(integer(), 3L, 0L), lines = matrix(integer(), 2L, 0L), points = matrix(integer(), 1L, 0L))
  for (prim in meshes) {
    shift <- nrow(out$position)
    out$position <- rbind(out$position, export_axes_3d(prim$position, scene$up, frame))
    out$normal <- rbind(out$normal, if (is.null(prim$normal)) matrix(0, nrow(prim$position), 3L) else
      export_axes_3d(prim$normal, scene$up, frame))
    out$colour <- rbind(out$colour, prim$colour)
    out[[prim$mode]] <- cbind(out[[prim$mode]], prim$index + shift)
  }
  out
}
