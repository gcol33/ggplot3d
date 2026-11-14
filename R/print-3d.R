#' Physical solids for 3D printing
#'
#' Maps the panel box into a millimetre bounding box with a uniform scale,
#' keeping its aspect, then applies physical feature dimensions. Strokes use
#' the rotation-minimising tube sweep; markers use their solid templates.
#' Tube radii include the polygonal cross-section's inradius correction.
#' Marker solids contain an inscribed ball of diameter min_mm.
#' Open faces become normal-offset shells or solids closed down to the base.
#' Layer-specific surface methods are keyed by the names from export_3d().
#'
#' Raised and engraved character labels use font outlines on backing plaques.
#' Outlines expand by min_mm / 2 and the extrusion depth is min_mm. A plaque
#' is min_mm thick for raised text and 2 * min_mm for engraved text. Axis
#' labels use the existing in-plane guide layout. Text and axis furniture
#' can extend beyond the panel box.
#'
#' Meshes are checked for edge incidence, winding and self-intersections.
#' Manifold performs Boolean union, preserves cavity boundaries and carries
#' colour properties through the bundled native Manifold library, using
#' double-precision meshes and a single-threaded Boolean engine.
#' Disconnected solid components are stored in one model object.
#' @param size_mm Three positive maximum dimensions of the panel box.
#' @param min_mm Minimum tube or marker width and text extrusion depth.
#' @param shell_mm Normal-offset thickness, clamped to min_mm.
#' @param base_mm Base thickness, clamped to min_mm when positive.
#' @param text `"raised"`, `"engraved"` or `"none"`.
#' @param furniture Components to include: `"base"` and/or `"axes"`.
#' @param surface Default open-surface method: `"shell"` or `"base"`.
#' @param layers Named vector of surface methods, keyed by layer name such as
#'   `c(layer1_surface = "base", layer2_mesh = "shell")`.
#' @param sides Number of tube cross-section sides, at least six.
#' @return A printing specification for `export_3d(..., print = )`.
#' @examples
#' settings <- print_3d(size_mm = c(100, 100, 60), text = "none")
#' settings$min_mm
#' @export
print_3d <- function(size_mm = c(100, 100, 60), min_mm = .8, shell_mm = 1.2, base_mm = 2,
                      text = c("raised", "engraved", "none"), furniture = c("base", "axes"),
                      surface = c("shell", "base"), layers = NULL, sides = 12L) {
  scene_assert(is.numeric(size_mm) && length(size_mm) == 3L && all(is.finite(size_mm) & size_mm > 0), "size_mm must contain three positive dimensions")
  for (name in c("min_mm", "shell_mm", "base_mm")) {
    value <- get(name)
    scene_assert(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0 && (name == "base_mm" || value > 0), paste(name, "has an invalid physical dimension"))
  }
  scene_assert(is.character(furniture) && !anyNA(furniture) && all(furniture %in% c("base", "axes")), "furniture must select base and/or axes")
  if (!is.null(layers)) scene_assert(is.character(layers) && !is.null(names(layers)) &&
    !anyNA(layers) && !anyDuplicated(names(layers)) && all(nzchar(names(layers))) && all(layers %in% c("shell", "base")), "layers must name a shell or base method for each selected layer")
  scene_assert(is.numeric(sides) && length(sides) == 1L && is.finite(sides) && sides >= 6 && sides == trunc(sides), "sides must be an integer of at least six")
  structure(list(size_mm = as.double(size_mm), min_mm = min_mm, shell_mm = max(min_mm, shell_mm),
    base_mm = if (base_mm > 0) max(min_mm, base_mm) else 0, text = match.arg(text), furniture = unique(furniture),
    surface = match.arg(surface), layers = layers, sides = as.integer(sides)), class = "ggplot3d_print")
}

print_layer_error_3d <- function(name, expression) {
  tryCatch(force(expression), error = function(e) stop(name, ": ", conditionMessage(e), call. = FALSE))
}

print_parts_3d <- function(mesh, name, settings, method = settings$surface, base_z = 0, engrave = FALSE) {
  components <- print_layer_error_3d(name, print_mesh_cpp(mesh$position, t(mesh$index), identity = mesh$identity))$parts
  closed <- vapply(components, `[[`, logical(1), "closed")
  volume <- vapply(components, `[[`, numeric(1), "volume")
  reverse_all <- any(closed) && all(volume[closed] < 0)
  unlist(lapply(components, function(component) {
    colour <- mesh$colour[mesh$index[1L, component$source], , drop = FALSE]
    if (!component$closed) {
      n <- nrow(component$position)
      if (method == "shell") {
        scene_assert(all(rowSums(component$normal^2) > 0), paste(name, "has an undefined shell vertex normal"))
        top <- component$position + component$normal * settings$shell_mm / 2
        bottom <- component$position - component$normal * settings$shell_mm / 2
      } else {top <- component$position;bottom <- top;bottom[, 3L] <- base_z}
      a <- component$boundary[, 1L];b <- component$boundary[, 2L]
      triangles <- rbind(component$triangles, component$triangles[, 3:1, drop = FALSE] + n,
        cbind(a, b + n, b), cbind(a, a + n, b + n))
      wall <- mesh$colour[mesh$index[1L, component$boundary_source], , drop = FALSE]
      colours <- rbind(colour, colour, wall, wall)
      shell <- print_layer_error_3d(name, print_mesh_cpp(rbind(top, bottom), triangles))$parts
      return(lapply(shell, function(s) {
        scene_assert(s$closed, paste(name, "could not be closed"))
        t <- if (s$volume < 0) s$triangles[, 3:1, drop = FALSE] else s$triangles
        list(name = name, role = if (engrave) 2L else 0L, position = s$position, triangles = t,
          colour = colours[s$source, , drop = FALSE])
      }))
    }
    negative <- component$volume < 0
    list(list(name = name, role = if (engrave) 2L else if (negative && !reverse_all) 1L else 0L,
      position = component$position, triangles = if (negative) component$triangles[, 3:1, drop = FALSE] else component$triangles,
      colour = colour))
  }), recursive = FALSE)
}

print_box_mesh_3d <- function(size, origin = c(0, 0, 0), colour = c(.85, .85, .85, 1)) {
  box <- export_solid_templates_3d()$cube
  box$position <- sweep(sweep(box$position * sqrt(3), 2L, size / 2, "*"), 2L, origin + size / 2, "+")
  box$colour[,] <- matrix(colour, nrow(box$colour), 4L, byrow = TRUE)
  box
}

print_paths_3d <- function(paths, name, settings) {
  unlist(lapply(seq_along(paths$radius), function(i) {
    path <- list(position = paths$position[paths$path == i, , drop = FALSE],
      path = rep(1L, sum(paths$path == i)), colour = paths$colour[i, , drop = FALSE],
      radius = max(paths$radius[i], settings$min_mm / (2 * cos(pi / settings$sides))))
    mesh <- export_tubes_3d(path, settings$sides)
    if (is.null(mesh)) return(list())
    print_parts_3d(mesh, name, settings)
  }), recursive = FALSE)
}

print_text_3d <- function(label, fontsize, family, face, centre, u, v, colour, settings,
                          font_scale, name, hjust = .5, vjust = .5) {
  outline <- print_layer_error_3d(name, text_outline_3d(label, fontsize * font_scale, family, face, hjust, vjust))
  if (!length(outline$x)) return(NULL)
  normal <- cross_rows_3d(matrix(u, 1L), matrix(v, 1L))[1L, ]
  normal <- normal / sqrt(sum(normal^2))
  list(name = name, mode = if (settings$text == "engraved") 2L else 1L,
    depth = settings$min_mm, dilation = settings$min_mm / 2, colour = colour,
    transform = cbind(u, v, normal, centre),
    contours = unname(split(data.frame(x = outline$x, y = outline$y), outline$id)))
}

print_axis_parts_3d <- function(scene, settings, transform, fit) {
  furniture <- scene$furniture_layout;overlay <- furniture$overlay;frame <- overlay$frame
  camera <- furniture$camera;viewport <- furniture$viewport
  basis <- frame_basis_3d(camera, viewport)
  project <- function(xyz) {
    p <- frame_positions_3d(xyz, camera, basis)
    cbind(p[, 1L] * viewport$width_mm, p[, 2L] * viewport$height_mm, p[, 3L])
  }
  ctx <- typography_context_3d(frame, camera, viewport, project)
  meshes <- texts <- list();occupied <- matrix(numeric(), 0L, 4L)
  for (name in names(overlay$params$axis_guides)) {
    guide <- overlay$params$axis_guides[[name]]
    layout <- axis_layout_3d(frame, guide, project, project(frame$bounds()))
    if (is.null(layout)) next
    label <- paste0("axis_", name)
    line <- transform(sweep(frame$map(axis_candidate_points_3d(layout$candidate, seq(0, 1, length.out = 33L))), 2L, scene$box, "*"))
    paths <- list(position = line, path = rep(1L, nrow(line)), colour = matrix(c(.3, .3, .3, 1), 1L), radius = settings$min_mm / 2)
    meshes <- c(meshes, print_paths_3d(paths, label, settings))
    guide$params$label.orientation <- if (settings$text == "none") "screen" else "plane"
    guide$params$ticks <- "in_plane"
    typography <- print_layer_error_3d(label, guide_typography_3d(ctx, guide, overlay$fit$metrics[[name]], layout))
    for (kind in c("major", "minor")) {
      ticks <- typography[[kind]]
      if (is.null(ticks) || is.null(ticks$model_from)) next
      rows <- seq_len(nrow(ticks$model_from))
      p <- rbind(ticks$model_from, ticks$model_to)[as.vector(rbind(rows, rows + length(rows))), , drop = FALSE]
      paths <- list(position = transform(sweep(p, 2L, scene$box, "*")), path = rep(rows, each = 2L),
        colour = matrix(c(.3, .3, .3, 1), length(rows), 4L, byrow = TRUE), radius = rep(settings$min_mm / 2, length(rows)))
      meshes <- c(meshes, print_paths_3d(paths, label, settings))
    }
    if (settings$text == "none") next
    metrics <- overlay$fit$metrics[[name]]
    add <- function(text, element, centre, axes) {
      print_text_3d(text, element$size, element$family %or3d% "", element$face %or3d% "plain",
        transform(matrix(centre * scene$box, 1L))[1L, ], export_axes_3d(matrix(axes$u, 1L), scene$up)[1L, ],
        export_axes_3d(matrix(axes$v, 1L), scene$up)[1L, ], export_rgba_3d(element$colour, 1)[1L, ],
        settings, fit / scene$mm, label)
    }
    labels <- typography$labels
    if (!is.null(labels)) {
      thinned <- thin_axis_labels_3d(labels$location, labels$size, layout$tangent, occupied, viewport,
        gap = .5, thin = thin_labels_of_3d(guide))
      occupied <- thinned$occupied
      for (i in which(thinned$keep)) texts <- c(texts, list(add(guide$labels[[i]], metrics$text, labels$centres[i, ], labels$axes[[i]])))
    }
    if (!is.null(typography$title)) texts <- c(texts, list(add(guide$title, metrics$title, typography$title$centre, typography$title$axes)))
  }
  list(meshes = meshes, texts = Filter(Negate(is.null), texts))
}

print_scene_3d <- function(scene, settings) {
  scene_assert(is.null(settings$layers) || all(names(settings$layers) %in% scene$layers$name), "layers contains unknown exported layer names")
  box <- as.numeric(export_axes_3d(matrix(scene$box, 1L), scene$up))
  fit <- min(settings$size_mm / box)
  base <- if ("base" %in% settings$furniture) settings$base_mm else 0
  transform <- function(xyz) sweep(export_axes_3d(xyz, scene$up) * fit, 2L, c(0, 0, base), "+")
  meshes <- texts <- list()
  if (base > 0) meshes <- print_parts_3d(print_box_mesh_3d(c(box[1:2] * fit, base)), "base", settings)
  for (node in scene$nodes) {
    name <- node$name
    is_text <- isTRUE(scene$layers$text[match(name, scene$layers$name)])
    if (is_text && settings$text == "none") next
    method <- if (name %in% names(settings$layers)) settings$layers[[name]] else settings$surface
    if (!is.null(node$triangles)) {
      t <- node$triangles
      mesh <- list(position = transform(t$position), index = matrix(seq_len(nrow(t$position)), 3L),
        colour = t$colour[rep(seq_len(nrow(t$colour)), each = 3L), , drop = FALSE], identity = t$identity)
      meshes <- c(meshes, print_parts_3d(mesh, name, settings, method, base_z = base / 2,
        engrave = is_text && settings$text == "engraved"))
    }
    if (!is.null(node$paths)) {
      paths <- node$paths;paths$position <- transform(paths$position)
      paths$radius <- paths$radius * fit
      meshes <- c(meshes, print_paths_3d(paths, name, settings))
    }
    if (!is.null(node$markers)) {
      m <- node$markers;m$position <- transform(m$position)
      templates <- export_solid_templates_3d();kind <- export_solid_shape_3d(m$shape)
      span <- vapply(templates, function(t) 2 * min(rowSums(t$position * t$normal)), numeric(1))
      m$radius <- pmax(m$radius * fit, settings$min_mm / span[kind])
      for (i in seq_len(nrow(m$position))) {
        marker <- lapply(m, function(x) if (is.matrix(x)) x[i, , drop = FALSE] else x[i])
        meshes <- c(meshes, print_parts_3d(export_solids_3d(marker), name, settings))
      }
    }
    if (!is.null(node$text) && settings$text != "none") for (i in seq_along(node$text$label)) {
      t <- node$text;angle <- t$angle[i] * pi / 180
      u <- cos(angle) * scene$camera$right + sin(angle) * scene$camera$up
      v <- -sin(angle) * scene$camera$right + cos(angle) * scene$camera$up
      texts <- c(texts, list(print_text_3d(t$label[i], t$fontsize[i], t$family[i], t$fontface[i],
        transform(t$position[i, , drop = FALSE])[1L, ], export_axes_3d(matrix(u, 1L), scene$up)[1L, ],
        export_axes_3d(matrix(v, 1L), scene$up)[1L, ], t$colour[i, ], settings, fit / scene$mm, name, t$hjust[i], t$vjust[i])))
    }
  }
  if ("axes" %in% settings$furniture) {
    axes <- print_axis_parts_3d(scene, settings, transform, fit)
    meshes <- c(meshes, axes$meshes);texts <- c(texts, axes$texts)
  }
  texts <- Filter(Negate(is.null), texts)
  scene_assert(length(meshes) + length(texts) > 0, "The selected panel has no printable geometry")
  solid <- print_union_3d(meshes, texts)
  topology <- print_layer_error_3d(paste(unique(vapply(meshes, `[[`, character(1), "name")), collapse = ", "),
    print_mesh_cpp(solid$position, solid$triangles, TRUE))
  checked <- topology$parts
  scene_assert(all(vapply(checked, `[[`, logical(1), "closed")) && sum(vapply(checked, `[[`, numeric(1), "volume")) > 0,
    "The union is not a closed, positively wound solid")
  index <- as.vector(t(solid$triangles))
  colours <- t(vapply(seq_len(nrow(solid$triangles)), function(i)
    colMeans(solid$colour[solid$triangles[i, ], , drop = FALSE]), numeric(4)))
  topology <- list(position = topology$position, triangles = topology$triangles, colour = colours[topology$source, , drop = FALSE])
  list(nodes = list(list(name = "print", triangles = list(position = solid$position[index, , drop = FALSE],
    colour = colours, lit = rep(FALSE, nrow(colours))))), up = 3L, printing = settings, printing_mesh = topology)
}

utils::globalVariables(c("print_mesh_cpp", "print_zip_cpp", "print_union_cpp"))
