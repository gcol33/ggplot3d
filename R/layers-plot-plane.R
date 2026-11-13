# Two-dimensional graphics on planes of the panel box. A grob drawn in the unit
# square of a 2D panel is decomposed into points, lines, polygons, rectangles
# and text in that square's (u, v) coordinates, and the primitives are emitted
# as faces lying on a plane of the box. matplotlib places 2D artists on a
# plane of a 3D axes the same way (zdir and zs of its 2D plotting functions),
# and ggcube projects layers onto the faces of its cube.

# Side of the square device, in inches, on which 2D grobs are measured: units
# that are not data units, such as text offsets and rug lengths, are resolved
# for a panel of this size.
plane_panel_inches_3d <- 4

# Plane axes from two distinct axis letters: the first carries the 2D x, the
# second the 2D y, the third is the fixed axis.
check_plane_axes_3d <- function(plane) {
  if (!is.character(plane) || length(plane) != 1L || !grepl("^[xyz]{2}$", plane) || substr(plane, 1L, 1L) == substr(plane, 2L, 2L)) {
    stop("plane must be two distinct axis letters, such as \"xz\".", call. = FALSE)
  }
  axes <- strsplit(plane, "")[[1L]]
  c(axes, setdiff(c("x", "y", "z"), axes))
}

# Position on a fixed axis: an infinite position lands on a box side, which
# needs a trained range on that axis; without one the plane sits at 0.
plane_at_3d <- function(at, scales, axis) {
  untrained <- is.null(scales[[axis]]) || is.null(scales[[axis]]$range$range)
  if (is.infinite(at) && untrained) 0 else at
}

# Moves a packet emitted in a horizontal plane onto the plane spanned by
# axes[1] (emitted x) and axes[2] (emitted y) at axes[3] (emitted z).
orient_packet_3d <- function(packet, axes) {
  if (identical(axes, c("x", "y", "z"))) return(packet)
  emitted <- packet$vertices[c("x", "y", "z")]
  for (k in seq_len(3L)) packet$vertices[[axes[k]]] <- emitted[[k]]
  packet
}

merge_gpar_3d <- function(parent, child) {
  if (is.null(child) || !length(child)) return(parent)
  child <- unclass(child)
  alpha <- (parent$alpha %or3d% 1) * (child$alpha %or3d% 1)
  parent[names(child)] <- child
  parent$alpha <- alpha
  parent
}

gp_value_3d <- function(gp, name, n, default) {
  value <- gp[[name]]
  if (is.null(value) || !length(value)) value <- default
  if (name %in% c("col", "fill") && !is.character(value) && !is.numeric(value) && !is.logical(value)) value <- NA_character_
  if (name %in% c("col", "fill") && is.logical(value)) value <- as.character(value)
  rep_len(value, n)
}

justification_3d <- function(just, hjust, vjust) {
  if (!is.null(hjust) && !is.null(vjust)) return(c(hjust[1L], vjust[1L]))
  horizontal <- c(left = 0, right = 1, centre = 0.5, center = 0.5)
  vertical <- c(bottom = 0, top = 1, centre = 0.5, center = 0.5)
  out <- c(0.5, 0.5)
  if (is.numeric(just)) out <- rep_len(just, 2L)
  else if (length(just) == 1L) {
    if (just %in% names(horizontal) && !just %in% c("centre", "center")) out[1L] <- horizontal[[just]]
    if (just %in% c("bottom", "top")) out[2L] <- vertical[[just]]
  } else if (length(just) >= 2L) out <- c(horizontal[[just[1L]]], vertical[[just[2L]]])
  c(hjust[1L] %or3d% out[1L], vjust[1L] %or3d% out[2L])
}

# Line paint of grid graphics parameters in ggplot2 units.
line_paint_3d <- function(gp, n) {
  data.frame(colour = gp_value_3d(gp, "col", n, "black"),
    linewidth = gp_value_3d(gp, "lwd", n, 1) / ggplot2::.pt,
    linetype = gp_value_3d(gp, "lty", n, 1),
    alpha = gp_value_3d(gp, "alpha", n, 1), stringsAsFactors = FALSE)
}

# Cells of a raster image as rectangles in the box [u0, u1] x [v0, v1]. Runs of
# equal colour along a row join into one rectangle, and runs repeated in
# consecutive rows join into taller ones; transparent cells are dropped.
raster_cells_3d <- function(image, u = c(0, 1), v = c(0, 1)) {
  colours <- as.matrix(grDevices::as.raster(image))
  rows <- nrow(colours); columns <- ncol(colours)
  if (!rows || !columns) return(NULL)
  start <- cbind(TRUE, colours[, -1L, drop = FALSE] != colours[, -columns, drop = FALSE])
  start[is.na(start)] <- TRUE
  runs <- which(t(start), arr.ind = TRUE)
  runs <- data.frame(row = runs[, 2L], first = runs[, 1L])
  runs <- runs[order(runs$row, runs$first), , drop = FALSE]
  following <- c(runs$first[-1L], NA)
  same_row <- c(runs$row[-1L], NA) == runs$row
  runs$last <- ifelse(!is.na(same_row) & same_row, following - 1L, columns)
  runs$colour <- colours[cbind(runs$row, runs$first)]
  runs <- runs[!is.na(runs$colour) & grDevices::col2rgb(runs$colour, alpha = TRUE)[4L, ] > 0, , drop = FALSE]
  if (!nrow(runs)) return(NULL)
  key <- paste(runs$first, runs$last, runs$colour)
  runs$top <- runs$row
  open <- integer()
  keep <- logical(nrow(runs))
  for (r in unique(runs$row)) {
    here <- which(runs$row == r)
    previous <- open[match(key[here], key[open])]
    continued <- !is.na(previous) & runs$row[previous] == r - 1L
    runs$row[previous[continued]] <- r
    keep[here[!continued]] <- TRUE
    open <- c(open, here[!continued])
    open <- open[runs$row[open] == r]
  }
  runs <- runs[keep, , drop = FALSE]
  du <- diff(u) / columns; dv <- diff(v) / rows
  data.frame(xmin = u[1L] + (runs$first - 1) * du, xmax = u[1L] + runs$last * du,
    ymin = v[2L] - runs$row * dv, ymax = v[2L] - (runs$top - 1) * dv,
    fill = runs$colour, colour = NA_character_, linewidth = 0, linetype = 1, alpha = 1, .order = 1L,
    stringsAsFactors = FALSE)
}

# Decomposes a grob into primitive tables in the (u, v) unit square of the
# viewport it is drawn in, following grid's drawing sequence: makeContext,
# viewport and graphical parameters, makeContent, then the children.
grob_primitives_3d <- function(grob, size = plane_panel_inches_3d) {
  previous <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = size, height = size)
  device <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(device)
    if (previous > 1L) grDevices::dev.set(previous)
  }, add = TRUE)
  grid::pushViewport(grid::viewport(xscale = c(0, 1), yscale = c(0, 1), name = "ggplot3d.plane"))
  out <- new.env(parent = emptyenv())
  kinds <- c("points", "paths", "polygons", "rects", "text")
  for (kind in kinds) out[[kind]] <- list()
  out$size <- size
  out$group <- 0L
  out$order <- 0L
  walk_grob_3d(grob, list(), out)
  tables <- lapply(kinds, function(kind) if (length(out[[kind]])) do.call(rbind, out[[kind]]) else NULL)
  names(tables) <- kinds
  tables
}

walk_grob_3d <- function(grob, gp, out) {
  if (is.null(grob) || inherits(grob, c("zeroGrob", "null"))) return(invisible())
  grob <- grid::makeContext(grob)
  path <- grid::current.vpPath()
  moved <- FALSE
  if (!is.null(grob$vp)) {
    if (inherits(grob$vp, "vpPath")) grid::downViewport(grob$vp) else grid::pushViewport(grob$vp)
    if (inherits(grob$vp, "viewport")) gp <- merge_gpar_3d(gp, grob$vp$gp)
    moved <- TRUE
  }
  gp <- merge_gpar_3d(gp, grob$gp)
  grob <- grid::makeContent(grob)
  if (inherits(grob, "gTree")) {
    if (!is.null(grob$childrenvp)) {
      here <- grid::current.vpPath()
      grid::pushViewport(grob$childrenvp)
      grid::upViewport(0L)
      if (!is.null(here)) grid::downViewport(here)
    }
    for (child in grob$children[grob$childrenOrder]) walk_grob_3d(child, gp, out)
  } else {
    primitive_grob_3d(grob, gp, out)
  }
  if (moved || !is.null(grob$childrenvp)) {
    grid::upViewport(0L)
    if (!is.null(path)) grid::downViewport(path)
  }
  invisible()
}

grob_location_3d <- function(x, y, out) {
  location <- grid::deviceLoc(x, y, valueOnly = TRUE)
  list(u = as.numeric(location$x) / out$size, v = as.numeric(location$y) / out$size)
}

next_groups_3d <- function(out, n) {
  ids <- out$group + seq_len(n)
  out$group <- out$group + n
  ids
}

# Each table records its drawing order, so marks of different kinds that lie
# in one plane are painted as grid draws them.
add_primitive_3d <- function(out, kind, table) {
  if (is.null(table) || !nrow(table)) return(invisible())
  out$order <- out$order + 1L
  table$.order <- rep(out$order, nrow(table))
  out[[kind]][[length(out[[kind]]) + 1L]] <- table
}

# Vertices of grid polylines, polygons and paths: one style per id.
grouped_vertices_3d <- function(grob, out) {
  n <- length(grob$x)
  id <- if (!is.null(grob$id)) grob$id else if (!is.null(grob$id.lengths)) rep(seq_along(grob$id.lengths), grob$id.lengths) else rep.int(1L, n)
  location <- grob_location_3d(grob$x, grob$y, out)
  ids <- unique(id)
  list(u = location$u, v = location$v, style = match(id, ids), count = length(ids))
}

primitive_grob_3d <- function(grob, gp, out) {
  class <- class(grob)[1L]
  if (class == "points") {
    n <- length(grob$x)
    if (!n) return(invisible())
    location <- grob_location_3d(grob$x, grob$y, out)
    lwd <- gp_value_3d(gp, "lwd", n, 1)
    fontsize <- gp_value_3d(gp, "fontsize", n, 12) * gp_value_3d(gp, "cex", n, 1)
    size <- grob$size %or3d% grid::unit(1, "char")
    fontsize <- if (all(grid::unitType(size) == "char")) fontsize * rep_len(as.numeric(size), n) else rep_len(grid::convertHeight(size, "points", TRUE), n)
    add_primitive_3d(out, "points", data.frame(x = location$u, y = location$v,
      colour = gp_value_3d(gp, "col", n, "black"), fill = gp_value_3d(gp, "fill", n, NA_character_),
      shape = rep_len(grob$pch %or3d% 1, n), size = pmax(fontsize - lwd, 0) / ggplot2::.pt,
      stroke = lwd * 2 / ggplot2::.stroke, alpha = gp_value_3d(gp, "alpha", n, 1), stringsAsFactors = FALSE))
  } else if (class %in% c("polyline", "lines", "xspline")) {
    if (class == "xspline") {
      points <- grid::xsplinePoints(grob)
      if (!is.null(points$x)) points <- list(points)
      grob <- list(x = do.call(grid::unit.c, lapply(points, `[[`, "x")), y = do.call(grid::unit.c, lapply(points, `[[`, "y")),
        id = rep(seq_along(points), vapply(points, function(p) length(p$x), integer(1))))
    }
    if (!length(grob$x)) return(invisible())
    vertices <- grouped_vertices_3d(grob, out)
    paint <- line_paint_3d(gp, vertices$count)
    # A missing vertex breaks a grid line, as grid draws it.
    missing <- !(is.finite(vertices$u) & is.finite(vertices$v))
    run <- paste(vertices$style, cumsum(missing))[!missing]
    if (!length(run)) return(invisible())
    pieces <- match(run, unique(run))
    style <- vertices$style[!missing]
    add_primitive_3d(out, "paths", cbind(data.frame(x = vertices$u[!missing], y = vertices$v[!missing],
      group = next_groups_3d(out, max(pieces))[pieces]), paint[style, , drop = FALSE]))
  } else if (class == "segments") {
    n <- max(length(grob$x0), length(grob$y0), length(grob$x1), length(grob$y1))
    if (!n) return(invisible())
    start <- grob_location_3d(rep(grob$x0, length.out = n), rep(grob$y0, length.out = n), out)
    end <- grob_location_3d(rep(grob$x1, length.out = n), rep(grob$y1, length.out = n), out)
    paint <- line_paint_3d(gp, n)
    groups <- next_groups_3d(out, n)
    add_primitive_3d(out, "paths", cbind(data.frame(x = as.vector(rbind(start$u, end$u)), y = as.vector(rbind(start$v, end$v)),
      group = rep(groups, each = 2L)), paint[rep(seq_len(n), each = 2L), , drop = FALSE]))
  } else if (class %in% c("polygon", "pathgrob")) {
    if (!length(grob$x)) return(invisible())
    if (class == "pathgrob") {
      ring <- if (!is.null(grob$id)) grob$id else if (!is.null(grob$id.lengths)) rep(seq_along(grob$id.lengths), grob$id.lengths) else rep.int(1L, length(grob$x))
      feature <- if (!is.null(grob$pathId)) grob$pathId else if (!is.null(grob$pathId.lengths)) rep(seq_along(grob$pathId.lengths), grob$pathId.lengths) else rep.int(1L, length(grob$x))
      vertices <- grouped_vertices_3d(list(x = grob$x, y = grob$y, id = feature), out)
      subgroup <- ring
    } else {
      vertices <- grouped_vertices_3d(grob, out)
      subgroup <- rep.int(1L, length(vertices$u))
    }
    paint <- line_paint_3d(gp, vertices$count)
    paint$fill <- gp_value_3d(gp, "fill", vertices$count, NA_character_)
    add_primitive_3d(out, "polygons", cbind(data.frame(x = vertices$u, y = vertices$v,
      group = next_groups_3d(out, vertices$count)[vertices$style], subgroup = subgroup), paint[vertices$style, , drop = FALSE]))
  } else if (class %in% c("rect", "roundrect")) {
    n <- max(length(grob$x), length(grob$y), length(grob$width), length(grob$height))
    if (!n) return(invisible())
    just <- justification_3d(grob$just, grob$hjust, grob$vjust)
    x <- rep(grob$x, length.out = n); y <- rep(grob$y, length.out = n)
    width <- rep(grob$width, length.out = n); height <- rep(grob$height, length.out = n)
    low <- grob_location_3d(x - width * just[1L], y - height * just[2L], out)
    high <- grob_location_3d(x + width * (1 - just[1L]), y + height * (1 - just[2L]), out)
    paint <- line_paint_3d(gp, n)
    paint$fill <- gp_value_3d(gp, "fill", n, NA_character_)
    add_primitive_3d(out, "rects", cbind(data.frame(xmin = pmin(low$u, high$u), xmax = pmax(low$u, high$u),
      ymin = pmin(low$v, high$v), ymax = pmax(low$v, high$v)), paint))
  } else if (class == "circle") {
    n <- max(length(grob$x), length(grob$y), length(grob$r))
    if (!n) return(invisible())
    centre <- grob_location_3d(rep(grob$x, length.out = n), rep(grob$y, length.out = n), out)
    radius <- rep_len(pmin(grid::convertWidth(grob$r, "inches", TRUE), grid::convertHeight(grob$r, "inches", TRUE)), n) / out$size
    angle <- seq(0, 2 * pi, length.out = 33L)[-33L]
    paint <- line_paint_3d(gp, n)
    paint$fill <- gp_value_3d(gp, "fill", n, NA_character_)
    keep <- which(radius > 0)
    if (!length(keep)) return(invisible())
    groups <- next_groups_3d(out, length(keep))
    add_primitive_3d(out, "polygons", cbind(data.frame(
      x = as.vector(outer(cos(angle), radius[keep]) + rep(centre$u[keep], each = 32L)),
      y = as.vector(outer(sin(angle), radius[keep]) + rep(centre$v[keep], each = 32L)),
      group = rep(groups, each = 32L), subgroup = 1L), paint[rep(keep, each = 32L), , drop = FALSE]))
  } else if (class == "text") {
    label <- grob$label
    n <- length(label)
    if (!n) return(invisible())
    if (is.expression(label) || is.call(label)) label <- as.character(label)
    location <- grob_location_3d(rep(grob$x, length.out = n), rep(grob$y, length.out = n), out)
    just <- justification_3d(grob$just, grob$hjust, grob$vjust)
    add_primitive_3d(out, "text", data.frame(x = location$u, y = location$v, label = as.character(label),
      colour = gp_value_3d(gp, "col", n, "black"),
      size = gp_value_3d(gp, "fontsize", n, 12) * gp_value_3d(gp, "cex", n, 1) / ggplot2::.pt,
      angle = rep_len(grob$rot %or3d% 0, n), hjust = rep_len(grob$hjust %or3d% just[1L], n),
      vjust = rep_len(grob$vjust %or3d% just[2L], n), family = gp_value_3d(gp, "fontfamily", n, ""),
      fontface = gp_value_3d(gp, "font", n, 1), alpha = gp_value_3d(gp, "alpha", n, 1), stringsAsFactors = FALSE))
  } else if (class == "rastergrob") {
    just <- justification_3d(grob$just, grob$hjust, grob$vjust)
    width <- grob$width %or3d% grid::unit(1, "npc"); height <- grob$height %or3d% grid::unit(1, "npc")
    low <- grob_location_3d(grob$x - width * just[1L], grob$y - height * just[2L], out)
    high <- grob_location_3d(grob$x + width * (1 - just[1L]), grob$y + height * (1 - just[2L]), out)
    cells <- raster_cells_3d(grob$raster, c(low$u, high$u), c(low$v, high$v))
    if (!is.null(cells)) cells$alpha <- gp_value_3d(gp, "alpha", 1L, 1)
    add_primitive_3d(out, "rects", cells)
  } else {
    stop("Cannot place a grob of class ", class, " on a 3D plane.", call. = FALSE)
  }
  invisible()
}

# Faces of a picture lying in its plane. Faces in one plane are painted in
# input order, while strokes and screen-facing markers on them would be split
# at every face edge or cut by the inclined face; so every mark of the picture
# becomes convex faces in the 2D drawing order, as the raster image of the
# same panel shows it: points become the outline of their shape, lines become
# strips of quadrilaterals with mitred joins and their dash pattern, and
# polygon outlines and rectangle borders become strips too.
face_accumulator_3d <- function() {
  env <- new.env(parent = emptyenv())
  env$x <- list(); env$y <- list(); env$count <- list(); env$fill <- list(); env$alpha <- list(); env$order <- list()
  env$add <- function(x, y, count, fill, alpha, order) {
    k <- length(env$x) + 1L
    env$x[[k]] <- x; env$y[[k]] <- y; env$count[[k]] <- count
    env$fill[[k]] <- rep_len(fill, length(count)); env$alpha[[k]] <- rep_len(alpha, length(count))
    env$order[[k]] <- rep_len(order, length(count))
  }
  env$table <- function() {
    if (!length(env$x)) return(NULL)
    count <- unlist(env$count); fill <- unlist(env$fill); alpha <- unlist(env$alpha)
    rank <- order(unlist(env$order), seq_along(count), method = "radix")
    start <- cumsum(c(0L, count))
    rows <- unlist(lapply(rank, function(f) start[f] + seq_len(count[f])), use.names = FALSE)
    face <- rep(seq_along(rank), count[rank])
    list(data = data.frame(x = unlist(env$x)[rows], y = unlist(env$y)[rows], fill = fill[rank][face],
      colour = NA_character_, alpha = alpha[rank][face], stringsAsFactors = FALSE), counts = count[rank])
  }
  env
}

# Vertices of a polyline kept by Ramer-Douglas-Peucker simplification: the
# dropped vertices lie within `tolerance` of the simplified line. Curves
# sampled finely by ggplot2 (512 points for a density) keep the vertices that
# change their drawn shape by more than the tolerance.
simplify_path_3d <- function(x, y, tolerance) {
  n <- length(x)
  if (n <= 2L) return(seq_len(n))
  keep <- c(TRUE, rep(FALSE, n - 2L), TRUE)
  stack <- list(c(1L, n))
  while (length(stack)) {
    span <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
    if (span[2L] - span[1L] < 2L) next
    inner <- seq.int(span[1L] + 1L, span[2L] - 1L)
    dx <- x[span[2L]] - x[span[1L]]; dy <- y[span[2L]] - y[span[1L]]
    length <- sqrt(dx^2 + dy^2)
    distance <- if (length > 0) abs(dx * (y[inner] - y[span[1L]]) - dy * (x[inner] - x[span[1L]])) / length else
      sqrt((x[inner] - x[span[1L]])^2 + (y[inner] - y[span[1L]])^2)
    far <- which.max(distance)
    if (distance[far] <= tolerance) next
    split <- inner[far]
    keep[split] <- TRUE
    stack[[length(stack) + 1L]] <- c(span[1L], split)
    stack[[length(stack) + 1L]] <- c(split, span[2L])
  }
  which(keep)
}

# Distance below which picture outlines are simplified: 0.02 mm on the
# measuring panel.
plane_tolerance_3d <- function() 0.02 / (plane_panel_inches_3d * 25.4)

# Dash pattern of a ggplot2 linetype as alternating on and off lengths in
# units of the line width; NULL for solid lines, empty for blank ones.
linetype_dashes_3d <- function(linetype) {
  if (is.null(linetype) || is.na(linetype)) return(NULL)
  if (is.numeric(linetype)) {
    if (linetype <= 0) return(numeric())
    linetype <- c("solid", "44", "13", "1343", "73", "2262")[((linetype - 1) %% 6) + 1]
  }
  named <- c(blank = "blank", solid = "solid", dashed = "44", dotted = "13", dotdash = "1343", longdash = "73", twodash = "2262")
  if (linetype %in% names(named)) linetype <- named[[linetype]]
  if (identical(linetype, "solid")) return(NULL)
  if (identical(linetype, "blank")) return(numeric())
  as.numeric(strtoi(strsplit(linetype, "")[[1L]], 16L))
}

# Pieces of a polyline drawn by a dash pattern of alternating on and off
# lengths, starting on.
dash_pieces_3d <- function(x, y, pattern) {
  if (is.null(pattern)) return(list(list(x = x, y = y)))
  if (!length(pattern) || sum(pattern) <= 0) return(list())
  s <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  total <- s[length(s)]
  if (total <= 0) return(list())
  on_index <- seq(1L, length(pattern), by = 2L)
  starts <- cumsum(c(0, pattern))[on_index]
  ends <- starts + pattern[on_index]
  cycles <- seq(0, total, by = sum(pattern))
  on <- cbind(as.vector(outer(starts, cycles, `+`)), as.vector(outer(ends, cycles, `+`)))
  on <- on[on[, 1L] < total, , drop = FALSE]
  on[, 2L] <- pmin(on[, 2L], total)
  lapply(seq_len(nrow(on)), function(i) {
    at <- c(on[i, 1L], s[s > on[i, 1L] & s < on[i, 2L]], on[i, 2L])
    list(x = stats::approx(s, x, at, ties = "ordered")$y, y = stats::approx(s, y, at, ties = "ordered")$y)
  })
}

# Convex faces covering a polyline of half width `half`: a rectangle per
# segment and a bevel triangle on the outer side of each joint that turns.
stroke_quads_3d <- function(x, y, half, closed = FALSE) {
  if (closed) {
    x <- c(x, x[1L]); y <- c(y, y[1L])
  }
  keep <- c(TRUE, sqrt(diff(x)^2 + diff(y)^2) > 1e-9)
  x <- x[keep]; y <- y[keep]
  n <- length(x)
  if (n < 2L || half <= 0) return(NULL)
  dx <- diff(x); dy <- diff(y); length <- sqrt(dx^2 + dy^2)
  nx <- -dy / length * half; ny <- dx / length * half
  a <- seq_len(n - 1L); b <- a + 1L
  out_x <- as.vector(rbind(x[a] + nx, x[b] + nx, x[b] - nx, x[a] - nx))
  out_y <- as.vector(rbind(y[a] + ny, y[b] + ny, y[b] - ny, y[a] - ny))
  count <- rep.int(4L, n - 1L)
  joints <- if (n > 2L) seq_len(n - 2L) else integer()
  if (closed && n > 2L) joints <- c(joints, n - 1L)
  if (length(joints)) {
    following <- ifelse(joints == n - 1L, 1L, joints + 1L)
    turn <- (dx[joints] * dy[following] - dy[joints] * dx[following]) / (length[joints] * length[following])
    side <- -sign(turn)
    corner <- b[joints]
    # Below a turn of about three degrees the gap at the joint is under a
    # twentieth of the line width.
    bevel <- abs(turn) > 0.05
    corner <- corner[bevel]; side <- side[bevel]; joints <- joints[bevel]; following <- following[bevel]
    out_x <- c(out_x, as.vector(rbind(x[corner], x[corner] + side * nx[joints], x[corner] + side * nx[following])))
    out_y <- c(out_y, as.vector(rbind(y[corner], y[corner] + side * ny[joints], y[corner] + side * ny[following])))
    count <- c(count, rep.int(3L, length(corner)))
  }
  list(x = out_x, y = out_y, count = count)
}

add_stroke_3d <- function(faces, x, y, colour, linewidth, linetype, alpha, order, closed = FALSE) {
  if (is.na(colour) || !is.finite(linewidth) || linewidth <= 0) return(invisible())
  width <- paint_metrics_3d(data.frame(linewidth = linewidth))$width_mm / (plane_panel_inches_3d * 25.4)
  pattern <- linetype_dashes_3d(linetype)
  if (!is.null(pattern)) {
    if (closed) {
      x <- c(x, x[1L]); y <- c(y, y[1L]); closed <- FALSE
    }
    pattern <- pattern * width
  }
  kept <- simplify_path_3d(x, y, plane_tolerance_3d())
  x <- x[kept]; y <- y[kept]
  for (piece in dash_pieces_3d(x, y, pattern)) {
    quads <- stroke_quads_3d(piece$x, piece$y, width / 2, closed)
    if (!is.null(quads)) faces$add(quads$x, quads$y, quads$count, colour, alpha, order)
  }
  invisible()
}

# Point glyphs: the body outline of the point shape (marker_body_3d(), a
# circle for round shapes) at the marker's size, a band around it for the
# outline of hollow and bordered shapes, and a bar per arm of the plus, cross
# and asterisk shapes.
add_glyphs_3d <- function(faces, points) {
  if (is.character(points$shape)) {
    identity_coord <- ggplot2::ggproto(NULL, ggplot2::Coord, transform = function(data, panel_params) data)
    points$shape <- ggplot2::GeomPoint$draw_panel(points, list(), identity_coord)$pch
  }
  metrics <- paint_metrics_3d(points)
  scale <- 1 / (plane_panel_inches_3d * 25.4)
  angle <- seq(0, 2 * pi, length.out = 25L)[-25L]
  circle <- cbind(cos(angle), sin(angle))
  base <- c(`7` = 0, `9` = 5, `10` = 1, `11` = 2, `12` = 0, `13` = 1, `14` = 0)
  for (i in seq_len(nrow(points))) {
    shape <- points$shape[i]
    if (is.na(shape)) next
    if (as.character(shape) %in% names(base)) shape <- base[[as.character(shape)]]
    r <- metrics$body_radius_mm[i]
    half <- metrics$point_lwd[i] * 25.4 / 192
    colour <- points$colour[i]
    add <- function(xy, fill) {
      if (!is.na(fill)) faces$add(points$x[i] + xy[, 1L] * scale, points$y[i] + xy[, 2L] * scale, nrow(xy),
        fill, points$alpha[i], points$.order[i])
    }
    band <- function(xy, inner, outer) {
      k <- nrow(xy); j <- c(seq.int(2L, k), 1L)
      for (e in seq_len(k)) add(rbind(xy[e, ] * inner, xy[j[e], ] * inner, xy[j[e], ] * outer, xy[e, ] * outer), colour)
    }
    if (shape %in% c(3, 4, 8)) {
      directions <- switch(as.character(shape), `3` = c(0, pi / 2), `4` = c(pi / 4, 3 * pi / 4), `8` = c(0, pi / 4, pi / 2, 3 * pi / 4))
      for (theta in directions) {
        along <- c(cos(theta), sin(theta)) * r
        across <- c(-sin(theta), cos(theta)) * half
        add(rbind(-along - across, along - across, along + across, -along + across), colour)
      }
      next
    }
    unit <- marker_body_3d(shape, 1) %or3d% circle
    if (shape %in% 15:20) {
      add(unit * r, colour)
    } else if (shape %in% 21:25) {
      add(unit * r, points$fill[i])
      if (half > 0) band(unit, r, r + 2 * half)
    } else if (half >= r) {
      add(unit * (r + half), colour)
    } else if (half > 0) {
      band(unit, r - half, r + half)
    }
  }
  invisible()
}

flat_faces_3d <- function(primitives) {
  faces <- face_accumulator_3d()
  rects <- primitives$rects
  for (i in seq_len(NROW(rects))) {
    x <- c(rects$xmin[i], rects$xmax[i], rects$xmax[i], rects$xmin[i])
    y <- c(rects$ymin[i], rects$ymin[i], rects$ymax[i], rects$ymax[i])
    if (!is.na(rects$fill[i]) && rects$xmax[i] > rects$xmin[i] && rects$ymax[i] > rects$ymin[i]) {
      faces$add(x, y, 4L, rects$fill[i], rects$alpha[i], rects$.order[i])
    }
    add_stroke_3d(faces, x, y, rects$colour[i], rects$linewidth[i], rects$linetype[i], rects$alpha[i], rects$.order[i] + 0.5, closed = TRUE)
  }
  polygons <- primitives$polygons
  for (group in unique(polygons$group)) {
    feature <- polygons[polygons$group == group, , drop = FALSE]
    feature$z <- 0
    rings <- lapply(split(seq_len(nrow(feature)), feature$subgroup), function(ring) {
      ring[simplify_path_3d(feature$x[ring], feature$y[ring], plane_tolerance_3d())]
    })
    if (!is.na(feature$fill[1L])) {
      triangles <- triangulate_polygon_3d(feature, rings, strict = FALSE)$triangles
      if (length(triangles)) {
        index <- unlist(triangles, use.names = FALSE)
        faces$add(feature$x[index], feature$y[index], rep.int(3L, length(triangles)), feature$fill[1L], feature$alpha[1L], feature$.order[1L])
      }
    }
    for (ring in rings) {
      add_stroke_3d(faces, feature$x[ring], feature$y[ring], feature$colour[1L], feature$linewidth[1L],
        feature$linetype[1L], feature$alpha[1L], feature$.order[1L] + 0.5, closed = TRUE)
    }
  }
  paths <- primitives$paths
  for (group in unique(paths$group)) {
    rows <- which(paths$group == group)
    add_stroke_3d(faces, paths$x[rows], paths$y[rows], paths$colour[rows[1L]], paths$linewidth[rows[1L]],
      paths$linetype[rows[1L]], paths$alpha[rows[1L]], paths$.order[rows[1L]])
  }
  if (!is.null(primitives$points)) add_glyphs_3d(faces, primitives$points)
  faces$table()
}

# Emits primitive tables of the (u, v) unit square on the rectangle
# u0..u1 x v0..v1 of the horizontal plane at w, in normalised panel
# coordinates, then turns that plane onto `axes`. Marks lie in the plane as
# faces (flat_faces_3d()).
emit_plane_primitives_3d <- function(primitives, u, v, w, axes, data, params, panel_params, alpha = 1) {
  located <- function(table) {
    for (column in intersect(c("x", "xmin", "xmax"), names(table))) table[[column]] <- u[1L] + table[[column]] * (u[2L] - u[1L])
    for (column in intersect(c("y", "ymin", "ymax"), names(table))) table[[column]] <- v[1L] + table[[column]] * (v[2L] - v[1L])
    table$z <- rep(w, nrow(table))
    table$.source <- rep(data$.source[1L], nrow(table))
    table$.layer <- rep(data$.layer[1L], nrow(table))
    table$alpha <- table$alpha * alpha
    table
  }
  params$size.unit <- "mm"
  params$parse <- FALSE
  packets <- list()
  faces <- flat_faces_3d(primitives)
  if (!is.null(faces)) {
    packets[[1L]] <- packet_from_primitives_3d(located(faces$data), seq_len(nrow(faces$data)),
      rep("faces", length(faces$counts)), params, panel_params$panel_id, faces$counts)
  }
  if (!is.null(primitives$text)) {
    # Text faces the viewer, so it does not lie in the plane; it is drawn over
    # the scene rather than cut by the faces it sits on.
    params$occlusion <- "overlay"
    packets[[length(packets) + 1L]] <- emit_text_3d(located(primitives$text), params, panel_params)
  }
  orient_packet_3d(merge_scene_packets(packets, panel_params$panel_id), axes)
}

# Draw parameters of a ggplot2 geom with their defaults, from the formals of
# its draw_panel and draw_group methods as ggplot2's Geom$parameters() reads them.
geom_draw_parameters_3d <- function(geom) {
  formal <- function(method) formals(environment(method)$f)
  panel <- formal(geom$draw_panel)
  group <- formal(geom$draw_group)
  all <- c(panel, group[setdiff(names(group), names(panel))])
  names <- setdiff(geom$parameters(TRUE), c("data", "panel_params", "coord", "...", "self", names(scene_draw_formals_3d())))
  defaults <- lapply(names, function(name) {
    value <- all[[name]]
    if (is.null(value) || (is.symbol(value) && !nzchar(as.character(value)))) return(NULL)
    tryCatch(eval(value, envir = asNamespace("ggplot2")), error = function(e) NULL)
  })
  names(defaults) <- names
  defaults
}

# Emitter for a ggplot2 geom that draws in two dimensions. ggplot2 draws the
# geom in the x/y panel of each horizontal plane the layer occupies, and the
# drawing is placed on that plane.
planar_emitter_3d <- function(geom) {
  force(geom)
  function(data, params, panel_params) {
    data <- data[valid_rows_3d(data, params, "z"), , drop = FALSE]
    if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
    spread <- tapply(data$z, data$group, function(z) diff(range(z)))
    if (any(spread > 1e-9)) stop(class(geom)[1L], " draws in a horizontal plane; its z must be constant within each group.", call. = FALSE)
    for (axis in c("x", "y")) {
      range <- panel_params$ranges[[axis]]
      for (column in intersect(panel_params[[axis]]$aesthetics, names(data))) {
        if (is.numeric(data[[column]])) data[[column]] <- range[1L] + data[[column]] * diff(range)
      }
    }
    coord <- ggplot2::coord_cartesian(xlim = panel_params$ranges$x, ylim = panel_params$ranges$y, expand = FALSE)
    view <- coord$setup_panel_params(ggplot2::scale_x_continuous(), ggplot2::scale_y_continuous(), coord$setup_params(list()))
    geom_params <- params[intersect(names(params), geom$parameters())]
    packets <- lapply(unique(data$z), function(w) {
      rows <- data[data$z == w, , drop = FALSE]
      grob <- do.call(geom$draw_panel, c(list(rows, view, coord), geom_params))
      emit_plane_primitives_3d(grob_primitives_3d(grob), c(0, 1), c(0, 1), w, c("x", "y", "z"), rows, params, panel_params)
    })
    merge_scene_packets(packets, panel_params$panel_id)
  }
}

for (planar_geom in lapply(c("GeomBoxplot", "GeomDotplot", "GeomHex",
    "GeomRaster", "GeomRibbon", "GeomRug", "GeomSmooth", "GeomViolin"),
    getExportedValue, ns = "ggplot2")) {
  register_geom_3d(planar_geom, planar_emitter_3d(planar_geom), parameters = geom_draw_parameters_3d(planar_geom),
    positions = c("nudge", "planar"))
}
rm(planar_geom)

GeomPlotPlane3D <- ggplot2::ggproto("GeomPlotPlane3D", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank
)

emit_plot_plane_3d <- function(data, params, panel_params) {
  axes <- params$axes
  if (nrow(data) < 2L) return(new_scene_packet(panel_id = panel_params$panel_id))
  emit_plane_primitives_3d(params$primitives, data[[axes[1L]]][1:2], data[[axes[2L]]][1:2], data[[axes[3L]]][1L],
    axes, data, params, panel_params, params$plot_alpha)
}

register_geom_3d(GeomPlotPlane3D, emit_plot_plane_3d,
  parameters = list(primitives = NULL, axes = c("x", "z", "y"), plot_alpha = 1))

StatPlotPlane3D <- ggplot2::ggproto("StatPlotPlane3D", ggplot2::Stat,
  dropped_aes = ".grid",
  compute_panel = function(data, scales, axes, at, extent) {
    out <- data.frame(extent[1:2], extent[3:4], plane_at_3d(at, scales, axes[3L]))
    names(out) <- axes
    out$PANEL <- data$PANEL[1L]
    out$group <- -1L
    out
  }
)

# The panel of a built 2D plot: its grob and its x and y ranges.
plot_panel_3d <- function(plot) {
  if (!ggplot2::is_ggplot(plot)) stop("plot must be a ggplot.", call. = FALSE)
  if (inherits(plot, "ggplot3d")) stop("plot must be a two-dimensional ggplot, without coord_3d().", call. = FALSE)
  built <- ggplot2::ggplot_build(plot)
  if (nrow(built$layout$layout) != 1L) stop("plot must have a single panel.", call. = FALSE)
  table <- ggplot2::ggplot_gtable(built)
  panel <- table$grobs[grepl("^panel", table$layout$name)]
  if (length(panel) != 1L) stop("plot must have a single panel.", call. = FALSE)
  params <- built$layout$panel_params[[1L]]
  list(grob = panel[[1L]], x = params$x.range, y = params$y.range)
}

render_panel_3d <- function(grob, resolution) {
  if (!requireNamespace("ragg", quietly = TRUE)) stop("route = \"raster\" requires the ragg package.", call. = FALSE)
  previous <- grDevices::dev.cur()
  capture <- ragg::agg_capture(width = resolution, height = resolution, units = "px",
    res = resolution / plane_panel_inches_3d, background = "transparent")
  device <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(device)
    if (previous > 1L) grDevices::dev.set(previous)
  }, add = TRUE)
  grid::grid.draw(grob)
  capture(native = FALSE)
}

#' A two-dimensional plot on a plane of the panel box
#'
#' Draws the panel of a whole two-dimensional ggplot, every layer with its
#' panel background and grid lines, on a plane of the 3D panel, such as a
#' wall behind the data. The plot's x runs along the first axis of `plane`
#' and its y along the second; the remaining axis is fixed at `at`, where
#' `Inf` and `-Inf` place the plot on the box sides at the upper and lower end
#' of that axis (0 when no other layer trains that axis). By default the plot
#' keeps its own coordinates: its panel covers its x and y ranges on the two
#' plane axes, so its data line up with the 3D layers that share those axes.
#' `extent` places it elsewhere.
#'
#' The vector route decomposes the drawn panel into points, lines, polygons,
#' rectangles and text that join the 3D scene and its exact visibility, as
#' matplotlib places 2D artists on a plane of 3D axes and ggcube projects
#' layers onto the faces of its cube. Marks lie in the plane, foreshortened
#' with it, at the size they have on a 2D panel 4 inches wide; text faces the
#' viewer and is drawn over the scene. The raster route renders the panel
#' to an image of `resolution` pixels a side, as rayshader does for a whole
#' ggplot, and lays its pixels on the plane as rectangles, joining runs of
#' equal colour; it suits dense plots whose vector form would be large.
#' @param plot A two-dimensional ggplot with a single panel.
#' @param plane Two distinct axis letters: the axis carrying the plot's x
#'   then the axis carrying its y, for example `"xz"`, `"yz"` or `"xy"`.
#' @param at Position on the remaining axis.
#' @param extent `NULL`, or four numbers `c(min, max, min, max)`: the
#'   positions of the plot's panel along the two plane axes. A reversed pair
#'   mirrors the plot.
#' @param route `"vector"` or `"raster"` (which requires ragg).
#' @param resolution Pixels along each side of the raster image.
#' @param alpha Opacity multiplying the plot's own.
#' @return A ggplot2 layer.
#' @references Hunter (2007). Matplotlib: a 2D graphics environment.
#'   Computing in Science & Engineering 9, 90-95.
#'   \doi{10.1109/MCSE.2007.55}
#' @seealso [facet_slices_3d()] for small multiples on parallel planes.
#' @examples
#' d <- data.frame(x = rnorm(200), y = rnorm(200))
#' d$z <- d$x * d$y + rnorm(200, sd = 0.3)
#' side <- ggplot2::ggplot(d, ggplot2::aes(x, z)) +
#'   ggplot2::geom_point(colour = "grey40", size = 0.8) +
#'   ggplot2::geom_smooth(method = "lm", formula = y ~ x)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   annotation_plot_3d(side, plane = "xz", at = Inf) +
#'   geom_point_3d(colour = "steelblue") +
#'   coord_3d()
#' @export
annotation_plot_3d <- function(plot, plane = "xz", at = Inf, extent = NULL, route = c("vector", "raster"),
                               resolution = 150, alpha = 1) {
  axes <- check_plane_axes_3d(plane)
  route <- match.arg(route)
  if (!is.numeric(at) || length(at) != 1L || is.na(at)) stop("at must be a number.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("alpha must be a number between 0 and 1.", call. = FALSE)
  if (!is.numeric(resolution) || length(resolution) != 1L || !is.finite(resolution) || resolution < 2) stop("resolution must be a number of pixels of at least 2.", call. = FALSE)
  panel <- plot_panel_3d(plot)
  if (is.null(extent)) {
    if (length(panel$x) != 2L || length(panel$y) != 2L) stop("The plot's coordinate system has no x and y ranges; supply extent.", call. = FALSE)
    extent <- c(panel$x, panel$y)
  }
  if (!is.numeric(extent) || length(extent) != 4L || any(!is.finite(extent)) || extent[1L] == extent[2L] || extent[3L] == extent[4L]) {
    stop("extent must be four finite numbers c(min, max, min, max) with distinct ends.", call. = FALSE)
  }
  primitives <- if (route == "vector") grob_primitives_3d(panel$grob) else
    list(rects = raster_cells_3d(render_panel_3d(panel$grob, round(resolution))))
  make_layer_3d(GeomPlotPlane3D, NULL, data.frame(.grid = 1), StatPlotPlane3D, "identity",
    list(axes = axes, at = at, extent = as.double(extent), primitives = primitives, plot_alpha = alpha),
    FALSE, FALSE)
}
