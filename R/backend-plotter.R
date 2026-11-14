# Explicit hidden-stroke classification removes paint that vector surfaces
# cover later in painter order.
compile_plotter_scene_3d <- function(packet, camera, viewport, stable_partition = FALSE) {
  hidden <- packet$polylines$hidden
  packet$polylines$hidden[hidden == 0L] <- 2L
  scene <- scene_compile(packet, camera, viewport, stable_partition)
  f <- scene$fragments
  discard <- f$kind == 2L & f$hidden
  discard[discard] <- hidden[f$source_index[discard]] == 0L
  scene$draw_order <- scene$draw_order[!discard[scene$draw_order]]
  scene$source$polylines$hidden <- hidden
  scene$plotter_visible <- TRUE
  scene
}

# Consecutive vertices closer than a fraction of a device pixel carry one
# position. Closed rings also omit their repeated closing vertex.
compact_path_points_3d <- function(x, y, id, tolerance, closed = FALSE) {
  keep <- logical(length(x))
  for (indices in split(seq_along(x), id)) {
    last <- indices[1L]; keep[last] <- TRUE
    for (i in indices[-1L]) {
      if (hypot_3d(x[i] - x[last], y[i] - y[last]) >= tolerance) {
        keep[i] <- TRUE; last <- i
      }
    }
    retained <- indices[keep[indices]]
    if (closed && length(retained) > 1L &&
        hypot_3d(x[last] - x[retained[1L]], y[last] - y[retained[1L]]) < tolerance)
      keep[last] <- FALSE
    if (sum(keep[indices]) < if (closed) 3L else 2L) keep[indices] <- FALSE
  }
  which(keep)
}

hypot_3d <- function(x, y) sqrt(x * x + y * y)

# Joins touching intervals of one source polyline, preserving gaps and paint.
join_stroke_paths_3d <- function(paths, tolerance = 1e-6) {
  if (length(paths) < 2L) return(paths)
  endpoint <- function(p, end) {
    i <- if (end) length(p$x) else 1L
    paste(round(c(p$x[i], p$y[i]) / tolerance), collapse = ":")
  }
  starts <- vapply(paths, endpoint, character(1), FALSE)
  ends <- vapply(paths, endpoint, character(1), TRUE)
  used <- logical(length(paths)); out <- list()
  for (first in seq_along(paths)) {
    if (used[first]) next
    chain <- paths[[first]]; used[first] <- TRUE
    repeat {
      next_path <- which(!used & starts == endpoint(chain, TRUE))
      if (length(next_path) != 1L) break
      j <- next_path[1L]; used[j] <- TRUE
      chain$x <- c(chain$x, paths[[j]]$x[-1L]); chain$y <- c(chain$y, paths[[j]]$y[-1L])
    }
    repeat {
      previous <- which(!used & ends == endpoint(chain, FALSE))
      if (length(previous) != 1L) break
      j <- previous[1L]; used[j] <- TRUE
      chain$x <- c(head(paths[[j]]$x, -1L), chain$x); chain$y <- c(head(paths[[j]]$y, -1L), chain$y)
    }
    out[[length(out) + 1L]] <- chain
  }
  out
}

# Greedy nearest-endpoint ordering in physical units, reversing a path when
# its last endpoint is closer to the pen than its first endpoint.
sort_stroke_paths_3d <- function(paths, pen = c(0, 0)) {
  n <- length(paths)
  if (!n) return(paths)
  first <- t(vapply(paths, function(p) c(p$x[1L], p$y[1L]), numeric(2)))
  last <- t(vapply(paths, function(p) c(tail(p$x, 1L), tail(p$y, 1L)), numeric(2)))
  used <- logical(n); out <- vector("list", n)
  for (k in seq_len(n)) {
    distance <- c((first[, 1L] - pen[1L])^2 + (first[, 2L] - pen[2L])^2,
      (last[, 1L] - pen[1L])^2 + (last[, 2L] - pen[2L])^2)
    distance[rep(used, 2L)] <- Inf
    endpoint <- which.min(distance); j <- (endpoint - 1L) %% n + 1L
    p <- paths[[j]]
    if (endpoint > n) { p$x <- rev(p$x); p$y <- rev(p$y) }
    out[[k]] <- p; used[j] <- TRUE; pen <- c(tail(p$x, 1L), tail(p$y, 1L))
  }
  out
}

# Plotter strokes are collected by layer and colour, with labels closing the
# scene. Visibility is already expressed by the centreline intervals.
plotter_strokes_grob_3d <- function(grobs, runs, scene, viewport) {
  f <- scene$fragments; paths <- list(); labels <- list()
  tolerance <- min(0.01, 2.54 / viewport$dpi)
  for (run in seq_along(grobs)) {
    g <- grobs[[run]]; row <- runs[[run]][1L]
    if (f$kind[row] == 4L) { labels[[length(labels) + 1L]] <- g; next }
    polylines <- function(g) {
      if (inherits(g, "polyline")) return(list(g))
      unlist(lapply(as.list(g$children), polylines), recursive = FALSE)
    }
    for (g in polylines(g)) for (i in split(seq_along(g$x), g$id)) {
      path <- match(g$id[i[1L]], unique(g$id))
      gp <- g$gp
      for (field in names(gp)) if (length(gp[[field]]) > 1L) gp[[field]] <- gp[[field]][path]
      x <- as.numeric(g$x[i]) * if (grid::unitType(g$x)[1L] == "mm") 1 else viewport$width_mm
      y <- as.numeric(g$y[i]) * if (grid::unitType(g$y)[1L] == "mm") 1 else viewport$height_mm
      paths[[length(paths) + 1L]] <- list(x = x, y = y, gp = gp,
        layer = f$layer_id[row], source = f$source_index[row])
    }
  }
  if (length(paths)) {
    source <- vapply(paths, function(p) paste(p$source, p$layer, paste(unlist(p$gp), collapse = ":")), character(1))
    paths <- unlist(lapply(split(paths, factor(source, levels = unique(source))), join_stroke_paths_3d), recursive = FALSE)
    paths <- Filter(Negate(is.null), lapply(paths, function(p) {
      keep <- compact_path_points_3d(p$x, p$y, rep(1L, length(p$x)), tolerance)
      if (!length(keep)) return(NULL)
      p$x <- p$x[keep]; p$y <- p$y[keep]; p
    }))
    group <- vapply(paths, function(p) paste(p$layer, p$gp$col), character(1))
    paths <- unlist(lapply(split(paths, factor(group, levels = unique(group))), sort_stroke_paths_3d), recursive = FALSE)
  }
  strokes <- lapply(paths, function(p) grid::polylineGrob(p$x, p$y, default.units = "mm", gp = p$gp))
  grid::gTree(children = do.call(grid::gList, c(strokes, labels)), name = "scene-plotter-3d")
}
