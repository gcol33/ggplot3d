#' Interpolate built layer data between animation states
#'
#' Adds `n` in-between frames to each adjacent pair of states. Statistics run
#' on each state before interpolation. Positions interpolate in transformed
#' scale units; colours interpolate in linear light. The scales are trained
#' on all states. Use an explicit group aesthetic for persistent identities.
#' @param n Number of in-between frames per adjacent state pair.
#' @param ease An easing name accepted by [camera_path_3d()], or a function
#'   taking and returning numeric fractions between zero and one.
#' @param match `"group"` matches marks by their group values and then by
#'   occurrence within a group, as in gganimate's `transition_states()`.
#'   `".source"` matches the source row numbers within each state.
#' @param enter,exit `"fade"` fades unmatched geometry, `"keep"` shows it
#'   throughout the transition, or a function returning an altered data frame
#'   with the same rows and columns. An enter function supplies starting
#'   attributes; an exit function supplies ending attributes.
#' @details Paths and polygon rings are resampled by three-dimensional arc
#'   length when their vertex counts differ. Polygon reading order is aligned
#'   by ring winding and the nearest start vertex. Fixed-topology surfaces
#'   and meshes match their face/corner keys; changes of mesh connectivity
#'   cross-fade the two shapes. Rotations use quaternion interpolation.
#' @return A tween specification for [animate_3d()].
#' @references gganimate, `transition_states()`; transformr, `tween_polygon()`.
#' @examples
#' d <- data.frame(x = c(0, 1, 1, 2), y = c(0, 1, 1, 0),
#'                 z = c(0, 1, 2, 0), id = c("a", "b", "a", "b"), t = c(0, 0, 1, 1))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id)) +
#'   geom_point_3d() + coord_3d()
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   animation <- animate_3d(p, time = "t", tween = tween_3d(n = 2),
#'     width = 320, height = 320, progress = FALSE)
#'   unlink(animation$frames)
#' }
#' @export
tween_3d <- function(n = 10L, ease = "linear", match = "group", enter = "fade", exit = "fade") {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n > .Machine$integer.max - 2 || n != trunc(n)) stop("n must be a nonnegative representable integer.", call. = FALSE)
  easing <- if (is.function(ease)) ease else ease_function_3d(ease)
  ends <- easing(c(0, 1))
  if (!is.numeric(ends) || length(ends) != 2L || any(!is.finite(ends)) || any(abs(ends - c(0, 1)) > 1e-12)) stop("ease must preserve zero and one.", call. = FALSE)
  for (transition in list(enter, exit)) {
    if (!is.function(transition) && !(is.character(transition) && length(transition) == 1L && transition %in% c("fade", "keep"))) {
      stop("enter and exit must be 'fade', 'keep' or functions of a data frame.", call. = FALSE)
    }
  }
  structure(list(n = as.integer(n), ease = easing, match = match.arg(match, c("group", ".source")),
    enter = enter, exit = exit), class = "ggplot3d_tween")
}

tween_keys_3d <- function(data, columns) {
  key <- rep("K", nrow(data))
  for (column in columns) {
    value <- as.character(data[[column]])
    value <- ifelse(is.na(value), "N", paste0("V", nchar(value, type = "bytes"), ":", value))
    key <- paste0(key, value)
  }
  key
}

tween_plot_3d <- function(plot) {
  if (".tween_group" %in% names(plot@mapping)) stop(".tween_group is reserved for tweening.", call. = FALSE)
  coord <- plot@coordinates
  plot@coordinates <- ggplot2::ggproto(NULL, coord)
  plot@coordinates$config$tween <- TRUE
  plot@layers <- lapply(plot@layers, function(layer) {
    if (".tween_group" %in% names(layer$mapping)) stop(".tween_group is reserved for tweening.", call. = FALSE)
    group <- layer$mapping$group
    if (is.null(group) && isTRUE(layer$inherit.aes)) group <- plot@mapping$group
    if (is.null(group)) return(layer)
    parent <- layer
    layer <- ggplot2::ggproto(NULL, parent)
    layer$mapping$.tween_group <- group
    layer
  })
  plot
}

tween_schedule_3d <- function(states, tween) {
  n <- length(states)
  if (n == 1L) return(data.frame(from = 1L, to = 1L, fraction = 0))
  from <- rep(seq_len(n - 1L), each = tween$n + 1L)
  fraction <- rep(seq(0, 1, length.out = tween$n + 2L)[-(tween$n + 2L)], n - 1L)
  eased <- tween$ease(fraction)
  if (!is.numeric(eased) || length(eased) != length(fraction) || any(!is.finite(eased))) stop("ease must return one finite fraction per input.", call. = FALSE)
  if (any(eased < 0 | eased > 1)) stop("Data tween easing must stay between zero and one.", call. = FALSE)
  rbind(data.frame(from = from, to = from + 1L, fraction = eased), data.frame(from = n, to = n, fraction = 0))
}

tween_colours_3d <- function(a, b, t) {
  aa <- t(colour_channels_3d(a, alpha = TRUE)) / 255
  bb <- t(colour_channels_3d(b, alpha = TRUE)) / 255
  rgb <- srgb_encode_3d(srgb_decode_3d(aa[, 1:3, drop = FALSE]) * (1 - t) + srgb_decode_3d(bb[, 1:3, drop = FALSE]) * t)
  grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L], aa[, 4L] * (1 - t) + bb[, 4L] * t)
}

tween_rows_3d <- function(a, b, t) {
  stopifnot(nrow(a) == nrow(b))
  if (!nrow(a)) return(a)
  out <- a
  discrete <- c("PANEL", "group", ".source", ".layer", ".tween_group", ".face", ".corner", ".vertex", ".mesh", "subgroup", "shape", "fontface", "linetype")
  colours <- c("colour", "fill", "border.colour", "border_colour", "segment.colour")
  for (name in intersect(names(a), names(b))) {
    x <- a[[name]]; y <- b[[name]]
    if (name %in% discrete) next
    if (name %in% colours && is.character(x) && is.character(y)) out[[name]] <- tween_colours_3d(x, y, t)
    else if (is.numeric(x) && is.numeric(y)) {
      good <- is.finite(x) & is.finite(y)
      value <- x
      choose <- rep_len(t, length(x)) >= .5
      value[choose] <- y[choose]
      value[good] <- x[good] * (1 - rep_len(t, length(x))[good]) + y[good] * rep_len(t, length(x))[good]
      out[[name]] <- value
    } else if (is.list(x) && is.list(y)) {
      out[[name]] <- lapply(seq_along(x), function(i) {
        u <- x[[i]]; v <- y[[i]]; fraction <- rep_len(t, length(x))[i]
        if (is.numeric(u) && is.numeric(v) && identical(dim(u), dim(v)) && length(u) == length(v)) {
          if (name == "rotation" && identical(dim(u), c(3L, 3L))) {
            return(quaternion_matrix_3d(slerp_3d(quaternion_from_matrix_3d(u), quaternion_from_matrix_3d(v), fraction)))
          }
          return(u * (1 - fraction) + v * fraction)
        }
        if (fraction < .5) u else v
      })
    } else {
      if (is.factor(x) && is.factor(y)) {
        x <- factor(as.character(x), levels = union(levels(x), levels(y)), ordered = is.ordered(x) && is.ordered(y))
        y <- as.character(y)
      }
      choose <- rep_len(t, length(x)) >= .5
      x[choose] <- y[choose]
      out[[name]] <- x
    }
  }
  out
}

tween_fade_3d <- function(data, alpha) {
  data$alpha <- (ifelse(is.na(data$alpha %or3d% rep(1, nrow(data))), 1, data$alpha %or3d% rep(1, nrow(data)))) * alpha
  data
}

tween_unmatched_3d <- function(data, t, transition, entering) {
  if (is.function(transition)) {
    changed <- transition(data)
    if (!is.data.frame(changed) || nrow(changed) != nrow(data) || !identical(names(changed), names(data))) stop("A tween enter/exit function must preserve rows and columns.", call. = FALSE)
    if (entering) tween_rows_3d(changed, data, t) else tween_rows_3d(data, changed, t)
  } else if (transition == "fade") tween_fade_3d(data, if (entering) t else 1 - t) else data
}

tween_ring_normal_3d <- function(xyz) {
  next_row <- xyz[c(seq.int(2L, nrow(xyz)), 1L), , drop = FALSE]
  colSums(cross_rows_3d(xyz, next_row))
}

tween_resample_3d <- function(data, n, closed) {
  if (nrow(data) == 1L) return(data[rep(1L, n), , drop = FALSE])
  columns <- intersect(c("x", "y", "z"), names(data))
  if (closed) data <- rbind(data, data[1L, , drop = FALSE])
  xyz <- as.matrix(data[columns])
  distance <- c(0, cumsum(sqrt(rowSums((xyz[-1L, , drop = FALSE] - xyz[-nrow(xyz), , drop = FALSE])^2))))
  keep <- !duplicated(distance)
  data <- data[keep, , drop = FALSE]; distance <- distance[keep]
  if (length(distance) == 1L) return(data[rep(1L, n), , drop = FALSE])
  at <- seq(0, tail(distance, 1L), length.out = n + as.integer(closed))
  if (closed) at <- head(at, -1L)
  i <- pmin(findInterval(at, distance, all.inside = TRUE), length(distance) - 1L)
  fraction <- (at - distance[i]) / (distance[i + 1L] - distance[i])
  result <- tween_rows_3d(data[i, , drop = FALSE], data[i + 1L, , drop = FALSE], fraction)
  result$.source <- seq_len(n)
  result
}

tween_paths_3d <- function(a, b, t, closed) {
  columns <- intersect(c("x", "y", "z"), names(a))
  if (closed && nrow(a) > 2L && nrow(b) > 2L) {
    xyz <- function(d) {
      x <- matrix(0, nrow(d), 3L); x[, match(columns, c("x", "y", "z"))] <- as.matrix(d[columns]); x
    }
    if (sum(tween_ring_normal_3d(xyz(a)) * tween_ring_normal_3d(xyz(b))) < 0) b <- b[nrow(b):1L, , drop = FALSE]
    distance <- rowSums(sweep(as.matrix(b[columns]), 2L, as.numeric(a[1L, columns]), "-")^2)
    start <- which.min(distance)
    b <- b[((seq_len(nrow(b)) + start - 2L) %% nrow(b)) + 1L, , drop = FALSE]
  }
  if (nrow(a) != nrow(b)) {
    n <- max(nrow(a), nrow(b))
    a <- tween_resample_3d(a, n, closed); b <- tween_resample_3d(b, n, closed)
  }
  tween_rows_3d(a, b, t)
}

tween_join_rows_3d <- function(a, b, ka, kb, t, spec) {
  paired <- match(ka, kb)
  both <- which(!is.na(paired))
  out <- list(tween_rows_3d(a[both, , drop = FALSE], b[paired[both], , drop = FALSE], t))
  if (anyNA(paired)) out[[length(out) + 1L]] <- tween_unmatched_3d(a[is.na(paired), , drop = FALSE], t, spec$exit, FALSE)
  entering <- which(!seq_len(nrow(b)) %in% paired)
  if (length(entering)) out[[length(out) + 1L]] <- tween_unmatched_3d(b[entering, , drop = FALSE], t, spec$enter, TRUE)
  out
}

tween_group_pair_3d <- function(a, b, t, type, spec) {
  if (!nrow(a)) return(list(tween_unmatched_3d(b, t, spec$enter, TRUE)))
  if (!nrow(b)) return(list(tween_unmatched_3d(a, t, spec$exit, FALSE)))
  if (type %in% c("path", "polygon")) {
    columns <- intersect(c("x", "y", "z"), names(a))
    if (any(!is.finite(as.matrix(a[columns]))) || any(!is.finite(as.matrix(b[columns])))) {
      return(list(tween_fade_3d(a, 1 - t), tween_fade_3d(b, t)))
    }
    if (type == "polygon" && "subgroup" %in% names(a)) {
      rings <- unique(a$subgroup)
      if (!setequal(rings, unique(b$subgroup))) return(list(tween_fade_3d(a, 1 - t), tween_fade_3d(b, t)))
      pieces <- lapply(rings, function(ring) tween_paths_3d(a[a$subgroup == ring, , drop = FALSE],
        b[b$subgroup == ring, , drop = FALSE], t, TRUE))
      return(list(do.call(rbind, pieces)))
    }
    return(list(tween_paths_3d(a, b, t, type == "polygon")))
  }
  keys <- intersect(c(".mesh", ".face", ".corner"), names(a))
  if (type == "row" || !length(keys)) {
    if (spec$match == ".source") {
      ka <- as.character(a$.source %or3d% seq_len(nrow(a)))
      kb <- as.character(b$.source %or3d% seq_len(nrow(b)))
    } else {
      ka <- as.character(seq_len(nrow(a))); kb <- as.character(seq_len(nrow(b)))
    }
  } else {
    ka <- tween_keys_3d(a, keys); kb <- tween_keys_3d(b, keys)
  }
  paired <- match(ka, kb)
  same <- !anyNA(paired) && length(ka) == length(kb) && !anyDuplicated(ka) && !anyDuplicated(kb)
  if (type == "group" && (!same || (".vertex" %in% names(a) && !identical(a$.vertex, b$.vertex[paired])))) {
    return(list(tween_fade_3d(a, 1 - t), tween_fade_3d(b, t)))
  }
  tween_join_rows_3d(a, b, ka, kb, t, spec)
}

tween_layer_3d <- function(a, b, t, entry, spec) {
  if (t == 0 || identical(a, b)) return(a)
  if (t == 1) return(b)
  for (name in setdiff(names(b), names(a))) a[[name]] <- b[[name]][rep(NA_integer_, nrow(a))]
  for (name in setdiff(names(a), names(b))) b[[name]] <- a[[name]][rep(NA_integer_, nrow(b))]
  if (is.null(a$alpha)) a$alpha <- rep(1, nrow(a))
  if (is.null(b$alpha)) b$alpha <- rep(1, nrow(b))
  type <- entry$tween %or3d% "group"
  groups <- intersect(c("PANEL", if (spec$match == "group") ".tween_group",
    if (type != "polygon") "subgroup", "level"), union(names(a), names(b)))
  key <- function(d) {
    if (!nrow(d)) return(character())
    for (name in setdiff(groups, names(d))) d[[name]] <- rep(NA_character_, nrow(d))
    base <- tween_keys_3d(d, groups)
    if (type %in% c("path", "polygon") && !is.null(d$group)) {
      component <- integer(nrow(d))
      for (rows in split(seq_len(nrow(d)), base)) component[rows] <- match(d$group[rows], sort(unique(d$group[rows])))
      base <- paste0(base, "G", component)
    }
    base
  }
  ka <- key(a); kb <- key(b)
  ga <- split(seq_len(nrow(a)), ka); gb <- split(seq_len(nrow(b)), kb)
  ids <- union(unique(ka), unique(kb))
  if (type == "row") {
    row_keys <- function(data, keys, grouped) {
      if (!nrow(data)) return(character())
      if (spec$match == ".source") return(paste0(keys, "R", data$.source %or3d% seq_len(nrow(data))))
      occurrence <- integer(nrow(data))
      for (rows in grouped) occurrence[rows] <- seq_along(rows)
      paste0(keys, "R", occurrence)
    }
    pieces <- tween_join_rows_3d(a, b, row_keys(a, ka, ga), row_keys(b, kb, gb), t, spec)
    out <- do.call(rbind, Filter(function(d) nrow(d) > 0L, pieces))
    if (is.null(out)) return(a[FALSE, , drop = FALSE])
    out$group <- as.integer(match(tween_keys_3d(out, groups), ids))
    return(out)
  }
  out <- vector("list", 3L * length(ids)); cursor <- 0L; group_id <- 0L; mesh_id <- 0L
  for (id in ids) {
    pieces <- tween_group_pair_3d(a[ga[[id]] %or3d% integer(), , drop = FALSE],
      b[gb[[id]] %or3d% integer(), , drop = FALSE], t, type, spec)
    for (piece in pieces) {
      if (!nrow(piece)) next
      group_id <- group_id + 1L
      piece$group <- rep.int(group_id, nrow(piece))
      if (".mesh" %in% names(piece)) {
        local <- match(piece$.mesh, unique(piece$.mesh)); piece$.mesh <- local + mesh_id
        mesh_id <- mesh_id + max(local)
      }
      cursor <- cursor + 1L
      out[[cursor]] <- piece
    }
  }
  if (cursor) do.call(rbind, out[seq_len(cursor)]) else a[FALSE, , drop = FALSE]
}

motion_tween_build_3d <- function(keyframes, schedule, i, spec) {
  a <- keyframes[[schedule$from[i]]]; b <- keyframes[[schedule$to[i]]]
  t <- schedule$fraction[i]
  if (t == 0 || schedule$from[i] == schedule$to[i]) return(ggplot2::ggplot_gtable(a))
  a$data <- lapply(seq_along(a$data), function(layer) tween_layer_3d(a$data[[layer]], b$data[[layer]], t,
    find_emitter_3d(a$plot@layers[[layer]]$geom), spec))
  ggplot2::ggplot_gtable(a)
}
