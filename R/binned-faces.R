# Scalar samples retained before the flat material is averaged over a face.
face_fill_samples_3d <- function(data, face, shading, average = mesh_face_mean_3d(data$fill, face)) {
  if (is.numeric(data$fill) && identical(shading, "flat")) {
    data$.vertex_fill <- data$fill
    data$fill <- average
    data$.face_fill <- data$fill
  }
  data
}

band_palette_breaks_3d <- function(scale) {
  if (scale$is_discrete() || inherits(scale, "ScaleContinuousIdentity")) return(NULL)
  palette <- scale$palette
  if (inherits(palette, "ggproto_method")) palette <- environment(palette)$f
  if (inherits(palette, "pal_continuous") &&
      identical(body(palette), body(scales::pal_gradient_n(c("black", "white"))))) return(NULL)
  # Plateaus distinguish stepped palettes from ordinary gradient ramps.
  x <- seq(0, 1, length.out = 1025L)
  colour <- scale$palette(x)
  runs <- rle(colour)
  if (length(runs$lengths) < 2L || any(runs$lengths < 2L)) return(NULL)
  at <- head(cumsum(runs$lengths), -1L)
  lo <- x[at]; hi <- x[at + 1L]
  for (i in seq_len(48L)) {
    mid <- (lo + hi) / 2
    left <- scale$palette(mid) == colour[at]
    lo[left] <- mid[left]; hi[!left] <- mid[!left]
  }
  (lo + hi) / 2
}

# Scale mapping uses transformed units; face interpolation uses the scalar
# field's original units, with breaks taken through the inverse transformation.
band_scale_3d <- function(scale) {
  if (scale$is_discrete() || inherits(scale, "ScaleContinuousIdentity")) return(NULL)
  if (inherits(scale, "ScaleBinned")) {
    breaks <- scale$get_breaks()
    limits <- scale$get_limits()
    breaks <- sort(unique(c(limits, breaks[is.finite(breaks) & breaks > min(limits) & breaks < max(limits)])))
  } else {
    targets <- band_palette_breaks_3d(scale)
    if (is.null(targets)) return(NULL)
    limits <- scale$get_limits()
    # Rescalers may reverse or nonlinearly place palette coordinates.
    raw <- seq(min(limits), max(limits), length.out = 1025L)
    mapped <- scale$rescale(raw, limits)
    if (any(!is.finite(mapped)) || any(diff(mapped) == 0) ||
        !(all(diff(mapped) > 0) || all(diff(mapped) < 0))) return(NULL)
    low <- rep(min(limits), length(targets)); high <- rep(max(limits), length(targets))
    increasing <- mapped[1L] < tail(mapped, 1L)
    for (i in seq_len(48L)) {
      mid <- (low + high) / 2
      left <- (scale$rescale(mid, limits) < targets) == increasing
      low[left] <- mid[left]; high[!left] <- mid[!left]
    }
    breaks <- (low + high) / 2
    breaks <- sort(unique(breaks))
  }
  transformation <- scale$get_transformation()
  raw <- transformation$inverse(breaks)
  attributes <- attributes(raw); attributes$names <- NULL
  list(breaks = sort(unique(as.double(raw))), inverse = transformation$inverse,
       map = function(value) {
         attributes(value) <- attributes
         scale$map(transformation$transform(value))
       })
}

band_train_df_3d <- function(df, scales) {
  for (scale in scales) {
    samples <- df
    if ("fill" %in% scale$aesthetics && is.numeric(df$.vertex_fill) &&
        identical(df$fill, df$.face_fill) &&
        (inherits(scale, "ScaleBinned") || !is.null(band_palette_breaks_3d(scale))))
      samples$fill <- df$.vertex_fill
    scale$train_df(samples)
  }
  invisible()
}

band_map_df_3d <- function(df, scales, context) {
  bands <- list()
  for (scale in scales) {
    aesthetics <- intersect(c("fill", "colour"), intersect(scale$aesthetics, names(df)))
    if (!length(aesthetics)) next
    band <- band_scale_3d(scale)
    if (is.null(band)) next
    for (name in aesthetics) {
      value <- df[[name]]
      if (name == "fill" && identical(value, df$.face_fill)) value <- df$.vertex_fill
      if (!is.numeric(value)) next
      df[[paste0(".band_", name)]] <- as.double(band$inverse(value))
      bands[[name]] <- band
    }
  }
  context$key_context$band_scales[names(bands)] <- bands
  df
}

# Clip barycentric polygons by a scalar half-plane. Coordinates, normals and
# other numeric corner attributes use the same interpolation weights.
band_clip_weights_3d <- function(weights, value, level, above) {
  scalar <- drop(weights %*% value)
  inside <- if (above) scalar >= level else scalar <= level
  if (all(inside)) return(weights)
  if (!any(inside)) return(weights[FALSE, , drop = FALSE])
  out <- vector("list", 2L * nrow(weights)); n <- 0L
  for (b in seq_len(nrow(weights))) {
    a <- if (b == 1L) nrow(weights) else b - 1L
    if (inside[a] != inside[b]) {
      # Ordering by scalar makes a shared edge's intersection independent of
      # its winding in the two incident faces.
      low <- if (scalar[a] < scalar[b]) a else b
      high <- if (low == a) b else a
      t <- (level - scalar[low]) / (scalar[high] - scalar[low])
      n <- n + 1L; out[[n]] <- (1 - t) * weights[low, ] + t * weights[high, ]
    }
    if (inside[b]) { n <- n + 1L; out[[n]] <- weights[b, ] }
  }
  out <- do.call(rbind, out[seq_len(n)])
  out[!duplicated(out), , drop = FALSE]
}

band_split_primitives_3d <- function(data, primitives, kinds, counts, params) {
  bands <- params$.band_scales
  bands <- bands[vapply(names(bands), function(n) paste0(".band_", n) %in% names(data), logical(1))]
  if (!length(bands) || !any(kinds == "faces"))
    return(list(data = data, primitives = primitives, kinds = kinds, counts = counts, params = params))
  if ("colour" %in% names(bands) && any(kinds == "polylines" & counts > 2L)) {
    offsets <- cumsum(c(1L, head(counts, -1L)))
    pieces <- lapply(seq_along(counts), function(i) {
      r <- primitives[offsets[i] + seq_len(counts[i]) - 1L]
      if (kinds[i] == "polylines") lapply(seq_len(length(r) - 1L), function(j) r[c(j, j + 1L)]) else list(r)
    })
    pieces <- unlist(pieces, recursive = FALSE)
    kinds <- rep(kinds, ifelse(kinds == "polylines", counts - 1L, 1L))
    counts <- lengths(pieces); primitives <- unlist(pieces, use.names = FALSE)
  }
  normal <- intersect(c("nx", "ny", "nz"), names(data))
  if (length(normal) == 3L) {
    n <- as.matrix(data[normal]); size <- sqrt(rowSums(n^2))
    good <- is.finite(size) & size > 0
    n[good, ] <- n[good, , drop = FALSE] / size[good]
    data[normal] <- n
    data[c(".band_nx", ".band_ny", ".band_nz")] <- NA_real_
    params$.band_normals <- TRUE
  }
  offsets <- cumsum(c(1L, head(counts, -1L)))
  chunks <- indices <- parents <- out_counts <- out_kinds <- vector("list", length(counts))
  numeric <- intersect(c("x", "y", "z", ".w", normal, "u", "v", ".texture_u", ".texture_v",
                        paste0(".band_", names(bands))), names(data))
  samples <- as.matrix(data[numeric])
  flat <- matrix(NA_real_, length(counts), 3L)
  interpolate_fill <- !"fill" %in% names(bands) && identical(params$shading, "smooth") && !is.null(data$fill)
  if (interpolate_fill) {
    fill <- linear_rgb_3d(data$fill)
    fill_alpha <- colour_channels_3d(data$fill, alpha = TRUE)[4L, primitives[offsets]]
    fills <- vector("list", length(counts)); fill_valid <- logical(length(counts))
  }
  for (i in seq_along(counts)) {
    rows <- primitives[offsets[i] + seq_len(counts[i]) - 1L]
    active <- if (kinds[i] == "faces") bands else if (kinds[i] == "polylines") bands[intersect("colour", names(bands))] else list()
    pieces <- list(diag(length(rows)))
    for (name in names(active)) {
      value <- data[[paste0(".band_", name)]][rows]
      if (any(!is.finite(value))) next
      cuts <- active[[name]]$breaks
      cuts <- cuts[cuts > min(value) & cuts < max(value)]
      if (!length(cuts)) next
      if (length(rows) > 3L && length(pieces) == 1L)
        pieces <- lapply(seq.int(2L, length(rows) - 1L), function(j) diag(length(rows))[c(1L, j, j + 1L), , drop = FALSE])
      for (cut in cuts) {
        divided <- list()
        for (w in pieces) {
          scalar <- drop(w %*% value)
          if (min(scalar) < cut && max(scalar) > cut) {
            divided <- c(divided, list(band_clip_weights_3d(w, value, cut, FALSE),
                                      band_clip_weights_3d(w, value, cut, TRUE)))
          } else divided <- c(divided, list(w))
        }
        pieces <- divided
      }
    }
    weights <- do.call(rbind, pieces)
    indices[[i]] <- rows[max.col(weights, ties.method = "first")]
    parents[[i]] <- rep.int(i, nrow(weights))
    chunks[[i]] <- weights %*% samples[rows, , drop = FALSE]
    if (length(normal) == 3L && kinds[i] == "faces") {
      average <- colMeans(n[rows, , drop = FALSE])
      flat[i, ] <- average / sqrt(sum(average^2))
    }
    if (interpolate_fill) {
      fills[[i]] <- weights %*% fill[rows, , drop = FALSE]
      fill_valid[i] <- length(active) > 0L && all(!is.na(data$fill[rows]))
    }
    out_counts[[i]] <- vapply(pieces, nrow, integer(1))
    out_kinds[[i]] <- rep(kinds[i], length(pieces))
  }
  parent <- unlist(parents, use.names = FALSE)
  data <- data[unlist(indices, use.names = FALSE), , drop = FALSE]
  data[numeric] <- do.call(rbind, chunks)
  counts <- unlist(out_counts, use.names = FALSE)
  kinds <- unlist(out_kinds, use.names = FALSE)
  if (length(normal) == 3L) data[c(".band_nx", ".band_ny", ".band_nz")] <- flat[parent, , drop = FALSE]
  if (interpolate_fill) {
    rgb <- do.call(rbind, fills)
    valid <- fill_valid[parent]
    data$fill[valid] <- sprintf("%s%02X", encode_rgb_3d(rgb[valid, , drop = FALSE]), fill_alpha[parent[valid]])
  }
  owner <- rep.int(seq_along(counts), counts)
  for (name in names(bands)) {
    active <- kinds == "faces" | (name == "colour" & kinds == "polylines")
    value <- rowsum(data[[paste0(".band_", name)]], owner, reorder = FALSE)[, 1L] / counts
    data[[name]][rep.int(active, counts)] <- rep.int(bands[[name]]$map(value[active]), counts[active])
  }
  params$.vertex_key <- NULL
  list(data = data, primitives = seq_len(nrow(data)), kinds = kinds, counts = counts, params = params)
}
