# Scalar samples retained before the flat material is averaged over a face.
face_fill_samples_3d <- function(data, face, shading, average = mesh_face_mean_3d(data$fill, face)) {
  if (is.numeric(data$fill) && identical(shading, "flat")) {
    data$.vertex_fill <- data$fill
    data$fill <- average
    data$.face_fill <- data$fill
  }
  data
}

# Breaks are in transformed aesthetic units, as are the values entering map_df.
band_scale_3d <- function(scale) {
  if (scale$is_discrete() || inherits(scale, "ScaleContinuousIdentity")) return(NULL)
  if (inherits(scale, "ScaleBinned")) {
    breaks <- scale$get_breaks()
    limits <- scale$get_limits()
    breaks <- sort(unique(c(limits, breaks[is.finite(breaks) & breaks > min(limits) & breaks < max(limits)])))
  } else {
    palette <- scale$palette
    if (inherits(palette, "ggproto_method")) palette <- environment(palette)$f
    if (inherits(palette, "pal_continuous")) return(NULL)
    # A constant plateau on both sides of every colour jump distinguishes a
    # stepped palette from the encoded channel quantisation of a gradient.
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
    limits <- scale$get_limits()
    # Rescalers may reverse or nonlinearly place palette coordinates.
    raw <- seq(min(limits), max(limits), length.out = 1025L)
    mapped <- scale$rescale(raw, limits)
    if (any(!is.finite(mapped)) || any(diff(mapped) == 0) ||
        !(all(diff(mapped) > 0) || all(diff(mapped) < 0))) return(NULL)
    targets <- (lo + hi) / 2
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
  list(breaks = breaks, map = scale$map)
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
      df[[paste0(".band_", name)]] <- value
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
  chunks <- vector("list", length(counts)); out_counts <- out_kinds <- vector("list", length(counts))
  numeric <- intersect(c("x", "y", "z", ".w", normal, "u", "v", ".texture_u", ".texture_v",
                        paste0(".band_", names(bands))), names(data))
  for (i in seq_along(counts)) {
    rows <- primitives[offsets[i] + seq_len(counts[i]) - 1L]
    d <- data[rows, , drop = FALSE]
    if (kinds[i] != "faces") {
      chunks[[i]] <- d; out_counts[[i]] <- counts[i]; out_kinds[[i]] <- kinds[i]
      next
    }
    pieces <- list(diag(length(rows)))
    for (name in names(bands)) {
      value <- d[[paste0(".band_", name)]]
      if (any(!is.finite(value))) next
      cuts <- bands[[name]]$breaks
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
    pieces <- Filter(function(w) nrow(w) >= 3L, pieces)
    chunks[[i]] <- do.call(rbind, lapply(pieces, function(w) {
      out <- d[max.col(w, ties.method = "first"), , drop = FALSE]
      out[numeric] <- w %*% as.matrix(d[numeric])
      if (!"fill" %in% names(bands) && identical(params$shading, "smooth") && all(!is.na(d$fill)))
        out$fill <- encode_rgb_3d(w %*% linear_rgb_3d(d$fill))
      for (name in names(bands)) {
        value <- mean(out[[paste0(".band_", name)]])
        out[[name]] <- bands[[name]]$map(value)
      }
      if (length(normal) == 3L) {
        flat <- colMeans(as.matrix(d[normal])); size <- sqrt(sum(flat^2))
        out[c(".band_nx", ".band_ny", ".band_nz")] <- as.list(flat / size)
      }
      out
    }))
    out_counts[[i]] <- vapply(pieces, nrow, integer(1))
    out_kinds[[i]] <- rep("faces", length(pieces))
  }
  data <- do.call(rbind, chunks)
  params$.vertex_key <- NULL
  list(data = data, primitives = seq_len(nrow(data)), kinds = unlist(out_kinds, use.names = FALSE),
       counts = unlist(out_counts, use.names = FALSE), params = params)
}
