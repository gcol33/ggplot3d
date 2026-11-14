# Each texel is a polygon obtained by clipping the source triangle in UV.
geometric_texture_packet <- function(packet, colours) {
  vertices <- list(); fills <- character()
  corner <- packet$corners
  xyz <- cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z)
  clip <- function(p, axis, bound, sign) {
    if (!nrow(p)) return(p)
    out <- list()
    for (i in seq_len(nrow(p))) {
      a <- p[if (i == 1L) nrow(p) else i - 1L, ]; b <- p[i, ]
      da <- sign * (a[axis] - bound); db <- sign * (b[axis] - bound)
      if ((da >= 0) != (db >= 0)) out[[length(out) + 1L]] <- a + da / (da - db) * (b - a)
      if (db >= 0) out[[length(out) + 1L]] <- b
    }
    if (!length(out)) p[integer(), , drop = FALSE] else do.call(rbind, out)
  }
  for (face in seq_along(packet$faces$count)) {
    index <- seq.int(packet$faces$offset[face], length.out = packet$faces$count[face])
    for (fan in seq_len(length(index) - 2L)) {
      k <- index[c(1L, fan + 1L, fan + 2L)]
      p <- cbind(xyz[corner$vertex[k], , drop = FALSE], corner$u[k], corner$v[k])
      for (row in seq_len(nrow(colours))) for (col in seq_len(ncol(colours))) {
        cell <- clip(p, 4L, (col - 1L) / ncol(colours), 1)
        cell <- clip(cell, 4L, col / ncol(colours), -1)
        cell <- clip(cell, 5L, 1 - row / nrow(colours), 1)
        cell <- clip(cell, 5L, 1 - (row - 1L) / nrow(colours), -1)
        cell <- unique(cell)
        if (nrow(cell) < 3L) next
        next_row <- c(seq.int(2L, nrow(cell)), 1L)
        area <- sum(cell[, 4L] * cell[next_row, 5L] - cell[, 5L] * cell[next_row, 4L])
        if (abs(area) < 1e-14) next
        vertices[[length(vertices) + 1L]] <- cell[, 1:3, drop = FALSE]
        fills <- c(fills, colours[row, col])
      }
    }
  }
  counts <- vapply(vertices, nrow, integer(1))
  points <- do.call(rbind, vertices)
  palette <- unique(fills)
  new_scene_packet(vertices = list(x = points[, 1L], y = points[, 2L], z = points[, 3L]),
    corners = list(vertex = seq_len(nrow(points))),
    faces = list(offset = cumsum(c(1L, head(counts, -1L))), count = counts,
      layer_id = rep(1L, length(counts)), style_id = match(fills, palette)),
    styles = data.frame(style_id = seq_along(palette), fill = palette, colour = NA_character_, shade = FALSE))
}

texture_volcano_plot <- function() {
  d <- expand.grid(x = seq_len(nrow(volcano)), y = seq_len(ncol(volcano)))
  d$z <- as.vector(volcano)
  d <- d[d$x %% 3 == 1 & d$y %% 3 == 1, ]
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(texture = texture_3d(checker_colours(), interpolate = FALSE), shade = FALSE) + coord_3d(light = NULL)
}

# Screen-space segments of checker boundaries within visible face fragments.
texture_edge_segments <- function(scene, pixels) {
  uv <- scene_vertex_attributes_3d(scene, c("u", "v"))
  xy <- cbind(scene$vertices$x_npc * pixels[1L], (1 - scene$vertices$y_npc) * pixels[2L])
  segments <- list()
  for (face in which(scene$fragments$kind == 1L)) {
    k <- seq.int(scene$fragments$offset[face], length.out = scene$fragments$count[face])
    for (axis in 1:2) for (value in (1:7) / 8) {
      ring <- c(k, k[1L]); points <- list()
      for (j in seq_len(length(k))) {
        a <- ring[j]; b <- ring[j + 1L]
        if ((uv[a, axis] < value) == (uv[b, axis] < value)) next
        t <- (value - uv[a, axis]) / (uv[b, axis] - uv[a, axis])
        if (camera_perspective_3d(scene$camera)) {
          qa <- 1 / scene$vertices$depth[a]; qb <- 1 / scene$vertices$depth[b]
          t <- t * qa / ((1 - t) * qb + t * qa)
        }
        points[[length(points) + 1L]] <- xy[a, ] + t * (xy[b, ] - xy[a, ])
      }
      if (length(points) == 2L) segments[[length(segments) + 1L]] <- do.call(rbind, points)
    }
  }
  segments
}

texture_gray_image <- function(image) {
  if (is.character(image)) {
    rgb <- png::readPNG(image)
    value <- rgb[, , 1L]
    alpha <- matrix(1, nrow(value), ncol(value))
  } else {
    channels <- texture_channels_3d(image)
    value <- matrix(channels[1L, ] / 255, nrow(image), ncol(image), byrow = TRUE)
    alpha <- matrix(channels[4L, ] / 255, nrow(image), ncol(image), byrow = TRUE)
  }
  result <- value
  attr(result, "coverage") <- alpha
  result
}

texture_profile_sample <- function(image, points) {
  x <- points[, 1L] + .5; y <- points[, 2L] + .5
  x0 <- floor(x); y0 <- floor(y); fx <- x - x0; fy <- y - y0
  at <- function(dx, dy) image[cbind(pmax(1L, pmin(nrow(image), y0 + dy)), pmax(1L, pmin(ncol(image), x0 + dx)))]
  (1 - fy) * ((1 - fx) * at(0, 0) + fx * at(1, 0)) +
    fy * ((1 - fx) * at(0, 1) + fx * at(1, 1))
}

# Boundary displacement along perpendicular profiles beside triangle edges.
texture_edge_displacements <- function(image, reference, segments) {
  steps <- seq(-2, 2, length.out = 161L)
  crossing <- function(values) {
    k <- which(diff(values >= .5) != 0)
    if (!length(k)) return(NA_real_)
    roots <- steps[k] + (.5 - values[k]) / (values[k + 1L] - values[k]) * (steps[k + 1L] - steps[k])
    roots[which.min(abs(roots))]
  }
  result <- numeric(); locations <- list()
  for (segment in segments) {
    direction <- segment[2L, ] - segment[1L, ]; length <- sqrt(sum(direction^2))
    if (length < 8) next
    direction <- direction / length; normal <- c(-direction[2L], direction[1L])
    for (end in 1:2) {
      centre <- segment[end, ] + (if (end == 1L) 1 else -1) * direction * 2
      points <- sweep(outer(steps, normal), 2L, centre, "+")
      values <- texture_profile_sample(reference, points)
      if (min(values) > .01 || max(values) < .99) next
      if (any(texture_profile_sample(attr(reference, "coverage"), points) < .999)) next
      if (!all(diff(values) >= -1e-8) && !all(diff(values) <= 1e-8)) next
      expected <- crossing(values)
      if (!is.finite(expected)) next
      actual <- crossing(texture_profile_sample(image, points))
      result <- c(result, if (is.finite(actual)) actual - expected else Inf)
      locations[[length(locations) + 1L]] <- c(centre, expected, actual, normal)
    }
  }
  attr(result, "locations") <- do.call(rbind, locations)
  result
}

texture_device_image <- function(scene, viewport, device, browser = NULL, light = NULL, reference = NULL) {
  width <- viewport$width_mm / 25.4; height <- viewport$height_mm / 25.4
  pixels <- round(c(width, height) * viewport$dpi)
  path <- tempfile(fileext = if (device == "ragg") ".png" else if (device == "svglite") ".svg" else ".pdf")
  png <- tempfile(fileext = ".png")
  on.exit(unlink(c(path, png)), add = TRUE)
  switch(device, ragg = ragg::agg_png(path, pixels[1L], pixels[2L], res = viewport$dpi, background = "grey50"),
    svglite = svglite::svglite(path, width, height, bg = "grey50"),
    pdf = grDevices::pdf(path, width, height, bg = "grey50"),
    cairo = grDevices::cairo_pdf(path, width, height, bg = "grey50"))
  grob <- if (is.null(reference)) render_scene_grid(scene, viewport, light = light) else {
    image <- raster_image_grob_3d(reference)
    image$interpolate <- TRUE
    image
  }
  grid::grid.draw(grob)
  grDevices::dev.off()
  if (device == "ragg") return(texture_gray_image(path))
  if (device == "svglite") {
    html <- tempfile(fileext = ".html")
    on.exit(unlink(html), add = TRUE)
    writeLines(sprintf('<body style="margin:0"><img width="%d" height="%d" src="file:///%s"></body>',
      pixels[1L], pixels[2L], normalizePath(path, winslash = "/")), html)
    browser$Emulation$setDeviceMetricsOverride(width = pixels[1L], height = pixels[2L], deviceScaleFactor = 1, mobile = FALSE)
    browser$Page$navigate(paste0("file:///", normalizePath(html, winslash = "/")))
    browser$Runtime$evaluate("Promise.all(Array.from(document.images, img => img.decode()))", awaitPromise = TRUE)
    shot <- browser$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE)
    writeBin(jsonlite::base64_dec(shot$data), png)
  } else {
    converted <- pdftools::pdf_convert(path, format = "png", dpi = viewport$dpi,
      filenames = paste0(tempfile(), "-%d.%s"), verbose = FALSE)
    stopifnot(file.rename(converted, png))
  }
  output <- Sys.getenv("TEXTURE_EDGE_OUT")
  if (nzchar(output) && dir.exists(output)) file.copy(png, file.path(output, paste0(device, "-unlit-", viewport$dpi, ".png")), overwrite = TRUE)
  texture_gray_image(png)
}

texture_reference_png <- function(packet, camera, viewport, pixels, light = NULL, blend = "linear") {
  sampling <- texture_run_sampling_3d(packet, viewport)
  result <- raster_scene_3d(packet, camera, sampling$viewport, sampling$antialias, light, blend = blend)
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, pixels, pixels, res = viewport$dpi, background = "transparent")
  grob <- raster_image_grob_3d(result); grob$interpolate <- sampling$interpolate
  grid::grid.draw(grob)
  grDevices::dev.off()
  path
}
