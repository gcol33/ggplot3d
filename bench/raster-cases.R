# Deterministic raster scenes built directly as scene packets, so the timings
# measure raster_prepare_cpp and raster_draw_cpp without ggplot2 build costs.

raster_case_viewport <- function(size_mm = 150, dpi = 150) {
  ggplot3d:::new_scene_viewport(size_mm, size_mm, dpi)
}

raster_case_camera <- function() ggplot3d:::new_camera_3d(azimuth = -60, elevation = 30)

raster_case_palette <- function(n) grDevices::hcl.colors(n, "viridis")

raster_case_surface <- function(cells, z0 = 0.5, amplitude = 0.25, opacity = 1,
                                styles = 64L, style_offset = 0L) {
  axis <- seq(0, 1, length.out = cells + 1L)
  grid <- expand.grid(x = axis, y = axis)
  z <- z0 + amplitude * sin(2 * pi * grid$x) * cos(2 * pi * grid$y)
  lower <- rep(seq_len(cells), cells) + rep((seq_len(cells) - 1L) * (cells + 1L), each = cells)
  tri <- rbind(cbind(lower, lower + 1L, lower + cells + 2L),
               cbind(lower, lower + cells + 2L, lower + cells + 1L))
  height <- rowMeans(matrix(z[tri], ncol = 3L))
  style <- style_offset + 1L + pmin(styles - 1L,
    as.integer((height - min(z)) / max(diff(range(z)), 1e-12) * styles))
  list(x = grid$x, y = grid$y, z = z, corners = as.vector(t(tri)), style = style,
       opacity = rep(opacity, nrow(tri)))
}

raster_case_faces <- function(parts, fills) {
  offsets <- cumsum(c(0L, head(vapply(parts, function(p) length(p$x), integer(1)), -1L)))
  vertices <- list(
    x = unlist(lapply(parts, `[[`, "x")), y = unlist(lapply(parts, `[[`, "y")),
    z = unlist(lapply(parts, `[[`, "z")))
  corners <- unlist(Map(function(p, o) p$corners + o, parts, offsets))
  nf <- length(corners) %/% 3L
  layer <- rep(seq_along(parts), vapply(parts, function(p) length(p$style), integer(1)))
  list(vertices = vertices, corners = list(vertex = as.integer(corners)),
       faces = list(offset = as.integer(seq(1L, by = 3L, length.out = nf)),
                    count = rep(3L, nf), layer_id = as.integer(layer),
                    style_id = as.integer(unlist(lapply(parts, `[[`, "style"))),
                    opacity = unlist(lapply(parts, `[[`, "opacity"))),
       styles = data.frame(style_id = seq_along(fills), fill = fills))
}

raster_case_packet <- function(f, markers = list(), text = list(), footprints = list(), styles = f$styles) {
  ggplot3d:::new_scene_packet(vertices = f$vertices, corners = f$corners, faces = f$faces,
    markers = markers, text = text, footprints = footprints, styles = styles)
}

raster_cases <- list(
  faces_1e5 = function() {
    f <- raster_case_faces(list(raster_case_surface(224L)), raster_case_palette(64L))
    raster_case_packet(f)
  },
  faces_1e6 = function() {
    f <- raster_case_faces(list(raster_case_surface(707L)), raster_case_palette(64L))
    raster_case_packet(f)
  },
  markers_1e6 = function(n = 1e6) {
    set.seed(1)
    n <- as.integer(n)
    ggplot3d:::new_scene_packet(
      vertices = list(x = stats::runif(n), y = stats::runif(n), z = stats::runif(n)),
      corners = list(vertex = seq_len(n)),
      markers = list(offset = seq_len(n), footprint_id = rep(1L, n),
                     style_id = 1L + (seq_len(n) %% 8L)),
      footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 0.5,
                        radius_y_mm = 0.5, angle = 0),
      styles = data.frame(style_id = 1:8, colour = raster_case_palette(8L)))
  },
  translucent_8 = function() {
    parts <- lapply(0:7, function(k) raster_case_surface(100L, z0 = 0.15 + k * 0.1, amplitude = 0.05,
      opacity = 0.35, styles = 1L, style_offset = k))
    f <- raster_case_faces(parts, grDevices::hcl.colors(8L, "Dark 3"))
    raster_case_packet(f)
  },
  text_2e3 = function() {
    f <- raster_case_faces(list(raster_case_surface(224L)), raster_case_palette(64L))
    set.seed(2)
    n <- 2000L
    nv <- length(f$vertices$x)
    f$vertices$x <- c(f$vertices$x, stats::runif(n))
    f$vertices$y <- c(f$vertices$y, stats::runif(n))
    f$vertices$z <- c(f$vertices$z, stats::runif(n))
    corners <- c(f$corners$vertex, nv + seq_len(n))
    f$corners <- list(vertex = corners)
    styles <- f$styles
    styles$colour <- "black"
    styles$label <- "a"
    raster_case_packet(f, text = list(offset = length(corners) - n + seq_len(n)), styles = styles)
  }
)
