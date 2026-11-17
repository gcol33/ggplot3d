# Screen rectangles occupied by point glyphs and line segments.
guide_obstacles_3d <- function(packets, project) {
  boxes <- list()
  add <- function(a, b, radius) {
    finite <- rowSums(is.finite(a) & is.finite(b)) == 2L
    if (any(finite)) boxes[[length(boxes) + 1L]] <<-
      cbind(pmin(a, b) - radius, pmax(a, b) + radius)[finite, , drop = FALSE]
  }
  for (packet in packets) {
    if (!length(packet$vertices$x) || (!length(packet$markers$offset) && !length(packet$polylines$offset))) next
    xy <- project(cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z))[, 1:2, drop = FALSE]
    for (kind in c("markers", "polylines")) {
      tab <- packet[[kind]]
      if (!length(tab$offset)) next
      if (kind == "markers") {
        points <- xy[packet$corners$vertex[tab$offset], , drop = FALSE]
        style <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
        add(points, points, paint_metrics_3d(style)$radius_mm)
      } else {
        counts <- pmax(tab$count - 1L, 0L)
        indices <- rep(tab$offset, counts) + sequence(counts) - 1L
        add(xy[packet$corners$vertex[indices], , drop = FALSE],
          xy[packet$corners$vertex[indices + 1L], , drop = FALSE], rep(tab$width_mm / 2, counts))
      }
    }
  }
  if (length(boxes)) do.call(rbind, boxes) else matrix(numeric(), 0L, 4L)
}

# Translate a label along its guide normal until its rectangle clears the data.
clear_label_location_3d <- function(location, size, obstacles, direction, gap = 0.8) {
  norm <- sqrt(sum(direction^2))
  if (!nrow(obstacles) || !is.finite(norm) || norm < 1e-9) return(location)
  direction <- direction / norm
  half <- size / 2 + gap
  for (step in seq_len(nrow(obstacles))) {
    box <- c(location - half, location + half)
    hit <- which(obstacles[, 1L] < box[3L] & obstacles[, 3L] > box[1L] &
      obstacles[, 2L] < box[4L] & obstacles[, 4L] > box[2L])
    if (!length(hit)) break
    distance <- vapply(hit, function(i) {
      travel <- rep(Inf, 2L)
      for (axis in 1:2) {
        if (direction[axis] > 1e-9) travel[axis] <- (obstacles[i, axis + 2L] - box[axis]) / direction[axis]
        else if (direction[axis] < -1e-9) travel[axis] <- (obstacles[i, axis] - box[axis + 2L]) / direction[axis]
      }
      min(travel)
    }, numeric(1))
    location <- location + direction * (max(distance) + 1e-6)
  }
  location
}

# Scene guide text uses justification to carry a physical offset from its anchor.
clear_guide_text_3d <- function(packet, project, obstacles) {
  tab <- packet$text
  if (!length(tab$offset) || !nrow(obstacles)) return(packet)
  styles <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
  extent <- text_extent_mm_3d(styles)
  anchors <- project(cbind(packet$vertices$x, packet$vertices$y, packet$vertices$z))[, 1:2, drop = FALSE]
  for (i in seq_along(tab$offset)) {
    a <- tab$angle[i] * pi / 180
    rotation <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2L)
    offset <- drop(rotation %*% ((0.5 - c(tab$hjust[i], tab$vjust[i])) * extent[i, ]))
    anchor <- anchors[packet$corners$vertex[tab$offset[i]], ]
    location <- anchor + offset
    direction <- if (sqrt(sum(offset^2)) > 1e-9) offset else c(0, 1)
    moved <- clear_label_location_3d(location, rotated_extent_3d(extent[i, , drop = FALSE], tab$angle[i])[1L, ], obstacles, direction)
    local <- drop(t(rotation) %*% (moved - location)) / pmax(extent[i, ], 1e-6)
    packet$text$hjust[i] <- tab$hjust[i] - local[1L]
    packet$text$vjust[i] <- tab$vjust[i] - local[2L]
  }
  packet
}
