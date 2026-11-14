quadric_rows <- function(kind, linear, origin, ...) {
  n <- length(kind)
  linear <- array(linear, c(3L, 3L, n))
  c(list(kind = as.integer(kind)), ggplot3d:::quadric_transforms_3d(linear, matrix(origin, n, 3L, byrow = TRUE)), list(...))
}
quadric_packet <- function(quadrics, faces = NULL, fill = "red") {
  styles <- data.frame(style_id = 1:2, fill = c(fill, "grey50"), shade = FALSE)
  if (is.null(faces)) return(new_scene_packet(quadrics = quadrics, styles = styles))
  quadrics$style_id <- rep(1L, length(quadrics$kind))
  new_scene_packet(vertices = faces$vertices, corners = list(vertex = seq_along(faces$vertices$x)),
    faces = list(offset = 1L, count = length(faces$vertices$x), style_id = 2L), quadrics = quadrics, styles = styles)
}
