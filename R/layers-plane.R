GeomPlane3D <- ggplot2::ggproto("GeomPlane3D", ggplot2::Geom,
  optional_aes = c("xintercept", "yintercept", "zintercept", "x", "y", "z", "nx", "ny", "nz"),
  default_aes = ggplot2::aes(fill = "grey50", colour = NA, alpha = 0.4, linewidth = 0.4, linetype = 1),
  draw_key = ggplot2::draw_key_polygon
)

#' Reference planes
#'
#' Draws planes through the panel box: planes of constant x, y or z, the
#' three-dimensional counterparts of [ggplot2::geom_hline()] and
#' [ggplot2::geom_vline()], and planes of any orientation, such as a fitted
#' regression plane. Each plane is cut to the panel box and drawn as one flat
#' face, so it intersects surfaces and solids exactly.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param xintercept,yintercept,zintercept Values at which to draw planes of
#'   constant x, y or z. Given here, they replace the layer's data and mapping,
#'   as in [ggplot2::geom_hline()].
#' @param normal,point A plane of any orientation: its normal along x, y and z
#'   in the units of the transformed position scales, and a point on it in data
#'   units, as in [clip_plane_3d()]. The point does not train the scales;
#'   `NULL` takes the centre of the panel box.
#' @details Without the arguments above, each row of the data gives one plane
#'   through the aesthetics `xintercept`, `yintercept` or `zintercept`, or
#'   through the point `x`, `y`, `z` with normal `nx`, `ny`, `nz` (in the units
#'   of the transformed position scales).
#'
#'   The plane z = a + b x + c y has normal `c(-b, -c, 1)` and passes through
#'   `c(0, 0, a)`.
#' @section Aesthetics:
#' `xintercept`, `yintercept`, `zintercept`, `x`, `y`, `z`, `nx`, `ny`, `nz`,
#' `fill`, `alpha`, `colour` (the outline where the plane meets the box),
#' `linewidth`, `linetype`.
#' @return A ggplot2 layer.
#' @seealso [clip_plane_3d()] cuts layers by a plane.
#' @examples
#' d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
#' d$z <- exp(-(d$x^2 + d$y^2))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(fill = "grey85")
#' p + geom_plane_3d(zintercept = 0.5, fill = "steelblue") + coord_3d()
#' p + geom_plane_3d(xintercept = c(-1, 1), colour = "grey20") + coord_3d()
#'
#' fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
#' b <- stats::coef(fit)
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
#'   geom_point_3d() +
#'   geom_plane_3d(normal = c(-b[["wt"]], -b[["hp"]], 1), point = c(0, 0, b[[1]])) +
#'   coord_3d()
#' @export
geom_plane_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          xintercept, yintercept, zintercept, normal = NULL, point = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  intercepts <- list()
  if (!missing(xintercept)) intercepts$xintercept <- xintercept
  if (!missing(yintercept)) intercepts$yintercept <- yintercept
  if (!missing(zintercept)) intercepts$zintercept <- zintercept
  if (length(intercepts) && !is.null(normal)) stop("Give intercepts or a normal, not both.", call. = FALSE)
  if (length(intercepts)) {
    rows <- lapply(names(intercepts), function(name) {
      values <- intercepts[[name]]
      out <- data.frame(xintercept = rep(NA_real_, length(values)), yintercept = NA_real_, zintercept = NA_real_)
      out[[name]] <- values
      out
    })
    data <- do.call(rbind, rows)
    data <- data[names(intercepts)]
    mapping <- do.call(ggplot2::aes, lapply(stats::setNames(nm = names(intercepts)), as.name))
    show.legend <- FALSE
    inherit.aes <- FALSE
  }
  if (!is.null(normal)) {
    plane_spec_3d(normal, point)
    data <- data.frame(plane = 1L)
    mapping <- ggplot2::aes()
    show.legend <- FALSE
    inherit.aes <- FALSE
  }
  make_layer_3d(GeomPlane3D, mapping, data, stat, position,
    c(list(normal = normal, point = point, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Planes of the rows in panel box coordinates: unit normals (rows) and
# offsets, NA where a row gives no complete plane.
plane_rows_3d <- function(data, params, panel_params) {
  n <- nrow(data)
  if (!is.null(params$normal)) {
    plane <- unit_plane_3d(plane_spec_3d(params$normal, params$point), panel_params)
    return(list(normal = matrix(plane$normal, n, 3L, byrow = TRUE), offset = rep(plane$offset, n)))
  }
  normal <- matrix(NA_real_, n, 3L); offset <- rep(NA_real_, n)
  for (k in 1:3) {
    value <- data[[paste0(c("x", "y", "z")[k], "intercept")]]
    if (is.null(value)) next
    rows <- is.na(offset) & is.finite(value)
    normal[rows, ] <- 0; normal[rows, k] <- 1
    offset[rows] <- -value[rows]
  }
  if (all(c("x", "y", "z", "nx", "ny", "nz") %in% names(data))) {
    rows <- which(is.na(offset))
    m <- cbind(data$nx, data$ny, data$nz)[rows, , drop = FALSE]
    m <- m / sqrt(rowSums(m^2))
    normal[rows, ] <- m
    offset[rows] <- -rowSums(m * cbind(data$x, data$y, data$z)[rows, , drop = FALSE])
  }
  list(normal = normal, offset = offset)
}

# Each plane is a large square around the box centre, cut by the six box
# planes to the convex polygon where it meets the box.
emit_planes_3d <- function(data, params, panel_params) {
  planes <- plane_rows_3d(data, params, panel_params)
  valid <- is.finite(planes$offset) & rowSums(is.finite(planes$normal)) == 3L
  if (any(!valid) && !isTRUE(params$na.rm)) warning("Removed ", sum(!valid), " rows without a complete plane.", call. = FALSE)
  rows <- which(valid)
  empty <- new_scene_packet(panel_id = panel_params$panel_id)
  if (!length(rows)) return(empty)
  square <- rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1)) * 2
  corners <- do.call(rbind, lapply(rows, function(i) {
    m <- planes$normal[i, ]
    centre <- c(0.5, 0.5, 0.5) - (sum(m * 0.5) + planes$offset[i]) * m
    sweep(square %*% t(plane_basis_3d(m)), 2L, centre, `+`)
  }))
  k <- length(rows)
  squares <- new_scene_packet(vertices = list(x = corners[, 1L], y = corners[, 2L], z = corners[, 3L]),
    corners = list(vertex = seq_len(4L * k), source_row = rep(rows, each = 4L)),
    faces = list(offset = as.integer(4L * seq_len(k) - 3L), count = rep.int(4L, k), source_row = rows),
    styles = data.frame(style_id = 1L), panel_id = panel_params$panel_id)
  cut <- clip_packet_planes_3d(squares, unit_box_planes_3d())
  faces <- cut$faces
  if (!length(faces$offset)) return(empty)
  ring <- unlist(lapply(seq_along(faces$offset), function(f) seq.int(faces$offset[f], length.out = faces$count[f])), use.names = FALSE)
  vertex <- cut$corners$vertex[ring]
  owner <- rep.int(faces$source_row, faces$count)
  points <- data[owner, setdiff(names(data), c("nx", "ny", "nz")), drop = FALSE]
  points$x <- cut$vertices$x[vertex]; points$y <- cut$vertices$y[vertex]; points$z <- cut$vertices$z[vertex]
  start <- cumsum(c(0L, faces$count[-length(faces$count)]))
  filled <- which(!is.na(data$fill[faces$source_row]))
  stroked <- which(!is.na(data$colour[faces$source_row]))
  face_corners <- lapply(filled, function(f) start[f] + seq_len(faces$count[f]))
  outlines <- lapply(stroked, function(f) start[f] + c(seq_len(faces$count[f]), 1L))
  packet_from_primitives_3d(points, unlist(c(face_corners, outlines), use.names = FALSE),
    c(rep("faces", length(filled)), rep("polylines", length(stroked))),
    params, panel_params$panel_id, c(faces$count[filled], faces$count[stroked] + 1L))
}

register_geom_3d(GeomPlane3D, emit_planes_3d, parameters = list(normal = NULL, point = NULL), default_z = -Inf)
