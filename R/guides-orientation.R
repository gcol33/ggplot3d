#' Camera-aligned orientation guides
#'
#' A triad shows the positive model x, y and z directions in each panel,
#' including panels drawn without axes. Its corner and physical size stay
#' fixed as the camera moves, as in the orientation widget in ParaView.
#' Add it with `ggplot2::guides(orientation = guide_triad_3d())`.
#' `annotation_north_3d()` adds a single direction arrow through the same guide
#' system. Its `north` vector specifies north in model coordinates.
#' @param position One of `"bottomleft"`, `"bottomright"`, `"topleft"`,
#'   `"topright"`.
#' @param size Width and height of the guide as a physical grid unit.
#' @param labels Three axis labels.
#' @param colour Three axis colours, or one colour for every axis.
#' @param linewidth Arrow width in millimetres.
#' @param theme Local theme overrides for the text (`axis.text`).
#' @param north A finite, nonzero numeric direction vector of length three.
#' @param label Label of the north arrow.
#' @return `guide_triad_3d()` returns a guide; `annotation_north_3d()` returns
#'   a ggplot2 guides specification that can be added to a plot.
#' @references ParaView, `vtkPVAxesWidget`; lattice, `panel.cloud` direction arrows.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   coord_3d(axes = character())
#' p + ggplot2::guides(orientation = guide_triad_3d())
#' p + annotation_north_3d(north = c(0, 1, 0))
#' @export
guide_triad_3d <- function(position = "bottomleft", size = grid::unit(16, "mm"),
                           labels = c("x", "y", "z"), colour = c("#B33B33", "#278048", "#3166AD"),
                           linewidth = 0.5, theme = NULL) {
  orientation_guide_3d(diag(3), labels, colour, position, size, linewidth, theme)
}

#' @rdname guide_triad_3d
#' @export
annotation_north_3d <- function(north = c(0, 1, 0), label = "N", position = "topright",
                                size = grid::unit(16, "mm"), colour = "black", linewidth = 0.5,
                                theme = NULL) {
  if (!is.numeric(north) || length(north) != 3L || any(!is.finite(north)) || sum(north^2) <= 0) {
    stop("north must be a finite nonzero direction of length three.", call. = FALSE)
  }
  ggplot2::guides(north = orientation_guide_3d(matrix(north / sqrt(sum(north^2)), 1L), label,
    colour, position, size, linewidth, theme))
}

orientation_guide_3d <- function(directions, labels, colour, position, size, linewidth, theme) {
  position <- match.arg(position, c("bottomleft", "bottomright", "topleft", "topright"))
  if (!grid::is.unit(size) || length(size) != 1L || !grid::unitType(size) %in% c("mm", "cm", "inches", "points", "bigpts", "picas")) {
    stop("size must be one physical grid unit.", call. = FALSE)
  }
  if (!is.finite(as.numeric(size)) || as.numeric(size) <= 0) stop("size must be positive.", call. = FALSE)
  n <- nrow(directions)
  if (!is.character(labels) || length(labels) != n || anyNA(labels)) stop("labels must give one label per direction.", call. = FALSE)
  if (!is.character(colour) || !length(colour) %in% c(1L, n) || anyNA(colour)) stop("colour must give one colour per direction.", call. = FALSE)
  grDevices::col2rgb(colour)
  if (!is.numeric(linewidth) || length(linewidth) != 1L || !is.finite(linewidth) || linewidth <= 0) stop("linewidth must be positive.", call. = FALSE)
  ggplot2::new_guide(position = position, size = size, labels = labels, colour = rep_len(colour, n),
    directions = directions, linewidth = linewidth, theme = theme,
    available_aes = c("orientation", "north"), super = GuideOrientation3D)
}

GuideOrientation3D <- ggplot2::ggproto("GuideOrientation3D", ggplot2::Guide,
  params = c(ggplot2::Guide$params, list(size = grid::unit(16, "mm"), labels = c("x", "y", "z"),
    colour = "black", directions = diag(3), linewidth = 0.5)),
  draw_panel_3d = function(self, furniture, theme) {
    p <- self$params
    viewport <- furniture$viewport
    basis <- frame_basis_3d(furniture$camera, viewport)
    direction <- p$directions %*% basis$basis
    size <- grid::convertUnit(p$size, "mm", valueOnly = TRUE)
    text <- element_3d("axis.text", theme + (p$theme %or3d% ggplot2::theme()))
    fontsize <- text$size %or3d% 9
    extent <- max(grid::convertWidth(grid::stringWidth(p$labels), "mm", TRUE), fontsize * 25.4 / 72)
    side <- max(size, 2 * extent + 4)
    arm <- max(0, (side - 2 * extent) / 2)
    ends <- direction * arm / max(1, sqrt(rowSums(direction^2)))
    depth <- drop(p$directions %*% basis$forward)
    children <- list()
    for (i in order(depth, decreasing = TRUE)) {
      length <- sqrt(sum(ends[i, ]^2))
      if (length > 1e-6) {
        children[[length(children) + 1L]] <- grid::segmentsGrob(side / 2, side / 2,
          side / 2 + ends[i, 1L], side / 2 + ends[i, 2L], default.units = "mm",
          arrow = grid::arrow(type = "closed", length = grid::unit(min(2, arm / 3), "mm")),
          gp = grid::gpar(col = p$colour[i], fill = p$colour[i], lwd = p$linewidth * 72.27 / 25.4))
      } else children[[length(children) + 1L]] <- grid::circleGrob(side / 2, side / 2, r = grid::unit(0.6, "mm"),
        default.units = "mm", gp = grid::gpar(col = p$colour[i], fill = p$colour[i]))
      offset <- if (length > 1e-6) ends[i, ] / length * (extent / 2 + 0.5) else c(0, extent / 2 + 1)
      children[[length(children) + 1L]] <- grid::textGrob(p$labels[i], side / 2 + ends[i, 1L] + offset[1L],
        side / 2 + ends[i, 2L] + offset[2L], default.units = "mm",
        gp = grid::gpar(col = p$colour[i], fontsize = fontsize, fontfamily = text$family %or3d% "", fontface = text$face %or3d% 1))
    }
    fit <- furniture$overlay$fit
    width <- viewport$width_mm + sum(fit$reserve[c(2L, 4L)])
    height <- viewport$height_mm + sum(fit$reserve[c(1L, 3L)])
    if (side + 4 > min(width, height)) stop("Panel is too small for the orientation guide.", call. = FALSE)
    left <- if (endsWith(p$position, "left")) 2 else width - side - 2
    bottom <- if (startsWith(p$position, "bottom")) 2 else height - side - 2
    grid::gTree(children = do.call(grid::gList, children), directions = direction, size_mm = side,
      vp = grid::viewport(grid::unit(left, "mm"), grid::unit(bottom, "mm"),
        grid::unit(side, "mm"), grid::unit(side, "mm"), just = c(0, 0)), name = "orientation-guide-3d")
  }
)
