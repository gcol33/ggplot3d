# Small multiples in depth. Each combination of the slicing variables is a
# panel of the layout while statistics and position adjustments are computed,
# as in facet_wrap(), so every slice gets its own bins, fits and stacks. The
# slice's position is a position aesthetic of every row, and at the end of the
# build all rows join the first panel, which draws them as one scene.

facet_expressions_3d <- function(facets) {
  if (inherits(facets, "formula")) {
    expressions <- lapply(all.vars(facets), as.symbol)
    names(expressions) <- all.vars(facets)
    return(lapply(expressions, function(e) list(expr = e, env = environment(facets))))
  }
  if (is.character(facets)) {
    return(stats::setNames(lapply(facets, function(name) list(expr = as.symbol(name), env = globalenv())), facets))
  }
  if (is.list(facets) && length(facets) && all(vapply(facets, inherits, logical(1), "quosure"))) {
    out <- lapply(facets, function(q) list(expr = unclass(q)[[2L]], env = environment(q)))
    names(out) <- vapply(seq_along(facets), function(i) {
      name <- names(facets)[i]
      if (!is.null(name) && nzchar(name)) name else axis_mapping_label_3d(facets[[i]])
    }, character(1))
    return(out)
  }
  stop("facets must be vars(), a one-sided formula or a character vector of column names.", call. = FALSE)
}

# Values of the slicing variables in a layer's data, or NULL when the data
# lack a variable, in which case the layer is repeated in every slice.
slice_values_3d <- function(data, facets) {
  if (!is.data.frame(data) || !nrow(data)) return(NULL)
  needed <- unique(unlist(lapply(facets, function(f) all.vars(f$expr))))
  if (!all(needed %in% names(data))) return(NULL)
  values <- lapply(facets, function(f) {
    value <- eval(f$expr, data, f$env)
    if (length(value) == 1L) value <- rep(value, nrow(data))
    if (length(value) != nrow(data)) stop("A slicing variable must have one value per row.", call. = FALSE)
    value
  })
  as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
}

slice_key_3d <- function(values) do.call(paste, c(lapply(values, as.character), sep = "\r"))

FacetSlices3D <- ggplot2::ggproto("FacetSlices3D", ggplot2::FacetNull,
  shrink = TRUE,
  compute_layout = function(self, data, params) {
    found <- Filter(Negate(is.null), lapply(data, slice_values_3d, facets = params$facets))
    if (!length(found)) stop("No layer has the slicing variables of facet_slices_3d().", call. = FALSE)
    values <- do.call(rbind, found)
    values <- values[!duplicated(slice_key_3d(values)), , drop = FALSE]
    ranks <- lapply(values, function(v) {
      levels <- if (is.factor(v)) levels(v) else sort(unique(v))
      match(as.character(v), as.character(levels))
    })
    values <- values[do.call(order, ranks), , drop = FALSE]
    n <- nrow(values)
    labels <- do.call(paste, c(lapply(values, as.character), sep = ", "))
    position <- if (ncol(values) == 1L && is.numeric(values[[1L]])) values[[1L]] else factor(labels, levels = unique(labels))
    layout <- data.frame(PANEL = factor(seq_len(n)), ROW = 1L, COL = seq_len(n), SCALE_X = 1L, SCALE_Y = 1L)
    layout$.slice_key_3d <- slice_key_3d(values)
    layout$.slice_3d <- position
    layout$.slice_label_3d <- labels
    layout
  },
  map_data = function(data, layout, params) {
    if (inherits(data, "waiver")) return(data.frame(PANEL = factor()))
    if (!nrow(data)) return(cbind(data, PANEL = factor()))
    values <- slice_values_3d(data, params$facets)
    if (is.null(values)) {
      index <- rep(seq_len(nrow(layout)), each = nrow(data))
      data <- data[rep(seq_len(nrow(data)), times = nrow(layout)), , drop = FALSE]
    } else {
      index <- match(slice_key_3d(values), layout$.slice_key_3d)
    }
    data$PANEL <- layout$PANEL[index]
    data$.slice_3d <- layout$.slice_3d[index]
    data$.slice_label_3d <- layout$.slice_label_3d[index]
    data
  },
  # The slice position of each row, set on the slicing axis.
  positions_3d = function(self, out, data) {
    along <- self$params$along
    if (along %in% names(out)) stop("facet_slices_3d() places the slices on ", along, "; a layer maps ", along, " itself.", call. = FALSE)
    if (!is.null(data$.slice_3d) && nrow(data) == nrow(out)) out[[along]] <- data$.slice_3d
    out
  },
  # Groups stay apart across slices once the slices share one panel.
  finish_data = function(data, layout, x_scales, y_scales, params) {
    if (!nrow(data)) return(data)
    key <- paste(as.integer(data$PANEL), data$group)
    data$group <- match(key, unique(key))
    data$PANEL <- factor(rep(levels(data$PANEL)[1L], nrow(data)), levels = levels(data$PANEL))
    data
  },
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if (!inherits(coord, "Coord3D")) stop("facet_slices_3d() requires coord_3d().", call. = FALSE)
    if (identical(coord$z_scales, "free")) stop("facet_slices_3d() draws all slices in one panel; use coord_3d(z_scales = \"fixed\").", call. = FALSE)
    ggplot2::ggproto_parent(ggplot2::FacetNull, self)$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
  }
)

#' Small multiples on parallel planes
#'
#' Splits the data by one or more variables, like [ggplot2::facet_wrap()],
#' and draws the subsets on parallel planes stacked along one axis of a
#' single 3D panel, one translucent plane per subset, as matplotlib stacks
#' 2D polygons and bars at several depths of one 3D axes. Statistics and
#' position adjustments run within each slice, so a histogram or a smooth is
#' computed for every subset; layers whose data lack the slicing variables
#' are repeated in every slice.
#'
#' The slice's position is set on the `along` aesthetic of every layer, so the
#' layers draw with the other two: with `along = "z"` ordinary x/y layers lie
#' on horizontal planes, and `coord_3d(up = "y")` turns those planes upright,
#' one behind the other. A single numeric slicing variable places the slices
#' at its values on a continuous scale; otherwise the slices sit at the levels
#' of a discrete scale, in the order of factor levels or sorted values. Each
#' plane carries its label, as a facet strip does.
#' @param facets Slicing variables: [ggplot2::vars()], a one-sided formula
#'   such as `~ year`, or a character vector of column names.
#' @param along Axis the slices are stacked along: `"z"`, `"y"` or `"x"`.
#' @param planes Whether to draw the translucent planes.
#' @param labels Whether to label each plane with its slice.
#' @param fill,colour,alpha Fill, outline colour and opacity of the planes.
#' @return An object to add to a plot with `+`: the facet, the plane layer
#'   (drawn first) and the label layer.
#' @seealso [annotation_plot_3d()] for one whole 2D plot on a plane.
#' @examples
#' d <- data.frame(year = rep(c(2000, 2010, 2020), each = 150),
#'   x = rnorm(450, mean = rep(c(0, 0.5, 1), each = 150)))
#' ggplot2::ggplot(d, ggplot2::aes(x)) +
#'   ggplot2::geom_density(fill = "steelblue", alpha = 0.6) +
#'   facet_slices_3d(~year) +
#'   coord_3d(up = "y", azimuth = 30, elevation = 20)
#' ggplot2::ggplot(d, ggplot2::aes(x)) +
#'   ggplot2::geom_histogram(bins = 15, position = "identity", fill = "grey60") +
#'   facet_slices_3d(~year, along = "z") +
#'   coord_3d()
#' @export
facet_slices_3d <- function(facets, along = "z", planes = TRUE, labels = TRUE,
                            fill = "grey85", colour = "grey60", alpha = 0.35) {
  expressions <- facet_expressions_3d(facets)
  along <- match.arg(along, c("z", "y", "x"))
  if (!is.logical(planes) || length(planes) != 1L || is.na(planes)) stop("planes must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) stop("labels must be TRUE or FALSE.", call. = FALSE)
  others <- setdiff(c("x", "y", "z"), along)
  facet <- ggplot2::ggproto(NULL, FacetSlices3D, params = list(facets = expressions, along = along,
    axis_titles_3d = stats::setNames(list(paste(names(expressions), collapse = ", ")), along)))
  plane_layer <- NULL
  if (planes) {
    corners <- data.frame(c(-Inf, Inf, Inf, -Inf), c(-Inf, -Inf, Inf, Inf))
    names(corners) <- others
    plane_layer <- make_layer_3d(ggplot2::GeomPolygon, do.call(ggplot2::aes, lapply(stats::setNames(others, others), as.symbol)),
      corners, "identity", "identity", list(fill = fill, colour = colour, alpha = alpha, linewidth = 0.3), FALSE, FALSE)
  }
  label_layer <- NULL
  if (labels) {
    corner <- data.frame(-Inf, Inf)
    names(corner) <- others
    label_layer <- make_layer_3d(ggplot2::GeomText, do.call(ggplot2::aes, c(lapply(stats::setNames(others, others), as.symbol),
      list(label = quote(.slice_label_3d)))), corner, "identity", "identity",
      list(hjust = 0, vjust = 1, size = 3.2, colour = "grey30"), FALSE, FALSE)
  }
  structure(list(facet = facet, planes = plane_layer, labels = label_layer), class = "ggplot3d_slices")
}

S7::method(update_ggplot, list(S7::new_S3_class("ggplot3d_slices"), ggplot2::class_ggplot)) <- function(object, plot, ...) {
  plot@facet <- object$facet
  if (!is.null(object$planes)) plot@layers <- c(list(slice_planes = object$planes), plot@layers)
  if (!is.null(object$labels)) plot <- plot + object$labels
  plot
}
