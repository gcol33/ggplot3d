#' Show one 3D scene from several views
#'
#' `facet_views()` draws all data of a [coord_3d()] plot once per view, one
#' panel per camera, like the multi-view drawings of engineering graphics and
#' the shared views of matplotlib's 3D axes. Every panel shows the same scene
#' with the same x, y and z scales and the same magnification, so a length
#' measures the same on the page in every orthographic view.
#'
#' With `layout = "third_angle"` or `"first_angle"` the panels follow the
#' projection methods of ISO 5456-2. The views are recognised by name:
#' `"front"`, `"top"`, `"bottom"`, `"left"`, `"right"` and `"rear"`. In
#' third-angle projection the top view sits above the front view and the right
#' view to its right; in first-angle projection the top view sits below the
#' front view and the right view to its left. The rear view sits at the end of
#' the row. Further views, such as an isometric one, fill the free corners,
#' first the upper right. Views in one row share their vertical page
#' positions and views in one column their horizontal ones, so an edge seen in
#' two adjacent views lines up across them.
#'
#' The layers and their statistics are computed once; the panels differ only
#' in their camera and lighting. The plot's layer data therefore all belong to the first
#' panel.
#' @param views Views to draw, in panel order: a character vector of
#'   [camera_3d()] preset names, or a named list whose entries are preset
#'   names or lists of camera arguments such as `camera_3d("front")` or
#'   `list(azimuth = 30, elevation = 20)`. Camera arguments a view leaves out
#'   come from [coord_3d()]. Names label the panels; unnamed preset entries
#'   are labelled by the preset name.
#'   `NULL` uses the coordinate's camera.
#' @param lights NULL inherits coordinate lighting. A named list of lights
#'   from [lights_3d()], [light_3d()] or the individual light constructors
#'   supplies lighting conditions; a NULL list entry selects an unlit panel.
#' @param combine `"cross"` visits the lights in order for each camera;
#'   `"pair"` pairs the specifications, broadcasting a singleton. Shared
#'   guides use the first panel's camera and lighting as their reference.
#'   Engineering layouts accept one lighting condition.
#' @param layout Panel placement: `"wrap"` fills a grid row by row,
#'   `"third_angle"` and `"first_angle"` place the principal views by the
#'   projection method.
#' @param nrow,ncol Grid dimensions of the `"wrap"` layout.
#' @param labeller,strip.position Panel strip labeller and position, as in
#'   [ggplot2::facet_wrap()]. Use `theme(strip.text = element_blank())` for a
#'   drawing without view names.
#' @return A ggplot2 Facet.
#' @seealso [camera_3d()] for the named views, [view_3d()] with `link` to turn
#'   several views together in the browser.
#' @examples
#' block <- expand.grid(x = 1:4, y = 1:3, z = 1:3)
#' block <- block[block$z == 1 | (block$x == 1 & block$y < 3), ]
#' p <- ggplot2::ggplot(block, ggplot2::aes(x, y, z = z)) +
#'   geom_voxel_3d(fill = "grey80", colour = "grey20") +
#'   coord_3d()
#' p + facet_views()
#' p + facet_views(c("front", "top", "right", iso = "isometric"), layout = "third_angle")
#' p + facet_views(c("front", "top", "right", iso = "isometric"), layout = "first_angle")
#' p + facet_views(list(a = list(azimuth = -30), b = list(azimuth = 60, zoom = 0.8)))
#' p + facet_views(views = NULL, lights = list(
#'   left = lights_3d(light_directional(c(-1, 1, 2))),
#'   right = lights_3d(light_directional(c(1, 1, 2)))))
#' @export
facet_views <- function(views = c("front", "top", "right", "isometric"),
                        layout = c("wrap", "third_angle", "first_angle"),
                        nrow = NULL, ncol = NULL, labeller = "label_value", strip.position = "top",
                        lights = NULL, combine = c("cross", "pair")) {
  layout <- match.arg(layout)
  inherit_view <- is.null(views)
  views <- if (inherit_view) list(camera = list()) else views_spec_3d(views)
  inherit_light <- is.null(lights)
  lights <- views_lights_3d(lights)
  if (layout != "wrap" && length(lights) > 1L)
    stop("Engineering layouts accept one lighting condition; use layout = 'wrap' for multiple lights.", call. = FALSE)
  specification <- views_conditions_3d(views, lights, match.arg(combine), inherit_view, inherit_light, layout)
  for (dim in list(nrow = nrow, ncol = ncol)) {
    if (!is.null(dim) && !(is.numeric(dim) && length(dim) == 1L && is.finite(dim) && dim >= 1 && dim == trunc(dim)))
      stop("nrow and ncol must be NULL or one positive whole number.", call. = FALSE)
  }
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right"))
  ggplot2::ggproto(NULL, FacetViews, shrink = TRUE,
    params = list(views = views, lights = lights, inherit_light = inherit_light, specification = specification,
      layout = layout, nrow = nrow, ncol = ncol,
      facets = ggplot2::vars(view = !!as.name("view")), labeller = labeller, strip.position = strip.position,
      free = list(x = FALSE, y = FALSE), space_free = list(x = FALSE, y = FALSE),
      draw_axes = list(x = FALSE, y = FALSE), axis_labels = list(x = TRUE, y = TRUE)))
}

views_lights_3d <- function(lights) {
  if (is.null(lights)) return(list(inherit = NULL))
  if (!is.list(lights) || !length(lights) || inherits(lights, c("ggplot3d_light", "ggplot3d_lights")))
    stop("lights must be a nonempty list of lighting conditions.", call. = FALSE)
  labels <- names(lights) %or3d% rep("", length(lights))
  if (anyNA(labels)) stop("Light names must be nonmissing and unique.", call. = FALSE)
  labels[!nzchar(labels)] <- paste0("light", which(!nzchar(labels)))
  if (anyDuplicated(labels)) stop("Light names must be unique.", call. = FALSE)
  stats::setNames(lapply(lights, as_lights_3d), labels)
}

# Stable camera and light keys, in wrap visitation order.
views_conditions_3d <- function(views, lights, combine, inherit_view, inherit_light, layout) {
  nv <- length(views); nl <- length(lights)
  if (combine == "cross") {
    out <- expand.grid(LIGHT = seq_len(nl), VIEW = seq_len(nv))
  } else {
    n <- max(nv, nl)
    if (nv != nl && min(nv, nl) != 1L)
      stop("Paired views and lights need equal lengths or one singleton.", call. = FALSE)
    out <- data.frame(LIGHT = rep(seq_len(nl), length.out = n), VIEW = rep(seq_len(nv), length.out = n))
  }
  view <- names(views)[out$VIEW]; light <- names(lights)[out$LIGHT]
  out$label <- if (inherit_light || layout != "wrap") view else if (inherit_view) light else paste(view, light, sep = " / ")
  out$label <- make.unique(out$label)
  out
}

# Named list of camera argument lists, one per view.
views_spec_3d <- function(views) {
  if (!length(views) || !(is.character(views) || is.list(views)))
    stop("views must be a character vector of camera_3d() presets or a list of views.", call. = FALSE)
  views <- as.list(views)
  labels <- names(views) %or3d% rep("", length(views))
  presets <- names(camera_presets_3d())
  out <- lapply(seq_along(views), function(i) {
    view <- views[[i]]
    if (is.character(view)) {
      if (length(view) != 1L || !(view %in% presets))
        stop("Unknown camera view '", paste(view, collapse = ", "), "'; use one of ", paste(presets, collapse = ", "), ".", call. = FALSE)
      if (!nzchar(labels[i])) labels[i] <<- view
      return(camera_3d(view))
    }
    if (!is.list(view)) stop("Each view is a camera_3d() preset name or a list of camera arguments.", call. = FALSE)
    if (!nzchar(labels[i])) stop("A view given as camera arguments needs a name.", call. = FALSE)
    camera_args_update_3d(list(), view)
  })
  if (anyDuplicated(labels)) stop("View names must be unique.", call. = FALSE)
  stats::setNames(out, labels)
}

# Grid cells of the principal views relative to the front view, by projection
# method (ISO 5456-2), and the free cells further views take in turn.
projection_cells_3d <- function(method) {
  principal <- switch(method,
    third_angle = list(front = c(2, 2), top = c(1, 2), bottom = c(3, 2), left = c(2, 1), right = c(2, 3), rear = c(2, 4)),
    first_angle = list(front = c(2, 2), top = c(3, 2), bottom = c(1, 2), left = c(2, 3), right = c(2, 1), rear = c(2, 4)))
  list(principal = principal, free = list(c(1, 3), c(3, 3), c(1, 1), c(3, 1), c(1, 4), c(3, 4)))
}

views_layout_3d <- function(labels, layout, nrow = NULL, ncol = NULL) {
  n <- length(labels)
  if (layout == "wrap") {
    if (is.null(nrow) && is.null(ncol)) ncol <- ceiling(sqrt(n))
    if (is.null(ncol)) ncol <- ceiling(n / nrow)
    if (is.null(nrow)) nrow <- ceiling(n / ncol)
    if (nrow * ncol < n) stop("nrow * ncol must be at least the number of views.", call. = FALSE)
    index <- seq_len(n) - 1L
    cells <- cbind((index %/% ncol) + 1L, (index %% ncol) + 1L)
  } else {
    grid <- projection_cells_3d(layout)
    key <- tolower(labels)
    principal <- key %in% names(grid$principal)
    others <- which(!principal)
    if (length(others) > length(grid$free))
      stop("A projection layout holds at most ", length(grid$free), " views besides the six principal views.", call. = FALSE)
    cells <- matrix(0L, n, 2L)
    cells[principal, ] <- do.call(rbind, grid$principal[key[principal]])
    if (length(others)) cells[others, ] <- do.call(rbind, grid$free[seq_along(others)])
    cells[, 1L] <- match(cells[, 1L], sort(unique(cells[, 1L])))
    cells[, 2L] <- match(cells[, 2L], sort(unique(cells[, 2L])))
  }
  data.frame(PANEL = factor(seq_len(n)), ROW = as.integer(cells[, 1L]), COL = as.integer(cells[, 2L]),
    view = factor(labels, levels = labels), VIEW = seq_len(n), SCALE_X = 1L, SCALE_Y = 1L)
}

FacetViews <- ggplot2::ggproto("FacetViews", ggplot2::FacetWrap,
  compute_layout = function(self, data, params) {
    spec <- params$specification
    out <- views_layout_3d(spec$label, params$layout, params$nrow, params$ncol)
    keys <- match(as.character(out$view), spec$label)
    out$VIEW <- spec$VIEW[keys]
    out$LIGHT <- spec$LIGHT[keys]
    out
  },
  # The scene is the same in every view: all rows belong to the first panel
  # and draw_panel_content() hands its layer grobs to every panel.
  map_data = function(data, layout, params) {
    if (is.null(data)) return(data)
    data$PANEL <- factor(rep(layout$PANEL[1L], nrow(data)), levels = levels(layout$PANEL))
    data
  },
  draw_panel_content = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params, ...) {
    if (!inherits(coord, "Coord3D")) stop("facet_views() requires coord_3d().", call. = FALSE)
    shared <- lapply(panels, function(layer) rep(list(layer[[1L]]), nrow(layout)))
    ggplot2::ggproto_parent(ggplot2::FacetWrap, self)$draw_panel_content(shared, layout, x_scales, y_scales,
      ranges, coord, data, theme, params, ...)
  }
)
