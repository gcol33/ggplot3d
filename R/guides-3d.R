#' Position guides for three-dimensional axes
#'
#' Tick labels and titles are measured in the drawing viewport. Automatic
#' placement uses an exterior cube edge. Integer positions 1 through 4 select
#' a fixed edge in lexicographic order of its two fixed coordinates.
#' Position `"origin"` draws the axis through the `origin` point of
#' [coord_3d()] as part of the scene, with ticks, tick labels and title in
#' three dimensions; `coord_3d(axis_lines = "origin")` places every guide with
#' position `"auto"` there.
#' Labels are thinned automatically using their measured extents along the edge.
#'
#' Tick labels can stand upright on the page (`label.orientation = "screen"`),
#' turn with the projected edge they label (`"edge"`, as `rotate_labels` in
#' ggcube and the automatic label rotation of Makie's `Axis3`), or lie flat in
#' one of the two box panes that meet at the edge (`"plane"`), drawn from the
#' glyph outlines of the font so that perspective foreshortens them like the
#' panes. `ticks = "in_plane"` points the ticks into the same pane, with a
#' length that shrinks with distance from the camera. The pane is chosen with
#' `plane`: `"auto"` takes the pane in which the ticks stand out furthest from
#' the edge on the page, `"floor"` the pane normal to the vertical axis of the
#' camera, and `"xy"`, `"xz"` or `"yz"` a pane by the two axes it spans, as
#' with the edge codes of rgl's `axis3d()`.
#'
#' Plane typography needs the 'systemfonts' package, which reads the glyph
#' outlines, and character labels. Use screen orientation for plotmath expressions.
#' @param title Guide title, with waiver inheriting the scale or plot title.
#' @param theme Local theme overrides.
#' @param position "auto", a side (top, bottom, left, right), edge 1 to 4, or
#'   "origin".
#' @param check.overlap Use extra clearance between measured label rectangles.
#' @param angle Label rotation in degrees for `label.orientation = "screen"`.
#'   Waiver keeps labels horizontal.
#' @param n.dodge Number of parallel label rows.
#' @param minor.ticks Draw minor ticks.
#' @param cap Trim the lower, upper, both, or neither end to major breaks.
#' @param order Guide ordering value.
#' @param label.orientation `"screen"`, `"edge"` or `"plane"`: tick labels
#'   upright on the page, turned along the projected edge and kept upright, or
#'   lying in a pane. Titles follow the edge in every orientation and lie in
#'   the pane with `"plane"`.
#' @param plane Pane for plane labels and in-plane ticks: `"auto"`,
#'   `"floor"`, `"xy"`, `"xz"` or `"yz"`. The pane must contain the axis.
#' @param title.position `"centre"` places the title beside the middle of the
#'   axis, beyond the tick labels; `"near"` places it at the end nearer the
#'   camera. Screen titles sit past that endpoint; plane titles sit inside
#'   the pane boundary.
#' @param depth.scale Under perspective projection, scale tick labels and
#'   tick lengths by their distance from the camera: `FALSE` (none), `TRUE`
#'   (full), or a nonnegative strength `s`, which maps the ratio `d` of the
#'   distance of the camera target to the distance of the tick to `d^s`, as
#'   `scale_depth` in ggcube. Titles are not scaled. Plane labels and in-plane
#'   ticks are foreshortened by the projection itself.
#' @param ticks `"outward"` draws ticks on the page, perpendicular to the
#'   projected edge; `"in_plane"` draws them into the pane, perpendicular to the
#'   edge in three dimensions. Plane labels and ticks depth-sort with data.
#' @return A ggplot2 guide for a three-dimensional position axis.
#' @references
#' Kling M (2025). ggcube: 3D plotting with ggplot2. `coord_3d()` arguments
#' `rotate_labels`, `scale_depth` and `title_position`.
#'
#' Danisch S, Krumbiegel J (2021). Makie.jl: Flexible high-performance data
#' visualization for Julia. Journal of Open Source Software 6(65), 3349.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 2, 1), z = 1:3)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
#' p + scale_z_continuous(guide = guide_axis_3d()) + coord_3d()
#' p + scale_z_continuous(guide = guide_axis_3d(position = "origin")) +
#'   coord_3d(origin = c(x = 2, y = 1.5))
#' p + ggplot2::guides(x = guide_axis_3d(label.orientation = "edge"),
#'                     y = guide_axis_3d(label.orientation = "edge")) +
#'   coord_3d()
#' if (requireNamespace("systemfonts", quietly = TRUE)) {
#'   p + ggplot2::guides(x = guide_axis_3d(label.orientation = "plane", ticks = "in_plane"),
#'                       y = guide_axis_3d(label.orientation = "plane", ticks = "in_plane")) +
#'     coord_3d(projection = "perspective")
#' }
#' p + ggplot2::guides(z = guide_axis_3d(title.position = "near", depth.scale = TRUE)) +
#'   coord_3d(projection = "perspective")
#' @export
guide_axis_3d <- function(title = ggplot2::waiver(), theme = NULL, position = "auto",
                          check.overlap = FALSE, angle = ggplot2::waiver(), n.dodge = 1L,
                          minor.ticks = FALSE, cap = "none", order = 0L,
                          label.orientation = "screen", plane = "auto", title.position = "centre",
                          depth.scale = FALSE, ticks = "outward") {
  if (!(length(position) == 1L && (position %in% c("auto", "origin", "top", "bottom", "left", "right", 1:4)))) stop("Invalid 3D axis position.", call. = FALSE)
  if (length(n.dodge) != 1L || !is.finite(n.dodge) || n.dodge < 1 || n.dodge != trunc(n.dodge)) stop("n.dodge must be a positive integer.", call. = FALSE)
  if (!inherits(angle, "waiver") && !(is.numeric(angle) && length(angle) == 1L && is.finite(angle))) stop("angle must be a finite number or waiver().", call. = FALSE)
  ggplot2::new_guide(title = title, theme = theme, position = position,
    check.overlap = check.overlap, angle = angle, n.dodge = as.integer(n.dodge),
    minor.ticks = minor.ticks, cap = match.arg(cap, c("none", "upper", "lower", "both")),
    order = as.integer(order),
    label.orientation = match.arg(label.orientation, c("screen", "edge", "plane")),
    plane = match.arg(plane, c("auto", "floor", "xy", "xz", "yz")),
    title.position = match.arg(title.position, c("centre", "near")),
    depth.scale = depth_strength_3d(depth.scale),
    ticks = match.arg(ticks, c("outward", "in_plane")),
    available_aes = c("x", "y", "z"), super = GuideAxis3D)
}

depth_strength_3d <- function(depth.scale) {
  if (is.logical(depth.scale) && length(depth.scale) == 1L && !is.na(depth.scale)) return(as.double(depth.scale))
  if (is.numeric(depth.scale) && length(depth.scale) == 1L && is.finite(depth.scale) && depth.scale >= 0) return(as.double(depth.scale))
  stop("depth.scale must be TRUE, FALSE or a nonnegative number.", call. = FALSE)
}

GuideAxis3D <- ggplot2::ggproto("GuideAxis3D", ggplot2::GuideAxis,
  available_aes = c("x", "y", "z"),
  params = c(ggplot2::GuideAxis$params, list(label.orientation = "screen", plane = "auto",
    title.position = "centre", depth.scale = 0, ticks = "outward")),
  extract_key = function(scale, aesthetic, ...) {
    values <- scale$get_breaks()
    data.frame(.value = values, .label = I(scale$get_labels()), stringsAsFactors = FALSE)
  },
  extract_decor = function(scale, ...) NULL,
  extract_params = function(scale, params, ...) params,
  transform = function(params, coord, panel_params) params,
  draw = function(theme, position = NULL, direction = NULL, params = list(), ...) {
    stop("3D position guides are drawn by the panel furniture renderer.", call. = FALSE)
  }
)

normalise_guide_3d <- function(spec, axis) {
  if (is.null(spec) || inherits(spec, "waiver") || identical(spec, "axis") || identical(spec, "axis_3d")) return(guide_axis_3d())
  if (identical(spec, "none") || inherits(spec, "GuideNone")) return(NULL)
  if (inherits(spec, "GuideAxis3D")) return(spec)
  if (!inherits(spec, "GuideAxis") || class(spec)[1L] != "GuideAxis") stop("Unsupported 3D position guide for ", axis, ".", call. = FALSE)
  allowed <- intersect(names(spec$params), names(formals(guide_axis_3d)))
  args <- spec$params[allowed]
  if (is.null(args$position) || inherits(args$position, "waiver")) args$position <- "auto"
  if (is.null(args$angle)) args$angle <- ggplot2::waiver()
  do.call(guide_axis_3d, args)
}

setup_guides_3d <- function(panel_params, guides, theme) {
  if (!isTRUE(.theme_state_3d$registered)) register_theme_3d()
  result <- list()
  for (axis in c("x", "y", "z")) {
    views <- list(panel_params[[axis]])
    secondary <- panel_params[[paste0(axis, ".sec")]]
    if (!is.null(secondary) && !isTRUE(secondary$scale_is_discrete) && !is.null(secondary$scale$secondary.axis) &&
        !inherits(secondary$scale$secondary.axis, "waiver")) views[[2L]] <- secondary
    for (j in seq_along(views)) {
      view <- views[[j]]
      name <- paste0(axis, if (j > 1L) ".sec" else "")
      spec <- if (is.null(guides)) NULL else guides$guides[[name]] %or3d% guides$get_guide(name)
      spec <- normalise_guide_3d(spec %or3d% view$guide, axis)
      if (is.null(spec)) next
      breaks <- view$get_breaks()
      labels <- view$get_labels()
      position <- if (is.function(view$break_positions)) view$break_positions() else (breaks - panel_params$ranges[[axis]][1L]) / diff(panel_params$ranges[[axis]])
      keep <- is.finite(position) & position >= 0 & position <= 1
      params <- spec$params
      title <- params$title
      if (inherits(title, "waiver")) title <- if (j == 1L) panel_params$axis_titles[[axis]] else view$name
      if (inherits(title, "waiver")) title <- NULL
      if (is.language(title)) title <- as.expression(title)
      minor <- if (is.function(view$break_positions_minor)) view$break_positions_minor() else numeric()
      result[[name]] <- list(axis = axis, secondary = j > 1L, discrete = isTRUE(view$is_discrete()),
        position = position[keep], labels = labels[keep], minor = minor[is.finite(minor) & minor >= 0 & minor <= 1],
        title = title, params = params, theme = theme + (params$theme %or3d% ggplot2::theme()))
    }
  }
  panel_params$axis_guides <- frame_of_3d(panel_params)$guides(result, panel_params)
  panel_params$panel_guides <- Filter(function(guide) inherits(guide, "Guide") && is.function(guide$draw_panel_3d), guides$guides)
  panel_params
}
