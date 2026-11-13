utils::globalVariables("value")

# Bin arguments --------------------------------------------------------------

check_bin_args_3d <- function(bins, binwidth, axes) {
  n <- length(axes)
  if (!length(bins) %in% c(1L, n) || any(!is.finite(bins) | bins < 1 | bins != trunc(bins)))
    stop("bins must be positive integers, one value or one per axis.", call. = FALSE)
  if (!is.null(binwidth) && (!length(binwidth) %in% c(1L, n) || any(!is.finite(binwidth) | binwidth <= 0)))
    stop("binwidth must be positive, one value or one per axis.", call. = FALSE)
  list(bins = rep_len(as.integer(bins), n), binwidth = if (!is.null(binwidth)) rep_len(binwidth, n))
}

# Equal intervals across `range`, or intervals of `binwidth` whose edges fall on
# boundary + k * binwidth.
bin_breaks_3d <- function(range, bins, binwidth = NULL, boundary = NULL) {
  if (!is.null(binwidth)) {
    shift <- boundary %or3d% 0
    lo <- shift + floor((range[1L] - shift) / binwidth) * binwidth
    hi <- shift + ceiling((range[2L] - shift) / binwidth) * binwidth
    return(seq(lo, max(lo + binwidth, hi), by = binwidth))
  }
  if (diff(range) == 0) range <- range + c(-0.5, 0.5)
  seq(range[1L], range[2L], length.out = bins + 1L)
}

bin_scale_ranges_3d <- function(scales, axes, caller) {
  lapply(stats::setNames(axes, axes), function(axis) {
    scale <- stats_scale_3d(scales, axis)
    if (scale$is_discrete()) stop(caller, " requires continuous x and y positions.", call. = FALSE)
    range <- scale$dimension()
    if (any(!is.finite(range))) stop(caller, " requires finite x and y ranges.", call. = FALSE)
    range
  })
}

check_no_input_z_3d <- function(data, params, caller) {
  if (!is.null(data[["z"]])) stop(caller, " counts x/y observations; map z only for summaries.", call. = FALSE)
  params
}

# Cell lattices ----------------------------------------------------------------
#
# A lattice assigns every observation a cell id (NA outside the lattice) and
# describes cells by id: centre, extent, size and area.

rect_lattice_3d <- function(data, scales, bins = 30L, binwidth = NULL, breaks = NULL, boundary = NULL) {
  axes <- c("x", "y")
  args <- check_bin_args_3d(bins, binwidth, axes)
  ranges <- bin_scale_ranges_3d(scales, axes, "stat_bin_2d_3d()")
  if (!is.null(boundary) && (!length(boundary) %in% 1:2 || any(!is.finite(boundary))))
    stop("boundary must be finite, one value or one per axis.", call. = FALSE)
  boundary <- if (!is.null(boundary)) rep_len(boundary, 2L)
  if (!is.null(breaks) && !is.list(breaks)) breaks <- list(breaks, breaks)
  edges <- lapply(seq_along(axes), function(a) {
    if (is.null(breaks[[a]])) return(bin_breaks_3d(ranges[[a]], args$bins[a], args$binwidth[a], boundary[a]))
    b <- sort(unique(breaks[[a]][is.finite(breaks[[a]])]))
    if (length(b) < 2L) stop("breaks must contain at least two distinct finite values per axis.", call. = FALSE)
    b
  })
  n <- lengths(edges) - 1L
  index <- lapply(seq_along(axes), function(a) {
    i <- findInterval(data[[axes[a]]], edges[[a]], rightmost.closed = TRUE)
    i[i < 1L | i > n[a]] <- NA_integer_
    i
  })
  list(
    id = index[[1L]] + n[1L] * (index[[2L]] - 1L),
    size = prod(as.double(n)),
    cells = function(id) {
      i <- (id - 1L) %% n[1L] + 1L; j <- (id - 1L) %/% n[1L] + 1L
      x <- edges[[1L]]; y <- edges[[2L]]
      data.frame(x = (x[i] + x[i + 1L]) / 2, y = (y[j] + y[j + 1L]) / 2,
        xmin = x[i], xmax = x[i + 1L], ymin = y[j], ymax = y[j + 1L],
        width = x[i + 1L] - x[i], depth = y[j + 1L] - y[j],
        area = (x[i + 1L] - x[i]) * (y[j + 1L] - y[j]))
    }
  )
}

# Pointy-topped hexagons: centres binwidth[1] apart along each row, rows
# binwidth[2] * sqrt(3) / 2 apart with odd rows shifted half a cell. With x in
# units of binwidth[1] and y in units of binwidth[2] the cells are regular, and
# each observation goes to the nearer of the two rectangular sublattices'
# closest centres (Carr et al. 1987, as in hexbin).
hex_lattice_3d <- function(data, scales, bins = 30L, binwidth = NULL) {
  axes <- c("x", "y")
  args <- check_bin_args_3d(bins, binwidth, axes)
  ranges <- bin_scale_ranges_3d(scales, axes, "stat_bin_hex_3d()")
  bw <- args$binwidth %or3d% vapply(seq_along(axes), function(a) diff(ranges[[a]]) / args$bins[a], numeric(1))
  bw[bw <= 0] <- 1
  origin <- vapply(seq_along(axes), function(a) floor(ranges[[a]][1L] / bw[a]) * bw[a], numeric(1))
  rise <- bw[2L] * sqrt(3) / 2
  s <- (data$x - origin[1L]) / bw[1L]
  t <- (data$y - origin[2L]) / (2 * rise)
  ia <- round(s); ka <- round(t)
  ib <- floor(s); kb <- floor(t)
  odd <- (s - ib - 0.5)^2 + 3 * (t - kb - 0.5)^2 < (s - ia)^2 + 3 * (t - ka)^2
  column <- ifelse(odd, ib, ia)
  row <- ifelse(odd, 2 * kb + 1, 2 * ka)
  ncol <- floor((ranges$x[2L] - origin[1L]) / bw[1L] + 0.5) + 1
  nrow <- 2 * floor((ranges$y[2L] - origin[2L]) / (2 * rise) + 0.5) + 2
  inside <- column >= 0 & column < ncol & row >= 0 & row < nrow
  list(
    id = ifelse(inside, 1 + column + ncol * row, NA_real_),
    size = ncol * nrow,
    cells = function(id) {
      column <- (id - 1) %% ncol; row <- (id - 1) %/% ncol
      data.frame(x = origin[1L] + (column + 0.5 * (row %% 2)) * bw[1L], y = origin[2L] + row * rise,
        width = bw[1L], depth = bw[2L], area = bw[1L] * rise)
    }
  )
}

# Summaries over a lattice --------------------------------------------------------

bin_counts_3d <- function(lattice, weight, drop) {
  ok <- !is.na(lattice$id)
  if (!any(ok)) return(data.frame())
  sums <- rowsum(weight[ok], lattice$id[ok])
  occupied <- as.numeric(rownames(sums))
  if (!drop && lattice$size > 1e7) stop("drop = FALSE would return more than 10 million empty bins.", call. = FALSE)
  id <- if (drop) occupied else seq_len(lattice$size)
  count <- numeric(length(id))
  count[match(occupied, id)] <- sums[, 1L]
  out <- lattice$cells(id)
  area <- out$area
  out$area <- NULL
  out$count <- count
  total <- sum(count)
  out$density <- if (total != 0) count / total / area else 0
  out$ncount <- if (max(count) > 0) count / max(count) else 0
  out$ndensity <- if (max(out$density) > 0) out$density / max(out$density) else 0
  if (drop) out <- out[count != 0, , drop = FALSE]
  out
}

bin_summaries_3d <- function(lattice, z, fun, fun.args, drop) {
  fun <- match.fun(fun)
  ok <- !is.na(lattice$id)
  if (!any(ok)) return(data.frame())
  id <- sort(unique(lattice$id[ok]))
  groups <- split(z[ok], factor(lattice$id[ok], levels = id))
  value <- vapply(groups, function(v) {
    result <- do.call(fun, c(list(v), fun.args))
    if (length(result) != 1L || !is.numeric(result) && !is.logical(result))
      stop("fun must return a single number per bin.", call. = FALSE)
    as.double(result)
  }, numeric(1), USE.NAMES = FALSE)
  out <- lattice$cells(id)
  out$area <- NULL
  out$value <- value
  out$count <- as.double(lengths(groups, use.names = FALSE))
  out$z <- value
  if (drop) out <- out[!is.na(value), , drop = FALSE]
  out
}

# Stats ------------------------------------------------------------------------

StatBin2d3D <- ggplot2::ggproto("StatBin2d3D", ggplot2::Stat,
  required_aes = c("x", "y"), optional_aes = "weight", dropped_aes = "weight",
  default_aes = ggplot2::aes(z = ggplot2::after_stat(count)),
  setup_params = function(data, params) check_no_input_z_3d(data, params, "stat_bin_2d_3d()"),
  compute_group = function(data, scales, bins = 30L, binwidth = NULL, breaks = NULL, boundary = NULL, drop = TRUE) {
    bin_counts_3d(rect_lattice_3d(data, scales, bins, binwidth, breaks, boundary),
      data$weight %or3d% rep(1, nrow(data)), drop)
  }
)

StatSummary2d3D <- ggplot2::ggproto("StatSummary2d3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"), dropped_aes = "z",
  compute_group = function(data, scales, fun = "mean", fun.args = list(), bins = 30L, binwidth = NULL,
                           breaks = NULL, boundary = NULL, drop = TRUE) {
    bin_summaries_3d(rect_lattice_3d(data, scales, bins, binwidth, breaks, boundary), data$z, fun, fun.args, drop)
  }
)

StatBinHex3D <- ggplot2::ggproto("StatBinHex3D", ggplot2::Stat,
  required_aes = c("x", "y"), optional_aes = "weight", dropped_aes = "weight",
  default_aes = ggplot2::aes(z = ggplot2::after_stat(count)),
  setup_params = function(data, params) check_no_input_z_3d(data, params, "stat_bin_hex_3d()"),
  compute_group = function(data, scales, bins = 30L, binwidth = NULL, drop = TRUE) {
    bin_counts_3d(hex_lattice_3d(data, scales, bins, binwidth), data$weight %or3d% rep(1, nrow(data)), drop)
  }
)

StatSummaryHex3D <- ggplot2::ggproto("StatSummaryHex3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"), dropped_aes = "z",
  compute_group = function(data, scales, fun = "mean", fun.args = list(), bins = 30L, binwidth = NULL, drop = TRUE) {
    bin_summaries_3d(hex_lattice_3d(data, scales, bins, binwidth), data$z, fun, fun.args, drop)
  }
)

# Hexagonal prisms ---------------------------------------------------------------

# Default hexagon size from the centres: the spacing within a row is the width,
# the spacing between rows is sqrt(3) / 2 of the depth.
hex_size_3d <- function(x, y) {
  within <- unlist(lapply(split(x, y), function(v) diff(sort(unique(v[is.finite(v)])))), use.names = FALSE)
  within <- within[within > 0]
  width <- if (length(within)) min(within) else 2 * lattice_spacing_3d(x)
  rows <- lattice_spacing_3d(y, NA_real_)
  c(width = width, depth = if (is.na(rows)) width else 2 * rows / sqrt(3))
}

GeomHex3D <- ggplot2::ggproto("GeomHex3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"), default_aes = solid_defaults_3d(),
  optional_aes = c("zmin", "zmax"),
  draw_key = ggplot2::draw_key_polygon,
  setup_data = function(data, params) {
    for (size in c("width", "depth")) {
      value <- params[[size]]
      if (!is.null(value) && (length(value) != 1L || !is.finite(value) || value <= 0))
        stop("width and depth must be positive scalars.", call. = FALSE)
    }
    sizes <- lapply(split(data[c("x", "y")], data$PANEL), function(d) hex_size_3d(d$x, d$y))
    panel <- as.character(data$PANEL)
    data$width <- data$width %or3d% params$width %or3d% vapply(sizes, `[[`, numeric(1), "width")[panel]
    data$depth <- data$depth %or3d% params$depth %or3d% vapply(sizes, `[[`, numeric(1), "depth")[panel]
    data$xmin <- data$x - data$width / 2; data$xmax <- data$x + data$width / 2
    data$ymin <- data$y - data$depth / sqrt(3); data$ymax <- data$y + data$depth / sqrt(3)
    data
  }
)

# The bounding box carries the hexagon through position normalisation; the six
# corners sit at the box's left and right edges and its top and bottom centres.
emit_hexagons_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("x", "y", "z", "zmin", "xmin", "xmax", "ymin", "ymax")), , drop = FALSE]
  n <- nrow(data)
  if (!n) return(new_scene_packet(panel_id = panel_params$panel_id))
  rows <- rep(seq_len(n), each = 6L)
  corners <- data[rows, , drop = FALSE]
  half <- (data$xmax - data$xmin)[rows] / 2
  radius <- (data$ymax - data$ymin)[rows] / 2
  corners$x <- data$x[rows] + half * c(1, 0, -1, -1, 0, 1)
  corners$y <- data$y[rows] + radius * c(0.5, 1, 0.5, -0.5, -1, -0.5)
  corners$group <- rows
  corners$.feature <- rows
  emit_extrusions_3d(corners, params, panel_params)
}

# Hexagons stack along z only: stacking along x or y would stretch the bounding
# box that carries the hexagon's shape.
register_geom_3d(GeomHex3D, emit_hexagons_3d, prepare_aesthetics = prepare_solid_3d,
  positions = c("nudge", "stack"), tween = "row",
  extents = list(x = extent_3d("xmin", "xmax", "x"), y = extent_3d("ymin", "ymax", "y"),
                 z = extent_3d("zmin", "z")),
  position_check = function(position, scales) {
    if (identical(position_kind_3d(position), "stack") && !identical(position$axis, "z"))
      stop("Hexagonal prisms stack along z only.", call. = FALSE)
  })

# Constructors -------------------------------------------------------------------

#' Two-dimensional bins drawn as columns
#'
#' `stat_bin_2d_3d()` divides the x/y plane into rectangles and counts the
#' observations, optionally weighted, in each; the count becomes the height of
#' a column. `geom_histogram_3d()` is the same layer under the bar-chart name,
#' the three-dimensional form of a bivariate histogram. `stat_summary_2d_3d()`
#' bins the same way and sets each column's height to a summary of the mapped
#' `z` values in the cell, for example their mean.
#'
#' Bins span the trained x and y scale ranges. With `bins` the range of each
#' axis is divided into that many equal intervals. With `binwidth` the edges
#' fall on `boundary + k * binwidth` (by default on multiples of `binwidth`), so
#' bins line up across layers and panels. `breaks` gives the edges directly;
#' observations outside them are not counted. Every argument takes one value
#' for both axes or one per axis (`breaks`: a vector, or a list of two).
#' Columns touch, as histogram bars do; groups share the same bins, so
#' `position_stack_3d()` stacks them.
#'
#' @section Computed variables:
#' \describe{
#'   \item{`count`}{Number of observations in the bin (sum of weights).}
#'   \item{`density`}{Count divided by the total count and by the bin area, so
#'     the column volumes sum to one.}
#'   \item{`ncount`, `ndensity`}{Count and density divided by their maximum.}
#'   \item{`width`, `depth`}{Bin size along x and y.}
#'   \item{`value`}{Summaries only: the result of `fun`, also used as `z`.}
#' }
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param geom Geom used to draw the bins.
#' @param stat Stat that computes the bins.
#' @param bins Number of bins along x and y.
#' @param binwidth Bin widths along x and y; overrides `bins`.
#' @param breaks Bin edges; overrides `bins` and `binwidth`.
#' @param boundary Position of one bin edge when `binwidth` is given.
#' @param drop For counts, remove empty bins; for summaries, remove bins whose
#'   summary is `NA`.
#' @param fun Summary function, or its name, applied to the `z` values of each
#'   bin; it must return one number.
#' @param fun.args List of further arguments to `fun`.
#' @param base Baseline of the columns in original z units.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @seealso [stat_bin_hex_3d()] for hexagonal bins, [stat_count_3d()] for
#'   counts at discrete x/y combinations, [stat_bin_3d()] for counts in boxes.
#' @examples
#' p <- ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting))
#' p + geom_histogram_3d(binwidth = c(0.5, 5)) + coord_3d()
#' p + stat_bin_2d_3d(ggplot2::aes(fill = ggplot2::after_stat(density)), bins = 12) +
#'   coord_3d()
#' set.seed(1)
#' d <- data.frame(x = runif(400), y = runif(400))
#' d$z <- sin(3 * d$x) + d$y + rnorm(400, sd = 0.1)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_summary_2d_3d(fun = "median", bins = 8, fill = "steelblue") + coord_3d()
#' @export
stat_bin_2d_3d <- function(mapping = NULL, data = NULL, geom = GeomCol3D, position = "identity", ...,
                           bins = 30L, binwidth = NULL, breaks = NULL, boundary = NULL, drop = TRUE,
                           base = 0, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatBin2d3D, position,
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, boundary = boundary, drop = drop,
         base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_bin_2d_3d
#' @export
geom_histogram_3d <- function(mapping = NULL, data = NULL, stat = StatBin2d3D, position = "identity", ...,
                              bins = 30L, binwidth = NULL, breaks = NULL, boundary = NULL, drop = TRUE,
                              base = 0, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomCol3D, mapping, data, stat, position,
    c(list(bins = bins, binwidth = binwidth, breaks = breaks, boundary = boundary, drop = drop,
         base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_bin_2d_3d
#' @export
stat_summary_2d_3d <- function(mapping = NULL, data = NULL, geom = GeomCol3D, position = "identity", ...,
                               fun = "mean", fun.args = list(), bins = 30L, binwidth = NULL, breaks = NULL,
                               boundary = NULL, drop = TRUE, base = 0, shade = TRUE,
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSummary2d3D, position,
    c(list(fun = fun, fun.args = fun.args, bins = bins, binwidth = binwidth, breaks = breaks, boundary = boundary,
         drop = drop, base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' Hexagonal bins drawn as prisms
#'
#' `geom_hex_3d()` draws one upright hexagonal prism per row, centred on x and
#' y and extending from the baseline (or `zmin`) to `z`. By default it counts
#' observations in hexagonal bins with `stat_bin_hex_3d()`, the
#' three-dimensional form of [ggplot2::geom_hex()] and of the extruded
#' hexagon layers of deck.gl. `stat_summary_hex_3d()` sets each prism's height
#' to a summary of the mapped `z` values in the hexagon.
#'
#' Hexagons have pointed tops. Centres lie `binwidth[1]` apart along a row and
#' rows lie `binwidth[2] * sqrt(3) / 2` apart, every second row shifted by half
#' a hexagon, so the cells are regular when x and y are drawn at the scale of
#' their bin widths; the lattice is anchored at multiples of the bin widths, so
#' groups and panels share it. The default `bins = 30` sets the bin widths to a
#' thirtieth of the x and y ranges, as in ggplot2. Each observation goes to the
#' hexagon whose centre is nearest in units of the bin widths.
#'
#' With `stat = "identity"`, the hexagon size comes from `width` and `depth`
#' (columns or arguments): `width` is the distance between the flat sides,
#' along x, and `depth` the row spacing divided by `sqrt(3) / 2`, along y.
#' Without them the size follows from the spacing of the centres.
#'
#' @inheritSection stat_bin_2d_3d Computed variables
#' @inheritParams stat_bin_2d_3d
#' @param width,depth Hexagon size along x and y; see Details.
#' @param cap Caps to draw: both, top, bottom or none.
#' @return A ggplot2 layer.
#' @references Carr, D. B., Littlefield, R. J., Nicholson, W. L. and
#'   Littlefield, J. S. (1987). Scatterplot matrix techniques for large N.
#'   Journal of the American Statistical Association 82, 424-436.
#' @seealso [stat_bin_2d_3d()] for rectangular bins.
#' @examples
#' p <- ggplot2::ggplot(faithful, ggplot2::aes(eruptions, waiting))
#' p + geom_hex_3d(bins = 12) + coord_3d()
#' p + geom_hex_3d(ggplot2::aes(fill = ggplot2::after_stat(count)),
#'   binwidth = c(0.4, 5), colour = "grey20") + coord_3d()
#' set.seed(1)
#' d <- data.frame(x = rnorm(500), y = rnorm(500))
#' d$z <- d$x^2 + d$y^2
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_summary_hex_3d(bins = 10, fill = "darkorange") + coord_3d()
#' @export
geom_hex_3d <- function(mapping = NULL, data = NULL, stat = StatBinHex3D, position = "identity", ...,
                        width = NULL, depth = NULL, base = 0, cap = "both", shade = TRUE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  cap <- match.arg(cap, c("both", "top", "bottom", "none"))
  make_layer_3d(GeomHex3D, mapping, data, stat, position,
    c(list(width = width, depth = depth, base = base, cap = cap, shade = shade, na.rm = na.rm),
    list(...)), show.legend, inherit.aes)
}

#' @rdname geom_hex_3d
#' @export
stat_bin_hex_3d <- function(mapping = NULL, data = NULL, geom = GeomHex3D, position = "identity", ...,
                            bins = 30L, binwidth = NULL, drop = TRUE, base = 0, shade = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatBinHex3D, position,
    c(list(bins = bins, binwidth = binwidth, drop = drop, base = base, shade = shade, na.rm = na.rm),
    list(...)), show.legend, inherit.aes)
}

#' @rdname geom_hex_3d
#' @export
stat_summary_hex_3d <- function(mapping = NULL, data = NULL, geom = GeomHex3D, position = "identity", ...,
                                fun = "mean", fun.args = list(), bins = 30L, binwidth = NULL, drop = TRUE,
                                base = 0, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSummaryHex3D, position,
    c(list(fun = fun, fun.args = fun.args, bins = bins, binwidth = binwidth, drop = drop,
         base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
