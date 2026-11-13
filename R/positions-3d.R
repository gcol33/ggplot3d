position_families_3d <- function() {
  list(x = c("x", "xmin", "xmax", "xend", "xintercept"),
       y = c("y", "ymin", "ymax", "yend", "yintercept"), z = z_aesthetics)
}

shift_axis_3d <- function(data, axis, offset) {
  for (column in intersect(position_families_3d()[[axis]], names(data))) data[[column]] <- data[[column]] + offset
  data
}

# Smallest positive gap between distinct finite values; 1 for a single value.
lattice_spacing_3d <- function(v) {
  d <- diff(sort(unique(v[is.finite(v)])))
  if (length(d)) min(d[d > 0]) else 1
}

default_extents_3d <- function() {
  list(x = extent_3d(upper = "x", centre = "x"), y = extent_3d(upper = "y", centre = "y"),
       z = extent_3d(upper = "z", centre = "z"))
}

resolve_extents_3d <- function(extents) utils::modifyList(default_extents_3d(), extents %or3d% list())

present_3d <- function(data, columns) intersect(columns[!is.na(columns)], names(data))

# Rows sharing every key column form one cell.
cells_3d <- function(data, keys) {
  keys <- intersect(c("PANEL", keys), names(data))
  if (!length(keys)) return(factor(rep(1L, nrow(data))))
  interaction(data[keys], drop = TRUE, lex.order = TRUE)
}

# Cumulative intervals per cell along one axis; positive and negative values
# stack apart from one another, as in ggplot2::position_stack().
stack_extents_3d <- function(data, axis, extents, vjust, reverse, fill) {
  spec <- extents[[axis]]
  lower <- spec[["lower"]]; upper <- spec[["upper"]]; centre <- spec[["centre"]]
  base <- if (!is.na(lower) && !is.null(data[[lower]])) data[[lower]] else rep(0, nrow(data))
  if (any(base != 0, na.rm = TRUE)) stop("Stacking along ", axis, " requires a shared zero baseline.", call. = FALSE)
  value <- data[[upper]] %or3d% data[[centre]]
  if (is.null(value)) stop("Stacking along ", axis, " requires ", upper, ".", call. = FALSE)
  keys <- unlist(lapply(extents[setdiff(c("x", "y", "z"), axis)], present_3d, data = data), use.names = FALSE)
  cells <- cells_3d(data, unique(keys))
  order <- order(cells, if (reverse) data$group else -data$group)
  lo <- hi <- rep(NA_real_, nrow(data))
  for (rows in split(order, cells[order])) {
    for (part in list(rows[which(value[rows] >= 0)], rows[which(value[rows] < 0)])) {
      if (!length(part)) next
      top <- cumsum(value[part])
      total <- abs(top[length(top)])
      if (fill && total > 0) top <- top / total
      lo[part] <- c(0, top[-length(top)])
      hi[part] <- top
    }
  }
  if (!is.na(lower)) data[[lower]] <- lo
  data[[upper]] <- hi
  if (!is.na(centre)) data[[centre]] <- lo + vjust * (hi - lo)
  data
}

# Side-by-side placement of the groups sharing an x/y cell, centred on the
# cell as in ggplot2::position_dodge2().
dodge_extents_3d <- function(data, axis, extents, width, preserve, reverse) {
  spec <- extents[[axis]]
  bounds <- present_3d(data, spec[c("lower", "upper")])
  has_extent <- length(bounds) == 2L
  anchor <- data[[present_3d(data, spec[c("centre", "upper")])[1L]]]
  position <- if (has_extent) (data[[bounds[1L]]] + data[[bounds[2L]]]) / 2 else anchor
  size <- if (has_extent) data[[bounds[2L]]] - data[[bounds[1L]]] else rep(0, nrow(data))
  keys <- unlist(lapply(extents[c("x", "y")], present_3d, data = data), use.names = FALSE)
  cells <- cells_3d(data, unique(keys))
  rank <- stats::ave(data$group, cells, FUN = function(g) match(g, sort(unique(g))))
  count <- stats::ave(data$group, cells, FUN = function(g) length(unique(g)))
  n <- if (preserve == "single") max(count) else count
  if (reverse) rank <- count + 1L - rank
  span <- width
  if (is.null(span) && has_extent) span <- stats::ave(size, cells, FUN = function(s) max(s, na.rm = TRUE))
  if (is.null(span)) span <- 0.9 * lattice_spacing_3d(position)
  offset <- span / n * (rank - (count + 1) / 2)
  data <- shift_axis_3d(data, axis, offset)
  if (has_extent) {
    centre <- position + offset
    data[[bounds[1L]]] <- centre - size / n / 2
    data[[bounds[2L]]] <- centre + size / n / 2
  }
  data
}

with_seed_3d <- function(seed, code) {
  if (is.null(seed)) return(code)
  env <- globalenv()
  old <- if (exists(".Random.seed", envir = env, inherits = FALSE)) get(".Random.seed", envir = env) else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = env) else assign(".Random.seed", old, envir = env), add = TRUE)
  set.seed(seed)
  code
}

Position3D <- ggplot2::ggproto("Position3D", ggplot2::Position,
  kind_3d = "identity",
  extents = NULL
)

check_axis_3d <- function(axis, choices) {
  if (!is.character(axis) || length(axis) != 1L || !axis %in% choices) {
    stop("axis must be one of ", paste0('"', choices, '"', collapse = ", "), ".", call. = FALSE)
  }
  axis
}

check_flag_3d <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) stop(name, " must be TRUE or FALSE.", call. = FALSE)
  value
}

check_amount_3d <- function(value, name) {
  if (!is.null(value) && (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0)) {
    stop(name, " must be NULL or a non-negative number.", call. = FALSE)
  }
  value
}

#' Stack or fill intervals along an axis
#'
#' `position_stack_3d()` places the intervals of groups that share a cell end
#' to end along `axis`; `position_fill_3d()` also rescales each stack to span
#' 0 to 1. Positive and negative values stack separately. A cell is the set of
#' rows with the same position on the two other axes.
#'
#' Columns stack along z from a zero baseline. They stack along x or y when
#' `xmin = 0` and `xmax` (or `ymin`, `ymax`) give each column's length. Bands
#' and walls stack along the axis on which they span from `x`, `y` or `z` to
#' the matching `*end`; groups stack where they share vertices. Points, paths,
#' text and drop lines take their coordinate on `axis` as the length, so
#' labels can follow stacked columns. Stacking happens on the transformed
#' scale and needs a linear continuous scale on `axis`.
#' @param axis Axis to stack along: `"z"`, `"x"` or `"y"`.
#' @param vjust Fraction of each interval at which the anchor (`z` for
#'   columns, the coordinate of points and text) is placed.
#' @param reverse Reverse the stacking order of the groups. By default the
#'   first group ends on top, as in [ggplot2::position_stack()].
#' @return A ggplot2 position adjustment.
#' @seealso [ggplot2::position_stack()], [position_dodge_3d()].
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:2, part = c("a", "b", "c"))
#' d$z <- c(1:6, 6:1, rep(2, 6))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
#'   geom_col_3d(position = position_stack_3d()) + coord_3d()
#'
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
#'   geom_col_3d(position = position_fill_3d()) + coord_3d()
#'
#' # Stacked walls: one stacked area per y slice
#' w <- expand.grid(x = seq(0, 6, by = 0.25), y = 1:2, part = c("a", "b"))
#' w$h <- 1 + sin(w$x + as.integer(w$part)) / 2
#' ggplot2::ggplot(w, ggplot2::aes(x, y, zmin = 0, zmax = h, fill = part,
#'                                 group = interaction(part, y))) +
#'   geom_band_3d(position = position_stack_3d()) + coord_3d()
#' @export
position_stack_3d <- function(axis = "z", vjust = 1, reverse = FALSE) {
  stack_position_3d(axis, vjust, reverse, fill = FALSE)
}

#' @rdname position_stack_3d
#' @export
position_fill_3d <- function(axis = "z", vjust = 1, reverse = FALSE) {
  stack_position_3d(axis, vjust, reverse, fill = TRUE)
}

stack_position_3d <- function(axis, vjust, reverse, fill) {
  axis <- check_axis_3d(axis, c("z", "x", "y"))
  if (!is.numeric(vjust) || length(vjust) != 1L || !is.finite(vjust)) stop("vjust must be a finite number.", call. = FALSE)
  ggplot2::ggproto(NULL, if (fill) PositionFill3D else PositionStack3D,
    axis = axis, vjust = vjust, reverse = check_flag_3d(reverse, "reverse"))
}

PositionStack3D <- ggplot2::ggproto("PositionStack3D", Position3D,
  kind_3d = "stack",
  fill = FALSE,
  setup_params = function(self, data) {
    list(axis = self$axis, vjust = self$vjust, reverse = self$reverse, fill = self$fill,
         extents = resolve_extents_3d(self$extents))
  },
  compute_panel = function(data, params, scales) {
    stack_extents_3d(data, params$axis, params$extents, params$vjust, params$reverse, params$fill)
  }
)

PositionFill3D <- ggplot2::ggproto("PositionFill3D", PositionStack3D, fill = TRUE)

#' Dodge groups side by side within a cell
#'
#' Groups that share an x/y cell are placed next to one another along `axis`.
#' Columns become narrower so that the dodged set fills the original cell;
#' points, text and segments move apart by `width / n` per group.
#' @param axis Axis to dodge along: `"x"` or `"y"`.
#' @param width Total width taken by the dodged groups in transformed units.
#'   Defaults to the width of the columns in each cell, or to 0.9 times the
#'   lattice spacing for geometries without a width.
#' @param preserve `"total"` shares each cell among the groups present in it;
#'   `"single"` gives every group the width it has in the fullest cell.
#' @param reverse Reverse the order of the groups.
#' @return A ggplot2 position adjustment.
#' @seealso [ggplot2::position_dodge()], [ggplot2::position_dodge2()].
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:2, part = c("a", "b"))
#' d$z <- seq_len(nrow(d))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
#'   geom_col_3d(position = position_dodge_3d()) + coord_3d()
#'
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
#'   geom_col_3d(position = position_dodge_3d(axis = "y")) + coord_3d()
#' @export
position_dodge_3d <- function(axis = "x", width = NULL, preserve = "total", reverse = FALSE) {
  preserve <- match.arg(preserve, c("total", "single"))
  if (!is.null(width) && (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0)) {
    stop("width must be NULL or a positive number.", call. = FALSE)
  }
  ggplot2::ggproto(NULL, PositionDodge3D, axis = check_axis_3d(axis, c("x", "y")), width = width,
    preserve = preserve, reverse = check_flag_3d(reverse, "reverse"))
}

PositionDodge3D <- ggplot2::ggproto("PositionDodge3D", Position3D,
  kind_3d = "dodge",
  setup_params = function(self, data) {
    list(axis = self$axis, width = self$width, preserve = self$preserve, reverse = self$reverse,
         extents = resolve_extents_3d(self$extents))
  },
  compute_layer = function(data, params, layout) {
    dodge_extents_3d(data, params$axis, params$extents, params$width, params$preserve, params$reverse)
  }
)

#' Jitter points in three dimensions
#'
#' Adds uniform random noise to the x, y and z positions of each row. Every
#' column of one position family (for example `z`, `zmin`, `zend`) moves by
#' the same amount, so segments and columns keep their shape.
#' @param width,depth,height Amount of jitter along x, y and z in transformed
#'   units: positions move by up to this amount in either direction. Defaults
#'   to 40% of the resolution of the data along that axis, as in
#'   [ggplot2::position_jitter()].
#' @param seed Random seed. `NA` (default) draws one seed when the position is
#'   created, so the jitter is the same each time the plot is drawn; `NULL`
#'   uses the current random state.
#' @return A ggplot2 position adjustment.
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:3, rep = 1:10)
#' d$z <- d$x + d$y
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(position = position_jitter_3d(seed = 1)) + coord_3d()
#' @export
position_jitter_3d <- function(width = NULL, depth = NULL, height = NULL, seed = NA) {
  if (!is.null(seed) && !(length(seed) == 1L && (is.na(seed) || is.numeric(seed)))) {
    stop("seed must be NULL, NA or a single number.", call. = FALSE)
  }
  if (!is.null(seed) && is.na(seed)) seed <- sample.int(.Machine$integer.max, 1L)
  ggplot2::ggproto(NULL, PositionJitter3D, width = check_amount_3d(width, "width"),
    depth = check_amount_3d(depth, "depth"), height = check_amount_3d(height, "height"), seed = seed)
}

PositionJitter3D <- ggplot2::ggproto("PositionJitter3D", Position3D,
  kind_3d = "jitter",
  setup_params = function(self, data) {
    amount <- function(value, column) {
      value %or3d% if (is.numeric(data[[column]])) 0.4 * ggplot2::resolution(data[[column]], zero = FALSE, TRUE) else 0
    }
    list(amount = c(x = amount(self$width, "x"), y = amount(self$depth, "y"), z = amount(self$height, "z")),
         seed = self$seed)
  },
  compute_panel = function(data, params, scales) {
    noise <- with_seed_3d(params$seed, lapply(params$amount, function(a) stats::runif(nrow(data), -a, a)))
    for (axis in names(noise)) {
      columns <- present_3d(data, position_families_3d()[[axis]])
      offset <- noise[[axis]]
      if (length(columns)) offset[is.infinite(data[[columns[1L]]])] <- 0
      data <- shift_axis_3d(data, axis, offset)
    }
    data
  }
)

#' Nudge all three position families
#' @param x,y,z Displacements in transformed coordinate units.
#' @param nudge_z Optional spelling of the z displacement.
#' @return A ggplot2 position adjustment.
#' @examples
#' ggplot2::ggplot(data.frame(x = 1, y = 1, z = 1), ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(position = position_nudge_3d(nudge_z = 0.5)) + coord_3d()
#' @export
position_nudge_3d <- function(x = 0, y = 0, z = 0, nudge_z = NULL) {
  if (!is.null(nudge_z)) {
    if (!missing(z)) stop("Supply only one of z and nudge_z.", call. = FALSE)
    z <- nudge_z
  }
  stopifnot(all(vapply(list(x, y, z), function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1))))
  ggplot2::ggproto(NULL, PositionNudge3D, x = x, y = y, z = z)
}

PositionNudge3D <- ggplot2::ggproto("PositionNudge3D", ggplot2::PositionNudge,
  kind_3d = "nudge",
  setup_params = function(self, data) list(x = self$x, y = self$y, z = self$z),
  compute_layer = function(data, params, layout) {
    for (axis in names(position_families_3d())) data <- shift_axis_3d(data, axis, params[[axis]])
    data
  }
)

planar_positions_3d <- c("PositionNudge", "PositionJitter", "PositionDodge", "PositionDodge2", "PositionJitterdodge")

position_kind_3d <- function(position) {
  if (!is.null(position$kind_3d)) return(position$kind_3d)
  if (inherits(position, "PositionIdentity")) return("identity")
  if (inherits(position, planar_positions_3d)) return("planar")
  NA_character_
}

check_position_kind_3d <- function(position, entry) {
  kind <- position_kind_3d(position)
  if (identical(kind, "identity")) return(invisible(position))
  if (is.na(kind) || !kind %in% (entry$positions %or3d% "nudge")) {
    stop("Unsupported 3D position for ", entry$name, ": ", class(position)[1L], ".", call. = FALSE)
  }
  invisible(position)
}

check_position_scales_3d <- function(position, scales) {
  if (!identical(position_kind_3d(position), "stack")) return(invisible())
  scale <- scales[[position$axis]]
  if (is.null(scale) || scale$is_discrete() || !identical(scale$get_transformation()$name, "identity")) {
    stop("Stacking along ", position$axis, " requires a linear continuous ", position$axis, " scale.", call. = FALSE)
  }
  invisible()
}

# Hands a 3D position the emitter's extent columns.
bind_position_3d <- function(position, entry) {
  if (is.null(position$kind_3d) || identical(position$kind_3d, "nudge")) return(position)
  ggplot2::ggproto(NULL, position, extents = entry$extents)
}
