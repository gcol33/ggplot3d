utils::globalVariables(c("streamlines_cpp", "norm"))

streamline_field_3d <- function(data) {
  axes <- lapply(data[c("x", "y", "z")], function(x) sort(unique(as.double(x))))
  if (any(lengths(axes) < 2L)) stop("A vector field needs at least two coordinates per axis.", call. = FALSE)
  sizes <- lengths(axes)
  index <- lapply(seq_len(3L), function(i) match(data[[c("x", "y", "z")[i]]], axes[[i]]) - 1L)
  key <- 1 + index[[1L]] + sizes[1L] * (index[[2L]] + sizes[2L] * index[[3L]])
  if (length(key) != prod(sizes) || anyDuplicated(key)) stop("The vector field must contain each x/y/z grid cell exactly once.", call. = FALSE)
  list(axes = axes, values = as.matrix(data[match(seq_len(prod(sizes)), key), c("u", "v", "w")]))
}

streamline_seeds_3d <- function(seeds, axes, scales, n) {
  transform <- function(x) {
    x <- line_rows_3d(x, "seeds")
    for (i in seq_len(3L)) x[, i] <- stats_scale_3d(scales, c("x", "y", "z")[i])$transform(x[, i])
    x
  }
  centre <- vapply(axes, function(x) mean(range(x)), numeric(1))
  if (is.matrix(seeds) || is.data.frame(seeds) || is.numeric(seeds)) {
    return(list(points = list(transform(seeds)), slice = -1L, spacing = 0, dtest = 0))
  }
  seeds <- seeds %or3d% list(plane = "yz")
  if (!is.list(seeds)) stop("seeds must be points or a seeding specification.", call. = FALSE)
  if (!is.null(seeds$from) && !is.null(seeds$to)) {
    ends <- transform(rbind(seeds$from, seeds$to))
    n <- mesh_count_3d(seeds$n %or3d% n[1L], "n", 2L)
    fraction <- seq(0, 1, length.out = n)
    return(list(points = list(outer(1 - fraction, ends[1L, ]) + outer(fraction, ends[2L, ])),
      slice = -1L, spacing = 0, dtest = 0))
  }
  plane <- match.arg(seeds$plane %or3d% "xy", c("xy", "xz", "yz"))
  normal <- match(setdiff(c("x", "y", "z"), strsplit(plane, "")[[1L]]), c("x", "y", "z"))
  at <- seeds$at
  if (is.null(at)) at <- centre[normal] else at <- stats_scale_3d(scales, c("x", "y", "z")[normal])$transform(at)
  if (!is.numeric(at) || !length(at) || any(!is.finite(at))) stop("Slice positions must be finite.", call. = FALSE)
  if (identical(seeds$type, "even")) {
    spacing <- seeds$spacing %or3d% (min(vapply(axes[-normal], function(x) diff(range(x)), numeric(1))) / 10)
    dtest <- seeds$dtest %or3d% (spacing / 2)
    if (!is.numeric(spacing) || length(spacing) != 1L || !is.finite(spacing) || spacing <= 0 ||
        !is.numeric(dtest) || length(dtest) != 1L || !is.finite(dtest) || dtest <= 0 || dtest > spacing) stop("spacing must be positive and dtest must lie in (0, spacing].", call. = FALSE)
    points <- lapply(at, function(value) { p <- matrix(centre, 1L); p[, normal] <- value; p })
    return(list(points = points, slice = as.integer(normal - 1L), spacing = spacing, dtest = dtest))
  }
  n <- rep_len(seeds$n %or3d% n, 2L)
  n <- vapply(n, function(value) mesh_count_3d(value, "n", 2L), integer(1))
  in_plane <- setdiff(seq_len(3L), normal)
  grid <- expand.grid(lapply(seq_len(2L), function(i) seq(min(axes[[in_plane[i]]]), max(axes[[in_plane[i]]]), length.out = n[i])))
  points <- lapply(at, function(value) {
    p <- matrix(rep(centre, each = nrow(grid)), ncol = 3L)
    p[, in_plane] <- as.matrix(grid);p[, normal] <- value;p
  })
  list(points = points, slice = -1L, spacing = 0, dtest = 0)
}

StatStreamline3D <- ggplot2::ggproto("StatStreamline3D", StatGeometry3D,
  required_aes = c("x", "y", "z"), optional_aes = c("u", "v", "w"),
  dropped_aes = c(StatGeometry3D$dropped_aes, "u", "v", "w"),
  setup_params = function(data, params) {
    if (!all(c("u", "v", "w") %in% names(data)) || !all(vapply(data[c("u", "v", "w")], is.numeric, logical(1)))) stop("Streamlines need numeric u, v and w components.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, seeds = NULL, n = 6L, step = 0.05, method = "rk4",
                           max_steps = 1000L, direction = "both", tolerance = 1e-5, max_lines = 200L,
                           tube = FALSE, radius = NULL, sides = 12L, cap = "round") {
    if (any(vapply(c("x", "y", "z"), function(a) stats_scale_3d(scales, a)$is_discrete(), logical(1)))) stop("Vector fields require continuous position scales.", call. = FALSE)
    field <- streamline_field_3d(data)
    starts <- streamline_seeds_3d(seeds, field$axes, scales, n)
    pieces <- lapply(starts$points, function(points) streamlines_cpp(field$axes, field$values, points,
      step, max_steps, method, direction, tolerance, starts$spacing, starts$dtest, starts$slice, max_lines))
    shift <- 0L
    for (i in seq_along(pieces)) if (nrow(pieces[[i]])) {
      pieces[[i]]$group <- pieces[[i]]$group + shift
      shift <- max(pieces[[i]]$group)
    }
    out <- do.call(rbind, pieces)
    if (is.null(out) || !nrow(out)) return(data.frame())
    if (!tube) return(out)
    surfaces <- lapply(split(out, out$group), function(path) StatSweep3D$compute_group(path, scales,
      radius = radius, sides = sides, cap = cap))
    stats_bind_faces_3d(surfaces)
  }
)

#' Integrate streamlines and streamtubes through a vector field
#'
#' A complete rectilinear x/y/z grid supplies u/v/w components. Trilinear
#' interpolation and integration run in C++. Integration solves dp/dt = v(p)
#' in transformed coordinate units; supply components in those units when
#' using nonlinear position scales. Missing field cells stop a streamline.
#' @param mapping,data,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param geom `"path"` for lines or `"tube"` for rotation-minimising streamtubes.
#' @param seeds Numeric x/y/z rows in original position units; a list with
#'   `from`, `to`, `n` for a line rake; or a list with `plane` (`"xy"`, `"xz"`,
#'   `"yz"`), `at` and `n` for a plane. NULL seeds a yz plane at the x midpoint.
#'   `list(type = "even", plane = "xy", at = ..., spacing = ..., dtest = ...)`
#'   uses Jobard-Lefer placement of streamlines of the projected field in
#'   each slice. The normal component is zero in this mode. Candidate seeds
#'   are `spacing` from accepted curves; tracing stops within `dtest`
#'   (default half the spacing). Sampling steps stay below half `dtest`.
#' @param n Samples along a rake or each plane axis; one or two integers.
#' @param step Maximum integration time step, default 0.05.
#' @param method Fixed-step `"rk4"` or adaptive Dormand-Prince `"rk45"`.
#' @param max_steps Maximum accepted steps in each direction from a seed.
#' @param direction `"both"`, `"forward"` or `"backward"`.
#' @param tolerance RK45 local error tolerance relative to each axis span.
#' @param max_lines Maximum curves per plane or explicit seed collection.
#' @param radius,sides,cap Passed to [geom_tube_3d()]'s sweep machinery.
#' @return A ggplot2 layer with computed `norm`, `step`, `arclength`, `time`
#'   and `group`. Streamtubes retain face means of these variables.
#' @references Dormand and Prince (1980), A family of embedded Runge-Kutta
#'   formulae. Jobard and Lefer (1997), Creating evenly-spaced streamlines of
#'   arbitrary density, \doi{10.1007/978-3-7091-6876-9_5}.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 5),
#'                  y = seq(-1, 1, length.out = 5), z = c(-1, 1))
#' d$u <- -d$y; d$v <- d$x; d$w <- 0.15
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w,
#'                                colour = ggplot2::after_stat(norm))) +
#'   stat_streamline_3d(seeds = rbind(c(0.6, 0, -0.8)), max_steps = 80) + coord_3d()
#' @export
stat_streamline_3d <- function(mapping = NULL, data = NULL, geom = c("path", "tube"), position = "identity", ...,
                                seeds = NULL, n = 6L, step = 0.05, method = c("rk4", "rk45"),
                                max_steps = 1000L, direction = c("both", "forward", "backward"),
                                tolerance = 1e-5, max_lines = 200L, radius = NULL, sides = 12L, cap = "round",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom <- match.arg(geom)
  for (name in c("step", "tolerance")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0) stop(name, " must be positive.", call. = FALSE)
  }
  make_layer_3d(if (geom == "tube") GeomStatsSurface3D else ggplot2::GeomPath, mapping, data,
    StatStreamline3D, position, stats_params_3d(list(seeds = seeds, n = n, step = step,
      method = match.arg(method), max_steps = mesh_count_3d(max_steps, "max_steps", 1L),
      direction = match.arg(direction), tolerance = tolerance, max_lines = mesh_count_3d(max_lines, "max_lines", 1L),
      tube = geom == "tube", radius = radius, sides = sides, cap = cap, shade = geom == "tube", na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}
