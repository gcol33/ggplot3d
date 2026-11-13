utils::globalVariables("geometry_octree_isosurface_cpp")

# Octree over the box `limits` for the level sets of fun (Plantinga and
# Vegter 2004). Boxes are evaluated in interval arithmetic one depth at a
# time. A box whose value interval contains no level is dropped (their
# condition C0). A box is split down to depth[1] in any case, and further, up
# to depth[2], while its gradient box fails the small normal variation test
# <grad f(B), grad f(B)> > 0 (their C1, which makes the level set inside a
# single sheet, monotone along any gradient), or while the box diagonal
# times r / |c| exceeds tol times the diagonal of the root box, where c and r
# are the centre and half diagonal of the gradient box: every gradient in
# the box lies within the angle asin(r / |c|) of c, so the level set
# deviates from a plane by about the box size times that ratio. Gradients are measured in
# box coordinates scaled to the unit cube. Returns the terminal boxes as
# lattice origins and sizes on 2^depth[2] intervals per axis, with a flag for
# boxes the level set cannot meet, and the number of boxes evaluated.
isofunction_octree_3d <- function(fun, args, limits, levels, depth, tol) {
  fun <- interval_function_3d(fun)
  top <- depth[2L]; lattice <- 2^top
  span <- vapply(limits, diff, numeric(1))
  corner <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  origin <- matrix(0, 1L, 3L)
  boxes <- list(); evaluated <- 0
  for (d in 0:top) {
    size <- 2^(top - d)
    bounds <- lapply(1:3, function(a) list(lo = limits[[a]][1L] + span[a] * origin[, a] / lattice,
      hi = limits[[a]][1L] + span[a] * (origin[, a] + size) / lattice))
    value <- interval_evaluate_3d(fun, args, bounds, span)
    evaluated <- evaluated + nrow(origin)
    meets <- Reduce(`|`, lapply(levels, function(l) value$lo <= l & value$hi >= l))
    meets <- meets %in% TRUE
    split <- meets & d < top
    if (d >= depth[1L] && d < top) {
      inner <- Reduce(`+`, lapply(value$d, function(g) bounds_mul_3d(g, g)$lo))
      centre <- sqrt(Reduce(`+`, lapply(value$d, function(g) ((g$lo + g$hi) / 2)^2)))
      radius <- sqrt(Reduce(`+`, lapply(value$d, function(g) ((g$hi - g$lo) / 2)^2)))
      settled <- is.finite(inner) & inner > 0 & is.finite(radius) & 2^-d * radius / centre <= tol
      split <- split & !settled
    }
    if (!all(split)) boxes[[length(boxes) + 1L]] <- cbind(origin[!split, , drop = FALSE], rep(size, sum(!split)), !meets[!split])
    if (!any(split)) break
    parents <- origin[split, , drop = FALSE]
    origin <- parents[rep(seq_len(nrow(parents)), each = 8L), , drop = FALSE] + corner[rep.int(1:8, nrow(parents)), , drop = FALSE] * size / 2
  }
  boxes <- do.call(rbind, boxes)
  list(boxes = matrix(as.integer(boxes[, 1:4]), ncol = 4L), empty = boxes[, 5L] == 1, evaluated = evaluated)
}

# Level surfaces of fun by dual contouring on an adaptive octree over the
# position scale box.
stats_adaptive_field_3d <- function(fun, args, scales, levels, caps, method, depth, tol, refine) {
  if (!is.logical(caps) || length(caps) != 1L || is.na(caps)) stop("caps must be TRUE or FALSE.", call. = FALSE)
  if (!method %in% c("dual", "nets")) stop('Adaptive extraction requires method = "dual" or "nets".', call. = FALSE)
  if (!is.numeric(depth) || length(depth) != 2L || any(!is.finite(depth) | depth != trunc(depth)) ||
      depth[1L] < 0 || depth[1L] > depth[2L] || depth[2L] > 10)
    stop("depth must be two integers 0 <= min <= max <= 10.", call. = FALSE)
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) stop("tol must be a positive number.", call. = FALSE)
  limits <- lapply(stats_grid_3d(scales, 2L, c("x", "y", "z")), range)
  f <- function_evaluator_3d(fun, args)
  if (is.null(levels)) levels <- stats_levels_3d(f(as.matrix(expand.grid(lapply(limits, function(r) seq(r[1L], r[2L], length.out = 9L))))), NULL)
  levels <- stats_levels_3d(numeric(), levels)
  tree <- isofunction_octree_3d(fun, args, limits, levels, depth, tol)
  crossing <- if (refine > 0L) function(a, b, fa, fb, level)
    a + (b - a) * regula_falsi_3d(a, b, fa - level, fb - level, function(p) f(p) - level, refine)
  out <- geometry_octree_isosurface_cpp(vapply(limits, `[`, 1, 1L), vapply(limits, `[`, 1, 2L), as.integer(depth[2L]),
    tree$boxes, tree$empty, levels, caps, method, f, crossing, function_gradient_3d(f, limits))
  isosurface_finish_3d(out, limits, NULL, f, 0L)
}
