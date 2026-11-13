# Signed distance functions as expression trees. A node holds an operator
# name, its numeric parameters and its operand nodes; the operator registry
# gives each name one evaluation rule written in generic arithmetic, so the
# same rule evaluates on numbers, on intervals with interval gradients
# (R/stats-interval.R) and, through degenerate intervals, on points with exact
# gradients (forward-mode differentiation). Each node also carries a
# Lipschitz constant of its value and whether the value is the exact signed
# Euclidean distance (Quilez, distance functions): a distance bound with
# Lipschitz constant L lets a sphere tracer step value / L along a ray.

sdf_ops_3d <- new.env(parent = emptyenv())

# eval(params, p, child) returns the value at coordinates p, a list of x, y
# and z (numbers or intervals); child(i, q) evaluates operand i at q.
# lipschitz(params, L) and exact(params, exact) combine the operands'
# Lipschitz constants and exactness flags.
sdf_register_3d <- function(op, eval, lipschitz = function(params, L) max(c(L, 0)),
                            exact = function(params, exact) FALSE) {
  assign(op, list(eval = eval, lipschitz = lipschitz, exact = exact), envir = sdf_ops_3d)
}

sdf_node_3d <- function(x) {
  if (!inherits(x, "ggplot3d_sdf")) stop("Operands must be ggplot3d_sdf objects.", call. = FALSE)
  get("node", envir = environment(x))
}

new_sdf_3d <- function(op, params = list(), children = list()) {
  entry <- sdf_ops_3d[[op]]
  nodes <- lapply(children, sdf_node_3d)
  node <- list(op = op, params = params, children = nodes,
    lipschitz = entry$lipschitz(params, vapply(nodes, `[[`, numeric(1), "lipschitz")),
    exact = entry$exact(params, vapply(nodes, `[[`, logical(1), "exact")))
  structure(function(x, y, z, ...) sdf_eval_3d(node, list(x, y, z)), class = c("ggplot3d_sdf", "function"))
}

sdf_eval_3d <- function(node, p) {
  sdf_ops_3d[[node$op]]$eval(node$params, p, function(i, q = p) sdf_eval_3d(node$children[[i]], q))
}

# Gradient of an SDF at the rows of an n x 3 matrix: the derivative part of
# the evaluation on degenerate intervals [p, p] seeded with unit derivatives.
sdf_gradient_3d <- function(sdf, p) {
  axis <- function(a) {
    d <- rep(list(list(lo = 0, hi = 0)), 3L); d[[a]] <- list(lo = 1, hi = 1)
    interval_3d(p[, a], p[, a], d)
  }
  out <- sdf_eval_3d(sdf_node_3d(sdf), list(axis(1L), axis(2L), axis(3L)))
  g <- matrix(0, nrow(p), 3L)
  if (inherits(out, "ggplot3d_interval") && !is.null(out$d))
    for (a in 1:3) g[, a] <- rep_len((out$d[[a]]$lo + out$d[[a]]$hi) / 2, nrow(p))
  g
}

# Arithmetic shared by the operator rules; every helper accepts numbers and
# intervals.
sdf_max_3d <- function(a, b) interval_extreme_3d(list(a, b), TRUE)
sdf_min_3d <- function(a, b) interval_extreme_3d(list(a, b), FALSE)
sdf_clamp_3d <- function(a, lo, hi) sdf_min_3d(sdf_max_3d(a, lo), hi)
sdf_shift_3d <- function(p, centre) list(p[[1L]] - centre[1L], p[[2L]] - centre[2L], p[[3L]] - centre[3L])
sdf_dot_3d <- function(p, n) p[[1L]] * n[1L] + p[[2L]] * n[2L] + p[[3L]] * n[3L]

# Euclidean length of a list of components. On intervals each partial
# derivative v_i / |v| is enclosed by the product of the bounds of v_i and
# 1 / |v|, intersected with [-1, 1], which keeps it finite where |v| may vanish.
sdf_length_3d <- function(v) {
  if (!any(vapply(v, inherits, logical(1), "ggplot3d_interval"))) return(sqrt(Reduce(`+`, lapply(v, function(a) a^2))))
  v <- lapply(v, as_interval_3d)
  s <- Reduce(`+`, lapply(v, function(a) a^2))
  len <- bounds_monotone_3d(list(lo = pmax(s$lo, 0), hi = s$hi), sqrt)
  inverse <- bounds_recip_3d(len)
  d <- NULL
  for (a in v) if (!is.null(a$d)) {
    u <- bounds_mul_3d(a, inverse)
    u <- list(lo = pmin(pmax(u$lo, -1), 1), hi = pmax(pmin(u$hi, 1), -1))
    d <- interval_d_add_3d(d, interval_d_scale_3d(a$d, u))
  }
  interval_3d(len$lo, len$hi, d)
}

# Quadratic polynomial smooth minimum with k normalised to the thickness added
# where the operands meet (Quilez, smooth minimum): h = max(4k - |a - b|, 0)
# and smin = min(a, b) - h^2 / (16 k). Its partial derivatives are 1 - w and
# w with w in [0, 1/2], so it keeps the larger Lipschitz constant.
sdf_smin_3d <- function(a, b, k) {
  h <- sdf_max_3d(4 * k - abs(a - b), 0)
  sdf_min_3d(a, b) - h^2 * (1 / (16 * k))
}

sdf_exact_all_3d <- function(params, exact) all(exact)

sdf_register_3d("sphere", function(params, p, child) sdf_length_3d(sdf_shift_3d(p, params$centre)) - params$radius,
  lipschitz = function(params, L) 1, exact = function(params, exact) TRUE)

sdf_register_3d("box", function(params, p, child) {
  q <- lapply(1:3, function(a) abs(p[[a]] - params$centre[a]) - params$size[a] / 2 + params$radius)
  outside <- sdf_length_3d(lapply(q, sdf_max_3d, 0))
  inside <- sdf_min_3d(sdf_max_3d(q[[1L]], sdf_max_3d(q[[2L]], q[[3L]])), 0)
  outside + inside - params$radius
}, lipschitz = function(params, L) 1, exact = function(params, exact) TRUE)

sdf_register_3d("torus", function(params, p, child) {
  q <- sdf_shift_3d(p, params$centre)
  sdf_length_3d(list(sdf_length_3d(q[1:2]) - params$major, q[[3L]])) - params$minor
}, lipschitz = function(params, L) 1, exact = function(params, exact) TRUE)

sdf_register_3d("capsule", function(params, p, child) {
  pa <- sdf_shift_3d(p, params$a); ba <- params$b - params$a
  h <- sdf_clamp_3d(sdf_dot_3d(pa, ba) * (1 / sum(ba^2)), 0, 1)
  sdf_length_3d(lapply(1:3, function(a) pa[[a]] - h * ba[a])) - params$radius
}, lipschitz = function(params, L) 1, exact = function(params, exact) TRUE)

sdf_register_3d("plane", function(params, p, child) sdf_dot_3d(sdf_shift_3d(p, params$point), params$normal),
  lipschitz = function(params, L) 1, exact = function(params, exact) TRUE)

# sin x cos y + sin y cos z + sin z cos x has partial derivatives such as
# cos x cos y - sin z sin x, each at most sqrt(2) in size, so dividing by
# sqrt(6) times the angular frequency bounds the gradient by one.
sdf_register_3d("gyroid", function(params, p, child) {
  k <- 2 * pi / params$period
  q <- lapply(1:3, function(a) (p[[a]] - params$centre[a]) * k)
  (sin(q[[1L]]) * cos(q[[2L]]) + sin(q[[2L]]) * cos(q[[3L]]) + sin(q[[3L]]) * cos(q[[1L]])) * (1 / (k * sqrt(6)))
}, lipschitz = function(params, L) 1)

# Sum of compactly supported Wyvill kernels (1 - d^2 / R^2)^3 below the
# threshold 1/2, with R chosen so that an isolated ball has the given radius.
# A kernel's slope peaks at d = R / sqrt(5) with 96 / (25 sqrt(5) R).
sdf_register_3d("metaballs", function(params, p, child) {
  field <- 0
  for (i in seq_along(params$radius)) {
    d2 <- Reduce(`+`, lapply(1:3, function(a) (p[[a]] - params$centres[i, a])^2))
    field <- field + sdf_max_3d(1 - d2 * (1 / params$support[i]^2), 0)^3
  }
  0.5 - field
}, lipschitz = function(params, L) sum(96 / (25 * sqrt(5) * params$support)))

sdf_register_3d("union", function(params, p, child) sdf_min_3d(child(1L), child(2L)))
sdf_register_3d("intersection", function(params, p, child) sdf_max_3d(child(1L), child(2L)))
sdf_register_3d("difference", function(params, p, child) sdf_max_3d(child(1L), -child(2L)))
sdf_register_3d("complement", function(params, p, child) -child(1L), exact = sdf_exact_all_3d)

sdf_register_3d("blend", function(params, p, child) {
  sign <- if (params$mode == "union") 1 else -1
  other <- if (params$mode == "difference") -1 else 1
  sign * sdf_smin_3d(sign * child(1L), sign * other * child(2L), params$k)
})

sdf_register_3d("offset", function(params, p, child) child(1L) - params$distance, exact = sdf_exact_all_3d)
sdf_register_3d("shell", function(params, p, child) abs(child(1L)) - params$thickness / 2, exact = sdf_exact_all_3d)

# Rotation and translation move the sampling point by the inverse map;
# uniform scaling s evaluates the operand at p / s and multiplies by s.
sdf_register_3d("transform", function(params, p, child) {
  q <- sdf_shift_3d(p, params$translate)
  r <- params$rotate
  q <- lapply(1:3, function(a) (q[[1L]] * r[1L, a] + q[[2L]] * r[2L, a] + q[[3L]] * r[3L, a]) * (1 / params$scale))
  child(1L, q) * params$scale
}, exact = sdf_exact_all_3d)

check_point_3d <- function(x, name) {
  if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x))) stop(name, " must be three finite numbers.", call. = FALSE)
  as.double(x)
}

check_positive_3d <- function(x, name, zero = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || (!zero && x == 0))
    stop(name, " must be a ", if (zero) "non-negative" else "positive", " number.", call. = FALSE)
  as.double(x)
}

#' Signed distance functions
#'
#' Constructors of implicit shapes as signed distance functions: negative
#' inside, positive outside, zero on the surface. The result is a function of
#' `x`, `y` and `z` that can be passed as `fun` to [stat_isofunction_3d()],
#' where it defaults to `levels = 0`. It also carries the expression tree it
#' was built from, so the extraction evaluates it on whole boxes in interval
#' arithmetic (`adaptive = TRUE`) and takes vertex normals from its exact
#' gradient. Shapes combine with the operators in [sdf_blend()].
#'
#' The formulas are those of Quilez's catalogue. `sdf_sphere()`, `sdf_box()`,
#' `sdf_torus()`, `sdf_capsule()` and `sdf_plane()` return the exact Euclidean
#' distance. `sdf_gyroid()` returns the gyroid function divided by an upper
#' bound of its gradient, a lower bound of the distance. `sdf_metaballs()`
#' returns one half minus a sum of Wyvill kernels, which is no distance; its
#' surface encloses a ball of each given radius where the balls are apart and
#' blends them where they meet.
#'
#' @param centre Centre of the shape.
#' @param radius Sphere radius, capsule radius, rounding radius of the box
#'   edges, or for `sdf_metaballs()` one radius per centre.
#' @param size Edge lengths of the box along x, y and z.
#' @param major,minor Torus radii: distance from the centre to the tube axis,
#'   and tube radius. The torus lies in the x-y plane.
#' @param a,b End points of the capsule axis.
#' @param normal Plane normal, pointing to the positive side.
#' @param point A point on the plane.
#' @param period Period of the gyroid along each axis.
#' @param centres Ball centres as a three-column matrix or data frame.
#' @return A `ggplot3d_sdf` function of `x`, `y` and `z`.
#' @references Quilez, I. Distance functions.
#'   <https://iquilezles.org/articles/distfunctions/>
#'
#'   Wyvill, McPheeters and Wyvill (1986). Data structure for soft objects.
#'   The Visual Computer 2, 227-234. \doi{10.1007/BF01900346}
#' @seealso [sdf_blend()] for unions, intersections, differences, smooth blends,
#'   offsets, shells and transforms.
#' @export
#' @examples
#' s <- sdf_sphere(radius = 1)
#' s(c(0, 2), 0, 0)
#' ggplot2::ggplot() +
#'   geom_isofunction_3d(fun = sdf_box(size = c(2, 1, 1), radius = 0.2), n = 16) +
#'   ggplot2::xlim(-1.5, 1.5) + ggplot2::ylim(-1.5, 1.5) +
#'   scale_z_continuous(limits = c(-1.5, 1.5)) + coord_3d()
sdf_sphere <- function(centre = c(0, 0, 0), radius = 1) {
  new_sdf_3d("sphere", list(centre = check_point_3d(centre, "centre"), radius = check_positive_3d(radius, "radius")))
}

#' @rdname sdf_sphere
#' @export
sdf_box <- function(centre = c(0, 0, 0), size = c(1, 1, 1), radius = 0) {
  size <- rep_len(as.double(size), 3L)
  if (any(!is.finite(size) | size <= 0)) stop("size must contain positive finite edge lengths.", call. = FALSE)
  radius <- check_positive_3d(radius, "radius", zero = TRUE)
  if (2 * radius > min(size)) stop("radius must not exceed half the shortest edge.", call. = FALSE)
  new_sdf_3d("box", list(centre = check_point_3d(centre, "centre"), size = size, radius = radius))
}

#' @rdname sdf_sphere
#' @export
sdf_torus <- function(centre = c(0, 0, 0), major = 1, minor = 0.25) {
  new_sdf_3d("torus", list(centre = check_point_3d(centre, "centre"), major = check_positive_3d(major, "major"),
    minor = check_positive_3d(minor, "minor")))
}

#' @rdname sdf_sphere
#' @export
sdf_capsule <- function(a = c(0, 0, -0.5), b = c(0, 0, 0.5), radius = 0.25) {
  a <- check_point_3d(a, "a"); b <- check_point_3d(b, "b")
  if (all(a == b)) stop("a and b must differ.", call. = FALSE)
  new_sdf_3d("capsule", list(a = a, b = b, radius = check_positive_3d(radius, "radius")))
}

#' @rdname sdf_sphere
#' @export
sdf_plane <- function(normal = c(0, 0, 1), point = c(0, 0, 0)) {
  normal <- check_point_3d(normal, "normal")
  if (sum(normal^2) == 0) stop("normal must be nonzero.", call. = FALSE)
  new_sdf_3d("plane", list(normal = normal / sqrt(sum(normal^2)), point = check_point_3d(point, "point")))
}

#' @rdname sdf_sphere
#' @export
sdf_gyroid <- function(period = 2 * pi, centre = c(0, 0, 0)) {
  new_sdf_3d("gyroid", list(period = check_positive_3d(period, "period"), centre = check_point_3d(centre, "centre")))
}

#' @rdname sdf_sphere
#' @export
sdf_metaballs <- function(centres, radius = 1) {
  centres <- as.matrix(centres)
  if (!is.numeric(centres) || ncol(centres) != 3L || !nrow(centres) || any(!is.finite(centres)))
    stop("centres must be a three-column numeric matrix with at least one row.", call. = FALSE)
  radius <- rep_len(as.double(radius), nrow(centres))
  if (any(!is.finite(radius) | radius <= 0)) stop("radius must contain positive finite values.", call. = FALSE)
  # (1 - (r / R)^2)^3 = 1/2 places an isolated ball's surface at radius r.
  support <- radius / sqrt(1 - 0.5^(1 / 3))
  new_sdf_3d("metaballs", list(centres = unname(centres) * 1, radius = radius, support = support))
}

#' Combine and transform signed distance functions
#'
#' Constructive solid geometry on `ggplot3d_sdf` shapes ([sdf_sphere()]).
#' `a | b` is the union (`min(a, b)`), `a & b` the intersection
#' (`max(a, b)`), `a - b` removes b from a (`max(a, -b)`) and `-a` swaps
#' inside and outside. As in Quilez's catalogue the union is the exact
#' distance outside the shape only, and intersections and differences are
#' lower bounds of the distance.
#'
#' `sdf_blend()` replaces the minimum (or maximum) by the quadratic polynomial
#' smooth minimum, with `k` the largest thickness added where the shapes
#' meet, in distance units. `sdf_offset()` moves the surface outwards by
#' `distance` (inwards when negative); `sdf_shell()` keeps a wall of the given
#' thickness centred on the surface. `sdf_transform()` scales the shape
#' uniformly about the origin, rotates it about the x, y and z axes in turn
#' and then translates it; both keep the distance exact.
#'
#' Every result records a Lipschitz constant of its value: one for exact
#' distances and their combinations, so the value never exceeds the distance
#' to the surface.
#' @param a,b Shapes.
#' @param k Blend thickness, in distance units.
#' @param mode `"union"`, `"intersection"` or `"difference"` (a without b).
#' @param sdf A shape.
#' @param distance Offset distance.
#' @param thickness Wall thickness.
#' @param translate Translation vector.
#' @param rotate Rotation angles in degrees about x, y and z, applied in that
#'   order, or a 3 x 3 rotation matrix.
#' @param scale Positive uniform scale factor.
#' @return A `ggplot3d_sdf` function of `x`, `y` and `z`.
#' @references Quilez, I. Smooth minimum. <https://iquilezles.org/articles/smin/>
#'
#'   Quilez, I. Interior SDFs. <https://iquilezles.org/articles/interiordistance/>
#' @export
#' @examples
#' cut <- sdf_box(size = c(1.6, 1.6, 1.6), radius = 0.1) - sdf_sphere(radius = 1)
#' blob <- sdf_blend(sdf_sphere(c(-0.5, 0, 0), 0.6), sdf_sphere(c(0.5, 0, 0), 0.6), k = 0.2)
#' cut
#' ggplot2::ggplot() +
#'   geom_isofunction_3d(fun = sdf_shell(blob, 0.1) & sdf_plane(c(0, 1, 0)), n = 20) +
#'   ggplot2::xlim(-1.3, 1.3) + ggplot2::ylim(-1, 1) +
#'   scale_z_continuous(limits = c(-1, 1)) + coord_3d()
sdf_blend <- function(a, b, k = 0.1, mode = c("union", "intersection", "difference")) {
  new_sdf_3d("blend", list(k = check_positive_3d(k, "k"), mode = match.arg(mode)), list(a, b))
}

#' @rdname sdf_blend
#' @export
sdf_offset <- function(sdf, distance) {
  if (!is.numeric(distance) || length(distance) != 1L || !is.finite(distance)) stop("distance must be a finite number.", call. = FALSE)
  new_sdf_3d("offset", list(distance = as.double(distance)), list(sdf))
}

#' @rdname sdf_blend
#' @export
sdf_shell <- function(sdf, thickness) {
  new_sdf_3d("shell", list(thickness = check_positive_3d(thickness, "thickness")), list(sdf))
}

#' @rdname sdf_blend
#' @export
sdf_transform <- function(sdf, translate = c(0, 0, 0), rotate = c(0, 0, 0), scale = 1) {
  if (is.matrix(rotate)) {
    if (!is.numeric(rotate) || any(dim(rotate) != 3L) || any(!is.finite(rotate)) ||
        max(abs(crossprod(rotate) - diag(3))) > 1e-8 || det(rotate) < 0)
      stop("rotate must be a 3 x 3 rotation matrix.", call. = FALSE)
  } else {
    angle <- check_point_3d(rotate, "rotate") * pi / 180
    turn <- function(a, i, j) { m <- diag(3); m[i, i] <- m[j, j] <- cos(a); m[j, i] <- sin(a); m[i, j] <- -sin(a); m }
    rotate <- turn(angle[3L], 1L, 2L) %*% turn(angle[2L], 3L, 1L) %*% turn(angle[1L], 2L, 3L)
  }
  new_sdf_3d("transform", list(translate = check_point_3d(translate, "translate"), rotate = unname(rotate) * 1,
    scale = check_positive_3d(scale, "scale")), list(sdf))
}

#' @rdname sdf_blend
#' @param e1,e2 Shapes.
#' @export
Ops.ggplot3d_sdf <- function(e1, e2) {
  if (missing(e2)) {
    if (.Generic == "-") return(new_sdf_3d("complement", list(), list(e1)))
    stop("Unary `", .Generic, "` is not defined for signed distance functions.", call. = FALSE)
  }
  op <- switch(.Generic, "|" = "union", "&" = "intersection", "-" = "difference",
    stop("`", .Generic, "` is not defined for signed distance functions; use |, & or -.", call. = FALSE))
  new_sdf_3d(op, list(), list(e1, e2))
}

sdf_format_3d <- function(node, indent = "") {
  params <- vapply(names(node$params), function(name) {
    v <- node$params[[name]]
    value <- if (is.character(v)) paste0('"', v, '"') else if (length(v) == 1L) format(signif(v, 4)) else
      paste0("(", paste(format(signif(as.vector(v), 4), trim = TRUE), collapse = ", "), ")")
    paste0(name, " = ", value)
  }, character(1))
  head <- paste0(indent, node$op, if (length(params)) paste0("(", paste(params, collapse = ", "), ")"))
  c(head, unlist(lapply(node$children, sdf_format_3d, paste0(indent, "  "))))
}

#' @export
print.ggplot3d_sdf <- function(x, ...) {
  node <- sdf_node_3d(x)
  kind <- if (node$exact) "exact distance" else if (node$lipschitz <= 1) "distance bound" else "implicit function"
  cat("<ggplot3d_sdf> ", kind, if (!node$exact) sprintf(" (Lipschitz %s)", format(signif(node$lipschitz, 4))), "\n", sep = "")
  cat(sdf_format_3d(node), sep = "\n")
  invisible(x)
}
