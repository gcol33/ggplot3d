# Interval arithmetic with first-order interval gradients. An interval holds
# vectors lo and hi (one entry per box) and d, the interval enclosures of the
# three partial derivatives (NULL for a constant). Evaluating a function on
# interval arguments encloses its range and gradient over each box (Moore
# 1966; forward-mode differentiation in interval arithmetic). NA bounds mark
# boxes where the function is undefined; NaN from indeterminate forms widens
# to the real line.

interval_3d <- function(lo, hi = lo, d = NULL) structure(list(lo = lo, hi = hi, d = d), class = "ggplot3d_interval")

as_interval_3d <- function(x) {
  if (inherits(x, "ggplot3d_interval")) return(x)
  if (!is.numeric(x) && !is.logical(x)) stop("Non-numeric value in an interval expression.", call. = FALSE)
  interval_3d(as.double(x))
}

# The call as written, with the S3 method name replaced by its generic.
generic_call_3d <- function(call, generic) {
  call[[1L]] <- as.name(generic)
  call
}

interval_unsupported_3d <- function(call) {
  stop(sprintf("`%s` has no interval extension.", paste(deparse(call, width.cutoff = 200L), collapse = " ")), call. = FALSE)
}

# Bounds arithmetic on lists with lo and hi.
bounds_3d <- function(lo, hi) {
  lo[is.nan(lo)] <- -Inf; hi[is.nan(hi)] <- Inf
  list(lo = lo, hi = hi)
}
bounds_add_3d <- function(a, b) bounds_3d(a$lo + b$lo, a$hi + b$hi)
bounds_neg_3d <- function(a) list(lo = -a$hi, hi = -a$lo)
bounds_mul_3d <- function(a, b) {
  p <- list(a$lo * b$lo, a$lo * b$hi, a$hi * b$lo, a$hi * b$hi)
  p <- lapply(p, function(x) { x[is.nan(x)] <- 0; x })
  list(lo = do.call(pmin, p), hi = do.call(pmax, p))
}
bounds_recip_3d <- function(a) {
  zero <- a$lo <= 0 & a$hi >= 0
  bounds_3d(ifelse(zero, -Inf, 1 / a$hi), ifelse(zero, Inf, 1 / a$lo))
}
bounds_scale_3d <- function(a, s) bounds_mul_3d(a, list(lo = s, hi = s))
bounds_hull_3d <- function(a, b) list(lo = pmin(a$lo, b$lo), hi = pmax(a$hi, b$hi))
bounds_powi_3d <- function(a, n) {
  if (n == 0) return(list(lo = a$lo * 0 + 1, hi = a$hi * 0 + 1))
  if (n < 0) return(bounds_recip_3d(bounds_powi_3d(a, -n)))
  lo <- a$lo^n; hi <- a$hi^n
  if (n %% 2 == 1) return(list(lo = lo, hi = hi))
  list(lo = ifelse(a$lo > 0, lo, ifelse(a$hi < 0, hi, 0)), hi = pmax(lo, hi))
}
# Restriction to the domain [floor, Inf); NA where the box lies outside it.
bounds_domain_3d <- function(a, floor) {
  out <- ifelse(a$hi < floor, NA_real_, 1)
  list(lo = pmax(a$lo, floor) * out, hi = a$hi * out)
}
bounds_monotone_3d <- function(a, f, increasing = TRUE) {
  if (increasing) list(lo = f(a$lo), hi = f(a$hi)) else list(lo = f(a$hi), hi = f(a$lo))
}
bounds_sin_3d <- function(a) {
  lo <- pmin(sin(a$lo), sin(a$hi)); hi <- pmax(sin(a$lo), sin(a$hi))
  top <- pi / 2 + 2 * pi * ceiling((a$lo - pi / 2) / (2 * pi))
  bottom <- -pi / 2 + 2 * pi * ceiling((a$lo + pi / 2) / (2 * pi))
  hi[which(top <= a$hi)] <- 1; lo[which(bottom <= a$hi)] <- -1
  wide <- which(!is.finite(a$lo) | !is.finite(a$hi) | a$hi - a$lo >= 2 * pi)
  lo[wide] <- -1; hi[wide] <- 1
  list(lo = lo, hi = hi)
}
bounds_cosh_3d <- function(a) {
  low <- ifelse(a$lo > 0, a$lo, ifelse(a$hi < 0, -a$hi, 0))
  list(lo = cosh(low), hi = cosh(pmax(abs(a$lo), abs(a$hi))))
}
bounds_abs_3d <- function(a) list(lo = ifelse(a$lo >= 0, a$lo, ifelse(a$hi <= 0, -a$hi, 0)), hi = pmax(abs(a$lo), abs(a$hi)))
bounds_sign_3d <- function(a) list(lo = ifelse(a$lo >= 0, 1, -1), hi = ifelse(a$hi <= 0, -1, 1))

# Chain rule: d f(u) = f'(u) du with f'(u) as bounds.
interval_chain_3d <- function(value, derivative, u) {
  d <- if (!is.null(u$d)) lapply(u$d, bounds_mul_3d, derivative)
  interval_3d(value$lo, value$hi, d)
}
interval_d_add_3d <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  Map(bounds_add_3d, a, b)
}
interval_d_scale_3d <- function(d, s) if (!is.null(d)) lapply(d, bounds_mul_3d, s)

interval_add_3d <- function(a, b) {
  v <- bounds_add_3d(a, b)
  interval_3d(v$lo, v$hi, interval_d_add_3d(a$d, b$d))
}
interval_neg_3d <- function(a) interval_3d(-a$hi, -a$lo, if (!is.null(a$d)) lapply(a$d, bounds_neg_3d))
interval_mul_3d <- function(a, b) {
  v <- bounds_mul_3d(a, b)
  interval_3d(v$lo, v$hi, interval_d_add_3d(interval_d_scale_3d(a$d, b), interval_d_scale_3d(b$d, a)))
}
interval_div_3d <- function(a, b) {
  r <- bounds_recip_3d(b)
  q <- bounds_mul_3d(a, r)
  d <- interval_d_add_3d(a$d, interval_d_scale_3d(b$d, bounds_neg_3d(q)))
  interval_3d(q$lo, q$hi, interval_d_scale_3d(d, r))
}
interval_pow_3d <- function(a, p) {
  if (p == trunc(p)) {
    v <- bounds_powi_3d(a, p)
    return(interval_chain_3d(v, bounds_scale_3d(bounds_powi_3d(a, p - 1), p), a))
  }
  u <- bounds_domain_3d(a, 0)
  v <- bounds_monotone_3d(u, function(x) x^p, p > 0)
  dv <- bounds_monotone_3d(u, function(x) p * x^(p - 1), (p - 1) * p > 0)
  interval_chain_3d(v, bounds_3d(pmin(dv$lo, dv$hi), pmax(dv$lo, dv$hi)), a)
}
interval_exp_3d <- function(a) {
  v <- bounds_monotone_3d(a, exp)
  interval_chain_3d(v, v, a)
}
interval_log_3d <- function(a) {
  u <- bounds_domain_3d(a, 0)
  interval_chain_3d(bounds_monotone_3d(u, log), bounds_recip_3d(u), a)
}

# Elementwise maximum (or minimum) of intervals: the bounds of the larger
# argument, with the gradient of the argument that dominates the box or the
# hull of both gradients where neither does.
interval_extreme_3d <- function(args, largest) {
  if (!any(vapply(args, inherits, logical(1), "ggplot3d_interval")))
    return(do.call(if (largest) base::pmax else base::pmin, args))
  args <- lapply(args, as_interval_3d)
  Reduce(function(a, b) {
    if (!largest) { a <- interval_neg_3d(a); b <- interval_neg_3d(b) }
    lo <- pmax(a$lo, b$lo); hi <- pmax(a$hi, b$hi)
    left <- a$lo > b$hi; right <- b$lo > a$hi
    zero <- list(lo = 0, hi = 0)
    da <- a$d %or3d% rep(list(zero), 3L); db <- b$d %or3d% rep(list(zero), 3L)
    d <- if (!is.null(a$d) || !is.null(b$d)) Map(function(x, y) {
      h <- bounds_hull_3d(x, y)
      list(lo = ifelse(left %in% TRUE, x$lo, ifelse(right %in% TRUE, y$lo, h$lo)),
           hi = ifelse(left %in% TRUE, x$hi, ifelse(right %in% TRUE, y$hi, h$hi)))
    }, da, db)
    out <- interval_3d(lo, hi, d)
    if (largest) out else interval_neg_3d(out)
  }, args)
}

#' @export
Ops.ggplot3d_interval <- function(e1, e2) {
  call <- generic_call_3d(sys.call(), .Generic)
  if (missing(e2)) {
    if (.Generic == "+") return(e1)
    if (.Generic == "-") return(interval_neg_3d(e1))
    interval_unsupported_3d(call)
  }
  if (.Generic == "^") {
    if (!inherits(e2, "ggplot3d_interval") && is.numeric(e2) && length(e2) == 1L && is.finite(e2)) return(interval_pow_3d(as_interval_3d(e1), e2))
    return(interval_exp_3d(interval_mul_3d(as_interval_3d(e2), interval_log_3d(as_interval_3d(e1)))))
  }
  a <- as_interval_3d(e1); b <- as_interval_3d(e2)
  switch(.Generic,
    "+" = interval_add_3d(a, b),
    "-" = interval_add_3d(a, interval_neg_3d(b)),
    "*" = interval_mul_3d(a, b),
    "/" = interval_div_3d(a, b),
    interval_unsupported_3d(call))
}

#' @export
Math.ggplot3d_interval <- function(x, ...) {
  call <- generic_call_3d(sys.call(), .Generic)
  switch(.Generic,
    abs = interval_chain_3d(bounds_abs_3d(x), bounds_sign_3d(x), x),
    sqrt = {
      u <- bounds_domain_3d(x, 0)
      v <- bounds_monotone_3d(u, sqrt)
      interval_chain_3d(v, bounds_scale_3d(bounds_recip_3d(v), 0.5), x)
    },
    exp = interval_exp_3d(x),
    expm1 = interval_chain_3d(bounds_monotone_3d(x, expm1), bounds_monotone_3d(x, exp), x),
    log = interval_log_3d(x),
    log2 = interval_log_3d(x) * (1 / log(2)),
    log10 = interval_log_3d(x) * (1 / log(10)),
    log1p = interval_log_3d(x + 1),
    sin = interval_chain_3d(bounds_sin_3d(x), bounds_sin_3d(bounds_add_3d(x, list(lo = pi / 2, hi = pi / 2))), x),
    cos = interval_chain_3d(bounds_sin_3d(bounds_add_3d(x, list(lo = pi / 2, hi = pi / 2))), bounds_neg_3d(bounds_sin_3d(x)), x),
    sinh = interval_chain_3d(bounds_monotone_3d(x, sinh), bounds_cosh_3d(x), x),
    cosh = interval_chain_3d(bounds_cosh_3d(x), bounds_monotone_3d(x, sinh), x),
    tanh = {
      v <- bounds_monotone_3d(x, tanh)
      interval_chain_3d(v, bounds_add_3d(list(lo = 1, hi = 1), bounds_neg_3d(bounds_powi_3d(v, 2))), x)
    },
    atan = interval_chain_3d(bounds_monotone_3d(x, atan),
      bounds_recip_3d(bounds_add_3d(list(lo = 1, hi = 1), bounds_powi_3d(x, 2))), x),
    interval_unsupported_3d(call))
}

#' @export
Summary.ggplot3d_interval <- function(..., na.rm = FALSE) {
  call <- generic_call_3d(sys.call(), .Generic)
  if (!.Generic %in% c("max", "min")) interval_unsupported_3d(call)
  args <- lapply(list(...), as_interval_3d)
  pieces <- unlist(lapply(args, function(a) lapply(seq_along(a$lo), function(i)
    interval_3d(a$lo[i], a$hi[i], if (!is.null(a$d)) lapply(a$d, function(b) list(lo = b$lo[pmin(i, length(b$lo))], hi = b$hi[pmin(i, length(b$hi))]))))), recursive = FALSE)
  interval_extreme_3d(pieces, .Generic == "max")
}

#' @export
print.ggplot3d_interval <- function(x, ...) {
  print(cbind(lo = x$lo, hi = x$hi))
  invisible(x)
}

# fun with pmax and pmin replaced by their interval extensions; both fall
# back to the base functions when no argument is an interval.
interval_function_3d <- function(fun) {
  env <- new.env(parent = environment(fun) %or3d% globalenv())
  env$pmax <- function(..., na.rm = FALSE) interval_extreme_3d(list(...), TRUE)
  env$pmin <- function(..., na.rm = FALSE) interval_extreme_3d(list(...), FALSE)
  if (!is.primitive(fun)) environment(fun) <- env
  fun
}

# Encloses fun and its gradient over n boxes given as lists of lo and hi per
# axis. Gradients are taken in box coordinates scaled to the unit cube by
# span, so the normal-variation test does not depend on axis units.
interval_evaluate_3d <- function(fun, args, boxes, span) {
  axis <- function(a) {
    d <- rep(list(list(lo = 0, hi = 0)), 3L); d[[a]] <- list(lo = span[a], hi = span[a])
    interval_3d(boxes[[a]]$lo, boxes[[a]]$hi, d)
  }
  n <- length(boxes[[1L]]$lo)
  out <- tryCatch(do.call(fun, c(list(x = axis(1L), y = axis(2L), z = axis(3L)), args)), error = function(e) {
    call <- conditionCall(e)
    where <- if (is.null(call)) "" else paste0(" in `", paste(deparse(call, width.cutoff = 200L), collapse = " "), "`")
    stop("fun cannot be evaluated on intervals", where, ": ", conditionMessage(e), call. = FALSE)
  })
  out <- as_interval_3d(out)
  if (!(length(out$lo) %in% c(1L, n))) stop("fun must return one interval per box.", call. = FALSE)
  zero <- list(lo = 0, hi = 0)
  d <- out$d %or3d% rep(list(zero), 3L)
  list(lo = rep_len(out$lo, n), hi = rep_len(out$hi, n),
       d = lapply(d, function(b) list(lo = rep_len(b$lo, n), hi = rep_len(b$hi, n))))
}
