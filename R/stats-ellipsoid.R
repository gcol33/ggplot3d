# Location and scatter estimates of one group of points, x an n x 3 matrix.

# Multivariate t estimate with nu degrees of freedom by iteratively reweighted
# moments (Kent, Tyler and Vardi 1994), as MASS::cov.trob: weights
# (nu + p) / (nu + Q) from the Mahalanobis distances Q, the scatter divided by n.
cov_trob_3d <- function(x, nu = 5, maxit = 25L, tol = 0.01) {
  n <- nrow(x); p <- ncol(x)
  loc <- colMeans(x)
  w <- rep(1, n)
  for (iter in seq_len(maxit)) {
    w0 <- w
    centred <- sweep(x, 2L, loc)
    s <- svd(sqrt(w / sum(w)) * centred, nu = 0L)
    if (min(s$d) <= 1e-12 * max(s$d)) return(NULL)
    q <- rowSums((centred %*% s$v %*% diag(1 / s$d, p))^2)
    w <- (nu + p) / (nu + q)
    loc <- colSums(w * x) / sum(w)
    if (all(abs(w - w0) < tol)) break
  }
  list(center = loc, cov = crossprod(sqrt(w) * centred) / n)
}

# Minimum-volume enclosing ellipsoid by Khachiyan's barycentric coordinate
# ascent with the Todd and Yildirim step. Returns centre c and shape S with
# every point satisfying (x - c)' S^-1 (x - c) <= 1.
ellipsoid_hull_3d <- function(x, tol = 1e-9, maxit = 10000L) {
  n <- nrow(x); d <- ncol(x)
  q <- rbind(t(x), 1)
  u <- rep(1 / n, n)
  for (iter in seq_len(maxit)) {
    m <- colSums(q * solve(q %*% (u * t(q)), q))
    j <- which.max(m)
    step <- (m[j] - d - 1) / ((d + 1) * (m[j] - 1))
    if (step <= tol) break
    u <- (1 - step) * u
    u[j] <- u[j] + step
  }
  centre <- drop(t(x) %*% u)
  shape <- d * (crossprod(x, u * x) - tcrossprod(centre))
  # The ascent stops short of the optimum; stretch until every point is inside.
  stretch <- max(rowSums((sweep(x, 2L, centre) %*% solve(shape)) * sweep(x, 2L, centre)))
  list(center = centre, cov = shape * max(1, stretch))
}

ellipsoid_types_3d <- c("t", "norm", "euclid")
ellipsoid_kinds_3d <- c("data", "se", "ehull")

check_ellipsoid_params_3d <- function(type, level, kind) {
  type <- match.arg(type, ellipsoid_types_3d)
  kind <- match.arg(kind, ellipsoid_kinds_3d)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0) stop("level must be a positive number.", call. = FALSE)
  if (type != "euclid" && kind != "ehull" && level >= 1) stop("level must lie strictly between zero and one.", call. = FALSE)
  if (type == "euclid" && kind == "se") stop("kind = \"se\" needs a covariance; type = \"euclid\" has none.", call. = FALSE)
  list(type = type, level = level, kind = kind)
}

# One confidence or coverage ellipsoid: centre and the shape matrix S of
# {p : (p - centre)' S^-1 (p - centre) <= 1}, or NULL when the group cannot
# carry one.
#   kind "data": S = r^2 V with r^2 = p F(level; p, n - 1), as ggplot2::stat_ellipse.
#   kind "se":   S = r^2 V / n with r^2 = p (n - 1) / (n - p) F(level; p, n - p),
#                the Hotelling T^2 confidence region of the mean.
#   type "euclid": a sphere of radius `level`.
ellipsoid_shape_3d <- function(x, type, level, kind) {
  n <- nrow(x); p <- 3L
  if (n - 1L < p) {
    message("Too few points to calculate an ellipsoid: a group needs at least four.")
    return(NULL)
  }
  if (kind == "ehull") {
    if (qr(sweep(x, 2L, colMeans(x)))$rank < p) return(flat_ellipsoid_3d())
    return(ellipsoid_hull_3d(x))
  }
  if (type == "euclid") return(list(center = colMeans(x), cov = diag(level^2, p)))
  v <- if (type == "t") cov_trob_3d(x) else list(center = colMeans(x), cov = stats::cov(x))
  if (is.null(v) || qr(v$cov)$rank < p) return(flat_ellipsoid_3d())
  r2 <- if (kind == "data") p * stats::qf(level, p, n - 1L) else p * (n - 1) / (n - p) * stats::qf(level, p, n - p)
  list(center = v$center, cov = r2 * if (kind == "se") v$cov / n else v$cov)
}

flat_ellipsoid_3d <- function() {
  message("Points of a group lie on a plane or a line: no ellipsoid.")
  NULL
}

StatEllipsoid3D <- ggplot2::ggproto("StatEllipsoid3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  setup_params = function(data, params) {
    params[c("type", "level", "kind")] <- check_ellipsoid_params_3d(params$type %or3d% "t", params$level %or3d% 0.95, params$kind %or3d% "data")
    params
  },
  compute_group = function(data, scales, type = "t", level = 0.95, kind = "data") {
    if (any(vapply(scales[intersect(c("x", "y", "z"), names(scales))], function(s) s$is_discrete(), logical(1))))
      stop("Ellipsoids require continuous position scales.", call. = FALSE)
    x <- as.matrix(data[c("x", "y", "z")])
    x <- x[stats::complete.cases(x), , drop = FALSE]
    shape <- ellipsoid_shape_3d(x, type, level, kind)
    if (is.null(shape)) return(data.frame())
    keep <- vapply(data, function(col) length(unique(col)) == 1L, logical(1))
    keep[c("x", "y", "z")] <- FALSE
    out <- data[1L, keep, drop = FALSE]
    out$x <- shape$center[1L]; out$y <- shape$center[2L]; out$z <- shape$center[3L]
    out$sigma <- list(unname(shape$cov))
    out$level <- if (kind == "ehull") 1 else level
    out$volume <- 4 / 3 * pi * sqrt(max(0, det(shape$cov)))
    out$n <- nrow(x)
    out
  }
)
