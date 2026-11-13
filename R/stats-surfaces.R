StatFunction3D <- ggplot2::ggproto("StatFunction3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, n = 40L, args = list()) {
    grid <- expand.grid(stats_grid_3d(scales, n))
    z <- do.call(fun, c(list(grid$x, grid$y), args))
    if (!is.numeric(z) || !(length(z) %in% c(1L, nrow(grid)))) stop("fun must return one numeric value per grid point.", call. = FALSE)
    grid$z <- rep_len(z, nrow(grid))
    stats_triangles_3d(grid)
  }
)

StatSmooth3D <- ggplot2::ggproto("StatSmooth3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "weight",
  compute_group = function(data, scales, method = "loess", formula = NULL, n = 30L,
                           se = TRUE, level = 0.95, span = 0.75, method.args = list(), se.alpha = 0.2) {
    method <- match.arg(method, c("loess", "lm", "gam"))
    if (scales$z$is_discrete()) stop("Smoothing requires a continuous z scale.", call. = FALSE)
    if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("level must lie strictly between zero and one.", call. = FALSE)
    if (length(se.alpha) != 1L || !is.finite(se.alpha) || se.alpha < 0 || se.alpha > 1) stop("se.alpha must lie between zero and one.", call. = FALSE)
    if (method == "gam" && !requireNamespace("mgcv", quietly = TRUE)) stop("Install mgcv to use method = 'gam'.", call. = FALSE)
    if (is.null(formula)) formula <- switch(method, loess = z ~ x + y, lm = z ~ x + y,
      gam = stats::as.formula("z ~ s(x, y)", env = asNamespace("mgcv")))
    fit_fun <- switch(method, loess = stats::loess, lm = stats::lm, gam = mgcv::gam)
    fit_args <- c(list(formula = formula, data = data), if (method == "loess") list(span = span), method.args)
    if ("weight" %in% names(data)) fit_args$weights <- data$weight
    model <- do.call(fit_fun, fit_args)
    grid <- expand.grid(stats_grid_3d(scales, n))
    prediction <- stats::predict(model, newdata = grid, se.fit = se)
    grid$z <- as.double(if (se) prediction$fit else prediction)
    grid$.opacity <- 1; grid$surface <- "fit"
    out <- list(stats_triangles_3d(grid))
    if (se) {
      df <- if (method == "loess") prediction$df else stats::df.residual(model)
      critical <- if (is.finite(df) && df > 0) stats::qt((1 + level) / 2, df) else stats::qnorm((1 + level) / 2)
      error <- as.double(prediction$se.fit) * critical
      for (sign in c(-1, 1)) {
        bound <- grid; bound$z <- grid$z + sign * error
        bound$surface <- if (sign < 0) "lower" else "upper"; bound$.opacity <- se.alpha
        mesh <- stats_triangles_3d(bound)
        if (nrow(mesh)) mesh$.face <- mesh$.face + sum(vapply(out, nrow, integer(1))) / 3L
        out[[length(out) + 1L]] <- mesh
      }
    }
    do.call(rbind, out)
  }
)

StatDensity2D3D <- ggplot2::ggproto("StatDensity2D3D", StatGeometry3D,
  required_aes = c("x", "y"),
  optional_aes = "z",
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 40L) {
    axes <- stats_grid_3d(scales, n)
    xy <- data[c("x", "y")]
    bandwidth <- stats_bandwidth_3d(xy, h, adjust, mass = TRUE)
    out <- expand.grid(axes)
    out$density <- geometry_kde_cpp(as.matrix(xy), axes, bandwidth)
    out$ndensity <- out$density / max(out$density); out$count <- nrow(data) * out$density
    out$z <- scales$z$get_transformation()$transform(out$density)
    stats_triangles_3d(out)
  }
)

StatRidgeline3D <- ggplot2::ggproto("StatRidgeline3D", StatGeometry3D,
  required_aes = c("x", "y"),
  optional_aes = "z",
  compute_group = function(data, scales, along = "x", n = 128L, bw = "nrd0", adjust = 1, base = 0, height = 1) {
    along <- match.arg(along, c("x", "y")); across <- setdiff(c("x", "y"), along)
    location <- unique(data[[across]])
    if (length(location) != 1L) stop("Each ridge group must have one location on the perpendicular axis.", call. = FALSE)
    if (length(base) != 1L || !is.finite(base) || length(height) != 1L || !is.finite(height) || height <= 0)
      stop("base must be finite and height must be positive and finite.", call. = FALSE)
    axis <- stats_grid_3d(scales, n, along)[[1L]]
    den <- stats::density(data[[along]], bw = bw, adjust = adjust, n = length(axis), from = min(axis), to = max(axis))
    # FFT roundoff at vanishing density tails produces numerically collapsed faces.
    den$y[den$y <= 256 * .Machine$double.eps * max(den$y)] <- 0
    grid <- data.frame(x = den$x, y = location, z = base + height * den$y, density = den$y)
    names(grid)[1:2] <- c(along, across)
    bottom <- grid; bottom$z <- base
    vertices <- rbind(grid, bottom); m <- nrow(grid); i <- seq_len(m - 1L)
    tri <- rbind(cbind(i, i + 1L, i + m), cbind(i + 1L, i + m + 1L, i + m))
    tri <- tri[nondegenerate_triangles_3d(vertices, tri), , drop = FALSE]
    out <- vertices[as.vector(t(tri)), , drop = FALSE]
    out$.face <- rep(seq_len(nrow(tri)), each = 3L); out$.corner <- rep.int(1:3, nrow(tri))
    out$.vertex <- as.vector(t(tri)); out$z_mean <- rep(rowMeans(matrix(vertices$z[tri], ncol = 3L)), each = 3L)
    out$density <- rep(rowMeans(matrix(vertices$density[tri], ncol = 3L)), each = 3L)
    out
  }
)
