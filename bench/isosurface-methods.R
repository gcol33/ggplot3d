# Triangle counts, timings and triangle quality of the isosurface methods.
# Run from the repository root:
#   Rscript bench/isosurface-methods.R
# Uses an optimised build of the package sources.
pkgbuild::compile_dll(debug = FALSE, quiet = TRUE)
pkgload::load_all(quiet = TRUE, compile = FALSE)

fields <- list(
  sphere = function(x, y, z) x^2 + y^2 + z^2 - 0.6,
  torus = function(x, y, z) (sqrt(x^2 + y^2) - 0.6)^2 + z^2 - 0.06,
  gyroid = function(x, y, z) sin(4 * x) * cos(4 * y) + sin(4 * y) * cos(4 * z) + sin(4 * z) * cos(4 * x)
)

min_angle <- function(b) {
  xyz <- as.matrix(b[c("x", "y", "z")])
  i <- seq(1L, nrow(xyz), 3L)
  a <- xyz[i, , drop = FALSE]; p <- xyz[i + 1L, , drop = FALSE]; q <- xyz[i + 2L, , drop = FALSE]
  angle <- function(o, u, v) {
    u <- u - o; v <- v - o
    acos(pmin(1, pmax(-1, rowSums(u * v) / sqrt(rowSums(u^2) * rowSums(v^2)))))
  }
  pmin(angle(a, p, q), angle(p, q, a), angle(q, a, p)) * 180 / pi
}

methods <- geometry_isosurface_methods_cpp()
rows <- list()
for (field in names(fields)) for (n in c(32L, 64L, 96L)) {
  axis <- seq(-1, 1, length.out = n)
  grid <- expand.grid(x = axis, y = axis, z = axis)
  value <- do.call(fields[[field]], grid)
  for (method in methods) {
    times <- vapply(1:5, function(i) system.time(
      b <<- geometry_isosurface_cpp(axis, axis, axis, value, 0, FALSE, method))[["elapsed"]], numeric(1))
    angles <- min_angle(b)
    rows[[length(rows) + 1L]] <- data.frame(field = field, n = n, method = method,
      triangles = nrow(b) / 3L, vertices = length(unique(b$.vertex)), seconds = median(times),
      min_angle_median = median(angles), below_10_degrees = mean(angles < 10))
  }
}
result <- do.call(rbind, rows)
print(result, digits = 3, row.names = FALSE)
