test_sphere_space <- function() ggplot2::ggproto("TestSphere", Space3D,
  name = "unit sphere", charts = c("embedded", "lonlat"), models = "embedded", default_model = "embedded",
  model_ranges = function(model, panel_params) list(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)),
  to_canonical = function(data, chart) {
    if (chart == "lonlat") return(cbind(cos(data$x) * cos(data$y), sin(data$x) * cos(data$y), sin(data$y)))
    X <- as.matrix(data[c("x", "y", "z")])
    if (any(abs(rowSums(X^2) - 1) > 1e-8)) stop("Sphere points must have unit norm.")
    X
  },
  from_canonical = function(X, model) X,
  distance = function(X, Y) 2 * atan2(sqrt(rowSums((X - Y)^2)), sqrt(rowSums((X + Y)^2))),
  geodesic = function(self, X, Y, s) {
    theta <- self$distance(X, Y)
    if (any(pi - theta < 1e-10)) stop("Antipodal sphere endpoints need a chosen geodesic.")
    small <- theta < 1e-10
    denominator <- ifelse(small, 1, sin(theta))
    a <- ifelse(small, 1 - s, sin((1 - s) * theta) / denominator)
    b <- ifelse(small, s, sin(s * theta) / denominator)
    a * X + b * Y
  },
  act = function(g, X) X %*% t(g)
)

space_test_plot <- function(backend = "vector", tolerance = 1e-3) {
  d <- data.frame(x = c(1, 0, 0), y = c(0, 1, 0), z = c(0, 0, 1))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_polygon_3d(fill = "#5CA7CE", colour = "#174A6C", linewidth = 0.8) +
    geom_point_3d(size = 2, colour = "#B84034") +
    coord_3d(space = test_sphere_space(), xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1),
      expand = FALSE, axes = character(), panes = character(), box = "none", azimuth = 40, elevation = 25,
      backend = backend_3d(backend, dpi = 72, antialias = 1), munch_tolerance = tolerance)
}
