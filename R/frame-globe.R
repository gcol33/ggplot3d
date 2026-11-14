#' @include space-3d.R frame-3d.R
NULL

sphere_angle_3d <- function(X, Y) {
  2 * atan2(sqrt(rowSums((X - Y)^2)), sqrt(rowSums((X + Y)^2)))
}

sphere_slerp_3d <- function(X, Y, s) {
  theta <- sphere_angle_3d(X, Y)
  if (any(pi - theta < 1e-10)) stop("Antipodal endpoints require an intermediate point to choose a great circle.", call. = FALSE)
  small <- theta < 1e-10
  den <- ifelse(small, 1, sin(theta))
  out <- ifelse(small, 1 - s, sin((1 - s) * theta) / den) * X +
    ifelse(small, s, sin(s * theta) / den) * Y
  out / sqrt(rowSums(out^2))
}

lonlat_xyz_3d <- function(lon, lat) {
  lon <- lon * pi / 180; lat <- lat * pi / 180
  cbind(cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat))
}

SpaceGlobe <- ggplot2::ggproto("SpaceGlobe", Space3D,
  name = "globe", charts = "lonlat", models = "embedded", default_model = "embedded",
  radius = 1, exaggeration = 1,
  to_canonical = function(self, data, chart) {
    if (any(abs(data$y) > 90)) stop("Latitude must lie in [-90, 90] degrees.", call. = FALSE)
    r <- self$radius + self$exaggeration * data$z
    if (any(r <= 0)) stop("Globe heights must stay above the centre.", call. = FALSE)
    cbind(lonlat_xyz_3d(data$x, data$y), r)
  },
  from_canonical = function(X, model) X[, 1:3, drop = FALSE] * X[, 4L],
  distance = function(self, X, Y) sqrt((self$radius * sphere_angle_3d(X[, 1:3, drop = FALSE], Y[, 1:3, drop = FALSE]))^2 + (X[, 4L] - Y[, 4L])^2),
  geodesic = function(X, Y, s) cbind(sphere_slerp_3d(X[, 1:3, drop = FALSE], Y[, 1:3, drop = FALSE], s), (1 - s) * X[, 4L] + s * Y[, 4L]),
  act = function(g, X) {
    if (!is.matrix(g) || !identical(dim(g), c(3L, 3L)) || any(!is.finite(g)) || max(abs(crossprod(g) - diag(3))) > 1e-10) stop("Globe isometry must be an orthogonal 3 by 3 matrix.", call. = FALSE)
    cbind(X[, 1:3, drop = FALSE] %*% t(g), X[, 4L])
  },
  model_ranges = function(self, model, panel_params) {
    r <- max(self$radius, self$radius + self$exaggeration * panel_params$ranges$z)
    stats::setNames(rep(list(c(-r, r)), 3L), c("x", "y", "z"))
  }
)

FrameGlobe <- ggplot2::ggproto("FrameGlobe", Frame3D,
  name = "globe", linear = FALSE, space_frame = FrameBox,
  radius = 1, exaggeration = 1, graticule = c(30, 30), inner = TRUE,
  bind = function(self, panel_params) ggplot2::ggproto(NULL, self,
    ranges = panel_params$ranges, model_ranges = self$space$model_ranges("embedded", panel_params)),
  map = function(self, xyz) {
    d <- sweep(sweep(xyz, 2L, vapply(self$ranges, diff, numeric(1)), "*"), 2L, vapply(self$ranges, `[`, numeric(1), 1L), "+")
    X <- self$space$to_canonical(as.data.frame(stats::setNames(as.data.frame(d), c("x", "y", "z"))), "lonlat")
    do.call(cbind, space_packet_coordinates_3d(X, list(space = self$space, model = "embedded", ranges = self$model_ranges)))
  },
  panes = function() list(), edges = function() list(),
  bounds = function() cube_edges_3d()$corners,
  axis_candidates = function(self, guide) {
    to <- function(x) (x - vapply(self$ranges, `[`, numeric(1), 1L)) / vapply(self$ranges, diff, numeric(1))
    candidates <- switch(guide$axis,
      x = list(list(start = to(c(-180, 0, 0)), end = to(c(180, 0, 0)))),
      y = list(list(start = to(c(0, -90, 0)), end = to(c(0, 90, 0)))),
      z = list(list(start = to(c(0, 0, self$ranges$z[1L])), end = to(c(0, 0, self$ranges$z[2L])))))
    attr(candidates, "auto") <- "lower"
    candidates
  },
  scene_packets = function(self, panel_params, theme) {
    packets <- list()
    emit <- function(d, geom, ...) emit_scene_3d(d, c(list(.emitter = geom, shade = TRUE), list(...)), panel_params, scene_furniture_layer_3d)
    paint <- function(d, colour, fill = NA_character_, linewidth = 0.3) {
      d$colour <- colour; d$fill <- fill; d$linewidth <- linewidth; d$linetype <- 1; d$alpha <- 1; d
    }
    if (self$inner) {
      uv <- expand.grid(x = seq(-180, 180, length.out = 37), y = seq(-90, 90, length.out = 19))
      tri <- lattice_triangles_3d(surface_lattice_3d(uv), rep(0, nrow(uv)))
      xyz <- lonlat_xyz_3d(uv$x, uv$y)
      area <- sqrt(rowSums(cross_rows_3d(xyz[tri[,2L],] - xyz[tri[,1L],], xyz[tri[,3L],] - xyz[tri[,1L],])^2))
      tri <- tri[area > 1e-10,,drop=FALSE]
      d <- as.data.frame(self$radius * xyz[as.vector(t(tri)), ])
      names(d) <- c("x", "y", "z")
      d[c("nx", "ny", "nz")] <- as.data.frame(xyz[as.vector(t(tri)), ])
      d$.face <- rep(seq_len(nrow(tri)), each = 3L); d$.corner <- rep(1:3, nrow(tri)); d$.vertex <- as.vector(t(tri)); d$.mesh <- 1L; d$group <- 1L
      packets[[length(packets) + 1L]] <- emit_model_scene_3d(paint(d, NA_character_, "#E0E7EB"),
        list(.emitter = "GeomMesh3D", shade = TRUE), panel_params, self$model_ranges)
    }
    if (all(self$graticule > 0)) {
      lines <- c(lapply(seq(-180, 180 - 1e-8, by = self$graticule[1L]), function(lon) data.frame(x = lon, y = seq(-90, 90, by = 5))),
        lapply(seq(-90 + self$graticule[2L], 90 - 1e-8, by = self$graticule[2L]), function(lat) data.frame(x = seq(-180, 180, by = 5), y = lat)))
      lines <- lapply(seq_along(lines), function(i) {d <- lines[[i]]; d$z <- self$radius * 1e-5 / self$exaggeration; d$group <- i; d})
      if (length(lines)) packets[[length(packets) + 1L]] <- emit(paint(do.call(rbind, lines), "#708A98"), "GeomPath")
    }
    packets
  }
)

#' Longitude, latitude and height on a globe
#'
#' Positions are longitude and latitude in degrees and height above a sphere.
#' Paths follow the shorter great circle and interpolate height linearly.
#' Supply an intermediate point for antipodal endpoints. Polygon edges and
#' interiors use the shared geodesic sampler. Simple features are transformed
#' to EPSG:4326. Heights of two-dimensional sf features default to zero.
#' Data-space lighting illuminates the opaque sphere and its day/night boundary.
#' @param radius Positive sphere radius, in height units.
#' @param exaggeration Positive height multiplier.
#' @param graticule Longitude and latitude spacing in degrees; zero disables it.
#' @param centre Longitude and latitude facing the camera, in degrees.
#' @param inner Draw an opaque sphere hiding the far side.
#' @param ... Other arguments to [coord_3d()].
#' @return A Coord3D object with a globe frame.
#' @references GeoMakie: <https://geo.makie.org/stable/>.
#' @examples
#' route <- data.frame(lon = c(-74, 140), lat = c(41, 36), height = 0.02)
#' ggplot2::ggplot(route, ggplot2::aes(lon, lat, z = height)) +
#'   geom_path_3d(colour = "#B54532", linewidth = 1) +
#'   coord_globe_3d(centre = c(-150, 35))
#' @export
coord_globe_3d <- function(radius = 1, exaggeration = 1, graticule = c(30, 30),
                           centre = c(0, 20), inner = TRUE, ...) {
  radius <- mesh_number_3d(radius, "radius", function(x) is.finite(x) && x > 0, "positive")
  exaggeration <- mesh_number_3d(exaggeration, "exaggeration", function(x) is.finite(x) && x > 0, "positive")
  if (!is.numeric(graticule) || length(graticule) != 2L || any(!is.finite(graticule) | graticule < 0)) stop("graticule must contain two nonnegative degree spacings.", call. = FALSE)
  if (!is.numeric(centre) || length(centre) != 2L || any(!is.finite(centre)) || abs(centre[2L]) > 90) stop("centre must give longitude and latitude in degrees.", call. = FALSE)
  if (!is.logical(inner) || length(inner) != 1L || is.na(inner)) stop("inner must be TRUE or FALSE.", call. = FALSE)
  space <- ggplot2::ggproto(NULL, SpaceGlobe, radius = radius, exaggeration = exaggeration)
  args <- stats_params_3d(list(space = space, input = "lonlat", azimuth = centre[1L], elevation = centre[2L],
    xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, clip_data = FALSE,
    axes = "z", panes = character(), box = "none", light = light_3d(space = "data")), list(...))
  coord <- do.call(coord_3d, args)
  coord$frame <- ggplot2::ggproto(NULL, FrameGlobe, radius = radius, exaggeration = exaggeration,
    space = space, graticule = graticule, inner = inner)
  coord
}
