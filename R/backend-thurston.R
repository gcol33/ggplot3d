utils::globalVariables(c("raymarch_geodesic_cpp", "raymarch_curved_cpp"))

thurston_geometry_3d <- function(geometry) match.arg(tolower(geometry), c("e3", "s3", "h3", "s2xe", "h2xe", "nil", "sol", "sl2r"))

thurston_point_3d <- function(point, geometry, name) {
  if (!is.numeric(point) || length(point) != 3L || any(!is.finite(point))) stop(name, " must have three finite coordinates.", call. = FALSE)
  d <- if (geometry == "h3") 3L else if (geometry == "h2xe") 2L else 0L
  if (d && sum(point[seq_len(d)]^2) >= 1) stop(name, " must lie inside the open unit hyperbolic ball or disk.", call. = FALSE)
  as.double(point)
}

thurston_settings_3d <- function(geometry, observer, facing, fov, max_distance, tolerance, max_steps, quotient, light_size) {
  geometry <- thurston_geometry_3d(geometry)
  observer <- thurston_point_3d(observer, geometry, "observer")
  if (!is.matrix(facing) || !identical(dim(facing), c(3L, 3L)) || any(!is.finite(facing)) ||
      max(abs(crossprod(facing) - diag(3))) > 1e-8 || det(facing) < 0)
    stop("facing must be a positively oriented orthonormal 3 by 3 matrix.", call. = FALSE)
  fov <- check_positive_3d(fov, "fov")
  if (fov >= 180) stop("fov must be below 180 degrees.", call. = FALSE)
  max_distance <- check_positive_3d(max_distance, "max_distance")
  tolerance <- check_positive_3d(tolerance, "tolerance")
  light_size <- check_positive_3d(light_size, "light_size")
  if (!is.numeric(max_steps) || length(max_steps) != 1L || !is.finite(max_steps) || max_steps < 1 ||
      max_steps != trunc(max_steps) || max_steps > .Machine$integer.max) stop("max_steps must be a positive integer.", call. = FALSE)
  if (is.null(quotient)) quotient <- list()
  if (!is.list(quotient) || any(!vapply(quotient, inherits, logical(1), "ggplot3d_portal")))
    stop("quotient must be a list of thurston_portal_3d() faces.", call. = FALSE)
  list(geometry = geometry, observer = observer, facing = facing, fov = fov, max_distance = max_distance,
    tolerance = tolerance, max_steps = as.integer(max_steps), quotient = quotient, light_size = light_size)
}

#' Follow a geodesic in a Thurston geometry
#'
#' Computes unit-speed geodesics, parallel transport and the area density of
#' geodesic spheres using the same kernel as `backend_3d("raymarch")`.
#' @param geometry One of `"e3"`, `"s3"`, `"h3"`, `"s2xe"`, `"h2xe"`,
#'   `"nil"`, `"sol"`, or `"sl2r"`.
#' @param origin Three coordinates in the geometry's chart.
#' @param direction Three components in the orthonormal frame at `origin`.
#'   The direction is normalised to unit length.
#' @param distance Finite signed arc lengths.
#' @details E3 uses Cartesian coordinates. S3 uses stereographic coordinates
#' with metric `4 * (dx^2 + dy^2 + dz^2) / (1 + x^2 + y^2 + z^2)^2`.
#' H3 uses the open Poincare ball, with a minus sign in its denominator.
#' S2xE and H2xE use the corresponding two-dimensional chart and a third,
#' Euclidean coordinate. Nil uses the rotation-invariant metric
#' `dx^2 + dy^2 + (dz - (x * dy - y * dx) / 2)^2`.
#' Sol uses `exp(-2*z) * dx^2 + exp(2*z) * dy^2 + dz^2`.
#' SL2R uses `(x, y, w)`, where `(x, y, sqrt(1+x^2+y^2))` lies on the
#' hyperboloid and `w` is the unwrapped fibre coordinate of the universal cover.
#'
#' Nil, Sol and SL2R use left-invariant orthonormal frames. The other charts
#' use their scaled coordinate frames. Parallel transport maps components in
#' the initial frame to components in the final frame. Sol evaluates Jacobi
#' elliptic functions by the arithmetic-geometric mean algorithm. Near its
#' degenerate directions and at short distances it integrates the reduced
#' geodesic equations with fourth-order Runge-Kutta. Its skew-symmetric
#' transport equation uses fourth-order commutator-free Magnus steps of at most
#' 0.05 arc-length units. Area densities use analytic constant-curvature and
#' product formulas. Nil, Sol and SL2R integrate two transverse Jacobi fields
#' by automatic differentiation of their reduced flow equations, with
#' fourth-order Runge-Kutta steps of at most 0.01 arc-length units.
#' @return A list with position, orthonormal-frame velocity, coordinate velocity,
#'   a row of nine column-major transport entries per distance, and area density.
#' @references Coulon, Matsumoto, Segerman and Trettel (2022).
#'   Ray-marching Thurston geometries. <https://arxiv.org/abs/2010.15801>
#' @examples
#' geodesic_3d("nil", direction = c(1, 0, 1), distance = c(0, 1, 2))
#' @export
geodesic_3d <- function(geometry, origin = c(0, 0, 0), direction = c(0, 0, 1), distance = 1) {
  geometry <- thurston_geometry_3d(geometry)
  origin <- thurston_point_3d(origin, geometry, "origin")
  direction <- thurston_point_3d(direction, "e3", "direction")
  if (sum(direction^2) == 0) stop("direction must be nonzero.", call. = FALSE)
  if (!is.numeric(distance) || any(!is.finite(distance))) stop("distance must be finite numeric arc lengths.", call. = FALSE)
  raymarch_geodesic_cpp(geometry, origin, direction, as.double(distance))
}

#' Pair a face of a Thurston fundamental domain
#' @param normal Four coefficients of the domain face in the geometry's
#'   embedded coordinates. Interior points satisfy `sum(normal * point) < offset`.
#' @param offset Face offset.
#' @param matrix A nonsingular 4 by 4 isometry matrix carrying this face to its
#'   paired face. The differential transports the ray's tangent on crossing.
#' @param lift Fibre displacement for product geometries, or an additional
#'   multiple of `4*pi` selecting a lift of an SL2R isometry.
#' @param fiber Use `normal[4] * w < offset` in the unwrapped fibre coordinate
#'   of SL2R or a product space. The first three normal entries must be zero.
#' @details E3, Nil and Sol embed as `(x,y,z,1)`. S3 and H3 embed on the unit
#'   sphere and hyperboloid with the distinguished fourth coordinate. Product
#'   spaces use the base sphere or hyperboloid in the first three coordinates
#'   and the Euclidean factor in the fourth. SL2R uses the four matrix-basis
#'   coordinates of Coulon et al.; its fibre lift is stored separately.
#'   Quadric and metric preservation are checked at crossings. Faces must
#'   map to the interior side of their partner. Supply every face of the domain.
#' @return A face descriptor for the `quotient` argument of [backend_3d()].
#' @examples
#' m <- diag(4); m[1, 4] <- -2
#' thurston_portal_3d(c(1, 0, 0, 0), 1, m)
#' @export
thurston_portal_3d <- function(normal, offset, matrix, lift = 0, fiber = FALSE) {
  if (!is.numeric(normal) || length(normal) != 4L || any(!is.finite(normal)) || sum(normal^2) == 0)
    stop("normal must have four finite, nonzero coefficients.", call. = FALSE)
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset) ||
      !is.numeric(lift) || length(lift) != 1L || !is.finite(lift)) stop("offset and lift must be finite scalars.", call. = FALSE)
  if (!is.matrix(matrix) || !identical(dim(matrix), c(4L, 4L)) || any(!is.finite(matrix)) || abs(det(matrix)) < 1e-12)
    stop("matrix must be a nonsingular finite 4 by 4 matrix.", call. = FALSE)
  if (!is.logical(fiber) || length(fiber) != 1L || is.na(fiber) || (fiber && any(normal[1:3] != 0)))
    stop("fiber must be TRUE or FALSE; fibre normals use only the fourth entry.", call. = FALSE)
  structure(list(normal = as.double(normal), offset = offset, matrix = matrix, lift = lift, fiber = fiber), class = "ggplot3d_portal")
}

thurston_scene_3d <- function(specs, viewport, backend, light) {
  fields <- lapply(specs, function(s) {
    if (!(s$kind %in% c("sdf", "thurston"))) stop("Intrinsic rendering requires SDF or geom_thurston_3d() surfaces.", call. = FALSE)
    if (s$kind == "thurston") {
      thurston_point_3d(s$center, backend$geometry, "center")
      if (backend$geometry == "s3" && s$shape < 2L && s$radius >= pi / (s$shape + 1L)) stop("S3 radius exceeds the primitive's injective range.", call. = FALSE)
      if (backend$geometry %in% c("s2xe", "h2xe") && s$shape == 2L && abs(s$direction[3L]) > 0 && any(s$direction[1:2] != 0))
        stop("Product half-spaces need a horizontal or vertical normal.", call. = FALSE)
    }
    s$rgb <- as.vector(srgb_decode_3d(colour_channels_3d(s$fill) / 255)); s
  })
  width <- max(1L, as.integer(ceiling(viewport$width_mm / 25.4 * viewport$dpi)))
  height <- max(1L, as.integer(ceiling(viewport$height_mm / 25.4 * viewport$dpi)))
  aa <- backend$antialias
  rays <- expand.grid(y = seq_len(height * aa), x = seq_len(width * aa))
  angle <- tan(backend$fov * pi / 360)
  directions <- cbind((2 * (rays$x - 0.5) / (width * aa) - 1) * angle * width / height,
    (1 - 2 * (rays$y - 0.5) / (height * aa)) * angle, 1) %*% t(backend$facing)
  hit <- raymarch_curved_cpp(fields, directions, backend, raster_threads_3d())
  light <- light %or3d% light_3d()
  cosine <- pmax(0, -rowSums(hit$normal * hit$incoming))
  intensity <- light$ambient + light$diffuse * cosine / (hit$area + backend$light_size^2)
  result <- list(image = structure(matrix(0L, height, width), class = "nativeRaster"),
    width = width, height = height, extent = c(0, 0, 1, 1), intrinsic = hit)
  rgba <- cbind(pmin(hit$colour * intensity, 1), as.double(is.finite(hit$distance)))
  rgba[!is.finite(hit$distance), ] <- 0
  pixel <- ceiling(rays$y / aa) + (ceiling(rays$x / aa) - 1L) * height
  rgba <- rowsum(rgba, pixel, reorder = TRUE) / aa^2
  take <- which(rgba[, 4L] > 0)
  raymarch_paint_3d(result, rgba[take, 1:3, drop = FALSE], rgba[take, 4L], take)
}

render_thurston_panel_3d <- function(x, viewport, backend) {
  if (length(x$overlay)) stop("Intrinsic views require annotations in the geometry's coordinates.", call. = FALSE)
  specs <- Filter(Negate(is.null), lapply(x$packets, attr, "raymarch"))
  ordinary <- Filter(function(p) is.null(attr(p, "raymarch")), x$packets)
  if (length(ordinary) && any(vapply(ordinary, function(p) length(p$faces$offset) + length(p$polylines$offset) + length(p$markers$style_id) + length(p$text$style_id) > 0L, logical(1))))
    stop("In-space views accept geom_thurston_3d() primitives and traced SDF layers. Express points as balls, paths as SDF capsules, and planes as half-spaces.", call. = FALSE)
  result <- thurston_scene_3d(specs, viewport, backend, x$panel_params$coord_params$light)
  grid::gTree(children = grid::gList(raster_image_grob_3d(result)), raster_result = result, name = "scene-thurston-3d")
}
