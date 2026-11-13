utils::globalVariables("mesh_process_cpp")

mesh_key_columns_3d <- c("x", "y", "z", ".mesh", ".face", ".corner", ".vertex", "subgroup")

# Columns computed from the geometry by a mesh verb; a verb that changes the
# geometry recomputes the sets its input carried.
mesh_derived_3d <- list(
  normals = list(columns = c("nx", "ny", "nz"), apply = function(m) mesh_normals(m)),
  curvature = list(columns = c("gaussian_curvature", "mean_curvature"), apply = function(m) mesh_curvature(m))
)

mesh_input_3d <- function(m) {
  if (inherits(m, "mesh3d")) m <- ggplot2::fortify(m)
  if (!is.data.frame(m) || !all(c("x", "y", "z", ".face", ".corner", ".vertex") %in% names(m)))
    stop("Mesh input requires keyed corners; use mesh_3d(vertices, faces).", call. = FALSE)
  m
}

mesh_same_3d <- function(x, y) all((x == y & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y)))

# Indexed arrays of one mesh part (rows of a corner table with one .mesh):
# unique vertices in order of first use, polygon faces triangulated (quads
# along the shorter diagonal, larger polygons and faces with rings by ear
# clipping), and the role of every attribute column. A column constant per
# vertex is a vertex attribute (double columns are interpolated as channels,
# others follow the source vertex), else a column constant per face is a
# face attribute; any other double column is averaged per vertex and other
# columns take the vertex's first corner.
mesh_arrays_3d <- function(d) {
  vertex_ids <- unique(d$.vertex); vi <- match(d$.vertex, vertex_ids); vertex_rows <- match(vertex_ids, d$.vertex)
  face_ids <- unique(d$.face); fi <- match(d$.face, face_ids); face_rows <- match(face_ids, d$.face)
  xyz <- matrix(as.double(as.matrix(d[vertex_rows, c("x", "y", "z")])), ncol = 3L)
  if (any(!is.finite(xyz))) stop("Mesh positions must be finite.", call. = FALSE)
  counts <- tabulate(fi, length(face_ids)); starts <- cumsum(c(1L, utils::head(counts, -1L)))
  if (any(counts < 3L)) stop("Mesh faces need at least three corners.", call. = FALSE)
  rings <- if (is.null(d$subgroup)) rep(1L, length(face_ids)) else
    vapply(split(d$subgroup, fi), function(s) length(unique(s)), integer(1))
  plain <- rings == 1L & counts <= 4L
  corners <- function(f, k) starts[f] + k - 1L
  t3 <- which(plain & counts == 3L)
  tri <- cbind(corners(t3, 1L), corners(t3, 2L), corners(t3, 3L)); parent <- t3
  q <- which(plain & counts == 4L)
  if (length(q)) {
    qr <- cbind(corners(q, 1L), corners(q, 2L), corners(q, 3L), corners(q, 4L))
    span <- function(a, b) rowSums((xyz[vi[qr[, a]], , drop = FALSE] - xyz[vi[qr[, b]], , drop = FALSE])^2)
    first <- span(1L, 3L) <= span(2L, 4L)
    tri <- rbind(tri, cbind(qr[, 1L], qr[, 2L], ifelse(first, qr[, 3L], qr[, 4L])),
      cbind(ifelse(first, qr[, 1L], qr[, 2L]), qr[, 3L], qr[, 4L]))
    parent <- c(parent, q, q)
  }
  for (f in which(!plain)) {
    r <- seq.int(starts[f], length.out = counts[f])
    t <- mesh_face_triangles_3d(d, r, split(seq_along(r), d$subgroup[r] %or3d% rep(1L, length(r))))
    tri <- rbind(tri, t); parent <- c(parent, rep(f, nrow(t)))
  }
  vt <- matrix(vi[tri], ncol = 3L)
  roles <- character(); channels <- list()
  for (name in setdiff(names(d), c(mesh_key_columns_3d, unlist(lapply(mesh_derived_3d, `[[`, "columns"))))) {
    x <- d[[name]]
    if (is.list(x)) { roles[name] <- "face"; next }
    if (mesh_same_3d(x, x[vertex_rows[vi]])) roles[name] <- if (is.double(x)) "channel" else "vertex"
    else if (mesh_same_3d(x, x[face_rows[fi]])) roles[name] <- "face"
    else roles[name] <- if (is.double(x)) "channel" else "vertex"
    if (roles[name] == "channel") channels[[name]] <- as.double(rowsum(as.double(x), vi, reorder = TRUE)[, 1L] / tabulate(vi))
  }
  list(xyz = xyz, triangles = matrix(as.integer(vt), ncol = 3L), parent = as.integer(parent), vi = vi, fi = fi,
    vertex_rows = vertex_rows, face_rows = face_rows, roles = roles,
    channels = if (length(channels)) do.call(cbind, channels) else matrix(0, nrow(xyz), 0L))
}

mesh_restore_3d <- function(x, values) {
  if (is.null(attributes(x))) return(values)
  y <- x[rep(1L, length(values))]; y[] <- values; y
}

# Corner table of a verb's result: positions and channels from the result,
# vertex attributes from each vertex's source vertex, face attributes from
# each triangle's parent face.
mesh_assemble_3d <- function(d, a, res) {
  nt <- nrow(res$triangles); v <- as.vector(t(res$triangles)); f <- rep(seq_len(nt), each = 3L)
  source_row <- a$vertex_rows[res$source[v]]; parent_row <- a$face_rows[res$parent[f]]
  out <- list()
  for (name in setdiff(names(d), c("subgroup", unlist(lapply(mesh_derived_3d, `[[`, "columns"))))) {
    out[[name]] <- switch(name,
      x = res$xyz[v, 1L], y = res$xyz[v, 2L], z = res$xyz[v, 3L],
      .face = f, .corner = rep.int(1:3, nt), .vertex = v, .mesh = rep(d$.mesh[1L], 3L * nt),
      switch(a$roles[[name]],
        channel = mesh_restore_3d(d[[name]], res$channels[v, match(name, colnames(a$channels))]),
        vertex = d[[name]][source_row],
        face = d[[name]][parent_row]))
  }
  structure(out, class = "data.frame", row.names = .set_row_names(3L * nt))
}

# Runs a verb on every .mesh of a corner table. A rebuilding verb returns a
# new table; otherwise the verb's corner and vertex values are joined onto
# the input corners by face and vertex.
mesh_run_3d <- function(m, verb, args, rebuild) {
  m <- mesh_input_3d(m)
  attributes_in <- attributes(m)
  d <- as.data.frame(m)
  tagged <- is.null(d$.mesh)
  if (tagged) d$.mesh <- 1L
  d <- d[order(d$.mesh, d$.face, d$.corner), , drop = FALSE]
  parts <- split(seq_len(nrow(d)), d$.mesh)
  results <- lapply(parts, function(rows) {
    part <- d[rows, , drop = FALSE]
    a <- mesh_arrays_3d(part)
    res <- mesh_process_cpp(a$xyz, a$triangles, a$parent, a$channels, verb, args)
    if (rebuild) return(mesh_assemble_3d(part, a, res))
    v <- as.vector(t(res$triangles)); f <- rep(seq_len(nrow(res$triangles)), each = 3L)
    values <- if (ncol(res$corner)) res$corner else res$vertex[v, , drop = FALSE]
    key <- function(face, vertex) face * (nrow(a$xyz) + 1) + vertex
    index <- match(key(a$fi, a$vi), key(res$parent[f], res$source[v]))
    for (name in colnames(values)) part[[name]] <- values[index, name]
    part
  })
  out <- do.call(rbind, unname(results))
  if (tagged) out$.mesh <- NULL
  rownames(out) <- NULL
  if (rebuild) for (set in mesh_derived_3d) if (all(set$columns %in% names(d))) out <- set$apply(out)
  class(out) <- attributes_in$class
  out
}

mesh_count_3d <- function(x, name, minimum = 0L) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < minimum || x != trunc(x))
    stop(name, " must be an integer of at least ", minimum, ".", call. = FALSE)
  as.integer(x)
}

mesh_number_3d <- function(x, name, test = function(x) TRUE, message = "a finite number") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !test(x)) stop(name, " must be ", message, ".", call. = FALSE)
  as.double(x)
}

#' Mesh processing
#'
#' Verbs on a [mesh_3d()] corner table (also an rgl `mesh3d` or any data frame
#' with x, y, z, `.face`, `.corner` and `.vertex`, optionally split into parts
#' by `.mesh`). They work on one half-edge representation of each part:
#' polygon faces are triangulated (quads along the shorter diagonal), faces
#' with a repeated corner are dropped, faces are wound consistently within
#' each connected component (with a warning when some had to be reversed),
#' and a vertex where several fans of faces meet only at a point is split
#' into one vertex per fan. An edge shared by more than two faces, or a
#' non-orientable surface, is an error.
#'
#' `mesh_normals()` sets `nx`, `ny`, `nz` per corner to the angle-weighted
#' mean of the face normals around the vertex (Thuermer and Wuethrich 1998),
#' taken separately on each side of an edge whose faces meet at more than
#' `crease` degrees and of a boundary edge; `crease = 180` gives one smooth
#' normal per vertex.
#'
#' `mesh_curvature()` adds `gaussian_curvature` (angle defect over the mixed
#' Voronoi area) and `mean_curvature` (half the norm of the cotangent
#' Laplacian) at every vertex (Meyer et al. 2003). Mean curvature is signed
#' against the vertex normal of the face winding, negative where the surface
#' bends away from its normal, so a sphere wound outwards has mean curvature
#' -1/r, as in [stat_parametric_3d()]. Boundary vertices are `NA`.
#'
#' `mesh_subdivide()` splits every triangle into four, `n` times. `"loop"`
#' places the points with Loop's (1987) masks, approximating a smooth
#' surface; boundary curves follow the cubic B-spline rule. `"midpoint"`
#' keeps the triangles' planes.
#'
#' `mesh_smooth()` runs Taubin's (1995) lambda/mu smoothing: each iteration
#' moves every interior vertex by `lambda` and then by `mu` times the vector
#' to the mean of its neighbours; `mu < -lambda` stops the shrinking of plain
#' Laplacian smoothing. Boundary vertices stay fixed.
#'
#' `mesh_decimate()` collapses edges by the quadric error metric of Garland
#' and Heckbert (1997) until the mesh has `target` faces (a fraction of the
#' triangle count when below one) or the next collapse would move the surface
#' by more than `max_error`. Vertex quadrics sum the planes of their faces
#' weighted by area; the error of a collapse is the area-weighted root mean
#' squared distance of the merged vertex from these planes, at the position
#' that minimises it (with directions of small curvature truncated, as in
#' Lindstrom 2000). Boundary edges add perpendicular planes weighted by
#' `boundary_weight` times their squared length. A collapse is refused when
#' it would make the surface non-manifold or turn a face normal by more than
#' 90 degrees.
#'
#' `mesh_remesh()` makes edges close to length `edge` by the isotropic
#' remeshing of Botsch and Kobbelt (2004): split edges longer than 4/3 `edge`,
#' collapse edges shorter than 4/5 `edge`, flip edges towards valence six,
#' relax vertices tangentially and project them back onto the input surface.
#' Boundary vertices are kept.
#'
#' Rebuilding verbs (all but `mesh_normals()` and `mesh_curvature()`) carry
#' attributes over: double columns constant per vertex are interpolated,
#' other vertex columns follow the vertex each new vertex derives from, and
#' columns constant per face follow the input face each new face lies in.
#' When the input has normals or curvature, they are recomputed.
#' @param m A mesh: a [mesh_3d()] corner table or a compatible data frame.
#' @param crease Dihedral angle in degrees above which an edge splits the
#'   normals of its vertices.
#' @param n Number of subdivision rounds.
#' @param scheme `"loop"` or `"midpoint"`.
#' @param iterations Number of smoothing or remeshing iterations.
#' @param lambda,mu Taubin step factors, `lambda > 0 > mu`.
#' @param target Number of faces to keep, or a fraction of the triangle count.
#' @param max_error Largest root mean squared distance a collapse may cause,
#'   in the units of the positions.
#' @param boundary_weight Weight of the planes that hold boundary edges.
#' @param edge Target edge length; NULL uses the mean edge length.
#' @return A mesh of the class of `m`.
#' @references
#'   Thuermer and Wuethrich (1998). Computing vertex normals from polygonal
#'   facets. Journal of Graphics Tools 3, 43-46. \doi{10.1080/10867651.1998.10487487}
#'
#'   Meyer, Desbrun, Schroeder and Barr (2003). Discrete differential-geometry
#'   operators for triangulated 2-manifolds. Visualization and Mathematics
#'   III, 35-57. \doi{10.1007/978-3-662-05105-4_2}
#'
#'   Loop (1987). Smooth subdivision surfaces based on triangles. MSc thesis,
#'   University of Utah.
#'   <https://www.microsoft.com/en-us/research/publication/smooth-subdivision-surfaces-based-on-triangles/>
#'
#'   Taubin (1995). A signal processing approach to fair surface design.
#'   SIGGRAPH '95, 351-358. \doi{10.1145/218380.218473}
#'
#'   Garland and Heckbert (1997). Surface simplification using quadric error
#'   metrics. SIGGRAPH '97, 209-216. \doi{10.1145/258734.258849}
#'
#'   Lindstrom (2000). Out-of-core simplification of large polygonal models.
#'   SIGGRAPH 2000, 259-262. \doi{10.1145/344779.344912}
#'
#'   Botsch and Kobbelt (2004). A remeshing approach to multiresolution
#'   modeling. Symposium on Geometry Processing, 185-192. \doi{10.2312/SGP/SGP04/189-196}
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 12),
#'                  y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
#' d$value <- with(d, x^2 + y^2 + z^2)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value))
#' sphere <- ggplot2::layer_data(p + stat_isosurface_3d(levels = 0.6) + coord_3d())
#' sphere <- sphere[c("x", "y", "z", ".face", ".corner", ".vertex")]
#' coarse <- mesh_decimate(sphere, 0.25)
#' smooth <- mesh_smooth(mesh_subdivide(coarse), iterations = 5)
#' curved <- mesh_curvature(smooth)
#' summary(curved$mean_curvature)
#' ggplot2::ggplot() +
#'   geom_mesh_3d(data = curved, ggplot2::aes(fill = mean_curvature)) + coord_3d()
#' @name mesh_processing
NULL

#' @rdname mesh_processing
#' @export
mesh_normals <- function(m, crease = 30) {
  crease <- mesh_number_3d(crease, "crease", function(x) x >= 0 && x <= 180, "an angle between 0 and 180")
  mesh_run_3d(m, "normals", list(crease = crease), rebuild = FALSE)
}

#' @rdname mesh_processing
#' @export
mesh_curvature <- function(m) mesh_run_3d(m, "curvature", list(), rebuild = FALSE)

#' @rdname mesh_processing
#' @export
mesh_subdivide <- function(m, n = 1L, scheme = c("loop", "midpoint")) {
  n <- mesh_count_3d(n, "n")
  mesh_run_3d(m, "subdivide", list(n = n, scheme = match.arg(scheme)), rebuild = TRUE)
}

#' @rdname mesh_processing
#' @export
mesh_smooth <- function(m, iterations = 10L, lambda = 0.5, mu = -0.53) {
  iterations <- mesh_count_3d(iterations, "iterations")
  lambda <- mesh_number_3d(lambda, "lambda", function(x) is.finite(x) && x > 0, "positive")
  mu <- mesh_number_3d(mu, "mu", function(x) is.finite(x) && x < 0, "negative")
  mesh_run_3d(m, "smooth", list(iterations = iterations, lambda = lambda, mu = mu), rebuild = TRUE)
}

#' @rdname mesh_processing
#' @export
mesh_decimate <- function(m, target = 0.5, max_error = Inf, boundary_weight = 100) {
  target <- mesh_number_3d(target, "target", function(x) x > 0 && is.finite(x), "a positive face count or fraction")
  max_error <- mesh_number_3d(max_error, "max_error", function(x) x >= 0, "nonnegative")
  boundary_weight <- mesh_number_3d(boundary_weight, "boundary_weight", function(x) is.finite(x) && x >= 0, "nonnegative")
  m <- mesh_input_3d(m)
  faces <- if (target < 1) {
    counts <- table(paste(m$.mesh %or3d% 1L, m$.face))
    target * sum(pmax(1L, counts - 2L))
  } else target
  mesh_run_3d(m, "decimate", list(target = as.integer(max(1, round(faces))), max_error = max_error,
    boundary_weight = boundary_weight), rebuild = TRUE)
}

#' @rdname mesh_processing
#' @export
mesh_remesh <- function(m, edge = NULL, iterations = 10L) {
  iterations <- mesh_count_3d(iterations, "iterations")
  m <- mesh_input_3d(m)
  if (is.null(edge)) {
    rows <- order(m$.mesh %or3d% 1L, m$.face, m$.corner)
    nxt <- ave(rows, (m$.mesh %or3d% 1L)[rows], m$.face[rows], FUN = function(r) c(r[-1L], r[1L]))
    edge <- mean(sqrt((m$x[rows] - m$x[nxt])^2 + (m$y[rows] - m$y[nxt])^2 + (m$z[rows] - m$z[nxt])^2))
  }
  edge <- mesh_number_3d(edge, "edge", function(x) is.finite(x) && x > 0, "a positive length")
  mesh_run_3d(m, "remesh", list(edge = edge, iterations = iterations), rebuild = TRUE)
}
