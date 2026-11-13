utils::globalVariables("outline_lines_cpp")

#' Feature lines of surfaces and meshes
#'
#' `outline_3d()` describes the lines a surface or mesh layer derives from
#' its own triangles, passed as its `outline` argument: silhouettes, where
#' the surface turns away from the camera, creases, where neighbouring faces
#' meet at a sharp angle, and boundaries, the edges of open surfaces
#' (Bénard and Hertzmann 2019). They are drawn as strokes of the scene, so
#' they are hidden by nearer surfaces like any other line, and `hidden`
#' chooses whether their hidden parts are left out, dashed or drawn.
#'
#' An edge is a boundary when it belongs to one triangle and a crease when
#' the normals of its two triangles differ by more than `crease` degrees.
#' Silhouettes depend on the view and are recomputed from the panel's camera
#' when the plot is built. On a mesh (`silhouette = "mesh"`) they are the
#' edges between a triangle facing the camera and one facing away; they
#' follow the edges, so they zigzag across a finely curved surface. On a
#' smooth surface (`silhouette = "smooth"`) they are the interpolated
#' contours of Hertzmann and Zorin (2000): the zero set of n . (c - p), the
#' vertex normal against the direction to the camera, interpolated linearly
#' along each edge, which crosses the faces as a smooth curve. Because that
#' curve lies on the triangles a little inside the mesh's own outline, nearer
#' triangles can hide short pieces of it. `silhouette = TRUE` takes the
#' smooth contour for layers with vertex normals (`nx`, `ny`, `nz`, as grid,
#' parametric and isosurface layers have) and the mesh silhouette otherwise.
#' In the interactive viewer, silhouettes stay those of the initial camera.
#' @param silhouette `TRUE`, `FALSE`, `"smooth"` or `"mesh"`.
#' @param crease Crease angle in degrees, or `NULL` for no creases.
#' @param boundary Whether to draw boundary edges.
#' @param colour,linewidth,linetype,alpha Stroke of the lines.
#' @param hidden Hidden stroke policy of the lines: `"hide"`, `"dashed"` or
#'   `"show"`.
#' @return An outline specification for the `outline` argument of
#'   [geom_surface_3d()], [geom_mesh_3d()] and the surface stats.
#' @references Bénard and Hertzmann (2019). Line drawings from 3D models: a
#'   tutorial. Foundations and Trends in Computer Graphics and Vision 11,
#'   1-159. \doi{10.1561/0600000075}
#'
#'   Hertzmann and Zorin (2000). Illustrating smooth surfaces. SIGGRAPH 2000,
#'   517-526. \doi{10.1145/344779.345074}
#' @examples
#' torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u),
#'   y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
#' ggplot2::ggplot() +
#'   geom_parametric_3d(fun = torus, n = c(48, 24), closed = TRUE, fill = "white",
#'     outline = outline_3d(linewidth = 0.8)) +
#'   coord_3d()
#' cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
#' quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4),
#'   c(1, 5, 7, 3), c(2, 4, 8, 6))
#' ggplot2::ggplot() +
#'   geom_mesh_3d(vertices = cube, faces = quads, fill = "grey90",
#'     outline = outline_3d(hidden = "dashed")) +
#'   coord_3d()
#' @export
outline_3d <- function(silhouette = TRUE, crease = 30, boundary = TRUE, colour = "black", linewidth = 0.5,
                       linetype = 1, alpha = 1, hidden = c("hide", "dashed", "show")) {
  if (isTRUE(silhouette)) silhouette <- "auto"
  else if (isFALSE(silhouette)) silhouette <- "none"
  if (!is.character(silhouette) || length(silhouette) != 1L || !silhouette %in% c("auto", "none", "smooth", "mesh"))
    stop("silhouette must be TRUE, FALSE, \"smooth\" or \"mesh\".", call. = FALSE)
  if (!is.null(crease)) crease <- mesh_number_3d(crease, "crease", function(x) is.finite(x) && x >= 0 && x <= 180, "an angle between 0 and 180")
  if (!is.logical(boundary) || length(boundary) != 1L || is.na(boundary)) stop("boundary must be TRUE or FALSE.", call. = FALSE)
  linewidth <- mesh_number_3d(linewidth, "linewidth", function(x) is.finite(x) && x >= 0, "nonnegative")
  alpha <- mesh_number_3d(alpha, "alpha", function(x) is.finite(x) && x >= 0 && x <= 1, "between 0 and 1")
  if (length(colour) != 1L) stop("colour must be a single colour.", call. = FALSE)
  grDevices::col2rgb(colour)
  structure(list(silhouette = silhouette, crease = crease, boundary = boundary, colour = colour,
    linewidth = linewidth, linetype = linetype, alpha = alpha, hidden = match.arg(hidden)), class = "ggplot3d_outline")
}

# Adds the feature lines of params$outline to a packet of triangles: faces
# holds rows of data, three per triangle. Rows share a vertex when they
# share .mesh, group and .vertex, or group and .source without .vertex.
outline_merge_3d <- function(packet, data, faces, params, panel_params) {
  spec <- params$outline
  if (is.null(spec)) return(packet)
  if (!inherits(spec, "ggplot3d_outline")) stop("outline must be created by outline_3d().", call. = FALSE)
  if (!nrow(faces)) return(packet)
  key <- if (!is.null(data$.vertex)) paste(data$.mesh %or3d% 1L, data$group %or3d% 1L, data$.vertex)
    else paste(data$group %or3d% 1L, data$.source)
  ids <- match(key, unique(key)); rows <- match(seq_len(max(ids)), ids)
  xyz <- matrix(as.double(as.matrix(data[rows, c("x", "y", "z")])), ncol = 3L)
  frame <- frame_of_3d(panel_params)
  model <- if (isTRUE(frame$identity)) xyz else frame$map(xyz)
  camera <- panel_params$camera %or3d% new_camera_3d()
  basis <- frame_basis_3d(camera, panel_params$viewport %or3d% new_scene_viewport(100, 100))
  view <- sweep(sweep(model, 2L, camera$target), 2L, basis$scale, "*")
  silhouette <- spec$silhouette
  normals <- matrix(0, nrow(xyz), 0L)
  has_normals <- all(c("nx", "ny", "nz") %in% names(data)) && isTRUE(frame$identity)
  if (has_normals) {
    normals <- sweep(matrix(as.double(as.matrix(data[rows, c("nx", "ny", "nz")])), ncol = 3L), 2L, basis$scale, "/")
    normals[!is.finite(normals)] <- 0
  }
  if (silhouette == "auto") silhouette <- if (has_normals) "smooth" else "mesh"
  lines <- outline_lines_cpp(xyz, view, normals, matrix(ids[faces], ncol = 3L),
    if (basis$perspective) -basis$distance * basis$forward else basis$ray, basis$perspective,
    match(silhouette, c("none", "mesh", "smooth")) - 1L,
    if (is.null(spec$crease)) NA_real_ else cos(spec$crease * pi / 180), spec$boundary)
  n <- length(lines$x)
  if (!n) return(packet)
  strokes <- data.frame(x = lines$x, y = lines$y, z = lines$z, group = lines$line,
    colour = spec$colour, linewidth = spec$linewidth, linetype = spec$linetype, alpha = spec$alpha,
    .source = data$.source[rows[lines$vertex]], .layer = data$.layer[rows[lines$vertex]])
  counts <- tabulate(lines$line)
  style <- utils::modifyList(params, list(hidden = spec$hidden, lineend = "round", linejoin = "round", shade = FALSE, arrow = NULL))
  outline <- packet_from_primitives_3d(strokes, seq_len(n), rep("polylines", length(counts)), style,
    panel_params$panel_id, counts)
  merge_scene_packets(list(packet, outline), panel_params$panel_id)
}
