#' Triangulated surfaces in three dimensions
#'
#' Grid surfaces estimate vertex normals from height differences along each
#' axis. With `shading = "flat"` lighting uses the normalised mean of each
#' face's vertex normals and a numeric fill is averaged over each face. With
#' `shading = "smooth"` the vertex normals are interpolated across each face
#' (Phong 1975) and a fill that varies between vertices, numeric or given as
#' colours such as `fill = I(hex)`, is interpolated in linear light (Gouraud
#' 1971), as MATLAB's `lighting gouraud` with `FaceColor = "interp"` and rgl's
#' `smooth` material. A discrete fill splits the rows into groups, so
#' `group = 1` keeps one surface.
#' Binned fill and colour scales split faces at the scalar iso-lines, using
#' linear interpolation of the mapped variable between vertices. Scale
#' transformations place the breaks in the variable's original units.
#' Each piece has one band colour, with normals interpolated across its edges.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard ggplot2 layer arguments.
#' @param method Grid connectivity or Delaunay triangulation.
#' @param max_edge Maximum triangle edge length in transformed x/y units.
#' @param shade Whether to apply lighting.
#' @param wireframe Whether to draw triangle edges.
#' @param hidden Hidden stroke policy.
#' @param outline Feature lines from [outline_3d()], or NULL for none.
#' @param material Surface material from [material_3d()]: ambient, diffuse
#'   and specular strengths, shininess, an optional back-side colour and an
#'   illustrative style (toon, Gooch, normal colours). NULL reflects ambient
#'   and diffuse light fully, with no highlight.
#' @param hatch Hatching from [hatch_3d()], or NULL for none.
#' @param shading `"flat"` lights and fills each face uniformly; `"smooth"`
#'   interpolates vertex normals and vertex fills across faces. The raster
#'   backend and the [view_3d()] viewer interpolate per sample, perspective
#'   correctly. The vector backend draws an isolated triangle whose colour
#'   varies along one direction as one linear gradient, with stops that follow the sRGB
#'   curve within the option `ggplot3d.shading_tolerance` (default 2 / 255 in
#'   encoded sRGB), and a triangle whose colour stays within that tolerance as
#'   one flat polygon. Adjacent smooth triangles and triangles whose colour
#'   varies in two directions are drawn as one image per drawing-order run,
#'   with per-sample lighting, clipped to the faces' union. The image uses
#'   sixteen samples per pixel, at the device resolution
#'   and at least `options(ggplot3d.smooth_dpi)` (default 150) dots per inch.
#'   `options(ggplot3d.smooth_vector = "subdivide")` splits such triangles
#'   into flat overlapping pieces instead, at most four times (256 pieces per
#'   triangle) and not into pieces narrower than a device pixel, so large
#'   faces with distant colours show steps. The layers that draw surfaces
#'   ([stat_isosurface_3d()], [geom_mesh_3d()] and others) accept it too.
#' @param texture An image draped over the faces: a [texture_3d()] object or
#'   anything it accepts. Texture coordinates come from the `u` and `v`
#'   aesthetics when mapped, and otherwise from x and y, spread over the
#'   texture's extent (default the range of the layer). [stat_parametric_3d()]
#'   surfaces use their parameters and rgl meshes their `texcoords`, whose
#'   `texture` file is read when this is `NULL`. Each face shows the image
#'   over its fill; `fill = NA` shows the image alone. The layers that draw
#'   surfaces accept it too.
#' @param alpha_mode How a translucent surface composites.
#'   `"face"` blends every face over what lies behind it, so a surface seen
#'   through itself darkens where it folds over. `"surface"` paints each sheet
#'   (a connected set of faces sharing vertices, such as one isosurface level)
#'   as one translucent film: other sheets show through it, its own pieces
#'   behind its nearer pieces do not. A sheet of several fill colours needs a
#'   device with Porter-Duff groups and alpha masks (ragg, cairo_pdf, svg);
#'   other devices, pdf() among them, paint it as `"face"`. The raster and
#'   hybrid backends keep, per sample, the nearest piece of each sheet and
#'   composite the sheets in depth order, with any fill colours; so does the
#'   [view_3d()] viewer. The layers
#'   that draw surfaces ([stat_isosurface_3d()], [stat_parametric_3d()],
#'   [geom_mesh_3d()] and others) accept it too.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 6),
#'                  y = seq(-1, 1, length.out = 6))
#' d$z <- with(d, x^2 + y^2)
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_surface_3d(fill = "steelblue") + coord_3d()
#' p + stat_surface_3d() + coord_3d()
#' p + geom_surface_3d(ggplot2::aes(fill = z), shading = "smooth") + coord_3d()
#' stripes <- matrix(rep(c("navy", "gold"), each = 4), 1)
#' p + geom_surface_3d(texture = texture_3d(stripes, interpolate = FALSE)) + coord_3d()
#' @export
geom_surface_3d <- function(mapping = NULL, data = NULL, stat = StatSurface3D,
                             position = "identity", ..., method = "grid", max_edge = Inf,
                             shade = TRUE, wireframe = FALSE, hidden = "hide", outline = NULL,
                             alpha_mode = "face", shading = "flat", material = NULL, hatch = NULL,
                             texture = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomSurface3D, mapping, data, stat, position,
    c(list(method = method, max_edge = max_edge, shade = shade, wireframe = wireframe,
           hidden = hidden, outline = outline, alpha_mode = alpha_mode_3d(alpha_mode),
           shading = shading_mode_3d(shading), material = material_argument_3d(material), hatch = hatch_argument_3d(hatch),
           texture = texture_argument_3d(texture), na.rm = na.rm), list(...)),
    material_legend_3d(material, show.legend), inherit.aes)
}

#' @rdname geom_surface_3d
#' @param geom Geom prototype used to draw the computed triangles.
#' @export
stat_surface_3d <- function(mapping = NULL, data = NULL, geom = GeomSurface3D,
                             position = "identity", ..., method = "grid", max_edge = Inf,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatSurface3D, position,
    c(list(method = method, max_edge = max_edge, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

surface_lattice_3d <- function(data) {
  list(i = match(data$x, sort(unique(data$x))), j = match(data$y, sort(unique(data$y))))
}

lattice_triangles_3d <- function(lattice, z) {
  grid <- matrix(NA_integer_, max(0L, lattice$i), max(0L, lattice$j))
  grid[cbind(lattice$i, lattice$j)] <- seq_along(lattice$i)
  if (nrow(grid) < 2L || ncol(grid) < 2L) return(matrix(integer(), ncol = 3L))
  i <- rep(seq_len(nrow(grid) - 1L), each = ncol(grid) - 1L)
  j <- rep.int(seq_len(ncol(grid) - 1L), nrow(grid) - 1L)
  cells <- cbind(grid[cbind(i, j)], grid[cbind(i + 1L, j)],
                 grid[cbind(i + 1L, j + 1L)], grid[cbind(i, j + 1L)])
  good <- rowSums(matrix(is.finite(z[cells]), ncol = 4L)) == 4L
  cells <- cells[good, , drop = FALSE]
  matrix(as.integer(t(cbind(cells[, c(1L, 2L, 3L), drop = FALSE],
                            cells[, c(1L, 3L, 4L), drop = FALSE]))), ncol = 3L, byrow = TRUE)
}

surface_triangles_3d <- function(data, method, max_edge, lattice = surface_lattice_3d(data)) {
  index <- order(lattice$i, lattice$j, method = "radix")
  a <- head(index, -1L); b <- tail(index, -1L)
  if (any(lattice$i[a] == lattice$i[b] & lattice$j[a] == lattice$j[b])) stop("Surface x/y coordinates must be unique within each group.", call. = FALSE)
  if (method == "grid") {
    triangles <- lattice_triangles_3d(lattice, data$z)
  } else {
    good <- which(is.finite(data$z))
    triangles <- matrix(good[delaunay_triangles_3d(as.matrix(data[good, c("x", "y")]))], ncol = 3L)
  }
  if (is.finite(max_edge) && nrow(triangles)) {
    next_corner <- triangles[, c(2L, 3L, 1L), drop = FALSE]
    lengths <- sqrt((data$x[triangles] - data$x[next_corner])^2 +
                      (data$y[triangles] - data$y[next_corner])^2)
    triangles <- triangles[rowSums(matrix(lengths <= max_edge, ncol = 3L)) == 3L, , drop = FALSE]
  }
  triangles
}

StatSurface3D <- ggplot2::ggproto("StatSurface3D", ggplot2::Stat,
  required_aes = c("x", "y"),
  optional_aes = c("z", "fill", "u", "v"),
  extra_params = c("na.rm", "method", "max_edge"),
  setup_params = function(data, params) {
    if (is.null(data$z)) stop("A surface requires a z aesthetic.", call. = FALSE)
    params$method <- match.arg(params$method %or3d% "grid", c("grid", "delaunay"))
    params$max_edge <- params$max_edge %or3d% Inf
    if (length(params$max_edge) != 1L || is.na(params$max_edge) || params$max_edge <= 0) stop("max_edge must be positive.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, method = "grid", max_edge = Inf, shading = "flat") {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Surfaces require continuous x/y/z scales.", call. = FALSE)
    triangles <- surface_triangles_3d(data, method, max_edge)
    if (!length(triangles)) return(data.frame())
    if (method == "grid") data <- surface_normals_3d(data)
    rows <- as.vector(t(triangles))
    out <- lapply(data, `[`, rows)
    out <- structure(out, class = "data.frame", row.names = .set_row_names(length(rows)))
    out$.face <- rep(seq_len(nrow(triangles)), each = 3L)
    out$.corner <- rep.int(1:3, nrow(triangles))
    out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
    face_fill_samples_3d(out, out$.face, shading,
      rep(rowMeans(matrix(data$fill[triangles], ncol = 3L)), each = 3L))
  }
)

GeomSurface3D <- ggplot2::ggproto("GeomSurface3D", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "z", ".face", ".corner"),
  optional_aes = c("nx", "ny", "nz", "u", "v", ".texture_u", ".texture_v"),
  default_aes = ggplot2::aes(colour = NA, fill = "grey70", linewidth = 0.3, linetype = 1, alpha = NA)
)

emit_surface_3d <- function(data, params, panel_params) {
  rows <- order(data$group, data$.face, data$.corner)
  first <- c(TRUE, data$group[head(rows, -1L)] != data$group[tail(rows, -1L)] |
                    data$.face[head(rows, -1L)] != data$.face[tail(rows, -1L)])[seq_along(rows)]
  counts <- diff(c(which(first), length(rows) + 1L))
  valid <- valid_rows_3d(data, params)
  bad <- c(0L, cumsum(!valid[rows]))
  keep <- diff(bad[c(which(first), length(rows) + 1L)]) == 0L
  rows <- rows[rep.int(keep, counts)]
  counts <- counts[keep]
  if (any(counts != 3L)) stop("Surface faces require three keyed corners.", call. = FALSE)
  faces <- matrix(rows, ncol = 3L, byrow = TRUE)
  corner <- matrix(data$.corner[faces], ncol = 3L)
  if (any(corner[, 1L] == corner[, 2L] | corner[, 2L] == corner[, 3L] | corner[, 1L] == corner[, 3L])) stop("Surface faces require three keyed corners.", call. = FALSE)
  check_face_materials_3d(data, as.vector(t(faces)), rep(seq_len(nrow(faces)), each = 3L), params, "Surface")
  edges <- matrix(integer(), ncol = 2L)
  if (isTRUE(params$wireframe) || any(!is.na(data$colour))) {
    edges <- matrix(as.vector(t(faces[, c(1L, 2L, 2L, 3L, 3L, 1L), drop = FALSE])), ncol = 2L, byrow = TRUE)
    edges <- unique_edges_3d(edges, data$.source, data$group)
  }
  surface_strokes_3d(packet_from_faces_edges_3d(data, faces, edges, params, panel_params), data, faces, params, panel_params)
}

# Materials of the corner rows (rows, in face order, with face ids) must be
# constant per face, except a fill under smooth shading, which is
# interpolated across the face from its corner colours.
check_face_materials_3d <- function(data, rows, face, params, label) {
  same <- face[-1L] == face[-length(face)]
  for (name in intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(data))) {
    if (paste0(".band_", name) %in% names(data)) next
    if (name == "fill" && identical(params$shading, "smooth")) next
    value <- data[[name]][rows]
    keys <- match(value, unique(value))
    if (any(same & keys[-1L] != keys[-length(keys)])) stop(label, " materials must be constant per face; a fill that varies per vertex needs shading = \"smooth\".", call. = FALSE)
  }
  invisible(TRUE)
}


# Delaunay triangles of the x/y positions as a three-column matrix of row
# indices, counterclockwise (native Bowyer-Watson with exact predicates).
delaunay_triangles_3d <- function(xy) {
  if (nrow(xy) < 3L) return(matrix(integer(), ncol = 3L))
  triangles <- geometry_delaunay_cpp(xy)
  if (!nrow(triangles)) stop("Delaunay surface coordinates are collinear.", call. = FALSE)
  triangles
}

register_geom_3d(GeomSurface3D, emit_surface_3d, parameters = surface_parameters_3d)
