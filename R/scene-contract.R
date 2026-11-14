scene_schema_version <- 2L

# Column-major entries of the 4 x 4 affine map of a quadric's unit primitive.
quadric_transform_columns <- paste0("transform_", 1:16)

scene_schemas <- function() {
  common <- c(primitive_id = "integer", layer_id = "integer",
              source_row = "integer", style_id = "integer", offset = "integer",
              count = "integer", role = "integer", partition = "integer",
              opacity = "double")
  list(
    vertices = c(x = "double", y = "double", z = "double", w = "double"),
    corners = c(vertex = "integer", source_row = "integer"),
    faces = common,
    polylines = c(common, width_mm = "double", mitre_limit = "double",
                  cap = "integer", join = "integer", hidden = "integer",
                  arrow_start = "logical", arrow_end = "logical",
                  mask_width_mm = "double"),
    markers = c(common, footprint_id = "integer"),
    text = c(common, width_mm = "double", height_mm = "double",
             hjust = "double", vjust = "double", angle = "double"),
    quadrics = c(primitive_id = "integer", layer_id = "integer",
                 source_row = "integer", style_id = "integer",
                 partition = "integer", opacity = "double", kind = "integer",
                 stats::setNames(rep("double", 16L), quadric_transform_columns),
                 radius_ratio = "double", cap_0 = "integer", cap_1 = "integer"),
    footprints = c(kind = "integer", offset = "integer", count = "integer",
                   radius_x_mm = "double", radius_y_mm = "double",
                   angle = "double"),
    footprint_vertices = c(x_mm = "double", y_mm = "double", ring = "integer")
  )
}

# Columns a table may carry after its required ones, in this order. Absent
# columns mean "not supplied".
scene_optional_schemas <- function() {
  list(corners = c(normal_x = "double", normal_y = "double", normal_z = "double",
                   red = "double", green = "double", blue = "double",
                   alpha = "double", u = "double", v = "double"))
}

scene_table_columns <- function(x, schema, optional, name) {
  extra <- setdiff(names(x), names(schema))
  scene_assert(all(extra %in% names(optional)), paste(name, "has unknown columns"))
  c(schema, optional[names(optional) %in% extra])
}

scene_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

scene_table <- function(x, schema, defaults, name, optional = character()) {
  scene_assert(is.list(x) && !anyDuplicated(names(x)), paste(name, "has unknown columns"))
  schema <- scene_table_columns(x, schema, optional, name)
  n <- if (length(x)) length(x[[1L]]) else 0L
  scene_assert(all(lengths(x) == n), paste(name, "has unequal column lengths"))
  out <- lapply(names(schema), function(column) {
    value <- x[[column]]
    if (is.null(value)) {
      value <- defaults[[column]]
      if (is.null(value)) {
        scene_assert(n == 0L, paste(name, "requires", column))
        value <- vector(schema[[column]], 0L)
      } else value <- rep_len(value, n)
    }
    scene_assert(is.null(dim(value)) && !is.object(value),
                 paste(name, column, "must be an unclassed vector"))
    type <- schema[[column]]
    if (type == "integer") {
      scene_assert(is.numeric(value) && all(is.finite(value)) &&
                     all(value == trunc(value)) &&
                     all(abs(value) <= .Machine$integer.max),
                   paste(name, column, "must contain finite integers"))
      value <- as.integer(value)
    } else if (type == "double") {
      scene_assert(is.numeric(value) &&
                     all(is.finite(value) | (column %in% names(optional) & is.na(value) & !is.nan(value))),
                   paste(name, column, "must contain finite numbers"))
      value <- as.double(value)
    } else {
      scene_assert(is.logical(value) && !anyNA(value),
                   paste(name, column, "must be logical without NA"))
    }
    value
  })
  names(out) <- names(schema)
  out
}

#' Construct a 3D geometry packet
#'
#' Emitters registered with [register_geom_3d()] return a validated scene packet.
#' Each geometry table is a named list of equal-length numeric vectors.
#' Indices start at one. Empty lists represent unused geometry kinds.
#' @param vertices List with x, y, z and optional w: homogeneous positions in
#'   normalised panel coordinates, see [normalise_homogeneous_3d()]. `w`
#'   defaults to 1, must be nonnegative, and `w = 0` places an ideal point (a
#'   direction) at infinity. Geometry through an ideal point is clipped by the
#'   panel box, the near and far planes or the viewport before it is drawn.
#' @param corners List with vertex indices and source_row input indices, and
#'   optional per-corner attributes normal_x, normal_y, normal_z, red, green,
#'   blue, alpha (encoded sRGB colour channels in \[0, 1\], interpolated in
#'   linear light) and u, v (texture coordinates),
#'   in that order; NA marks a corner without the attribute. Compiled vertex
#'   weights carry them through clipping.
#' @param faces,polylines,markers,text Primitive tables. Each requires offset
#'   into corners. Faces default to three corners, polylines to two, and markers
#'   and text to one. Common columns include count, primitive_id, layer_id,
#'   source_row, style_id, role, partition and opacity. Markers also require
#'   footprint_id. Text sizes use width_mm and height_mm. Polylines take an
#'   optional mask_width_mm, at least width_mm and by default equal to it: the
#'   width of the band in which the line hides what lies behind it, while the
#'   line itself is drawn at width_mm (a haloed line, Appel 1967).
#' @param quadrics Analytic primitives, each a unit primitive of `kind` (1
#'   sphere, 2 cylinder, 3 cone) placed by an affine map into normalised panel
#'   coordinates: `transform_1` to `transform_16`, the column-major entries of
#'   a 4 x 4 matrix whose last row is (0, 0, 0, 1), identity by default (Sigg
#'   et al. 2006). The unit sphere is the ball of radius 1 about the origin;
#'   unit cylinders and cones lie along the local z axis for z in \[0, 1\],
#'   with radius 1 at z = 0 and `radius_ratio` (default 1, exactly 1 for
#'   cylinders) at z = 1. `cap_0` and `cap_1` close the ends at z = 0 and z = 1
#'   with flat disks (1, the default) or leave them open (0). Common columns are
#'   primitive_id, layer_id, source_row, style_id, partition and opacity.
#' @param footprints Marker shapes: kind (1 for ellipse, 2 for polygon), offset,
#'   count, radius_x_mm, radius_y_mm and angle.
#' @param footprint_vertices Polygon footprint coordinates x_mm, y_mm and ring.
#' @param styles Data frame with contiguous integer style_id keys, fill, colour
#'   and other rendering properties. Face lighting uses shade and optional
#'   normal_x, normal_y, normal_z in normalised panel coordinates. An optional
#'   integer texture_id (NA for none) names a texture held on the R side.
#' @param materials Data frame of surface materials with contiguous integer
#'   material_id keys and columns ambient, diffuse, specular, shininess,
#'   specular_colour and back (a colour or NA), as made by [material_3d()].
#'   A style's optional integer material_id (NA for the default material)
#'   refers to it. The engine reads neither; the shading kernel does.
#' @param panel_id Positive integer panel identifier.
#' @return A validated ggplot3d_scene_packet containing geometry and styles.
#' @examples
#' new_scene_packet()
#' # A triangle with one corner at infinity in the x direction.
#' new_scene_packet(
#'   vertices = list(x = c(0.2, 0.2, 1), y = c(0.2, 0.8, 0), z = c(0.5, 0.5, 0),
#'                   w = c(1, 1, 0)),
#'   corners = list(vertex = 1:3),
#'   faces = list(offset = 1L),
#'   styles = data.frame(style_id = 1L, fill = "grey60"))
#' @export
new_scene_packet <- function(vertices = list(), corners = list(),
                             faces = list(), polylines = list(),
                             markers = list(), text = list(),
                             footprints = list(), footprint_vertices = list(),
                             styles = data.frame(style_id = integer()),
                             panel_id = 1L, quadrics = list(),
                             materials = data.frame(material_id = integer())) {
  schema <- scene_schemas()
  optional <- scene_optional_schemas()
  tables <- list(vertices = vertices, corners = corners, faces = faces,
                 polylines = polylines, markers = markers, text = text,
                 quadrics = quadrics, footprints = footprints,
                 footprint_vertices = footprint_vertices)
  next_id <- 1
  for (name in names(tables)) {
    n <- if (length(tables[[name]])) length(tables[[name]][[1L]]) else 0L
    kind <- match(name, c("faces", "polylines", "markers", "text"))
    defaults <- list(source_row = seq_len(n))
    if (name == "vertices") defaults$w <- 1
    if (name == "quadrics") {
      defaults <- c(defaults, list(primitive_id = next_id + seq_len(n) - 1L,
        layer_id = 1L, style_id = 1L, partition = 1L, opacity = 1,
        radius_ratio = 1, cap_0 = 1L, cap_1 = 1L),
        stats::setNames(as.list(as.double(diag(4))), quadric_transform_columns))
      next_id <- next_id + n
    }
    if (!is.na(kind)) {
      defaults <- c(defaults, list(
        primitive_id = next_id + seq_len(n) - 1L, layer_id = 1L,
        style_id = 1L, count = c(3L, 2L, 1L, 1L)[kind],
        role = kind - 1L, partition = 1L, opacity = 1,
        width_mm = 0, height_mm = 0, mitre_limit = 10,
        cap = 0L, join = 0L, hidden = 0L,
        arrow_start = FALSE, arrow_end = FALSE,
        hjust = 0.5, vjust = 0.5, angle = 0))
      next_id <- next_id + n
    }
    if (name == "polylines") defaults$mask_width_mm <- tables$polylines$width_mm %or3d% 0
    tables[[name]] <- scene_table(tables[[name]], schema[[name]], defaults, name,
                                  optional[[name]] %or3d% character())
  }
  scene_assert(is.numeric(panel_id) && length(panel_id) == 1L &&
                 is.finite(panel_id) && panel_id >= 1 &&
                 panel_id <= .Machine$integer.max && panel_id == trunc(panel_id),
               "panel_id must be a positive integer")
  scene_assert(is.data.frame(styles), "styles must be a data frame")
  packet <- c(list(schema_version = scene_schema_version, panel_id = as.integer(panel_id),
                   style_count = nrow(styles)), tables, list(styles = styles, materials = materials))
  class(packet) <- "ggplot3d_scene_packet"
  validate_scene_packet(packet)
  packet
}

validate_scene_packet <- function(packet) {
  schemas <- scene_schemas()
  expected <- c("schema_version", "panel_id", "style_count", names(schemas), "styles", "materials")
  scene_assert(is.list(packet) && identical(names(packet), expected),
               "packet has an invalid schema")
  for (key in c("schema_version", "panel_id", "style_count")) {
    scene_assert(is.integer(packet[[key]]) && length(packet[[key]]) == 1L &&
                   !anyNA(packet[[key]]), paste(key, "must be an integer scalar"))
  }
  scene_assert(packet$schema_version == scene_schema_version, "unsupported scene schema version")
  scene_assert(packet$panel_id >= 1L, "panel_id must be positive")
  scene_assert(is.data.frame(packet$styles) &&
                 identical(packet$styles$style_id, seq_len(nrow(packet$styles))) &&
                 packet$style_count == nrow(packet$styles),
               "styles must have contiguous integer style_id keys starting at 1")
  validate_scene_materials_3d(packet$materials, packet$styles)
  optional <- scene_optional_schemas()
  texture <- packet$styles$texture_id
  scene_assert(is.null(texture) || (is.numeric(texture) && is.null(dim(texture)) &&
                 all(is.na(texture) | (texture >= 1 & texture == trunc(texture)))),
               "styles texture_id must hold positive integers or NA")
  for (name in names(schemas)) {
    tab <- packet[[name]]
    spec <- schemas[[name]]
    extra <- optional[[name]]
    scene_assert(is.list(tab) && !is.null(names(tab)) &&
                   identical(names(tab)[seq_along(spec)], names(spec)) &&
                   identical(names(tab)[-seq_along(spec)],
                             intersect(as.character(names(extra)), names(tab)[-seq_along(spec)])),
                 paste(name, "has an invalid schema"))
    spec <- c(spec, extra[names(tab)[-seq_along(spec)]])
    scene_assert(length(unique(lengths(tab))) == 1L,
                 paste(name, "has unequal column lengths"))
    for (column in names(spec)) {
      scene_assert(identical(typeof(tab[[column]]), spec[[column]]) &&
                     is.null(dim(tab[[column]])) && !is.object(tab[[column]]) &&
                     all(is.finite(tab[[column]]) | (column %in% names(extra) &
                       is.na(tab[[column]]) & !is.nan(tab[[column]]))),
                   paste(name, column, "has invalid type or non-finite values"))
    }
  }
  v <- packet$vertices
  scene_assert(all(v$w >= 0) && all(v$w > 0 | v$x != 0 | v$y != 0 | v$z != 0),
               "vertex w must be nonnegative, and ideal vertices (w = 0) need a nonzero direction")
  colours <- intersect(c("red", "green", "blue", "alpha"), names(packet$corners))
  scene_assert(all(vapply(packet$corners[colours], function(x) all(is.na(x) | (x >= 0 & x <= 1)), logical(1))),
               "corner colours must lie in [0, 1]")
  nv <- length(packet$vertices$x)
  nc <- length(packet$corners$vertex)
  scene_assert(all(packet$corners$vertex >= 1L & packet$corners$vertex <= nv) &&
                 all(packet$corners$source_row >= 0L), "invalid corner references")
  ids <- integer()
  for (name in c("faces", "polylines", "markers", "text")) {
    tab <- packet[[name]]
    ids <- c(ids, tab$primitive_id)
    minimum <- switch(name, faces = 3L, polylines = 2L, 1L)
    scene_assert(all(tab$count >= minimum) &&
                   all(tab$offset >= 1L & as.double(tab$offset) + tab$count - 1 <= nc),
                 paste(name, "has invalid corner ranges"))
    if (name %in% c("markers", "text")) {
      scene_assert(all(tab$count == 1L), paste(name, "requires one anchor"))
    }
    scene_assert(all(tab$layer_id >= 0L & tab$source_row >= 0L) &&
                   all(tab$style_id >= 1L & tab$style_id <= packet$style_count) &&
                   all(tab$role %in% 0:3) && all(tab$partition %in% 0:2) &&
                   all(tab$opacity >= 0 & tab$opacity <= 1),
                 paste(name, "has invalid metadata"))
  }
  q <- packet$quadrics
  ids <- c(ids, q$primitive_id)
  m <- q[quadric_transform_columns]
  linear <- function(i, j) m[[(j - 1L) * 4L + i]]
  determinant <- linear(1, 1) * (linear(2, 2) * linear(3, 3) - linear(3, 2) * linear(2, 3)) -
    linear(1, 2) * (linear(2, 1) * linear(3, 3) - linear(3, 1) * linear(2, 3)) +
    linear(1, 3) * (linear(2, 1) * linear(3, 2) - linear(3, 1) * linear(2, 2))
  scene_assert(all(q$layer_id >= 0L & q$source_row >= 0L) &&
                 all(q$style_id >= 1L & q$style_id <= packet$style_count) &&
                 all(q$partition %in% 0:2) && all(q$opacity >= 0 & q$opacity <= 1) &&
                 all(q$kind %in% 1:3) && all(q$radius_ratio >= 0) &&
                 all(q$kind != 2L | q$radius_ratio == 1) &&
                 all(q$cap_0 %in% 0:1 & q$cap_1 %in% 0:1),
               "quadrics has invalid metadata, kind, radius ratio or caps")
  scene_assert(all(m$transform_4 == 0 & m$transform_8 == 0 & m$transform_12 == 0 & m$transform_16 == 1) &&
                 all(determinant != 0),
               "quadric transforms must be affine with an invertible linear part")
  scene_assert(all(ids >= 1L) && !anyDuplicated(ids),
               "primitive_id must be positive and unique across the panel")
  line <- packet$polylines
  scene_assert(all(line$mask_width_mm >= line$width_mm),
               "polyline mask_width_mm must be at least width_mm")
  scene_assert(all(line$width_mm >= 0 & line$mitre_limit >= 1) &&
                 all(line$cap %in% 0:2) && all(line$join %in% 0:2) &&
                 all(line$hidden %in% 0:2), "invalid polyline footprint or policy")
  fp <- packet$footprints
  fv <- packet$footprint_vertices
  scene_assert(all(fp$kind %in% 1:2) &&
                 all(fp$radius_x_mm >= 0 & fp$radius_y_mm >= 0) &&
                 all(fv$ring >= 1L), "invalid footprint geometry")
  polygon <- fp$kind == 2L
  scene_assert(all(fp$count[!polygon] == 0L & fp$offset[!polygon] == 1L) &&
                 all(fp$count[polygon] >= 3L) &&
                 all(fp$offset[polygon] >= 1L &
                       as.double(fp$offset[polygon]) + fp$count[polygon] - 1 <= length(fv$ring)),
               "invalid footprint ranges")
  if (any(polygon)) {
    runs <- rle(fv$ring)
    end <- cumsum(runs$lengths)
    start <- end - runs$lengths + 1L
    run <- rep.int(seq_along(end), runs$lengths)
    lo <- fp$offset[polygon]
    hi <- as.double(lo) + fp$count[polygon] - 1
    first <- run[lo]; last <- run[hi]
    short <- c(0L, cumsum(runs$lengths < 3L))
    index <- order(runs$values, method = "radix")
    repeated <- c(FALSE, diff(runs$values[index]) == 0L)
    previous_end <- integer(length(end))
    previous_end[index[repeated]] <- end[head(index, -1L)[repeated[-1L]]]
    # A repeated ring intersects a range when its preceding run ends inside it.
    duplicate_end <- cummax(previous_end)
    scene_assert(all(pmin(end[first], hi) - lo + 1L >= 3L) &&
                   all(hi - pmax(start[last], lo) + 1L >= 3L) &&
                   all(first == last | short[last] == short[first + 1L]) &&
                   all(duplicate_end[last] < lo),
                 "footprint rings must be contiguous with at least three vertices")
  }
  scene_assert(all(packet$markers$footprint_id >= 1L &
                   packet$markers$footprint_id <= length(fp$kind)),
               "invalid marker footprint_id")
  scene_assert(all(packet$text$width_mm >= 0 & packet$text$height_mm >= 0),
               "text measurements must be nonnegative")
  invisible(packet)
}

# Materials table: contiguous material_id keys, nonnegative strengths,
# positive shininess, reflectance in [0, 1], back a colour or NA; style
# material_id NA or a key.
validate_scene_materials_3d <- function(materials, styles) {
  scene_assert(is.data.frame(materials) && identical(as.integer(materials$material_id), seq_len(nrow(materials))) &&
                 is.integer(materials$material_id),
               "materials must have contiguous integer material_id keys starting at 1")
  if (nrow(materials)) {
    columns <- c("ambient", "diffuse", "specular", "shininess", "specular_colour")
    scene_assert(all(c(columns, "back") %in% names(materials)) &&
                   all(vapply(materials[columns], function(x) is.numeric(x) && all(is.finite(x) & x >= 0), logical(1))) &&
                   all(materials$shininess > 0) && all(materials$specular_colour <= 1) &&
                   (is.character(materials$back) || all(is.na(materials$back))),
                 "materials need nonnegative ambient, diffuse, specular, positive shininess, specular_colour in [0, 1] and back")
  }
  id <- styles$material_id
  scene_assert(is.null(id) || (is.numeric(id) && all(is.na(id) | (id >= 1 & id <= nrow(materials) & id == trunc(id)))),
               "styles material_id must be NA or a materials key")
  invisible(materials)
}

# The packet as the engine reads it: without the R-side styles and materials.
scene_engine_packet_3d <- function(packet) packet[!names(packet) %in% c("styles", "materials")]

#' Normalise homogeneous positions to the panel box
#'
#' Homogeneous coordinates (x, y, z, w) with w > 0 stand for the point
#' (x / w, y / w, z / w); with w = 0 they stand for the direction (x, y, z), a
#' point at infinity. This maps homogeneous positions in scale units to the
#' homogeneous panel-box fractions that [new_scene_packet()] takes: each
#' coordinate becomes (x - lo * w) / (hi - lo) and w is kept, so that positive
#' multiples of a row give the same point and directions stay at infinity.
#' @param x,y,z Homogeneous coordinates in scale units.
#' @param w Nonnegative homogeneous weight; 1 for ordinary points.
#' @param ranges List with x, y and z, each the lower and upper limit of the
#'   panel range, as in `panel_params$ranges`.
#' @return A list with x, y, z and w, the `vertices` of [new_scene_packet()].
#' @examples
#' normalise_homogeneous_3d(c(5, 1), c(0, 0), c(1, 0), w = c(1, 0),
#'   ranges = list(x = c(0, 10), y = c(-1, 1), z = c(0, 2)))
#' @export
normalise_homogeneous_3d <- function(x, y, z, w = 1, ranges) {
  coordinates <- list(x = x, y = y, z = z, w = w)
  n <- max(lengths(coordinates))
  scene_assert(all(vapply(coordinates, function(v) is.numeric(v) && all(is.finite(v)) &&
                            length(v) %in% c(1L, n), logical(1))),
               "x, y, z and w must be finite numbers of one common length")
  w <- rep_len(as.double(w), n)
  scene_assert(all(w >= 0), "w must be nonnegative")
  scene_assert(is.list(ranges) && all(c("x", "y", "z") %in% names(ranges)),
               "ranges must be a list with x, y and z limits")
  out <- lapply(c(x = "x", y = "y", z = "z"), function(axis) {
    range <- ranges[[axis]]
    scene_assert(is.numeric(range) && length(range) == 2L && all(is.finite(range)) &&
                   range[2L] != range[1L], paste("ranges", axis, "must be two distinct finite limits"))
    (rep_len(as.double(coordinates[[axis]]), n) - range[1L] * w) / diff(range)
  })
  out$w <- w
  out
}

# Optional corner attributes (`columns`) at the output vertices of a compiled
# scene: each output vertex combines the corners of its source primitive with
# the coefficients weight * w, w the homogeneous weight after camera pre,
# which sum to one. A vertex takes NA when a corner it depends on lacks the
# attribute. NULL when the packet supplies none of the columns.
scene_vertex_attributes_3d <- function(scene, columns) {
  corners <- scene$source$corners
  present <- intersect(columns, names(corners))
  n <- length(scene$vertices$x)
  if (!length(present) || !n) return(NULL)
  weights <- scene$weights
  xyzw <- scene_vertex_matrix_3d(scene$source$vertices)[corners$vertex[weights$corner], , drop = FALSE]
  pre <- scene$pre %or3d% c(diag(4))
  w <- drop(xyzw %*% matrix(as.double(pre), 4L, 4L)[4L, ])
  coefficient <- weights$weight * w
  out <- vapply(present, function(name) {
    value <- corners[[name]][weights$corner]
    used <- coefficient != 0
    total <- numeric(n); missing <- logical(n)
    sums <- rowsum(coefficient[used] * value[used], weights$vertex[used])
    total[as.integer(rownames(sums))] <- sums[, 1L]
    missing[weights$vertex[used & is.na(value)]] <- TRUE
    seen <- logical(n); seen[weights$vertex] <- TRUE
    total[missing | !seen] <- NA_real_
    total
  }, numeric(n))
  if (n == 1L) out <- matrix(out, 1L, dimnames = list(NULL, present))
  out
}

# Rows (x, y, z, w) of packet vertices.
scene_vertex_matrix_3d <- function(vertices) {
  cbind(x = vertices$x, y = vertices$y, z = vertices$z, w = vertices$w)
}

# Euclidean positions of homogeneous rows; every row must be a finite point.
homogeneous_xyz_3d <- function(xyzw) {
  w <- xyzw[, 4L]
  scene_assert(all(w > 0), "unbounded geometry: this step needs finite vertices (w > 0)")
  xyzw[, 1:3, drop = FALSE] / w
}

scene_vertices_xyz_3d <- function(vertices) homogeneous_xyz_3d(scene_vertex_matrix_3d(vertices))

# Packet vertices from three Euclidean or four homogeneous columns.
scene_vertices_3d <- function(points) {
  w <- if (ncol(points) == 4L) points[, 4L] else rep(1, nrow(points))
  list(x = as.double(points[, 1L]), y = as.double(points[, 2L]),
       z = as.double(points[, 3L]), w = as.double(w))
}

# The packet with camera pre applied and every vertex finite (w = 1): geometry
# through infinity, or behind it after pre, is first cut to the panel box. For
# consumers without a homogeneous clipper (viewer, export).
scene_finite_packet_3d <- function(packet, pre = diag(4)) {
  packet <- quadric_faces_3d(packet)
  xyzw <- scene_vertex_matrix_3d(packet$vertices)
  if (!identical(as.double(pre), as.double(diag(4)))) {
    xyzw <- xyzw %*% t(matrix(as.double(pre), 4L, 4L))
    packet$vertices <- scene_vertices_3d(xyzw)
  }
  if (any(xyzw[, 4L] <= 0)) {
    packet <- clip_packet_planes_3d(packet, unit_box_planes_3d())
    attr(packet, "parent") <- NULL
  }
  packet$vertices <- scene_vertices_3d(scene_vertices_xyz_3d(packet$vertices))
  validate_scene_packet(packet)
  packet
}

# `pre` is a 4 x 4 matrix (or its 16 column-major entries) applied to the
# homogeneous box fractions before aspect, clipping and projection.
new_camera_3d <- function(projection = "orthographic", azimuth = -60,
                          elevation = 30, roll = 0, distance = NULL,
                          fov = 35, zoom = 1, target = c(0.5, 0.5, 0.5),
                          aspect = c(1, 1, 1), near = NULL, far = NULL,
                          clip_data = TRUE, clip = TRUE, up = 3L, shear = c(0, 0),
                          pre = diag(4)) {
  camera <- list(projection = projection, azimuth = as.double(azimuth),
                 elevation = as.double(elevation), roll = as.double(roll),
                 distance = if (is.null(distance)) {
                   if (identical(projection, "perspective")) NA_real_ else Inf
                 } else as.double(distance),
                 fov = as.double(fov), zoom = as.double(zoom),
                 target = as.double(target), aspect = as.double(aspect),
                 near = if (is.null(near)) NA_real_ else as.double(near),
                 far = if (is.null(far)) NA_real_ else as.double(far),
                 clip_data = clip_data, clip = clip, up = as.double(up), shear = as.double(shear),
                 pre = as.double(pre))
  validate_camera_3d(camera)
  structure(camera, class = "ggplot3d_camera")
}

validate_camera_3d <- function(camera) {
  scene_assert(identical(names(camera), c("projection", "azimuth", "elevation",
    "roll", "distance", "fov", "zoom", "target", "aspect", "near", "far",
    "clip_data", "clip", "up", "shear", "pre")), "invalid camera schema")
  scene_assert(is.double(camera$pre) && length(camera$pre) == 16L && all(is.finite(camera$pre)),
               "camera pre must be 16 finite numbers, a column-major 4 x 4 matrix")
  scene_assert(length(camera$projection) == 1L &&
                 camera$projection %in% c("orthographic", "perspective", "oblique"),
               "invalid camera projection")
  scene_assert(is.double(camera$up) && length(camera$up) == 1L && camera$up %in% 1:3,
               "camera up must be axis 1, 2 or 3")
  scene_assert(is.double(camera$shear) && length(camera$shear) == 2L && all(is.finite(camera$shear)),
               "invalid camera shear")
  scene_assert(camera$projection == "oblique" || all(camera$shear == 0),
               "camera shear requires the oblique projection")
  for (key in c("azimuth", "elevation", "roll", "fov", "zoom")) {
    scene_assert(is.double(camera[[key]]) && length(camera[[key]]) == 1L &&
                   is.finite(camera[[key]]), paste("invalid camera", key))
  }
  scene_assert(abs(camera$elevation) <= 90 && camera$fov > 0 &&
                 camera$fov < 180 && camera$zoom > 0, "invalid camera angles or zoom")
  for (key in c("target", "aspect")) {
    scene_assert(is.double(camera[[key]]) && length(camera[[key]]) == 3L &&
                   all(is.finite(camera[[key]])), paste("invalid camera", key))
  }
  scene_assert(all(camera$aspect > 0), "camera aspect must be positive")
  for (key in c("distance", "near", "far")) {
    value <- camera[[key]]
    scene_assert(is.double(value) && length(value) == 1L && !is.nan(value) &&
                   (is.na(value) || value > 0), paste("invalid camera", key))
  }
  if (camera$projection != "perspective") {
    scene_assert(identical(camera$distance, Inf), "orthographic distance must be Inf")
  } else scene_assert(is.na(camera$distance) || is.finite(camera$distance),
                      "perspective distance must be finite")
  for (key in c("near", "far")) {
    scene_assert(is.na(camera[[key]]) || is.finite(camera[[key]]),
                 paste("camera", key, "must be finite"))
  }
  if (!is.na(camera$near) && !is.na(camera$far)) {
    scene_assert(camera$near < camera$far, "camera near must be less than far")
  }
  for (key in c("clip_data", "clip")) {
    scene_assert(is.logical(camera[[key]]) && length(camera[[key]]) == 1L &&
                   !anyNA(camera[[key]]), paste("invalid camera", key))
  }
  invisible(camera)
}

new_scene_viewport <- function(width_mm, height_mm, dpi = 144) {
  if (grid::is.unit(width_mm)) width_mm <- grid::convertWidth(width_mm, "mm", TRUE)
  if (grid::is.unit(height_mm)) height_mm <- grid::convertHeight(height_mm, "mm", TRUE)
  viewport <- list(width_mm = as.double(width_mm), height_mm = as.double(height_mm),
                   dpi = as.double(dpi))
  scene_assert(all(lengths(viewport) == 1L) &&
                 all(is.finite(unlist(viewport))) && all(unlist(viewport) > 0),
               "viewport dimensions and dpi must be positive and finite")
  viewport
}

scene_layer_grob <- function(packet) {
  validate_scene_packet(packet)
  n <- sum(vapply(packet[c("faces", "polylines", "markers", "text", "quadrics")],
                  function(x) length(x$primitive_id), integer(1)))
  if (n == 0L) return(ggplot2::zeroGrob())
  grid::gTree(packet = packet, cl = "scene_layer_grob")
}

engine_threads <- function() {
  threads <- getOption("ggplot3d.threads")
  if (!is.null(threads)) {
    scene_assert(is.numeric(threads) && length(threads) == 1L && is.finite(threads) &&
                   threads >= 1 && threads == round(threads),
                 "option ggplot3d.threads must be a positive whole number")
    return(as.integer(threads))
  }
  limit <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(limit) && limit != "false") 2L else NULL
}

scene_compile <- function(packet, camera = new_camera_3d(), viewport) {
  scene_assert(identical(names(viewport), c("width_mm", "height_mm", "dpi")),
               "invalid viewport schema")
  viewport <- do.call(new_scene_viewport, viewport)
  threads <- engine_threads()
  limit <- scene_cache_limit()
  if (limit == 0) scene_cache$entries <- list()
  cached <- scene_cache_find(packet, camera, viewport)
  if (!is.null(cached)) return(cached)
  validate_scene_packet(packet)
  validate_camera_3d(camera)
  attr(viewport, "engine_threads") <- threads
  result <- scene_compile_cpp(scene_engine_packet_3d(packet), camera, viewport)
  attr(viewport, "engine_threads") <- NULL
  bytes <- as.numeric(utils::object.size(result)) + as.numeric(utils::object.size(packet))
  result$styles <- packet$styles
  result$source <- scene_engine_packet_3d(packet)
  result$materials <- packet$materials
  result$pre <- camera$pre
  result$camera_input <- camera
  class(result) <- "ggplot3d_compiled_scene"
  scene_cache_store(packet, camera, viewport, result, bytes, limit)
  result
}
