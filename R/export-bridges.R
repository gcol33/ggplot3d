#' Convert a 3D panel to an rgl mesh or rgl scene
#'
#' `as.mesh3d()` returns the scene of one panel as one rgl `mesh3d` object
#' with vertex colours, ready for [rgl::shade3d()], [rgl::writeOBJ()] or
#' other mesh tools. `plot3d()` draws the panel in an rgl window with rgl's
#' own triangles, segments, points and text, viewed from the plot's camera.
#' Coordinates are model coordinates with the vertical axis of the plot as +z;
#' see [export_3d()].
#' @param x A ggplot with [coord_3d()].
#' @param panel,furniture,size,width,height,dpi See [export_3d()].
#' @param lines,points See [export_3d()]. `"line"` and `"point"` fill the
#'   mesh's segment (`is`) and point (`ip`) indices.
#' @param add Whether to draw into the current rgl scene instead of a new
#'   one, default FALSE.
#' @param ... Unused.
#' @return `as.mesh3d()`: a `mesh3d` object. `plot3d()`: the rgl object ids,
#'   invisibly.
#' @details Faces drawn with lighting in the plot are drawn lit by rgl; all
#'   other geometry is unlit. Line widths and point sizes are converted to
#'   pixels at `dpi`. rgl centres the view on the bounding box of the drawn
#'   geometry, so the view matches the plot's camera angles and projection
#'   but can differ in framing.
#' @examples
#' if (requireNamespace("rgl", quietly = TRUE)) {
#'   d <- expand.grid(x = seq(-1, 1, length.out = 10), y = seq(-1, 1, length.out = 10))
#'   d$z <- d$x * exp(-d$x^2 - d$y^2)
#'   p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'     geom_surface_3d(ggplot2::aes(fill = z)) + coord_3d()
#'   old <- options(rgl.useNULL = TRUE)
#'   rgl::plot3d(p)
#'   rgl::close3d()
#'   options(old)
#'   mesh <- rgl::as.mesh3d(p)
#'   dim(mesh$it)
#' }
#' @name rgl-bridge-3d
NULL

#' @rdname rgl-bridge-3d
#' @exportS3Method rgl::as.mesh3d
as.mesh3d.ggplot3d <- function(x, panel = 1L, furniture = FALSE, lines = c("tube", "line"),
                               points = c("mesh", "point"), size = 1, width = 640L, height = 640L,
                               dpi = 96, ...) {
  require_geometry_package_3d("rgl")
  options <- export_options_3d(furniture, match.arg(lines), match.arg(points), size)
  export_rgl_mesh_3d(export_merged_3d(export_scene_3d(x, panel, options, width, height, dpi), options))
}

export_rgl_mesh_3d <- function(mesh) {
  primitive <- function(index) if (length(index)) index
  rgl::mesh3d(vertices = t(mesh$position), triangles = primitive(mesh$triangles),
    segments = primitive(mesh$lines), points = primitive(as.vector(mesh$points)),
    normals = if (!length(mesh$lines) && !length(mesh$points)) mesh$normal,
    material = list(color = grDevices::rgb(mesh$colour[, 1L], mesh$colour[, 2L], mesh$colour[, 3L]),
      alpha = mesh$colour[, 4L]), meshColor = "vertices")
}

#' @rdname rgl-bridge-3d
#' @exportS3Method rgl::plot3d
plot3d.ggplot3d <- function(x, panel = 1L, furniture = FALSE, size = 1, width = 640L, height = 640L,
                            dpi = 96, add = FALSE, ...) {
  require_geometry_package_3d("rgl")
  scene_assert(is.logical(add) && length(add) == 1L && !is.na(add), "add must be TRUE or FALSE")
  options <- export_options_3d(furniture, "line", "point", size)
  scene <- export_scene_3d(x, panel, options, width, height, dpi)
  if (!add) rgl::open3d()
  hex <- function(rgba) grDevices::rgb(rgba[, 1L], rgba[, 2L], rgba[, 3L])
  z <- function(xyz) export_axes_3d(xyz, scene$up, "z")
  pixels <- function(mm) mm / 25.4 * dpi
  ids <- integer()
  for (node in scene$nodes) {
    t <- node$triangles
    if (!is.null(t)) for (lit in unique(t$lit)) {
      i <- which(t$lit == lit)
      rows <- as.vector(rbind(3L * i - 2L, 3L * i - 1L, 3L * i))
      colour <- t$colour[rep(i, each = 3L), , drop = FALSE]
      ids <- c(ids, rgl::triangles3d(z(t$position[rows, , drop = FALSE]), color = hex(colour),
        alpha = colour[, 4L], lit = lit))
    }
    p <- node$paths
    if (!is.null(p)) {
      n <- nrow(p$position)
      joined <- which(p$path[-1L] == p$path[-n])
      for (w in unique(p$width_mm)) {
        segment <- joined[p$width_mm[p$path[joined]] == w]
        if (!length(segment)) next
        rows <- as.vector(rbind(segment, segment + 1L))
        colour <- p$colour[p$path[rows], , drop = FALSE]
        ids <- c(ids, rgl::segments3d(z(p$position[rows, , drop = FALSE]), color = hex(colour),
          alpha = colour[, 4L], lwd = pixels(w), lit = FALSE))
      }
    }
    m <- node$markers
    if (!is.null(m)) for (r in unique(m$radius_mm)) {
      i <- which(m$radius_mm == r)
      ids <- c(ids, rgl::points3d(z(m$position[i, , drop = FALSE]), color = hex(m$colour[i, , drop = FALSE]),
        alpha = m$colour[i, 4L], size = pixels(2 * r), point_antialias = TRUE, lit = FALSE))
    }
    l <- node$text
    if (!is.null(l)) {
      ids <- c(ids, rgl::text3d(z(l$position), texts = l$label, color = hex(l$colour),
        alpha = l$colour[, 4L], cex = l$fontsize / 12))
    }
  }
  camera <- scene$camera
  basis <- z(rbind(camera$right, camera$up, -camera$forward))
  view <- diag(4L)
  view[1:3, 1:3] <- basis
  rgl::aspect3d("iso")
  rgl::view3d(userMatrix = view,
    fov = if (camera$projection == "perspective") camera$yfov * 180 / pi else 0)
  invisible(ids)
}

#' Convert a 3D panel to a rayrender scene
#'
#' Build a [rayrender](https://www.rayrender.net) scene from the triangles of
#' one panel, one object per layer with its vertex colours and a diffuse
#' material, and optionally path-trace it from the plot's camera. Paths
#' become tubes and points become solids, as in [export_3d()].
#' @param plot A ggplot with [coord_3d()].
#' @param panel,furniture,size,width,height,dpi See [export_3d()]. `width`
#'   and `height` also set the rendered image size.
#' @param render Whether to render the scene with [rayrender::render_scene()]
#'   from the plot's camera, default FALSE.
#' @param ... Further arguments to [rayrender::render_scene()] when `render`
#'   is TRUE, overriding the camera and the white background.
#' @return The rayrender scene, or with `render = TRUE` the value of
#'   [rayrender::render_scene()].
#' @details Coordinates are model coordinates with the vertical axis of the
#'   plot as +z. Translucency is not transferred.
#' @examples
#' if (requireNamespace("rayrender", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'     geom_point_3d(ggplot2::aes(colour = cyl)) + coord_3d()
#'   scene <- as_rayrender_3d(p)
#'   nrow(scene)
#' }
#' @export
as_rayrender_3d <- function(plot, panel = 1L, furniture = FALSE, size = 1, width = 640L,
                            height = 640L, dpi = 96, render = FALSE, ...) {
  motion_require_3d("rayrender")
  scene_assert(is.logical(render) && length(render) == 1L && !is.na(render), "render must be TRUE or FALSE")
  options <- export_options_3d(furniture, "tube", "mesh", size)
  scene <- export_scene_3d(plot, panel, options, width, height, dpi)
  objects <- lapply(scene$nodes, function(node) {
    mesh <- export_merged_3d(list(nodes = list(node), up = scene$up), options)
    if (!length(mesh$triangles)) return(NULL)
    # Vertex colours carry the material; rayrender warns that the mesh has no texture.
    withCallingHandlers(rayrender::mesh3d_model(export_rgl_mesh_3d(mesh), material = rayrender::diffuse()),
      warning = function(w) if (grepl("vertex color", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))
  })
  objects <- Filter(Negate(is.null), objects)
  scene_assert(length(objects) > 0L, "the panel has no geometry to render")
  ray <- Reduce(rayrender::add_object, objects)
  if (!render) return(ray)
  camera <- scene$camera
  z <- function(v) drop(export_axes_3d(matrix(v, 1L), scene$up, "z"))
  defaults <- list(lookfrom = z(camera$eye), lookat = z(camera$target), camera_up = z(camera$up),
    fov = if (camera$projection == "perspective") camera$yfov * 180 / pi else 0,
    ortho_dimensions = 2 * camera$ymag * c(camera$aspect, 1), width = width, height = height,
    backgroundhigh = "#FFFFFF", backgroundlow = "#FFFFFF")
  do.call(rayrender::render_scene, c(list(ray), utils::modifyList(defaults, list(...))))
}
