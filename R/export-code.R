tex_escape_3d <- function(text) {
  replacements <- c("\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}", "$" = "\\$",
    "&" = "\\&", "#" = "\\#", "%" = "\\%", "_" = "\\_", "^" = "\\textasciicircum{}", "~" = "\\textasciitilde{}")
  vapply(as.character(text), function(x) paste0(vapply(strsplit(x, "", fixed = TRUE)[[1L]], function(ch)
    if (ch %in% names(replacements)) replacements[[ch]] else ch, character(1)), collapse = ""), character(1), USE.NAMES = FALSE)
}

svg_numbers_3d <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return(numeric())
  matches <- gregexpr("[-+]?(?:[0-9]*[.])?[0-9]+(?:[eE][-+]?[0-9]+)?", text, perl = TRUE)
  as.numeric(regmatches(text, matches)[[1L]])
}

svg_tikz_style_3d <- function(node, inherited) {
  attrs <- as.list(xml2::xml_attrs(node))
  style <- attrs[["style"]]
  if (!is.null(style)) for (part in strsplit(style, ";", fixed = TRUE)[[1L]]) {
    item <- strsplit(part, ":", fixed = TRUE)[[1L]]
    if (length(item) == 2L) attrs[trimws(item[1L])] <- trimws(item[2L])
  }
  keys <- c("fill", "stroke", "stroke-width", "fill-rule", "clip-rule", "fill-opacity", "stroke-opacity",
    "font-size", "font-family", "font-weight", "font-style", "text-anchor", "dominant-baseline",
    "stroke-linecap", "stroke-linejoin", "stroke-miterlimit")
  for (key in intersect(keys, names(attrs))) if (attrs[[key]] != "inherit") inherited[[key]] <- attrs[[key]]
  if (identical(attrs[["class"]], "svglite")) inherited$.svglite <- TRUE
  inherited
}

svg_tikz_colour_3d <- function(colour) {
  if (is.null(colour) || colour %in% c("none", "transparent")) return("none")
  rgb <- grDevices::col2rgb(colour)[, 1L]
  sprintf("{rgb,255:red,%d;green,%d;blue,%d}", rgb[1L], rgb[2L], rgb[3L])
}

svg_tikz_transform_3d <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character())
  operations <- regmatches(text, gregexpr("[[:alpha:]]+[[:space:]]*[(][^)]*[)]", text))[[1L]]
  vapply(operations, function(operation) {
    name <- trimws(strsplit(operation, "(", fixed = TRUE)[[1L]][1L])
    v <- svg_numbers_3d(substring(operation, nchar(name) + 1L))
    matrix <- switch(name,
      matrix = v,
      translate = c(1, 0, 0, 1, v[1L], if (length(v) > 1L) v[2L] else 0),
      scale = c(v[1L], 0, 0, if (length(v) > 1L) v[2L] else v[1L], 0, 0),
      rotate = {
        angle <- v[1L] * pi / 180
        c <- cos(angle); s <- sin(angle)
        centre <- if (length(v) == 3L) v[2:3] else c(0, 0)
        c(c, s, -s, c, centre[1L] * (1 - c) + centre[2L] * s, centre[2L] * (1 - c) - centre[1L] * s)
      },
      stop("Unsupported SVG transform: ", name, call. = FALSE))
    scene_assert(length(matrix) == 6L && all(is.finite(matrix)), "Invalid SVG transform")
    paste0("cm={", paste(svg_number_3d(matrix[1:4]), collapse = ","), ",(",
      svg_number_3d(matrix[5L]), "pt,", svg_number_3d(matrix[6L]), "pt)}")
  }, character(1))
}

svg_tikz_geometry_3d <- function(node, context) {
  name <- xml2::xml_name(node); a <- as.list(xml2::xml_attrs(node))
  number <- function(key, default = 0, axis = 1L) {
    value <- a[[key]]
    if (is.null(value)) return(default)
    numeric <- svg_numbers_3d(value)[1L]
    if (endsWith(value, "%")) numeric <- numeric * context$size[axis] / 100
    numeric
  }
  point <- function(x, y) paste0("(", svg_number_3d(x), "pt,", svg_number_3d(y), "pt)")
  switch(name,
    path = paste0("svg {", a[["d"]], "}"),
    rect = paste0(point(number("x"), number("y", axis = 2L)), " rectangle ",
      point(number("x") + number("width"), number("y", axis = 2L) + number("height", axis = 2L))),
    circle = paste0(point(number("cx"), number("cy", axis = 2L)), " circle[radius=", svg_number_3d(number("r")), "pt]"),
    ellipse = paste0(point(number("cx"), number("cy", axis = 2L)), " ellipse[x radius=", svg_number_3d(number("rx")), "pt,y radius=", svg_number_3d(number("ry")), "pt]"),
    line = paste(point(number("x1"), number("y1", axis = 2L)), point(number("x2"), number("y2", axis = 2L)), sep = " -- "),
    polygon = , polyline = {
      xy <- matrix(svg_numbers_3d(a[["points"]]), ncol = 2L, byrow = TRUE)
      paste0(paste(vapply(seq_len(nrow(xy)), function(i) point(xy[i, 1L], xy[i, 2L]), character(1)), collapse = " -- "), if (name == "polygon") " -- cycle" else "")
    }, NULL)
}

svg_tikz_image_3d <- function(node) {
  rlang::check_installed("png")
  attrs <- as.list(xml2::xml_attrs(node))
  href <- attrs[["href"]] %or3d% attrs[["xlink:href"]]
  if (is.null(href)) href <- xml2::xml_attr(node, "xlink:href", ns = c(xlink = "http://www.w3.org/1999/xlink"))
  prefix <- "data:image/png;base64,"
  scene_assert(!is.na(href) && startsWith(href, prefix), "TikZ raster images must be embedded PNG data")
  image <- png::readPNG(base64_decode_3d(substring(href, nchar(prefix) + 1L)))
  size <- dim(image)[1:2]
  channels <- if (length(dim(image)) == 2L) 1L else dim(image)[3L]
  dim(image) <- c(size, channels)
  rgba <- array(1, c(size, 4L))
  if (channels <= 2L) for (i in 1:3) rgba[, , i] <- image[, , 1L]
  else rgba[, , 1:3] <- image[, , 1:3]
  if (channels %in% c(2L, 4L)) rgba[, , 4L] <- image[, , channels]
  colours <- matrix(grDevices::rgb(rgba[, , 1L], rgba[, , 2L], rgba[, , 3L], rgba[, , 4L]), size[1L], size[2L])
  number <- function(name, default = 0) (svg_numbers_3d(attrs[[name]] %or3d% as.character(default)))[1L]
  x <- number("x");y <- number("y");width <- number("width");height <- number("height")
  commands <- character()
  if (all(rgba[, , 4L] == 1)) {
    mean <- grDevices::rgb(mean(rgba[, , 1L]), mean(rgba[, , 2L]), mean(rgba[, , 3L]))
    commands <- sprintf("\\fill[fill=%s,draw=none] (%spt,%spt) rectangle (%spt,%spt);",
      svg_tikz_colour_3d(mean), svg_number_3d(x), svg_number_3d(y), svg_number_3d(x + width), svg_number_3d(y + height))
  }
  for (row in seq_len(size[1L])) {
    runs <- rle(colours[row, ])
    end <- cumsum(runs$lengths);start <- c(0, head(end, -1L))
    for (i in seq_along(end)) {
      alpha <- strtoi(substring(runs$values[i], 8L, 9L), 16L) / 255
      if (!alpha) next
      commands <- c(commands, sprintf("\\fill[fill=%s,fill opacity=%s,draw=none] (%spt,%spt) rectangle (%spt,%spt);",
        svg_tikz_colour_3d(substring(runs$values[i], 1L, 7L)), svg_number_3d(alpha),
        svg_number_3d(x + start[i] / size[2L] * width), svg_number_3d(y + (row - 1L) / size[1L] * height),
        svg_number_3d(x + end[i] / size[2L] * width), svg_number_3d(y + row / size[1L] * height)))
    }
  }
  commands
}

svg_tikz_node_3d <- function(node, context, inherited = list(fill = "black", stroke = "none"), clip = FALSE) {
  name <- xml2::xml_name(node)
  if (name %in% c("defs", "style", "script", "title", "desc")) return(character())
  attrs <- as.list(xml2::xml_attrs(node))
  style <- svg_tikz_style_3d(node, inherited)
  before <- after <- character()
  scope <- function(options) {
    before <<- c(before, paste0("\\begin{scope}[", options, "]"))
    after <<- c("\\end{scope}", after)
  }
  if (name == "svg") {
    view <- svg_numbers_3d(attrs[["viewBox"]])
    if (length(view) == 4L) {
      width <- svg_numbers_3d(attrs[["width"]])[1L];height <- svg_numbers_3d(attrs[["height"]])[1L]
      if (!is.finite(width)) width <- context$size[1L]
      if (!is.finite(height)) height <- context$size[2L]
      scope(sprintf("cm={%s,0,0,%s,(%spt,%spt)}", svg_number_3d(width / view[3L]),
        svg_number_3d(height / view[4L]), svg_number_3d(-view[1L]), svg_number_3d(-view[2L])))
      context$stroke_scale <- context$stroke_scale * sqrt(width * height / prod(view[3:4]))
      context$size <- view[3:4]
    }
  }
  transform <- attrs[["transform"]]
  if (!is.null(transform)) for (operation in svg_tikz_transform_3d(transform)) scope(operation)
  opacity <- attrs[["opacity"]]
  if (!is.null(opacity) && as.numeric(opacity) < 1) scope(paste0("transparency group,opacity=", opacity))
  for (key in c("clip-path", "mask")) if (!is.null(attrs[[key]])) {
    id <- substring(attrs[[key]], 6L, nchar(attrs[[key]]) - 1L)
    definition <- context$definitions[[id]]
    scene_assert(!is.null(definition), paste("Unknown SVG clip", id))
    scope("")
    clips <- xml2::xml_children(definition)
    paths <- vapply(clips, function(child) svg_tikz_geometry_3d(child, context) %or3d% "", character(1))
    rule <- if (any(xml2::xml_attr(clips, "clip-rule") == "evenodd", na.rm = TRUE)) "even odd rule" else "nonzero rule"
    before <- c(before, if (rule == "even odd rule") "\\pgfseteorule" else "\\pgfsetnonzerorule",
      paste0("\\clip ", paste(paths, collapse = " "), ";"))
  }
  geometry <- svg_tikz_geometry_3d(node, context)
  body <- character()
  if (!is.null(geometry)) {
    if (isTRUE(style$.svglite)) {
      if (is.null(style$stroke) || style$stroke == "none") style$stroke <- if (is.null(attrs[["style"]]) && is.null(attrs[["stroke"]])) "black" else style$stroke
      if (is.null(attrs[["style"]]) && is.null(attrs[["fill"]])) style$fill <- "none"
    }
    options <- c(paste0("fill=", svg_tikz_colour_3d(style$fill)), paste0("draw=", svg_tikz_colour_3d(style$stroke)))
    for (pair in list(c("stroke-width", "line width"), c("fill-opacity", "fill opacity"), c("stroke-opacity", "draw opacity"),
      c("stroke-linecap", "line cap"), c("stroke-linejoin", "line join"), c("stroke-miterlimit", "miter limit"))) {
      value <- style[[pair[1L]]]
      if (!is.null(value) && pair[1L] == "stroke-width") value <- svg_number_3d(svg_numbers_3d(value)[1L] * context$stroke_scale)
      if (!is.null(value)) options <- c(options, paste0(pair[2L], "=", value, if (pair[1L] == "stroke-width") "pt" else ""))
    }
    if (identical(style$`fill-rule`, "evenodd")) options <- c(options, "even odd rule")
    if (name == "rect" && !is.null(attrs[["rx"]])) options <- c(options, paste0("rounded corners=", svg_number_3d(as.numeric(attrs[["rx"]]) * context$stroke_scale), "pt"))
    action <- if (clip) "clip" else if (identical(style$stroke, "none")) "fill" else if (identical(style$fill, "none")) "draw" else "path"
    body <- paste0("\\", action, "[", paste(options, collapse = ","), "] ", geometry, ";")
  } else if (name == "image") body <- svg_tikz_image_3d(node)
  else if (name == "text") {
    x <- (svg_numbers_3d(attrs[["x"]] %or3d% "0"))[1L]
    y <- (svg_numbers_3d(attrs[["y"]] %or3d% "0"))[1L]
    size <- svg_numbers_3d(style$`font-size` %or3d% "11")[1L]
    anchor <- switch(style$`text-anchor` %or3d% "start", start = "west", end = "east", "center")
    if (!(style$`dominant-baseline` %or3d% "auto") %in% c("central", "middle")) anchor <- if (anchor == "center") "base" else paste("base", anchor)
    face <- paste0("\\sffamily", if (identical(style$`font-weight`, "bold")) "\\bfseries", if (identical(style$`font-style`, "italic")) "\\itshape")
    body <- sprintf("\\node[transform shape,anchor=%s,inner sep=0pt,text=%s] at (%spt,%spt) {\\scalebox{1}[-1]{%s\\fontsize{%s}{%s}\\selectfont %s}};",
      anchor, svg_tikz_colour_3d(style$fill %or3d% "black"), svg_number_3d(x), svg_number_3d(y), face,
      svg_number_3d(size), svg_number_3d(size * 1.2), tex_escape_3d(xml2::xml_text(node)))
  } else if (name == "use") {
    href <- attrs[["href"]] %or3d% xml2::xml_attr(node, "href", ns = c(xlink = "http://www.w3.org/1999/xlink"))
    id <- substring(href, 2L)
    definition <- context$definitions[[id]]
    scene_assert(!is.null(definition), paste("Unknown SVG reference", id))
    body <- svg_tikz_node_3d(definition, context, style)
  } else if (name %in% c("svg", "g")) {
    body <- unlist(lapply(xml2::xml_children(node), svg_tikz_node_3d, context = context, inherited = style), use.names = FALSE)
  } else stop("SVG element cannot be written as TikZ paths: ", name, call. = FALSE)
  c(before, body, after)
}

tikz_code_3d <- function(svg, standalone) {
  root <- xml2::xml_root(xml2::read_xml(svg))
  size <- svg_numbers_3d(xml2::xml_attr(root, "viewBox"))[3:4]
  definitions <- xml2::xml_find_all(root, ".//*[@id]")
  context <- list(size = size, stroke_scale = 72.27 / 25.4,
    definitions = stats::setNames(as.list(definitions), xml2::xml_attr(definitions, "id")))
  content <- unlist(lapply(xml2::xml_children(root), svg_tikz_node_3d, context = context), use.names = FALSE)
  factor <- 72.27 / 25.4
  lines <- c(if (standalone) c("\\documentclass[tikz,border=0pt]{standalone}", "\\usepackage{graphicx}", "\\usetikzlibrary{svg.path}", "\\begin{document}"),
    "\\begin{tikzpicture}[x=1pt,y=1pt]",
    sprintf("\\path[use as bounding box] (0mm,0mm) rectangle (%smm,-%smm);", svg_number_3d(size[1L]), svg_number_3d(size[2L])),
    sprintf("\\begin{scope}[xscale=%s,yscale=-%s]", svg_number_3d(factor), svg_number_3d(factor)),
    content, "\\end{scope}", "\\end{tikzpicture}", if (standalone) "\\end{document}")
  paste(lines, collapse = "\n")
}

asy_code_3d <- function(plot, panel, width, height, prc) {
  options <- export_options_3d(TRUE, "line", "mesh", 1)
  scene <- export_scene_3d(plot, panel, options, width, height, 96)
  triple <- function(x) paste0("(", paste(svg_number_3d(x), collapse = ","), ")")
  pen <- function(colour) paste0("rgb(", paste(svg_number_3d(colour[1:3]), collapse = ","), ")+opacity(", svg_number_3d(colour[4L]), ")")
  camera <- scene$camera
  lines <- c(paste0("settings.prc=", tolower(as.character(prc)), ";"), "import three;",
    sprintf("size(%smm,%smm);", svg_number_3d(width / 96 * 25.4), svg_number_3d(height / 96 * 25.4)),
    sprintf("currentprojection=%s(camera=%s,up=%s,target=%s,center=false%s);", camera$projection,
      triple(camera$eye), triple(camera$up), triple(camera$target), if (camera$projection == "perspective")
        paste0(",autoadjust=false,angle=", svg_number_3d(camera$yfov * 180 / pi)) else ""))
  surfaces <- c("surface ggplotModel;", "material[] ggplotMaterials;")
  strokes <- character()
  for (node in scene$nodes) {
    meshes <- export_mesh_3d(node, options)
    for (mesh in meshes) if (mesh$mode == "triangles") for (i in seq_len(ncol(mesh$index))) {
      ids <- mesh$index[, i]
      path <- paste(vapply(ids, function(j) triple(mesh$position[j, ]), character(1)), collapse = "--")
      colour <- colMeans(mesh$colour[ids, , drop = FALSE])
      material <- if (isTRUE(mesh$lit)) paste0("material(diffusepen=", pen(colour), ")") else
        paste0("material(diffusepen=black,emissivepen=", pen(colour), ",specularpen=black,opacity=", svg_number_3d(colour[4L]), ",lightOn=false)")
      surfaces <- c(surfaces, paste0("ggplotModel.push(", path, "--cycle);"),
        paste0("ggplotMaterials.push(", material, ");"))
    }
    if (!is.null(node$paths)) {
      p <- node$paths
      for (i in seq_len(nrow(p$colour))) {
        rows <- which(p$path == i)
        path <- paste(vapply(rows, function(j) triple(p$position[j, ]), character(1)), collapse = "--")
        strokes <- c(strokes, paste0("draw(", path, ",", pen(p$colour[i, ]), "+linewidth(", svg_number_3d(p$width_mm[i]), "mm));"))
      }
    }
    if (!is.null(node$text)) for (i in seq_along(node$text$label)) {
      strokes <- c(strokes, paste0("label(", encodeString(tex_escape_3d(node$text$label[i]), quote = '"'), ",",
        triple(node$text$position[i, ]), ",p=", pen(node$text$colour[i, ]), "+fontsize(", svg_number_3d(node$text$fontsize[i]), "pt));"))
    }
  }
  paste(c(lines, surfaces, "draw(ggplotModel,surfacepen=ggplotMaterials,meshpen=nullpen);", strokes), collapse = "\n")
}

#' Export TikZ paths or an Asymptote scene
#'
#' TikZ writes the complete SVG paint order as editable fill, draw and text
#' commands, including clip paths, furniture and legends. Requires TikZ's
#' `svg.path` library. Raster colourbars and annotations become coloured pixel
#' rectangles using the optional png decoder. TeX selects the text font.
#' Asymptote writes model-space
#' triangles, paths, marker solids, text and a camera, so it can re-render the
#' scene from another view or produce an interactive PRC PDF.
#' For a PRC PDF, compile with `asy -tex pdflatex -f pdf plot.asy` and install
#' the TeX packages media9 and ocgx2. A PDF reader with PRC support supplies
#' the interactive view.
#' @param plot A ggplot with [coord_3d()].
#' @param format `"tikz"` or `"asy"`.
#' @param file Optional output path; NULL returns the code without writing.
#' @param width,height Plot size in pixels at 96 pixels per inch.
#' @param panel Panel index for the Asymptote scene; TikZ includes every panel.
#' @param standalone Whether TikZ includes a LaTeX document preamble.
#' @param prc Whether Asymptote should include PRC in its PDF output.
#' @return One character string of source code, invisibly when `file` is given.
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3)),
#'   ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d()
#' code <- export_code_3d(p, "asy")
#' if (requireNamespace("xml2", quietly = TRUE) && requireNamespace("svglite", quietly = TRUE)) {
#'   code <- export_code_3d(p, "tikz")
#' }
#' @export
export_code_3d <- function(plot, format = c("tikz", "asy"), file = NULL, width = 640L, height = 640L,
                            panel = 1L, standalone = TRUE, prc = FALSE) {
  format <- match.arg(format)
  standalone <- check_flag_3d(standalone, "standalone");prc <- check_flag_3d(prc, "prc")
  code <- switch(format, tikz = tikz_code_3d(native_svg_3d(plot, width, height, FALSE), standalone),
    asy = asy_code_3d(plot, panel, width, height, prc))
  if (is.null(file)) return(code)
  scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && dir.exists(dirname(file)), "file must be an output path in an existing directory")
  writeLines(code, file, useBytes = TRUE)
  invisible(code)
}
