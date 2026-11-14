register_exporter_3d("3mf", function(frames, file, options, fps) write_3mf_3d(frames[[1L]], file, options), triangles = TRUE, printing = TRUE, print_only = TRUE)

# 3MF Core 1.2 with Materials and Properties 1.2.1 colour groups.
write_3mf_3d <- function(scene, file, options) {
  scene_assert(!is.null(scene$printing), "3MF output requires print = print_3d()")
  mesh <- scene$printing_mesh
  rgb <- mesh$colour;rgb[] <- pmax(0, pmin(1, rgb))
  hex <- grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L], rgb[, 4L])
  tri <- t(mesh$triangles)
  colours <- hex
  palette <- unique(colours);property <- match(colours, palette) - 1L
  vertices <- sprintf('<vertex x="%.17g" y="%.17g" z="%.17g"/>', mesh$position[, 1L], mesh$position[, 2L], mesh$position[, 3L])
  triangles <- sprintf('<triangle v1="%d" v2="%d" v3="%d" pid="1" p1="%d" p2="%d" p3="%d"/>',
    tri[1L, ] - 1L, tri[2L, ] - 1L, tri[3L, ] - 1L, property, property, property)
  model <- paste0('<?xml version="1.0" encoding="UTF-8"?>',
    '<model unit="millimeter" xml:lang="en-US" xmlns="http://schemas.microsoft.com/3dmanufacturing/core/2015/02" ',
    'xmlns:m="http://schemas.microsoft.com/3dmanufacturing/material/2015/02" requiredextensions="m">',
    '<metadata name="Application">ggplot3d</metadata><resources><m:colorgroup id="1">',
    paste0('<m:color color="', palette, '"/>', collapse = ""), '</m:colorgroup>',
    '<object id="2" type="model" pid="1" pindex="0" name="', knit_escape_3d(basename(file)), '"><mesh><vertices>',
    paste(vertices, collapse = ""), '</vertices><triangles>', paste(triangles, collapse = ""),
    '</triangles></mesh></object></resources><build><item objectid="2"/></build></model>')
  types <- paste0('<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="model" ContentType="application/vnd.ms-package.3dmanufacturing-3dmodel+xml"/></Types>')
  relationships <- paste0('<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Target="/3D/3dmodel.model" Id="model" Type="http://schemas.microsoft.com/3dmanufacturing/2013/01/3dmodel"/></Relationships>')
  names <- c("[Content_Types].xml", "_rels/.rels", "3D/3dmodel.model")
  writeBin(print_zip_cpp(names, lapply(list(types, relationships, model), function(x) charToRaw(enc2utf8(x)))), file)
  invisible(file)
}
