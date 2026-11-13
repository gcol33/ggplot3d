library(ggplot2)
library(ggplot3d)

# Maunga Whau (datasets::volcano, 10 m grid, heights 94 to 195 m) in 10 m
# height bands: a flat ggplot2 contour map, and the same bands drawn by
# geom_contour_filled_3d() on the surface in coord_3d().
heights <- data.frame(x = 10 * rep(seq_len(nrow(volcano)) - 1, ncol(volcano)),
                      y = 10 * rep(seq_len(ncol(volcano)) - 1, each = nrow(volcano)),
                      z = as.vector(volcano))

blue <- "#2E6F9E"
red <- "#B5342B"
ink <- "#5C6166"
font <- "'Segoe UI', Helvetica, Arial, sans-serif"
mono <- "Consolas, 'SFMono-Regular', Menlo, monospace"

breaks <- seq(90, 200, by = 10)
bands <- colorRampPalette(c(blue, "#E8E2D8", red))(length(breaks) - 1)
base <- ggplot(heights, aes(x, y, z = z)) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

flat <- base +
  geom_contour_filled(breaks = breaks) +
  scale_fill_manual(values = bands, guide = "none") +
  geom_contour(breaks = breaks, colour = "white", linewidth = 0.3) +
  coord_equal(expand = FALSE)
lifted <- base +
  geom_contour_filled_3d(breaks = breaks) +
  scale_fill_gradientn(colours = bands, limits = range(breaks), guide = "none") +
  coord_3d(azimuth = -25, elevation = 26, aspect = c(1.43, 1, 0.75),
           axes = character(), panes = character(), box = "none", expand = FALSE,
           backend = backend_3d("raster", dpi = 200, antialias = 3))

# Renders a plot with svglite and returns its children for nesting.
svg_body <- function(p, width, height) {
  path <- tempfile(fileext = ".svg")
  ggsave(path, p, device = svglite::svglite, width = width, height = height, bg = "transparent")
  on.exit(unlink(path))
  doc <- xml2::read_xml(path)
  list(body = paste(as.character(xml2::xml_children(doc)), collapse = "\n"),
       viewbox = xml2::xml_attr(doc, "viewBox"))
}

# Frame in user units (72 per inch, as svglite writes).
height_in <- 5
width_in <- 12.5
W <- 72 * width_in
H <- 72 * height_in

flat_w <- 3.9
flat_h <- flat_w * 610 / 870
lifted_w <- 5.8
lifted_h <- 5
flat_svg <- svg_body(flat, flat_w, flat_h)
lifted_svg <- svg_body(lifted, lifted_w, lifted_h)

nest <- function(part, x, y, w, h) sprintf(
  '<svg x="%.1f" y="%.1f" width="%.1f" height="%.1f" viewBox="%s">%s</svg>',
  x, y, w, h, part$viewbox, part$body)

flat_x <- 0.35 * 72
flat_y <- (H - 72 * flat_h) / 2
lifted_x <- W - 72 * lifted_w
lifted_y <- (H - 72 * lifted_h) / 2

# Label between the two plots, with a soft white glow traced round the letters.
label_x <- flat_x + 72 * flat_w + 1.45 * 72
label_y <- H / 2 - 8
arrow_x0 <- label_x - 62
arrow_x1 <- label_x + 62
arrow_y <- label_y + 24

svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" ',
  sprintf('viewBox="0 0 %d %d" width="%d" height="%d" role="img" ', W, H, W, H),
  'aria-label="Height bands of the Maunga Whau volcano every 10 metres, from blue at 90 metres through beige to red at 200 metres, drawn as a flat ggplot2 contour map on the left and, with coord_3d(), draped over the shaded volcano surface on the right.">',
  '<defs><filter id="glow" x="-20%" y="-50%" width="140%" height="200%">',
  '<feMorphology in="SourceAlpha" operator="dilate" radius="2" result="thick"/>',
  '<feGaussianBlur in="thick" stdDeviation="5" result="blur"/>',
  '<feFlood flood-color="#FFFFFF"/><feComposite in2="blur" operator="in" result="halo"/>',
  '<feMerge><feMergeNode in="halo"/><feMergeNode in="halo"/><feMergeNode in="SourceGraphic"/></feMerge>',
  '</filter>',
  sprintf('<marker id="head" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="%s"/></marker>', ink),
  '</defs>',
  sprintf('<rect width="%d" height="%d" fill="#FFFFFF"/>', W, H),
  nest(flat_svg, flat_x, flat_y, 72 * flat_w, 72 * flat_h),
  nest(lifted_svg, lifted_x, lifted_y, 72 * lifted_w, 72 * lifted_h),
  sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-family="%s" font-size="32" font-weight="bold" fill="%s" filter="url(#glow)">coord_3d()</text>',
          label_x, label_y, mono, ink),
  sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="2.5" marker-end="url(#head)"/>',
          arrow_x0, arrow_y, arrow_x1, arrow_y, ink),
  '</svg>')

stopifnot(!grepl("Inf|NaN", gsub("base64,[^\"]*", "", svg)))
writeLines(svg, "man/figures/ggplot3d-hero.svg", useBytes = TRUE)
