library(ggplot2)
library(ggplot3d)

paper <- "#F5F6F8"
hex <- function(radius) {
  angle <- seq(-90, 210, by = 60) * pi / 180
  paste(sprintf("%.2f,%.2f", 260 + radius * cos(angle),
                300 + radius * sin(angle)), collapse = " ")
}

# The two sampled surfaces intersect along x = 0.
surface <- expand.grid(x = seq(-1, 1, length.out = 7),
                       y = seq(-1, 1, length.out = 7))
surface$z <- with(surface, 0.65 * x + 0.18 * cos(pi * y))
crossing <- surface
crossing$z <- with(crossing, -0.65 * x + 0.18 * cos(pi * y))
scene <- ggplot(surface, aes(x, y, z = z)) +
  geom_surface_3d(fill = "#2E6F9E", colour = NA) +
  geom_surface_3d(data = crossing, fill = "#9E5E0F", colour = NA) +
  coord_3d(azimuth = -42, elevation = 32, axes = character(),
           panes = character(), box = "none", expand = FALSE,
           light = light_3d(ambient = 0.65, diffuse = 0.35)) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0),
                       plot.background = element_rect(fill = NA, colour = NA))

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
path <- tempfile(fileext = ".svg")
ggsave(path, scene, device = svglite::svglite, width = 6, height = 4.4,
       bg = "transparent")
rendered <- xml2::read_xml(path)
children <- xml2::xml_children(rendered)
geometry <- paste(as.character(children), collapse = "\n")
unlink(path)

# A periodic projected grid translates by one cell in a twelve-second loop.
grid <- paste(vapply(seq(-600, 900, by = 60), function(x) {
  sprintf('<path d="M %s -200 L %s 800"/>', x, x + 500)
}, character(1)), collapse = "\n")
grid_layer <- function(class, animate = FALSE) paste0(
  '<g class="', class, '" stroke="#2A7A7A" stroke-width="2.5" opacity="0.18" fill="none">',
  if (animate) '<animateTransform attributeName="transform" type="translate" from="0 0" to="60 0" dur="12s" repeatCount="indefinite"/>' else '',
  grid, '</g>')

svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 520 600" role="img" aria-label="ggplot3d logo">',
  '<title>ggplot3d logo</title>',
  '<style>.still{display:none}@media(prefers-reduced-motion:reduce){.moving{display:none}.still{display:inline}}</style>',
  '<defs><clipPath id="hex"><polygon points="', hex(288), '"/></clipPath></defs>',
  '<polygon points="', hex(288), '" fill="', paper, '"/>',
  '<g clip-path="url(#hex)">', grid_layer('moving', TRUE), grid_layer('still'),
  '<svg class="svglite" x="-45" y="72" width="610" height="340" viewBox="0 0 432 316.8">', geometry, '</svg>',
  '</g><text x="260" y="472" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-weight="bold" font-style="italic" font-size="78" fill="#2E6F9E" stroke="',
  paper, '" stroke-width="10" paint-order="stroke">ggplot3d</text>',
  '<polygon points="', hex(291), '" fill="none" stroke="#2B3A4A" stroke-width="18" stroke-linejoin="round"/></svg>')
stopifnot(!grepl("Inf|NaN", svg), nchar(svg, type = "bytes") < 200000)
writeLines(svg, "man/figures/logo.svg", useBytes = TRUE)
