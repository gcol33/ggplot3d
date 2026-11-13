library(ggplot2)
library(ggplot3d)

panel <- "#EBEBEB"
ink <- "#333333"
hex <- function(radius) {
  angle <- seq(-90, 210, by = 60) * pi / 180
  paste(sprintf("%.2f,%.2f", 260 + radius * cos(angle),
                300 + radius * sin(angle)), collapse = " ")
}

# A smooth bump filled along ggplot2's default continuous gradient.
surface <- expand.grid(x = seq(-2, 2, length.out = 21),
                       y = seq(-2, 2, length.out = 21))
surface$z <- with(surface, exp(-(x^2 + y^2) / 1.6) + 0.25 * cos(1.6 * x))
scene <- ggplot(surface, aes(x, y, z = z, fill = z)) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(guide = "none") +
  coord_3d(azimuth = -50, elevation = 28, axes = character(),
           panes = character(), box = "none", expand = FALSE,
           light = light_3d(ambient = 0.7, diffuse = 0.3)) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0),
                       plot.background = element_rect(fill = NA, colour = NA))

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
path <- tempfile(fileext = ".svg")
ggsave(path, scene, device = svglite::svglite, width = 6, height = 4.4,
       bg = "transparent")
rendered <- xml2::read_xml(path)
geometry <- paste(as.character(xml2::xml_children(rendered)), collapse = "\n")
unlink(path)

# theme_grey()'s panel grid: white major lines every 80 units, minor lines
# halfway between, drifting by one major step in a twelve-second loop.
step <- 80
lines <- function(at, width) paste0(
  '<g stroke-width="', width, '">',
  paste(sprintf('<path d="M %s -100 V 700"/><path d="M -100 %s H 700"/>', at, at),
        collapse = ""), '</g>')
major <- seq(-step * 2, 520 + step * 2, by = step)
grid <- paste0(lines(major, 5), lines(major + step / 2, 2.5))
grid_layer <- function(class, animate = FALSE) paste0(
  '<g class="', class, '" stroke="#FFFFFF" fill="none">',
  if (animate) sprintf('<animateTransform attributeName="transform" type="translate" from="0 0" to="%s %s" dur="12s" repeatCount="indefinite"/>', step, -step) else '',
  grid, '</g>')

svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 520 600" role="img" aria-label="ggplot3d logo">',
  '<title>ggplot3d logo</title>',
  '<style>.still{display:none}@media(prefers-reduced-motion:reduce){.moving{display:none}.still{display:inline}}</style>',
  '<defs><clipPath id="hex"><polygon points="', hex(288), '"/></clipPath></defs>',
  '<polygon points="', hex(288), '" fill="', panel, '"/>',
  '<g clip-path="url(#hex)">', grid_layer('moving', TRUE), grid_layer('still'),
  '<svg class="svglite" x="-95" y="18" width="710" height="404" viewBox="0 0 432 316.8">', geometry, '</svg>',
  '</g><text x="260" y="440" text-anchor="middle" font-family="Helvetica, Arial, sans-serif" font-weight="bold" font-size="76" letter-spacing="-1" fill="', ink, '" stroke="',
  panel, '" stroke-width="10" paint-order="stroke">ggplot3d</text>',
  '<polygon points="', hex(291), '" fill="none" stroke="', ink, '" stroke-width="18" stroke-linejoin="round"/></svg>')
stopifnot(!grepl("Inf|NaN", svg), nchar(svg, type = "bytes") < 200000)
writeLines(svg, "man/figures/logo.svg", useBytes = TRUE)
