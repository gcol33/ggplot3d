# Interactive viewing

[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
opens a plot in an interactive browser widget with orbit controls,
tooltips and selection. It uses WebGL 2, the browser graphics API, and
the R packages [htmlwidgets](https://www.htmlwidgets.org/) and
[svglite](https://svglite.r-lib.org/) to display the scene and its
surrounding plot elements. This article builds a viewer and links its
selections to a filter. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).

## Example

The motor-car data contain weight in thousands of pounds, fuel economy
in miles per US gallon, gross horsepower and cylinder count. Model names
identify the observations. Map them to the hover text and selection key
before creating the widget.

``` r

cars <- data.frame(mtcars, model = rownames(mtcars))
p <- ggplot(cars, aes(wt, mpg, z = hp, tooltip = model, data_id = model)) +
  geom_point_3d(size = 4, colour = PAL["blue"]) +
  coord_3d() + labs(x = "weight", y = "mpg", z = "horsepower")
```

Drag to orbit and use the wheel or a pinch gesture to zoom. Arrow keys
orbit, and the plus and minus keys zoom. Camera code copies the current
angles into a coordinate call, and Reset restores the initial view.

``` r

widget <- view_3d(p, width = 624, height = 432, elementId = "cars-view")
widget
```

## Picking and selection

Picking identifies the primitive under the pointer by its identifier in
an offscreen image. Hover over a point to see its model name, then click
it to select that key. Shift-click or control-click adds and removes
keys; clicking empty space clears the selection. Unselected geometry
dims while a selection exists.

Rows sharing a key highlight together across layers. In the absence of a
key mapping, each row in each layer receives its own key. The default
tooltip lists mapped values. Use `tooltip = FALSE` to disable those
default tooltips; an explicit tooltip mapping still supplies its text.

``` r

view_3d(p, selection = "single", width = 624, height = 432)
```

Set the selection mode to `"none"` to disable it. In a Shiny
application, the widget identifier above gives the selected keys in
`input[["cars-view_selected"]]` and camera updates in
`input[["cars-view_camera"]]`. Browser integrations can listen on the
widget element for `ggplot3d:hover`, `ggplot3d:select` and
`ggplot3d:camera` events.

## Shiny applications

[Shiny](https://shiny.posit.co/) connects user inputs to reactive R
expressions. Use
[`view3dOutput()`](https://gillescolling.com/ggplot3d/reference/view_3d-shiny.md)
for the output container and
[`renderView3d()`](https://gillescolling.com/ggplot3d/reference/view_3d-shiny.md)
to return a viewer whenever its inputs change. This module filters the
cars by horsepower. Its selected keys and camera values appear below the
viewer. Each module’s `NS()` namespace gives its viewer and input events
independent identifiers.

``` r

carUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("hp"), "Minimum horsepower", 50, 300, 50),
    view3dOutput(ns("scene"), height = "400px"),
    shiny::verbatimTextOutput(ns("events")))
}
carServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$scene <- renderView3d({
      filtered <- cars[cars$hp >= input$hp, ]
      plot <- ggplot(filtered, aes(wt, mpg, z = hp,
        tooltip = model, data_id = model)) + geom_point_3d(size = 4) + coord_3d()
      view_3d(plot)
    })
    output$events <- shiny::renderPrint(list(
      selected = input$scene_selected, camera = input$scene_camera))
  })
}
ui <- shiny::fluidPage(carUI("first"), carUI("second"))
server <- function(input, output, session) {
  carServer("first")
  carServer("second")
}
shiny::shinyApp(ui, server)
```

Inside the module, use the local input names `scene_selected` and
`scene_camera`. The browser sends events with the output container’s
namespaced identifier. Leave `elementId` unset in
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
so Shiny assigns that identifier.

## Several panels

Facets stay interactive and retain their strips, titles and legends. The
example splits the cars by transmission, with zero denoting automatic
and one manual. Orbiting either panel turns both by the same amount,
retaining their relative camera angles.

``` r

panels <- p + facet_wrap(~am, ncol = 1,
  labeller = as_labeller(c("0" = "automatic", "1" = "manual")))
view_3d(panels, width = 624, height = 576, link = TRUE)
```

Set `link = FALSE` to move the panel under the pointer alone. An index
in `panel` selects one facet without the surrounding plot elements. The
viewers below show automatic and manual cars separately. A shared string
links their camera changes on the same page.

``` r

view_3d(panels, panel = 1, link = "cars-cameras", width = 624, height = 432)
```

The second viewer uses the same link name. Orbit, zoom and reset changes
travel between the two, with angle changes added and zoom factors
multiplied.

``` r

other <- panels + coord_3d(azimuth = 30)
#> Coordinate system already present. Adding coord_3d() replaces it.
view_3d(other, panel = 2, link = "cars-cameras", width = 624, height = 432)
```

## Linking data with crosstalk

[crosstalk](https://rstudio.github.io/crosstalk/) links selections and
filters between widgets using shared row keys. Construct the plot from
the shared object’s original data and pass that same shared object to
the viewer. The cylinder filter below hides cars excluded by the chosen
cylinder counts.

``` r

shared <- crosstalk::SharedData$new(cars, key = ~model, group = "cars")
linked <- ggplot(shared$origData(), aes(wt, mpg, z = hp, tooltip = model)) +
  geom_point_3d(size = 4, colour = PAL["blue"]) + coord_3d()
```

The original rows let the viewer take its keys from the shared object.
Other crosstalk widgets in the group receive its selections and can send
selections back. Keep the plotted rows aligned with the shared data by
constructing the plot from that original table.

``` r

htmltools::tagList(
  crosstalk::filter_checkbox("cylinders", "Cylinders", shared, ~cyl),
  view_3d(linked, data = shared, width = 624, height = 432))
```

Cylinders

4

6

8

## Saving the widget

Save the widget as HTML to share an interactive view. This example
writes the supporting files beside the HTML; distribute the HTML and its
companion directory together. Use `selfcontained = TRUE` for a single
HTML file when Pandoc is available.

``` r

directory <- tempfile("cars-viewer-")
dir.create(directory)
html <- file.path(directory, "cars.html")
htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
file.info(html)$size > 0
#> [1] TRUE
```

## Rendering behaviour

Opaque geometry uses a depth buffer. Transparent geometry uses weighted
blended order-independent transparency: linear-light colour weights and
fragment transmittances accumulate in separate passes, then composite
over the opaque scene. This approximates the colour of intersecting
sheets and keeps it independent of primitive order. Transparent scenes
require the WebGL 2 `EXT_color_buffer_float` extension. Circular markers
use one GPU instance per point and a disc fragment shader for their fill
and border. Text is drawn as plain browser text over the scene; a label
is hidden only when its anchor is clipped. Geometry, mapped colours and
lighting are transferred once; camera interaction runs in the browser.

Stereo plots use their central camera in the viewer. Static eye pairs
and anaglyphs are covered in [Camera and
framing](https://gillescolling.com/ggplot3d/articles/camera.md). Use
[Documents](https://gillescolling.com/ggplot3d/articles/documents.md) to
switch between an HTML viewer and a static figure from the same knitr or
Quarto chunk.
