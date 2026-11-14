# Shiny bindings for the interactive 3D viewer

Place a viewer in a Shiny user interface and render a reactive
expression returning
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md).
In a module, pass `ns("scene")` to `view3dOutput()` and assign
`output$scene` inside
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
Camera updates arrive in `input$scene_camera`; selected data keys arrive
in `input$scene_selected`. Shiny supplies the widget identifier, so
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
can leave `elementId` at its default.

## Usage

``` r
view3dOutput(outputId, width = "100%", height = "400px")

renderView3d(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Output identifier, namespaced in a module's user interface.

- width, height:

  CSS dimensions, default `"100%"` and `"400px"`.

- expr:

  An expression returning a `ggplot3d_view` htmlwidget.

- env:

  Environment in which to evaluate `expr`.

- quoted:

  Whether `expr` is already quoted.

## Value

`view3dOutput()` returns an HTML output container. `renderView3d()`
returns a Shiny render function.

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {
  ui <- shiny::fluidPage(view3dOutput("scene"))
  server <- function(input, output, session) {
    output$scene <- renderView3d({
      p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
        geom_point_3d() + coord_3d()
      view_3d(p, panel = 1)
    })
  }
  app <- shiny::shinyApp(ui, server)
}
```
