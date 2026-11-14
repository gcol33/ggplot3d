test_that("Shiny output has the htmlwidget binding, dimensions and dependencies", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmlwidgets")
  output <- view3dOutput(shiny::NS("first", "scene"), "75%", "320px")
  expected <- htmlwidgets::shinyWidgetOutput("first-scene", "ggplot3d_view",
    "75%", "320px", package = "ggplot3d")
  expect_identical(output, expected)
  html <- htmltools::renderTags(output)
  expect_match(html$html, 'id="first-scene"', fixed = TRUE)
  expect_match(html$html, "ggplot3d_view html-widget html-widget-output", fixed = TRUE)
  expect_match(html$html, "width:75%;height:320px", fixed = TRUE)
  expect_true(any(vapply(html$dependencies, function(x) x$name == "ggplot3d-view", logical(1))))
})

test_that("Shiny renders quoted and unquoted expressions in their environments", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmlwidgets")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d() + coord_3d()
  widget <- view_3d(p, panel = 1, width = 300, height = 300)
  env <- new.env(parent = environment())
  env$local_widget <- widget
  renderers <- list(
    renderView3d(local_widget, env = env),
    renderView3d(quote(local_widget), env = env, quoted = TRUE),
    local({ captured <- widget; renderView3d(captured) }))
  for (renderer in renderers) {
    shiny::testServer(function(input, output, session) {
      output$scene <- renderer
    }, {
      value <- jsonlite::fromJSON(output$scene, simplifyVector = FALSE)
      expect_identical(value$x$schema_version, widget$x$schema_version)
      expect_identical(value$x$capabilities, widget$x$capabilities)
      expect_equal(value$x$frame$width_mm, widget$x$frame$width_mm)
    })
  }
})

test_that("Shiny modules update reactive scenes and keep event inputs independent", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmlwidgets")
  module <- function(id) shiny::moduleServer(id, function(input, output, session) {
    output$scene <- renderView3d({
      p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
        geom_point_3d() + coord_3d(azimuth = input$angle)
      view_3d(p, panel = 1, width = 240, height = 240)
    })
    list(scene = function() output$scene,
      selected = shiny::reactive(input$scene_selected),
      camera = shiny::reactive(input$scene_camera))
  })
  shiny::testServer(function(input, output, session) {
    first <- module("first")
    second <- module("second")
  }, {
    session$setInputs(`first-angle` = 15, `second-angle` = 60)
    camera <- function(value) jsonlite::fromJSON(value, simplifyVector = FALSE)$x$panels[[1]]$camera$azimuth
    expect_equal(camera(first$scene()), 15)
    expect_equal(camera(second$scene()), 60)
    session$setInputs(`first-angle` = 35, `first-scene_selected` = "Mazda RX4",
      `first-scene_camera` = list(azimuth = 35))
    expect_equal(camera(first$scene()), 35)
    expect_equal(camera(second$scene()), 60)
    expect_identical(first$selected(), "Mazda RX4")
    expect_equal(first$camera()$azimuth, 35)
    expect_null(second$selected())
    expect_null(second$camera())
  })
})

test_that("Shiny helpers check optional dependencies before delegating", {
  calls <- character()
  local_mocked_bindings(motion_require_3d = function(package) {
    calls <<- c(calls, package)
    stop(paste("Install", package))
  })
  expect_error(view3dOutput("scene"), "Install shiny")
  expect_error(renderView3d(NULL), "Install shiny")
  expect_identical(calls, c("shiny", "shiny"))
})

test_that("viewer replacement releases buffers and keeps one event subscription", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("crosstalk")
  shared <- crosstalk::SharedData$new(mtcars, key = rownames(mtcars), group = "shiny-lifecycle")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d(size = 4) + coord_3d()
  widget <- view_3d(p, data = shared, panel = 1, width = 400, height = 400, elementId = "first-scene")
  other <- view_3d(p, panel = 1, width = 300, height = 300, elementId = "second-scene")
  b <- viewer_browser(list(widget, other))
  on.exit(b$close(), add = TRUE)
  b$eval(paste0("window.sent = []; window.Shiny = {setInputValue: (id, value) => sent.push({id, value})};",
    "window.deleted = 0; window.handles = 0;",
    "const gl = document.querySelector('#first-scene canvas').getContext('webgl2');",
    "const del = gl.deleteBuffer.bind(gl); gl.deleteBuffer = x => { deleted++; del(x); };",
    "const close = crosstalk.SelectionHandle.prototype.close;",
    "crosstalk.SelectionHandle.prototype.close = function() { handles++; return close.call(this); };",
    "window.binding = HTMLWidgets.find('#first-scene');"))
  payload <- htmlwidgets:::toJSON(widget$x)
  for (i in 1:3) b$eval(paste0("binding.renderValue(", payload, "); binding.resize(360, 320);"))
  expect_gt(b$eval("deleted"), 0)
  expect_equal(b$eval("handles"), 3)
  expect_equal(b$eval("document.querySelectorAll('#first-scene canvas.gg3d-surface').length"), 1)
  b$eval("sent = []; document.getElementById('first-scene').ggplot3d.select(['Mazda RX4']); document.getElementById('first-scene').ggplot3d.turn({azimuth: 5});")
  expect_equal(b$eval("sent.filter(x => x.id === 'first-scene_selected').length"), 1)
  expect_equal(b$eval("sent.filter(x => x.id === 'first-scene_camera').length"), 1)
  expect_equal(b$eval("sent.filter(x => x.id.startsWith('second-')).length"), 0)
  expect_identical(b$eval("sent.find(x => x.id === 'first-scene_selected').value[0]"), "Mazda RX4")
})


