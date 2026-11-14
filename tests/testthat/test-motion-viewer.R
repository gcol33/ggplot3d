viewer_cars <- function(coord = coord_3d()) {
  cars <- data.frame(mtcars, model = rownames(mtcars), row.names = NULL)
  ggplot2::ggplot(cars, ggplot2::aes(wt, mpg, z = hp, tooltip = model, data_id = model)) +
    geom_point_3d(size = 4) + coord
}

test_that("the browser picks primitives by id and maps them to their rows", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("svglite")
  cars <- data.frame(mtcars, model = rownames(mtcars), row.names = NULL)
  p <- viewer_cars() + ggplot2::facet_wrap(~am)
  b <- viewer_browser(list(view_3d(p, width = 640, height = 360, elementId = "pick")))
  on.exit(b$close(), add = TRUE)
  expect_identical(b$eval("document.getElementById('pick').dataset.renderer"), "webgl2")
  hits <- viewer_hits(b, "pick")
  expect_gt(length(hits), 0L)
  panels <- vapply(hits, `[[`, integer(1), "panel")
  expect_setequal(panels, 1:2)
  field <- function(name, type) vapply(hits, `[[`, type, name)
  rows <- field("row", integer(1))
  expect_true(all(field("layer", integer(1)) == 1L))
  expect_identical(field("key", character(1)), cars$model[rows])
  expect_identical(field("tooltip", character(1)), cars$model[rows])
  expect_identical(cars$am[rows], panels - 1)
  rows <- unique(rows)
  expect_gt(length(rows), 20L)
  expect_null(b$eval("document.getElementById('pick').ggplot3d.pick(2, 2, 0)"))
  expect_match(b$eval("document.querySelector('#pick .gg3d-chrome svg').outerHTML"), "^<svg")
})

test_that("primitive ids match the scene packet's primitive_id", {
  skip_if_not_installed("htmlwidgets")
  d <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0.5, 0.5, 0.5, 0.5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_polygon_3d(fill = "steelblue") +
    geom_text_3d(data = data.frame(x = 0.5, y = 0.5, z = 0.9), ggplot2::aes(label = "label"), size = 8) + coord_3d()
  w <- view_3d(p, panel = 1, width = 400, height = 400, elementId = "ids")
  b <- viewer_browser(list(w))
  on.exit(b$close(), add = TRUE)
  hits <- viewer_hits(b, "ids", step = 4)
  scene <- w$x$panels[[1L]]$scene
  ids <- vapply(hits, `[[`, integer(1), "primitive_id")
  expect_setequal(unique(ids), c(decode_column(scene$faces$primitive_id), decode_column(scene$text$primitive_id)))
  layers <- vapply(hits, `[[`, integer(1), "layer")
  expect_setequal(unique(layers), 1:2)
})

test_that("clicks select keys, dim the rest and report through events", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(view_3d(viewer_cars(), panel = 1, width = 400, height = 400, elementId = "sel")))
  on.exit(b$close(), add = TRUE)
  b$eval(paste0("window.events = []; document.getElementById('sel').addEventListener('ggplot3d:select',",
    " function(e) { window.events.push(e.detail.keys); });"))
  hits <- viewer_hits(b, "sel", step = 4)
  keys <- vapply(hits, `[[`, character(1), "key")
  centre <- function(key) {
    own <- hits[keys == key]
    x <- vapply(own, `[[`, numeric(1), "x"); y <- vapply(own, `[[`, numeric(1), "y")
    own[[which.min((x - mean(x))^2 + (y - mean(y))^2)]]
  }
  hx <- vapply(hits, `[[`, numeric(1), "x"); hy <- vapply(hits, `[[`, numeric(1), "y")
  isolation <- vapply(unique(keys), function(key) {
    h <- centre(key)
    min((hx[keys != key] - h$x)^2 + (hy[keys != key] - h$y)^2)
  }, numeric(1))
  ranked <- names(sort(isolation, decreasing = TRUE))
  first <- centre(ranked[1L])
  second <- centre(ranked[2L])
  viewer_click(b, "sel", first$x, first$y)
  expect_identical(unlist(b$eval("document.getElementById('sel').ggplot3d.getSelection()")), first$key)
  viewer_click(b, "sel", second$x, second$y, modifiers = 8L)
  expect_setequal(unlist(b$eval("document.getElementById('sel').ggplot3d.getSelection()")), c(first$key, second$key))
  viewer_click(b, "sel", second$x, second$y, modifiers = 8L)
  expect_identical(unlist(b$eval("document.getElementById('sel').ggplot3d.getSelection()")), first$key)
  viewer_click(b, "sel", 2, 2)
  expect_length(b$eval("document.getElementById('sel').ggplot3d.getSelection()"), 0L)
  events <- b$eval("window.events")
  expect_length(events, 4L)
  expect_identical(unlist(events[[1L]]), first$key)
  b$eval(sprintf("document.getElementById('sel').ggplot3d.select(['%s'])", first$key))
  pixel <- function(h) unlist(b$eval(sprintf(paste0("(function() { var c = document.querySelector('#sel canvas.gg3d-surface');",
    "var g = c.getContext('webgl2'), r = c.width / c.clientWidth, out = new Uint8Array(4);",
    "g.readPixels(Math.round(%f * r), Math.round((c.clientHeight - %f) * r), 1, 1, g.RGBA, g.UNSIGNED_BYTE, out);",
    "return Array.from(out); })()"), h$x, h$y)))
  b$eval("document.getElementById('sel').ggplot3d.redraw()")
  expect_lt(sum(pixel(first)[1:3]), 60)
  expect_gt(sum(pixel(second)[1:3]), 400)
})

test_that("hover shows the tooltip of the primitive under the pointer", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(view_3d(viewer_cars(), panel = 1, width = 400, height = 400, elementId = "tip")))
  on.exit(b$close(), add = TRUE)
  h <- viewer_hits(b, "tip", step = 4)[[1L]]
  box <- b$eval("(function() { var r = document.querySelector('#tip canvas').getBoundingClientRect(); return {left: r.left, top: r.top}; })()")
  b$session$Input$dispatchMouseEvent(type = "mouseMoved", x = box$left + h$x, y = box$top + h$y)
  Sys.sleep(0.3)
  expect_false(b$eval("document.querySelector('#tip .gg3d-tooltip').hidden"))
  expect_identical(b$eval("document.querySelector('#tip .gg3d-tooltip').textContent"), h$tooltip)
  b$session$Input$dispatchMouseEvent(type = "mouseMoved", x = box$left + 2, y = box$top + 2)
  Sys.sleep(0.3)
  expect_true(b$eval("document.querySelector('#tip .gg3d-tooltip').hidden"))
})

test_that("linked cameras turn every panel by the same amount", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("svglite")
  p <- viewer_cars(coord_3d(azimuth = c(-60, 30))) + ggplot2::facet_wrap(~am)
  for (link in c(TRUE, FALSE)) {
    b <- viewer_browser(list(view_3d(p, link = link, width = 640, height = 360, elementId = "link")))
    box <- b$eval("(function() { var r = document.querySelector('#link canvas').getBoundingClientRect(); return {left: r.left, top: r.top}; })()")
    x <- box$left + 120; y <- box$top + 180
    b$session$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1L)
    for (k in 1:5) b$session$Input$dispatchMouseEvent(type = "mouseMoved", x = x + 8 * k, y = y, button = "left", buttons = 1L)
    b$session$Input$dispatchMouseEvent(type = "mouseReleased", x = x + 40, y = y, button = "left", clickCount = 1L)
    Sys.sleep(0.2)
    azimuth <- vapply(1:2, function(i) b$eval(sprintf("document.getElementById('link').ggplot3d.getCamera(%d).azimuth", i)), numeric(1))
    expect_equal(azimuth, if (link) c(-80, 10) else c(-80, 30))
    b$close()
  }
})

test_that("crosstalk shares selections and filters between widgets", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("crosstalk")
  cars <- data.frame(mtcars, model = rownames(mtcars), row.names = NULL)
  shared <- crosstalk::SharedData$new(cars, key = ~model, group = "viewer-test")
  p <- ggplot2::ggplot(shared$origData(), ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d(size = 4) + coord_3d()
  b <- viewer_browser(list(view_3d(p, data = shared, panel = 1, width = 360, height = 360, elementId = "one"),
    view_3d(p, data = shared, panel = 1, width = 360, height = 360, elementId = "two")), width = 800)
  on.exit(b$close(), add = TRUE)
  b$eval("document.getElementById('one').ggplot3d.select(['Valiant', 'Fiat 128'])")
  expect_setequal(unlist(b$eval("document.getElementById('two').ggplot3d.getSelection()")), c("Valiant", "Fiat 128"))
  b$eval("var h = new crosstalk.SelectionHandle('viewer-test'); h.set(['Duster 360']);")
  expect_identical(unlist(b$eval("document.getElementById('one').ggplot3d.getSelection()")), "Duster 360")
  b$eval("var f = new crosstalk.FilterHandle('viewer-test'); f.set(['Valiant', 'Duster 360']);")
  b$eval("document.getElementById('two').ggplot3d.redraw()")
  rows <- unique(vapply(viewer_hits(b, "two"), `[[`, character(1), "key"))
  expect_setequal(rows, c("Valiant", "Duster 360"))
})

test_that("viewers in one link group turn and reset together, and paths play", {
  skip_if_not_installed("htmlwidgets")
  p <- viewer_cars() + facet_views(c("front", "isometric"))
  path <- orbit_3d(360, 2)
  b <- viewer_browser(list(
    view_3d(p, panel = 1, width = 320, height = 320, link = "cars", elementId = "front", path = path, fps = 10),
    view_3d(p, panel = 2, width = 320, height = 320, link = "cars", elementId = "iso"),
    view_3d(p, panel = 2, width = 320, height = 320, elementId = "alone")), width = 1000)
  on.exit(b$close(), add = TRUE)
  azimuth <- function(id) b$eval(sprintf("document.getElementById('%s').ggplot3d.getCamera().azimuth", id))
  expect_identical(b$eval("document.getElementById('front').dataset.link"), "cars")
  b$eval("document.getElementById('front').ggplot3d.turn({azimuth: 10, zoom: 2})")
  expect_equal(c(azimuth("front"), azimuth("iso"), azimuth("alone")), c(-80, -35, -45))
  expect_equal(b$eval("document.getElementById('iso').ggplot3d.getCamera().zoom"),
    2 * view_3d(p, panel = 2)$x$panels[[1L]]$camera$zoom)
  b$eval("document.querySelector('#iso .gg3d-controls button:nth-child(2)').click()")
  expect_equal(c(azimuth("front"), azimuth("iso")), c(-90, -45))
  expect_false(b$eval("document.querySelector('#front .gg3d-controls button').hidden"))
  expect_true(b$eval("document.querySelector('#iso .gg3d-controls button').hidden"))
  frames <- view_3d(p, panel = 1, path = path, fps = 10)$x$panels[[1L]]$path
  b$eval("document.getElementById('front').ggplot3d.setPathFrame(3)")
  expect_equal(azimuth("front"), frames$azimuth[4L])
  b$eval("document.getElementById('front').ggplot3d.play()")
  expect_true(b$eval("document.getElementById('front').ggplot3d.playing()"))
  b$eval("document.getElementById('iso').ggplot3d.turn({azimuth: 5})")
  expect_false(b$eval("document.getElementById('front').ggplot3d.playing()"))
})
