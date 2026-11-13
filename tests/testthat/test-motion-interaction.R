cars_3d <- function() data.frame(mtcars, model = rownames(mtcars), row.names = NULL)

test_that("tooltip and data_id map without warnings, groups or scales", {
  d <- data.frame(x = 1:6, y = c(1, 2, 1, 2, 1, 2), z = 1:6, lab = letters[1:6])
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_path_3d(ggplot2::aes(tooltip = lab, data_id = lab)) + coord_3d()
  expect_no_warning(built <- ggplot2::ggplot_build(p))
  data <- built$data[[1L]]
  expect_identical(data$tooltip, d$lab)
  expect_identical(data$data_id, d$lab)
  expect_identical(unique(data$group), -1L)
  expect_null(built$plot$scales$get_scales("tooltip"))
  expect_identical(names(built$plot$layers[[1L]]$computed_mapping), c("tooltip", "data_id", "x", "y", "z"))
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("each panel carries the source rows, keys and tooltips of its primitives", {
  skip_if_not_installed("htmlwidgets")
  cars <- cars_3d()
  p <- ggplot2::ggplot(cars, ggplot2::aes(wt, mpg, z = hp, tooltip = model, data_id = model)) +
    geom_point_3d() + coord_3d() + ggplot2::facet_wrap(~am)
  w <- view_3d(p, panel = 2)
  layer <- w$x$panels[[1L]]$layers[[1L]]
  rows <- as.integer(layer$rows)
  expect_identical(sort(rows), which(cars$am == 1))
  expect_identical(as.character(layer$key), cars$model[rows])
  expect_identical(as.character(layer$tooltip), cars$model[rows])
  expect_false(layer$crosstalk)
  scene <- w$x$panels[[1L]]$scene
  source <- decode_column(scene$markers$source_row)
  expect_setequal(source, rows)
  expect_true(all(decode_column(scene$markers$layer_id) == 1L))
})

test_that("default tooltips list mapped values by their labels", {
  skip_if_not_installed("htmlwidgets")
  cars <- cars_3d()
  p <- ggplot2::ggplot(cars, ggplot2::aes(wt, mpg, z = hp, colour = factor(cyl))) +
    geom_point_3d() + coord_3d() + ggplot2::labs(z = "Power")
  layer <- view_3d(p, panel = 1)$x$panels[[1L]]$layers[[1L]]
  row <- as.integer(layer$rows)[1L]
  expect_identical(layer$tooltip[[1L]], sprintf("wt: %s\nmpg: %s\nPower: %s\nfactor(cyl): %s",
    signif(cars$wt[row], 4), signif(cars$mpg[row], 4), cars$hp[row], cars$cyl[row]))
  expect_identical(layer$key[[1L]], paste0("1:", row))
  expect_null(view_3d(p, panel = 1, tooltip = FALSE)$x$panels[[1L]]$layers[[1L]]$tooltip)
})

test_that("tooltips of computed layers read computed values and positions", {
  skip_if_not_installed("htmlwidgets")
  set.seed(1)
  d <- data.frame(x = rnorm(60), y = rnorm(60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_density_2d_3d(ggplot2::aes(fill = ggplot2::after_stat(density))) + coord_3d()
  built <- ggplot2::ggplot_build(p)
  w <- view_3d(p, panel = 1)
  layer <- w$x$panels[[1L]]$layers[[1L]]
  expect_gt(length(layer$tooltip), 0L)
  expect_true(all(grepl("^x: .*\ny: .*\nz: .*\ndensity: ", layer$tooltip)))
  data <- built$data[[1L]]
  expect_null(data$.source)
  first <- data$PANEL == 1L
  expect_identical(layer$tooltip[[1L]], sprintf("x: %s\ny: %s\nz: %s\ndensity: %s",
    signif(data$x[first][1L], 4), signif(data$y[first][1L], 4), signif(data$z[first][1L], 4),
    signif(data$density[first][1L], 4)))
})

test_that("SharedData keys link the layers that draw its rows", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("crosstalk")
  cars <- cars_3d()
  shared <- crosstalk::SharedData$new(cars, key = ~model, group = "cars-test")
  p <- ggplot2::ggplot(shared$origData(), ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + geom_point_3d(data = data.frame(wt = 3, mpg = 20, hp = 200), colour = "red") + coord_3d()
  w <- view_3d(p, data = shared, panel = 1)
  layers <- w$x$panels[[1L]]$layers
  expect_true(layers[[1L]]$crosstalk)
  expect_false(layers[[2L]]$crosstalk)
  expect_identical(as.character(layers[[1L]]$key), cars$model[as.integer(layers[[1L]]$rows)])
  expect_identical(w$x$selection$group, "cars-test")
  expect_true(any(vapply(w$dependencies, function(d) d$name == "crosstalk", logical(1))))
  other <- crosstalk::SharedData$new(iris)
  expect_error(view_3d(p, data = other), "No layer")
})
