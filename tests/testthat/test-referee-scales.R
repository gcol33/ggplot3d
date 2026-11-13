test_that("H log segment transforms both endpoints through the public build", {
  data <- data.frame(x=1,y=1,z=10,xend=2,yend=2,zend=100)
  plot <- ggplot2::ggplot(data,ggplot2::aes(x,y,z=z,xend=xend,yend=yend,zend=zend)) +
    geom_segment_3d() + scale_z_continuous(transform="log10") + coord_3d()
  built <- ggplot2::ggplot_build(plot)
  expect_equal(built$data[[1]]$z,1)
  expect_equal(built$data[[1]]$zend,2)
  expect_equal(built$layout$get_scales(1)$z$range$range,c(1,2))
})

test_that("H reversed discrete limits control built values and guide positions", {
  data <- data.frame(x=1:4,y=1,z=factor(c("low","high","low","high"),levels=c("low","high")))
  plot <- ggplot2::ggplot(data,ggplot2::aes(x,y,z=z)) + geom_point_3d() +
    scale_z_discrete(limits=c("high","low")) + coord_3d()
  built <- ggplot2::ggplot_build(plot)
  expect_equal(as.numeric(built$data[[1]]$z),c(2,1,2,1))
  expect_equal(as.character(built$layout$panel_params[[1]]$z$get_labels()),c("high","low"))
  expect_equal(as.numeric(built$layout$panel_params[[1]]$z$mapped_breaks()),c(1,2))
})
