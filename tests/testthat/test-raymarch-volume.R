volume_test_field <- function(fun, mode=0L, opacity=1, samples=16L, level=0, axes=rep(list(c(0,1)),3)) {
  names(axes) <- c("x","y","z")
  grid <- expand.grid(axes);grid$value <- fun(grid$x,grid$y,grid$z)
  field <- ggplot3d:::stats_scalar_grid_3d(grid)
  c(field,list(mode=mode,samples=samples,level=level),ggplot3d:::volume_transfer_3d(field,opacity,"white"))
}
volume_test_ray <- function(fields, origin=c(.5,.5,-1), direction=c(0,0,1), limit=Inf, threads=1L) {
  ggplot3d:::raymarch_volume_cpp(fields,matrix(origin,1),matrix(direction,1),limit,threads)
}

test_that("homogeneous volumes integrate optical depth and stop at opaque depth", {
  field <- volume_test_field(function(x,y,z) 1,opacity=2)
  hit <- volume_test_ray(list(field))
  expect_equal(hit$alpha,1-exp(-2),tolerance=1e-12)
  expect_equal(as.vector(hit$colour),rep(1-exp(-2),3),tolerance=1e-12)
  field$samples <- 127L
  expect_equal(volume_test_ray(list(field))$alpha,hit$alpha,tolerance=1e-12)
  expect_equal(volume_test_ray(list(field),limit=1.25)$alpha,1-exp(-.5),tolerance=1e-12)
  expect_equal(volume_test_ray(list(field),limit=.5)$alpha,0)
  expect_equal(volume_test_ray(list(field,field))$alpha,1-exp(-4),tolerance=1e-12)
})

test_that("per-cell cubics find interior maxima and equal-sign isosurface roots", {
  f <- function(x,y,z) (x-.25)*(y-.75)
  field <- volume_test_field(f,mode=2L)
  hit <- volume_test_ray(list(field),origin=c(0,0,.5),direction=c(1,1,0))
  expect_equal(hit$distance,sqrt(2)*.25,tolerance=1e-12)
  expect_equal(as.vector(hit$normal),c(-1,0,0),tolerance=1e-10)
  maximum <- volume_test_field(function(x,y,z) x*(1-y),mode=1L)
  hit <- volume_test_ray(list(maximum),origin=c(0,0,.5),direction=c(1,1,0))
  expect_equal(hit$value,.25,tolerance=1e-12)
  expect_equal(hit$distance,sqrt(2)/2,tolerance=1e-12)
  tangent <- volume_test_field(function(x,y,z) (x-.5)*(y-.5),mode=2L)
  expect_equal(volume_test_ray(list(tangent),origin=c(0,0,.5),direction=c(1,1,0))$distance,sqrt(2)/2,tolerance=1e-10)
})

test_that("nonuniform grids and shuffled input retain coordinate keys", {
  d <- expand.grid(x=c(0,.2,1),y=c(0,.7,1),z=c(0,.1,1))
  d$value <- with(d,x+2*y+3*z)
  a <- ggplot3d:::stats_scalar_grid_3d(d)
  b <- ggplot3d:::stats_scalar_grid_3d(d[rev(seq_len(nrow(d))),])
  expect_identical(a,b)
  field <- c(a,list(mode=2L,samples=4L,level=3),ggplot3d:::volume_transfer_3d(a,1,"white"))
  hit <- volume_test_ray(list(field))
  expect_equal(hit$distance,1.5,tolerance=1e-12)
  expect_equal(as.vector(hit$normal),c(1,2,3)/sqrt(14),tolerance=1e-12)
  d$x[2] <- d$x[1]
  expect_error(ggplot3d:::stats_scalar_grid_3d(d),"unique")
})

test_that("volume integration is deterministic across worker counts", {
  field <- volume_test_field(function(x,y,z) x+y+z,opacity=c(0,2))
  origin <- cbind(rep(.5,300),rep(.5,300),rep(-1,300))
  direction <- cbind(rep(0,300),rep(0,300),rep(1,300))
  run <- function(threads) ggplot3d:::raymarch_volume_cpp(list(field),origin,direction,rep(3,300),threads)
  expect_identical(run(1L),run(2L))
})

test_that("volume layers draw all three modes on small images", {
  d <- expand.grid(x=seq(-1,1,length.out=4),y=seq(-1,1,length.out=4),z=seq(-1,1,length.out=4))
  d$v <- with(d,x^2+y^2+z^2)
  for (mode in c("absorption","mip","iso")) {
    p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,value=v)) + geom_volume_3d(mode=mode,level=.8,samples=8) +
      coord_3d(backend=backend_3d("raster",dpi=20,antialias=1))
    path <- tempfile(fileext=".png")
    grDevices::png(path,width=100,height=100)
    expect_no_error(print(p))
    grDevices::dev.off();unlink(path)
  }
  expect_error(geom_volume_3d(samples=0),"positive integer")
  field <- volume_test_field(function(x,y,z) x)
  expect_error(ggplot3d:::volume_transfer_3d(field,-1,"white"),"nonnegative")
})
