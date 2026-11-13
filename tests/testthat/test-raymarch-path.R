source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local=TRUE)
pathtrace_test_scene <- function(backend, packet=NULL, specs=list(), camera=NULL, light=NULL) {
  if (is.null(packet)) {packet <- ref_packet(faces=list(ref_quad(.3)));packet$styles$fill <- "white"}
  if (is.null(camera)) camera <- ggplot3d:::new_camera_3d(azimuth=-90,elevation=90)
  ggplot3d:::pathtrace_scene_3d(packet,specs,camera,ggplot3d:::new_scene_viewport(25.4,25.4,9),backend,
    light %||% light_3d(ambient=1,diffuse=0))
}

test_that("diffuse white under a unit environment conserves radiance", {
  result <- pathtrace_test_scene(backend_3d("pathtrace",samples=8,bounces=1))
  expect_equal(result$sum[41,],rep(8,4),tolerance=1e-12)
  expect_true(all(result$sum[,1:3] <= result$sum[,4]+1e-12))
})

test_that("path tracing is seeded, progressive and thread independent", {
  old <- options(ggplot3d.threads=1L);on.exit(options(old))
  set.seed(492);before <- .Random.seed
  full <- pathtrace_test_scene(backend_3d("pathtrace",samples=12,batch_size=12,seed=19))
  expect_identical(.Random.seed,before)
  split <- pathtrace_test_scene(backend_3d("pathtrace",samples=12,batch_size=3,seed=19))
  expect_identical(full$sum,split$sum)
  options(ggplot3d.threads=2L)
  parallel <- pathtrace_test_scene(backend_3d("pathtrace",samples=12,batch_size=4,seed=19))
  expect_identical(full$sum,parallel$sum)
  progressive <- backend_3d("pathtrace",samples=4,batch_size=3,seed=19,progressive=TRUE)
  first <- pathtrace_test_scene(progressive)
  pathtrace_test_scene(progressive)
  third <- pathtrace_test_scene(progressive)
  expect_identical(third$sum,full$sum)
  expect_equal(c(first$samples,third$samples),c(4L,12L))
  changed <- pathtrace_test_scene(progressive,camera=ggplot3d:::new_camera_3d(azimuth=20))
  expect_equal(changed$samples,4L)
  other <- pathtrace_test_scene(backend_3d("pathtrace",samples=12,seed=20))
  expect_false(identical(full$sum,other$sum))
})

test_that("the path tracer intersects compiled SDFs and the thin lens", {
  spec <- list(kind="sdf",tape=ggplot3d:::sdf_compile_3d(sdf_sphere(c(.5,.5,.5),.25)),
    ranges=list(x=c(0,1),y=c(0,1),z=c(0,1)),fill="red",tolerance=1e-6,max_steps=512L)
  camera <- ggplot3d:::new_camera_3d(projection="perspective",azimuth=-90,elevation=30)
  backend <- backend_3d("pathtrace",samples=8,bounces=2,seed=5)
  pinhole <- pathtrace_test_scene(backend,new_scene_packet(),list(spec),camera)
  expect_gt(sum(pinhole$sum[,4]),0)
  expect_gt(sum(pinhole$sum[,1]),0)
  expect_equal(sum(pinhole$sum[,2:3]),0)
  lens <- backend_3d("pathtrace",samples=8,bounces=2,seed=5,aperture=.15,focus=1)
  blurred <- pathtrace_test_scene(lens,new_scene_packet(),list(spec),camera)
  expect_false(identical(pinhole$sum,blurred$sum))
})

test_that("pathtrace composes a panel with vector furniture and text", {
  d <- data.frame(x=c(0,1),y=c(0,1),z=c(0,1))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_point_3d(size=3,colour="steelblue")+
    coord_3d(backend=backend_3d("pathtrace",dpi=15,samples=2,bounces=2))
  path <- tempfile(fileext=".png")
  grDevices::png(path,width=100,height=100)
  expect_no_error(print(p))
  grid::grid.force()
  expect_equal(grid::grid.get("scene-pathtrace-3d")$raster_result$samples,2L)
  grDevices::dev.off();unlink(path)
  expect_error(backend_3d("pathtrace",samples=0),"positive")
  expect_error(backend_3d("pathtrace",seed=-1),"nonnegative")
  expect_error(backend_3d("pathtrace",aperture=-1),"non-negative")
})

test_that("diffuse paths transfer colour between surfaces", {
  floor <- ref_quad(.2)
  wall <- rbind(c(.25,.1,.2),c(.25,.9,.2),c(.25,.9,.9),c(.25,.1,.9))
  packet <- ref_packet(faces=list(floor,wall))
  packet$styles <- data.frame(style_id=1:2,fill=c("white","red"))
  packet$style_count <- 2L;packet$faces$style_id <- 1:2
  direct <- pathtrace_test_scene(backend_3d("pathtrace",samples=96,bounces=1,seed=34),packet)
  indirect <- pathtrace_test_scene(backend_3d("pathtrace",samples=96,bounces=3,seed=34),packet)
  expect_equal(direct$sum[41,1],direct$sum[41,2])
  expect_gt(indirect$sum[41,1],indirect$sum[41,2]+1)
})
