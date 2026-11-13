test_that("registered arithmetic compiles all SDF operations and exact gradients", {
  shapes <- list(sdf_sphere(), sdf_box(radius=.1), sdf_torus(), sdf_capsule(), sdf_plane(),
    sdf_gyroid(), sdf_metaballs(rbind(c(0,0,0),c(1,0,0))), sdf_sphere() | sdf_box(),
    sdf_sphere() & sdf_box(), sdf_box() - sdf_sphere(), -sdf_sphere(),
    sdf_blend(sdf_sphere(), sdf_box()), sdf_blend(sdf_sphere(), sdf_box(), mode="intersection"),
    sdf_blend(sdf_sphere(), sdf_box(), mode="difference"), sdf_offset(sdf_sphere(), .1),
    sdf_shell(sdf_sphere(), .1), sdf_transform(sdf_box(), translate=c(.2,.3,.4), rotate=c(20,30,40), scale=2))
  p <- rbind(c(.23,.47,1.63),c(1.12,.63,.37),c(-.64,.23,-.87))
  for (s in shapes) {
    got <- ggplot3d:::raymarch_eval_cpp(ggplot3d:::sdf_compile_3d(s),p)
    expect_equal(got[,1],s(p[,1],p[,2],p[,3]),tolerance=1e-12)
    expect_equal(got[,2:4],ggplot3d:::sdf_gradient_3d(s,p),tolerance=1e-10)
  }
})

test_that("compiler uses registered rules and rejects unsupported arithmetic", {
  registry <- ggplot3d:::sdf_ops_3d
  on.exit(rm(list=c("compiled_test", "unsupported_test"), envir=registry))
  ggplot3d:::sdf_register_3d("compiled_test",function(params,p,child) sqrt(p[[1]]^2+p[[2]]^2+p[[3]]^2)-2,
    lipschitz=function(params,L) 1)
  s <- ggplot3d:::new_sdf_3d("compiled_test")
  expect_equal(ggplot3d:::raymarch_eval_cpp(ggplot3d:::sdf_compile_3d(s),matrix(c(3,0,0),1))[,1],1)
  ggplot3d:::sdf_register_3d("unsupported_test",function(params,p,child) if(p[[1]]>0) p[[1]] else 0,
    lipschitz=function(params,L) 1)
  expect_error(ggplot3d:::sdf_compile_3d(ggplot3d:::new_sdf_3d("unsupported_test")),"unsupported_test.*compiled implementation")
})

test_that("sphere tracing agrees with analytic intersections and thread counts", {
  tape <- ggplot3d:::sdf_compile_3d(sdf_sphere())
  origin <- cbind(seq(-.8,.8,length.out=600),0,3)
  direction <- matrix(rep(c(0,0,-1),600),ncol=3,byrow=TRUE)
  trace <- function(threads) ggplot3d:::raymarch_trace_cpp(tape,origin,direction,rep(c(-2,2),3),1e-8,512L,threads)
  hit <- trace(1L)
  expect_equal(hit$distance,3-sqrt(1-origin[,1]^2),tolerance=3e-8)
  expect_equal(hit$normal,cbind(origin[,1],0,sqrt(1-origin[,1]^2)),tolerance=3e-8)
  expect_identical(hit,trace(2L))
  inside <- ggplot3d:::raymarch_trace_cpp(tape,matrix(c(0,0,0),1),matrix(c(1,0,0),1),rep(c(-2,2),3),1e-8,512L,1L)
  expect_equal(inside$distance,1)
})

test_that("nonunit Lipschitz constants protect steps", {
  s <- sdf_metaballs(matrix(c(0,0,0),1),radius=.03)
  tape <- ggplot3d:::sdf_compile_3d(s)
  expect_gt(tape$lipschitz,1)
  h <- ggplot3d:::raymarch_trace_cpp(tape,matrix(c(0,0,.2),1),matrix(c(0,0,-1),1),rep(c(-.3,.3),3),1e-7,2048L,1L)
  expect_equal(h$distance,.17,tolerance=1e-6)
})

test_that("ray layers render and retain raster depth buffer layout", {
  p <- ggplot2::ggplot() + geom_isofunction_3d(fun=sdf_sphere(radius=.7),render="trace",fill="steelblue") +
    ggplot2::xlim(-1,1) + ggplot2::ylim(-1,1) + scale_z_continuous(limits=c(-1,1)) +
    coord_3d(backend=backend_3d("raster",dpi=25,antialias=1))
  path <- tempfile(fileext=".png")
  grDevices::png(path,width=120,height=120)
  on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
  expect_no_error(print(p))
  grid::grid.force()
  tree <- grid::grid.get("scene-raymarch-3d")
  expect_true(any(is.finite(tree$raster_result$buffers$depth)))
  expect_named(tree$raster_result$buffers,c("depth","opaque_depth","kind","row","normal"))
})

source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local=TRUE)
test_that("SDF and face depth use the same camera coordinate", {
  ranges <- list(x=c(0,1),y=c(0,1),z=c(0,1))
  spec <- list(tape=ggplot3d:::sdf_compile_3d(sdf_sphere(c(.5,.5,.5),.2)),
    ranges=ranges,fill="blue",tolerance=1e-8,max_steps=512L)
  viewport <- ggplot3d:::new_scene_viewport(25.4,25.4,31)
  for (projection in c("orthographic","perspective","oblique")) {
    camera <- ggplot3d:::new_camera_3d(projection=projection,azimuth=-90,elevation=90)
    draw <- function(z) {
      packet <- ref_packet(faces=list(ref_quad(z)))
      packet$styles$fill <- "red"
      ggplot3d:::raymarch_scene_3d(list(spec),packet,camera,viewport,backend_3d("raster",antialias=1L),NULL)
    }
    front <- draw(.9); behind <- draw(.1)
    expect_lt(front$buffers$depth[16,16],behind$buffers$depth[16,16])
    expect_equal(behind$buffers$normal$z[16,16],1,tolerance=1e-6)
    expect_equal(behind$image[16+31*15],as.integer(-65536))
    expect_equal(front$image[16+31*15],as.integer(-16776961))
  }
})
