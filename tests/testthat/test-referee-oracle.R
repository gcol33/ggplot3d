source(testthat::test_path("..","fixtures","oracle","reference.R"),local=TRUE)
source(testthat::test_path("..","fixtures","scenes","builders.R"),local=TRUE)

test_that("independent cameras have analytic axes, sphere framing and pole roll", {
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  for(mode in c("orthographic","perspective")) {
    specification <- ggplot3d:::new_camera_3d(projection=mode,azimuth=0,elevation=0)
    camera <- ref_camera(specification,viewport)
    expect_equal(camera$right,c(0,1,0),tolerance=1e-14)
    expect_equal(camera$up,c(0,0,1),tolerance=1e-14)
    expect_equal(camera$forward,c(-1,0,0),tolerance=1e-14)
    expect_equal(ref_project(matrix(c(.5,.5,.5),1,3),camera)[1,1:2],c(x=.5,y=.5))
    expected <- if(mode=="orthographic") 1/sqrt(3) else
      camera$sy*.5/camera$distance
    expect_equal(unname(ref_project(matrix(c(.5,1,.5),1,3),camera)[1,1]-.5),expected/2)
    specification <- ggplot3d:::new_camera_3d(projection=mode,azimuth=-90,elevation=90,roll=90)
    pole <- ref_camera(specification,viewport)
    expect_equal(pole$right,c(0,-1,0),tolerance=1e-14)
    expect_equal(pole$up,c(1,0,0),tolerance=1e-14)
  }
})

test_that("rays recover analytic planes and crossing half squares", {
  f <- ref_fixture("G1_crossing","pole")
  camera <- ref_camera(f$camera,f$viewport)
  xy <- rbind(c(.3,.4),c(.7,.6))
  npc <- ref_project(cbind(xy,.5),camera)[,1:2,drop=FALSE]
  expect_equal(ref_face_depth(npc,ref_quad(.7),camera),rep(camera$distance-.2,2),tolerance=1e-12)
  oracle <- ref_render(f$packet,f$camera,f$viewport,80L)
  square <- abs(oracle$grid[,1]-.5)<.4/sqrt(3) & abs(oracle$grid[,2]-.5)<.4/sqrt(3)
  expect_identical(oracle$visible,ifelse(square,ifelse(oracle$grid[,1]>.5,1L,2L),0L))
  covered <- which(oracle$visible!=0 & !oracle$boundary & abs(oracle$grid[,1]-.5)>.02)
  expect_gt(length(covered),500)
  expect_identical(oracle$visible[covered],ifelse(oracle$grid[covered,1]>.5,1L,2L))
})

test_that("cyclic strips have three analytically specified front primitives", {
  f <- ref_fixture("G2_cycle","pole")
  camera <- ref_camera(f$camera,f$viewport)
  points <- ref_project(cbind(rbind(c(.76,.22),c(.5,.75),c(.24,.22)),.5),camera)[,1:2,drop=FALSE]
  depths <- vapply(seq_len(3L),function(i)
    ref_face_depth(points,ref_primitive_xyz(f$packet,"faces",i),camera),numeric(3))
  expect_identical(apply(depths,1,which.min),c(2L,3L,1L))
})

test_that("resolved geometric tolerance separates nearby noncoplanar faces", {
  packet <- ref_packet(list(ref_quad(.5+1e-13),ref_quad(.5)),layers=c(1L,2L))
  camera <- ref_cameras()$pole; viewport <- ggplot3d:::new_scene_viewport(100,100)
  oracle <- ref_render(packet,camera,viewport,20L)
  expect_identical(unique(oracle$visible[oracle$visible!=0]),1L)
})

test_that("alpha stacks stop at opaque faces and ignore zero opacity", {
  packet <- ref_packet(list(ref_quad(.2),ref_quad(.5),ref_quad(.8)),face_alpha=c(1,.4,0))
  camera <- ref_cameras()$pole; viewport <- ggplot3d:::new_scene_viewport(100,100)
  oracle <- ref_render(packet,camera,viewport,40L)
  centre <- which.min(rowSums((oracle$grid-.5)^2))
  expect_identical(oracle$layers[[centre]],c(2L,1L))
  expect_equal(oracle$alpha[[centre]],c(.4,1))
  weights <- cumprod(c(1,1-head(oracle$alpha[[centre]],-1))) * oracle$alpha[[centre]]
  expect_equal(weights,c(.4,.6))
})

test_that("physical ellipse and stroke coverage match analytic distances", {
  packet <- ref_packet(markers=list(matrix(c(.5,.5,.5),1,3)),marker_radius=5)
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  points <- rbind(c(.5,.5),c(.549,.5),c(.551,.5),c(.54,.54))
  expect_identical(ref_footprint(points,c(.5,.5,1),packet,1L,viewport),c(TRUE,TRUE,FALSE,FALSE))
  segment <- rbind(c(.2,.5,1),c(.8,.5,1))
  expect_identical(ref_segment(rbind(c(.5,.519),c(.5,.521),c(.19,.5)),segment,4,viewport)$covered,
                   c(TRUE,FALSE,FALSE))
  expect_true(ref_segment(matrix(c(.19,.5),1,2),segment,4,viewport,1L)$covered)
})

test_that("scoring detects an intentionally reversed crossing order", {
  f <- ref_fixture("G1_crossing","pole")
  compiled <- ggplot3d:::scene_compile(f$packet,f$camera,f$viewport)
  if(!isTRUE(compiled$capabilities$exact_visibility)) skip("missing capability: exact_visibility")
  expect_equal(ref_score(f$packet,f$camera,f$viewport,compiled,80L)$wrong,0L)
  compiled$draw_order <- rev(compiled$draw_order)
  expect_gt(ref_score(f$packet,f$camera,f$viewport,compiled,80L)$wrong,100)
})

test_that("perspective rays recover the analytic tilted plane depth", {
  viewport <- ggplot3d:::new_scene_viewport(140,90)
  specification <- ggplot3d:::new_camera_3d(projection="perspective",azimuth=-90,
    elevation=90,distance=2,near=.01,far=5)
  camera <- ref_camera(specification,viewport)
  samples <- rbind(c(.47,.48),c(.53,.52))
  plane <- ref_quad(.3+.4*c(.1,.9,.9,.1))
  dx <- (2*samples[,1]-1)/camera$sx
  expect_equal(ref_face_depth(samples,plane,camera),2/(1+.4*dx),tolerance=1e-12)
})

test_that("box and near clipping have analytic half-plane coverage", {
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  camera <- ref_cameras()$pole
  camera$near <- ref_camera(camera,viewport)$distance
  packet <- ref_packet(list(ref_quad(c(.1,.9,.9,.1),x=c(-.2,1.2,1.2,-.2))))
  oracle <- ref_render(packet,camera,viewport,80L)
  inside <- which(oracle$visible!=0 & !oracle$boundary)
  expect_gt(length(inside),100)
  # The retained half has z <= .5, hence x <= .5 under this top camera.
  expect_true(all(oracle$grid[inside,1]<.5))
  model <- ref_clip(ref_primitive_xyz(packet,"faces",1),oracle$camera,camera)
  expect_true(all(model[,1]>=0 & model[,1]<=1))
  expect_true(all(ref_project(model,oracle$camera)[,3]>=camera$near-1e-12))
})

test_that("polygon footprints apply even-odd holes independently of ring orientation", {
  packet <- ref_packet(markers=list(matrix(c(.5,.5,.5),1,3)))
  packet$footprints <- list(kind=2L,offset=1L,count=8L,radius_x_mm=0,
    radius_y_mm=0,angle=0)
  packet$footprint_vertices <- list(x_mm=c(-10,10,10,-10,-4,4,4,-4),
    y_mm=c(-10,-10,10,10,-4,-4,4,4),ring=rep(1:2,each=4))
  viewport <- ggplot3d:::new_scene_viewport(100,100)
  expect_identical(ref_footprint(rbind(c(.5,.5),c(.57,.5),c(.62,.5)),c(.5,.5,1),
    packet,1L,viewport),c(FALSE,TRUE,FALSE))
})

test_that("the scorer rejects wrong paint order without a production renderer", {
  packet <- ref_packet(list(ref_quad(.3),ref_quad(.7)))
  camera <- ref_cameras()$pole; viewport <- ggplot3d:::new_scene_viewport(100,100)
  projected <- ref_project(cbind(packet$vertices$x,packet$vertices$y,packet$vertices$z),
    ref_camera(camera,viewport))
  compiled <- list(vertices=list(x_npc=projected[,1],y_npc=projected[,2],depth=projected[,3]),
    fragments=list(offset=c(1L,5L),count=c(4L,4L),kind=c(1L,1L),mask_count=c(0L,0L),
      parent_primitive_id=c(1L,2L),opacity=c(1,1)),draw_order=c(1L,2L),viewport=viewport)
  expect_equal(ref_score(packet,camera,viewport,compiled,60L)$wrong,0L)
  compiled$draw_order <- c(2L,1L)
  expect_gt(ref_score(packet,camera,viewport,compiled,60L)$wrong,500)
})
