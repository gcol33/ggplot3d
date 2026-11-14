intersection_fixture_3d <- function() {
  v <- data.frame(x=c(-1,1,0,0,0,0),y=c(-1,-1,1,-1,1,0),z=c(0,0,0,-1,-1,1))
  mesh_3d(v,matrix(1:6,2,byrow=TRUE))
}

test_that("triangle crossings recover the analytic overlap of plane intervals", {
  m <- intersection_fixture_3d()
  d <- mesh_intersection_curves_3d(m,1e-10)
  expect_equal(nrow(d),2L)
  expect_equal(d$x,c(0,0),tolerance=1e-12)
  expect_equal(d$z,c(0,0),tolerance=1e-12)
  expect_equal(sort(d$y),c(-.5,.5),tolerance=1e-12)
  expect_equal(sort(unique(c(d$face1,d$face2))),1:2)
  rot <- matrix(c(0,1,0,-1,0,0,0,0,1),3)
  xyz <- as.matrix(m[c("x","y","z")]) %*% rot * 3
  xyz <- sweep(xyz,2,c(2,3,4),"+")
  m[c("x","y","z")] <- xyz
  got <- mesh_intersection_curves_3d(m,1e-10)
  expect_equal(sort(got$x),c(.5,3.5),tolerance=1e-10)
  expect_equal(got$y,c(3,3),tolerance=1e-10)
  expect_equal(got$z,c(4,4),tolerance=1e-10)
})

test_that("adjacency coplanarity separation and isolated contact make no curves", {
  m <- mesh_3d(data.frame(x=c(0,1,1,0),y=c(0,0,1,1),z=0),rbind(c(1,2,3),c(1,3,4)))
  expect_equal(nrow(mesh_intersection_curves_3d(m,1e-9)),0L)
  m <- intersection_fixture_3d(); m$z[4:6] <- 1
  expect_equal(nrow(mesh_intersection_curves_3d(m,1e-9)),0L)
  m$z <- 0
  expect_equal(nrow(mesh_intersection_curves_3d(m,1e-9)),0L)
  m <- intersection_fixture_3d(); m$y[4:6] <- m$y[4:6]+1.5
  expect_equal(nrow(mesh_intersection_curves_3d(m,1e-9)),0L)
  expect_error(stat_self_intersection_3d(tolerance=0),"positive")
})

test_that("crossings stitch and independent mesh keys remain independent", {
  d <- data.frame(x=c(0,1,1,2),y=0,z=0,face1=1L,face2=2L,group=c(1,1,2,2))
  out <- intersection_paths_3d(d,1e-9)
  expect_equal(nrow(out),3L); expect_equal(length(unique(out$group)),1L)
  expect_equal(sort(out$x),0:2)
  m <- intersection_fixture_3d(); m$.mesh <- rep(1:2,each=3); m$.vertex <- rep(1:3,2); m$.face <- 1L
  expect_equal(nrow(mesh_intersection_curves_3d(m,1e-9)),2L)
  p <- ggplot2::ggplot(m)+stat_self_intersection_3d(colour="red")+coord_3d()
  packet <- stats_scene(p)$packet
  expect_equal(length(packet$polylines$offset),1L)
  expect_silent(validate_scene_packet(packet))
})

test_that("torus knots close and tube topology is a torus", {
  d <- knot_curve_3d(2,3,80,2,.7)
  expect_equal(sqrt(d$x^2+d$y^2),2+.7*cos(3*d$t),tolerance=1e-12)
  expect_equal(d$z,.7*sin(3*d$t),tolerance=1e-12)
  expect_error(stat_knot_3d(p=2,q=4),"coprime")
  p <- ggplot2::ggplot()+stat_knot_3d(n=40,radius=.12,sides=6)+coord_3d()
  d <- ggplot2::layer_data(p)
  f <- matrix(d$.vertex,ncol=3,byrow=TRUE)
  e <- rbind(f[,1:2],f[,2:3],f[,c(3,1)])
  counts <- table(paste(pmin(e[,1],e[,2]),pmax(e[,1],e[,2])))
  expect_true(all(counts==2))
  expect_equal(length(unique(d$.vertex))-length(counts)+nrow(f),0)
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
})

test_that("immersed showpieces compile within the resource budget and pass the visibility referee", {
  source(testthat::test_path("..","fixtures","oracle","reference.R"),local=TRUE)
  source(testthat::test_path("..","fixtures","scenes","immersed.R"),local=TRUE)
  for (name in c(names(immersed_recipes_3d),"trefoil")) {
    p <- immersed_showpiece_plot(name)
    packet <- stats_scene(p)$packet
    camera <- new_camera_3d(azimuth=25,elevation=20)
    viewport <- new_scene_viewport(90,90)
    attr(viewport,"engine_budget_bytes") <- 32*1024^2
    compiled <- ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],camera,viewport)
    expect_true(compiled$capabilities$exact_visibility,info=name)
    expect_true(length(compiled$fragments$offset)>0,info=name)
    score <- ref_score(packet,camera,viewport,compiled,n=24L)
    expect_equal(score$wrong,0L,info=name)
  }
})

test_that("welded double curves remain visible as intersections", {
  d <- ggplot2::layer_data(ggplot2::ggplot()+stat_immersed_3d(n=c(24,16))+coord_3d())
  curves <- mesh_intersection_curves_3d(d,1e-9)
  expect_gt(nrow(curves),10L)
  expect_equal(curves$z,rep(0,nrow(curves)),tolerance=1e-8)
  expect_equal(sqrt(curves$x^2+curves$y^2),rep(2,nrow(curves)),tolerance=1e-8)
})