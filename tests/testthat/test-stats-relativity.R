test_that("Lorentz boosts preserve the metric and rapidity velocity relation", {
  for(direction in list(c(1,0),c(1,2,3))) {
    B <- lorentz_boost_3d(.7,direction); p <- length(direction)
    J <- diag(c(rep(1,p),-1))
    expect_equal(t(B)%*%J%*%B,J,tolerance=1e-12)
    expect_equal(lorentz_boost_3d(-.7,direction)%*%B,diag(p+1),tolerance=1e-12)
    expect_equal(sqrt(sum(B[seq_len(p),p+1]^2))/B[p+1,p+1],tanh(.7),tolerance=1e-12)
  }
})

test_that("light cones and hyperboloids recover the signed interval", {
  for(interval in c(-1,0,1)) {
    p <- ggplot2::ggplot()+stat_light_cone_3d(apex=c(.4,-.2,.3),interval=interval,speed=2,n=c(12,8)) + coord_3d()
    d <- ggplot2::layer_data(p)
    expect_equal(4*(d$z-.3)^2-(d$x-.4)^2-(d$y+.2)^2,rep(interval,nrow(d)),tolerance=1e-11)
    expect_silent(validate_scene_packet(stats_scene(p)$packet))
  }
  p <- ggplot2::ggplot(data.frame(x=c(0,2),y=c(0,1),t=c(0,3)),ggplot2::aes(x,y,t=t))+
    stat_light_cone_3d(n=8)+coord_3d()
  expect_equal(sort(unique(ggplot2::layer_data(p)$event)),1:2)
  p <- ggplot2::ggplot()+stat_light_cone_3d(interval=1,sheet="future",boost=lorentz_boost_3d(.5,c(1,0)),n=10)+coord_3d()
  d <- ggplot2::layer_data(p)
  expect_equal(d$z^2-d$x^2-d$y^2,rep(1,nrow(d)),tolerance=1e-11)
  expect_equal(d$klein_x,d$x/d$z,tolerance=1e-12)
  expect_equal(d$klein_y,d$y/d$z,tolerance=1e-12)
  expect_true(all(d$klein_x^2+d$klein_y^2<1))
})

test_that("Flamm embedding has the Schwarzschild spatial metric", {
  p <- ggplot2::ggplot()+stat_embedding_3d(rs=2,n=c(16,12))+coord_3d()
  d <- ggplot2::layer_data(p); r <- sqrt(d$x^2+d$y^2)
  expect_equal(d$z^2,8*(r-2),tolerance=1e-10)
  sample <- matrix(c(.4,.8),1)
  derivatives <- parametric_derivatives_3d(function(uv) flamm_surface_3d(uv[,1],uv[,2],2),sample,c(1e-5,1e-5),FALSE)
  expect_equal(sum(derivatives$ru^2),(2+.8^2)^2,tolerance=1e-8)
  expect_equal(sum(derivatives$rv^2),4*.8^2+8,tolerance=1e-8)
  expect_equal(sum(derivatives$ru*derivatives$rv),0,tolerance=1e-8)
  expect_error(stat_embedding_3d(rs=2,r=c(1,4)),"outside")
})

test_that("Schwarzschild orbits recover circular and null solutions", {
  circular <- schwarzschild_orbit_3d(1,sqrt(100/7),FALSE,.1,0,c(0,20*pi),400,c(2,100),FALSE)
  expect_equal(circular$r,rep(10,nrow(circular)),tolerance=1e-10)
  photon <- schwarzschild_orbit_3d(1,4,TRUE,1/3,0,c(0,3*pi),100,c(2,100),FALSE)
  expect_equal(photon$r,rep(3,nrow(photon)),tolerance=1e-10)
  orbit <- function(n) schwarzschild_orbit_3d(1,4,FALSE,.08,0,c(0,8*pi),n,c(2,100),FALSE)
  coarse <- orbit(100); fine <- orbit(200); reference <- orbit(1600)
  a <- abs(tail(coarse$u,1)-tail(reference$u,1)); b <- abs(tail(fine$u,1)-tail(reference$u,1))
  expect_lt(b,a/10)
  expect_lt(diff(range(reference$invariant)),1e-11)
  turning <- sort(Re(polyroot(c(-reference$invariant[1L],-1/8,1,-2))))
  pericentre <- 1/turning[2L]
  expect_equal(max(reference$r),12.5,tolerance=1e-10)
  expect_gte(min(reference$r),pericentre-1e-9)
  expect_lt(min(reference$r)-pericentre,1e-4)
})

test_that("orbit events stop at finite radial boundaries and project onto the slice", {
  d <- schwarzschild_orbit_3d(1,4,TRUE,.1,.2,c(0,10*pi),600,c(2,100),TRUE)
  expect_equal(tail(d$r,1),2,tolerance=1e-10)
  expect_true(all(is.finite(as.matrix(d))))
  expect_equal(d$z^2,8*(d$r-2),tolerance=1e-10)
  p <- ggplot2::ggplot()+stat_geodesic_ode_3d(n=100)+coord_3d()
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
})

test_that("space adapters apply boosts to finite event packets", {
  d <- data.frame(x=c(.2,.4),y=c(.1,-.2),z=c(.8,1.1))
  B <- lorentz_boost_3d(.4,c(1,0))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_point_3d()+
    coord_spacetime_3d(isometry=B,xlim=c(-2,2),ylim=c(-2,2),zlim=c(-2,2),expand=FALSE,axes=character(),panes=character(),box="none")
  packet <- stats_scene(p)$packet
  got <- do.call(cbind,packet$vertices)*4-2
  expected <- as.matrix(d)%*%t(B)
  expect_equal(unname(got),unname(expected),tolerance=1e-12)
})

test_that("isometry animation re-emits canonical geometry at each frame", {
  d <- data.frame(x=c(.2,.4),y=c(.1,-.2),z=c(.8,1.1))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_point_3d(size=3)+
    coord_spacetime_3d(xlim=c(-2,2),ylim=c(-2,2),zlim=c(-2,2),axes=character(),panes=character(),box="none")
  frames <- list(diag(3),lorentz_boost_3d(.5,c(1,0)))
  a <- animate_3d(p,isometry=frames,frames=2,width=120,height=120,progress=FALSE,workers=1)
  on.exit(unlink(a$frames),add=TRUE)
  expect_equal(a$frame_count,2L)
  expect_false(identical(readBin(a$frames[1L],"raw",n=file.info(a$frames[1L])$size),readBin(a$frames[2L],"raw",n=file.info(a$frames[2L])$size)))
  expect_equal(a$camera$azimuth,rep(p@coordinates$camera$azimuth,2L))
})
