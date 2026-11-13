thurston_types <- c("e3", "s3", "h3", "s2xe", "h2xe", "nil", "sol", "sl2r")
thurston_unit <- function(x) x / sqrt(sum(x^2))
thurston_flow <- function(g, p, u, t) ggplot3d:::raymarch_geodesic_cpp(g, p, u, t, FALSE)
thurston_field <- function(center, shape=0L, radius=.2) list(kind="thurston", shape=shape,
  center=center, direction=c(0,0,1), radius=radius, rgb=c(1,0,0), fill="red")

test_that("geodesics preserve speed, composition and parallel transport in all eight spaces", {
  p <- c(.1,-.15,.2); u <- thurston_unit(c(.4,.3,.7)); h <- 1e-5
  for (g in thurston_types) {
    f <- geodesic_3d(g,p,u,.7)
    nearby <- thurston_flow(g,p,u,c(.7-h,.7+h))
    expect_equal(drop(diff(nearby$position))/(2*h),drop(f$coordinate_velocity),tolerance=2e-7,info=g)
    expect_equal(sum(f$velocity^2),1,tolerance=2e-9,info=g)
    first <- thurston_flow(g,p,u,.3)
    composed <- thurston_flow(g,drop(first$position),drop(first$velocity),.4)
    expect_equal(composed$position,f$position,tolerance=2e-8,info=g)
    Q <- matrix(f$transport,3,3)
    expect_equal(crossprod(Q),diag(3),tolerance=2e-9,info=g)
    expect_equal(drop(Q %*% u),drop(f$velocity),tolerance=1e-5,info=g)
    expect_true(f$area>0,info=g)
  }
})

test_that("constant-curvature flows and sphere areas have their analytic values", {
  t <- c(.01,.7,2)
  e <- geodesic_3d("e3",direction=c(1,0,0),distance=t)
  s <- geodesic_3d("s3",direction=c(1,0,0),distance=t)
  h <- geodesic_3d("h3",direction=c(1,0,0),distance=t)
  expect_equal(e$position[,1],t)
  expect_equal(s$position[,1],tan(t/2))
  expect_equal(h$position[,1],tanh(t/2))
  expect_equal(e$area,t^2)
  expect_equal(s$area,sin(t)^2)
  expect_equal(h$area,sinh(t)^2)
  expect_equal(geodesic_3d("s3",direction=c(1,0,0),distance=2*pi)$position,matrix(0,1,3),tolerance=1e-12)
  expect_equal(geodesic_3d("s2xe",direction=c(0,0,1),distance=2)$position,matrix(c(0,0,2),1,3))
})

test_that("Grayson velocities satisfy their independent reduced equations", {
  u <- thurston_unit(c(.4,-.6,.5)); h <- 1e-5
  for (g in c("nil","sol","sl2r")) for (t in c(-.7,.002,.8,5)) {
    f <- thurston_flow(g,c(0,0,0),u,c(t-h,t,t+h)); v <- f$velocity[2,]
    rhs <- switch(g,nil=c(-v[3]*v[2],v[3]*v[1],0),
      sol=c(v[1]*v[3],-v[2]*v[3],v[2]^2-v[1]^2),sl2r=c(2*v[2]*v[3],-2*v[1]*v[3],0))
    expect_equal((f$velocity[3,]-f$velocity[1,])/(2*h),rhs,tolerance=2e-7,info=paste(g,t))
    expect_equal((f$position[3,]-f$position[1,])/(2*h),f$coordinate_velocity[2,],tolerance=2e-7,info=paste(g,t))
  }
  for (u in list(c(0,.6,.8),c(.6,0,.8),c(1e-10,.6,.8),c(1e-7,.6,.8),c(1e-4,.6,.8),c(.002,.6,.8),c(1,1,0))) {
    f <- thurston_flow("sol",c(0,0,0),u,c(.2-h,.2,.2+h))
    expect_equal((f$position[3,]-f$position[1,])/(2*h),f$coordinate_velocity[2,],tolerance=2e-7)
  }
  lift <- geodesic_3d("sl2r",direction=c(0,0,1),distance=c(-20,20))
  expect_equal(lift$position[,3],c(-20,20),tolerance=1e-10)
})

test_that("intrinsic balls intersect at geodesic radius and threads agree", {
  u <- thurston_unit(c(.3,.4,1))
  for (g in setdiff(thurston_types,"sol")) {
    center <- drop(thurston_flow(g,c(0,0,0),u,1.5)$position)
    fields <- list(thurston_field(center))
    settings <- backend_3d("raymarch",geometry=g,max_distance=3,tolerance=1e-7)
    one <- ggplot3d:::raymarch_curved_cpp(fields,matrix(u,1,3),settings,1L)
    expect_equal(one$distance,1.3,tolerance=2e-6,info=g)
    expect_equal(sum(one$normal^2),1,tolerance=1e-8,info=g)
    two <- ggplot3d:::raymarch_curved_cpp(fields,matrix(rep(u,40),40,3,byrow=TRUE),settings,2L)
    expect_equal(two$distance,rep(one$distance,40),tolerance=0,info=g)
  }
})

test_that("quotient faces carry rays through isometries", {
  left <- right <- diag(4);right[1,4] <- -2;left[1,4] <- 2
  faces <- list(thurston_portal_3d(c(1,0,0,0),1,right),thurston_portal_3d(c(-1,0,0,0),1,left))
  settings <- backend_3d("raymarch",quotient=faces,max_distance=4,tolerance=1e-7)
  hit <- ggplot3d:::raymarch_curved_cpp(list(thurston_field(c(-.5,0,0))),matrix(c(1,0,0),1,3),settings,1L)
  expect_equal(hit$distance,1.3,tolerance=2e-6)
  expect_equal(hit$crossings,1L)
  expect_equal(hit$position[1,1],-.7,tolerance=2e-6)
  right[2,2] <- 2
  settings$quotient[[1]] <- thurston_portal_3d(c(1,0,0,0),1,right)
  expect_error(ggplot3d:::raymarch_curved_cpp(list(thurston_field(c(-.5,0,0))),matrix(c(1,0,0),1,3),settings,1L),"isometry")
})

test_that("charted SDFs use the compiled kernel across the spherical pole", {
  spec <- list(kind="sdf",tape=ggplot3d:::sdf_compile_3d(sdf_sphere(c(-.25,0,0),.05)),
    ranges=list(x=c(-.4,-.1),y=c(-.1,.1),z=c(-.1,.1)),rgb=c(1,0,0))
  hit <- ggplot3d:::raymarch_curved_cpp(list(spec),matrix(c(1,0,0),1,3),
    backend_3d("raymarch",geometry="s3",max_distance=7,tolerance=1e-7),1L)
  expect_equal(hit$distance,2*pi-2*atan(.3),tolerance=2e-6)
})

test_that("transport solves the skew-symmetric connection equation", {
  u <- thurston_unit(c(.4,.3,.7));h <- 1e-4
  for (g in c("nil","sol","sl2r")) {
    f <- geodesic_3d(g,direction=u,distance=c(.7-h,.7,.7+h));v <- f$velocity[2,]
    B <- switch(g,nil=matrix(c(0,-v[3],-v[2],v[3],0,v[1],v[2],-v[1],0),3)/2,
      sol=matrix(c(0,0,v[1],0,0,-v[2],-v[1],v[2],0),3),
      sl2r=matrix(c(0,3*v[3],v[2],-3*v[3],0,-v[1],-v[2],v[1],0),3)/2)
    dQ <- matrix((f$transport[3,]-f$transport[1,])/(2*h),3)
    expect_equal(dQ,-B %*% matrix(f$transport[2,],3),tolerance=3e-5,info=g)
  }
})

test_that("Jacobi areas agree with axial closed forms", {
  t <- c(.2,.7,1.5)
  for (g in c("nil","sl2r")) expect_equal(geodesic_3d(g,distance=t)$area,4*sin(t/2)^2,tolerance=1e-7)
  expect_equal(geodesic_3d("sol",distance=t)$area,sinh(t)^2,tolerance=1e-7)
  expect_equal(geodesic_3d("s2xe",direction=c(1,0,0),distance=t)$area,t*sin(t),tolerance=1e-7)
  expect_equal(geodesic_3d("h2xe",direction=c(1,0,0),distance=t)$area,t*sinh(t),tolerance=1e-7)
  long <- geodesic_3d("sol",distance=c(-20,20))
  expect_equal(long$position[,3],c(-20,20),tolerance=1e-10)
  expect_equal(long$area/sinh(20)^2,c(1,1),tolerance=2e-7)
  small <- thurston_flow("sol",c(0,0,0),c(1/sqrt(2),1/sqrt(2),1e-8),.7)
  expect_equal(small$position[1,3]/1e-8,sin(sqrt(2)*.7)/sqrt(2),tolerance=1e-7)
})

test_that("Nil, Sol, hyperbolic and lifted fibre quotients preserve rays", {
  for (g in c("nil","sol","h3","sl2r","s2xe","h2xe")) {
    right <- left <- diag(4);axis <- 1L;extent <- 1;origin <- c(0,0,0);center <- c(-.5,0,0)
    lift <- 0;fiber <- FALSE
    if (g=="nil") {right[1,4] <- -2;right[3,2] <- -1;left <- solve(right)}
    if (g=="sol") {right[1,1] <- exp(-2);right[2,2] <- exp(2);right[3,4] <- -2;left <- solve(right);axis <- 3L;center <- c(0,0,-.5)}
    if (g=="h3") {right[c(1,4),c(1,4)] <- matrix(c(cosh(2),-sinh(2),-sinh(2),cosh(2)),2);left <- solve(right);extent <- sinh(1);center <- c(-tanh(.25),0,0)}
    if (g %in% c("sl2r","s2xe","h2xe")) {axis <- 4L;fiber <- TRUE;extent <- if(g=="sl2r") 2*pi else 1;lift <- -2*extent;origin <- c(0,0,extent-1);center <- c(0,0,-extent+.5)}
    n <- rep(0,4);n[axis] <- 1;u <- rep(0,3);u[min(3L,axis)] <- 1
    faces <- list(thurston_portal_3d(n,extent,right,lift,fiber),thurston_portal_3d(-n,extent,left,-lift,fiber))
    settings <- backend_3d("raymarch",geometry=g,observer=origin,quotient=faces,max_distance=3,tolerance=1e-7)
    hit <- ggplot3d:::raymarch_curved_cpp(list(thurston_field(center)),matrix(u,1,3),settings,1L)
    expect_equal(hit$distance,1.3,tolerance=3e-6,info=g)
    expect_equal(hit$crossings,1L,info=g)
  }
})

test_that("variational areas agree with transverse geometric perturbations", {
  u <- thurston_unit(c(.3,.4,.8));a <- thurston_unit(c(u[2],-u[1],0))
  b <- c(u[2]*a[3]-u[3]*a[2],u[3]*a[1]-u[1]*a[3],u[1]*a[2]-u[2]*a[1]);h <- 1e-5
  for (g in c("nil","sol","sl2r")) {
    origin <- c(.2,-.1,.3);f <- geodesic_3d(g,origin,u,2);p <- drop(f$position)
    J <- lapply(list(a,b),function(v) drop(thurston_flow(g,origin,u+h*v,2)$position-thurston_flow(g,origin,u-h*v,2)$position)/(2*h))
    inner <- function(v,w) {
      if(g=="sol") return(sum(v*w*c(exp(-2*p[3]),exp(2*p[3]),1)))
      if(g=="nil") return(sum(v[1:2]*w[1:2])+(v[3]-(p[1]*v[2]-p[2]*v[1])/2)*(w[3]-(p[1]*w[2]-p[2]*w[1])/2))
      z <- sqrt(1+sum(p[1:2]^2))
      sum(v[1:2]*w[1:2])-sum(p[1:2]*v[1:2])*sum(p[1:2]*w[1:2])/z^2+
        (v[3]+(p[1]*v[2]-p[2]*v[1])/(z+1))*(w[3]+(p[1]*w[2]-p[2]*w[1])/(z+1))
    }
    area <- sqrt(inner(J[[1]],J[[1]])*inner(J[[2]],J[[2]])-inner(J[[1]],J[[2]])^2)
    expect_equal(f$area,area,tolerance=2e-7,info=g)
  }
})

test_that("cylinders and half-spaces have intrinsic distances", {
  for (g in thurston_types) {
    direction <- if(g=="sol") c(0,1,0) else c(1,0,0)
    origin <- drop(thurston_flow(g,c(0,0,0),direction,1)$position)
    backward <- -drop(thurston_flow(g,c(0,0,0),direction,1)$velocity)
    settings <- backend_3d("raymarch",geometry=g,observer=origin,max_distance=2,tolerance=1e-7)
    hit <- ggplot3d:::raymarch_curved_cpp(list(thurston_field(c(0,0,0),shape=1L,radius=.25)),matrix(backward,1,3),settings,1L)
    expect_equal(hit$distance,.75,tolerance=3e-6,info=g)
  }
})

test_that("intrinsic panels render without exterior furniture", {
  p <- ggplot2::ggplot()+geom_thurston_3d(center=c(0,0,2),fill="red")+
    coord_3d(backend=backend_3d("raymarch",geometry="nil",dpi=12,max_distance=4))
  path <- tempfile(fileext=".png");grDevices::png(path,width=100,height=100)
  expect_no_error(print(p));grid::grid.force()
  expect_gt(sum(is.finite(grid::grid.get("scene-thurston-3d")$raster_result$intrinsic$distance)),0)
  grDevices::dev.off();unlink(path)
  expect_error(backend_3d("raymarch",geometry="h3",observer=c(1,0,0)),"open unit")
  expect_error(backend_3d("raymarch",facing=matrix(1,3,3)),"orthonormal")
})
