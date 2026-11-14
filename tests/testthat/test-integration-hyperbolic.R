test_that("hyperbolic curvature and centring preserve intrinsic geometry", {
  coord <- coord_hyperbolic_3d(curvature=-4,centre=c(.3,.1,0))
  expect_identical(coord$model,"klein")
  s <- coord$space
  X <- s$to_canonical(data.frame(x=c(0,.3),y=c(0,.1),z=0),"poincare")
  Y <- s$act(coord$isometry,X)
  expect_equal(unname(Y[2,]),c(0,0,0,1),tolerance=1e-12)
  expect_equal(s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE]),hyperbolic_distance_3d(X[1,,drop=FALSE],X[2,,drop=FALSE])/2)
  expect_equal(s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE]),s$distance(Y[1,,drop=FALSE],Y[2,,drop=FALSE]),tolerance=1e-12)
  for(t in c(0,.25,.5,.75,1)) {
    Z <- s$geodesic(X[1,,drop=FALSE],X[2,,drop=FALSE],t)
    expect_equal(s$distance(X[1,,drop=FALSE],Z),t*s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE]),tolerance=1e-12)
  }
  expect_error(coord_hyperbolic_3d(curvature=0),"negative")
  expect_error(coord_hyperbolic_3d(centre=c(1,0,0)),"strictly")
  expect_error(coord_hyperbolic_3d(aspect=c(1,2,1)),"aspect")
})

test_that("hyperbolic chart geodesics have their analytic circle shapes", {
  s <- SpaceHyperbolicFinite
  X <- s$to_canonical(data.frame(x=c(-.5,.5),y=.2,z=0),"poincare")
  Z <- s$geodesic(X[rep(1,21),,drop=FALSE],X[rep(2,21),,drop=FALSE],seq(0,1,length.out=21))
  P <- s$from_canonical(Z,"poincare")
  centre <- (1+.5^2+.2^2)/(2*.2)
  expect_equal(unname(P[,1]^2+(P[,2]-centre)^2),rep(centre^2-1,21),tolerance=1e-10)
  H <- s$from_canonical(Z,"halfspace")
  expect_true(all(H[,3]>0))
  expect_equal(lorentz_dot_3d(Z,Z),rep(-1,21),tolerance=1e-12)
  expect_equal(s$geodesic(X[2,,drop=FALSE],X[1,,drop=FALSE],.25),s$geodesic(X[1,,drop=FALSE],X[2,,drop=FALSE],.75))
})

test_that("half-space paths truncate to the finite panel box", {
  d <- data.frame(x=c(-4,4),y=0,z=.5)
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+
    coord_hyperbolic_3d(input="halfspace",model="halfspace",extent=1)
  out <- stats_scene(p)
  expect_true(out$built$layout$panel_params[[1]]$camera$clip_data)
  expect_gt(length(out$packet$vertices$x),2)
  expect_silent(validate_scene_packet(out$packet))
})

test_that("hyperbolic identity scales and radial distance guides are explicit", {
  d <- data.frame(x=c(.1,.3),y=.1,z=.1)
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+coord_hyperbolic_3d(distance_breaks=c(0,1,2))
  out <- stats_scene(p)
  expect_equal(length(out$packet$text$offset),3)
  expect_setequal(out$packet$styles$label[out$packet$styles$label!=""],c("0","1","2"))
  expect_error(ggplot2::ggplot_build(p+ggplot2::scale_x_log10()),"identity")
  expect_error(ggplot2::ggplot_build(p+scale_z_log10()),"identity")
})

test_that("Klein face lighting uses transported Minkowski normals", {
  d <- data.frame(x=c(-.2,.2,0),y=c(0,0,.3),z=.3)
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d(fill="steelblue",shade=TRUE)+coord_hyperbolic_3d(input="klein")
  out <- stats_scene(p)$packet
  styles <- out$styles[match(out$faces$style_id,out$styles$style_id),]
  expect_true(all(styles$intrinsic_normal))
  normal <- unname(as.matrix(styles[c("normal_x","normal_y","normal_z")]))
  expect_equal(rowSums(normal^2),rep(1,nrow(normal)),tolerance=1e-12)
  X <- c(0,.1,.3,1)/sqrt(1-.1^2-.3^2)
  expected <- drop(hyperbolic_origin_boost_3d(X,TRUE)%*%(c(0,0,1,.3)/sqrt(1-.3^2)))[1:3]
  expect_equal(abs(sum(normal[1,]*expected)),1,tolerance=1e-12)
  expect_silent(validate_scene_packet(out))
})
