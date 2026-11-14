test_that("inside cameras put the eye at the model origin for every view", {
  for(coord in list(coord_hyperbolic_3d(inside=TRUE),coord_spherical_geometry_3d(inside=TRUE))) {
    expect_identical(coord$camera$projection,"perspective")
    expect_identical(coord$config$frame,"sphere")
    for(camera in list(coord$camera,coord$view_camera(camera_3d(azimuth=37,elevation=-12)))) {
      basis <- frame_basis_3d(camera,new_scene_viewport(100,70))
      eye <- camera$target*basis$scale-camera$distance*basis$forward
      expect_equal(eye,.5*basis$scale,tolerance=1e-12)
      expect_gt(camera$near,0)
      expect_gt(camera$far,camera$near)
    }
  }
})

test_that("observer transformations send finite observers to their origins", {
  h <- coord_hyperbolic_3d(inside=TRUE,input="poincare",observer=c(.2,.1,0))
  X <- h$space$to_canonical(data.frame(x=.2,y=.1,z=0),"poincare")
  expect_equal(unname(h$space$act(h$isometry,X)),matrix(c(0,0,0,1),1),tolerance=1e-12)
  observer <- c(.2,.3,.1,sqrt(.86))
  s <- coord_spherical_geometry_3d(inside=TRUE,input="ambient",observer=observer)
  expect_equal(unname(s$space$act(s$isometry,matrix(observer,1))),matrix(c(0,0,0,1),1),tolerance=1e-12)
  expect_equal(det(s$isometry),1,tolerance=1e-12)
  expect_equal(spherical_observer_3d(c(0,0,0,-1),s$space,"ambient")%*%c(0,0,0,-1),matrix(c(0,0,0,1),4),tolerance=1e-12)
  expect_error(coord_hyperbolic_3d(inside=TRUE,model="poincare"),"Klein")
  expect_error(coord_spherical_geometry_3d(inside=TRUE,model="stereographic"),"gnomonic")
})

test_that("first-person projective angles agree with intrinsic tangent directions", {
  coord <- coord_hyperbolic_3d(inside=TRUE,input="poincare",azimuth=180,elevation=0)
  d <- data.frame(x=c(.2,.4),y=c(.1,.2),z=c(.05,.1))
  X <- coord$space$to_canonical(d,"poincare")
  context <- list(space=coord$space,model="klein",ranges=list(x=c(-1,1),y=c(-1,1),z=c(-1,1)))
  xyz <- homogeneous_xyz_3d(do.call(cbind,space_packet_coordinates_3d(X,context)))
  camera <- coord$camera; basis <- frame_basis_3d(camera,new_scene_viewport(100,100))
  eye <- frame_eye_coordinates_3d(xyz,camera,basis)
  expect_equal(unname(eye[,1]/eye[,3]),unname(-X[,2]/X[,1]),tolerance=1e-12)
  expect_equal(unname(eye[,2]/eye[,3]),unname(X[,3]/X[,1]),tolerance=1e-12)
})

test_that("depth cue uses geodesic-sphere inverse area in linear light", {
  for(kind in c("hyperbolic","spherical")) {
    radius <- c(.5,1.4)
    d <- data.frame(x=if(kind=="hyperbolic") tanh(radius/2) else tan(radius),y=0,z=0)
    coord <- if(kind=="hyperbolic") coord_hyperbolic_3d(inside=TRUE,input="poincare",fog=.1) else coord_spherical_geometry_3d(inside=TRUE,fog=.1,extent=10)
    packet <- stats_scene(ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_point_3d(colour="white")+coord)$packet
    colour <- packet$styles$colour[match(packet$markers$style_id,packet$styles$style_id)]
    actual <- as.numeric(grDevices::col2rgb(colour)[1,])
    expected <- pmin(1,.1/(if(kind=="hyperbolic") sinh(radius)^2 else sin(radius)^2))
    expect_equal(actual,round(255*srgb_encode_3d(expected)))
    expect_silent(validate_scene_packet(packet))
  }
})

test_that("inside vector rendering clips geometry behind and across the eye", {
  d <- data.frame(x=c(-.5,.5,.5,-.5),y=c(-.3,-.3,.3,.3),z=.2)
  for(coord in list(coord_hyperbolic_3d(inside=TRUE,azimuth=0,elevation=0),
                    coord_spherical_geometry_3d(inside=TRUE,azimuth=0,elevation=0))) {
    p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d(fill="steelblue")+coord+ggplot2::theme_void()
    f <- tempfile(fileext=".png"); on.exit(unlink(f),add=TRUE)
    ragg::agg_png(f,width=120,height=120)
    expect_silent(print(p))
    grDevices::dev.off()
    expect_gt(file.info(f)$size,0)
  }
})

test_that("gnomonic faces crossing the observer hemisphere clip to finite box planes", {
  d <- data.frame(x=c(-1,1,1,-1),y=1,z=c(-3,-3,1,1))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d()+
    coord_spherical_geometry_3d(inside=TRUE,observer=c(0,0,1))
  packet <- stats_scene(p)$packet
  expect_gt(length(packet$faces$offset),0)
  expect_true(all(unlist(packet$vertices)>=-1e-10&unlist(packet$vertices)<=1+1e-10))
  expect_silent(validate_scene_packet(packet))
})

test_that("gnomonic Lambert normals are spherical tangent normals", {
  d <- data.frame(x=c(-.2,.2,0),y=c(0,0,.3),z=.4)
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d()+
    coord_spherical_geometry_3d(model="gnomonic")
  packet <- stats_scene(p)$packet
  styles <- packet$styles[match(packet$faces$style_id,packet$styles$style_id),]
  normal <- unname(as.matrix(styles[c("normal_x","normal_y","normal_z")]))
  expect_true(all(styles$intrinsic_normal))
  expect_equal(rowSums(normal^2),rep(1,nrow(normal)),tolerance=1e-12)
  X <- c(0,.1,.4,1)/sqrt(1+.1^2+.4^2)
  N <- c(0,0,1,-.4)/sqrt(1+.4^2)
  expected <- N[1:3]-N[4]*X[1:3]/(1+X[4])
  expect_equal(abs(sum(normal[1,]*expected)),1,tolerance=1e-12)
})
