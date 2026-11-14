test_that("stereographic and gnomonic charts preserve spherical points", {
  s <- SpaceSpherical3D
  P <- data.frame(x=c(-1,0,.3),y=c(.2,0,.1),z=c(0,0,-.2))
  for(chart in c("stereographic","gnomonic")) {
    X <- s$to_canonical(P,chart)
    expect_equal(rowSums(X^2),rep(1,3),tolerance=1e-12)
    expect_equal(unname(s$from_canonical(X,chart)),unname(as.matrix(P)),tolerance=1e-12)
  }
  X <- s$to_canonical(P,"stereographic")
  G <- rotate_4d("xy",.4)%*%rotate_4d("zw",.2)
  expect_equal(s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE]),s$distance(s$act(G,X[1,,drop=FALSE]),s$act(G,X[2,,drop=FALSE])),tolerance=1e-12)
  expect_error(s$act(diag(c(1,1,1,-1)),X),"SO\\(4\\)")
  Z <- s$geodesic(X[1,,drop=FALSE],X[2,,drop=FALSE],.3)
  expect_equal(s$distance(X[1,,drop=FALSE],Z),.3*s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE]),tolerance=1e-12)
})

test_that("Hopf fibres lie on S3 and have constant Hopf image", {
  lon <- 43; lat <- -22
  X <- hopf_points_3d(lon,lat,seq(0,2*pi,length.out=65))
  z1 <- X[,1]+1i*X[,2]; z2 <- X[,3]+1i*X[,4]
  image <- cbind(Re(2*z1*Conj(z2)),Im(2*z1*Conj(z2)),Mod(z1)^2-Mod(z2)^2)
  expect_equal(rowSums(X^2),rep(1,nrow(X)),tolerance=1e-12)
  expect_equal(unname(image),unname(lonlat_xyz_3d(rep(lon,nrow(X)),rep(lat,nrow(X)))),tolerance=1e-12)
  expect_equal(X[1,],X[nrow(X),],tolerance=1e-12)
  X <- StatHopfFibre3D$compute_group(data.frame(lon=0,lat=0,group=1),NULL,n=12,what="torus")
  expect_equal(X$.s1^2+X$.s2^2,rep(.5,nrow(X)),tolerance=1e-12)
  expect_equal(X$.s3^2+X$.s4^2,rep(.5,nrow(X)),tolerance=1e-12)
  expect_equal(length(unique(X$.face)),2*12^2)
})

test_that("distinct Hopf fibres have linking number one", {
  project <- function(lon) SpaceSpherical3D$from_canonical(hopf_points_3d(lon,20,seq(0,2*pi,length.out=129)),"stereographic")
  A <- project(0); B <- project(120)
  dA <- diff(A); dB <- diff(B); A <- (head(A,-1L)+tail(A,-1L))/2; B <- (head(B,-1L)+tail(B,-1L))/2
  integral <- sum(vapply(seq_len(nrow(A)),function(i) {
    r <- sweep(B,2L,A[i,],"-")
    sum(rowSums(cross_rows_3d(dA[rep(i,nrow(B)),,drop=FALSE],dB)*r)/rowSums(r^2)^1.5)
  },numeric(1)))/(4*pi)
  expect_equal(abs(integral),1,tolerance=.02)
})

test_that("pole-crossing fibres split into finite panel-box paths", {
  coord <- coord_spherical_geometry_3d(extent=2)
  params <- list(ranges=list(x=c(-1,1),y=c(-1,1),z=c(-1,1)),camera=coord$camera,frame=FrameBox)
  context <- bind_space_3d(space_spec_3d(coord$space,"stereographic","canonical",NULL,1e-3,9L),params)
  X <- hopf_points_3d(0,-90,seq(0,2*pi,length.out=17))
  paths <- space_path_samples_3d(X,context)
  expect_gte(length(paths),2)
  xyz <- do.call(rbind,lapply(paths,`[[`,"model"))
  expect_true(all(is.finite(xyz)))
  expect_true(all(xyz>=-1e-10&xyz<=1+1e-10))
  expect_equal(range(xyz[,3]),c(0,1),tolerance=1e-10)
  expect_false(coord$space$contains(matrix(c(0,0,0,1),1),"stereographic"))
})

test_that("Hopf paths tubes tori and matching bases emit valid packets", {
  d <- data.frame(lon=c(0,120),lat=c(0,0),id=factor(1:2))
  for(what in c("fibres","torus","base")) {
    p <- ggplot2::ggplot(d,ggplot2::aes(lon=lon,lat=lat,fill=id,colour=id))+
      stat_hopf_fibre_3d(what=what,n=8,sides=4)+
      if(what=="base") coord_3d(axes=character(),panes=character(),box="none") else coord_spherical_geometry_3d()
    packet <- stats_scene(p)$packet
    expect_silent(validate_scene_packet(packet))
    expect_gt(length(packet$vertices$x),0)
    if(what=="base") expect_equal(length(packet$markers$offset),2L) else expect_gt(length(packet$faces$offset),0)
  }
  p <- ggplot2::ggplot(data.frame(lon=0,lat=-90),ggplot2::aes(lon=lon,lat=lat))+
    stat_hopf_fibre_3d(n=8,radius=0)+coord_spherical_geometry_3d()
  packet <- stats_scene(p)$packet
  expect_gte(length(packet$polylines$offset),2)
  expect_true(all(is.finite(unlist(packet$vertices))))
})

test_that("SO4 schedules animate spherical fibres", {
  p <- ggplot2::ggplot(data.frame(lon=0,lat=0),ggplot2::aes(lon=lon,lat=lat))+
    stat_hopf_fibre_3d(n=8,radius=0)+coord_spherical_geometry_3d()
  a <- animate_3d(p,isometry=list(diag(4),rotate_4d("xw",.3)),frames=2,width=80,height=80,workers=1,progress=FALSE)
  on.exit(unlink(a$frames),add=TRUE)
  expect_equal(a$frame_count,2L)
  expect_false(identical(readBin(a$frames[1],"raw",file.info(a$frames[1])$size),readBin(a$frames[2],"raw",file.info(a$frames[2])$size)))
})
