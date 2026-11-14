test_that("all six plane rotations and quaternion double rotations lie in SO4", {
  for(plane in c("xy","xz","xw","yz","yw","zw")) {
    R <- rotate_4d(plane,.4)
    expect_equal(crossprod(R),diag(4),tolerance=1e-12)
    expect_equal(det(R),1,tolerance=1e-12)
    expect_equal(rotate_4d(plane,-.4)%*%R,diag(4),tolerance=1e-12)
  }
  L <- c(sin(.3),0,0,cos(.3)); Q <- c(0,sin(.4),0,cos(.4))
  R <- rotate_4d(left=L,right=Q)
  expect_equal(crossprod(R),diag(4),tolerance=1e-12); expect_equal(det(R),1,tolerance=1e-12)
  expect_error(rotate_4d(left=2*L,right=Q),"unit")
})

test_that("linear and finite perspective projections agree with matrix formulas", {
  d <- data.frame(x=c(1,2,3),y=c(-1,0,1),z=c(2,1,0),w=c(.2,.4,.6))
  R <- rotate_4d("xw",.3)
  for(projection in c("orthographic","perspective")) {
    p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+stat_project_3d(rotation=R,projection=projection,eye=3)+coord_3d()
    got <- ggplot2::layer_data(p)
    X <- as.matrix(d)%*%t(R); expected <- X[,1:3,drop=FALSE]
    if(projection=="perspective") expected <- expected*3/(3-X[,4])
    expect_equal(unname(as.matrix(got[c("x","y","z")])),unname(expected),tolerance=1e-12)
    expect_silent(validate_scene_packet(stats_scene(p)$packet))
  }
  basis <- qr.Q(qr(matrix(sin(1:18),6,3)))
  X <- matrix(cos(1:30),5,6); d <- as.data.frame(X); names(d) <- c("x","y","z","a","b","c")
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,a=a,b=b,c=c))+stat_project_3d(dimensions=names(d),basis=basis)+coord_3d()
  got <- ggplot2::layer_data(p)
  expect_equal(unname(as.matrix(got[c("x","y","z")])),unname(X%*%basis),tolerance=1e-12)
  expect_error(project_nd_3d(matrix(c(0,0,0,1),1),diag(4)[,1:3],"stereographic",4),"#16")
  near_eye <- matrix(c(1,0,0,1-1e-14),1)
  expect_true(all(is.finite(project_nd_3d(near_eye,diag(4)[,1:3],"perspective",1))))
})

test_that("projected tesseract faces stay planar and compile under a finite budget", {
  source(testthat::test_path("..","fixtures","scenes","polytopes4d.R"),local=TRUE)
  m <- tesseract_mesh_4d()
  expect_equal(length(unique(m$.face)),24L)
  R <- rotate_4d("xw",.4)%*%rotate_4d("yw",.3)%*%rotate_4d("zw",.2)
  for(projection in c("orthographic","perspective")) {
    p <- ggplot2::ggplot(m,ggplot2::aes(w=w))+stat_project_3d(geom="mesh",rotation=R,projection=projection)+coord_3d()
    d <- ggplot2::layer_data(p)
    for(rows in split(seq_len(nrow(d)),d$.face)) {
      X <- as.matrix(d[rows,c("x","y","z")]); X <- sweep(X,2,colMeans(X),"-")
      expect_lt(tail(svd(X,nu=0,nv=0)$d,1),1e-10)
    }
    packet <- stats_scene(p)$packet
    viewport <- new_scene_viewport(80,80); attr(viewport,"engine_budget_bytes") <- 32*2^20
    compiled <- ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],new_camera_3d(),viewport)
    expect_true(compiled$capabilities$exact_visibility)
    expect_gt(length(compiled$fragments$offset),0)
  }
})

test_that("stereographic paths are sampled as spherical arcs", {
  d <- data.frame(x=c(.6,0),y=c(0,.6),z=c(.4,.4),w=c(.3,.3))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+stat_project_3d(geom="path",projection="stereographic")+
    coord_3d(xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
  packet <- stats_scene(p)$packet
  expect_gt(length(packet$vertices$x),2L)
  expect_silent(validate_scene_packet(packet))
})

test_that("Grassmann tours preserve frames endpoints principal angles and random state", {
  A <- diag(5)[,1:3]; B <- A; angle <- .7
  B[,3] <- c(0,0,cos(angle),sin(angle),0)
  M <- grassmann_basis_3d(A,B,.5)
  expect_equal(crossprod(M),diag(3),tolerance=1e-12)
  expect_equal(M[,3],c(0,0,cos(angle/2),sin(angle/2),0),tolerance=1e-12)
  history <- array(c(A,B),c(5,3,2))
  frames <- tour_basis_frames_3d(tour_grand(history=history),5,5)
  expect_equal(tour_basis_frames_3d(history,5,5),frames,tolerance=1e-12)
  expect_equal(frames[[1]],A,tolerance=1e-12)
  expect_equal(tcrossprod(frames[[5]]),tcrossprod(B),tolerance=1e-12)
  set.seed(42); before <- .Random.seed
  first <- tour_basis_frames_3d(tour_grand(seed=3),6,5)
  expect_identical(.Random.seed,before)
  expect_identical(first,tour_basis_frames_3d(tour_grand(seed=3),6,5))
  expect_true(all(vapply(first,function(B) max(abs(crossprod(B)-diag(3)))<1e-10,logical(1))))
})

test_that("four-dimensional sections recover a closed tetrahedron including boundary slices", {
  v <- rbind(c(0,0,0,0),diag(4)); cells <- t(combn(5L,4L))
  for(offset in c(.3,0)) {
    m <- section_mesh_4d(v,cells,c(0,0,0,1),offset)
    expect_equal(length(unique(m$.face)),4L)
    expect_equal(length(unique(m$.vertex)),4L)
    f <- matrix(m$.vertex,ncol=3,byrow=TRUE); e <- rbind(f[,1:2],f[,2:3],f[,c(3,1)])
    count <- table(paste(pmin(e[,1],e[,2]),pmax(e[,1],e[,2])))
    expect_true(all(count==2L))
    X <- unique(m[c("x","y","z")]); distance <- as.numeric(stats::dist(X))
    expect_equal(sort(distance),sort(c(rep(1-offset,3),rep(sqrt(2)*(1-offset),3))),tolerance=1e-10)
  }
  d <- as.data.frame(v); names(d) <- c("x","y","z","w")
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+stat_section_4d(cells=cells,offset=.3)+coord_3d()
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
})

test_that("grand tours render changing orthographic projections with fixed limits", {
  d <- as.data.frame(matrix(sin(1:40),10,4)); names(d) <- c("x","y","z","w")
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+stat_project_3d(size=3)+coord_3d(axes=character(),panes=character(),box="none")
  a <- animate_3d(p,tour=tour_grand(steps=2,seed=1),frames=3,width=120,height=120,workers=1,progress=FALSE)
  on.exit(unlink(a$frames),add=TRUE)
  expect_equal(a$frame_count,3L)
  expect_equal(length(a$tour_bases),3L)
  expect_equal(a$camera$azimuth,rep(p@coordinates$camera$azimuth,3L))
  expect_false(identical(readBin(a$frames[1],"raw",n=file.info(a$frames[1])$size),readBin(a$frames[3],"raw",n=file.info(a$frames[3])$size)))
  state <- motion_tour_prepare_3d(p,tour_grand(),3)
  radius <- max(sqrt(rowSums(as.matrix(d)^2)))
  expect_equal(state$plot@coordinates$limits$x,c(-radius,radius))
  expect_null(p@coordinates$limits$x)
  motion_tour_plot_3d(state$plot,a$tour_bases[[3]])
  expect_equal(p@layers[[1]]$stat_params$basis,diag(4)[,1:3])
})

test_that("collapsed projective faces and curved stereographic polygons are handled", {
  source(testthat::test_path("..","fixtures","scenes","polytopes4d.R"),local=TRUE)
  m <- tesseract_mesh_4d()
  p <- ggplot2::ggplot(m,ggplot2::aes(w=w))+stat_project_3d(geom="mesh")+coord_3d()
  expect_equal(length(unique(ggplot2::layer_data(p)$.face)),12L)
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
  p <- ggplot2::ggplot(m,ggplot2::aes(w=w))+stat_project_3d(geom="mesh",projection="stereographic")+
    coord_3d(xlim=c(-4,4),ylim=c(-4,4),zlim=c(-4,4))
  packet <- stats_scene(p)$packet
  expect_gt(length(packet$faces$offset),48L)
  expect_silent(validate_scene_packet(packet))
})

test_that("the projected 120-cell has exact combinatorics and honours the vector budget", {
  source(testthat::test_path("..","fixtures","scenes","polytopes4d.R"),local=TRUE)
  m <- polytope_120_cell_mesh_4d()
  expect_equal(length(unique(m$.vertex)),600L)
  expect_equal(length(unique(m$.face)),720L)
  R <- rotate_4d("xw",.4)%*%rotate_4d("yw",.3)%*%rotate_4d("zw",.2)
  p <- ggplot2::ggplot(m,ggplot2::aes(w=w))+stat_project_3d(geom="mesh",rotation=R)+coord_3d()
  packet <- stats_scene(p)$packet; expect_silent(validate_scene_packet(packet))
  viewport <- new_scene_viewport(80,80); attr(viewport,"engine_budget_bytes") <- 64*2^20
  result <- ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],new_camera_3d(),viewport)
  expect_true(result$capabilities$exact_visibility)
  attr(viewport,"engine_budget_bytes") <- 1024
  expect_error(ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],new_camera_3d(),viewport),"resource budget exceeded")
})

test_that("mesh projections retain ambient coordinates and face materials", {
  source(testthat::test_path("..","fixtures","scenes","polytopes4d.R"),local=TRUE)
  m <- tesseract_mesh_4d()
  p <- ggplot2::ggplot(m,ggplot2::aes(fill=w))+stat_project_3d(geom="mesh",rotation=rotate_4d("xw",.4))+coord_3d()
  d <- ggplot2::layer_data(p)
  expect_true(all(vapply(split(d$fill,d$.face),function(x) length(unique(x))==1L,logical(1))))
  index <- match(paste(d$.mesh,d$.face,d$.corner),paste(m$.mesh,m$.face,m$.corner))
  expect_equal(d$w,m$w[index])
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
})

test_that("finite perspective tours share projection frames and fixed limits", {
  d <- data.frame(x=c(.2,.4),y=c(-.4,.3),z=c(.2,-.2),w=c(.3,-.4))
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+stat_project_3d(projection="perspective",eye=3)+coord_3d()
  state <- motion_tour_prepare_3d(p,tour_grand(steps=1),3)
  for(basis in state$bases) {
    frame <- motion_tour_plot_3d(state$plot,basis)
    got <- ggplot2::layer_data(frame)
    expected <- project_nd_3d(as.matrix(d),basis,"perspective",3)
    expect_equal(unname(as.matrix(got[c("x","y","z")])),unname(expected),tolerance=1e-12)
    expect_true(all(abs(expected)<=state$plot@coordinates$limits$x[2]))
    expect_silent(validate_scene_packet(stats_scene(frame)$packet))
  }
})
