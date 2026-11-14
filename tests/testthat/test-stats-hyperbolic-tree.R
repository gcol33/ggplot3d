test_that("hyperbolic chart conversions agree on finite points", {
  s <- SpaceHyperbolicFinite
  P <- rbind(c(0,0,0),c(.5,0,0),c(.2,-.3,.1))
  data <- as.data.frame(P); names(data) <- c("x","y","z")
  X <- s$to_canonical(data,"poincare")
  expect_equal(lorentz_dot_3d(X,X),rep(-1,3),tolerance=1e-12)
  expect_equal(unname(s$from_canonical(X,"poincare")),P,tolerance=1e-12)
  K <- 2*P/(1+rowSums(P^2)); data[] <- K
  expect_equal(unname(s$to_canonical(data,"klein")),unname(X),tolerance=1e-12)
  H <- s$from_canonical(X,"halfspace"); data[] <- H
  expect_equal(unname(s$to_canonical(data,"halfspace")),unname(X),tolerance=1e-12)
  expect_equal(unname(s$distance(X[1,,drop=FALSE],X[2,,drop=FALSE])),log(3),tolerance=1e-12)
  middle <- s$geodesic(X[1,,drop=FALSE],X[2,,drop=FALSE],.5)
  expect_equal(unname(s$from_canonical(middle,"poincare")),matrix(c(tanh(log(3)/4),0,0),1),tolerance=1e-12)
  expect_error(s$to_canonical(data.frame(x=1,y=0,z=0),"poincare"),"strictly")
})

test_that("network edges are munched as ball geodesics", {
  d <- data.frame(x=-.5,y=.2,z=0,xend=.5,yend=.2,zend=0)
  p <- ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,xend=xend,yend=yend,zend=zend))+
    geom_segment_3d()+coord_hyperbolic_3d()
  packet <- stats_scene(p)$packet
  xyz <- do.call(cbind,packet$vertices[c("x","y","z")])*2-1
  expect_gt(nrow(xyz),2)
  expect_lt(min(xyz[,2]),.2)
  expect_true(all(rowSums(xyz^2)<1))
  expect_silent(validate_scene_packet(packet))
  q <- p; q@coordinates <- coord_hyperbolic_3d(model="klein")
  expect_equal(length(stats_scene(q)$packet$vertices$x),2L)
})

test_that("tree layout preserves keyed structure and intrinsic edge lengths", {
  from <- c("a","a","b","b","c","c"); to <- c("b","c","d","e","f","g")
  for(dimension in 2:3) {
    tree <- hyperbolic_tree_layout_3d(from,to,dimension,1.2)
    s <- SpaceHyperbolicFinite
    distance <- s$distance(tree$canonical[tree$parent,,drop=FALSE],tree$canonical[tree$child,,drop=FALSE])
    expect_equal(unname(distance),rep(1.2,length(from)),tolerance=1e-10)
    reverse <- hyperbolic_tree_layout_3d(rev(from),rev(to),dimension,1.2)
    expect_equal(reverse$nodes,tree$nodes,tolerance=1e-12)
    expect_equal(tree$nodes$depth,c(0,1,1,2,2,2,2))
    expect_true(all(rowSums(as.matrix(tree$nodes[c("x","y","z")])^2)<1))
    if(dimension==2L) expect_true(all(tree$nodes$z==0))
  }
  expect_error(hyperbolic_tree_layout_3d(c("a","b"),c("b","a"),3,1),"rooted")
  expect_error(hyperbolic_tree_layout_3d(c("a","b"),c("c","c"),3,1),"unique parents")
  expect_error(hyperbolic_tree_layout_3d(c("a","c","d"),c("b","d","c"),3,1),"cycle")
})

test_that("tree stats draw edges nodes and hyperboloid reference disks", {
  edges <- data.frame(from=c("a","a","b","b"),to=c("b","c","d","e"))
  p <- ggplot2::ggplot(edges)+stat_tree_hyperbolic_3d(dimension=2,edge_length=1)+
    stat_tree_hyperbolic_3d(what="nodes",dimension=2,edge_length=1)+
    coord_hyperbolic_3d(model="hyperboloid_sheet",references=c("klein","poincare"))
  out <- stats_scene(p)
  expect_equal(length(out$packet$markers$offset),5L)
  expect_equal(length(out$packet$polylines$offset),6L)
  d <- out$built$data[[2L]]
  sheet <- SpaceHyperbolicFinite$from_canonical(SpaceHyperbolicFinite$to_canonical(d,"poincare"),"hyperboloid_sheet")
  expect_equal(unname(sheet[,3]^2-sheet[,1]^2-sheet[,2]^2),rep(1,nrow(sheet)),tolerance=1e-10)
  expect_silent(validate_scene_packet(out$packet))
})

test_that("focus boosts move the selected node continuously to the origin", {
  point <- c(.4,.2,.1); d <- data.frame(x=point[1],y=point[2],z=point[3])
  s <- SpaceHyperbolicFinite; X <- s$to_canonical(d,"poincare")
  frames <- hyperbolic_focus_3d(point,5)
  moved <- do.call(rbind,lapply(frames,function(B) s$from_canonical(s$act(B,X),"poincare")))
  expect_equal(unname(moved[1,]),point,tolerance=1e-12)
  expect_equal(unname(moved[5,]),c(0,0,0),tolerance=1e-12)
  expect_true(all(diff(rowSums(moved^2))<0))
  expect_true(all(vapply(frames,function(B) max(abs(t(B)%*%diag(c(1,1,1,-1))%*%B-diag(c(1,1,1,-1))))<1e-12,logical(1))))
})

test_that("hyperbolic focus schedules animate the embedding", {
  edges <- data.frame(from=c("a","a","b","b"),to=c("b","c","d","e"))
  p <- ggplot2::ggplot(edges)+stat_tree_hyperbolic_3d(dimension=2,edge_length=1)+
    stat_tree_hyperbolic_3d(what="nodes",dimension=2,edge_length=1,size=2)+coord_hyperbolic_3d()
  points <- ggplot2::layer_data(p,2L)
  point <- as.numeric(points[points$node=="e",c("x","y","z")])
  a <- animate_3d(p,isometry=hyperbolic_focus_3d(point,2),frames=2,width=120,height=120,progress=FALSE,workers=1)
  on.exit(unlink(a$frames),add=TRUE)
  expect_equal(a$frame_count,2L)
  expect_false(identical(readBin(a$frames[1],"raw",n=file.info(a$frames[1])$size),readBin(a$frames[2],"raw",n=file.info(a$frames[2])$size)))
})