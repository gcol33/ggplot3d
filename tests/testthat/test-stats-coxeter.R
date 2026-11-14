test_that("Coxeter mirrors realise the Gram signature and reflection orders", {
  for(symbol in list(c(3,3),c(4,4),c(7,3),c(3,3,3),c(4,3,4),c(5,3,4))) {
    s <- coxeter_simplex_3d(symbol); rank <- length(symbol)+1L
    for(M in s$mirrors) {
      expect_equal(M%*%M,diag(rank),tolerance=1e-9)
      if(s$kind!="euclidean") expect_equal(t(M)%*%s$J%*%M,s$J,tolerance=1e-9)
    }
    for(i in seq_along(symbol)) {
      product <- s$mirrors[[i]]%*%s$mirrors[[i+1L]]; power <- diag(rank)
      for(k in seq_len(symbol[i])) power <- product%*%power
      expect_equal(power,diag(rank),tolerance=1e-8)
    }
  }
})

test_that("regular seed cells have the expected combinatorics", {
  for(symbol in list(c(3,3,3),c(4,3,4),c(5,3,4))) {
    out <- suppressMessages(coxeter_cells_3d(symbol,0,1L))
    expected <- switch(as.character(symbol[1]),`3`=c(4,4,6),`4`=c(8,6,12),`5`=c(20,12,30))
    edges <- do.call(rbind,lapply(out$faces,function(f) cbind(f,c(tail(f,-1),f[1]))))
    edges <- unique(cbind(pmin(edges[,1],edges[,2]),pmax(edges[,1],edges[,2])))
    expect_equal(c(nrow(out$vertices),length(out$faces),nrow(edges)),expected)
    expect_equal(nrow(out$vertices)-nrow(edges)+length(out$faces),2)
    expect_equal(length(out$transforms),1L)
  }
  expect_error(coxeter_simplex_3d(c(3,3,6)),"Ideal/hyperideal")
  expect_error(coxeter_simplex_3d(c(5,3,7)),"Ideal/hyperideal")
})

test_that("hyperbolic polygon circumradii satisfy the right-triangle identity", {
  for(symbol in list(c(7,3),c(5,4),c(4,5))) {
    s <- coxeter_simplex_3d(symbol)
    center <- s$vertices[3,]; vertex <- s$vertices[1,]
    cosh_radius <- -sum((center%*%s$J)*vertex)
    expect_equal(cosh_radius,1/tan(pi/symbol[1])/tan(pi/symbol[2]),tolerance=1e-10)
  }
})

test_that("spherical orbits close and radius budgets never silently truncate", {
  tetra <- suppressMessages(coxeter_cells_3d(c(3,3),pi,100))
  expect_equal(length(tetra$transforms),4L)
  simplex <- suppressMessages(coxeter_cells_3d(c(3,3,3),pi,100))
  expect_equal(length(simplex$transforms),5L)
  cube <- suppressMessages(coxeter_cells_3d(c(4,3,3),pi,100))
  expect_equal(length(cube$transforms),8L)
  expect_error(suppressMessages(coxeter_cells_3d(c(7,3),3,2)),"max_cells")
  out <- suppressMessages(coxeter_cells_3d(c(4,4),2,100))
  expect_gt(length(out$transforms),1L)
  expect_true(all(sqrt(rowSums(out$centers[,1:2]^2))<=2+1e-8))
  expect_false(anyDuplicated(apply(out$centers,1,coxeter_key_3d))>0)
})

test_that("finite tilings and honeycombs emit through the geodesic adapter", {
  for(what in c("edges","faces","cells")) {
    for(symbol in list(c(7,3),c(3,3),c(4,4),c(5,3,4),c(3,3,3))) {
      layer <- if(length(symbol)==2L) stat_tiling_3d(schlafli=symbol,radius=0,what=what) else stat_honeycomb_3d(schlafli=symbol,radius=0,what=what)
      p <- ggplot2::ggplot()+layer+coord_3d(axes=character(),panes=character(),box="none")
      packet <- suppressMessages(stats_scene(p)$packet)
      expect_silent(validate_scene_packet(packet))
      if(what=="edges") expect_gt(length(packet$polylines$offset),0) else expect_gt(length(packet$faces$offset),0)
    }
  }
  p <- ggplot2::ggplot()+stat_tiling_3d(schlafli=c(7,3),radius=0,model="poincare")+coord_3d()
  packet <- suppressMessages(stats_scene(p)$packet)
  expect_true(any(packet$polylines$count>2L))
})

test_that("filled tiling faces draw only polygon boundaries unless wireframe is requested", {
  p <- ggplot2::ggplot()+stat_tiling_3d(schlafli=c(7,3),radius=0,what="faces",colour="black")+coord_3d()
  packet <- suppressMessages(stats_scene(p)$packet)
  expect_equal(length(packet$polylines$offset),7L)
  expect_gt(length(packet$faces$offset),0L)
})
test_that("layer-owned canonical spaces remunch against the final viewport", {
  p <- ggplot2::ggplot()+stat_tiling_3d(schlafli=c(7,3),radius=0,model="poincare")+
    coord_3d(axes=character(),panes=character(),box="none")
  collected <- suppressMessages(integration_packets(p))
  packet <- collected$grobs[[1L]]$packet
  expect_true(inherits(attr(packet,"space_recipe")$panel_params$space_context$space,"SpaceCoxeter3D"))
  viewport <- new_scene_viewport(100,100)
  top <- suppressMessages(resolve_space_packets_3d(list(packet),new_camera_3d(azimuth=17,elevation=90),viewport))[[1L]]
  side <- suppressMessages(resolve_space_packets_3d(list(packet),new_camera_3d(azimuth=17,elevation=0),viewport))[[1L]]
  expect_gt(length(top$vertices$x),length(side$vertices$x))
  expect_silent(validate_scene_packet(top));expect_silent(validate_scene_packet(side))
})