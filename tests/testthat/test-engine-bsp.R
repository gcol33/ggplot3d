engine_packet <- function(polygons, opacity = 1, layers = seq_along(polygons)) {
  xyz <- do.call(rbind, polygons)
  counts <- vapply(polygons, nrow, integer(1))
  ggplot3d:::new_scene_packet(
    vertices = list(x = xyz[, 1], y = xyz[, 2], z = xyz[, 3]),
    corners = list(vertex = seq_len(nrow(xyz))),
    faces = list(offset = cumsum(c(1L, head(counts, -1))), count = counts,
                 layer_id = layers, opacity = rep_len(opacity, length(polygons))),
    styles = data.frame(style_id = 1L))
}

engine_compile <- function(packet, camera = ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90),
                           width = 100, height = 100) {
  ggplot3d:::scene_compile(packet, camera, ggplot3d:::new_scene_viewport(width, height))
}

engine_quad <- function(z) cbind(c(0, 1, 1, 0), c(0, 0, 1, 1), z)

engine_weights <- function(result, packet) {
  w <- result$weights
  expect_equal(as.numeric(rowsum(w$weight, w$vertex, reorder = FALSE)),
               rep(1, length(result$vertices$x)), tolerance = 2e-13)
  for (axis in c("x", "y", "z")) {
    original <- packet$vertices[[axis]][packet$corners$vertex[w$corner]]
    expect_equal(as.numeric(rowsum(w$weight * original, w$vertex, reorder = FALSE)),
                 result$vertices[[axis]], tolerance = 2e-13)
  }
}

engine_contains <- function(xy, q) {
  j <- c(seq.int(2L, nrow(xy)), 1L)
  area <- (xy[j, 1] - xy[, 1]) * (q[2] - xy[, 2]) -
    (xy[j, 2] - xy[, 2]) * (q[1] - xy[, 1])
  all(area > -1e-10) || all(area < 1e-10)
}

engine_depth_at <- function(xyz, q) {
  for (i in seq.int(2L, nrow(xyz) - 1L)) {
    triangle <- xyz[c(1L, i, i + 1L), , drop = FALSE]
    a <- cbind(1, triangle[, 1:2])
    if (abs(det(a)) > 1e-13) return(sum(c(1, q) * solve(a, triangle[, 3])))
  }
  NA_real_
}

engine_check_top_order <- function(packet, result, samples) {
  for (q in split(samples, row(samples))) {
    expected <- integer()
    depth <- numeric()
    for (i in seq_along(packet$faces$count)) {
      corners <- seq.int(packet$faces$offset[i], length.out = packet$faces$count[i])
      v <- packet$corners$vertex[corners]
      xyz <- cbind(packet$vertices$x[v], packet$vertices$y[v], packet$vertices$z[v])
      if (engine_contains(xyz[, 1:2], q)) {
        expected <- c(expected, packet$faces$primitive_id[i])
        depth <- c(depth, engine_depth_at(xyz, q))
      }
    }
    expected <- expected[order(depth)]
    observed <- integer()
    for (i in result$draw_order) {
      f <- result$fragments
      v <- seq.int(f$offset[i], length.out = f$count[i])
      xy <- cbind(result$vertices$x[v], result$vertices$y[v])
      if (engine_contains(xy, q)) observed <- c(observed, f$parent_primitive_id[i])
    }
    expect_identical(observed, expected, info = paste(q, collapse = ","))
  }
}

test_that("BSP splits crossing translucent faces and preserves affine provenance", {
  packet <- engine_packet(list(engine_quad(c(.2, .8, .8, .2)),
                               engine_quad(c(.8, .2, .2, .8))), opacity = .4)
  result <- engine_compile(packet)
  expect_identical(result$capabilities$visibility, "bsp")
  expect_gt(length(result$draw_order), 2L)
  engine_check_top_order(packet, result, as.matrix(expand.grid(c(.13, .39, .61, .87), c(.17, .71))))
  engine_weights(result, packet)
  packet$faces$layer_id <- rev(packet$faces$layer_id)
  reversed <- engine_compile(packet)
  engine_check_top_order(packet, reversed, as.matrix(expand.grid(c(.13, .39, .61, .87), c(.17, .71))))
  expect_identical(result$vertices, reversed$vertices)
  expect_identical(engine_compile(packet), reversed)
  packet$faces <- lapply(packet$faces,rev)
  engine_check_top_order(packet,engine_compile(packet),
    as.matrix(expand.grid(c(.13,.39,.61,.87),c(.17,.71))))
})

test_that("cyclic strips satisfy every local depth order after splitting", {
  a <- cbind(c(.1,.9,.9,.1), c(.15,.15,.25,.25), c(.2,.8,.8,.2))
  b <- cbind(c(.7,.8,.4,.3), c(.1,.2,.9,.8), c(.2,.2,.8,.8))
  c <- cbind(c(.1,.2,.7,.6), c(.1,.1,.9,.9), c(.8,.8,.2,.2))
  packet <- engine_packet(list(a,b,c), opacity = c(.2,.5,.7))
  result <- engine_compile(packet)
  expect_length(result$draw_order,4L)
  samples <- as.matrix(expand.grid(seq(.123,.887,length.out=19), seq(.137,.873,length.out=17)))
  engine_check_top_order(packet,result,samples)
  engine_weights(result,packet)
})

test_that("schema tolerance controls near-coplanar ties without moving geometry", {
  tolerance <- 64*.Machine$double.eps
  for (delta in c(0, tolerance/2, tolerance*4)) {
    packet <- engine_packet(list(engine_quad(rep(.5,4)), engine_quad(rep(.5+delta,4))), layers=c(2L,1L))
    result <- engine_compile(packet)
    expected <- if (delta <= tolerance) c(2L,1L) else c(1L,2L)
    expect_identical(result$fragments$parent_primitive_id[result$draw_order],expected)
    expect_identical(result$vertices$z,packet$vertices$z)
  }
})

test_that("line plane cuts preserve dash distance and endpoint flags", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(0,1,1,0,.2,.8), y=c(0,0,1,1,.5,.5), z=c(.5,.5,.5,.5,.2,.8)),
    corners=list(vertex=1:6), faces=list(offset=1L,count=4L,opacity=.5),
    polylines=list(offset=5L,arrow_start=TRUE,arrow_end=TRUE), styles=data.frame(style_id=1L))
  result <- engine_compile(packet)
  f <- result$fragments; line <- which(f$kind==2L)
  expect_length(line,2L)
  line <- line[order(f$path_start_mm[line])]
  expect_equal(f$path_end_mm[line[1]],f$path_start_mm[line[2]])
  expect_identical(f$arrow_start[line],c(TRUE,FALSE))
  expect_identical(f$arrow_end[line],c(FALSE,TRUE))
  expect_identical(f$kind[result$draw_order],c(2L,1L,2L))
  engine_weights(result,packet)
})

test_that("marker masks partition physical coverage and preserve the anchor", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(0,1,1,0,.5),y=c(0,0,1,1,.5),z=c(.2,.8,.8,.2,.5)),
    corners=list(vertex=1:5),faces=list(offset=1L,count=4L,opacity=.5),
    markers=list(offset=5L,footprint_id=1L,opacity=.3),
    footprints=list(kind=1L,offset=1L,count=0L,radius_x_mm=10,radius_y_mm=5,angle=30),
    styles=data.frame(style_id=1L))
  result <- engine_compile(packet)
  m <- which(result$fragments$kind==3L)
  expect_gte(length(m),2L)
  expect_true(all(result$fragments$count[m]==1L))
  expect_true(all(result$fragments$mask_count[m]>=3L))
  v <- result$fragments$offset[m]
  expect_equal(result$vertices$x[v],rep(.5,length(m)))
  for (q in list(c(.47,.51),c(.53,.51))) {
    hit <- vapply(m,function(i) {
      j <- seq.int(result$fragments$mask_offset[i],length.out=result$fragments$mask_count[i])
      engine_contains(cbind(result$masks$x_npc[j],result$masks$y_npc[j]),q)
    },logical(1))
    expect_equal(sum(hit),1L)
  }
  engine_weights(result,packet)
})

test_that("text is hidden as a whole only by opaque coverage at its anchor", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(0,1,1,0,.5),y=c(0,0,1,1,.5),z=c(.6,.6,.6,.6,.4)),
    corners=list(vertex=1:5),faces=list(offset=1L,count=4L),
    text=list(offset=5L,width_mm=20,height_mm=5),styles=data.frame(style_id=1L))
  expect_false(any(engine_compile(packet)$fragments$kind==4L))
  packet$faces$opacity <- .5
  expect_true(any(engine_compile(packet)$fragments$kind==4L))
})

test_that("eligible text retains one anchor depth across disjoint compositing masks", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(0,1,1,0,.5),y=c(0,0,1,1,.5),z=c(.2,.8,.8,.2,.5)),
    corners=list(vertex=1:5),faces=list(offset=1L,count=4L,opacity=.5),
    text=list(offset=5L,width_mm=30,height_mm=6),styles=data.frame(style_id=1L))
  result <- engine_compile(packet)
  text <- which(result$fragments$kind==4L)
  expect_gte(length(text),2L)
  expect_true(all(result$fragments$count[text]==1L))
  expect_true(all(result$fragments$mask_count[text]>=3L))
  expect_equal(result$fragments$depth_min[text],result$fragments$depth_max[text])
  engine_weights(result,packet)
})

test_that("native malformed and degenerate inputs fail without entering partition loops", {
  packet <- engine_packet(list(engine_quad(rep(.5,4))))
  native <- function(x) ggplot3d:::scene_compile_cpp(x[names(x)!="styles"],
    ggplot3d:::new_camera_3d(),ggplot3d:::new_scene_viewport(100,100))
  broken <- packet; broken$faces$offset <- 0L
  expect_error(native(broken),"corner range")
  broken <- packet; broken$vertices$x[1] <- NaN
  expect_error(native(broken),"non-finite")
  broken <- packet; broken$vertices$y[] <- 0
  expect_error(native(broken),"degenerate")
  broken <- packet; broken$vertices$z[1] <- .7
  expect_error(native(broken),"planar")
  camera <- ggplot3d:::new_camera_3d()
  camera$zoom <- matrix(1,1,1)
  expect_error(ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],camera,
    ggplot3d:::new_scene_viewport(100,100)),"scalar")
})

test_that("geometric tolerance is a distance for small valid faces", {
  packet <- engine_packet(list(cbind(c(.5,.5+1e-8,.5),c(.5,.5,.5+1e-8),rep(.5,3))))
  result <- engine_compile(packet)
  expect_length(result$draw_order,1L)
  expect_equal(sum(result$fragments$normal_z^2),1)
  engine_weights(result,packet)
})

test_that("opaque silhouettes preserve whole hidden lines and subdivide retained policies", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(.4,.6,.6,.4,.1,.9),y=c(.2,.2,.8,.8,.5,.5),z=c(.6,.6,.6,.6,.4,.4)),
    corners=list(vertex=1:6),faces=list(offset=1L,count=4L),
    polylines=list(offset=5L,arrow_start=TRUE,arrow_end=TRUE),styles=data.frame(style_id=1L))
  for (width in c(0,4)) {
    packet$polylines$width_mm <- width
    for (policy in 0:2) {
      packet$polylines$hidden <- as.integer(policy)
      result <- engine_compile(packet)
      f <- result$fragments; line <- which(f$kind==2L)
      if(policy==0L) {
        expect_length(line,1L)
        expect_identical(f$kind[result$draw_order],c(2L,1L))
        expect_identical(f$mask_count[line],0L)
      } else expect_gte(length(line),2L)
      expect_identical(any(f$hidden),policy>0L)
      expect_equal(sum(f$arrow_start),1L)
      expect_equal(sum(f$arrow_end),1L)
      if(width>0 && policy>0L) expect_true(all(f$mask_count[line]>=3L))
      expect_true(all(f$normal_x[line]==0 & f$normal_y[line]==0 & f$normal_z[line]==0))
      if(policy>0) expect_true(all(match(which(f$hidden),result$draw_order) >
        match(which(f$kind==1L),result$draw_order)))
      engine_weights(result,packet)
    }
  }
})

test_that("projection matrices reproduce native coordinates including poles and inside eyes", {
  packet <- engine_packet(list(engine_quad(c(.2,.8,.8,.2))))
  for (mode in c("orthographic","perspective")) for (el in c(-90,0,90)) {
    camera <- ggplot3d:::new_camera_3d(projection=mode,azimuth=37,elevation=el,roll=23,
      aspect=c(2,.5,1),distance=if(mode=="perspective") .25 else NULL,
      near=.03,far=4,clip_data=FALSE,clip=FALSE)
    result <- engine_compile(packet,camera,170,90)
    v <- result$vertices
    model <- rbind(v$x*result$camera$model_scale[1],v$y*result$camera$model_scale[2],
                   v$z*result$camera$model_scale[3],1)
    projected <- result$camera$projection %*% result$camera$view %*% model
    expect_equal(as.numeric((projected[1,]/projected[4,]+1)/2),v$x_npc,tolerance=1e-12)
    expect_equal(as.numeric((projected[2,]/projected[4,]+1)/2),v$y_npc,tolerance=1e-12)
    expect_true(all(v$depth>=.03-1e-13 & v$depth<=4+1e-13))
    engine_weights(result,packet)
  }
})

test_that("random intersecting triangles agree with an independent planar depth oracle", {
  set.seed(306)
  polygons <- replicate(25,cbind(matrix(runif(6,.05,.95),3,2),runif(3,.1,.9)),simplify=FALSE)
  packet <- engine_packet(polygons,opacity=.4)
  result <- engine_compile(packet)
  engine_check_top_order(packet,result,as.matrix(expand.grid(seq(.071,.923,length.out=17),
                                                           seq(.083,.917,length.out=13))))
  engine_weights(result,packet)
})

engine_cross <- function(a,b) c(a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1])

engine_ray_triangle <- function(origin,direction,triangle) {
  a <- triangle[1,]; e1 <- triangle[2,]-a; e2 <- triangle[3,]-a
  p <- engine_cross(direction,e2)
  determinant <- sum(e1*p)
  if(abs(determinant)<1e-12) return(NA_real_)
  s <- origin-a
  u <- sum(s*p)/determinant
  q <- engine_cross(s,e1)
  v <- sum(direction*q)/determinant
  if(u< -1e-10 || v< -1e-10 || u+v>1+1e-10) return(NA_real_)
  sum(e2*q)/determinant
}

engine_ray_polygon <- function(origin,direction,polygon) {
  for(i in seq.int(2L,nrow(polygon)-1L)) {
    hit <- engine_ray_triangle(origin,direction,polygon[c(1L,i,i+1L),,drop=FALSE])
    if(!is.na(hit)) return(hit)
  }
  NA_real_
}

test_that("BSP face compositing order agrees with independent rays in both projections", {
  set.seed(79)
  polygons <- replicate(18,matrix(runif(9,.05,.95),3,3),simplify=FALSE)
  packet <- engine_packet(polygons,opacity=seq(.1,.9,length.out=18))
  samples <- as.matrix(expand.grid(seq(.137,.863,length.out=11),seq(.151,.849,length.out=9)))
  for(mode in c("orthographic","perspective")) for(distance in c(.3,3)) {
    camera <- ggplot3d:::new_camera_3d(projection=mode,azimuth=31,elevation=23,roll=17,
      aspect=c(1,.6,.8),distance=if(mode=="perspective") distance else NULL,
      near=.04,far=8,clip=FALSE)
    result <- engine_compile(packet,camera,140,90)
    c <- result$camera
    source <- lapply(polygons,function(x) sweep(x,2,c$model_scale,`*`))
    for(q in split(samples,row(samples))) {
      offset <- c$right*(2*q[1]-1)/c$projection[1,1]+c$up*(2*q[2]-1)/c$projection[2,2]
      origin <- c$eye + if(mode=="orthographic") offset else 0
      direction <- c$forward + if(mode=="perspective") offset else 0
      depths <- vapply(source,function(x) engine_ray_polygon(origin,direction,x),numeric(1))
      eligible <- which(!is.na(depths) & depths>=c$near & depths<=c$far)
      expected <- as.integer(eligible[order(depths[eligible],decreasing=TRUE)])
      observed <- integer()
      for(i in result$draw_order) {
        v <- seq.int(result$fragments$offset[i],length.out=result$fragments$count[i])
        xyz <- cbind(result$vertices$x[v],result$vertices$y[v],result$vertices$z[v])
        hit <- engine_ray_polygon(origin,direction,sweep(xyz,2,c$model_scale,`*`))
        if(!is.na(hit)) observed <- c(observed,result$fragments$parent_primitive_id[i])
      }
      expect_identical(observed,expected,info=paste(mode,distance,paste(q,collapse=",")))
    }
    engine_weights(result,packet)
  }
})
engine_grid <- function(n = 200L) {
  nx <- ceiling(sqrt(n / 2)); ny <- ceiling(n / (2 * nx))
  cell <- rep(seq_len(nx * ny) - 1L, each = 2L)[seq_len(n)]
  odd <- seq_len(n) %% 2L == 1L
  x <- cbind(cell %% nx, cell %% nx + 1, cell %% nx + as.integer(odd)) / nx
  y <- cbind(cell %/% nx, cell %/% nx + as.integer(!odd), cell %/% nx + 1) / ny
  z <- .5 + .1 * sin(4 * pi * x) * cos(4 * pi * y)
  xyz <- cbind(as.vector(t(x)), as.vector(t(y)), as.vector(t(z)))
  ggplot3d:::new_scene_packet(vertices = list(x=xyz[,1],y=xyz[,2],z=xyz[,3]),
    corners=list(vertex=seq_len(3*n)),faces=list(offset=seq.int(1L,by=3L,length.out=n)),
    styles=data.frame(style_id=1L))
}

test_that("acyclic grids retain one fragment per input triangle across cameras", {
  packet <- engine_grid(2000L)
  for(mode in c("orthographic","perspective")) for(el in c(-90,-30,0,30,90)) {
    camera <- ggplot3d:::new_camera_3d(projection=mode,azimuth=37,elevation=el,roll=19)
    result <- engine_compile(packet,camera)
    expect_length(result$draw_order,2000L)
    expect_identical(sort(result$fragments$parent_primitive_id),seq_len(2000L))
    expect_false(any(result$fragments$clipped))
  }
})

test_that("two crossing quads require just one cut", {
  packet <- engine_packet(list(engine_quad(c(.2,.8,.8,.2)),engine_quad(c(.8,.2,.2,.8))),.4)
  result <- engine_compile(packet)
  expect_length(result$draw_order,3L)
  engine_check_top_order(packet,result,as.matrix(expand.grid(c(.2,.8),c(.3,.7))))
})

test_that("the resource budget rejects growth with actionable scene counts", {
  packet <- engine_grid(100L)
  view <- ggplot3d:::new_scene_viewport(100,100)
  attr(view,"engine_budget_bytes") <- 100000
  native <- function() ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"],
    ggplot3d:::new_camera_3d(),view)
  expect_error(native(),"100 input primitives.*intersecting pairs.*fragments.*raster backend")
  attr(view,"engine_budget_bytes") <- -1
  expect_error(native(),"positive finite")
})

test_that("the compiled scene does not depend on the thread count", {
  packet <- engine_grid(1500L)
  camera <- ggplot3d:::new_camera_3d(azimuth = 30, elevation = 20)
  old <- options(ggplot3d.threads = 1L, ggplot3d.cache_bytes = 0); on.exit(options(old))
  serial <- engine_compile(packet, camera)
  options(ggplot3d.threads = 3L)
  expect_identical(engine_compile(packet, camera), serial)
  options(ggplot3d.threads = 0)
  expect_error(engine_compile(packet, camera), "positive whole number")
  view <- ggplot3d:::new_scene_viewport(100,100)
  attr(view,"engine_threads") <- 1.5
  expect_error(ggplot3d:::scene_compile_cpp(packet[names(packet)!="styles"], camera, view),
               "positive whole number")
})

test_that("repeated compiles reuse the cached scene within the byte limit", {
  packet <- engine_grid(200L)
  camera <- ggplot3d:::new_camera_3d(azimuth = 30, elevation = 20)
  old <- options(ggplot3d.cache_bytes = 256 * 2^20); on.exit(options(old))
  first <- engine_compile(packet, camera)
  testthat::local_mocked_bindings(scene_compile_cpp = function(...) stop("engine called"),
                                  .package = "ggplot3d")
  expect_identical(engine_compile(packet, camera), first)
  expect_error(engine_compile(packet, camera, width = 120), "engine called")
  moved <- packet; moved$vertices$z[1] <- moved$vertices$z[1] + .01
  expect_error(engine_compile(moved, camera), "engine called")
  options(ggplot3d.cache_bytes = 0)
  expect_error(engine_compile(packet, camera), "engine called")
  options(ggplot3d.cache_bytes = -1)
  expect_error(engine_compile(packet, camera), "non-negative")
})

test_that("ellipse and holed polygon footprints reject empty carrier intersections", {
  for(kind in 1:2) {
    packet <- ggplot3d:::new_scene_packet(
      vertices=list(x=c(.64,.66,.66,.64,.5),y=c(.64,.64,.66,.66,.5),z=c(.3,.7,.7,.3,.5)),
      corners=list(vertex=1:5),faces=list(offset=1L,count=4L,opacity=.5),
      markers=list(offset=5L,footprint_id=1L,opacity=.5),
      footprints=list(kind=as.integer(kind),offset=1L,count=if(kind==1) 0L else 8L,
        radius_x_mm=10,radius_y_mm=10,angle=0),
      footprint_vertices=if(kind==1) list() else list(x_mm=c(-10,10,10,-10,-9,9,9,-9),
        y_mm=c(-10,-10,10,10,-9,-9,9,9),ring=rep(1:2,each=4)),
      styles=data.frame(style_id=1L))
    if(kind==2) {packet$vertices$x[1:4]<-c(.52,.54,.54,.52);packet$vertices$y[1:4]<-c(.52,.52,.54,.54)}
    result <- engine_compile(packet)
    expect_length(result$draw_order,2L)
    expect_false(any(result$fragments$clipped))
  }
})

test_that("constant-depth joined strokes replay whole paths through disjoint masks", {
  for(join in 0:2) for(cap in 0:2) {
    packet <- ggplot3d:::new_scene_packet(
      vertices=list(x=c(0,1,1,0,.2,.8,.2,.8),y=c(0,0,1,1,.2,.8,.8,.2),
        z=c(.2,.8,.8,.2,rep(.5,4))),corners=list(vertex=1:8),
      faces=list(offset=1L,count=4L,opacity=.4),
      polylines=list(offset=5L,count=4L,width_mm=8,join=as.integer(join),cap=as.integer(cap),
        opacity=.5,arrow_start=TRUE,arrow_end=TRUE),styles=data.frame(style_id=1L))
    result <- engine_compile(packet)
    f <- result$fragments;line <- which(f$kind==2L)
    expect_true(all(f$count[line]==4L))
    expect_equal(sum(f$arrow_start[line]),1L)
    expect_equal(sum(f$arrow_end[line]),1L)
    expect_equal(unique(f$path_start_mm[line]),0)
    expect_length(unique(f$path_end_mm[line]),1L)
    for(q in list(c(.31,.31),c(.69,.69),c(.31,.8),c(.69,.8),c(.49,.51))) {
      # These source-path points are inside the painted stroke for every cap and join.
      point <- .5+(q-.5)/sqrt(3)
      hits <- vapply(line,function(i) {
        m <- seq.int(f$mask_offset[i],length.out=f$mask_count[i])
        engine_contains(cbind(result$masks$x_npc[m],result$masks$y_npc[m]),point)
      },logical(1))
      expect_equal(sum(hits),1L)
      order <- result$draw_order[result$draw_order %in% c(which(f$kind==1L),line[hits])]
      depth <- .2+.6*q[1]
      expect_identical(tail(f$kind[order],1L),if(depth>.5) 1L else 2L)
    }
    engine_weights(result,packet)
  }
})

test_that("retained hidden strokes composite between their opaque blocker and nearer transparency", {
  for(mode in c("orthographic","perspective")) for(width in c(0,4)) for(policy in 1:2) {
    packet <- ggplot3d:::new_scene_packet(
      vertices=list(x=c(0,1,1,0,0,1,1,0,.2,.8),y=c(0,0,1,1,0,0,1,1,.5,.5),
        z=c(rep(.6,4),rep(.8,4),.3,.3)),corners=list(vertex=1:10),
      faces=list(offset=c(1L,5L),count=c(4L,4L),opacity=c(1,.5)),
      polylines=list(offset=9L,width_mm=width,hidden=as.integer(policy),opacity=.5),
      styles=data.frame(style_id=1L))
    result <- engine_compile(packet,ggplot3d:::new_camera_3d(projection=mode,azimuth=-90,elevation=90))
    f <- result$fragments
    expect_identical(f$parent_primitive_id[result$draw_order],c(1L,3L,2L))
    expect_identical(f$hidden[result$draw_order],c(FALSE,TRUE,FALSE))
    line <- which(f$kind==2L);v <- seq.int(f$offset[line],length.out=f$count[line])
    expect_equal(result$vertices$z[v],c(.3,.3))
    colours <- list(c(1,0,0),c(0,0,1),c(0,1,0))
    composite <- c(1,1,1)
    for(i in result$draw_order) composite <- f$opacity[i]*colours[[f$parent_primitive_id[i]]] + (1-f$opacity[i])*composite
    expect_equal(composite,c(.25,.25,.5))
    engine_weights(result,packet)
  }
})

test_that("disjoint coplanar components obey role and layer tie ordering", {
  packet <- engine_packet(list(cbind(c(.1,.2,.1),c(.1,.1,.2),.5),
    cbind(c(.7,.8,.7),c(.7,.7,.8),.5),cbind(c(.4,.5,.4),c(.4,.4,.5),.5)),layers=c(3L,2L,1L))
  packet$faces$role <- c(0L,1L,0L)
  result <- engine_compile(packet)
  expect_identical(result$fragments$parent_primitive_id[result$draw_order],c(3L,1L,2L))
})

test_that("a cut through an original stroke endpoint assigns its arrow once", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(0,1,1,0,.5,.8),y=c(0,0,1,1,.5,.5),z=c(.2,.8,.8,.2,.5,.5)),
    corners=list(vertex=1:6),faces=list(offset=1L,count=4L,opacity=.5),
    polylines=list(offset=5L,width_mm=4,cap=1L,arrow_start=TRUE,arrow_end=TRUE),styles=data.frame(style_id=1L))
  result <- engine_compile(packet)
  expect_equal(sum(result$fragments$arrow_start),1L)
  expect_equal(sum(result$fragments$arrow_end),1L)
})

test_that("hidden stroke depth follows the nearest of crossing opaque blockers", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(rep(c(0,1,1,0),3),.1,.9),y=c(rep(c(0,0,1,1),3),.5,.5),
      z=c(.4,.8,.8,.4,.8,.4,.4,.8,rep(.9,4),.2,.2)),corners=list(vertex=1:14),
    faces=list(offset=c(1L,5L,9L),count=rep(4L,3),opacity=c(1,1,.5)),
    polylines=list(offset=13L,width_mm=4,hidden=2L,opacity=.5),styles=data.frame(style_id=1L))
  result <- engine_compile(packet);f <- result$fragments
  for(x in c(.21,.37,.63,.79)) {
    q <- c(x,.5);point <- .5+(q-.5)/sqrt(3)
    hit <- vapply(seq_along(f$count),function(i) {
      if(f$kind[i]==1L) {
        v <- seq.int(f$offset[i],length.out=f$count[i])
        engine_contains(cbind(result$vertices$x[v],result$vertices$y[v]),q)
      } else {
        m <- seq.int(f$mask_offset[i],length.out=f$mask_count[i])
        engine_contains(cbind(result$masks$x_npc[m],result$masks$y_npc[m]),point)
      }
    },logical(1))
    observed <- f$parent_primitive_id[result$draw_order[hit[result$draw_order]]]
    expect_identical(observed,c(if(x<.5) c(1L,2L) else c(2L,1L),4L,3L))
  }
  engine_weights(result,packet)
})


test_that("uncut markers have no masks with other markers or a noncrossing face", {
  for(surface in c(FALSE,TRUE)) {
    packet <- ggplot3d:::new_scene_packet(
      vertices=list(x=c(0,1,1,0,.49,.51),y=c(0,0,1,1,.5,.5),z=c(rep(.4,4),.3,.7)),
      corners=list(vertex=1:6),faces=if(surface) list(offset=1L,count=4L) else list(),
      markers=list(offset=5:6,footprint_id=c(1L,1L),opacity=c(.4,.6)),
      footprints=list(kind=1L,offset=1L,count=0L,radius_x_mm=10,radius_y_mm=6,angle=27),
      styles=data.frame(style_id=1L))
    result <- engine_compile(packet)
    marker <- result$fragments$kind==3L
    expect_equal(sum(marker),2L)
    expect_true(all(result$fragments$mask_count[marker]==0L))
    expect_identical(result$fragments$source_index[marker],1:2)
    engine_weights(result,packet)
  }
})

test_that("underlays and overlays keep supplied order and complete paths", {
  for(partition in c(0L,2L)) {
    packet <- ggplot3d:::new_scene_packet(
      vertices=list(x=c(0,1,1,0,.1,.5,.9,.9,.5,.1),
        y=c(0,0,1,1,.3,.7,.3,.7,.3,.7),z=c(rep(.5,4),.2,.8,.2,.8,.2,.8)),
      corners=list(vertex=1:10),faces=list(offset=1L,count=4L),
      polylines=list(offset=c(5L,8L),count=c(3L,3L),width_mm=c(2,2),
        source_row=c(10L,1L),partition=rep(partition,2)),styles=data.frame(style_id=1L))
    result <- engine_compile(packet)
    f <- result$fragments; line <- which(f$kind==2L)
    expect_identical(f$count[line],c(3L,3L))
    expect_identical(f$mask_count[line],c(0L,0L))
    expect_identical(f$source_index[intersect(result$draw_order,line)],1:2)
    expect_identical(f$kind[result$draw_order],if(partition==0L) c(2L,2L,1L) else c(1L,2L,2L))
    engine_weights(result,packet)
  }
})

test_that("rear depth strokes remain whole over a triangulated surface", {
  packet <- engine_grid(2000L)
  xyz <- rbind(c(.1,.5,.1),c(.9,.5,.1))
  line <- ggplot3d:::new_scene_packet(vertices=list(x=xyz[,1],y=xyz[,2],z=xyz[,3]),
    corners=list(vertex=1:2),polylines=list(offset=1L,width_mm=1),styles=data.frame(style_id=1L))
  packet <- ggplot3d:::merge_scene_packets(list(packet,line),1L)
  result <- engine_compile(packet)
  f <- result$fragments; strokes <- which(f$kind==2L)
  expect_length(strokes,1L)
  expect_identical(f$mask_count[strokes],0L)
  expect_equal(sum(f$kind==1L),2000L)
})

test_that("face-plane crossings outside screen overlap do not split a stroke", {
  packet <- ggplot3d:::new_scene_packet(
    vertices=list(x=c(.7,.9,.9,.7,.1,.9),y=c(.3,.3,.7,.7,.5,.5),
      z=c(rep(.5,4),.1,.9)),corners=list(vertex=1:6),faces=list(offset=1L,count=4L),
    polylines=list(offset=5L,width_mm=1),styles=data.frame(style_id=1L))
  result <- engine_compile(packet)
  expect_length(result$draw_order,2L)
  expect_true(all(result$fragments$mask_count==0L))
})

test_that("pole-aligned strokes have finite geometry in every partition", {
  for(mode in c("orthographic","perspective")) for(elevation in c(-90,90))
    for(partition in 0:2) {
      packet <- ggplot3d:::new_scene_packet(vertices=list(x=c(1,1),y=c(1,1),z=c(0,1)),
        corners=list(vertex=1:2),polylines=list(offset=1L,width_mm=.5,partition=as.integer(partition)),
        styles=data.frame(style_id=1L))
      camera <- ggplot3d:::new_camera_3d(projection=mode,azimuth=-60,elevation=elevation)
      result <- engine_compile(packet,camera,141.6143,141.6143)
      expect_true(all(is.finite(unlist(result$vertices))))
      expect_true(all(is.finite(unlist(result$masks))))
      engine_weights(result,packet)
    }
})

test_that("variable-depth endpoint caps agree with the independent ray oracle", {
  reference <- new.env(parent=environment())
  source(testthat::test_path("..","fixtures","oracle","reference.R"),local=reference)
  source(testthat::test_path("..","fixtures","scenes","builders.R"),local=reference)
  for(mode in c("orthographic","perspective")) for(cap in 1:2) {
    packet <- reference$ref_packet(list(reference$ref_quad(.65)),
      lines=list(rbind(c(.2,.5,.3),c(.8,.5,.7))),face_alpha=.4,line_width=10)
    packet$polylines$cap <- as.integer(cap)
    packet$polylines$opacity <- .5
    camera <- ggplot3d:::new_camera_3d(projection=mode,azimuth=-90,elevation=90)
    viewport <- ggplot3d:::new_scene_viewport(100,100)
    result <- ggplot3d:::scene_compile(packet,camera,viewport)
    score <- reference$ref_score(packet,camera,viewport,result,100L)
    expect_equal(score$wrong,0L,info=paste(mode,cap))
    engine_weights(result,packet)
  }
})

test_that("uninterrupted variable-depth cap sectors replay as one unmasked stroke", {
  for(cap in 1:2) {
    packet <- ggplot3d:::new_scene_packet(vertices=list(x=c(.2,.8),y=c(.5,.5),z=c(.2,.8)),
      corners=list(vertex=1:2),polylines=list(offset=1L,width_mm=8,cap=as.integer(cap),opacity=.5,
        arrow_start=TRUE,arrow_end=TRUE),styles=data.frame(style_id=1L))
    result <- engine_compile(packet)
    expect_length(result$draw_order,1L)
    expect_identical(result$fragments$mask_count,0L)
    expect_false(result$fragments$clipped)
    expect_true(result$fragments$arrow_start && result$fragments$arrow_end)
    engine_weights(result,packet)
  }
})
