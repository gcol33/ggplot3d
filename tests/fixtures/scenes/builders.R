ref_packet <- function(faces=list(), lines=list(), markers=list(), layers=NULL,
                       face_alpha=1, line_width=1.2, marker_radius=2.2) {
  geometry <- c(faces,lines,markers)
  xyz <- do.call(rbind,geometry)
  counts <- vapply(geometry,nrow,integer(1))
  offsets <- cumsum(c(1L,head(counts,-1L)))
  nf <- length(faces); nl <- length(lines); nm <- length(markers)
  if(is.null(layers)) layers <- rep(1L,length(geometry))
  table <- function(index) list(offset=offsets[index],count=counts[index],layer_id=layers[index])
  face <- table(seq_len(nf)); face$opacity <- rep_len(face_alpha,nf)
  line <- table(nf+seq_len(nl)); line$width_mm <- rep_len(line_width,nl)
  mark <- table(nf+nl+seq_len(nm)); mark$footprint_id <- rep(1L,nm)
  ggplot3d:::new_scene_packet(vertices=list(x=xyz[,1],y=xyz[,2],z=xyz[,3]),
    corners=list(vertex=seq_len(nrow(xyz))),faces=face,polylines=line,markers=mark,
    footprints=if(nm) list(kind=1L,offset=1L,count=0L,radius_x_mm=marker_radius,
                          radius_y_mm=marker_radius,angle=0) else list(),
    styles=data.frame(style_id=1L))
}

ref_quad <- function(z=.5, x=c(.1,.9,.9,.1), y=c(.1,.1,.9,.9)) cbind(x,y,rep_len(z,4L))

ref_surface <- function(fun,n=40L) {
  axis <- seq(-1,1,length.out=n)
  xy <- as.matrix(expand.grid(axis,axis))
  xyz <- cbind((xy+1)/2, .5+fun(xy[,1],xy[,2])/2)
  cells <- as.matrix(expand.grid(seq_len(n-1L),seq_len(n-1L)))
  lower <- cells[,1]+(cells[,2]-1L)*n
  triangles <- rbind(cbind(lower,lower+1L,lower+n+1L),cbind(lower,lower+n+1L,lower+n))
  lapply(seq_len(nrow(triangles)),function(i) xyz[triangles[i,],,drop=FALSE])
}

ref_sphere <- function(centre,radius,n=12L) {
  polar <- seq(0,pi,length.out=n+1L); azimuth <- seq(0,2*pi,length.out=2L*n+1L)
  grid <- as.matrix(expand.grid(polar,azimuth))
  xyz <- sweep(radius*cbind(sin(grid[,1])*cos(grid[,2]),sin(grid[,1])*sin(grid[,2]),cos(grid[,1])),2,centre,`+`)
  rows <- n+1L
  cells <- as.matrix(expand.grid(seq_len(n),seq_len(2L*n)))
  lower <- cells[,1]+(cells[,2]-1L)*rows
  triangles <- rbind(cbind(lower,lower+1L,lower+rows+1L),cbind(lower,lower+rows+1L,lower+rows))
  triangles <- triangles[c(cells[,1]!=n,cells[,1]!=1L),,drop=FALSE]
  lapply(seq_len(nrow(triangles)),function(i) xyz[triangles[i,],,drop=FALSE])
}

ref_box <- function(x,y,width=.2,height=1) {
  p <- as.matrix(expand.grid(x+c(-1,1)*width/2,y+c(-1,1)*width/2,c(0,height)))
  lapply(list(c(1,2,4,3),c(5,6,8,7),c(1,2,6,5),c(3,4,8,7),c(1,3,7,5),c(2,4,8,6)),
         function(i) p[i,,drop=FALSE])
}

ref_fixture_builders <- function() {
  a <- function(mode) {
    first <- ref_surface(function(x,y) x^2-y^2)
    second <- ref_surface(function(x,y) .5*x*y)
    layers <- if(mode=="grouped") rep(1L,length(first)+length(second)) else
      rep(if(mode=="reversed") c(2L,1L) else c(1L,2L),c(length(first),length(second)))
    ref_packet(c(first,second),layers=layers)
  }
  b <- function(reverse=FALSE) {
    points <- lapply(seq_len(6L),function(i) matrix(c(rep(c(.25,.5,.75),2)[i],.5,
      c(rep(.5+.35/1.3,3),rep(.5-.35/1.3,3))[i]),1,3))
    path <- cbind(seq(.1,.9,length.out=41),.55,seq(0,1,length.out=41))
    ref_packet(list(ref_quad(x=c(0,1,1,0),y=c(0,0,1,1))),list(path),points,
      layers=if(reverse) c(3L,1L,rep(2L,6)) else c(1L,3L,rep(2L,6)))
  }
  strip <- function(a,b) {
    d <- b-a; normal <- c(-d[2],d[1])/sqrt(sum(d*d))*.06
    cbind(rbind(a+normal,b+normal,b-normal,a-normal),c(.8,.2,.2,.8))
  }
  list(
    A_separate=function() a("separate"), A_reversed=function() a("reversed"),
    A_grouped=function() a("grouped"), B_surface_first=function() b(),
    B_surface_last=function() b(TRUE),
    C_columns=function() {
      positions <- as.matrix(expand.grid(c(.2,.5,.8),c(.2,.5,.8)))
      boxes <- unlist(lapply(seq_len(9),function(i) ref_box(positions[i,1],positions[i,2])),recursive=FALSE)
      x <- c(0,1,1,0); y <- c(0,0,1,1)
      plane <- ref_quad((.6+.35*(2*x-1)+.2*(2*y-1))/1.2,x=x,y=y)
      ref_packet(c(boxes,list(plane)),layers=c(rep(1L,length(boxes)),2L))
    },
    G1_crossing=function() ref_packet(list(ref_quad(c(.1,.9,.9,.1)),ref_quad(c(.9,.1,.1,.9)))),
    G2_cycle=function() ref_packet(list(strip(c(.2,.2),c(.8,.2)),strip(c(.8,.2),c(.5,.8)),strip(c(.5,.8),c(.2,.2)))),
    near_crossing=function() ref_packet(list(ref_quad(c(.1,.9,.9,.1)))),
    coplanar_layers=function() ref_packet(list(ref_quad(),ref_quad(x=c(.3,.95,.95,.3))),layers=c(1L,2L)),
    nested_isosurfaces=function() ref_packet(c(ref_sphere(c(.5,.5,.5),.42,8L),ref_sphere(c(.54,.47,.53),.22,8L)),face_alpha=.5),
    translucent_points=function() ref_packet(list(ref_quad(.7)),markers=list(matrix(c(.4,.5,.3),1,3),matrix(c(.6,.5,.9),1,3)),face_alpha=.4),
    thick_line=function() ref_packet(list(ref_quad()),list(rbind(c(.15,.5,.2),c(.85,.5,.8))),line_width=7),
    marker_silhouette=function() ref_packet(list(ref_quad()),markers=list(matrix(c(.1,.5,.35),1,3)),marker_radius=6),
    pole=function() ref_packet(list(ref_quad(c(.2,.8,.8,.2)),ref_quad(c(.8,.2,.2,.8))))
  )
}

ref_cameras <- function() list(
  oblique=ggplot3d:::new_camera_3d(azimuth=-60,elevation=30),
  perspective=ggplot3d:::new_camera_3d(projection="perspective",azimuth=35,elevation=25,roll=13),
  pole=ggplot3d:::new_camera_3d(azimuth=-90,elevation=90))

ref_fixture <- function(name,view) {
  packet <- ref_fixture_builders()[[name]]()
  camera <- ref_cameras()[[view]]
  zoom <- if(startsWith(name,"A_")) 2.5 else c(nested_isosurfaces=2)[name]
  if(!is.na(zoom)) camera$zoom <- unname(zoom)
  viewport <- ggplot3d:::new_scene_viewport(100,100,144)
  if(name=="near_crossing") camera$near <- ref_camera(camera,viewport)$distance
  list(packet=packet,camera=camera,viewport=viewport)
}
