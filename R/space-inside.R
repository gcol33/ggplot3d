#' @include coord-3d.R space-3d.R
NULL

# Target displacement sets the viewing direction while the eye stays fixed.
inside_camera_3d <- function(camera,origin=c(.5,.5,.5)) {
  camera$projection <- "perspective"; camera$distance <- 1
  basis <- frame_basis_3d(camera,new_scene_viewport(100,100))
  camera$target <- origin+camera$distance*basis$forward/basis$scale
  if(is.na(camera$near)) camera$near <- sqrt(.Machine$double.eps)
  if(is.na(camera$far)) camera$far <- sqrt(sum((pmax(origin,1-origin)*basis$scale)^2))+camera$near
  validate_camera_3d(camera)
  camera
}

inside_coord_3d <- function(coord) {
  parent <- coord
  coord <- ggplot2::ggproto(NULL,parent,
    panel_camera=function(self,index,count) inside_camera_3d(ggplot2::ggproto_parent(Coord3D,self)$panel_camera(index,count)),
    view_camera=function(self,view) inside_camera_3d(ggplot2::ggproto_parent(Coord3D,self)$view_camera(view)))
  coord$cameras <- lapply(coord$cameras,inside_camera_3d)
  coord$camera <- coord$cameras[[1L]]
  coord$config$frame <- "sphere"
  coord
}

spherical_observer_3d <- function(observer,space,input) {
  if(is.matrix(observer)) return(check_so4_3d(observer))
  if(!is.numeric(observer)||!length(observer)%in%c(3L,4L)||any(!is.finite(observer))) stop("observer must be an input-chart point or an SO(4) matrix.",call.=FALSE)
  data <- as.data.frame(as.list(stats::setNames(observer,c("x","y","z","w")[seq_along(observer)])))
  X <- drop(space$to_canonical(data,if(length(observer)==4L) "ambient" else input))
  G <- diag(4)
  for(j in 1:3) {
    R <- rotate_4d(paste0(c("x","y","z")[j],"w"),atan2(X[j],X[4L]))
    X <- drop(R%*%X); G <- R%*%G
  }
  G
}

check_inside_options_3d <- function(inside,fog,args) {
  if(!is.logical(inside)||length(inside)!=1L||is.na(inside)) stop("inside must be TRUE or FALSE.",call.=FALSE)
  if(!is.null(fog)) mesh_number_3d(fog,"fog",function(x) is.finite(x)&&x>0,"positive")
  if(inside) {
    if(!is.null(args$projection)&&args$projection!="perspective") stop("Inside views require perspective projection.",call.=FALSE)
    args$projection <- "perspective"
    args$fov <- args$fov %or3d% 90
    if(!is.null(args$aspect)&&(!is.numeric(args$aspect)||length(args$aspect)!=3L||any(!is.finite(args$aspect))||any(args$aspect<=0)||any(args$aspect!=args$aspect[1L]))) stop("Inside views require an isotropic model aspect.",call.=FALSE)
  }
  args
}

# Gnomonic box planes pass through the spherical origin, so their cuts
# preserve great-circle edges and totally geodesic face interiors.
spherical_clip_polygon_3d <- function(X,weights,extent) {
  if(any(pi-sphere_angle_3d(X,rbind(X[-1L,,drop=FALSE],X[1L,,drop=FALSE]))<1e-10)) stop("Antipodal face endpoints require intermediate points.",call.=FALSE)
  planes <- spherical_clip_planes_3d("gnomonic",extent,.01)[1:6,1:4,drop=FALSE]
  for(i in seq_len(nrow(planes))) {
    if(nrow(X)<3L) break
    vertices <- list(); values <- list()
    for(j in seq_len(nrow(X))) {
      k <- if(j==nrow(X)) 1L else j+1L
      a <- sum(X[j,]*planes[i,]); b <- sum(X[k,]*planes[i,])
      if(a>=0) {vertices[[length(vertices)+1L]] <- X[j,]; values[[length(values)+1L]] <- weights[j,]}
      if((a<0&&b>0)||(a>0&&b<0)) {
        t <- a/(a-b)
        point <- (1-t)*X[j,]+t*X[k,]
        vertices[[length(vertices)+1L]] <- point/sqrt(sum(point^2))
        theta <- sphere_angle_3d(X[j,,drop=FALSE],X[k,,drop=FALSE])
        s <- if(theta<1e-12) t else sphere_angle_3d(X[j,,drop=FALSE],matrix(vertices[[length(vertices)]],1L))/theta
        values[[length(values)+1L]] <- (1-s)*weights[j,]+s*weights[k,]
      }
    }
    X <- if(length(vertices)) do.call(rbind,vertices) else matrix(numeric(),0L,4L)
    weights <- if(length(values)) do.call(rbind,values) else matrix(numeric(),0L,ncol(weights))
  }
  list(points=X,weights=weights)
}

projective_face_normals_3d <- function(packet,context,transport) {
  if(!length(packet$faces$offset)) return(packet)
  P <- packet_model_points_3d(packet,context)
  normal <- t(vapply(seq_along(packet$faces$offset),function(i) {
    vertices <- packet$corners$vertex[seq.int(packet$faces$offset[i],length.out=packet$faces$count[i])]
    K <- P[vertices,,drop=FALSE]
    n <- colSums(cross_rows_3d(K,rbind(K[-1L,,drop=FALSE],K[1L,,drop=FALSE])))
    transport(n,sum(n*colMeans(K)),colMeans(K))
  },numeric(3)))
  styles <- packet$styles[match(packet$faces$style_id,packet$styles$style_id),,drop=FALSE]
  styles[c("normal_x","normal_y","normal_z")] <- as.data.frame(normal)
  styles$intrinsic_normal <- TRUE
  if(is.null(packet$styles$intrinsic_normal)) packet$styles$intrinsic_normal <- FALSE
  for(name in setdiff(names(styles),names(packet$styles))) packet$styles[[name]] <- NA_real_
  styles$style_id <- nrow(packet$styles)+seq_len(nrow(styles))
  packet$styles <- rbind(packet$styles,styles)
  packet$faces$style_id <- styles$style_id
  packet$style_count <- as.integer(nrow(packet$styles))
  packet
}

spherical_style_packet_3d <- function(packet,context) {
  if(context$model!="gnomonic") return(packet)
  projective_face_normals_3d(packet,context,function(n,offset,k) {
    norm <- sqrt(sum(n^2)+offset^2)
    if(!is.finite(norm)||norm<=0) return(rep(NA_real_,3L))
    N <- c(n,-offset)/norm
    X <- c(k,1)/sqrt(1+sum(k^2))
    N[1:3]-N[4L]*X[1:3]/(1+X[4L])
  })
}

# Each primitive receives the inverse geodesic-sphere area at its centre.
space_depth_cue_3d <- function(packet,context,fog,area) {
  if(is.null(fog)||!length(packet$vertices$x)) return(packet)
  centres <- primitive_centres_3d(packet)
  sizes <- vapply(centres,nrow,integer(1))
  if(!sum(sizes)) return(packet)
  P <- do.call(rbind,centres)
  P <- sweep(sweep(P,2L,vapply(context$ranges,diff,numeric(1)),"*"),2L,vapply(context$ranges,`[`,numeric(1),1L),"+")
  data <- as.data.frame(P); names(data) <- c("x","y","z")
  X <- context$space$to_canonical(data,context$model)
  brightness <- pmin(1,fog/pmax(area(X),.Machine$double.xmin))
  styles <- primitive_styles_3d(packet)
  for(name in intersect(c("colour","fill","border_colour"),names(styles))) styles[[name]] <- apply_brightness_3d(styles[[name]],brightness,"multiply")
  restyle_primitives_3d(packet,styles,sizes)
}
