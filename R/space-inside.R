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

# Coefficients C of the oriented hyperplane of R^4 through the origin and the
# rows A, B, D, with C . H = det(H, A, B, D). For rows (k, 1) this is
# (n, -n . k1) with n = (k2 - k1) x (k3 - k1); it is multilinear, so positive
# rescaling of a row keeps it, and the sign of a row is part of the point
# (Stolfi 1991).
hyperplane_rows_4d <- function(A,B,D) {
  minor <- function(i,j,k) A[,i]*(B[,j]*D[,k]-B[,k]*D[,j])-A[,j]*(B[,i]*D[,k]-B[,k]*D[,i])+A[,k]*(B[,i]*D[,j]-B[,j]*D[,i])
  cbind(minor(2,3,4),-minor(1,3,4),minor(1,2,4),-minor(1,2,3))
}

# Each face's hyperplane C in homogeneous model coordinates (summed over a
# fan) and its centre, the chart centroid of corners rescaled to |w| = 1, as a
# canonical point; transport(C, X) returns the tangent normal at the observer.
projective_face_normals_3d <- function(packet,context,transport) {
  if(!length(packet$faces$offset)) return(packet)
  H <- packet_model_homogeneous_3d(packet,context)
  normal <- t(vapply(seq_along(packet$faces$offset),function(i) {
    vertices <- packet$corners$vertex[seq.int(packet$faces$offset[i],length.out=packet$faces$count[i])]
    K <- H[vertices,,drop=FALSE]; m <- nrow(K)
    C <- colSums(hyperplane_rows_4d(K[rep(1L,m-2L),,drop=FALSE],K[2:(m-1L),,drop=FALSE],K[3:m,,drop=FALSE]))
    size <- ifelse(K[,4L]!=0,abs(K[,4L]),sqrt(rowSums(K^2)))
    out <- transport(C,drop(space_model_canonical_3d(matrix(colMeans(K/size),1L),context)))
    if(all(is.finite(out))) out else rep(NA_real_,3L)
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
  projective_face_normals_3d(packet,context,function(C,X) {
    norm <- sqrt(sum(C^2))
    if(!is.finite(norm)||norm<=0) return(rep(NA_real_,3L))
    N <- C/norm
    N[1:3]-N[4L]*X[1:3]/(1+X[4L])
  })
}

# Each primitive receives the inverse geodesic-sphere area at its centre.
space_depth_cue_3d <- function(packet,context,fog,area) {
  if(is.null(fog)||!length(packet$vertices$x)) return(packet)
  means <- primitive_means_3d(packet,space_action_pre_3d(context) %or3d% diag(4))
  sizes <- vapply(means,nrow,integer(1))
  if(!sum(sizes)) return(packet)
  X <- space_model_canonical_3d(space_box_model_3d(do.call(rbind,means),context),context)
  brightness <- pmin(1,fog/pmax(area(X),.Machine$double.xmin))
  styles <- primitive_styles_3d(packet)
  for(name in intersect(c("colour","fill","border_colour"),names(styles))) styles[[name]] <- apply_brightness_3d(styles[[name]],brightness,"multiply")
  restyle_primitives_3d(packet,styles,sizes)
}
