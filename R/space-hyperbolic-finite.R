#' @include space-3d.R layers-relativity.R
NULL

utils::globalVariables(c("from", "to"))

lorentz_dot_3d <- function(X,Y) {
  n <- ncol(X); spatial <- seq_len(n-1L)
  rowSums(X[,spatial,drop=FALSE]*Y[,spatial,drop=FALSE])-X[,n]*Y[,n]
}

# Canonical hyperbolic points are future hyperboloid points (q = 1) or ideal
# points on the future light cone (q = 0), where q = -<X, X>.
hyperbolic_ideal_3d <- function(X) abs(lorentz_dot_3d(X,X))<=1e-9*X[,ncol(X)]^2

hyperbolic_distance_3d <- function(X,Y) {
  out <- 2*asinh(sqrt(pmax(0,lorentz_dot_3d(X-Y,X-Y)))/2)
  ideal <- hyperbolic_ideal_3d(X)|hyperbolic_ideal_3d(Y)
  out[ideal] <- ifelse(rowSums(abs(X[ideal,,drop=FALSE]/X[ideal,ncol(X)]-Y[ideal,,drop=FALSE]/Y[ideal,ncol(Y)]))<1e-12,0,Inf)
  out
}

# Geodesic simplices with an ideal corner are parametrised in the Klein model
# with squared barycentric weights, mu_i = lambda_i^2 / sum(lambda^2): the
# distance to the boundary then vanishes quadratically at an ideal corner, so
# the Poincare image, whose chart map has a square-root singularity there, is
# a smooth function of the parameters. Interior points are lifted to the
# hyperboloid; simplices without ideal corners return NA rows.
hyperbolic_ideal_blend_3d <- function(corners,weights) {
  n <- ncol(corners[[1L]])
  ideal <- Reduce(`|`,lapply(corners,hyperbolic_ideal_3d))
  out <- matrix(NA_real_,nrow(corners[[1L]]),n)
  if(!any(ideal)) return(out)
  mu <- weights[ideal,,drop=FALSE]^2; mu <- mu/rowSums(mu)
  Z <- Reduce(`+`,lapply(seq_along(corners),function(i) mu[,i]*corners[[i]][ideal,,drop=FALSE]/corners[[i]][ideal,n]))
  q <- -lorentz_dot_3d(Z,Z)
  interior <- q>1e-12*Z[,n]^2
  Z[interior,] <- Z[interior,,drop=FALSE]/sqrt(q[interior])
  out[ideal,] <- Z
  out
}

hyperbolic_geodesic_3d <- function(X,Y,s) {
  s <- rep_len(s,nrow(X))
  out <- hyperbolic_ideal_blend_3d(list(X,Y),cbind(1-s,s))
  finite <- is.na(out[,1L])
  if(any(finite)) out[finite,] <- hyperbolic_slerp_3d(X[finite,,drop=FALSE],Y[finite,,drop=FALSE],s[finite])
  out[s==0,] <- X[s==0,,drop=FALSE]
  out[s==1,] <- Y[s==1,,drop=FALSE]
  out
}

hyperbolic_triangle_3d <- function(P0,P1,P2,a,b) hyperbolic_ideal_blend_3d(list(P0,P1,P2),cbind(1-a-b,a,b))

# Homogeneous ball coordinates: Klein (X, X0); Poincare (X, X0 + 1), which is
# (X, X0) at an ideal point. Both are positively homogeneous in X.
hyperbolic_ball_homogeneous_3d <- function(X,model) {
  n <- ncol(X); spatial <- X[,seq_len(n-1L),drop=FALSE]
  if(model=="klein") return(cbind(spatial,X[,n]))
  cbind(spatial,X[,n]+ifelse(hyperbolic_ideal_3d(X),0,1))
}

hyperbolic_slerp_3d <- function(X,Y,s) {
  distance <- hyperbolic_distance_3d(X,Y); small <- distance<1e-8
  denominator <- ifelse(small,1,sinh(distance))
  a <- ifelse(small,1-s,sinh((1-s)*distance)/denominator)
  b <- ifelse(small,s,sinh(s*distance)/denominator)
  out <- a*X+b*Y; n <- ncol(X)
  out[,n] <- sqrt(1+rowSums(out[,seq_len(n-1L),drop=FALSE]^2))
  parameter <- rep_len(s,nrow(X))
  out[parameter==0,] <- X[parameter==0,,drop=FALSE]
  out[parameter==1,] <- Y[parameter==1,,drop=FALSE]
  out
}

hyperbolic_origin_boost_3d <- function(X,inverse=FALSE) {
  r <- sqrt(sum(X[1:3]^2))
  if(r==0) return(diag(4))
  lorentz_boost_3d(if(inverse) -asinh(r) else asinh(r),X[1:3]/r)
}

# Parallel transport along the radial geodesic identifies tangent spaces
# with the Euclidean tangent space at the hyperboloid origin.
hyperbolic_style_packet_3d <- function(packet,context) {
  if(context$model!="klein") return(packet)
  projective_face_normals_3d(packet,context,function(C,X) {
    norm <- sqrt(sum(C[1:3]^2)-C[4L]^2)
    if(!is.finite(norm)||norm<=0) return(rep(NA_real_,3L))
    N <- c(C[1:3],-C[4L])/norm
    drop(hyperbolic_origin_boost_3d(X,TRUE)%*%N)[1:3]
  })
}

SpaceHyperbolicFinite <- ggplot2::ggproto("SpaceHyperbolicFinite",Space3D,
  name="hyperbolic",charts=c("poincare","klein","halfspace","hyperboloid_sheet"),
  models=c("poincare","klein","halfspace","hyperboloid_sheet"),default_model="klein",extent=3,curvature=-1,fog=NULL,
  to_canonical=function(data,chart) {
    P <- as.matrix(data[c("x","y","z")])
    if(chart=="hyperboloid_sheet") {
      if(any(abs(P[,3L]^2-P[,1L]^2-P[,2L]^2-1)>1e-7)||any(P[,3L]<1)) stop("Sheet coordinates must satisfy z^2-x^2-y^2=1 and z>=1.",call.=FALSE)
      return(cbind(P[,1:2,drop=FALSE],0,P[,3L]))
    }
    if(chart=="halfspace") {
      if(any(P[,3L]<=0)) stop("Half-space heights must be positive.",call.=FALSE)
      norm <- rowSums(P^2)
      return(cbind(P[,1:2,drop=FALSE]/P[,3L],(norm-1)/(2*P[,3L]),(norm+1)/(2*P[,3L])))
    }
    norm <- rowSums(P^2)
    ideal <- abs(norm-1)<=1e-10
    if(any(norm>1&!ideal)) stop("Ball coordinates must lie in the closed unit ball; boundary points are ideal.",call.=FALSE)
    out <- matrix(0,nrow(P),4L)
    finite <- !ideal
    if(any(finite)) out[finite,] <- if(chart=="poincare") cbind(2*P[finite,,drop=FALSE]/(1-norm[finite]),(1+norm[finite])/(1-norm[finite])) else
      cbind(P[finite,,drop=FALSE],1)/sqrt(1-norm[finite])
    if(any(ideal)) out[ideal,] <- cbind(P[ideal,,drop=FALSE]/sqrt(norm[ideal]),1)
    out
  },
  from_canonical=function(X,model) {
    if(model%in%c("poincare","klein")) return(hyperbolic_ball_homogeneous_3d(X,model))
    if(model=="hyperboloid_sheet") {
      if(any(abs(X[,3L])>1e-9)) stop("hyperboloid_sheet requires points in the H^2 plane z=0.",call.=FALSE)
      return(cbind(X[,1:2,drop=FALSE],X[,4L]))
    }
    q <- ifelse(hyperbolic_ideal_3d(X),0,1)
    den <- ifelse(X[,3L]>0,(q+rowSums(X[,1:2,drop=FALSE]^2))/(X[,4L]+X[,3L]),X[,4L]-X[,3L])
    cbind(X[,1:2,drop=FALSE],q,den)
  },
  distance=function(self,X,Y) hyperbolic_distance_3d(X,Y)/sqrt(-self$curvature),
  geodesic=function(X,Y,s) hyperbolic_geodesic_3d(X,Y,s),
  triangle=function(P0,P1,P2,a,b) hyperbolic_triangle_3d(P0,P1,P2,a,b),
  act=function(g,X) {
    check_lorentz_matrix_3d(g,3L)
    X%*%t(g)
  },
  chart_is_projective=function(model) model=="klein",
  projective_action=function(g,model) if(model=="klein") check_lorentz_matrix_3d(g,3L) else NULL,
  projective_canonical=function(H,model) H*sign(H[,4L])/sqrt(H[,4L]^2-rowSums(H[,1:3,drop=FALSE]^2)),
  style_packet=function(self,packet,context) space_depth_cue_3d(hyperbolic_style_packet_3d(packet,context),context,self$fog,
    function(X) rowSums(X[,1:3,drop=FALSE]^2)/(-self$curvature)),
  model_ranges=function(self,model,panel_params) {
    if(model%in%c("poincare","klein")) return(stats::setNames(rep(list(c(-1,1)),3L),c("x","y","z")))
    r <- sinh(self$extent)
    list(x=c(-r,r),y=c(-r,r),z=if(model=="hyperboloid_sheet") c(0,cosh(self$extent)) else exp(c(-1,1)*self$extent))
  }
)

FrameHyperbolicFinite <- ggplot2::ggproto("FrameHyperbolicFinite",FrameBox,
  references=character(),space=NULL,model="poincare",distance_breaks=NULL,
  axis_view=function(scale,limits,expand,axis) {
    if(scale$is_discrete()||scale$get_transformation()$name!="identity") stop("Hyperbolic position scales must be continuous identity scales.",call.=FALSE)
    FrameBox$axis_view(scale,limits,expand,axis)
  },
  scene_packets=function(self,panel_params,theme) {
    packets <- list()
    if(length(self$distance_breaks)) {
      breaks <- self$distance_breaks
      r <- breaks*sqrt(-self$space$curvature)
      X <- cbind(sinh(r),0,0,cosh(r))
      action <- panel_params$space_context$action
      if(!is.null(action)) X <- t(solve(action,t(X)))
      xyz <- space_model_xyz_3d(X,self$space,self$model)
      d <- data.frame(x=xyz[,1L],y=xyz[,2L],z=xyz[,3L],group=1L,
        colour="grey35",alpha=1,linewidth=.4,linetype=1,label=format(breaks,trim=TRUE),size=3,vjust=-.5)
      ranges <- self$space$model_ranges(self$model,panel_params)
      packets <- list(emit_model_scene_3d(d,list(.emitter="GeomPath"),panel_params,ranges),
        emit_model_scene_3d(d,list(.emitter="GeomText"),panel_params,ranges))
    }
    if(self$model!="hyperboloid_sheet"||!length(self$references)) return(packets)
    d <- do.call(rbind,lapply(seq_along(self$references),function(i) {
      theta <- seq(0,2*pi,length.out=73L)
      data.frame(x=cos(theta),y=sin(theta),z=if(self$references[i]=="klein") 1 else 0,
        group=i,colour=if(self$references[i]=="klein") "#9B6E46" else "#486F9B",alpha=1,linewidth=.5,linetype=1)
    }))
    c(packets,list(emit_model_scene_3d(d,list(.emitter="GeomPath"),panel_params,self$space$model_ranges(self$model,panel_params))))
  }
)

#' Finite hyperbolic ball and sheet coordinates
#'
#' Accepts Poincare or Klein ball positions and draws their intrinsic
#' geodesics through the shared space sampler. Canonical points lie on the
#' future Lorentz hyperboloid. The hyperboloid_sheet model lifts points in the
#' z=0 disk to (X1,X2,X0). Its reference circles are the Klein disk at X0=1
#' and the Poincare disk at X0=0. Half-space has positive z. All input points
#' are finite and strictly interior to their charts.
#' @param input,model Input chart and display model.
#' @param extent Hyperbolic radius setting display bounds of unbounded models.
#' @param references Reference circles for hyperboloid_sheet: "klein", "poincare".
#' @param curvature Negative sectional curvature. Ball coordinates are unitless;
#'   intrinsic distances are scaled by 1/sqrt(-curvature).
#' @param centre A point in the input chart, or a future Lorentz isometry.
#'   Points are moved to the origin before display.
#' @param distance_breaks Optional increasing nonnegative distances for a radial
#'   distance guide, in intrinsic units.
#' @param inside Use a first-person perspective eye at the Klein origin.
#' @param observer Observer point in the input chart, or an isometry taking the
#'   observer to the origin. Supply one of observer, centre or isometry.
#' @param fog Optional positive inverse-area brightness scale. At each primitive
#'   centre brightness is min(1, fog/area), where area is
#'   sinh(sqrt(-curvature)*d)^2/(-curvature). Colours are scaled in linear light.
#' @param ... Arguments to [coord_3d()], including isometry.
#' @return A Coord3D object.
#' @references Nickel and Kiela (2017). Poincare embeddings.
#' <https://arxiv.org/abs/1705.08039>.
#' Phillips and Gunn (1992). Visualizing hyperbolic space.
#' <https://graphics.stanford.edu/papers/webviz/webviz/node2.html>.
#' Weeks (2002). Real-time rendering in curved spaces.
#' \doi{10.1109/MCG.2002.1046633}. Coulon et al. (2022), Section 7.
#' <https://arxiv.org/abs/2010.15801>.
#' @details Klein is the default display and input. Hyperbolic layout statistics
#'   register their own Poincare input chart. Half-space is truncated
#'   to the finite display box set by extent. Geodesic faces and arcs use the
#'   common screen-error sampler. Position scales must be identity transforms.
#'   Klein lighting transports Minkowski plane normals to the origin.
#'   Inside views use the existing vector visibility pipeline with a perspective
#'   eye at the Klein origin. Near defaults to sqrt(machine epsilon), and far
#'   to the farthest panel-box corner, in normalised camera units. In the
#'   Klein model an observer, centre or isometry is a Lorentz transformation
#'   of the homogeneous coordinates (X, X0) and is carried by the camera
#'   pre-transform, as in Weeks (2002): the geometry is emitted once, and
#'   animate_3d(isometry = ) frames only recompile, with fog and intrinsic
#'   normals evaluated for the moved observer. Poincare and half-space
#'   models apply the action before packing. Camera azimuth, elevation and
#'   roll set facing.
#' @examples
#' d <- data.frame(x=c(-.7,.7),y=c(.2,.2),z=0)
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+coord_hyperbolic_3d()
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+
#'   coord_hyperbolic_3d(model="poincare",curvature=-4,distance_breaks=c(0,.5,1))
#' d <- data.frame(x=.5,y=c(-.2,.2,.2,-.2),z=c(-.2,-.2,.2,.2))
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d(fill="steelblue")+
#'   coord_hyperbolic_3d(inside=TRUE,observer=c(.1,0,0),azimuth=180,elevation=0)
#' @export
coord_hyperbolic_3d <- function(model="klein",input=model,
                               extent=3,references=character(),curvature=-1,centre=NULL,distance_breaks=NULL,
                               inside=FALSE,observer=NULL,fog=NULL,...) {
  model <- match.arg(model,SpaceHyperbolicFinite$models); input <- match.arg(input,SpaceHyperbolicFinite$charts)
  extent <- mesh_number_3d(extent,"extent",function(x) is.finite(x)&&x>0,"positive")
  if(!is.character(references)||any(!references%in%c("klein","poincare"))) stop("Unknown hyperboloid reference disk.",call.=FALSE)
  curvature <- mesh_number_3d(curvature,"curvature",function(x) is.finite(x)&&x<0,"negative")
  if(!is.null(distance_breaks)&&(!is.numeric(distance_breaks)||any(!is.finite(distance_breaks))||any(distance_breaks<0)||is.unsorted(distance_breaks,strictly=TRUE))) stop("distance_breaks must be increasing finite nonnegative distances.",call.=FALSE)
  space <- ggplot2::ggproto(NULL,SpaceHyperbolicFinite,extent=extent,curvature=curvature,fog=fog)
  args <- check_inside_options_3d(inside,fog,list(...))
  if(inside&&model!="klein") stop("Inside hyperbolic views require the Klein model.",call.=FALSE)
  if(!is.null(observer)) {
    if(!is.null(centre)) stop("Supply observer or centre, one action at a time.",call.=FALSE)
    centre <- observer
  }
  if(!is.null(centre)) {
    if(!is.null(args$isometry)) stop("Supply centre or isometry, one action at a time.",call.=FALSE)
    if(is.matrix(centre)) {check_lorentz_matrix_3d(centre,3L); args$isometry <- centre} else {
      if(!is.numeric(centre)||length(centre)!=3L||any(!is.finite(centre))) stop("centre must be a finite input-chart point or Lorentz matrix.",call.=FALSE)
      X <- space$to_canonical(data.frame(x=centre[1L],y=centre[2L],z=centre[3L]),input)
      if(hyperbolic_ideal_3d(X)) stop("centre must lie strictly inside the model; boundary points are ideal.",call.=FALSE)
      args$isometry <- hyperbolic_origin_boost_3d(drop(X),TRUE)
    }
  }
  bounds <- space$model_ranges(model,NULL)
  if(!is.null(args$aspect)&&!isTRUE(all.equal(as.numeric(args$aspect),vapply(bounds,diff,numeric(1))/diff(bounds$x)))) stop("Hyperbolic aspect is fixed by model units.",call.=FALSE)
  args$aspect <- vapply(bounds,diff,numeric(1))
  coord <- do.call(coord_3d,stats_params_3d(list(space=space,input=input,model=model,
    axes=character(),panes=character(),box="none",clip_data=TRUE),args))
  coord$frame <- ggplot2::ggproto(NULL,FrameHyperbolicFinite,space=space,model=model,references=references,distance_breaks=distance_breaks)
  if(inside) inside_coord_3d(coord) else coord
}

#' Focus paths for a hyperbolic embedding
#'
#' Returns Lorentz boosts taking a Poincare-ball point to the origin along a
#' geodesic. Pass the matrices to animate_3d(isometry=...).
#' @param point Finite point strictly inside the Poincare ball.
#' @param frames Positive number of frames.
#' @return A list of Lorentz matrices, from identity to the focus transform.
#' @examples
#' hyperbolic_focus_3d(c(.4,0,0),frames=3)
#' @export
hyperbolic_focus_3d <- function(point,frames=30L) {
  if(!is.numeric(point)||length(point)!=3L||any(!is.finite(point))||sum(point^2)>=1) stop("point must lie strictly inside the Poincare ball.",call.=FALSE)
  frames <- mesh_count_3d(frames,"frames",1L); r <- sqrt(sum(point^2))
  if(r==0) return(rep(list(diag(4)),frames))
  lapply(seq(0,1,length.out=frames),function(s) lorentz_boost_3d(-2*s*atanh(r),point/r))
}
