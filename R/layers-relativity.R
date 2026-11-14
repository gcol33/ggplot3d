#' @include space-3d.R stats-common.R
NULL

#' Lorentz boosts in canonical coordinates
#'
#' Uses the metric diag(1,...,1,-1), with time in the last coordinate.
#' Positive rapidity sends the time axis toward direction. In velocity units
#' with c=1 the speed is tanh(rapidity). The returned matrix acts on column
#' vectors; multiply a row matrix by its transpose.
#' @param rapidity Finite scalar rapidity.
#' @param direction Nonzero spatial direction in two or three dimensions.
#' @return A 3 by 3 or 4 by 4 Lorentz matrix.
#' @examples
#' B <- lorentz_boost_3d(.5,c(1,0,0))
#' B %*% c(0,0,0,1)
#' @export
lorentz_boost_3d <- function(rapidity,direction=c(1,0,0)) {
  rapidity <- mesh_number_3d(rapidity,"rapidity",is.finite,"finite")
  if(!is.numeric(direction)||!length(direction)%in%c(2L,3L)||any(!is.finite(direction))||sum(direction^2)==0) stop("direction must be a nonzero two- or three-vector.",call.=FALSE)
  direction <- direction/max(abs(direction))
  direction <- direction/sqrt(sum(direction^2)); n <- length(direction)
  ch <- cosh(rapidity); sh <- sinh(rapidity)
  if(!is.finite(ch)) stop("rapidity overflows finite coordinates.",call.=FALSE)
  out <- diag(n+1L); out[seq_len(n),seq_len(n)] <- diag(n)+(ch-1)*tcrossprod(direction)
  out[seq_len(n),n+1L] <- out[n+1L,seq_len(n)] <- sh*direction; out[n+1L,n+1L] <- ch
  out
}

StatLightCone3D <- ggplot2::ggproto("StatLightCone3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z","t"),
  dropped_aes=c(StatGeometry3D$dropped_aes,"t"),
  compute_group=function(data,scales,apex=c(0,0,0),extent=2,interval=0,speed=1,sheet="both",n=30L,boost=NULL) {
    events <- if(all(c("x","y","t")%in%names(data))) as.matrix(data[c("x","y","t")]) else matrix(apex,1L)
    if(any(!is.finite(events))) stop("Event coordinates must be finite.",call.=FALSE)
    signs <- switch(sheet,both=c(-1,1),future=1,past=-1)
    if(interval<0) signs <- 1
    surfaces <- list(); face_offset <- 0L; vertex_offset <- 0L
    for(i in seq_len(nrow(events))) for(sign in signs) {
      evaluate <- function(u,v) {
        if(interval>=0) {rho <- v; time <- sign*sqrt(v^2+interval)/speed}
        else {time <- v; rho <- sqrt((speed*v)^2-interval)}
        X <- cbind(rho*cos(u),rho*sin(u),speed*time)
        if(!is.null(boost)) X <- X %*% t(boost)
        sweep(cbind(X[,1:2,drop=FALSE],X[,3L]/speed),2L,events[i,],"+")
      }
      range <- if(interval<0) switch(sheet,both=c(-extent,extent),future=c(0,extent),past=c(-extent,0)) else c(0,extent)
      d <- parametric_surface_3d(evaluate,c(0,2*pi),range,n,c(TRUE,FALSE),FALSE,list(),FALSE,.002,10,0L)
      d$event <- i; d$sheet <- sign; d$.mesh <- length(surfaces)+1L
      d$.face <- d$.face+face_offset; face_offset <- max(d$.face)
      d$.vertex <- d$.vertex+vertex_offset; vertex_offset <- max(d$.vertex)
      d$interval <- interval
      ct <- speed*(d$z-events[i,3L])
      d$klein_x <- if(interval>0) (d$x-events[i,1L])/ct else NA_real_
      d$klein_y <- if(interval>0) (d$y-events[i,2L])/ct else NA_real_
      d$rapidity <- if(interval>0) acosh(pmax(1,abs(ct)/sqrt(interval))) else NA_real_
      surfaces[[length(surfaces)+1L]] <- d
    }
    do.call(rbind,surfaces)
  }
)

#' Light cones and surfaces of constant spacetime interval
#'
#' Draws c^2 (t-t0)^2 - (x-x0)^2 - (y-y0)^2 = interval with time along z.
#' The zero interval gives light cones, positive intervals timelike
#' hyperboloids, and negative intervals spacelike hyperboloids. Map x, y and
#' t for a cone at each event, or supply apex. Spatial extent is radial for
#' nonnegative interval and temporal for negative interval. Timelike shells
#' compute klein_x and klein_y as v/c, and rapidity from the rest event.
#' @inheritParams stat_parametric_3d
#' @param apex Event (x,y,t) when no event aesthetics are mapped.
#' @param extent Positive radial or temporal parameter extent.
#' @param interval Signed squared spacetime interval.
#' @param speed Positive speed of light in the chosen units.
#' @param sheet Both, future or past sheets.
#' @param boost Optional 3 by 3 [lorentz_boost_3d()] in (x,y,c*t).
#' @return A surface layer.
#' @examples
#' ggplot2::ggplot()+stat_light_cone_3d(extent=2,n=16,alpha=.4)+coord_3d()
#' ggplot2::ggplot()+stat_light_cone_3d(interval=1,sheet="future",n=16)+coord_3d()
#' @export
stat_light_cone_3d <- function(mapping=NULL,data=NULL,geom=GeomStatsSurface3D,position="identity",...,
                               apex=c(0,0,0),extent=2,interval=0,speed=1,sheet=c("both","future","past"),n=30L,
                               boost=NULL,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  if(!is.numeric(apex)||length(apex)!=3L||any(!is.finite(apex))) stop("apex must be a finite (x,y,t) vector.",call.=FALSE)
  extent <- mesh_number_3d(extent,"extent",function(x) is.finite(x)&&x>0,"positive")
  speed <- mesh_number_3d(speed,"speed",function(x) is.finite(x)&&x>0,"positive")
  interval <- mesh_number_3d(interval,"interval",is.finite,"finite")
  if(!is.null(boost)) check_lorentz_matrix_3d(boost,2L)
  make_layer_3d(geom,mapping,stats_seed_data_3d(data),StatLightCone3D,position,
    stats_params_3d(list(apex=apex,extent=extent,interval=interval,speed=speed,sheet=match.arg(sheet),n=n,boost=boost,na.rm=na.rm,shade=TRUE),list(...)),show.legend,inherit.aes)
}

flamm_surface_3d <- function(u,v,rs,sign=1) {
  r <- rs+v^2
  cbind(r*cos(u),r*sin(u),sign*2*sqrt(rs)*v)
}

#' Schwarzschild spatial embedding diagram
#'
#' Flamm's paraboloid has height 2 sqrt(rs (r-rs)), where rs=2M in G=c=1
#' units. It embeds the t=constant, theta=pi/2 spatial slice. Spacetime orbits
#' drawn on it are projections. Its intrinsic surface geodesics are available
#' through [stat_parametric_geodesic_3d()]. A square-root radial parameter
#' keeps the throat regular. Set both=TRUE for both sheets of the bridge.
#' @inheritParams stat_parametric_3d
#' @inheritParams ggplot2::stat_identity
#' @param metric Spatial metric, currently "schwarzschild".
#' @param rs Positive Schwarzschild radius.
#' @param r Radial range, beginning at or outside rs.
#' @param both Draw both sheets.
#' @return A surface layer or a list of two surface layers.
#' @references Sadegh, Kiani and Nouri-Zonoz (2024). Embedding diagrams in
#'   stationary spacetimes. <https://arxiv.org/abs/2308.03104>.
#' @examples
#' ggplot2::ggplot()+stat_embedding_3d(rs=1,n=c(24,12))+coord_3d()
#' @export
stat_embedding_3d <- function(mapping=NULL,data=NULL,...,metric="schwarzschild",rs=1,r=c(rs,4*rs),both=FALSE,n=40L) {
  if(!identical(metric,"schwarzschild")) stop("metric must be 'schwarzschild'.",call.=FALSE)
  rs <- mesh_number_3d(rs,"rs",function(x) is.finite(x)&&x>0,"positive")
  if(!is.numeric(r)||length(r)!=2L||any(!is.finite(r))||r[1L]<rs||diff(r)<=0) stop("r must be an increasing range at or outside rs.",call.=FALSE)
  if(!is.logical(both)||length(both)!=1L||is.na(both)) stop("both must be TRUE or FALSE.",call.=FALSE)
  layers <- lapply(if(both) c(1,-1) else 1,function(sign) stat_parametric_3d(mapping=mapping,data=data,
    fun=function(u,v) flamm_surface_3d(u,v,rs,sign),u=c(0,2*pi),v=sqrt(r-rs),n=n,closed=c(TRUE,FALSE),...))
  if(both) layers else layers[[1L]]
}

schwarzschild_orbit_3d <- function(M,L,null,u0,du0,phi,n,r_range,embedding) {
  h <- diff(phi)/n; state <- c(u0,du0)
  force <- function(y) c(y[2L],if(null) 3*M*y[1L]^2-y[1L] else M/L^2+3*M*y[1L]^2-y[1L])
  advance <- function(y,h) {
    a <- force(y); b <- force(y+h*a/2); c <- force(y+h*b/2); d <- force(y+h*c)
    y+h*(a+2*b+2*c+d)/6
  }
  bound <- rev(1/r_range); states <- matrix(NA_real_,n+1L,3L); states[1,] <- c(phi[1L],state); last <- n+1L
  for(i in seq_len(n)) {
    next_state <- advance(state,h)
    if(any(!is.finite(next_state))) stop("The orbit integration overflowed; increase n.",call.=FALSE)
    if(next_state[1L]<bound[1L]||next_state[1L]>bound[2L]) {
      target <- if(next_state[1L]<bound[1L]) bound[1L] else bound[2L]
      lo <- 0; hi <- 1
      for(k in seq_len(50L)) {
        mid <- (lo+hi)/2; value <- advance(state,h*mid)[1L]
        if((value-target)*(state[1L]-target)>0) lo <- mid else hi <- mid
      }
      fraction <- (lo+hi)/2
      next_state <- advance(state,h*fraction); next_state[1L] <- target
      states[i+1L,] <- c(phi[1L]+h*(i-1+fraction),next_state); last <- i+1L; break
    }
    states[i+1L,] <- c(phi[1L]+h*i,next_state); state <- next_state
  }
  states <- states[seq_len(last),,drop=FALSE]; r <- 1/states[,2L]; angle <- states[,1L]
  invariant <- states[,3L]^2+states[,2L]^2-2*M*states[,2L]^3-if(null) 0 else 2*M/L^2*states[,2L]
  data.frame(x=r*cos(angle),y=r*sin(angle),z=if(embedding) 2*sqrt(2*M*pmax(0,r-2*M)) else 0,
    phi=angle,r=r,u=states[,2L],du=states[,3L],invariant=invariant)
}

StatGeodesicODE3D <- ggplot2::ggproto("StatGeodesicODE3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z"),
  compute_group=function(data,scales,M=1,L=4,null=FALSE,u0=.1,du0=0,phi=c(0,8*pi),n=1000L,r_range=c(2,100),embedding=FALSE) {
    schwarzschild_orbit_3d(M,L,null,u0,du0,phi,n,r_range,embedding)
  }
)

#' Equatorial Schwarzschild spacetime geodesics
#'
#' Integrates u''+u=M/L^2+3 M u^2, u=1/r, using fourth-order Runge-Kutta in
#' azimuth phi. Null geodesics omit M/L^2. Units are G=c=1. Integration ends
#' at the finite radial limits, locating the first crossing by bisection.
#' Computed invariant is u'^2+u^2-2 M u^3-2 M u/L^2 (last term omitted for
#' null paths). Refine n to check convergence and invariant conservation.
#' With embedding=TRUE the spatial orbit is projected onto Flamm's upper
#' paraboloid. These curves are spacetime geodesics projected onto a spatial
#' slice; the paraboloid's own geodesics use [stat_parametric_geodesic_3d()].
#' Use the raster backend for dense orbits on a filled embedding diagram.
#' @inheritParams stat_parametric_3d
#' @param M Positive mass.
#' @param L Positive specific angular momentum, used for timelike paths.
#' @param null Whether the geodesic is null.
#' @param u0,du0 Initial inverse radius and its derivative with respect to phi.
#' @param phi Increasing azimuth range in radians.
#' @param n Number of RK4 steps over phi.
#' @param r_range Finite radial domain, at or outside the horizon r=2M.
#' @param embedding Project the orbit onto the spatial embedding diagram.
#' @return A path layer with phi, r, u, du and invariant computed variables.
#' @references Sadegh, Kiani and Nouri-Zonoz (2024).
#'   <https://arxiv.org/abs/2308.03104>.
#' @examples
#' ggplot2::ggplot()+stat_geodesic_ode_3d(M=1,L=4,u0=.1,n=300)+coord_3d()
#' @export
stat_geodesic_ode_3d <- function(mapping=NULL,data=NULL,geom=ggplot2::GeomPath,position="identity",...,
                                 M=1,L=4,null=FALSE,u0=.1,du0=0,phi=c(0,8*pi),n=1000L,
                                 r_range=c(2*M,100*M),embedding=FALSE,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  M <- mesh_number_3d(M,"M",function(x) is.finite(x)&&x>0,"positive")
  L <- mesh_number_3d(L,"L",function(x) is.finite(x)&&x>0,"positive")
  if(!is.numeric(r_range)||length(r_range)!=2L||any(!is.finite(r_range))||r_range[1L]<2*M||diff(r_range)<=0) stop("r_range must be finite and outside the horizon.",call.=FALSE)
  u0 <- mesh_number_3d(u0,"u0",function(x) is.finite(x)&&x>0&&1/x>=r_range[1L]&&1/x<=r_range[2L],"inside the radial domain")
  du0 <- mesh_number_3d(du0,"du0",is.finite,"finite")
  if(!is.numeric(phi)||length(phi)!=2L||any(!is.finite(phi))||diff(phi)<=0) stop("phi must be an increasing finite range.",call.=FALSE)
  for(value in list(null,embedding)) if(!is.logical(value)||length(value)!=1L||is.na(value)) stop("null and embedding must be TRUE or FALSE.",call.=FALSE)
  make_layer_3d(geom,mapping,stats_seed_data_3d(data),StatGeodesicODE3D,position,
    stats_params_3d(list(M=M,L=L,null=null,u0=u0,du0=du0,phi=phi,n=mesh_count_3d(n,"n",2L),r_range=r_range,
      embedding=embedding,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}

SpaceSpacetime3D <- ggplot2::ggproto("SpaceSpacetime3D",Space3D,
  name="spacetime",charts="cartesian",models="cartesian",default_model="cartesian",speed=1,
  to_canonical=function(self,data,chart) cbind(data$x,data$y,self$speed*data$z),
  from_canonical=function(self,X,model) cbind(X[,1:2,drop=FALSE],X[,3L]/self$speed),
  distance=function(X,Y) sqrt(abs((X[,1L]-Y[,1L])^2+(X[,2L]-Y[,2L])^2-(X[,3L]-Y[,3L])^2)),
  geodesic=function(X,Y,s) segment_points_3d(X,Y,s),
  chart_is_projective=function(model) TRUE,
  act=function(g,X) {
    check_lorentz_matrix_3d(g,2L)
    X%*%t(g)
  }
)

#' Finite Minkowski spacetime coordinates
#'
#' Uses x and y as spatial coordinates and z as time. Lorentz isometries act
#' on (x,y,c*t). The shared scene adapter projects finite events and straight
#' world lines; the camera gives an exterior view of the spacetime diagram.
#' animate_3d(isometry=...) applies a sequence of boosts to the same scene.
#' @param speed Positive speed of light in plot units.
#' @param ... Arguments to [coord_3d()], including limits and isometry.
#' @return A Coord3D object.
#' @examples
#' ggplot2::ggplot()+stat_light_cone_3d(n=12)+coord_spacetime_3d()
#' @export
coord_spacetime_3d <- function(speed=1,...) {
  speed <- mesh_number_3d(speed,"speed",function(x) is.finite(x)&&x>0,"positive")
  do.call(coord_3d,stats_params_3d(list(space=ggplot2::ggproto(NULL,SpaceSpacetime3D,speed=speed),
    input="cartesian",clip_data=FALSE),list(...)))
}
check_lorentz_matrix_3d <- function(g,dimension) {
  n <- as.integer(dimension+1L)
  if(!is.matrix(g)||!is.numeric(g)||!identical(dim(g),c(n,n))||any(!is.finite(g))) stop("A Lorentz isometry must be a finite square matrix of canonical dimension.",call.=FALSE)
  J <- diag(c(rep(1,dimension),-1)); product <- t(g)%*%J%*%g
  scale <- sum(g^2)
  if(any(!is.finite(product))||!is.finite(scale)) stop("The Lorentz matrix exceeds finite metric precision; reduce the rapidity.",call.=FALSE)
  if(max(abs(product-J))>1e-9*max(1,scale)||g[n,n]<1-1e-10) stop("The matrix must be a future-preserving Lorentz isometry.",call.=FALSE)
  invisible(g)
}
