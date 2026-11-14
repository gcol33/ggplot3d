#' @include space-3d.R frame-globe.R layers-projection-nd.R layers-sweep.R
NULL

check_so4_3d <- function(g) {
  if(!is.matrix(g)||!is.numeric(g)||!identical(dim(g),c(4L,4L))||any(!is.finite(g))||
     max(abs(crossprod(g)-diag(4)))>1e-9||abs(det(g)-1)>1e-9) stop("Spherical isometry must be an SO(4) matrix.",call.=FALSE)
  g
}

# Roots on a great-circle arc of each affine clipping plane n.X + c >= 0.
spherical_clip_segment_3d <- function(X,Y,planes) {
  theta <- sphere_angle_3d(matrix(X,1L),matrix(Y,1L))
  if(pi-theta<1e-10) stop("Antipodal endpoints require an intermediate point.",call.=FALSE)
  inside <- function(Z) all(drop(planes[,1:4,drop=FALSE]%*%Z)+planes[,5L]>=-1e-12)
  if(theta<1e-12) return(if(inside(X)) matrix(c(0,1),1L) else matrix(numeric(),0L,2L))
  V <- (Y-cos(theta)*X)/sin(theta)
  roots <- c(0,theta)
  for(i in seq_len(nrow(planes))) {
    a <- sum(planes[i,1:4]*X); b <- sum(planes[i,1:4]*V); r <- sqrt(a*a+b*b)
    if(r==0||abs(planes[i,5L])>r) next
    phase <- atan2(b,a); angle <- acos(pmin(1,pmax(-1,-planes[i,5L]/r)))
    candidate <- as.vector(outer(c(phase-angle,phase+angle),2*pi*(-1:1),"+"))
    roots <- c(roots,candidate[candidate>0&candidate<theta])
  }
  roots <- sort(unique(roots)); intervals <- cbind(head(roots,-1L),tail(roots,-1L))
  keep <- apply(intervals,1L,function(t) inside(cos(mean(t))*X+sin(mean(t))*V))
  intervals[keep,,drop=FALSE]/theta
}

spherical_clip_planes_3d <- function(model,extent,pole_radius) {
  den <- if(model=="stereographic") c(0,0,0,-1,1) else c(0,0,0,1,0)
  box <- do.call(rbind,lapply(1:3,function(j) {
    n <- numeric(5); n[j] <- 1
    rbind(extent*den+n,extent*den-n)
  }))
  rbind(box,if(model=="stereographic") c(0,0,0,-1,cos(pole_radius)) else c(0,0,0,1,sin(-pole_radius)))
}

SpaceSpherical3D <- ggplot2::ggproto("SpaceSpherical3D",Space3D,
  name="spherical",charts=c("stereographic","gnomonic","ambient","canonical"),
  models=c("stereographic","gnomonic"),default_model="stereographic",extent=3,pole_radius=.01,
  to_canonical=function(data,chart) {
    if(chart=="canonical") return(as.matrix(data[paste0(".s",1:4)]))
    P <- as.matrix(data[c("x","y","z")])
    if(chart=="ambient") {
      if(is.null(data$w)) stop("Ambient S^3 input requires the w aesthetic.",call.=FALSE)
      X <- cbind(P,data$w)
      if(any(abs(rowSums(X^2)-1)>1e-8)) stop("Ambient points must lie on the unit three-sphere.",call.=FALSE)
      return(X)
    }
    if(chart=="gnomonic") return(cbind(P,1)/sqrt(1+rowSums(P^2)))
    r <- rowSums(P^2)
    cbind(2*P/(1+r),(r-1)/(1+r))
  },
  from_canonical=function(X,model) {
    den <- if(model=="stereographic") 1-X[,4L] else X[,4L]
    if(any(den<=0)) stop("Projection pole requires clipping before division.",call.=FALSE)
    X[,1:3,drop=FALSE]/den
  },
  distance=function(X,Y) sphere_angle_3d(X,Y),
  geodesic=function(X,Y,s) sphere_slerp_3d(X,Y,s),
  act=function(g,X) X%*%t(check_so4_3d(g)),
  chart_is_projective=function(model) model=="gnomonic",
  model_ranges=function(self,model,panel_params) stats::setNames(rep(list(c(-self$extent,self$extent)),3L),c("x","y","z")),
  clip_segment=function(self,X,Y,model) spherical_clip_segment_3d(X,Y,spherical_clip_planes_3d(model,self$extent,self$pole_radius)),
  contains=function(self,X,model) {
    planes <- spherical_clip_planes_3d(model,self$extent,self$pole_radius)
    apply(sweep(X%*%t(planes[,1:4,drop=FALSE]),2L,planes[,5L],"+"),1L,function(x) all(x>=-1e-12))
  }
)

#' Spherical geometry in stereographic and gnomonic charts
#'
#' Maps the unit three-sphere in four dimensions to three display coordinates.
#' Paths and faces follow shorter great circles through the shared sampler.
#' Ambient inputs map x, y, z and w. Stereographic input is the inverse chart
#' (2p, |p|^2-1)/(1+|p|^2). The display map is `X[1:3]/(1-X[4])`.
#' Gnomonic display `X[1:3]/X[4]` covers the positive fourth-coordinate hemisphere.
#' Paths are clipped analytically to the panel box and a ball around the pole.
#' @param dim Spherical dimension, currently three.
#' @param model,input Display model and input chart.
#' @param extent Positive half-width of the finite display box.
#' @param pole_radius Angular radius excluded around the stereographic pole,
#'   or excluded from the gnomonic equator, in radians.
#' @param ... Arguments to [coord_3d()], including SO(4) isometry matrices
#'   from [rotate_4d()]. These also work in animate_3d(isometry=).
#' @return A Coord3D object.
#' @references Johnson (2011). A visualization of the Hopf fibration.
#' <https://nilesjohnson.net/hopf.html>. Obermeyer. Jenn3d.
#' <https://jenn3d.org/>.
#' @examples
#' d <- data.frame(x=c(-1,1),y=.4,z=0)
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+
#'   coord_spherical_geometry_3d(isometry=rotate_4d("xw",.2))
#' @export
coord_spherical_geometry_3d <- function(dim=3,model=c("stereographic","gnomonic"),input=model,
                                       extent=3,pole_radius=.01,...) {
  if(length(dim)!=1L||is.na(dim)||dim!=3) stop("dim must be 3.",call.=FALSE)
  model <- match.arg(model); input <- match.arg(input,SpaceSpherical3D$charts)
  extent <- mesh_number_3d(extent,"extent",function(x) is.finite(x)&&x>0,"positive")
  pole_radius <- mesh_number_3d(pole_radius,"pole_radius",function(x) is.finite(x)&&x>0&&x<pi/2,"between zero and pi/2")
  space <- ggplot2::ggproto(NULL,SpaceSpherical3D,extent=extent,pole_radius=pole_radius)
  coord <- do.call(coord_3d,stats_params_3d(list(space=space,model=model,input=input,
    aspect=c(1,1,1),axes=character(),panes=character(),box="none",clip_data=TRUE),list(...)))
  coord
}

hopf_points_3d <- function(lon,lat,t) {
  theta <- (90-lat)*pi/180; phi <- lon*pi/180
  cbind(cos(theta/2)*cos(phi+t),cos(theta/2)*sin(phi+t),sin(theta/2)*cos(t),sin(theta/2)*sin(t))
}

StatHopfFibre3D <- ggplot2::ggproto("StatHopfFibre3D",ggplot2::Stat,
  required_aes=c("lon","lat"),dropped_aes=c("lon","lat"),
  compute_group=function(data,scales,n=64L,what="fibres") {
    if(any(!is.finite(data$lon))||any(!is.finite(data$lat))||any(abs(data$lat)>90)) stop("Hopf longitude and latitude must be finite, with latitude in [-90,90].",call.=FALSE)
    blocks <- lapply(seq_len(nrow(data)),function(i) {
      if(what=="base") {
        out <- data[i,,drop=FALSE]; xyz <- lonlat_xyz_3d(out$lon,out$lat)
        out[c("x","y","z")] <- as.data.frame(xyz); return(out)
      }
      if(what=="torus") {
        if(abs(data$lat[i])==90) stop("A latitude torus requires latitude strictly between -90 and 90.",call.=FALSE)
        grid <- expand.grid(t=seq(0,2*pi,length.out=n+1L)[-1L],phi=seq(0,360,length.out=n+1L)[-1L])
        X <- hopf_points_3d(grid$phi,data$lat[i],grid$t)
        a <- seq_len(n*n); b <- (a-1L)%/%n*n+a%%n+1L; c <- (a+n-1L)%%(n*n)+1L; d <- (b+n-1L)%%(n*n)+1L
        tri <- rbind(cbind(a,b,c),cbind(b,d,c)); index <- as.vector(t(tri))
      } else {X <- hopf_points_3d(data$lon[i],data$lat[i],seq(0,2*pi,length.out=n+1L)); index <- seq_len(nrow(X))}
      out <- data[rep(i,length(index)),,drop=FALSE]
      out[paste0(".s",1:4)] <- as.data.frame(X[index,,drop=FALSE])
      out$x <- out$y <- out$z <- 0
      out$.fibre <- i
      if(what=="torus") out$.face <- rep(seq_len(nrow(tri)),each=3L)
      out
    })
    do.call(rbind,blocks)
  }
)

GeomHopfFibre3D <- ggplot2::ggproto("GeomHopfFibre3D",ggplot2::Geom,
  required_aes=c("x","y","z"),default_aes=local({a <- solid_defaults_3d(); a$colour <- "grey25"; a$size <- 1.5; a$shape <- 19; a}),draw_key=ggplot2::draw_key_polygon)

emit_hopf_fibre_3d <- function(data,params,panel_params) {
  if(params$what=="base") return(.emitters_3d[["GeomPoint"]]$emit(data,params,panel_params))
  keys <- interaction(data$group,data$.fibre,if(params$what=="torus") data$.face else 1L,drop=TRUE)
  chunks <- split(seq_len(nrow(data)),keys)
  primitives <- unlist(chunks,use.names=FALSE); counts <- lengths(chunks)
  if(params$what=="torus"||params$radius==0) return(packet_from_primitives_3d(data,primitives,
    rep(if(params$what=="torus") "faces" else "polylines",length(counts)),params,panel_params$panel_id,counts))
  context <- params$.space_context
  packets <- list()
  for(rows in chunks) {
    X <- space_canonical_3d(data[rows,,drop=FALSE],context)
    for(path in space_path_samples_3d(X,context)) {
      d <- space_attributes_3d(data,cbind(rows[path$segment],rows[path$segment+1L]),cbind(1-path$s,path$s))
      model <- path$model
      xyz <- sweep(sweep(model,2L,vapply(context$ranges,diff,numeric(1)),"*"),2L,vapply(context$ranges,`[`,numeric(1),1L),"+")
      d[c("x","y","z")] <- as.data.frame(xyz)
      closed <- max(abs(xyz[1L,]-xyz[nrow(xyz),]))<1e-10
      d <- curve_points_3d(d,closed)
      surface <- sweep_surface_3d(d,rep(params$radius,nrow(d)),rep(0,nrow(d)),sweep_profile_3d(NULL,params$sides,NULL),closed,"none",NULL)
      packets[[length(packets)+1L]] <- emit_model_scene_3d(surface,list(.emitter="GeomStatsSurface3D",shade=TRUE),panel_params,context$ranges,unique(data$.layer))
    }
  }
  if(!length(packets)) return(new_scene_packet(panel_id=panel_params$panel_id))
  packet <- merge_scene_packets(packets,panel_params$panel_id)
  attr(packet,"space_mapped") <- TRUE; attr(packet,"model_space") <- TRUE; packet
}

register_geom_3d(GeomHopfFibre3D,emit_hopf_fibre_3d,positions=character(),
  parameters=list(what="fibres",n=64L,radius=.025,sides=8L),
  space=function(data,params,panel_params) {
    if(params$what=="base") return(space_spec_3d(SpaceEuclidean,"cartesian","cartesian",NULL,1e-3,9L))
    context <- panel_params$space_context
    space <- if(!is.null(context)&&inherits(context$space,"SpaceSpherical3D")) context$space else SpaceSpherical3D
    space_spec_3d(space,context$model %or3d% "stereographic","canonical",context$isometry,context$tolerance %or3d% 1e-3,context$max_depth %or3d% 9L)
  })

#' Hopf fibres, latitude tori and their spherical base
#'
#' Over colatitude theta and longitude phi, samples the complex pair
#' (cos(theta/2) exp(i(phi+t)), sin(theta/2) exp(it)). The Hopf map
#' (2 z1 conjugate(z2), |z1|^2-|z2|^2) is constant along each fibre.
#' Latitude zero gives the Clifford torus. Tubes use the shared rotation
#' minimising sweep after geodesic sampling and finite-box clipping.
#' Use what="base" in a second plot with coord_3d() and the same colour mapping
#' to display matching points on the base two-sphere.
#' @inheritParams stat_parametric_3d
#' @param what Fibres, a full torus for each latitude, or points on the base sphere.
#' @param n Number of angular intervals per fibre or torus direction.
#' @param radius Tube radius in display-model units; zero draws paths.
#' @param sides Number of tube sides.
#' @return A registered 3D layer; lon and lat are in degrees.
#' @references Johnson (2011). Hopf fibration production notes.
#' <https://nilesjohnson.net/hopf-production.html>.
#' @examples
#' d <- data.frame(lon=seq(0,300,by=60),lat=0,id=factor(1:6))
#' ggplot2::ggplot(d,ggplot2::aes(lon=lon,lat=lat,fill=id))+
#'   stat_hopf_fibre_3d(n=24)+coord_spherical_geometry_3d()
#' ggplot2::ggplot(d,ggplot2::aes(lon=lon,lat=lat,colour=id))+
#'   stat_hopf_fibre_3d(what="base",size=3)+
#'   geom_sphere_3d(data=data.frame(x=0,y=0,z=0),
#'     mapping=ggplot2::aes(x,y,z=z),inherit.aes=FALSE,
#'     radius=.98,fill="grey90",colour=NA)+coord_3d()
#' ggplot2::ggplot(data.frame(lon=0,lat=0),ggplot2::aes(lon=lon,lat=lat))+
#'   stat_hopf_fibre_3d(what="torus",n=12,fill="steelblue")+
#'   coord_spherical_geometry_3d()
#' @export
stat_hopf_fibre_3d <- function(mapping=NULL,data=NULL,position="identity",...,what=c("fibres","torus","base"),
                              n=64L,radius=.025,sides=8L,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  what <- match.arg(what); n <- mesh_count_3d(n,"n",4L); sides <- mesh_count_3d(sides,"sides",3L)
  radius <- mesh_number_3d(radius,"radius",function(x) is.finite(x)&&x>=0,"nonnegative")
  make_layer_3d(GeomHopfFibre3D,mapping,data,StatHopfFibre3D,position,
    stats_params_3d(list(what=what,n=n,radius=radius,sides=sides,shade=TRUE,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}
