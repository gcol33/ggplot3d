#' @include layers-projection-nd.R
NULL

grassmann_basis_3d <- function(A,B,s) {
  decomposition <- svd(crossprod(A,B))
  U <- decomposition$u; V <- decomposition$v
  angle <- acos(pmax(0,pmin(1,decomposition$d)))
  first <- A%*%U; second <- B%*%V
  sine <- sin(angle); normal <- sweep(second-sweep(first,2L,cos(angle),"*"),2L,ifelse(sine>1e-10,sine,1),"/")
  normal[,sine<=1e-10] <- 0
  (sweep(first,2L,cos(s*angle),"*")+sweep(normal,2L,sin(s*angle),"*"))%*%t(U)
}

tour_basis_frames_3d <- function(spec,p,frames) {
  if(!inherits(spec,"ggplot3d_tour")) spec <- tour_grand(history=spec)
  history <- spec$history
  if(is.null(history)) {
    exists_seed <- exists(".Random.seed",.GlobalEnv,inherits=FALSE)
    if(exists_seed) previous_seed <- get(".Random.seed",.GlobalEnv)
    on.exit(if(exists_seed) assign(".Random.seed",previous_seed,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
    set.seed(spec$seed)
    k <- max(2L,ceiling((frames-1L)/spec$steps)+1L)
    history <- array(0,c(p,3L,k)); history[,,1L] <- diag(p)[,1:3,drop=FALSE]
    for(i in 2:k) {
      random <- matrix(stats::rnorm(p*3L),p,3L); q <- qr(random)
      history[,,i] <- sweep(qr.Q(q),2L,sign(diag(qr.R(q))),"*")
    }
  }
  dimensions <- dim(history)
  if(!is.numeric(history)||length(dimensions)!=3L||dimensions[1L]!=p||dimensions[2L]!=3L||dimensions[3L]<1L||any(!is.finite(history))) stop("Tour history must be a finite p by 3 by k array.",call.=FALSE)
  k <- dimensions[3L]
  for(i in seq_len(k)) projection_basis_3d(history[,,i,drop=FALSE][,,1L],p)
  if(k>1L) for(i in 2:k) {
    alignment <- svd(crossprod(history[,,i-1L],history[,,i]))
    history[,,i] <- history[,,i]%*%alignment$v%*%t(alignment$u)
  }
  position <- if(k==1L) rep(0,frames) else seq(0,k-1L,length.out=frames)
  lapply(position,function(at) {
    if(at==k-1L) return(history[,,k])
    i <- floor(at)+1L; fraction <- at-floor(at)
    if(fraction==0) history[,,i] else grassmann_basis_3d(history[,,i],history[,,i+1L],fraction)
  })
}

#' Grand tours of three-dimensional projections
#'
#' Creates a sequence of orthonormal projection frames interpolated along
#' Grassmann geodesics, using principal angles from an SVD. Target frames are
#' aligned by an orthogonal Procrustes rotation for a continuous projected
#' orientation. A tourr history array (p by 3 by k) is accepted directly.
#' Random targets use a local seed and preserve the caller's random state.
#' Pass the result to animate_3d(tour=...) on a plot using [stat_project_3d()].
#' @param d Projection dimension, currently three.
#' @param history Optional p by 3 by k array of orthonormal projection frames.
#' @param steps Frames between random target subspaces.
#' @param seed Local random seed for a generated grand tour.
#' @return A ggplot3d_tour specification.
#' @references Wickham, Cook, Hofmann and Buja (2011). tourr.
#' \doi{10.18637/jss.v040.i02}.
#' @examples
#' tour_grand(d=3,steps=20,seed=1)
#' @export
tour_grand <- function(d=3L,history=NULL,steps=30L,seed=1L) {
  if(!identical(as.integer(d),3L)||length(d)!=1L) stop("A 3D tour requires d=3.",call.=FALSE)
  steps <- mesh_count_3d(steps,"steps",1L); seed <- mesh_count_3d(seed,"seed",0L)
  structure(list(history=history,steps=steps,seed=seed),class="ggplot3d_tour")
}

section_mesh_4d <- function(vertices,cells,normal,offset,basis=NULL,tolerance=1e-10) {
  vertices <- as.matrix(vertices); cells <- as.matrix(cells)
  if(!is.numeric(vertices)||ncol(vertices)!=4L||any(!is.finite(vertices))||!is.numeric(cells)||ncol(cells)!=4L||any(!is.finite(cells))||any(cells<1|cells>nrow(vertices)|cells!=trunc(cells))) stop("Supply finite four-dimensional vertices and four-index tetrahedral cells.",call.=FALSE)
  if(!is.numeric(normal)||length(normal)!=4L||any(!is.finite(normal))||sum(normal^2)==0) stop("normal must be a nonzero four-vector.",call.=FALSE)
  length <- sqrt(sum(normal^2)); normal <- normal/length; offset <- offset/length
  basis <- basis %or3d% qr.Q(qr(matrix(normal,ncol=1L)),complete=TRUE)[,-1L,drop=FALSE]
  basis <- projection_basis_3d(basis,4L)
  if(max(abs(drop(crossprod(normal,basis))))>1e-8) stop("Section basis must be perpendicular to normal.",call.=FALSE)
  signed <- drop(vertices%*%normal)-offset
  signed[abs(signed)<=tolerance] <- 0
  section <- geometry_section_mesh_cpp(vertices%*%basis,signed,matrix(as.integer(cells),ncol=4L))
  if(!nrow(section$triangles)) return(data.frame(x=numeric(),y=numeric(),z=numeric(),.face=integer(),.corner=integer(),.vertex=integer(),cell=integer()))
  xyz <- as.data.frame(section$vertices); names(xyz) <- c("x","y","z")
  mesh_3d(xyz,section$triangles,face_data=data.frame(.face=seq_along(section$cell),cell=section$cell))
}

StatSection4D <- ggplot2::ggproto("StatSection4D",ggplot2::Stat,
  required_aes=c("x","y","z","w"),dropped_aes=c(".source","w"),
  compute_panel=function(data,scales,cells,normal=c(0,0,0,1),offset=0,basis=NULL,tolerance=1e-10) {
    out <- section_mesh_4d(data[c("x","y","z","w")],cells,normal,offset,basis,tolerance)
    out$group <- rep.int(1L,nrow(out))
    out
  }
)

#' Hyperplane sections of four-dimensional tetrahedral meshes
#'
#' Cuts each tetrahedral boundary cell by dot(normal, position)=offset,
#' interpolates shared mesh edges once, and triangulates the resulting
#' three-dimensional section. Cells lying entirely in the section contribute
#' their boundary triangles; shared internal triangles are removed. The
#' output basis is an orthonormal basis of the hyperplane's direction space.
#' @inheritParams stat_parametric_3d
#' @param cells Four-column matrix of vertex indices for each tetrahedral cell.
#' @param normal Nonzero hyperplane normal in four dimensions.
#' @param offset Hyperplane offset using the supplied normal's scale.
#' @param basis Optional 4 by 3 orthonormal basis perpendicular to normal.
#' @param tolerance Absolute tolerance in normalised signed plane distance.
#' @return A mesh layer with cell identifiers.
#' @examples
#' v <- data.frame(x=c(0,1,0,0,0),y=c(0,0,1,0,0),z=c(0,0,0,1,0),w=c(0,0,0,0,1))
#' cells <- t(utils::combn(5,4))
#' ggplot2::ggplot(v,ggplot2::aes(x,y,z=z,w=w))+
#'   stat_section_4d(cells=cells,offset=.3)+coord_3d()
#' @export
stat_section_4d <- function(mapping=NULL,data=NULL,geom=GeomStatsSurface3D,position="identity",...,
                             cells,normal=c(0,0,0,1),offset=0,basis=NULL,tolerance=1e-10,
                             na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  offset <- mesh_number_3d(offset,"offset",is.finite,"finite")
  tolerance <- mesh_number_3d(tolerance,"tolerance",function(x) is.finite(x)&&x>0,"positive")
  make_layer_3d(geom,mapping,data,StatSection4D,position,
    stats_params_3d(list(cells=cells,normal=normal,offset=offset,basis=basis,tolerance=tolerance,na.rm=na.rm,shade=TRUE),list(...)),show.legend,inherit.aes)
}
