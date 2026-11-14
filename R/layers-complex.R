#' @include stats-common.R
NULL

complex_values_3d <- function(fun,z) {
  if(!is.function(fun)) stop("fun must be a vectorised complex function.",call.=FALSE)
  value <- fun(z)
  if(!(is.numeric(value)||is.complex(value))||!length(value)%in%c(1L,length(z))) stop("fun must return one complex or numeric value per input.",call.=FALSE)
  rep_len(as.complex(value),length(z))
}

sphere_complex_3d <- function(X) {
  z <- complex(real=X[,1L],imaginary=X[,2L])/(1-X[,3L])
  z[X[,3L]>=1-1e-14] <- complex(real=Inf)
  z
}

complex_sphere_3d <- function(u,v=rep(1+0i,length(u))) {
  scale <- pmax(Mod(u),Mod(v)); u <- u/scale; v <- v/scale
  den <- Mod(u)^2+Mod(v)^2
  cross <- u*Conj(v)
  cbind(x=2*Re(cross)/den,y=2*Im(cross)/den,z=(Mod(u)^2-Mod(v)^2)/den)
}

#' Mobius transformations of complex numbers, spheres and half-space
#'
#' Returns the map (a z + b)/(c z + d). On the Riemann sphere it acts on
#' complex homogeneous pairs, keeping the north pole finite in R^3. On the
#' upper half-space it uses the quaternionic extension to hyperbolic H^3.
#' These maps are conformal on the round sphere; the SU(2) subgroup preserves
#' its round metric. Every nonsingular complex matrix acts isometrically on H^3.
#' @param a,b,c,d Complex scalar matrix coefficients with nonzero determinant.
#' @return A function of z and model ("complex", "sphere" or "halfspace").
#'   Sphere and half-space inputs are matrices with three columns; complex
#'   inputs are vectors and may contain Inf. Half-space heights are positive.
#' @references Wegert (2010). Phase plots of complex functions.
#'   <https://arxiv.org/abs/1007.2295>.
#' @examples
#' f <- mobius(a=0,b=1,c=1,d=0)
#' f(c(0,1,Inf))
#' f(rbind(c(0,0,1),c(0,0,-1)),model="sphere")
#' @export
mobius <- function(a=1,b=0,c=0,d=1) {
  coefficients <- c(a,b,c,d)
  if(!(is.numeric(coefficients)||is.complex(coefficients))||length(coefficients)!=4L||any(!is.finite(coefficients))) stop("a, b, c and d must be finite complex scalars.",call.=FALSE)
  coefficients <- as.complex(coefficients)/max(Mod(coefficients))
  a <- coefficients[1L]; b <- coefficients[2L]; c <- coefficients[3L]; d <- coefficients[4L]
  determinant <- a*d-b*c
  if(!is.finite(determinant)||Mod(determinant)==0) stop("The Mobius matrix must have nonzero determinant.",call.=FALSE)
  function(z,model=c("complex","sphere","halfspace")) {
    model <- match.arg(model)
    if(model=="complex") {
      if(!(is.numeric(z)||is.complex(z))||anyNA(z)) stop("z must be a complex or numeric vector without missing values.",call.=FALSE)
      u <- as.complex(z); v <- rep(1+0i,length(u)); ideal <- !is.finite(u)
      u[ideal] <- 1; v[ideal] <- 0
      scale <- pmax(Mod(u),Mod(v)); u <- u/scale; v <- v/scale
    } else {
      z <- as.matrix(z)
      if(!is.numeric(z)||ncol(z)!=3L||any(!is.finite(z))) stop("Coordinates must be a finite three-column matrix.",call.=FALSE)
      if(model=="halfspace") {
        if(any(z[,3L]<=0)) stop("Half-space heights must be positive.",call.=FALSE)
        w <- complex(real=z[,1L],imaginary=z[,2L]); h <- z[,3L]
        den <- Mod(c*w+d)^2+Mod(c)^2*h^2
        out <- ((a*w+b)*Conj(c*w+d)+a*Conj(c)*h^2)/den
        return(cbind(x=Re(out),y=Im(out),z=Mod(determinant)*h/den))
      }
      if(any(abs(rowSums(z^2)-1)>1e-8)) stop("Sphere inputs must have unit norm.",call.=FALSE)
      u <- complex(real=z[,1L],imaginary=z[,2L]); v <- as.complex(1-z[,3L])
      north <- z[,3L]>=1-1e-14; u[north] <- 1; v[north] <- 0
    }
    first <- a*u+b*v; second <- c*u+d*v
    if(model=="sphere") return(complex_sphere_3d(first,second))
    out <- first/second; out[Mod(second)==0] <- complex(real=Inf); out
  }
}

phase_colour_3d <- function(value,contours) {
  modulus <- Mod(value); phase <- Arg(value)
  brightness <- if(contours) .65+.35*(log2(modulus)%%1) else rep(1,length(value))
  hue <- (phase/(2*pi))%%1
  invalid <- !is.finite(hue)|!is.finite(brightness)
  hue[invalid] <- 0; brightness[invalid] <- 0
  colour <- grDevices::hsv(hue,.85,brightness)
  colour[modulus==0 & !is.na(modulus)] <- "#000000"
  colour[is.infinite(modulus)] <- "#FFFFFF"
  colour[is.na(modulus)] <- "#808080"
  colour
}

StatRiemannSphere3D <- ggplot2::ggproto("StatRiemannSphere3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z"),
  compute_group=function(data,scales,fun,n=60L,contours=TRUE,transform=NULL) {
    sphere <- function(u,v) cbind(cos(u)*sin(v),sin(u)*sin(v),cos(v))
    if(!is.null(transform)) {
      original <- sphere
      sphere <- function(u,v) transform(original(u,v),model="sphere")
    }
    out <- parametric_surface_3d(sphere,c(0,2*pi),c(0,pi),c(n,max(3L,ceiling(n/2))),c(TRUE,FALSE),FALSE,list(),FALSE,.002,10,0L)
    X <- as.matrix(out[c("x","y","z")])
    centroid <- vapply(seq_len(3L),function(j) rep(rowMeans(matrix(X[,j],ncol=3,byrow=TRUE)),each=3L),numeric(nrow(out)))
    centroid <- centroid/sqrt(rowSums(centroid^2))
    value <- complex_values_3d(fun,sphere_complex_3d(centroid))
    out$arg <- Arg(value); out$mod <- Mod(value); out$domain_colour <- phase_colour_3d(value,contours)
    out[c("nx","ny","nz")] <- as.data.frame(X)
    out
  }
)

#' Domain colouring on the Riemann sphere
#'
#' Samples a complex function after inverse stereographic projection. Hue
#' encodes phase cyclically, and optional brightness bands mark powers of two
#' in modulus. The computed arg and mod variables support custom scales. The
#' north pole represents infinity at the finite point (0,0,1). Values are
#' sampled at projected triangle centroids and paint is constant per triangle.
#' Increase n with the raster backend for detailed phase portraits.
#' @inheritParams stat_parametric_3d
#' @param fun Vectorised complex function.
#' @param n Longitude resolution; latitude uses half as many samples.
#' @param contours Draw modulus bands in the domain colour.
#' @param transform Optional [mobius()] map applied to the sphere mesh.
#' @return A surface layer.
#' @references Wegert (2010). <https://arxiv.org/abs/1007.2295>.
#'   Poelke and Polthier (2009). Lifted domain coloring.
#'   <https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742>
#' @examples
#' ggplot2::ggplot()+stat_riemann_sphere_3d(fun=function(z) (z-1)/(z^2+1),n=24)+
#'   coord_3d(backend=backend_3d("raster"),axes=character(),panes=character(),box="none")
#' @export
stat_riemann_sphere_3d <- function(mapping=NULL,data=NULL,geom=GeomStatsSurface3D,position="identity",...,
                                   fun,n=60L,contours=TRUE,transform=NULL,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  if(!is.function(fun)) stop("fun must be a function.",call.=FALSE)
  if(!is.logical(contours)||length(contours)!=1L||is.na(contours)) stop("contours must be TRUE or FALSE.",call.=FALSE)
  if(!is.null(transform)&&!is.function(transform)) stop("transform must be a Mobius map.",call.=FALSE)
  mapping <- mapping %or3d% ggplot2::aes(fill=ggplot2::after_stat(I(domain_colour)))
  make_layer_3d(geom,mapping,stats_seed_data_3d(data),StatRiemannSphere3D,position,
    stats_params_3d(list(fun=fun,n=mesh_count_3d(n,"n",6L),contours=contours,transform=transform,
      na.rm=na.rm,shade=FALSE),list(...)),show.legend,inherit.aes)
}

riemann_parameter_3d <- function(u,v,domain) if(domain=="polar") u*exp(1i*v) else complex(real=u,imaginary=v)

StatRiemannSurface3D <- ggplot2::ggproto("StatRiemannSurface3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z"),
  compute_group=function(data,scales,fun_inverse,sheets=2L,domain="polar",u=c(0,1),v=c(0,2*pi),n=40L) {
    evaluate <- function(u,v) {
      w <- riemann_parameter_3d(u,v,domain); z <- complex_values_3d(fun_inverse,w)
      cbind(Re(z),Im(z),Re(w))
    }
    out <- parametric_surface_3d(evaluate,u,v,n,FALSE,FALSE,list(),FALSE,.002,10,0L,vertex_parameters=TRUE)
    w <- riemann_parameter_3d(out$u,out$v,domain)
    vertex <- riemann_parameter_3d(out$.u_vertex,out$.v_vertex,domain)
    out$w_re <- Re(vertex); out$w_im <- Im(vertex)
    out$re_w <- Re(w); out$im_w <- Im(w)
    out$sheet <- pmin(sheets,pmax(1L,1L+floor(sheets*(out$v-v[1L])/diff(v))))
    out
  }
)

#' Riemann surfaces parametrised by an inverse function
#'
#' For w in the parameter domain, draws (Re g(w), Im g(w), Re w), coloured
#' by Im w. For z=w^k, a full polar disk covers k sheets over z. For z=exp(w),
#' a rectangular domain with imaginary range spanning several turns covers
#' several logarithm sheets. sheets labels equal sectors of the v interval.
#' Crossings are resolved by the visibility engine. The supplied inverse g
#' must be single-valued and finite throughout the sampled parameter domain.
#' @inheritParams stat_parametric_3d
#' @param fun_inverse Vectorised single-valued complex function z=g(w).
#' @param sheets Number of sheet sectors, also sets the default imaginary range
#'   `[-pi*sheets, pi*sheets]` for a Cartesian domain.
#' @param domain Polar (u radius, v angle) or Cartesian (u real, v imaginary).
#' @param u,v Parameter ranges; v=NULL chooses the domain's default.
#' @return A surface layer with re_w, im_w and sheet computed variables.
#' @references Poelke and Polthier (2009). Lifted domain coloring.
#'   <https://diglib.eg.org/handle/10.1111/v28i3pp0735-0742>.
#' @examples
#' ggplot2::ggplot()+stat_riemann_surface_3d(fun_inverse=function(w) w^3,sheets=3,n=20)+coord_3d()
#' ggplot2::ggplot()+stat_riemann_surface_3d(fun_inverse=exp,sheets=2,domain="cartesian",u=c(-1,1),n=20)+coord_3d()
#' @export
stat_riemann_surface_3d <- function(mapping=NULL,data=NULL,geom=GeomStatsSurface3D,position="identity",...,
                                    fun_inverse,sheets=2L,domain=c("polar","cartesian"),u=c(0,1),v=NULL,n=40L,
                                    na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  sheets <- mesh_count_3d(sheets,"sheets",1L); domain <- match.arg(domain)
  v <- v %or3d% if(domain=="polar") c(0,2*pi) else c(-pi,pi)*sheets
  mapping <- mapping %or3d% ggplot2::aes(fill=ggplot2::after_stat(im_w))
  make_layer_3d(geom,mapping,stats_seed_data_3d(data),StatRiemannSurface3D,position,
    stats_params_3d(list(fun_inverse=fun_inverse,sheets=sheets,domain=domain,u=u,v=v,n=n,na.rm=na.rm,shade=TRUE),list(...)),show.legend,inherit.aes)
}
