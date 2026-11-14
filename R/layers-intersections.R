#' @include layers-surface.R stats-common.R layers-mesh.R
NULL

utils::globalVariables("geometry_intersections_cpp")

intersection_paths_3d <- function(d, tolerance) {
  if (!nrow(d)) return(d)
  key <- apply(round(as.matrix(d[c("x", "y", "z")]) / tolerance), 1L, paste, collapse = ":")
  nodes <- match(key, unique(key)); ends <- matrix(nodes, ncol = 2L, byrow = TRUE)
  keep <- !duplicated(cbind(pmin(ends[,1L], ends[,2L]), pmax(ends[,1L], ends[,2L]))) & ends[,1L] != ends[,2L]
  ends <- ends[keep,,drop=FALSE]; rows <- which(keep) * 2L - 1L
  adjacency <- split(rep(seq_len(nrow(ends)), 2L), as.vector(ends))
  degree <- lengths(adjacency); used <- rep(FALSE,nrow(ends)); paths <- list()
  walk <- function(start, edge) {
    at <- start; vertices <- start; source <- integer()
    repeat {
      used[edge] <<- TRUE; source <- c(source, rows[edge])
      at <- ends[edge, if (ends[edge,1L] == at) 2L else 1L]
      vertices <- c(vertices, at)
      next_edges <- adjacency[[as.character(at)]]
      next_edges <- next_edges[!used[next_edges]]
      if (degree[[as.character(at)]] != 2L || !length(next_edges)) break
      edge <- next_edges[1L]
    }
    out <- d[c(source, tail(source,1L)),,drop=FALSE]
    out[c("x","y","z")] <- d[match(vertices,nodes),c("x","y","z")]
    out$group <- length(paths)+1L
    paths[[length(paths)+1L]] <<- out
  }
  for (node in names(degree)[degree != 2L]) for (edge in adjacency[[node]]) if (!used[edge]) walk(as.integer(node),edge)
  for (edge in seq_len(nrow(ends))) if (!used[edge]) walk(ends[edge,1L],edge)
  do.call(rbind,paths)
}

mesh_intersection_curves_3d <- function(data, tolerance) {
  if (is.null(data$.mesh)) data$.mesh <- 1L
  data <- data[order(data$.mesh,data$.face,data$.corner),,drop=FALSE]
  # Distinct meshes have independent vertex and face key domains.
  data$.vertex <- match(paste(data$.mesh,data$.vertex), unique(paste(data$.mesh,data$.vertex)))
  data$.face <- match(paste(data$.mesh,data$.face),unique(paste(data$.mesh,data$.face)))
  a <- mesh_arrays_3d(data)
  scale <- max(apply(a$xyz,2L,function(x) diff(range(x))))
  tolerance <- tolerance * if (scale > 0) scale else 1
  out <- geometry_intersections_cpp(a$xyz,a$triangles,as.integer(a$parent),tolerance)
  intersection_paths_3d(out,tolerance)
}

StatSelfIntersection3D <- ggplot2::ggproto("StatSelfIntersection3D", StatMesh3D,
  compute_panel = function(data,scales,tolerance=1e-9) mesh_intersection_curves_3d(data,tolerance)
)

GeomIntersection3D <- ggplot2::ggproto("GeomIntersection3D", ggplot2::GeomPath)
register_geom_3d(GeomIntersection3D,emit_paths_3d,
  prepare_data=prepare_mesh_data_3d,prepare_aesthetics=prepare_mesh_aesthetics_3d)

#' Transverse intersection curves of a triangle mesh
#'
#' Intersects pairs of triangles after a bounding-box sweep, clips each to the
#' other triangle's plane and overlaps their intervals on the common line.
#' Manifold shared edges and isolated tangencies are omitted. Welded double
#' curves with more than two incident faces are retained. Coplanar overlaps are
#' two-dimensional and have no transverse curve. Zero-area faces are ignored.
#' Endpoints are welded on a tolerance grid, then stitched through degree-two
#' vertices; junctions start separate paths. Computed face1 and face2 identify
#' the contributing faces in the order of the keyed input table.
#' @inheritParams geom_mesh_3d
#' @param tolerance Positive positional tolerance relative to the mesh extent.
#' @return A ggplot2 layer drawing paths.
#' @references van Wijk and Cohen (2006). Visualization of Seifert Surfaces.
#'   IEEE TVCG 12, 485-496. \doi{10.1109/TVCG.2006.83}
#' @examples
#' v <- data.frame(x=c(-1,1,0,0,0,0),y=c(-1,-1,1,-1,1,0),z=c(0,0,0,-1,-1,1))
#' m <- mesh_3d(v,matrix(1:6,2,byrow=TRUE))
#' ggplot2::ggplot(m) + geom_mesh_3d(fill="grey80") +
#'   stat_self_intersection_3d(colour="red",linewidth=1) + coord_3d()
#' @export
stat_self_intersection_3d <- function(mapping=NULL,data=NULL,position="identity",...,
                                      tolerance=1e-9,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  tolerance <- mesh_number_3d(tolerance,"tolerance",function(x) is.finite(x)&&x>0,"positive")
  make_layer_3d(GeomIntersection3D,mapping,data,StatSelfIntersection3D,position,
    c(list(tolerance=tolerance,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}

immersed_recipes_3d <- list(
  klein_figure8=list(fun=function(u,v) {
    r <- 2 + sin(v)*cos(u/2) - sin(2*v)*sin(u/2)/2
    list(x=r*cos(u),y=r*sin(u),z=sin(u/2)*sin(v)+cos(u/2)*sin(2*v)/2)
  },u=c(0,2*pi),v=c(0,2*pi),closed=TRUE,twist=c(TRUE,FALSE)),
  klein_bottle=list(fun=function(u,v) {
    r <- 4*(1-cos(u)/2)
    list(x=ifelse(u<pi,6*cos(u)*(1+sin(u))+r*cos(u)*cos(v),6*cos(u)*(1+sin(u))+r*cos(v+pi)),
      y=ifelse(u<pi,16*sin(u)+r*sin(u)*cos(v),16*sin(u)),z=r*sin(v))
  },u=c(0,2*pi),v=c(0,2*pi),closed=TRUE,twist=FALSE),
  boy=list(fun=function(u,v) {
    X <- cos(u)*sin(v); Y <- sin(u)*sin(v); Z <- cos(v)
    list(x=(2*X^2-Y^2-Z^2+2*Y*Z*(Y^2-Z^2)+Z*X*(X^2-Z^2)+X*Y*(Y^2-X^2))/2,
      y=sqrt(3)/2*(Y^2-Z^2+Z*X*(Z^2-X^2)+X*Y*(Y^2-X^2)),
      z=(X+Y+Z)*((X+Y+Z)^3+4*(Y-X)*(Z-Y)*(X-Z))/8)
  },u=c(0,pi),v=c(0,pi),closed=TRUE,twist=TRUE),
  cross_cap=list(fun=function(u,v) list(x=cos(u)*sin(2*v),y=sin(u)*sin(2*v),z=cos(v)^2-cos(u)^2*sin(v)^2),
    u=c(0,pi),v=c(0,pi),closed=TRUE,twist=TRUE),
  moebius=list(fun=function(u,v) list(x=(1+v*cos(u/2)/2)*cos(u),y=(1+v*cos(u/2)/2)*sin(u),z=v*sin(u/2)/2),
    u=c(0,2*pi),v=c(-1,1),closed=c(TRUE,FALSE),twist=c(TRUE,FALSE))
)

#' Immersed surface examples
#'
#' Generates a figure-eight Klein bottle, a bottle immersion, Boy's surface,
#' a cross-cap with pinch points, or a Moebius band. Uses the indexed and
#' twisted parameter seams of [stat_parametric_3d()]. Lighting is two-sided.
#' @param type Surface name.
#' @param n Samples per parameter, a scalar or pair of integers.
#' @param ... Arguments to [stat_parametric_3d()].
#' @return A surface layer.
#' @references VTK parametric surface definitions:
#' <https://vtk.org/doc/nightly/html/classvtkParametricBoy.html>,
#' <https://vtk.org/doc/nightly/html/classvtkParametricFigure8Klein.html>.
#' @examples
#' ggplot2::ggplot() + stat_immersed_3d(type="klein_figure8",n=c(24,16)) + coord_3d()
#' @export
stat_immersed_3d <- function(type=c("klein_figure8","klein_bottle","boy","cross_cap","moebius"),n=40L,...) {
  do.call(stat_parametric_3d,c(immersed_recipes_3d[[match.arg(type)]],list(n=n),list(...)))
}

knot_curve_3d <- function(p,q,n,major,minor) {
  t <- 2*pi*(seq_len(n)-1)/n
  data.frame(x=(major+minor*cos(q*t))*cos(p*t),y=(major+minor*cos(q*t))*sin(p*t),z=minor*sin(q*t),t=t)
}

StatKnot3D <- ggplot2::ggproto("StatKnot3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z"),
  compute_group=function(data,scales,p=2L,q=3L,n=200L,major=2,minor=0.7,radius=NULL,sides=12L) {
    d <- knot_curve_3d(p,q,n,major,minor)
    if(is.null(radius)) return(d[c(seq_len(n),1L),,drop=FALSE])
    sweep_surface_3d(d,rep(radius,n),rep(0,n),sweep_profile_3d(NULL,sides,TRUE),TRUE,"none",NULL)
  }
)

#' Torus knots and knot tubes
#'
#' A (p,q) torus knot winds p times around the central axis and q times
#' around the tube. The integers must be coprime. A positive radius sweeps
#' a closed tube using the shared rotation-minimising frame.
#' @inheritParams stat_parametric_3d
#' @param type Knot family, currently "torus".
#' @param p,q Coprime nonzero integer winding numbers.
#' @param n Number of samples around the closed knot.
#' @param major,minor Major and minor radii of the supporting torus.
#' @param radius Tube radius; NULL draws a path.
#' @param sides Number of sides of a tube.
#' @return A path or surface layer.
#' @references van Wijk and Cohen (2006). \doi{10.1109/TVCG.2006.83}.
#' @examples
#' ggplot2::ggplot() + stat_knot_3d(p=2,q=3,radius=.12,n=80,sides=8) + coord_3d()
#' @export
stat_knot_3d <- function(mapping=NULL,data=NULL,position="identity",...,type="torus",p=2L,q=3L,n=200L,
                         major=2,minor=.7,radius=NULL,sides=12L,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  if(!identical(type,"torus")) stop("type must be 'torus'.",call.=FALSE)
  integer_winding <- function(x) is.numeric(x)&&length(x)==1L&&is.finite(x)&&x!=0&&x==trunc(x)
  if(!integer_winding(p)||!integer_winding(q)) stop("p and q must be nonzero integers.",call.=FALSE)
  a <- abs(p); b <- abs(q); while(b!=0) {r <- a%%b; a <- b; b <- r}
  if(a!=1) stop("p and q must be coprime for a knot.",call.=FALSE)
  major <- mesh_number_3d(major,"major",function(x) is.finite(x)&&x>0,"positive")
  minor <- mesh_number_3d(minor,"minor",function(x) is.finite(x)&&x>0&&x<major,"positive and smaller than major")
  if(!is.null(radius)) radius <- mesh_number_3d(radius,"radius",function(x) is.finite(x)&&x>0,"positive")
  make_layer_3d(if(is.null(radius)) ggplot2::GeomPath else GeomStatsSurface3D,mapping,stats_seed_data_3d(data),StatKnot3D,position,
    stats_params_3d(list(p=p,q=q,n=mesh_count_3d(n,"n",8L),major=major,minor=minor,radius=radius,
      sides=mesh_count_3d(sides,"sides",3L),na.rm=na.rm,shade=TRUE),list(...)),show.legend,inherit.aes)
}
