#' @include space-3d.R frame-globe.R layers-mesh.R
NULL

quaternion_product_3d <- function(a,b) {
  c(a[4L]*b[1L]+a[1L]*b[4L]+a[2L]*b[3L]-a[3L]*b[2L],
    a[4L]*b[2L]-a[1L]*b[3L]+a[2L]*b[4L]+a[3L]*b[1L],
    a[4L]*b[3L]+a[1L]*b[2L]-a[2L]*b[1L]+a[3L]*b[4L],a[4L]*b[4L]-sum(a[1:3]*b[1:3]))
}

#' Rotations in four dimensions
#'
#' Returns an SO(4) matrix for a rotation in one of the six coordinate planes.
#' Alternatively, supply two unit quaternions (vector xyz, scalar w last)
#' for the double rotation q -> left*q*Conj(right). Row coordinates multiply
#' by the transpose of the matrix.
#' @param plane Two coordinate letters from x,y,z,w.
#' @param angle Rotation angle in radians.
#' @param left,right Unit quaternions specifying a double rotation.
#' @return A 4 by 4 orthogonal matrix with determinant one.
#' @examples
#' rotate_4d("xw",pi/4)
#' rotate_4d(left=c(0,0,sin(.2),cos(.2)),right=c(sin(.3),0,0,cos(.3)))
#' @export
rotate_4d <- function(plane="xw",angle=0,left=NULL,right=NULL) {
  if(!is.null(left)||!is.null(right)) {
    valid <- function(q) is.numeric(q)&&length(q)==4L&&all(is.finite(q))&&abs(sum(q^2)-1)<1e-10
    if(!valid(left)||!valid(right)) stop("left and right must both be unit quaternions.",call.=FALSE)
    conjugate <- right*c(-1,-1,-1,1)
    return(vapply(seq_len(4L),function(i) quaternion_product_3d(quaternion_product_3d(left,diag(4)[,i]),conjugate),numeric(4)))
  }
  if(!is.character(plane)||length(plane)!=1L||!plane%in%c("xy","xz","xw","yz","yw","zw")) stop("plane must be xy, xz, xw, yz, yw or zw.",call.=FALSE)
  angle <- mesh_number_3d(angle,"angle",is.finite,"finite")
  ij <- match(strsplit(plane,"",fixed=TRUE)[[1L]],c("x","y","z","w"))
  out <- diag(4); out[ij,ij] <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),2L); out
}

projection_basis_3d <- function(basis,p) {
  basis <- basis %or3d% diag(p)[,1:3,drop=FALSE]
  if(!is.matrix(basis)||!is.numeric(basis)||!identical(dim(basis),c(as.integer(p),3L))||any(!is.finite(basis))||max(abs(crossprod(basis)-diag(3)))>1e-8) stop("basis must be a p by 3 orthonormal frame.",call.=FALSE)
  basis
}

projection_normal_3d <- function(basis) qr.Q(qr(basis),complete=TRUE)[,4L]

# Homogeneous images (x, y, z, w): orthographic (Y, 1), four-dimensional
# perspective (eye Y, eye - depth) and stereographic (Y, 1 - depth). Both
# projective maps are linear from R^4 to P^3, so the eye hyperplane and the
# pole go to infinity (w = 0) and points behind the eye have w < 0.
project_nd_3d <- function(X,basis,projection,eye) {
  Y <- X%*%basis
  if(projection=="orthographic") return(cbind(Y,1))
  depth <- drop(X%*%projection_normal_3d(basis))
  scale <- if(projection=="perspective") eye else 1
  w <- scale-depth
  w[w<0&w>-64*.Machine$double.eps*scale] <- 0
  cbind(Y*scale,w)
}

# Finite images for training position scales; rows at or behind infinity
# take the centre of the finite rows.
project_nd_xyz_3d <- function(X,basis,projection,eye) {
  H <- project_nd_3d(X,basis,projection,eye)
  finite <- H[,4L]>0
  out <- H[,1:3,drop=FALSE]/H[,4L]
  if(any(!finite)) out[!finite,] <- matrix(if(any(finite)) colMeans(out[finite,,drop=FALSE]) else 0,sum(!finite),3L,byrow=TRUE)
  out
}

# Visible straight-segment parameters for planes n.X + c >= 0 (rows n, c).
linear_clip_segment_3d <- function(X,Y,planes) {
  k <- ncol(planes); lo <- 0; hi <- 1
  for(i in seq_len(nrow(planes))) {
    a <- sum(planes[i,-k]*X)+planes[i,k]; b <- sum(planes[i,-k]*Y)+planes[i,k]
    if(a<0&&b<0) return(matrix(numeric(),0L,2L))
    if(a<0) lo <- max(lo,a/(a-b)) else if(b<0) hi <- min(hi,a/(a-b))
  }
  if(hi<=lo) matrix(numeric(),0L,2L) else matrix(c(lo,hi),1L)
}

# Sutherland-Hodgman clipping of a straight-edged polygon, with corner weights.
linear_clip_polygon_3d <- function(X,weights,planes) {
  k <- ncol(planes)
  for(i in seq_len(nrow(planes))) {
    if(nrow(X)<3L) break
    value <- drop(X%*%planes[i,-k])+planes[i,k]
    following <- c(seq_len(nrow(X))[-1L],1L)
    points <- list(); values <- list()
    for(j in seq_len(nrow(X))) {
      a <- value[j]; b <- value[following[j]]
      if(a>=0) {points[[length(points)+1L]] <- X[j,]; values[[length(values)+1L]] <- weights[j,]}
      if((a<0&&b>0)||(a>0&&b<0)) {
        t <- a/(a-b)
        points[[length(points)+1L]] <- (1-t)*X[j,]+t*X[following[j],]
        values[[length(values)+1L]] <- (1-t)*weights[j,]+t*weights[following[j],]
      }
    }
    X <- if(length(points)) do.call(rbind,points) else matrix(numeric(),0L,ncol(X))
    weights <- if(length(values)) do.call(rbind,values) else matrix(numeric(),0L,ncol(weights))
  }
  list(points=X,weights=weights)
}

# Box planes lo w <= Y <= hi w pulled back to R^4 for the stereographic map,
# and the pole cap whose image lies outside twice the box circumradius.
projection_sphere_planes_3d <- function(basis,ranges) {
  n <- projection_normal_3d(basis)
  lo <- vapply(ranges,`[`,numeric(1),1L); hi <- vapply(ranges,`[`,numeric(1),2L)
  box <- do.call(rbind,lapply(1:3,function(j) rbind(c(basis[,j]+lo[j]*n,-lo[j]),c(-basis[,j]-hi[j]*n,hi[j]))))
  rbind(box,c(-n,cos(projection_pole_radius_3d(ranges))))
}

projection_pole_radius_3d <- function(ranges) 2*atan(1/(2*sqrt(sum(vapply(ranges,function(r) max(abs(r)),numeric(1))^2))))

SpaceProjectionND <- ggplot2::ggproto("SpaceProjectionND",Space3D,
  charts="ambient",models="projection",default_model="projection",basis=NULL,projection="orthographic",eye=4,
  to_canonical=function(self,data,chart) as.matrix(data[paste0(".nd",seq_len(nrow(self$basis)))]),
  from_canonical=function(self,X,model) project_nd_3d(X,self$basis,self$projection,self$eye),
  distance=function(X,Y) sqrt(rowSums((X-Y)^2)),
  geodesic=function(self,X,Y,s) if(self$projection=="stereographic") sphere_slerp_3d(X,Y,s) else segment_points_3d(X,Y,s),
  chart_is_projective=function(self,model) self$projection!="stereographic",
  eye_plane=function(self) c(-projection_normal_3d(self$basis),self$eye),
  clip_segment=function(self,X,Y,model,ranges) switch(self$projection,orthographic=matrix(c(0,1),1L),
    perspective=linear_clip_segment_3d(X,Y,matrix(self$eye_plane(),1L)),
    stereographic=spherical_clip_segment_3d(X,Y,projection_sphere_planes_3d(self$basis,ranges))),
  clip_polygon=function(self,X,model,ranges) if(self$projection=="perspective") linear_clip_polygon_3d(X,diag(nrow(X)),matrix(self$eye_plane(),1L)) else NULL,
  visible=function(self,X,model,ranges) {
    if(self$projection!="stereographic") return(rep(TRUE,nrow(X)))
    drop(X%*%projection_normal_3d(self$basis))<=cos(projection_pole_radius_3d(ranges))
  },
  cull=function(self,corners,model,ranges) {
    if(self$projection!="stereographic") return(rep(FALSE,nrow(corners[[1L]])))
    spherical_cull_3d(corners,projection_sphere_planes_3d(self$basis,ranges))
  },
  contains=function(self,X,model,ranges) {
    if(self$projection=="orthographic") return(rep(TRUE,nrow(X)))
    planes <- if(self$projection=="perspective") matrix(self$eye_plane(),1L) else projection_sphere_planes_3d(self$basis,ranges)
    value <- sweep(X%*%t(planes[,-ncol(planes),drop=FALSE]),2L,planes[,ncol(planes)],"+")
    if(self$projection=="perspective") drop(value)>0 else apply(value,1L,function(v) all(v>=-1e-12))
  },
  model_ranges=function(model,panel_params) panel_params$ranges
)

StatProject3D <- ggplot2::ggproto("StatProject3D",ggplot2::Stat,
  compute_panel=function(data,scales,dimensions=c("x","y","z","w"),basis=NULL,rotation=NULL,
                         projection="orthographic",eye=4,source_geom="GeomPoint") {
    if(any(!dimensions%in%names(data))) stop("Map every requested projection dimension.",call.=FALSE)
    X <- as.matrix(data[dimensions]); p <- ncol(X)
    if(!is.numeric(X)||any(!is.finite(X))) stop("Projection coordinates must be finite numeric values.",call.=FALSE)
    basis <- projection_basis_3d(basis,p)
    if(!is.null(rotation)) {
      if(!is.matrix(rotation)||!identical(dim(rotation),c(p,p))||any(!is.finite(rotation))||max(abs(crossprod(rotation)-diag(p)))>1e-8) stop("rotation must be a p by p orthogonal matrix.",call.=FALSE)
      X <- X%*%t(rotation)
    }
    if(projection!="orthographic"&&p!=4L) stop("Perspective and stereographic projections require four dimensions.",call.=FALSE)
    topology <- X
    if(projection=="stereographic") {
      r <- sqrt(rowSums(X^2)); if(any(r==0)) stop("Stereographic inputs must be nonzero.",call.=FALSE)
      X <- X/r
    }
    out <- data; out[paste0(".nd",seq_len(p))] <- as.data.frame(X)
    out[c("x","y","z")] <- as.data.frame(project_nd_xyz_3d(X,basis,projection,eye))
    if(source_geom=="GeomMesh3D") out <- mesh_face_materials_3d(out)
    unbounded <- project_nd_3d(X,basis,projection,eye)[,4L]<=0
    if(source_geom%in%c("GeomMesh3D","GeomPolygon")) out <- projection_surface_rows_3d(out,topology,source_geom,projection,unbounded)
    out
  }
)

GeomProject3D <- ggplot2::ggproto("GeomProject3D",ggplot2::GeomPoint)

emit_projection_nd_3d <- function(data,params,panel_params) {
  entry <- .emitters_3d[[data$.projection_geom[1L] %or3d% params$source_geom]]
  entry$emit(data,params,panel_params)
}

register_geom_3d(GeomProject3D,emit_projection_nd_3d,positions=character(),
  parameters=c(surface_parameters_3d,list(dimensions=c("x","y","z","w"),basis=NULL,projection="orthographic",eye=4,source_geom="GeomPoint",munch_tolerance=1e-3,munch_max_depth=9L)),
  space=function(data,params,panel_params) {
    space <- ggplot2::ggproto(NULL,SpaceProjectionND,basis=projection_basis_3d(params$basis,length(params$dimensions)),projection=params$projection,eye=params$eye)
    space_spec_3d(space,"projection","ambient",NULL,params$munch_tolerance,params$munch_max_depth)
  },
  prepare_data=function(data) if(inherits(data,"ggplot3d_mesh")||inherits(data,"mesh3d")||all(c(".face",".vertex",".corner")%in%names(data))) prepare_mesh_data_3d(data) else data,
  prepare_aesthetics=function(out,data,plot,params) {
    if(!all(c(".face",".vertex",".corner")%in%names(data))) return(out)
    out <- prepare_mesh_aesthetics_3d(out,data,plot,params)
    for(axis in params$dimensions) if(is.null(out[[axis]])&&!is.null(data[[axis]])) out[[axis]] <- data[[axis]]
    out
  })

#' Project four-dimensional and higher-dimensional data into three dimensions
#'
#' Orthographic projection multiplies scale-transformed coordinates by a p by
#' 3 orthonormal basis. A four-dimensional perspective divides by eye minus
#' the orthogonal fourth coordinate. Finite projections preserve straight
#' edges and planar faces. Stereographic projection first normalises points
#' to S^3 and tessellates spherical geodesic edges and triangular faces through
#' the shared space adapter. Both projective maps are linear from R^4 to
#' homogeneous P^3 coordinates: the perspective eye hyperplane and the
#' stereographic pole go to infinity. Perspective edges and faces are clipped
#' to the half-space in front of the eye in R^4, where the cut vertices become
#' explicit ideal points (w = 0), and the scene engine clips them to the panel
#' box before division; points behind the eye are dropped. Stereographic
#' arcs are clipped on their great circles against the box planes pulled back
#' to R^4, and faces are sampled outside a cap around the pole whose image lies
#' outside the box. Position scales train on the finite images, so set limits
#' when geometry approaches the eye or the pole.
#' Stereographic polygon faces are triangulated in their ambient plane, then
#' tessellated on S^3. Collapsed projective faces are omitted. Map dimensions by
#' name; dimensions lists the aesthetic names in matrix row order.
#' @inheritParams geom_mesh_3d
#' @inheritParams ggplot2::stat_identity
#' @param geom Registered geom prototype, or "point", "path", "polygon" or "mesh".
#' @param basis Projection frame, NULL for the first three coordinate axes.
#' @param rotation Optional p by p orthogonal matrix, such as [rotate_4d()].
#' @param dimensions Input aesthetic names, default x,y,z,w.
#' @param projection Orthographic, perspective or stereographic.
#' @param eye Positive fourth-axis eye distance for perspective projection.
#' @param munch_tolerance,munch_max_depth Shared subdivision controls.
#' @return A projection layer.
#' @references Wickham, Cook, Hofmann and Buja (2011). tourr.
#' \doi{10.18637/jss.v040.i02}. Stella4D: <https://www.software3d.com/TutePaper4D.php>.
#' @examples
#' d <- expand.grid(x=c(-1,1),y=c(-1,1),z=c(-1,1),w=c(-1,1))
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z,w=w))+
#'   stat_project_3d(rotation=rotate_4d("xw",.5),size=3)+coord_3d()
#' @export
stat_project_3d <- function(mapping=NULL,data=NULL,geom="point",position="identity",...,basis=NULL,rotation=NULL,
                             dimensions=c("x","y","z","w"),projection=c("orthographic","perspective","stereographic"),eye=4,
                             munch_tolerance=1e-3,munch_max_depth=9L,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  if(!is.character(dimensions)||length(dimensions)<3L||anyNA(dimensions)||anyDuplicated(dimensions)) stop("dimensions must give at least three unique aesthetic names.",call.=FALSE)
  projection <- match.arg(projection)
  if(is.character(geom)) geom <- switch(geom,point=ggplot2::GeomPoint,path=ggplot2::GeomPath,polygon=ggplot2::GeomPolygon,mesh=GeomMesh3D,stop("Unknown projection geom.",call.=FALSE))
  name <- find_emitter_3d(geom)$name
  if(!name%in%c("GeomPoint","GeomPath","GeomLine","GeomPolygon","GeomMesh3D")) stop("Projection supports point, path, polygon and mesh geometry.",call.=FALSE)
  eye <- mesh_number_3d(eye,"eye",function(x) is.finite(x)&&x>0,"positive")
  basis <- projection_basis_3d(basis,length(dimensions))
  spec <- space_spec_3d(SpaceProjectionND,"projection","ambient",NULL,munch_tolerance,munch_max_depth)
  stat <- ggplot2::ggproto(NULL,StatProject3D,required_aes=dimensions)
  parent <- geom
  optional <- unique(c(parent$optional_aes,dimensions,paste0(".nd",seq_along(dimensions))))
  geom <- ggplot2::ggproto("GeomProject3D",parent,optional_aes=optional)
  make_layer_3d(geom,mapping,data,stat,position,
    c(list(dimensions=dimensions,basis=basis,rotation=rotation,projection=projection,eye=eye,source_geom=name,
      munch_tolerance=spec$tolerance,munch_max_depth=spec$max_depth,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}


# Stereographic faces, and every face of a perspective layer in which some
# face has a corner at or behind the eye (no finite image to triangulate), are
# triangulated in their ambient planes.
projection_surface_rows_3d <- function(data, X, source_geom, projection, unbounded) {
  if(source_geom=="GeomPolygon") {
    data$.mesh <- 1L; data$.face <- data$group
    data$.corner <- seq_len(nrow(data)); data$.vertex <- seq_len(nrow(data))
  }
  keys <- interaction(data$.mesh,data$.face,data$group,drop=TRUE)
  groups <- split(seq_len(nrow(data)),keys)
  if(projection!="stereographic") {
    unbounded_face <- vapply(groups,function(rows) any(unbounded[rows]),logical(1))
    flat <- vapply(groups,function(rows) {
      xyz <- as.matrix(data[rows,c("x","y","z")])
      singular <- svd(sweep(xyz,2L,colMeans(xyz),"-"),nu=0,nv=0)$d
      length(singular)>=2L && singular[2L]>1024*.Machine$double.eps*singular[1L]
    },logical(1))
    groups <- groups[flat|unbounded_face]
    if(!any(unbounded_face)) return(data[unlist(groups,use.names=FALSE),,drop=FALSE])
  }
  pieces <- list(); next_face <- 0L
  for(rows in groups) {
    rows <- rows[order(data$.corner[rows])]
    centered <- sweep(X[rows,,drop=FALSE],2L,colMeans(X[rows,,drop=FALSE]),"-")
    decomposition <- svd(centered,nu=0,nv=2L)
    if(length(decomposition$d)>2L && decomposition$d[3L]>1e-8*max(1,decomposition$d[1L])) stop("Ambient polygon faces must be planar before stereographic projection.",call.=FALSE)
    uv <- centered%*%decomposition$v
    flat <- data[rows,,drop=FALSE]; flat[c("x","y","z")] <- as.data.frame(cbind(uv,0))
    rings <- split(seq_along(rows),flat$subgroup %or3d% rep(1L,length(rows)))
    triangles <- mesh_face_triangles_3d(flat,seq_along(rows),rings)
    if(!nrow(triangles)) next
    out <- data[rows[as.vector(t(triangles))],,drop=FALSE]
    out$.face <- rep(next_face+seq_len(nrow(triangles)),each=3L)
    next_face <- next_face+nrow(triangles)
    out$.corner <- rep(1:3,nrow(triangles)); out$subgroup <- NULL
    out$.projection_geom <- "GeomMesh3D"
    pieces[[length(pieces)+1L]] <- out
  }
  if(length(pieces)) do.call(rbind,pieces) else data[FALSE,,drop=FALSE]
}
