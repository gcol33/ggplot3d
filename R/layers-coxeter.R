#' @include space-3d.R frame-globe.R layers-surface.R stats-common.R
NULL

coxeter_key_3d <- function(x) paste(round(x,8L),collapse=":")

# Simplex points are finite (timelike), ideal (null) or hyperideal (spacelike)
# by the sign of their Lorentz norm.
coxeter_point_kind_3d <- function(norm,scale) ifelse(abs(norm)<=1e-9*scale,"ideal",ifelse(norm<0,"finite","hyperideal"))

coxeter_simplex_3d <- function(schlafli) {
  d <- length(schlafli); rank <- d+1L
  if(!d%in%c(2L,3L)||!is.numeric(schlafli)||any(is.na(schlafli)|schlafli<3|schlafli!=trunc(schlafli))) stop("schlafli must give two or three integers >=3 (or Inf).",call.=FALSE)
  G <- diag(rank)
  for(i in seq_len(d)) G[i,i+1L] <- G[i+1L,i] <- -cos(pi/schlafli[i])
  eig <- eigen(G,symmetric=TRUE); values <- eig$values
  kind <- if(min(values)>1e-10) "spherical" else if(min(values)>=-1e-10) "euclidean" else "hyperbolic"
  if(sum(values< -1e-10)>1L) stop("The Coxeter Gram matrix has more than one negative direction.",call.=FALSE)
  points <- rep("finite",rank)
  if(kind=="euclidean") {
    positive <- which(values>1e-10)
    if(length(positive)!=d) stop("Degenerate Coxeter simplex.",call.=FALSE)
    B <- diag(sqrt(values[positive]),d)%*%t(eig$vectors[,positive,drop=FALSE])
    offsets <- c(rep(0,d),1)
    mirrors <- lapply(seq_len(rank),function(i) {
      M <- diag(rank); n <- B[,i]
      M[seq_len(d),seq_len(d)] <- diag(d)-2*tcrossprod(n)
      M[seq_len(d),rank] <- 2*offsets[i]*n; M
    })
    V <- t(vapply(seq_len(rank),function(i) c(solve(t(B[,-i,drop=FALSE]),offsets[-i]),1),numeric(rank)))
    J <- diag(rank)
  } else {
    J <- diag(if(kind=="spherical") rep(1,rank) else c(rep(1,d),-1))
    B <- diag(sqrt(abs(values)),rank)%*%t(eig$vectors)
    mirrors <- lapply(seq_len(rank),function(i) diag(rank)-2*B[,i,drop=FALSE]%*%t(B[,i,drop=FALSE])%*%J)
    V <- t(solve(t(B)%*%J))
    if(kind=="hyperbolic") points <- coxeter_point_kind_3d(rowSums((V%*%J)*V),rowSums(V^2))
    V <- t(apply(V,1L,coxeter_normalise_3d,list(kind=kind,J=J)))
  }
  # The display is centred on the cell centre, or on the seed face (edge for
  # tilings) when the cell is not compact.
  centre_index <- if(points[rank]=="finite") rank else d
  center <- V[centre_index,]
  if(kind=="hyperbolic") {
    spatial <- center[seq_len(d)]; r <- sqrt(sum(spatial^2)); direction <- if(r>0) spatial/r else spatial
    T <- diag(rank)
    if(r>0) {
      T[seq_len(d),seq_len(d)] <- diag(d)+(center[rank]-1)*tcrossprod(direction)
      T[seq_len(d),rank] <- T[rank,seq_len(d)] <- -r*direction; T[rank,rank] <- center[rank]
    }
  } else if(kind=="spherical") {
    pole <- c(rep(0,d),1); v <- center-pole
    T <- if(sum(v^2)<1e-20) diag(rank) else diag(rank)-2*tcrossprod(v)/sum(v^2)
  } else {T <- diag(rank); T[seq_len(d),rank] <- -center[seq_len(d)]}
  inv <- solve(T,diag(rank))
  simplex <- list(d=d,kind=kind,G=G,J=J,mirrors=lapply(mirrors,function(M) T%*%M%*%inv),symbol=schlafli,points=points,centre_index=centre_index)
  simplex$vertices <- t(apply(V%*%t(T),1L,coxeter_normalise_3d,simplex))
  simplex
}

# Canonical representatives: Euclidean points with last coordinate one, unit
# vectors on spheres, future hyperboloid points, future null vectors with
# last coordinate one (ideal), and unit spacelike vectors with a fixed sign
# (hyperideal; the sign carries no geometry).
coxeter_normalise_3d <- function(x,simplex) {
  n <- length(x)
  if(simplex$kind=="euclidean") return(x/x[n])
  norm <- sum((x%*%simplex$J)*x)
  if(simplex$kind=="spherical") return(x/sqrt(norm))
  if(abs(norm)<=1e-9*sum(x^2)) return(x/x[n])
  out <- x/sqrt(abs(norm))
  lead <- if(norm<0||abs(out[n])>1e-12) out[n] else out[which(abs(out)>1e-12)[1L]]
  if(lead<0) -out else out
}

coxeter_orbit_3d <- function(seed,mirrors,simplex,max_count) {
  points <- list(seed); seen <- new.env(hash=TRUE,parent=emptyenv()); seen[[coxeter_key_3d(seed)]] <- TRUE
  i <- 1L
  while(i<=length(points)) {
    for(M in mirrors) {
      point <- coxeter_normalise_3d(drop(M%*%points[[i]]),simplex); key <- coxeter_key_3d(point)
      if(!exists(key,seen,inherits=FALSE)) {
        if(length(points)>=max_count) stop("The Coxeter orbit exceeds max_cells; increase the explicit resource budget.",call.=FALSE)
        points[[length(points)+1L]] <- point; seen[[key]] <- TRUE
      }
    }
    i <- i+1L
  }
  do.call(rbind,points)
}

# Breadth-first images of a seed point, with the group elements reaching them
# and their reflection depth; keep() bounds the search.
coxeter_transforms_3d <- function(seed,mirrors,simplex,keep,max_count,overflow) {
  transforms <- list(diag(length(seed))); points <- list(seed); depth <- 0L
  seen <- new.env(hash=TRUE,parent=emptyenv()); seen[[coxeter_key_3d(seed)]] <- TRUE
  i <- 1L
  while(i<=length(transforms)) {
    for(M in mirrors) {
      T <- M%*%transforms[[i]]; candidate <- coxeter_normalise_3d(drop(T%*%seed),simplex)
      if(!keep(candidate)) next
      key <- coxeter_key_3d(candidate)
      if(!exists(key,seen,inherits=FALSE)) {
        if(length(transforms)>=max_count) stop(overflow,call.=FALSE)
        transforms[[length(transforms)+1L]] <- T; points[[length(points)+1L]] <- candidate
        depth <- c(depth,depth[i]+1L); seen[[key]] <- TRUE
      }
    }
    i <- i+1L
  }
  list(transforms=transforms,points=do.call(rbind,points),depth=depth)
}

coxeter_ring_order_3d <- function(points) {
  centered <- sweep(points,2L,colMeans(points),"-")
  basis <- svd(centered,nu=0,nv=2L)$v
  xy <- centered%*%basis
  order(atan2(xy[,2L],xy[,1L]))
}

coxeter_distance_3d <- function(x,simplex) {
  rank <- length(x); d <- rank-1L
  if(simplex$kind=="hyperbolic") acosh(max(1,x[rank])) else
    if(simplex$kind=="spherical") acos(max(-1,min(1,x[rank]))) else sqrt(sum(x[seq_len(d)]^2))
}

# Truncation of a hyperideal vertex a by its polar plane <X, a> = 0 meets the
# edge towards b at b - <a, b> a, a finite point when |<a, b>| > 1, that is
# when the edge's line enters hyperbolic space beyond both polar planes.
coxeter_truncation_point_3d <- function(a,b,simplex) {
  product <- sum((a%*%simplex$J)*b)
  if(abs(product)<=1+1e-9) stop("Polar planes of adjacent hyperideal vertices meet; the truncation is not defined.",call.=FALSE)
  coxeter_normalise_3d(b-product*a,simplex)
}

# A polygon whose hyperideal corners are cut by their polar planes.
coxeter_truncate_polygon_3d <- function(X,simplex) {
  n <- nrow(X); out <- list()
  for(k in seq_len(n)) {
    previous <- X[if(k==1L) n else k-1L,]; following <- X[if(k==n) 1L else k+1L,]
    out <- c(out,list(coxeter_truncation_point_3d(X[k,],previous,simplex),coxeter_truncation_point_3d(X[k,],following,simplex)))
  }
  do.call(rbind,out)
}

# The face that truncation leaves in the polar plane of vertex a, from the
# far ends of the edges at a.
coxeter_truncation_face_3d <- function(a,neighbours,simplex) {
  P <- t(apply(neighbours,1L,function(b) coxeter_truncation_point_3d(a,b,simplex)))
  P[coxeter_ring_order_3d(P),,drop=FALSE]
}

# The seed polygon: the orbit of the vertex under the first two mirrors, in
# ring order. For a tiling it is the seed cell, for a honeycomb a cell face.
coxeter_seed_face_3d <- function(simplex) {
  p <- simplex$symbol[1L]
  if(!is.finite(p)) return(NULL)
  ring <- coxeter_orbit_3d(simplex$vertices[1L,],simplex$mirrors[1:2],simplex,p)
  ring[coxeter_ring_order_3d(ring),,drop=FALSE]
}

# Edges at the seed vertex inside one cell end at the images of the second
# edge endpoint under the vertex's stabiliser in the cell.
coxeter_seed_neighbours_3d <- function(simplex) {
  far <- coxeter_normalise_3d(drop(simplex$mirrors[[1L]]%*%simplex$vertices[1L,]),simplex)
  if(simplex$d==2L) return(rbind(far,coxeter_normalise_3d(drop(simplex$mirrors[[2L]]%*%far),simplex)))
  coxeter_orbit_3d(far,simplex$mirrors[2:3],simplex,max(3,simplex$symbol[2L]))
}

# Index a list of canonical polygons into one vertex table.
coxeter_index_3d <- function(polygons) {
  keys <- character(); points <- list()
  faces <- lapply(polygons,function(X) vapply(seq_len(nrow(X)),function(i) {
    key <- coxeter_key_3d(X[i,]); index <- match(key,keys)
    if(is.na(index)) {keys <<- c(keys,key); points[[length(points)+1L]] <<- X[i,]; index <- length(keys)}
    index
  },integer(1)))
  list(vertices=do.call(rbind,points),faces=faces)
}

coxeter_cells_3d <- function(schlafli,radius,max_cells) {
  simplex <- coxeter_simplex_3d(schlafli); d <- simplex$d; rank <- d+1L
  hyperideal <- simplex$points[1L]=="hyperideal"
  inside <- function(x) coxeter_distance_3d(x,simplex)<=radius+1e-8
  if(simplex$points[rank]!="finite") return(coxeter_faces_3d(simplex,radius,max_cells))
  seed <- coxeter_seed_face_3d(simplex)
  polygons <- if(d==2L) list(seed) else {
    images <- coxeter_transforms_3d(simplex$vertices[d,],simplex$mirrors[seq_len(d)],simplex,function(x) TRUE,64L,"Degenerate Coxeter cell.")
    lapply(images$transforms,function(T) t(apply(seed%*%t(T),1L,coxeter_normalise_3d,simplex)))
  }
  if(hyperideal) {
    corners <- coxeter_index_3d(polygons)
    neighbours <- coxeter_seed_neighbours_3d(simplex)
    stabiliser <- coxeter_transforms_3d(simplex$vertices[1L,],simplex$mirrors[seq_len(d)],simplex,function(x) TRUE,256L,"Degenerate Coxeter cell.")
    cuts <- lapply(stabiliser$transforms,function(T) coxeter_truncation_face_3d(drop(T%*%simplex$vertices[1L,]),
      t(apply(neighbours%*%t(T),1L,coxeter_normalise_3d,simplex)),simplex))
    polygons <- c(lapply(polygons,coxeter_truncate_polygon_3d,simplex),cuts)
  }
  indexed <- coxeter_index_3d(polygons)
  if(any(lengths(indexed$faces)<3L)) stop("The Coxeter cell has a degenerate facet.",call.=FALSE)
  center <- simplex$vertices[rank,]
  cells <- coxeter_transforms_3d(center,simplex$mirrors,simplex,inside,max_cells,
    "The radius requires more than max_cells; increase the explicit resource budget.")
  message(length(cells$transforms)," cells in ",simplex$kind," ",d,"-space")
  list(simplex=simplex,vertices=indexed$vertices,faces=indexed$faces,transforms=cells$transforms,centers=cells$points,depth=cells$depth,mode="cells")
}

# Honeycombs whose cells are not compact (ideal or hyperideal cell centres)
# have infinitely many faces per cell. Faces, edges for tilings, and the
# truncation faces of hyperideal vertices are generated as orbits of their
# seed pieces, retained within radius of the seed by their finite centres.
coxeter_faces_3d <- function(simplex,radius,max_cells) {
  d <- simplex$d; rank <- d+1L
  inside <- function(x) coxeter_distance_3d(x,simplex)<=radius+1e-8
  overflow <- "The radius requires more than max_cells; increase the explicit resource budget."
  hyperideal <- simplex$points[1L]=="hyperideal"
  cell <- simplex$vertices[rank,]
  swap <- simplex$mirrors[[rank]]
  seed <- if(d==2L) {
    if(hyperideal) stop("Hyperideal vertices of a tiling are not supported.",call.=FALSE)
    rbind(simplex$vertices[1L,],coxeter_normalise_3d(drop(simplex$mirrors[[1L]]%*%simplex$vertices[1L,]),simplex))
  } else coxeter_seed_face_3d(simplex)
  if(is.null(seed)) stop("Apeirogonal faces have no finite boundary.",call.=FALSE)
  pieces <- list(list(X=if(hyperideal) coxeter_truncate_polygon_3d(seed,simplex) else seed,cells=list(cell,drop(swap%*%cell)),edge=d==2L))
  if(hyperideal) pieces[[2L]] <- list(X=coxeter_truncation_face_3d(simplex$vertices[1L,],coxeter_seed_neighbours_3d(simplex),simplex),cells=list(cell),edge=FALSE)
  out <- list()
  for(piece in pieces) {
    centre <- coxeter_normalise_3d(colMeans(piece$X),simplex)
    orbit <- coxeter_transforms_3d(centre,simplex$mirrors,simplex,inside,max_cells,overflow)
    for(i in seq_along(orbit$transforms)) {
      T <- orbit$transforms[[i]]
      out[[length(out)+1L]] <- list(X=t(apply(piece$X%*%t(T),1L,coxeter_normalise_3d,simplex)),
        cells=vapply(piece$cells,function(x) coxeter_key_3d(coxeter_normalise_3d(drop(T%*%x),simplex)),character(1)),
        depth=orbit$depth[i],edge=piece$edge)
    }
  }
  message(length(out)," ",if(d==2L) "edges" else "faces"," in ",simplex$kind," ",d,"-space")
  list(simplex=simplex,pieces=out,mode="faces")
}

# Polygons of a tiling with their cell identities and depths.
coxeter_pieces_3d <- function(tiling,what) {
  if(tiling$mode=="faces") {
    if(what!="edges"&&any(vapply(tiling$pieces,`[[`,logical(1),"edge"))) stop("Apeirogonal cells have no finite faces; use what = \"edges\".",call.=FALSE)
    keys <- unique(unlist(lapply(tiling$pieces,`[[`,"cells")))
    return(unlist(lapply(tiling$pieces,function(piece) {
      cells <- match(piece$cells,keys)
      if(what!="cells") cells <- cells[1L]
      lapply(cells,function(cell) list(X=piece$X,cell=cell,depth=piece$depth,edge=piece$edge))
    }),recursive=FALSE))
  }
  unlist(lapply(seq_along(tiling$transforms),function(cell) {
    points <- tiling$vertices%*%t(tiling$transforms[[cell]])
    lapply(tiling$faces,function(face) list(X=points[face,,drop=FALSE],cell=cell,depth=tiling$depth[cell],edge=FALSE))
  }),recursive=FALSE)
}

SpaceCoxeter3D <- ggplot2::ggproto("SpaceCoxeter3D",Space3D,
  charts="canonical",models=c("klein","poincare","embedded","stereographic","cartesian"),default_model="klein",
  dimension=3L,kind="hyperbolic",bound=1,
  to_canonical=function(self,data,chart) as.matrix(data[paste0(".canonical",seq_len(self$dimension+1L))]),
  from_canonical=function(self,X,model) {
    d <- self$dimension; rank <- d+1L
    if(self$kind=="hyperbolic") {
      out <- hyperbolic_ball_homogeneous_3d(X,model)
      return(if(d==2L) cbind(out[,1:2,drop=FALSE],0,out[,3L]) else out)
    }
    if(self$kind=="spherical"&&model=="stereographic") return(cbind(X[,seq_len(d),drop=FALSE],1-X[,rank]))
    out <- X[,seq_len(d),drop=FALSE]
    if(d==2L) cbind(out,if(self$kind=="spherical") X[,3L] else 0) else out
  },
  distance=function(self,X,Y) {
    if(self$kind=="spherical") return(sphere_angle_3d(X,Y))
    if(self$kind=="euclidean") return(sqrt(rowSums((X-Y)^2)))
    hyperbolic_distance_3d(X,Y)
  },
  geodesic=function(self,X,Y,s) {
    if(self$kind=="spherical") return(sphere_slerp_3d(X,Y,s))
    if(self$kind=="euclidean") return(segment_points_3d(X,Y,s))
    hyperbolic_geodesic_3d(X,Y,s)
  },
  chart_is_projective=function(self,model) self$kind=="euclidean"||(self$kind=="hyperbolic"&&model=="klein"),
  model_ranges=function(self,model,panel_params) {
    if(self$kind=="euclidean") return(panel_params$ranges)
    stats::setNames(rep(list(c(-self$bound,self$bound)),3L),c("x","y","z"))
  }
)

coxeter_data_3d <- function(tiling,what,model) {
  simplex <- tiling$simplex; space <- ggplot2::ggproto(NULL,SpaceCoxeter3D,dimension=simplex$d,kind=simplex$kind)
  blocks <- list(); seen <- new.env(hash=TRUE,parent=emptyenv()); face_id <- 0L
  for(piece in coxeter_pieces_3d(tiling,what)) {
    X <- piece$X
    rings <- if(what=="edges") lapply(seq_len(nrow(X)),function(k) c(k,k%%nrow(X)+1L)) else list(seq_len(nrow(X)))
    for(ids in rings) {
      Y <- X[ids,,drop=FALSE]
      key <- paste(sort(apply(Y,1L,coxeter_key_3d)),collapse=";")
      if(what!="cells"&&exists(key,seen,inherits=FALSE)) next
      seen[[key]] <- TRUE
      if(what=="edges") triangles <- list(Y) else {
        # A face's canonical centre and its boundary define spherical or hyperbolic fan triangles.
        center <- coxeter_normalise_3d(colMeans(Y),simplex)
        triangles <- lapply(seq_len(nrow(Y)),function(k) rbind(center,Y[k,],Y[k%%nrow(Y)+1L,]))
      }
      for(corners in triangles) {
        face_id <- face_id+1L
        xyz <- space_model_xyz_3d(corners,space,model)
        d <- as.data.frame(xyz); names(d) <- c("x","y","z")
        d[paste0(".canonical",seq_len(ncol(corners)))] <- as.data.frame(corners)
        d$.face <- face_id; d$cell <- piece$cell; d$depth <- piece$depth
        blocks[[length(blocks)+1L]] <- d
      }
    }
  }
  do.call(rbind,blocks)
}

StatCoxeter3D <- ggplot2::ggproto("StatCoxeter3D",StatGeometry3D,
  required_aes=character(),optional_aes=c("x","y","z"),
  compute_group=function(data,scales,schlafli,radius=2,what="edges",max_cells=10000L,model=NULL) {
    tiling <- coxeter_cells_3d(schlafli,radius,max_cells)
    model <- model %or3d% switch(tiling$simplex$kind,hyperbolic="klein",spherical="embedded",euclidean="cartesian")
    out <- coxeter_data_3d(tiling,what,model)
    out$.geometry <- match(tiling$simplex$kind,c("hyperbolic","spherical","euclidean"))
    out$.dimension <- length(schlafli); out$.model <- model
    out$.bound <- if(tiling$simplex$kind=="spherical"&&model=="stereographic") 1/tan(acos(tiling$simplex$vertices[length(schlafli),length(schlafli)+1L])/2) else 1
    out
  }
)

GeomCoxeter3D <- ggplot2::ggproto("GeomCoxeter3D",ggplot2::Geom,
  required_aes=c("x","y","z"),default_aes=local({defaults <- solid_defaults_3d(); defaults$colour <- "grey25"; defaults}),draw_key=ggplot2::draw_key_polygon,
  optional_aes=c(".canonical1",".canonical2",".canonical3",".canonical4",".face",".geometry",".dimension",".model"))

emit_coxeter_3d <- function(data,params,panel_params) {
  data <- data[order(data$group,data$.face),,drop=FALSE]
  keys <- interaction(data$group,data$.face,drop=TRUE)
  counts <- as.integer(rle(as.integer(keys))$lengths)
  kind <- if(params$what=="edges") "polylines" else "faces"
  primitives <- seq_len(nrow(data)); kinds <- rep(kind,length(counts))
  if(kind=="faces"&&(isTRUE(params$wireframe)||any(!is.na(data$colour)))) {
    triangles <- matrix(seq_len(nrow(data)),ncol=3L,byrow=TRUE)
    edges <- if(isTRUE(params$wireframe)) rbind(triangles[,1:2],triangles[,2:3],triangles[,c(3,1)]) else triangles[,2:3,drop=FALSE]
    canonical <- data[paste0(".canonical",seq_len(data$.dimension[1L]+1L))]
    vertex_keys <- apply(canonical,1L,coxeter_key_3d)
    vertex <- match(vertex_keys,unique(vertex_keys))
    identity <- cbind(pmin(vertex[edges[,1L]],vertex[edges[,2L]]),pmax(vertex[edges[,1L]],vertex[edges[,2L]]))
    edges <- edges[!duplicated(identity),,drop=FALSE]
    primitives <- c(primitives,as.vector(t(edges)))
    kinds <- c(kinds,rep("polylines",nrow(edges))); counts <- c(counts,rep.int(2L,nrow(edges)))
  }
  packet_from_primitives_3d(data,primitives,kinds,params,panel_params$panel_id,counts)
}

register_geom_3d(GeomCoxeter3D,emit_coxeter_3d,positions=character(),parameters=list(what="edges",munch_tolerance=1e-3,munch_max_depth=9L),
  space=function(data,params,panel_params) {
    space <- ggplot2::ggproto(NULL,SpaceCoxeter3D,dimension=data$.dimension[1L],kind=c("hyperbolic","spherical","euclidean")[data$.geometry[1L]],bound=data$.bound[1L])
    space_spec_3d(space,data$.model[1L],"canonical",NULL,params$munch_tolerance,params$munch_max_depth)
  })

#' Regular tilings and honeycombs from Coxeter reflections
#'
#' Factors the simplex Gram matrix with off-diagonal entries -cos(pi/m).
#' Mirror reflections generate a seed cell and its orbit. Cell centres are
#' deduplicated and retained within radius of the seed centre. Reports the
#' number of cells before emission. Spherical, Euclidean and hyperbolic
#' symbols are supported, with finite, ideal or hyperideal vertices and cells
#' (Nelson and Segerman 2016). Ideal vertices are light-cone points, drawn on
#' the boundary of the Klein and Poincare balls. Hyperideal vertices are cut
#' off by their polar planes: each face loses its corners and every vertex
#' leaves a truncation face orthogonal to its edges, so the truncated cells are
#' compact. When the cell centre is ideal or hyperideal the cells are infinite;
#' faces (edges for tilings with apeirogonal cells) are then generated one by
#' one and retained within radius of the seed face, depth counts reflections
#' from the seed face, and what = "cells" draws each face once for each of its
#' two cells. The message then reports faces or edges instead of cells.
#'
#' Hyperbolic output uses the Klein model by default; Poincare output is
#' adaptively tessellated. Spherical tiles lie on S^2. Spherical honeycombs
#' are projected from S^3 into R^3 by their first three coordinates or by
#' stereographic projection. Their curved edges and faces use the shared
#' space sampler. what="faces" deduplicates shared faces, what="cells"
#' retains each cell's boundary, and what="edges" deduplicates shared edges.
#' @inheritParams stat_parametric_3d
#' @param schlafli Two entries \{p,q\} for tilings, three \{p,q,r\} for honeycombs.
#' @param radius Centre distance cutoff in intrinsic units (radians on spheres).
#' @param what Geometry to emit: edges, faces or cells.
#' @param max_cells Explicit orbit resource limit; exceeding it raises an error.
#' @param model NULL chooses Klein, embedded spherical or Cartesian output.
#'   Other choices are Poincare for hyperbolic geometry and stereographic for S^3.
#' @param munch_tolerance,munch_max_depth Shared geodesic sampling controls.
#' @return A ggplot2 layer with cell and depth computed variables; depth counts simplex reflections.
#' @references Nelson and Segerman (2016). Visualizing hyperbolic honeycombs.
#' <https://arxiv.org/abs/1511.02851>.
#' Kopczynski and Celinska-Kopczynska (2020).
#' <https://arxiv.org/abs/2002.09533>.
#' @examples
#' ggplot2::ggplot()+stat_tiling_3d(schlafli=c(7,3),radius=2)+
#'   coord_3d(axes=character(),panes=character(),box="none")
#' ggplot2::ggplot()+stat_honeycomb_3d(schlafli=c(5,3,4),radius=1,what="faces")+
#'   coord_3d(axes=character(),panes=character(),box="none")
#' @export
stat_tiling_3d <- function(mapping=NULL,data=NULL,position="identity",...,schlafli=c(7,3),radius=2,
                           what=c("edges","faces","cells"),max_cells=10000L,model=NULL,
                           munch_tolerance=1e-3,munch_max_depth=9L,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  coxeter_layer_3d(mapping,data,position,list(...),schlafli,radius,what,max_cells,model,munch_tolerance,munch_max_depth,
    na.rm,show.legend,inherit.aes,2L)
}

#' @rdname stat_tiling_3d
#' @export
stat_honeycomb_3d <- function(mapping=NULL,data=NULL,position="identity",...,schlafli=c(5,3,4),radius=2,
                              what=c("edges","faces","cells"),max_cells=10000L,model=NULL,
                              munch_tolerance=1e-3,munch_max_depth=9L,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  coxeter_layer_3d(mapping,data,position,list(...),schlafli,radius,what,max_cells,model,munch_tolerance,munch_max_depth,
    na.rm,show.legend,inherit.aes,3L)
}

coxeter_layer_3d <- function(mapping,data,position,extra,schlafli,radius,what,max_cells,model,tolerance,max_depth,na.rm,show.legend,inherit.aes,dimension) {
  if(length(schlafli)!=dimension) stop("Wrong number of Schlafli entries for this layer.",call.=FALSE)
  simplex <- coxeter_simplex_3d(schlafli)
  radius <- mesh_number_3d(radius,"radius",function(x) is.finite(x)&&x>=0,"nonnegative")
  if(!is.null(model)&&!model%in%switch(simplex$kind,hyperbolic=c("klein","poincare"),spherical=if(dimension==3L) c("embedded","stereographic") else "embedded",euclidean="cartesian")) stop("Unknown display model for this geometry.",call.=FALSE)
  spec <- space_spec_3d(SpaceCoxeter3D,"klein","canonical",NULL,tolerance,max_depth)
  make_layer_3d(GeomCoxeter3D,mapping,stats_seed_data_3d(data),StatCoxeter3D,position,
    stats_params_3d(list(schlafli=schlafli,radius=radius,what=match.arg(what,c("edges","faces","cells")),max_cells=mesh_count_3d(max_cells,"max_cells",1L),model=model,
      munch_tolerance=spec$tolerance,munch_max_depth=spec$max_depth,na.rm=na.rm,shade=TRUE),extra),show.legend,inherit.aes)
}
