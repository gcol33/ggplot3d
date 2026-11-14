#' @include space-3d.R frame-globe.R layers-surface.R stats-common.R
NULL

coxeter_key_3d <- function(x) paste(round(x,8L),collapse=":")

coxeter_simplex_3d <- function(schlafli) {
  d <- length(schlafli); rank <- d+1L
  if(!d%in%c(2L,3L)||!is.numeric(schlafli)||any(is.na(schlafli)|schlafli<3|schlafli!=trunc(schlafli))) stop("schlafli must give two or three integers >=3 (or Inf).",call.=FALSE)
  G <- diag(rank)
  for(i in seq_len(d)) G[i,i+1L] <- G[i+1L,i] <- -cos(pi/schlafli[i])
  eig <- eigen(G,symmetric=TRUE); values <- eig$values
  kind <- if(min(values)>1e-10) "spherical" else if(min(values)>=-1e-10) "euclidean" else "hyperbolic"
  if(sum(values< -1e-10)>1L) stop("The Coxeter Gram matrix has more than one negative direction.",call.=FALSE)
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
    norm <- rowSums((V%*%J)*V)
    if(kind=="hyperbolic"&&(any(eigen(G[seq_len(d),seq_len(d)],symmetric=TRUE,only.values=TRUE)$values<=1e-10)||norm[1L]>=-1e-10||norm[rank]>=-1e-10)) stop("Ideal/hyperideal vertices or cells require schema 2 or explicit truncation (#16).",call.=FALSE)
    V <- V/sqrt(abs(norm))
    if(kind=="hyperbolic") V <- V*ifelse(V[,rank]<0,-1,1)
  }
  center <- V[rank,]
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
  inv <- solve(T)
  list(d=d,kind=kind,G=G,J=J,vertices=V%*%t(T),mirrors=lapply(mirrors,function(M) T%*%M%*%inv),symbol=schlafli)
}

coxeter_normalise_3d <- function(x,simplex) {
  if(simplex$kind=="euclidean") return(x/x[length(x)])
  norm <- sum((x%*%simplex$J)*x)
  out <- x/sqrt(abs(norm))
  if(simplex$kind=="hyperbolic"&&tail(out,1L)<0) out <- -out
  out
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

coxeter_ring_order_3d <- function(points) {
  centered <- sweep(points,2L,colMeans(points),"-")
  basis <- svd(centered,nu=0,nv=2L)$v
  xy <- centered%*%basis
  order(atan2(xy[,2L],xy[,1L]))
}

coxeter_cells_3d <- function(schlafli,radius,max_cells) {
  simplex <- coxeter_simplex_3d(schlafli); d <- simplex$d; rank <- d+1L
  group <- simplex$mirrors[seq_len(d)]
  expected_vertices <- if(d==2L) schlafli[1L] else round(4*schlafli[1L]/(2*schlafli[1L]+2*schlafli[2L]-prod(schlafli[1:2])))
  vertices <- coxeter_orbit_3d(simplex$vertices[1L,],group,simplex,expected_vertices)
  if(d==2L) faces <- list(coxeter_ring_order_3d(vertices)) else {
    centers <- coxeter_orbit_3d(simplex$vertices[d,],group,simplex,round(4*schlafli[2L]/(2*schlafli[1L]+2*schlafli[2L]-prod(schlafli[1:2]))))
    if(simplex$kind=="euclidean") {
      normal <- simplex$vertices[d,seq_len(d)]
      relative <- sweep(vertices[,seq_len(d),drop=FALSE],2L,simplex$vertices[rank,seq_len(d)],"-")
      target <- sum((simplex$vertices[1L,seq_len(d)]-simplex$vertices[rank,seq_len(d)])*normal)
      faces <- lapply(seq_len(nrow(centers)),function(i) {
        n <- centers[i,seq_len(d)]; value <- drop(relative%*%n)
        ids <- which(abs(value-target)<1e-7); ids[coxeter_ring_order_3d(vertices[ids,,drop=FALSE])]
      })
    } else {
      target <- sum((simplex$vertices[1L,]%*%simplex$J)*simplex$vertices[d,])
      values <- vertices%*%simplex$J%*%t(centers)
      faces <- lapply(seq_len(nrow(centers)),function(i) {
        ids <- which(abs(values[,i]-target)<1e-7)
        ids[coxeter_ring_order_3d(vertices[ids,,drop=FALSE])]
      })
    }
  }
  if(any(lengths(faces)<3L)) stop("The Coxeter cell has a degenerate facet.",call.=FALSE)
  center <- simplex$vertices[rank,]; transforms <- list(diag(rank)); centers <- list(center)
  depth <- 0L; seen <- new.env(hash=TRUE,parent=emptyenv()); seen[[coxeter_key_3d(center)]] <- TRUE
  distance <- function(x) {
    if(simplex$kind=="hyperbolic") acosh(max(1,x[rank])) else
      if(simplex$kind=="spherical") acos(max(-1,min(1,x[rank]))) else sqrt(sum(x[seq_len(d)]^2))
  }
  i <- 1L
  while(i<=length(transforms)) {
    for(M in simplex$mirrors) {
      T <- M%*%transforms[[i]]; candidate <- coxeter_normalise_3d(drop(T%*%center),simplex)
      if(distance(candidate)>radius+1e-8) next
      key <- coxeter_key_3d(candidate)
      if(!exists(key,seen,inherits=FALSE)) {
        if(length(transforms)>=max_cells) stop("The radius requires more than max_cells; increase the explicit resource budget.",call.=FALSE)
        transforms[[length(transforms)+1L]] <- T; centers[[length(centers)+1L]] <- candidate
        depth <- c(depth,depth[i]+1L); seen[[key]] <- TRUE
      }
    }
    i <- i+1L
  }
  message(length(transforms)," cells in ",simplex$kind," ",d,"-space")
  list(simplex=simplex,vertices=vertices,faces=faces,transforms=transforms,centers=do.call(rbind,centers),depth=depth)
}

SpaceCoxeter3D <- ggplot2::ggproto("SpaceCoxeter3D",Space3D,
  charts="canonical",models=c("klein","poincare","embedded","stereographic","cartesian"),default_model="klein",
  dimension=3L,kind="hyperbolic",bound=1,
  to_canonical=function(self,data,chart) as.matrix(data[paste0(".canonical",seq_len(self$dimension+1L))]),
  from_canonical=function(self,X,model) {
    d <- self$dimension; rank <- d+1L
    out <- X[,seq_len(d),drop=FALSE]
    if(self$kind=="hyperbolic") out <- out/if(model=="poincare") X[,rank]+1 else X[,rank]
    if(self$kind=="spherical"&&model=="stereographic") {
      den <- 1-X[,rank]
      if(any(den<=1e-12)) stop("A stereographic sample reaches the projection pole; ideal output requires schema 2.",call.=FALSE)
      out <- out/den
    }
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
    hyperbolic_slerp_3d(X,Y,s)
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
  for(cell in seq_along(tiling$transforms)) {
    points <- tiling$vertices%*%t(tiling$transforms[[cell]])
    for(face in tiling$faces) {
      if(what=="edges") rings <- lapply(seq_along(face),function(k) face[c(k,k%%length(face)+1L)]) else rings <- list(face)
      for(ids in rings) {
        X <- points[ids,,drop=FALSE]
        key <- paste(sort(apply(X,1L,coxeter_key_3d)),collapse=";")
        if(what!="cells"&&exists(key,seen,inherits=FALSE)) next
        seen[[key]] <- TRUE
        if(what=="edges") triangles <- list(X) else {
          # A face's canonical centre and its boundary define spherical or hyperbolic fan triangles.
          center <- coxeter_normalise_3d(colMeans(X),simplex)
          triangles <- lapply(seq_len(nrow(X)),function(k) rbind(center,X[k,],X[k%%nrow(X)+1L,]))
        }
        for(corners in triangles) {
          face_id <- face_id+1L
          xyz <- space$from_canonical(corners,model)
          d <- as.data.frame(xyz); names(d) <- c("x","y","z")
          d[paste0(".canonical",seq_len(ncol(corners)))] <- as.data.frame(corners)
          d$.face <- face_id; d$cell <- cell; d$depth <- tiling$depth[cell]
          blocks[[length(blocks)+1L]] <- d
        }
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
#' number of cells before emission. Spherical, Euclidean and compact
#' hyperbolic symbols are supported. Ideal or hyperideal vertices and cells
#' require homogeneous clipping or an explicit truncation and raise an error.
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
