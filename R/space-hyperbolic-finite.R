#' @include space-3d.R layers-relativity.R
NULL

utils::globalVariables(c("from", "to"))

lorentz_dot_3d <- function(X,Y) {
  n <- ncol(X); spatial <- seq_len(n-1L)
  rowSums(X[,spatial,drop=FALSE]*Y[,spatial,drop=FALSE])-X[,n]*Y[,n]
}

hyperbolic_distance_3d <- function(X,Y) 2*asinh(sqrt(pmax(0,lorentz_dot_3d(X-Y,X-Y)))/2)

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
  if(context$model!="klein"||!length(packet$faces$offset)) return(packet)
  P <- do.call(cbind,packet$vertices[c("x","y","z")])
  P <- sweep(sweep(P,2L,vapply(context$ranges,diff,numeric(1)),"*"),2L,vapply(context$ranges,`[`,numeric(1),1L),"+")
  normal <- t(vapply(seq_along(packet$faces$offset),function(i) {
    vertices <- packet$corners$vertex[seq.int(packet$faces$offset[i],length.out=packet$faces$count[i])]
    K <- P[vertices,,drop=FALSE]
    n <- cross_rows_3d(K[2L,,drop=FALSE]-K[1L,],K[3L,,drop=FALSE]-K[1L,])
    c0 <- sum(n*K[1L,]); norm <- sqrt(sum(n^2)-c0^2)
    if(!is.finite(norm)||norm<=0) return(rep(NA_real_,3L))
    N <- c(drop(n),c0)/norm
    k <- colMeans(K); X <- c(k,1)/sqrt(1-sum(k^2))
    drop(hyperbolic_origin_boost_3d(X,TRUE)%*%N)[1:3]
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

SpaceHyperbolicFinite <- ggplot2::ggproto("SpaceHyperbolicFinite",Space3D,
  name="hyperbolic",charts=c("poincare","klein","halfspace","hyperboloid_sheet"),
  models=c("poincare","klein","halfspace","hyperboloid_sheet"),default_model="klein",extent=3,curvature=-1,
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
    if(any(norm>=1)) stop("Ball coordinates must lie strictly inside the unit ball; ideal points require schema 2.",call.=FALSE)
    if(chart=="poincare") cbind(2*P/(1-norm),(1+norm)/(1-norm)) else cbind(P,1)/sqrt(1-norm)
  },
  from_canonical=function(X,model) {
    if(model=="poincare") return(X[,1:3,drop=FALSE]/(X[,4L]+1))
    if(model=="klein") return(X[,1:3,drop=FALSE]/X[,4L])
    if(model=="hyperboloid_sheet") {
      if(any(abs(X[,3L])>1e-9)) stop("hyperboloid_sheet requires points in the H^2 plane z=0.",call.=FALSE)
      return(cbind(X[,1:2,drop=FALSE],X[,4L]))
    }
    den <- ifelse(X[,3L]>0,(1+rowSums(X[,1:2,drop=FALSE]^2))/(X[,4L]+X[,3L]),X[,4L]-X[,3L])
    cbind(X[,1:2,drop=FALSE]/den,1/den)
  },
  distance=function(self,X,Y) hyperbolic_distance_3d(X,Y)/sqrt(-self$curvature),
  geodesic=function(X,Y,s) hyperbolic_slerp_3d(X,Y,s),
  act=function(g,X) {
    check_lorentz_matrix_3d(g,3L)
    X%*%t(g)
  },
  chart_is_projective=function(model) model=="klein",
  style_packet=function(packet,context) hyperbolic_style_packet_3d(packet,context),
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
      xyz <- self$space$from_canonical(X,self$model)
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
#' @param ... Arguments to [coord_3d()], including isometry.
#' @return A Coord3D object.
#' @references Nickel and Kiela (2017). Poincare embeddings.
#' <https://arxiv.org/abs/1705.08039>.
#' Phillips and Gunn (1992). Visualizing hyperbolic space.
#' <https://graphics.stanford.edu/papers/webviz/webviz/node2.html>.
#' @details Klein is the default display. Poincare is the default input for
#'   compatibility with hyperbolic layout statistics. Half-space is truncated
#'   to the finite display box set by extent. Geodesic faces and arcs use the
#'   common screen-error sampler. Position scales must be identity transforms.
#'   Klein lighting transports Minkowski plane normals to the origin.
#' @examples
#' d <- data.frame(x=c(-.7,.7),y=c(.2,.2),z=0)
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+coord_hyperbolic_3d()
#' ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+
#'   coord_hyperbolic_3d(model="poincare",curvature=-4,distance_breaks=c(0,.5,1))
#' @export
coord_hyperbolic_3d <- function(input=c("poincare","klein","halfspace","hyperboloid_sheet"),model="klein",
                               extent=3,references=character(),curvature=-1,centre=NULL,distance_breaks=NULL,...) {
  input <- match.arg(input); model <- match.arg(model,SpaceHyperbolicFinite$models)
  extent <- mesh_number_3d(extent,"extent",function(x) is.finite(x)&&x>0,"positive")
  if(!is.character(references)||any(!references%in%c("klein","poincare"))) stop("Unknown hyperboloid reference disk.",call.=FALSE)
  curvature <- mesh_number_3d(curvature,"curvature",function(x) is.finite(x)&&x<0,"negative")
  if(!is.null(distance_breaks)&&(!is.numeric(distance_breaks)||any(!is.finite(distance_breaks))||any(distance_breaks<0)||is.unsorted(distance_breaks,strictly=TRUE))) stop("distance_breaks must be increasing finite nonnegative distances.",call.=FALSE)
  space <- ggplot2::ggproto(NULL,SpaceHyperbolicFinite,extent=extent,curvature=curvature)
  args <- list(...)
  if(!is.null(centre)) {
    if(!is.null(args$isometry)) stop("Supply centre or isometry, one action at a time.",call.=FALSE)
    if(is.matrix(centre)) {check_lorentz_matrix_3d(centre,3L); args$isometry <- centre} else {
      if(!is.numeric(centre)||length(centre)!=3L||any(!is.finite(centre))) stop("centre must be a finite input-chart point or Lorentz matrix.",call.=FALSE)
      X <- space$to_canonical(data.frame(x=centre[1L],y=centre[2L],z=centre[3L]),input)
      args$isometry <- hyperbolic_origin_boost_3d(drop(X),TRUE)
    }
  }
  bounds <- space$model_ranges(model,NULL)
  if(!is.null(args$aspect)&&!isTRUE(all.equal(as.numeric(args$aspect),vapply(bounds,diff,numeric(1))/diff(bounds$x)))) stop("Hyperbolic aspect is fixed by model units.",call.=FALSE)
  args$aspect <- vapply(bounds,diff,numeric(1))
  coord <- do.call(coord_3d,stats_params_3d(list(space=space,input=input,model=model,
    axes=character(),panes=character(),box="none",clip_data=TRUE),args))
  coord$frame <- ggplot2::ggproto(NULL,FrameHyperbolicFinite,space=space,model=model,references=references,distance_breaks=distance_breaks)
  coord
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

hyperbolic_tree_layout_3d <- function(from,to,dimension,edge_length) {
  from <- as.character(from); to <- as.character(to)
  if(!length(from)||length(from)!=length(to)||anyNA(from)||anyNA(to)||any(from==to)||anyDuplicated(to)) stop("Edges must form one rooted tree with unique parents.",call.=FALSE)
  nodes <- sort(unique(c(from,to))); root <- setdiff(nodes,to)
  if(length(root)!=1L||length(from)!=length(nodes)-1L) stop("Edges must form one connected rooted tree.",call.=FALSE)
  parent <- match(from,nodes); child <- match(to,nodes); children <- split(child,parent)
  parent_of <- integer(length(nodes)); parent_of[child] <- parent
  degree <- tabulate(c(parent,child),length(nodes))
  if(is.null(edge_length)) edge_length <- if(dimension==2L) 1+max(0,-2*log(tan(pi/(2*max(2L,degree))))) else 1
  X <- matrix(NA_real_,length(nodes),4L); depth <- integer(length(nodes)); index <- match(root,nodes)
  X[index,] <- c(0,0,0,1); queue <- integer(length(nodes)); queue[1L] <- index; tail <- 1L
  for(i in seq_len(length(nodes))) {
    if(i>tail) stop("Tree contains a cycle or disconnected component.",call.=FALSE)
    node <- queue[i]; kids <- children[[as.character(node)]]
    if(!length(kids)) next
    kids <- kids[order(nodes[kids])]; k <- length(kids)
    boost <- hyperbolic_origin_boost_3d(X[node,])
    origin_parent <- if(node==index) c(0,0,0) else drop(matrix(X[parent_of[node],],1L)%*%t(hyperbolic_origin_boost_3d(X[node,],TRUE)))[1:3]
    if(dimension==2L) {
      angle <- if(node==index) 2*pi*(seq_len(k)-1)/k else atan2(origin_parent[2L],origin_parent[1L])+2*pi*seq_len(k)/(k+1L)
      direction <- cbind(cos(angle),sin(angle),0)
    } else {
      axis <- if(sum(origin_parent^2)==0) c(0,0,1) else -origin_parent/sqrt(sum(origin_parent^2))
      other <- diag(3)[which.min(abs(axis)),]; first <- c(axis[2]*other[3]-axis[3]*other[2],axis[3]*other[1]-axis[1]*other[3],axis[1]*other[2]-axis[2]*other[1]); first <- first/sqrt(sum(first^2))
      second <- c(axis[2]*first[3]-axis[3]*first[2],axis[3]*first[1]-axis[1]*first[3],axis[1]*first[2]-axis[2]*first[1])
      height <- (seq_len(k)-.5)/k; angle <- pi*(3-sqrt(5))*(seq_len(k)-1)
      direction <- height%o%axis+sqrt(1-height^2)*cos(angle)%o%first+sqrt(1-height^2)*sin(angle)%o%second
    }
    local <- cbind(sinh(edge_length)*direction,cosh(edge_length))
    X[kids,] <- local%*%t(boost); X[kids,4L] <- sqrt(1+rowSums(X[kids,1:3,drop=FALSE]^2))
    depth[kids] <- depth[node]+1L; queue[tail+seq_len(k)] <- kids; tail <- tail+k
  }
  if(any(!is.finite(X))) stop("The tree exceeds finite coordinate precision; reduce edge_length.",call.=FALSE)
  P <- X[,1:3,drop=FALSE]/(X[,4L]+1)
  if(any(rowSums(P^2)>=1)) stop("The tree reaches the floating-point ball boundary; reduce edge_length.",call.=FALSE)
  list(nodes=data.frame(node=nodes,x=P[,1L],y=P[,2L],z=P[,3L],depth=depth),parent=parent,child=child,canonical=X,edge_length=edge_length)
}

StatTreeHyperbolic3D <- ggplot2::ggproto("StatTreeHyperbolic3D",StatGeometry3D,
  required_aes=c("from","to"),dropped_aes=c(StatGeometry3D$dropped_aes,"from","to"),
  compute_panel=function(data,scales,dimension=3L,edge_length=NULL,what="edges") {
    tree <- hyperbolic_tree_layout_3d(data$from,data$to,dimension,edge_length)
    if(what=="nodes") return(tree$nodes)
    out <- data
    out[c("x","y","z")] <- tree$nodes[tree$parent,c("x","y","z")]
    out[c("xend","yend","zend")] <- tree$nodes[tree$child,c("x","y","z")]
    out$depth <- tree$nodes$depth[tree$child]
    out
  }
)

#' Deterministic tree layouts in hyperbolic space
#'
#' Takes directed from/to edges forming a rooted tree. Children are sorted by
#' node key. In H^2 a regular star is placed at each parent by a Lorentz
#' translation, as in Sarkar's construction. Its default edge length exceeds
#' the regular-star Delaunay separation bound. In H^3 children are distributed
#' on the outward hemisphere using equal-area golden-angle samples, following
#' the hemisphere placement used by Munzner. Every edge has the requested
#' intrinsic length. Coordinates are returned in the Poincare ball.
#' @inheritParams stat_parametric_3d
#' @param what Draw edges or nodes.
#' @param dimension Two for a disk, three for a ball.
#' @param edge_length Positive intrinsic edge length; NULL chooses one for H^3
#'   or the regular-star separation bound plus one for H^2.
#' @return A segment or point layer; add [coord_hyperbolic_3d()].
#' @references Munzner (1997). H3. <https://graphics.stanford.edu/papers/h3/>.
#' Sarkar (2011). Low distortion Delaunay embedding of trees in hyperbolic plane.
#' <https://homepages.inf.ed.ac.uk/rsarkar/papers/HyperbolicDelaunayFull.pdf>.
#' @examples
#' edges <- data.frame(from=c("a","a","b","b"),to=c("b","c","d","e"))
#' ggplot2::ggplot(edges)+stat_tree_hyperbolic_3d()+
#'   stat_tree_hyperbolic_3d(what="nodes",size=3)+coord_hyperbolic_3d()
#' @export
stat_tree_hyperbolic_3d <- function(mapping=NULL,data=NULL,position="identity",...,what=c("edges","nodes"),
                                    dimension=3L,edge_length=NULL,na.rm=FALSE,show.legend=NA,inherit.aes=TRUE) {
  what <- match.arg(what)
  if(!dimension%in%c(2L,3L)||length(dimension)!=1L) stop("dimension must be 2 or 3.",call.=FALSE)
  if(!is.null(edge_length)) edge_length <- mesh_number_3d(edge_length,"edge_length",function(x) is.finite(x)&&x>0,"positive")
  mapping <- mapping %or3d% ggplot2::aes(from=from,to=to)
  make_layer_3d(if(what=="edges") ggplot2::GeomSegment else ggplot2::GeomPoint,mapping,data,StatTreeHyperbolic3D,position,
    stats_params_3d(list(dimension=dimension,edge_length=edge_length,what=what,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}

