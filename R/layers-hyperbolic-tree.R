#' @include space-hyperbolic-finite.R stats-common.R
NULL

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

GeomTreeHyperbolic3D <- ggplot2::ggproto("GeomTreeHyperbolic3D",ggplot2::GeomPoint,
  required_aes=c("x","y","z"),optional_aes=c("xend","yend","zend"),
  default_aes=ggplot2::aes(colour="black",size=1.5,shape=19,fill=NA,alpha=NA,stroke=.5,linewidth=.5,linetype=1),
  draw_key=function(data,params,size) {
    if(params$what=="nodes") ggplot2::draw_key_point(data,params,size) else ggplot2::draw_key_path(data,params,size)
  })

register_geom_3d(GeomTreeHyperbolic3D,
  function(data,params,panel_params) .emitters_3d[[if(params$what=="nodes") "GeomPoint" else "GeomSegment"]]$emit(data,params,panel_params),
  positions=character(),parameters=list(what="edges"),
  space=function(data,params,panel_params) {
    context <- panel_params$space_context
    if(is.null(context)||!inherits(context$space,"SpaceHyperbolicFinite")) return(space_spec_3d(SpaceHyperbolicFinite,"poincare","poincare",NULL,1e-3,9L))
    space_spec_3d(context$space,context$model,"poincare",context$isometry,context$tolerance,context$max_depth)
  })

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
  make_layer_3d(GeomTreeHyperbolic3D,mapping,data,StatTreeHyperbolic3D,position,
    stats_params_3d(list(dimension=dimension,edge_length=edge_length,what=what,na.rm=na.rm),list(...)),show.legend,inherit.aes)
}
