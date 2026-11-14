tesseract_mesh_4d <- function() {
  v <- expand.grid(x=c(-1,1),y=c(-1,1),z=c(-1,1),w=c(-1,1))
  keys <- apply(v,1,paste,collapse=":"); faces <- list()
  for(free in combn(4L,2L,simplify=FALSE)) {
    fixed <- setdiff(1:4,free)
    for(a in c(-1,1)) for(b in c(-1,1)) {
      corners <- matrix(0,4,4); corners[,fixed[1]] <- a; corners[,fixed[2]] <- b
      corners[,free] <- rbind(c(-1,-1),c(1,-1),c(1,1),c(-1,1))
      faces[[length(faces)+1L]] <- match(apply(corners,1,paste,collapse=":"),keys)
    }
  }
  mesh_3d(v,do.call(rbind,faces))
}

polytope_120_cell_mesh_4d <- function() {
  tiling <- suppressMessages(coxeter_cells_3d(c(5,3,3),pi,200))
  points <- list(); faces <- list(); seen_points <- new.env(hash=TRUE,parent=emptyenv()); seen_faces <- new.env(hash=TRUE,parent=emptyenv())
  for(T in tiling$transforms) {
    X <- tiling$vertices%*%t(T)
    vertex <- vapply(seq_len(nrow(X)),function(i) {
      key <- coxeter_key_3d(X[i,])
      if(exists(key,seen_points,inherits=FALSE)) return(seen_points[[key]])
      index <- length(points)+1L; points[[index]] <<- X[i,]; seen_points[[key]] <- index; index
    },integer(1))
    for(face in tiling$faces) {
      ids <- vertex[face]; key <- paste(sort(ids),collapse=":")
      if(exists(key,seen_faces,inherits=FALSE)) next
      seen_faces[[key]] <- TRUE; faces[[length(faces)+1L]] <- ids
    }
  }
  v <- as.data.frame(do.call(rbind,points)); names(v) <- c("x","y","z","w")
  face <- data.frame(.face=rep(seq_along(faces),lengths(faces)),.corner=unlist(lapply(faces,seq_along)),.vertex=unlist(faces))
  mesh_3d(v,face)
}
