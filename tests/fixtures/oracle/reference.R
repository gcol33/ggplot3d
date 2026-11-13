# Pixel centres use lower-left NPC coordinates. Rays have unit axial depth.
ref_camera <- function(camera, viewport) {
  a <- camera$azimuth * pi / 180; e <- camera$elevation * pi / 180
  r <- camera$roll * pi / 180
  outward <- c(cos(e) * cos(a), cos(e) * sin(a), sin(e))
  right <- c(-sin(a), cos(a), 0)
  up <- c(-sin(e) * cos(a), -sin(e) * sin(a), cos(e))
  scale <- camera$aspect / max(camera$aspect)
  target <- camera$target * scale
  corners <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))
  radius <- max(sqrt(rowSums(sweep(sweep(corners, 2, scale, `*`), 2, target)^2)))
  tolerance <- 64 * .Machine$double.eps * max(1, radius)
  ratio <- viewport$width_mm / viewport$height_mm
  half <- camera$fov * pi / 360
  distance <- if (camera$projection == "orthographic") 2 * radius + 1 else
    if (is.na(camera$distance)) radius / sin(min(half, atan(tan(half) * ratio))) + tolerance else camera$distance
  height <- 2 * radius / min(1, ratio) / camera$zoom
  sy <- if (camera$projection == "orthographic") 2 / height else camera$zoom / tan(half)
  list(right = cos(r) * right - sin(r) * up,
       up = sin(r) * right + cos(r) * up, forward = -outward,
       eye = target + distance * outward, scale = scale, distance = distance,
       near = if (is.na(camera$near)) max(tolerance, distance - radius - tolerance) else camera$near,
       far = if (is.na(camera$far)) distance + radius + tolerance else camera$far,
       sx = sy / ratio, sy = sy, tolerance=tolerance,
       perspective = camera$projection == "perspective")
}

ref_project <- function(xyz, camera) {
  model <- sweep(xyz, 2, camera$scale, `*`)
  relative <- sweep(model, 2, camera$eye)
  depth <- as.vector(relative %*% camera$forward)
  divisor <- if (camera$perspective) depth else rep(1, nrow(xyz))
  cbind(x = (.5 + .5 * as.vector(relative %*% camera$right) * camera$sx / divisor),
        y = (.5 + .5 * as.vector(relative %*% camera$up) * camera$sy / divisor), depth = depth)
}

ref_grid <- function(n = 160L) {
  grid <- as.matrix(expand.grid(x = (seq_len(n) - .5) / n, y = (seq_len(n) - .5) / n))
  attr(grid,"ref_n") <- n
  grid
}

ref_cross <- function(a, b) c(a[2] * b[3] - a[3] * b[2],
                            a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])

# Half-space interpolation retains the exact intersection of each convex face.
ref_clip <- function(xyz, camera, specification) {
  planes <- list()
  if (specification$clip_data) for (axis in 1:3) {
    normal <- diag(3)[axis, ]
    planes <- c(planes, list(c(normal, 0), c(-normal, 1)))
  }
  normal <- camera$forward * camera$scale
  intercept <- -sum(camera$eye * camera$forward)
  planes <- c(planes, list(c(normal, intercept - camera$near),
                          c(-normal, camera$far - intercept)))
  for (plane in planes) {
    if (!nrow(xyz)) break
    distance <- as.vector(xyz %*% plane[1:3] + plane[4])
    out <- list()
    for (i in seq_len(nrow(xyz))) {
      j <- if (i == nrow(xyz)) 1L else i + 1L
      if (distance[i] >= 0) out[[length(out) + 1L]] <- xyz[i, ]
      if ((distance[i] >= 0) != (distance[j] >= 0))
        out[[length(out) + 1L]] <- xyz[i, ] + distance[i] / (distance[i] - distance[j]) * (xyz[j, ] - xyz[i, ])
    }
    xyz <- if (length(out)) do.call(rbind, out) else matrix(numeric(), 0, 3)
  }
  xyz
}

ref_contains <- function(points, ring) {
  inside <- rep(FALSE, nrow(points))
  if (nrow(ring) < 3L) return(inside)
  eligible <- ref_candidates(points,ring)
  if(!length(eligible)) return(inside)
  samples <- points[eligible,,drop=FALSE]
  local <- rep(FALSE,length(eligible))
  for (i in seq_len(nrow(ring))) {
    j <- if (i == nrow(ring)) 1L else i + 1L
    a <- ring[i, ]; b <- ring[j, ]
    if (b[2] == a[2]) next
    crossing <- (a[2] > samples[, 2]) != (b[2] > samples[, 2])
    local <- xor(local, crossing & samples[, 1] < a[1] +
                    (samples[, 2] - a[2]) * (b[1] - a[1]) / (b[2] - a[2]))
  }
  inside[eligible] <- local
  inside
}

ref_edge_band <- function(points, polygon, n, closed = TRUE) {
  band <- rep(FALSE, nrow(points))
  if (nrow(polygon) < 2L) return(band)
  for (i in seq_len(nrow(polygon) - as.integer(!closed))) {
    j <- if (i == nrow(polygon)) 1L else i + 1L
    a <- polygon[i, ]; delta <- polygon[j, ] - a
    eligible <- ref_candidates(points,rbind(a,polygon[j,]),rep(1/n,2))
    if(!length(eligible)) next
    samples <- points[eligible,,drop=FALSE]
    norm <- sum(delta^2)
    t <- if (norm == 0) rep(0, nrow(samples)) else
      pmin(1, pmax(0, as.vector(sweep(samples, 2, a) %*% delta) / norm))
    d <- samples - matrix(a, nrow(samples), 2, byrow = TRUE) - outer(t, delta)
    band[eligible] <- band[eligible] | rowSums(d^2) <= (1 / n)^2
  }
  band
}

ref_candidates <- function(grid, polygon, margin = c(0, 0)) {
  if (!nrow(polygon)) return(integer())
  n <- attr(grid,"ref_n",exact=TRUE)
  if(!is.null(n)) {
    lo <- pmax(1,ceiling(n*(apply(polygon[,1:2,drop=FALSE],2,min)-margin)+.5))
    hi <- pmin(n,floor(n*(apply(polygon[,1:2,drop=FALSE],2,max)+margin)+.5))
    if(any(lo>hi)) return(integer())
    return(as.integer(outer(seq.int(lo[1],hi[1]),(seq.int(lo[2],hi[2])-1L)*n,`+`)))
  }
  which(grid[, 1] >= min(polygon[, 1]) - margin[1] & grid[, 1] <= max(polygon[, 1]) + margin[1] &
        grid[, 2] >= min(polygon[, 2]) - margin[2] & grid[, 2] <= max(polygon[, 2]) + margin[2])
}

ref_rays <- function(points, camera) {
  offset <- outer((2 * points[, 1] - 1) / camera$sx, camera$right) +
    outer((2 * points[, 2] - 1) / camera$sy, camera$up)
  origin <- matrix(camera$eye, nrow(points), 3, byrow = TRUE)
  direction <- matrix(camera$forward, nrow(points), 3, byrow = TRUE)
  if (camera$perspective) direction <- direction + offset else origin <- origin + offset
  list(origin = origin, direction = direction)
}

# Ray-plane intersection followed by three barycentric inequalities is independent
# of the compiler's projected polygon ordering and splitting implementation.
ref_face_depth <- function(points, xyz, camera) {
  if(!nrow(points)) return(numeric())
  rays <- ref_rays(points, camera)
  model <- sweep(xyz, 2, camera$scale, `*`)
  result <- rep(Inf, nrow(points))
  for (i in seq.int(2L, nrow(model) - 1L)) {
    a <- model[1, ]; u <- model[i, ] - a; v <- model[i + 1L, ] - a
    normal <- ref_cross(u, v)
    denominator <- as.vector(rays$direction %*% normal)
    depth <- as.vector(sweep(rays$origin, 2, a) %*% (-normal)) / denominator
    q <- rays$origin + rays$direction * depth - matrix(a, nrow(points), 3, byrow = TRUE)
    uu <- sum(u*u); uv <- sum(u*v); vv <- sum(v*v); determinant <- uu*vv-uv*uv
    if (determinant <= .Machine$double.eps^2) next
    qu <- as.vector(q %*% u); qv <- as.vector(q %*% v)
    s <- (vv*qu-uv*qv)/determinant; t <- (uu*qv-uv*qu)/determinant
    parallel_tolerance <- 64*.Machine$double.eps*sqrt(sum(normal^2))*sqrt(rowSums(rays$direction^2))
    hit <- is.finite(depth) & abs(denominator) > parallel_tolerance & s >= -1e-12 & t >= -1e-12 & s+t <= 1+1e-12 &
      depth >= camera$near & depth <= camera$far
    result[hit] <- pmin(result[hit], depth[hit])
  }
  result
}

ref_footprint <- function(points, anchor, packet, index, viewport) {
  f <- packet$footprints; id <- packet$markers$footprint_id[index]
  delta <- sweep(points, 2, anchor[1:2])
  delta <- sweep(delta, 2, c(viewport$width_mm, viewport$height_mm), `*`)
  angle <- f$angle[id] * pi / 180
  local <- cbind(cos(angle)*delta[, 1]+sin(angle)*delta[, 2],
                 -sin(angle)*delta[, 1]+cos(angle)*delta[, 2])
  if (f$kind[id] == 1L) {
    if(f$radius_x_mm[id]==0 || f$radius_y_mm[id]==0) return(rep(FALSE,nrow(points)))
    return(rowSums(sweep(local, 2, c(f$radius_x_mm[id], f$radius_y_mm[id]), `/`)^2) <= 1)
  }
  rows <- seq.int(f$offset[id], length.out = f$count[id])
  fv <- packet$footprint_vertices
  inside <- rep(FALSE, nrow(points))
  for (ring in unique(fv$ring[rows])) {
    ii <- rows[fv$ring[rows] == ring]
    inside <- xor(inside, ref_contains(local, cbind(fv$x_mm[ii], fv$y_mm[ii])))
  }
  inside
}

ref_segment <- function(points, segment, width_mm, viewport, cap = 0L) {
  physical <- c(viewport$width_mm, viewport$height_mm)
  p <- sweep(points, 2, physical, `*`)
  s <- sweep(segment[, 1:2, drop = FALSE], 2, physical, `*`)
  delta <- s[2, ] - s[1, ]; norm <- sum(delta^2)
  t <- if (norm == 0) rep(0, nrow(points)) else as.vector(sweep(p, 2, s[1, ]) %*% delta) / norm
  limit <- if (norm == 0) 0 else width_mm / 2 / sqrt(norm)
  clamped <- pmin(1, pmax(0, t))
  if (cap == 2L) clamped <- pmin(1 + limit, pmax(-limit, t))
  distance <- sqrt(rowSums((sweep(p, 2, s[1, ]) - outer(clamped, delta))^2))
  covered <- distance <= width_mm / 2
  if (cap == 0L) covered <- covered & t >= 0 & t <= 1
  outline <- matrix(numeric(),0,2)
  if(norm > 0 && cap != 1L) {
    normal <- c(-delta[2],delta[1])/sqrt(norm)*width_mm/2
    extension <- if(cap==2L) delta/sqrt(norm)*width_mm/2 else c(0,0)
    a <- s[1,]-extension; b <- s[2,]+extension
    outline <- sweep(rbind(a+normal,b+normal,b-normal,a-normal),2,physical,`/`)
  }
  list(covered = covered, t = pmin(1, pmax(0, t)), distance = distance,outline=outline)
}

ref_primitive_xyz <- function(packet, table, i) {
  tab <- packet[[table]]
  corners <- seq.int(tab$offset[i], length.out = tab$count[i])
  v <- packet$corners$vertex[corners]
  cbind(packet$vertices$x[v], packet$vertices$y[v], packet$vertices$z[v])
}

ref_render <- function(packet, specification, viewport, n = 160L) {
  if(length(packet$text$primitive_id)) stop("Oracle does not implement whole-anchor text")
  for(table in c("faces","polylines","markers")) if(any(packet[[table]]$partition!=1L))
    stop("Oracle requires depth-scene partition 1")
  if(any(packet$polylines$hidden!=0L)) stop("Oracle requires hidden stroke policy 0")
  camera <- ref_camera(specification, viewport); grid <- ref_grid(n)
  hits <- list(); boundary <- rep(FALSE, nrow(grid))
  if(specification$clip) boundary <- grid[,1]<=1/n | grid[,1]>=1-1/n |
    grid[,2]<=1/n | grid[,2]>=1-1/n
  for (table in c("faces", "polylines", "markers")) {
    tab <- packet[[table]]
    for (i in seq_along(tab$primitive_id)) {
      if (tab$opacity[i] == 0) next
      xyz <- ref_primitive_xyz(packet, table, i)
      if (table == "faces") {
        xyz <- ref_clip(xyz, camera, specification)
        if (nrow(xyz) < 3L) next
        projected <- ref_project(xyz, camera)
        boundary <- boundary | ref_edge_band(grid, projected[, 1:2, drop = FALSE], n)
        pixels <- ref_candidates(grid, projected)
        depth <- ref_face_depth(grid[pixels, , drop = FALSE], xyz, camera)
      } else if (table == "markers") {
        projected <- ref_project(xyz, camera)
        if (projected[1, 3] < camera$near || projected[1, 3] > camera$far ||
            (specification$clip_data && any(xyz < 0 | xyz > 1))) next
        pixels <- seq_len(nrow(grid))
        covered <- ref_footprint(grid, projected[1, ], packet, i, viewport)
        fp <- packet$footprints; id <- tab$footprint_id[i]
        if(fp$kind[id]==1L && (fp$radius_x_mm[id]==0 || fp$radius_y_mm[id]==0)) next
        if(fp$kind[id]==1L && fp$radius_x_mm[id]==fp$radius_y_mm[id] && viewport$width_mm==viewport$height_mm) {
          distance <- sqrt(rowSums(sweep(grid,2,projected[1,1:2])^2))
          boundary <- boundary | abs(distance-fp$radius_x_mm[id]/viewport$width_mm)<=1/n
        } else if(fp$kind[id]==2L) {
          rows <- seq.int(fp$offset[id],length.out=fp$count[id]); fv <- packet$footprint_vertices
          angle <- fp$angle[id]*pi/180
          for(ring in unique(fv$ring[rows])) {
            ii <- rows[fv$ring[rows]==ring]
            local <- cbind(cos(angle)*fv$x_mm[ii]-sin(angle)*fv$y_mm[ii],
                           sin(angle)*fv$x_mm[ii]+cos(angle)*fv$y_mm[ii])
            polygon <- sweep(sweep(local,2,c(viewport$width_mm,viewport$height_mm),`/`),2,projected[1,1:2],`+`)
            boundary <- boundary | ref_edge_band(grid,polygon,n)
          }
        } else for (angle in seq(0, 2*pi, length.out = 33L)[-33L]) {
            shifted <- sweep(grid, 2, c(cos(angle), sin(angle)) / n, `+`)
            boundary <- boundary | (covered != ref_footprint(shifted, projected[1, ], packet, i, viewport))
        }
        depth <- ifelse(covered, projected[1, 3], Inf)
      } else {
        if(sum((xyz[nrow(xyz),]-xyz[1,])^2)==0) stop("Oracle requires nondegenerate stroke paths")
        if(nrow(xyz)>2L) {
          delta <- xyz[nrow(xyz),]-xyz[1,]
          normals <- t(apply(sweep(xyz,2,xyz[1,]),1,function(x) ref_cross(x,delta)))
          if(any(abs(normals)>1e-12)) stop("Oracle requires straight stroke paths")
          along <- as.vector(sweep(xyz,2,xyz[1,]) %*% delta)
          if(any(diff(along)<0)) stop("Oracle requires monotone straight stroke paths")
          xyz <- xyz[c(1L,nrow(xyz)),,drop=FALSE]
        }
        for (j in seq_len(nrow(xyz) - 1L)) {
          ends <- xyz[c(j, j+1L), , drop = FALSE]
          # A two-vertex ring has the same half-space crossings as a segment.
          ends <- unique(ref_clip(ends, camera, specification))
          if (nrow(ends) < 2L) next
          projected <- ref_project(ends[1:2, , drop = FALSE], camera)
          margin <- (tab$width_mm[i]/2 + max(viewport$width_mm, viewport$height_mm)/n) /
            c(viewport$width_mm, viewport$height_mm)
          pixels <- ref_candidates(grid, projected, margin)
          samples <- grid[pixels, , drop = FALSE]
          coverage <- ref_segment(samples, projected, tab$width_mm[i], viewport, tab$cap[i])
          t <- coverage$t
          depth <- if (camera$perspective) 1/((1-t)/projected[1, 3]+t/projected[2, 3]) else
            (1-t)*projected[1, 3]+t*projected[2, 3]
          depth[!coverage$covered] <- Inf
          edge <- if(nrow(coverage$outline)) ref_edge_band(samples,coverage$outline,n) else rep(FALSE, length(pixels))
          if(!nrow(coverage$outline)) for (angle in seq(0, 2*pi, length.out = 17L)[-17L]) {
            shifted <- sweep(samples, 2, c(cos(angle), sin(angle))/n, `+`)
            edge <- edge | (coverage$covered != ref_segment(shifted, projected, tab$width_mm[i], viewport, tab$cap[i])$covered)
          }
          boundary[pixels] <- boundary[pixels] | edge
          keep <- is.finite(depth)
          if(any(keep)) hits[[length(hits)+1L]] <- data.frame(pixel=pixels[keep], depth=depth[keep],
            id=tab$primitive_id[i], opacity=tab$opacity[i], role=tab$role[i], layer=tab$layer_id[i], source=tab$source_row[i])
        }
        next
      }
      keep <- is.finite(depth)
      if (any(keep)) hits[[length(hits)+1L]] <- data.frame(pixel=pixels[keep], depth=depth[keep],
        id=tab$primitive_id[i], opacity=tab$opacity[i], role=tab$role[i], layer=tab$layer_id[i], source=tab$source_row[i])
    }
  }
  records <- if (length(hits)) do.call(rbind, hits) else data.frame(pixel=integer(), depth=double(), id=integer(), opacity=double(),role=integer(),layer=integer(),source=integer())
  records <- records[order(records$pixel,records$depth), ]
  lists <- rep(list(integer()), nrow(grid)); alpha <- rep(list(double()), nrow(grid))
  for (rows in split(seq_len(nrow(records)), records$pixel)) {
    if(length(rows)>1L && any(diff(records$depth[rows])<=camera$tolerance)) {
      remaining <- rows; ordered <- integer()
      while(length(remaining)) {
        group <- remaining[records$depth[remaining]<=records$depth[remaining[1]]+camera$tolerance]
        group <- group[order(-records$role[group],-records$layer[group],-records$source[group],-records$id[group])]
        ordered <- c(ordered,group)
        remaining <- remaining[!remaining %in% group]
      }
      rows <- ordered
    }
    rows <- rows[!duplicated(records$id[rows])]
    opaque <- which(records$opacity[rows] == 1)
    if (length(opaque)) rows <- rows[seq_len(opaque[1])]
    pixel <- records$pixel[rows[1]]
    lists[[pixel]] <- records$id[rows]; alpha[[pixel]] <- records$opacity[rows]
  }
  visible <- vapply(lists, function(x) if(length(x)) x[1] else 0L, integer(1))
  list(visible=visible, layers=lists, alpha=alpha, boundary=boundary, grid=grid, camera=camera, n=n)
}

ref_rasterise <- function(compiled, packet, n = 160L) {
  grid <- ref_grid(n); f <- compiled$fragments; v <- compiled$vertices
  layers <- rep(list(integer()), nrow(grid)); alpha <- rep(list(double()), nrow(grid))
  for (i in compiled$draw_order) {
    rows <- seq.int(f$offset[i], length.out=f$count[i])
    projected <- cbind(v$x_npc[rows], v$y_npc[rows], v$depth[rows])
    if (f$kind[i] == 1L) covered <- ref_contains(grid, projected[,1:2,drop=FALSE]) else
      if (f$kind[i] == 3L) covered <- ref_footprint(grid, projected[1,], packet, f$source_index[i], compiled$viewport) else
        if (f$kind[i] == 2L) {
          line <- packet$polylines; index <- f$source_index[i]
          covered <- rep(FALSE,nrow(grid))
          for(j in seq_len(nrow(projected)-1L)) covered <- covered |
            ref_segment(grid,projected[c(j,j+1L),,drop=FALSE],line$width_mm[index],compiled$viewport,line$cap[index])$covered
        } else stop("Referee rasteriser requires face, line or marker fragments")
    if (f$mask_count[i] > 0L) {
      mask <- seq.int(f$mask_offset[i],length.out=f$mask_count[i])
      covered <- covered & ref_contains(grid,cbind(compiled$masks$x_npc[mask],compiled$masks$y_npc[mask]))
    }
    if(f$opacity[i] == 0) next
    for(pixel in which(covered)) {
      old <- layers[[pixel]]; aa <- alpha[[pixel]]
      if(f$opacity[i] == 1) {old <- integer(); aa <- double()}
      layers[[pixel]] <- c(f$parent_primitive_id[i],old)
      alpha[[pixel]] <- c(f$opacity[i],aa)
    }
  }
  list(visible=vapply(layers,function(x) if(length(x)) x[1] else 0L,integer(1)),layers=layers,alpha=alpha)
}

ref_score <- function(packet, camera, viewport, compiled, n=160L) {
  oracle <- ref_render(packet,camera,viewport,n)
  observed <- ref_rasterise(compiled,packet,n)
  interior <- !oracle$boundary
  mismatch <- oracle$visible != observed$visible
  translucent <- any(vapply(packet[c("faces","polylines","markers")],function(t) any(t$opacity>0 & t$opacity<1),logical(1)))
  if(translucent) mismatch <- mismatch | !vapply(seq_along(interior),function(i)
    identical(oracle$layers[[i]],observed$layers[[i]]) &&
      isTRUE(all.equal(oracle$alpha[[i]],observed$alpha[[i]],tolerance=1e-12)),logical(1))
  wrong <- which(mismatch & interior)
  list(wrong=length(wrong),scored=sum(interior),covered=sum(interior & oracle$visible!=0),
       fraction=length(wrong)/sum(interior),pixels=wrong,
       locations=oracle$grid[wrong,,drop=FALSE],oracle=oracle,observed=observed)
}
