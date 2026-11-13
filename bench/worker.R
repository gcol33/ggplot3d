Sys.setlocale("LC_ALL", "C")
args <- commandArgs(TRUE)
package <- args[1]; cells <- as.integer(args[2]); destination <- args[3]
profile_only <- length(args)>=4L && args[4]=="profile"
metadata_destination <- if(profile_only) paste0(destination,"-profile-run") else destination
if(package=="ggplot3d") devtools::load_all() else {
  stopifnot(as.character(utils::packageVersion("ggcube"))=="0.3.0")
  loadNamespace("ggcube")
}
options(device=function(...) grDevices::pdf(file=NULL,...))
stopifnot(requireNamespace("ragg",quietly=TRUE),requireNamespace("svglite",quietly=TRUE))
make_plot <- function(cells) {
  axis <- seq(-1,1,length.out=cells+1L)
  data <- expand.grid(x=axis,y=axis)
  data$z <- sin(pi*data$x)*cos(pi*data$y)
  stopifnot(nrow(data)==(cells+1L)^2L,!anyDuplicated(data[c("x","y")]))
  geom <- getExportedValue(package,"geom_surface_3d")
  coord <- getExportedValue(package,"coord_3d")
  camera <- if(package=="ggplot3d") coord(azimuth=-60,elevation=30,light=NULL) else
    coord(pitch=0,roll=-60,yaw=-30,persp=FALSE,light=NULL)
  ggplot2::ggplot(data,ggplot2::aes(x,y,z=z)) + geom(colour=NA) + camera +
    ggplot2::theme_bw(base_size=12) + ggplot2::theme(legend.position="none",
      panel.border=ggplot2::element_rect(fill=NA))
}
writeLines(c(capture.output(utils::sessionInfo()),paste("cells",cells),paste("package",package),
             "surface z=sin(pi*x)*cos(pi*y); deterministic input; 7in square; 150dpi; white background",
             "ggplot3d: default vector backend; ggcube: default auto sorting; lighting disabled",
             "panel.border = element_rect(fill = NA) for both packages"),paste0(metadata_destination,"-session.txt"))
if(package=="ggplot3d") {
  dll <- getLoadedDLLs()[["ggplot3d"]][["path"]]
  writeLines(c(paste("DLL",dll),paste("DLL MD5",tools::md5sum(dll))),paste0(metadata_destination,"-engine.txt"))
}
profile_file <- paste0(destination,".Rprof")
write_profile <- function() {
  utils::Rprof(NULL)
  if(file.exists(profile_file)) {
    profile <- utils::summaryRprof(profile_file)$by.self
    write.csv(data.frame(function_name=rownames(profile),profile),paste0(destination,"-profile.csv"),row.names=FALSE)
  }
}
grob_count <- function(grob) {
  children <- c(as.list(grob$children),grob$grobs)
  1L + sum(vapply(children,grob_count,integer(1)))
}
rows <- list(); stage <- "warmup"
tryCatch({
  warm <- make_plot(5L)
  invisible(ggplot2::ggplotGrob(warm))
  plot <- make_plot(cells)
  if(profile_only) {
    stopifnot(package=="ggplot3d",cells==100L)
    stage <- "build/gtable Rprof"
    build_profile <- paste0(destination,"-build.Rprof")
    utils::Rprof(build_profile,interval=.01)
    profile_built <- ggplot2::ggplot_build(plot)
    profile_grob <- ggplot2::ggplot_gtable(profile_built)
    utils::Rprof(NULL)
    profile <- utils::summaryRprof(build_profile)$by.self
    write.csv(data.frame(function_name=rownames(profile),profile),paste0(destination,"-build-profile.csv"),row.names=FALSE)
    rm(profile_built,profile_grob); gc()
    stage <- "Rprof"
    utils::Rprof(profile_file,interval=.01)
    ggplot2::ggsave(paste0(destination,"-profile.png"),plot,width=7,height=7,units="in",dpi=150,
      device=ragg::agg_png,bg="white")
    write_profile()
  } else for(repetition in 1:3) {
    row <- data.frame(package=package,version=as.character(utils::packageVersion(package)),cells=cells,
      vertices=(cells+1L)^2L,faces=if(package=="ggplot3d") 2L*cells^2L else cells^2L,
      corners=if(package=="ggplot3d") 6L*cells^2L else 4L*cells^2L,rep=repetition,
      build_s=NA_real_,gtable_s=NA_real_,png_grob_s=NA_real_,svg_grob_s=NA_real_,
      ggsave_png_s=NA_real_,ggsave_svg_s=NA_real_,svg_bytes=NA_real_,grob_count=NA_integer_,status="running")
    rows[[repetition]] <- row
    record <- function() write.csv(do.call(rbind,rows),paste0(destination,".csv"),row.names=FALSE)
    elapsed <- function(expr) unname(system.time(force(expr))[["elapsed"]])
    gc(); stage <- "ggplot_build"
    rows[[repetition]]$build_s <- elapsed(built <- ggplot2::ggplot_build(plot)); record()
    expected <- if(package=="ggplot3d") 6L*cells^2L else 4L*cells^2L
    stopifnot(nrow(built$data[[1]])==expected)
    stage <- "ggplot_gtable"
    rows[[repetition]]$gtable_s <- elapsed(grob <- ggplot2::ggplot_gtable(built)); record()
    rows[[repetition]]$grob_count <- grob_count(grob)
    stem <- paste0(destination,"-rep",repetition)
    save <- function(object,extension,device) ggplot2::ggsave(paste0(stem,extension),object,
      width=7,height=7,units="in",dpi=150,device=device,bg="white",limitsize=FALSE)
    stage <- "PNG grob writing"
    rows[[repetition]]$png_grob_s <- elapsed(save(grob,".png",ragg::agg_png)); record()
    stage <- "SVG grob writing"
    rows[[repetition]]$svg_grob_s <- elapsed(save(grob,".svg",svglite::svglite)); record()
    rows[[repetition]]$svg_bytes <- file.info(paste0(stem,".svg"))$size
    stage <- "direct PNG ggsave"
    rows[[repetition]]$ggsave_png_s <- elapsed(save(plot,"-direct.png",ragg::agg_png)); record()
    stage <- "direct SVG ggsave"
    rows[[repetition]]$ggsave_svg_s <- elapsed(save(plot,"-direct.svg",svglite::svglite)); record()
    rows[[repetition]]$status <- "ok"; record()
    cat(package,cells,repetition,"complete\n"); flush.console()
    rm(built,grob); gc()
  }
},error=function(error) {
  if(profile_only) write_profile()
  if(length(rows)) {
    rows[[length(rows)]]$status <- paste(stage,conditionMessage(error),sep=": ")
    write.csv(do.call(rbind,rows),paste0(destination,".csv"),row.names=FALSE)
  }
  writeLines(paste(stage,conditionMessage(error),sep=": "),paste0(metadata_destination,"-error.txt"))
})
writeLines("complete",paste0(metadata_destination,"-done.txt"))
