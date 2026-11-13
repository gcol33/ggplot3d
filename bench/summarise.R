Sys.setlocale("LC_ALL", "C")
args <- commandArgs(TRUE)
run_date <- readLines("bench/run-date.txt",warn=FALSE)[1]
directory <- if(length(args)) args[1] else file.path("bench",paste0("raw-",run_date))
run_date <- sub("^raw-","",basename(directory))
rows <- list()
for(package in c("ggplot3d","ggcube")) for(cells in c(50L,100L,200L,316L)) {
  stem <- file.path(directory,paste0(package,"-",cells))
  path <- paste0(stem,".csv")
  raw <- if(file.exists(path)) read.csv(path) else NULL
  metrics <- c("build_s","gtable_s","png_grob_s","svg_grob_s","ggsave_png_s","ggsave_svg_s","svg_bytes")
  row <- data.frame(package=package,cells=cells,faces=if(package=="ggplot3d") 2*cells^2 else cells^2,
                    repeats=if(is.null(raw)) 0L else sum(raw$status=="ok"))
  for(metric in metrics) row[[metric]] <- if(!is.null(raw) && row$repeats==3L) median(raw[[metric]][raw$status=="ok"]) else NA_real_
  row$peak_commit_bytes <- if(file.exists(paste0(stem,"-memory.txt"))) as.numeric(readLines(paste0(stem,"-memory.txt"),warn=FALSE)[1]) else NA_real_
  row$engine_md5 <- if(file.exists(paste0(stem,"-engine.txt")))
    sub("^DLL MD5 ","",readLines(paste0(stem,"-engine.txt"),warn=FALSE)[2]) else NA_character_
  row$status <- if(row$repeats==3L) "ok" else if(file.exists(paste0(stem,"-error.txt")))
    paste(readLines(paste0(stem,"-error.txt")),collapse=" ") else "process terminated; see stderr and exit code"
  exit_file <- paste0(stem,"-exit.txt")
  if(row$repeats==3L && file.exists(exit_file) && readLines(exit_file,warn=FALSE)[1]!="0")
    row$status <- paste("3 complete repeats; worker exit",readLines(exit_file,warn=FALSE)[1],"(see stderr)")
  if(row$repeats<3L && !file.exists(paste0(stem,"-error.txt")) && file.exists(paste0(stem,"-stderr.log"))) {
    errors <- grep("Error|cannot allocate|bad_alloc|unexpected|Permission denied",
      readLines(paste0(stem,"-stderr.log"),warn=FALSE),value=TRUE)
    if(length(errors)) row$status <- paste(head(errors,2),collapse=" ")
  }
  rows[[length(rows)+1L]] <- row
}
result <- do.call(rbind,rows)
write.csv(result,file.path("bench",paste0("results-",run_date,".csv")),row.names=FALSE)
lines <- c("# Surface benchmarks","",paste("Run date:",run_date),"",
  "Median of three complete repetitions per package and cell grid. Each package/grid runs in a fresh R process, sequentially, with a Windows job memory limit below 4 GiB. Both render deterministic z = sin(pi*x) cos(pi*y), without lighting, at 7 inches square and 150 DPI. ggplot3d emits two triangles per cell; ggcube emits one quad. Default sorting is used for each package.","",
  "Build and gtable timings exclude drawing. Grob device timings include deferred scene compilation. Direct ggsave timings include building, gtable assembly and device drawing. SVG bytes refer to grob output. Incomplete repetitions have no reported median.","",
  "| Package | Cells | Faces | Build s | Gtable s | PNG grob s | SVG grob s | ggsave PNG s | ggsave SVG s | SVG bytes | Status |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|")
for(i in seq_len(nrow(result))) {
  r <- result[i,]
  values <- c(r$package,paste0(r$cells," x ",r$cells),r$faces,
              vapply(r[c("build_s","gtable_s","png_grob_s","svg_grob_s","ggsave_png_s","ggsave_svg_s")],
                function(x) if(is.na(x)) "NA" else sprintf("%.3f",x),character(1)),
              if(is.na(r$svg_bytes)) "NA" else format(r$svg_bytes,scientific=FALSE),r$status)
  lines <- c(lines,paste0("| ",paste(values,collapse=" | ")," |"))
}
hashes <- unique(stats::na.omit(result$engine_md5))
lines <- c(lines,"",paste("Engine DLL MD5:",paste(hashes,collapse=", ")),
  if(length(hashes)>1L) "Engine binaries changed between grid sizes; each CSV row records its measured binary." else character())
profile <- file.path(directory,"ggplot3d-100-profile.csv")
lines <- c(lines,"","## Rprof","","Sampling interval: 0.01 s; 100 x 100 direct PNG ggsave. Self time includes R-visible native entry points.")
profile_error <- file.path(directory,"ggplot3d-100-profile-run-error.txt")
if(file.exists(profile_error) && startsWith(readLines(profile_error,warn=FALSE)[1],"Rprof:"))
  lines <- c(lines,"","The direct PNG profile is partial:",readLines(profile_error,warn=FALSE)[1],
             "These self times describe that captured phase.")
profile_table <- function(path) {
  profile <- head(read.csv(path),5)
  table <- c("| Function | Self seconds | Self percent |","|---|---:|---:|")
  for(i in seq_len(nrow(profile))) table <- c(table,paste0("| `",gsub('"','',profile$function_name[i]),"` | ",
    profile$self.time[i]," | ",profile$self.pct[i]," |"))
  table
}
if(file.exists(profile)) lines <- c(lines,"",profile_table(profile)) else
  lines <- c(lines,"","Direct PNG profile did not complete; consult the worker diagnostic.")
build_profile <- file.path(directory,"ggplot3d-100-build-profile.csv")
if(file.exists(build_profile)) lines <- c(lines,"","Completed build/gtable profile, measured separately:","",profile_table(build_profile))
writeLines(lines,file.path("bench",paste0("summary-",run_date,".md")))
