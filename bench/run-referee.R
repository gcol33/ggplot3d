Sys.setlocale("LC_ALL", "C")
devtools::load_all()
source("tests/fixtures/oracle/reference.R")
source("tests/fixtures/scenes/builders.R")
rows <- list()
selected <- commandArgs(TRUE)
selected_names <- names(ref_fixture_builders())
if(length(selected)) {
  selected_names <- intersect(selected_names,selected)
  if(file.exists("bench/referee-matrix.csv")) {
    previous <- read.csv("bench/referee-matrix.csv")
    previous <- previous[!previous$fixture %in% selected_names,,drop=FALSE]
    if(nrow(previous)) rows <- list(previous)
  }
}
for(name in selected_names) for(view in names(ref_cameras())) {
  cat(name,view,"\n"); flush.console()
  f <- ref_fixture(name,view)
  result <- tryCatch({
    compiled <- ggplot3d:::scene_compile(f$packet,f$camera,f$viewport)
    if(!file.exists("bench/referee-engine.rds") || identical(name,selected_names[1]) && identical(view,names(ref_cameras())[1])) {
      dll <- getLoadedDLLs()[["ggplot3d"]][["path"]]
      saveRDS(list(capabilities=compiled$capabilities,dll_md5=tools::md5sum(dll),
        measured_at=Sys.time(),r_version=R.version.string),"bench/referee-engine.rds")
    }
    needed <- c("exact_visibility",if(length(f$packet$markers$primitive_id) || length(f$packet$polylines$primitive_id)) "footprint_clipping")
    missing <- needed[!vapply(needed,function(x) isTRUE(compiled$capabilities[[x]]),logical(1))]
    {
      score <- ref_score(f$packet,f$camera,f$viewport,compiled)
      if(score$wrong) saveRDS(list(fixture=f,pixels=score$pixels,locations=score$locations,
        expected=score$oracle$visible[score$pixels],observed=score$observed$visible[score$pixels]),
        file.path("bench",paste0("failure-",name,"-",view,".rds")))
      list(status=if(length(missing)) paste("SKIP",paste(missing,collapse=",")) else if(score$wrong) "FAIL" else "PASS",
           diagnostic=if(score$wrong) "FAIL" else "PASS",wrong=score$wrong,scored=score$scored,covered=score$covered)
    }
  },error=function(e) list(status=paste("ERROR",conditionMessage(e)),diagnostic="ERROR",wrong=NA,scored=NA,covered=NA))
  rows[[length(rows)+1L]] <- data.frame(fixture=name,camera=view,as.data.frame(result))
  write.csv(do.call(rbind,rows),"bench/referee-matrix.csv",row.names=FALSE)
}
test_results <- devtools::test(filter="referee")
saveRDS(test_results,"bench/referee-tests.rds")
test_table <- as.data.frame(test_results)
test_table <- test_table[!vapply(test_table,is.list,logical(1))]
write.csv(test_table,"bench/referee-tests.csv",row.names=FALSE)
