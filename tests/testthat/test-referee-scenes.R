source(testthat::test_path("..","fixtures","oracle","reference.R"),local=TRUE)
source(testthat::test_path("..","fixtures","scenes","builders.R"),local=TRUE)

for(fixture_name in names(ref_fixture_builders())) for(camera_name in names(ref_cameras())) local({
  name <- fixture_name; view <- camera_name
  test_that(paste("referee",name,view,"has zero mismatched interior pixels"), {
    f <- ref_fixture(name,view)
    compiled <- ggplot3d:::scene_compile(f$packet,f$camera,f$viewport)
    required <- "exact_visibility"
    if(length(f$packet$markers$primitive_id) || length(f$packet$polylines$primitive_id))
      required <- c(required,"footprint_clipping")
    missing <- required[!vapply(required,function(capability)
      isTRUE(compiled$capabilities[[capability]]),logical(1))]
    if(length(missing)) skip(paste("missing capability:",paste(missing,collapse=", ")))
    score <- ref_score(f$packet,f$camera,f$viewport,compiled)
    expect_gt(score$scored,0L)
    expect_gt(score$covered,0L)
    expect_equal(score$wrong,0L,info=paste("wrong share",score$fraction,"first NPC",
      paste(head(score$locations,1),collapse=",")))
  })
})

test_that("nested closed sheets of one material paint in one seam group per depth layer", {
  for (view in names(ref_cameras())) {
    f <- ref_fixture("nested_isosurfaces", view)
    compiled <- ggplot3d:::scene_compile(f$packet, f$camera, f$viewport)
    expect_length(unique(compiled$fragments$seam_group), 4L)
  }
})
