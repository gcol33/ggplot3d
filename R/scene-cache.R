# Compiled scenes, most recently used last. A draw that repeats a packet,
# camera and viewport (a redraw, a replay, ggsave() after print()) reuses the
# compiled scene. Entries are matched with identical(), which returns at once
# when the inputs share memory with the cached ones, as they do on a redraw.
scene_cache <- new.env(parent = emptyenv())
scene_cache$entries <- list()

scene_cache_limit <- function() {
  bytes <- getOption("ggplot3d.cache_bytes", 256 * 2^20)
  scene_assert(is.numeric(bytes) && length(bytes) == 1L && !is.na(bytes) && bytes >= 0,
               "option ggplot3d.cache_bytes must be a non-negative number")
  bytes
}

scene_cache_find <- function(packet, camera, viewport) {
  entries <- scene_cache$entries
  for (k in rev(seq_along(entries))) {
    entry <- entries[[k]]
    if (identical(entry$viewport, viewport) && identical(entry$camera, camera) &&
        identical(entry$packet, packet)) {
      scene_cache$entries <- c(entries[-k], entries[k])
      return(entry$scene)
    }
  }
  NULL
}

scene_cache_store <- function(packet, camera, viewport, scene, bytes, limit) {
  entries <- scene_cache$entries
  if (bytes > limit) return(invisible())
  entries[[length(entries) + 1L]] <- list(packet = packet, camera = camera, viewport = viewport,
                                          scene = scene, bytes = bytes)
  total <- sum(vapply(entries, function(entry) entry$bytes, numeric(1)))
  while (total > limit) {
    total <- total - entries[[1L]]$bytes
    entries <- entries[-1L]
  }
  scene_cache$entries <- entries
  invisible()
}
