.gptr_state <- local({
  e <- new.env(parent = emptyenv())
  e$detected_backend <- NULL
  e$warned_missing <- FALSE
  e$ping_cache <- new.env(parent = emptyenv()) # base_root -> TRUE/FALSE
  e
})
