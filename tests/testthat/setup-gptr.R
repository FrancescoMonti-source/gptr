# Setup hooks for gptr tests

# Capture default gptr.* options at load time
.gptr_default_options <- options()[grepl("^gptr\\.", names(options()))]

# Ensure each test starts with a clean cache and default options
testthat::set_hook("test", function(desc, env) {
  delete_models_cache()
  withr::local_options(.gptr_default_options, .local_envir = env)
}, action = "before")
