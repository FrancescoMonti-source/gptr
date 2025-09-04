# Setup hooks for gptr tests

# Capture default gptr.* options at load time
.gptr_default_options <- options()[grepl("^gptr\\.", names(options()))]

# Ensure each test starts with a clean cache and default options
delete_models_cache()
withr::defer(delete_models_cache(), testthat::teardown_env())
withr::local_options(.gptr_default_options, .local_envir = testthat::teardown_env())
