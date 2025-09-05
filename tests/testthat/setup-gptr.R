# Setup hooks for gptr tests

# Capture default gptr.* options at load time
.gptr_default_options <- options()[grepl("^gptr\\.", names(options()))]

# Capture the original backend resolution functions
.orig_resolve_model_provider <- gptr:::.resolve_model_provider
.orig_fetch_models_cached    <- gptr:::.fetch_models_cached

# Ensure each test starts with a clean cache and default options
delete_models_cache()
withr::local_options(.gptr_default_options, .local_envir = testthat::teardown_env())

withr::defer({
  assignInNamespace(".resolve_model_provider",
                    .orig_resolve_model_provider, "gptr")
  assignInNamespace(".fetch_models_cached",
                    .orig_fetch_models_cached, "gptr")
  delete_models_cache()
}, testthat::teardown_env())

