local_gptr_mock <- function(..., .envir = parent.frame()) {
    fn <- testthat::local_mocked_bindings
    args <- names(formals(fn))
    if (".local_envir" %in% args) {
        fn(..., .env = asNamespace("gptr"), .local_envir = .envir)
    } else if (".local_env" %in% args) {
        fn(..., .env = asNamespace("gptr"), .local_env = .envir)
    } else {
        fn(..., .env = asNamespace("gptr"), .envir = .envir)
    }
}
