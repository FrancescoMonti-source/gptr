local_gptr_mock <- function(..., .envir = parent.frame()) {
    testthat::local_mocked_bindings(
        ..., .env = asNamespace("gptr"), .local_envir = .envir
    )
}
