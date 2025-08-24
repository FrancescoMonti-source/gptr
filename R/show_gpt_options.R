#' Show current gptr package options
#'
#' Prints all options that start with "gpt." along with their current values.
#' This is useful to inspect defaults set in `zzz.R` and any overrides applied
#' in your `.Rprofile` or current session.
#'
#' In R, to change an option, you call options() with a name = value pair:
#' options(gpt.lmstudio_base_url = "http://localhost:5678/v1/chat/completions")

#' @return Invisibly returns a named list of package options.
#' @examples
#' show_gpt_options()
#' @export
show_gpt_options <- function() {
  opts <- options()
  gpt_opts <- opts[grep("^gpt\\.", names(opts))]
  if (length(gpt_opts) == 0) {
    cli::cli_warn("No gpt.* options are currently set.")
    return(invisible(list()))
  }
  cli::cli_inform("Current gptr package options:")
  print(gpt_opts)
  invisible(gpt_opts)
}
