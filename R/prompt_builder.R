.is_single_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

.prompt_has_placeholder <- function(template, name) {
  grepl(sprintf("(?<!\\{)\\{%s\\}(?!\\})", name), template, perl = TRUE)
}

# The prompt interface now has two lanes:
#   1. legacy/raw `prompt`, where the user owns the full wording
#   2. managed `instruction` / `template`, where `gptr` owns the structure boilerplate
# This helper validates that the call is using exactly one lane and returns the
# normalized strategy that `gpt_column()` / `patch_failed_rows()` should follow.
.resolve_prompt_strategy <- function(prompt = NULL,
                                     instruction = NULL,
                                     template = NULL,
                                     caller = "gpt_column") {
  has_prompt <- !is.null(prompt)
  has_instruction <- !is.null(instruction)
  has_template <- !is.null(template)

  if (has_prompt && (has_instruction || has_template)) {
    stop(
      sprintf(
        "`%s()` accepts either `prompt` (legacy/raw mode) or `instruction`/`template`, not both.",
        caller
      ),
      call. = FALSE
    )
  }

  if (has_template && !has_instruction) {
    stop("`template` requires `instruction`.", call. = FALSE)
  }

  if (has_prompt) {
    if (!is.function(prompt) && !.is_single_string(prompt)) {
      stop("`prompt` must be a single character string or a function(text, keys).", call. = FALSE)
    }
    return(list(
      kind = "legacy",
      prompt = prompt,
      instruction = NULL,
      template = NULL
    ))
  }

  if (!has_instruction) {
    stop(
      sprintf("Supply either `instruction` or `prompt` in `%s()`.", caller),
      call. = FALSE
    )
  }
  if (!.is_single_string(instruction)) {
    stop("`instruction` must be a single character string.", call. = FALSE)
  }
  if (has_template && !.is_single_string(template)) {
    stop("`template` must be a single character string.", call. = FALSE)
  }

  list(
    kind = "managed",
    prompt = NULL,
    instruction = instruction,
    template = template
  )
}

.render_template_section <- function(template, instruction, text) {
  as.character(glue::glue(
    template,
    .envir = rlang::env(
      instruction = instruction,
      text = text
    )
  ))
}

.format_prompt_section <- function(title, lines) {
  lines <- lines[!is.na(lines)]
  lines <- as.character(lines)
  paste(c(title, lines), collapse = "\n")
}

.format_prompt_rule_value <- function(value) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return("NA")
  }
  if (is.logical(value) && length(value) == 1L) {
    return(tolower(as.character(value)))
  }
  as.character(value)
}

.describe_prompt_value_rule <- function(name, spec) {
  if (!is.null(spec$allowed)) {
    allowed <- vapply(spec$allowed, .format_prompt_rule_value, character(1))
    return(sprintf("- %s: one of %s, or NA if not stated", name, paste(allowed, collapse = ", ")))
  }

  label <- switch(
    .effective_key_type(spec),
    integer = "integer",
    numeric = "number",
    logical = "boolean",
    character = "short text",
    "value"
  )

  sprintf("- %s: %s, or NA if not stated", name, label)
}

.build_repair_prompt_scaffold <- function(key_specs = NULL) {
  sections <- list()

  if (!is.null(key_specs) && length(key_specs)) {
    sections[[length(sections) + 1L]] <- .format_prompt_section(
      "Required keys:",
      paste0("- ", names(key_specs))
    )
    sections[[length(sections) + 1L]] <- .format_prompt_section(
      "Value rules:",
      mapply(
        .describe_prompt_value_rule,
        name = names(key_specs),
        spec = unname(key_specs),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      )
    )
  }

  output_lines <- c(
    "- Return one JSON object on a single line.",
    if (!is.null(key_specs) && length(key_specs)) "- Use exactly the required keys and no others.",
    "- Use NA when a value is not stated.",
    "- Do not add commentary or markdown."
  )
  sections[[length(sections) + 1L]] <- .format_prompt_section("Output constraints:", output_lines)

  sections
}

.build_native_prompt_scaffold <- function() {
  list(
    .format_prompt_section(
      "Structured output:",
      c(
        "- Follow the requested extraction task for this text.",
        "- The output schema is enforced separately.",
        "- Do not add commentary or markdown."
      )
    )
  )
}

# This helper builds the prompt text for the managed path only.
# In repair mode it adds key/value/output rules because the model has to honor
# structure via the prompt itself. In native mode it stays much lighter because
# the schema already travels separately through `response_format`.
.build_managed_extraction_prompt <- function(text,
                                             instruction,
                                             template = NULL,
                                             key_specs = NULL,
                                             mode = c("repair", "native")) {
  mode <- match.arg(mode)
  scaffold <- if (identical(mode, "native")) {
    .build_native_prompt_scaffold()
  } else {
    .build_repair_prompt_scaffold(key_specs)
  }

  sections <- if (is.null(template)) {
    c(
      .format_prompt_section("Task:", instruction),
      scaffold,
      .format_prompt_section("Text:", text)
    )
  } else {
    rendered_template <- .render_template_section(template, instruction = instruction, text = text)
    extra_sections <- character()

    if (!.prompt_has_placeholder(template, "instruction")) {
      extra_sections <- c(extra_sections, .format_prompt_section("Task:", instruction))
    }
    if (!.prompt_has_placeholder(template, "text")) {
      extra_sections <- c(extra_sections, .format_prompt_section("Text:", text))
    }

    c(rendered_template, extra_sections, scaffold)
  }

  paste(sections, collapse = "\n\n")
}
