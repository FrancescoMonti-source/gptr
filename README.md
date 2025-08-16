# gptr — Structured Data Extraction from Free Text with LLMs

`gptr` is an R package that turns messy, domain-specific text into tidy, validated variables using large language models (LLMs).

It’s **model-agnostic** (local or API), robust to imperfect outputs, and designed for **reproducible pipelines** in research or production.

------------------------------------------------------------------------

## ✨ Why use gptr

-   Consistent, validated outputs from LLMs — no manual cleanup
-   Works with both **local** (LM Studio, Ollama, LocalAI) and **API** (OpenAI, Mistral) models
-   Built for **data pipelines** — tidyverse-friendly, parallel-ready
-   Rich diagnostics: raw outputs, invalid row tracking, retry helpers

------------------------------------------------------------------------

## 📦 Installation

``` r
# Install from GitHub
remotes::install_github("FrancescoMonti-source/gptr")
```

------------------------------------------------------------------------

## 🚀 Quick start

``` r
library(gptr)
library(tibble)

# Define a prompt template
template <- "Extract age and diagnosis from:\n{text}\nFormat: {json_format}"

# Example data
df <- tibble(
  id   = 1,
  note = "Patient is 64 years old, diagnosed with type 2 diabetes."
)

# Run extraction
res <- gpt_column(
  data   = df,
  col    = note,
  prompt = template,  # placeholders {text} and {json_format} are filled automatically
  keys   = list(
    age       = "integer",
    diagnosis = "character"
  ),
  provider     = "openai",       # or "lmstudio"
  model        = "gpt-4o-mini",  # change to your model
  temperature  = 0.2,
  return_debug = TRUE
)

res
```

------------------------------------------------------------------------

## 🔍 How it works — the big picture

```         
┌─────────────────────────────────────────────────────────────────────┐
│ LAYER 3 — ORCHESTRATION                                             │
│  gpt_column()                                                       │
│   ├─ loops over data[[col]]                                         │
│   ├─ progress + ETA (progressr)                                     │
│   ├─ optional parallel (furrr)                                      │
│   ├─ attaches .raw_output / .invalid_rows                           │
│   └─ binds per-row tibble rows back to original data                │
└─────────────────────────────────────────────────────────────────────┘
                 │ per row
                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│ LAYER 2 — MID-LEVEL (one-row pipeline)                              │
│  1) trim_text()                               (preprocess)          │
│  2) build_prompt() ← json_format_from_keys()  (prompt)              │
│  3) gpt()                                        (LLM call)         │
│  4) tidy_json() → jsonlite::fromJSON()          (repair+parse)      │
│  5) coerce_type() / in_allowed() / is_na_like() (validate)          │
│  6) match_arg_tol() + fill-missing             (align keys)         │
│  7) row_to_tibble()                            (shape output)       │
│  (If relaxed && no keys: skip validation/alignment; return raw/parsed)
└─────────────────────────────────────────────────────────────────────┘
                 ▲                 ▲
                 │                 │ uses
                 │                 │
┌─────────────────────────────────────────────────────────────────────┐
│ LAYER 1 — LOW-LEVEL UTILITIES                                       │
│  %||%, normalize_token(), parse_key_spec(), coerce_type(),          │
│  in_allowed(), is_na_like(), tidy_json(), match_arg_tol(), trim_text() │
└─────────────────────────────────────────────────────────────────────┘
```

------------------------------------------------------------------------

## 📖 Core API

-   **`gpt_column()`** — Main orchestrator: builds prompts, calls model, repairs/parses JSON, validates, aligns keys, returns tibble. Supports progress, ETA, parallel, debug columns.
-   **`build_prompt()`** — Injects `{text}` and `{json_format}` into your prompt template, using `keys` to document allowed values.
-   **Key utilities** (used internally, available to you):
    -   `trim_text()`, `tidy_json()`, `parse_key_spec()`, `coerce_type()`, `in_allowed()`, `match_arg_tol()`, `is_na_like()`, `%||%`, `normalize_token()`

> All functions link via `@seealso` in pkgdown for easy navigation.

------------------------------------------------------------------------

## 🗝 Designing keys & prompts

`keys` is a **named list** that defines the variables to extract and their expected types: - `"integer"`, `"numeric"`, `"character"`, `"logical"`, or - a **set of allowed values**: e.g., `c("léger", "modéré", "sévère")`

Example:

``` r
keys <- list(
  impulsivite = "integer",
  severite    = c("léger", "modéré", "sévère")
)
```

These guide the LLM and enable strict validation (`coerce_type()`, `in_allowed()`).

### Prompt templates

When `prompt` is a character template: - **`{text}`** → replaced with the current row's text from `col` - **`{json_format}`** → a JSON skeleton from `keys`, showing expected fields and allowed values

Example:

``` r
tpl <- paste0(
  'Tu es un assistant d\'extraction structurée.',
  '\nTexte: "{text}"',
  '\nRenvoie STRICTEMENT un JSON une seule ligne: {json_format}'
)
```

Example JSON format for:

``` r
keys = list(age = "integer", diagnosis = c("diabetes", "hypertension"))
```

would be:

``` json
{"age": "0"|"1"|"NA", "diagnosis": "diabetes"|"hypertension"}
```

You can also pass a **function** to `prompt` for full control.

------------------------------------------------------------------------

## ⚡ Parallel & progress

``` r
library(future)
future::plan(multisession, workers = 4)

res <- gpt_column(
  data = df,
  col = notes,
  prompt = tpl,
  keys = keys,
  parallel = TRUE
)
```

Progress and ETA are provided via `progressr`.

------------------------------------------------------------------------

## 🐞 Debugging

-   `return_debug = TRUE` (default) adds:
    -   `.raw_output` — model’s raw string per row
    -   `.invalid_rows` — rows failing parsing/validation
-   `show_invalid_rows = TRUE` prints offending inputs

------------------------------------------------------------------------

## 🔁 Retrying failed rows

If some rows fail schema validation, `gpt_column()` tags them in the `"invalid_rows"` attribute.

``` r
attr(res, "invalid_rows")

res2 <- patch_failed_rows(
  data   = res,
  prompt = template,
  col    = note,
  id_col = id,
  keys   = list(
    age       = "integer",
    diagnosis = "character"
  ),
  max_attempts = 2
)

attr(res2, "invalid_rows")
```

------------------------------------------------------------------------

## 💬 Multi-turn conversations

``` r
gpt_chat("Hi! My name is Bob.")
gpt_chat("What's my name?")
gpt_chat(show_history = TRUE)
gpt_chat(reset = TRUE)  # start fresh
```

Multi-turn chat works with all supported providers (see below).

------------------------------------------------------------------------

## 🔗 Supported providers

-   **LM Studio** (local inference server, OpenAI-compatible API)
-   **OpenAI** (`gpt-4o`, `gpt-4o-mini`, `gpt-3.5-turbo`, `gpt-4.1`, etc.)
-   Any OpenAI-compatible endpoint (self-hosted models, fine-tuned endpoints, etc.)

------------------------------------------------------------------------

## 📄 License

MIT License — see [LICENSE.md](LICENSE.md)

------------------------------------------------------------------------

## 🛠 Requirements

-   R ≥ 4.1
-   Packages: `httr`, `httr2`, `tidyverse`, `jsonlite`, `stringr`, `purrr`, `tools`, `cli`
-   Suggested: `furrr`, `pdftools`, `officer`, `mime`

------------------------------------------------------------------------

## 🤝 Contributing

Issues and pull requests are welcome. Please open an issue to discuss proposed changes before submitting a PR.

------------------------------------------------------------------------

## 📚 See also

-   [OpenAI API documentation](https://platform.openai.com/docs/)
-   [LM Studio](https://lmstudio.ai/)
-   [furrr parallel docs](https://furrr.futureverse.org/)
