# gptr — Structured Data Extraction from Free Text with LLMs

`gptr` is an R package that turns messy, domain-specific text into tidy, validated variables using large language models (LLMs).

It’s **model-agnostic** (local or API), robust to imperfect outputs, and designed for **reproducible pipelines** in research or production.

------------------------------------------------------------------------

## ✨ Features

-   **Prompt templating** with `{text}` and `{json_format}` placeholders.
-   **Model-agnostic LLM calls** via `gpt()` — works with local models (LM Studio, Ollama, LocalAI) or APIs (OpenAI, Mistral).
-   **Automatic JSON repair** for slightly broken model outputs (`tidy_json()`).
-   **Schema validation**: enforce types and allowed values from `keys`.
-   **Key autocorrect** with fuzzy matching (`match_arg_tol()`).
-   **Progress & ETA**, parallel processing via `furrr`.
-   **Debug mode** with raw outputs and invalid row tracking.

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
df <- tibble(id = 1, note = "Patient is 64 years old, diagnosed with type 2 diabetes.")

# Send to the LLM and parse
res <- gpt_column(
  data   = df,
  col    = note,
  prompt = template,  # placeholders {text} and {json_format} are filled automatically
  keys   = list(
    age       = "integer",
    diagnosis = "character"
  ),
  provider = "openai",           # or "lmstudio"
  model    = "gpt-4o-mini",      # change to your model
  temperature = 0.2,
  return_debug = TRUE
)

res
#> # A tibble: 1 × 5
#>      id note                                   age diagnosis            .invalid_rows
#>   <dbl> <chr>                                <int> <chr>                        <dbl>
#> 1     1 Patient is 64 years old, diagn...       64 type 2 diabetes                   0
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
│ LAYER 1 — LOW-LEVEL UTILITIES (pure helpers, no LLM knowledge)      │
│  %||%   normalize_token()  parse_key_spec()  coerce_type()          │
│  in_allowed()  is_na_like()  tidy_json()  match_arg_tol()  trim_text() │
└─────────────────────────────────────────────────────────────────────┘
```

------------------------------------------------------------------------

## 📖 Core API

-   **`gpt_column()`** — Main orchestrator. Loops through your column, builds prompts, calls the model, repairs/parses JSON, validates, aligns keys, returns tibble.\
    Supports progress, ETA, parallel, debug columns.

-   **`build_prompt()`** — Injects `{text}` and `{json_format}` into your prompt template, using `keys` to document allowed values.

-   **Utilities** (used internally, available to you):

    -   Text processing: `trim_text()`
    -   JSON repair: `tidy_json()`
    -   Schema parsing: `parse_key_spec()`
    -   Type handling: `coerce_type()`, `in_allowed()`
    -   Name matching: `match_arg_tol()`
    -   Missing detection: `is_na_like()`
    -   Misc: `%||%`, `normalize_token()`

> All function docs are linked via `@seealso` for easy navigation in pkgdown.

------------------------------------------------------------------------

## 🗝 Designing `keys`

`keys` is a **named list** and corresponds to the columns you wanna create. 
Providing this parameter as list allows us to both define the name of the variable and the type of variable we expect: `"integer"`, `"numeric"`, `"character"`, `"logical"`, or we can provide - an **allowed set**: e.g., `c("léger", "modéré", "sévère")`.

Example:

``` r
keys <- list(
  impulsivite = "integer",
  severite    = c("léger", "modéré", "sévère")
)
```

Defining this scheme routes the LLM in the right direction. Values are validated via `coerce_type()` and `in_allowed()`.

------------------------------------------------------------------------

## ✍ Prompt building

Example prompt:

``` r
tpl <- paste0(
  'Tu es un assistant d\'extraction structurée.',
  '\nTexte: "{text}"',
  '\nRenvoie STRICTEMENT un JSON une seule ligne: {json_format}'
)
```

When `prompt` is given as a character template, `gptr` automatically substitutes:

-   **`{text}`** → the current row's value from the column specified in `col` (your unstructured free text). This works as a placeholder, dinamically replace with text by glue() when gpt_column() is called.
-   **`{json_format}`** → a JSON skeleton generated from `keys`, showing expected fields according to the data type specified. Alternatively, each key can take a vector of predefined "acceptable" values.


Example JSON format if:

``` r
keys = list(age = "integer", diagnosis = "character")
```

would be:

``` json
{"age": "0"|"1"|"NA", "diagnosis": "value1|value2|etc"}
```

This ensures the LLM sees exactly what fields to return and how to format them.

For full control, you can pass a function to `prompt` instead of a character template:

``` r
prompt_fun <- function(text, keys) {
  paste0(
    "Analyse the following text:\n", text, "\n\n",
    "Return a JSON with these keys: ",
    paste(names(keys), collapse = ", "), "\n",
    "JSON format:\n",
    jsonlite::toJSON(setNames(as.list(rep("NA", length(keys))), names(keys)), auto_unbox = TRUE)
  )
}

res <- gpt_column(
  data   = df,
  col    = note,
  prompt = prompt_fun,
  keys   = list(
    age       = "integer",
    diagnosis = "character"
  ),
  provider = "openai",
  model    = "gpt-4o-mini",
  temperature = 0.2,
  return_debug = TRUE
)
```

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

-   Set `return_debug = TRUE` (default) to get:
    -   `.raw_output` — model’s raw string per row.
    -   `.invalid_rows` — rows where parsing/validation failed.
-   Use `show_invalid_rows = TRUE` to print offending inputs.

------------------------------------------------------------------------

## 💡 Usage patterns

-   **Free-text to binary flags**:\
    Extract presence/absence of conditions, habits, or events.
-   **Free-text to enums**:\
    Map text to predefined categories (`"léger"`, `"modéré"`, `"sévère"`).
-   **Free-text to numeric**:\
    Extract counts, doses, scores.

------------------------------------------------------------------------
## 🔁 Retrying failed rows

If some rows fail schema validation, `gpt_column()` tags them in the `"invalid_rows"` attribute.

``` r
attr(res, "invalid_rows")
#> [1] 3 7 12

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
#> integer(0)  # all fixed
```

------------------------------------------------------------------------

## 💬 Multi-turn conversations

``` r
gpt_chat(reset = TRUE)  # start fresh
gpt_chat(system = "You are a concise medical data assistant.")
gpt_chat("Ciao! Facciamo un test?")
gpt_chat("Ricordati che lavoro in sanità pubblica.")
gpt_chat(show_history = TRUE)
```

Works with LM Studio (local) or OpenAI (set `base_url` and `api_key`).

------------------------------------------------------------------------

## 🔗 Supported providers

-   **LM Studio** (local inference server, OpenAI-compatible API)
-   **OpenAI** models (e.g., `gpt-4o`, `gpt-4o-mini`, `gpt-3.5-turbo`, `gpt-4.1`)
-   Any OpenAI-compatible endpoint (self-hosted models, fine-tuned endpoints, etc.)

------------------------------------------------------------------------

## 📄 License

This package is released under the [MIT License](LICENSE.md).

------------------------------------------------------------------------

## 🛠 Requirements

-   R ≥ 4.1
-   Packages: `httr`, `httr2`, `tidyverse`, `jsonlite`, `stringr`, `purrr`, `tools`, `cli`, plus suggested packages for specific features (`furrr`, `pdftools`, `officer`, `mime`).

------------------------------------------------------------------------

## 🤝 Contributing

Issues and pull requests are welcome!\
Please open an issue to discuss proposed changes before submitting a PR.
------------------------------------------------------------------------

## 📚 See also

-   [OpenAI API documentation](https://platform.openai.com/docs/)
-   [LM Studio](https://lmstudio.ai/)
-   [furrr parallel docs](https://furrr.futureverse.org/)
