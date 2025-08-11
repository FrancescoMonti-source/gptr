# gptr <a href="https://github.com/francescomonti/gptr"><img src="https://img.shields.io/badge/dev%20version-0.1.0-blue.svg" alt="Dev version"/></a> <a href="LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-green.svg" alt="License: MIT"/></a>

> High-Level R Interface to LLM APIs for Structured Text Extraction

**gptr** provides a set of high-level helper functions to interact with local or remote Large Language Model (LLM) APIs using OpenAI-compatible endpoints.\
The main goal of this package is **schema-based extraction** from unstructured text, with built-in validation and retries - ideal for reproducible data workflows.

The package is functional but it's heavily work in progress and i'm no developer so expect quite a few rough edges.

------------------------------------------------------------------------

## 🚀 Installation

You can install the development version from GitHub:

``` r
# Install remotes if needed
install.packages("remotes")

# Install gptr from GitHub
remotes::install_github("FrancescoMonti-source/gptr")
```

Once on CRAN, you’ll also be able to install with:

``` r
install.packages("gptr")
```

------------------------------------------------------------------------

## 📝 Prompt building

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

------------------------------------------------------------------------

## 🔑 Keys and value types

In `gpt_column()`, the `keys` parameter defines the schema of the expected output.\
Each key can take **one of two forms**:

1.  **Type string** — values will be coerced to this type:

    -   `"integer"`, `"numeric"`, `"character"`, `"logical"`

2.  **Vector of allowed values** — any output not in the set is replaced with `NA`:

    ``` r
    keys = list(
      severity = c("mild", "moderate", "severe", "NA"),
      smoker   = c(0, 1)  # 0 = no, 1 = yes
    )
    ```

Example:

``` r
keys <- list(
  age       = "integer",                 # coerced to integer
  diagnosis = "character",               # coerced to character
  smoker    = c(0, 1),                    # must be 0 or 1
  severity  = c("mild", "moderate", "severe", "NA")
)

# LLM output outside these constraints will be set to NA
```

This schema is passed to the LLM via `{json_format}` (so it knows what to return). Note that to avoid bind_rows() errors dues the model once answering with `0` and then with `"0"`, everything is coerced to character before joining the results.

------------------------------------------------------------------------

## 📦 Quick start

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

## 🎯 Advanced: function-based prompts

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
