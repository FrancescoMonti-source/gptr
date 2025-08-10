# gptr <a href="https://github.com/francescomonti/gptr"><img src="https://img.shields.io/badge/dev%20version-0.1.0-blue.svg" alt="Dev version"></a> <a href="LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-green.svg" alt="License: MIT"></a>

> High-Level R Interface to LLM APIs for Structured Text Extraction

**gptr** provides a set of high-level helper functions to interact with local or remote Large Language Model (LLM) APIs using OpenAI-compatible endpoints.  
It is designed for **schema-based extraction** from unstructured text, with built-in validation, retries, and conversation memory — ideal for reproducible data workflows.

---

## 🚀 Installation

You can install the development version from GitHub:

```r
# Install remotes if needed
install.packages("remotes")

# Install gptr from GitHub
remotes::install_github("francescomonti/gptr")
```

Once on CRAN, you’ll also be able to install with:

```r
install.packages("gptr")
```

---

## ✨ Features

- **Unified LLM interface**: Call local servers (e.g., [LM Studio](https://lmstudio.ai)) or hosted APIs (e.g., OpenAI) with the same syntax.
- **Schema validation**: Define expected keys and value sets, with type coercion.
- **Fuzzy key matching**: Auto-correct near-miss keys returned by the model.
- **Handles multiple input types**: Plain text, PDF/Word files, and images.
- **Retry mechanism**: `patch_failed_rows()` reprocesses only failed rows.
- **Conversation memory**: Maintain context across turns with `gpt_chat()`.
- **Progress tracking**: Built-in ETA and progress reporting.

---

## 📦 Quick start

```r
library(gptr)
library(tibble)

# Define a prompt template
template <- "Extract age and diagnosis from:\n{text}\nFormat: {json_format}"

# Build a row-specific prompt
prompt_fun <- function(text, keys) build_prompt(template, text, keys)

# Example data
df <- tibble(id = 1, note = "Patient is 64 years old, diagnosed with type 2 diabetes.")

# Send to the LLM and parse
res <- gpt_column(
  data   = df,
  col    = note,
  prompt = prompt_fun,
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

---

## 🔁 Retrying failed rows

If some rows fail schema validation, `gpt_column()` tags them in the `"invalid_rows"` attribute.

```r
attr(res, "invalid_rows")
#> [1] 3 7 12

res2 <- patch_failed_rows(
  data   = res,
  prompt = prompt_fun,
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

---

## 💬 Multi-turn conversations

```r
gpt_chat(reset = TRUE)  # start fresh
gpt_chat(system = "You are a concise medical data assistant.")
gpt_chat("Ciao! Facciamo un test?")
gpt_chat("Ricordati che lavoro in sanità pubblica.")
gpt_chat(show_history = TRUE)
```

Works with LM Studio (local) or OpenAI (set `base_url` and `api_key`).

---

## 🔗 Supported providers

- **LM Studio** (local inference server, OpenAI-compatible API)
- **OpenAI** models (e.g., `gpt-4o`, `gpt-4o-mini`, `gpt-3.5-turbo`, `gpt-4.1`)
- Any OpenAI-compatible endpoint (self-hosted models, fine-tuned endpoints, etc.)

---

## 📄 License

This package is released under the [MIT License](LICENSE.md).

---

## 🛠 Requirements

- R ≥ 4.1
- Packages: `httr`, `httr2`, `tidyverse`, `jsonlite`, `stringr`, `purrr`, `tools`, `cli`, plus suggested packages for specific features (`furrr`, `pdftools`, `officer`, `mime`).

---

## 🤝 Contributing

Issues and pull requests are welcome!  
Please open an issue to discuss proposed changes before submitting a PR.
