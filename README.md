# gptr - Structured Data Extraction with R + LLMs

`gptr` turns unstructured, domain-specific text into tidy, validated data frames using large language models (LLMs). It wraps prompt templating, provider orchestration, schema enforcement, and observability so you can rely on LLM-powered extraction inside reproducible R pipelines.

## Contents

-   [Why gptr](#why-gptr)
-   [New here? Start in 5 minutes](#new-here-start-in-5-minutes)
-   [Installation](#installation)
-   [Quick start](#quick-start)
-   [Common first-run issues](#common-first-run-issues)
-   [A practical extraction checklist](#a-practical-extraction-checklist)
-   [Workflow overview](#workflow-overview)
-   [Schema keys](#schema-keys)
-   [Prompt building and schema injection](#prompt-building-and-schema-injection)
-   [Response validation pipeline](#response-validation-pipeline)
-   [Debugging, retries, and auditing](#debugging-retries-and-auditing)
-   [Multiple ways to use gptr](#multiple-ways-to-use-gptr)
-   [Quality-of-life helpers](#quality-of-life-helpers)
-   [Other helpers](#other-helpers)
-   [Advanced tuning](#advanced-tuning)
-   [Provider support](#provider-support)
-   [FAQ](#faq)
-   [Requirements](#requirements)
-   [Contributing](#contributing)
-   [License](#license)

## Why gptr

-   Schema-aware parsing that repairs JSON, coerces column types, enforces allowed values, and optionally aligns fuzzy key names.
-   Provider-agnostic orchestration: talk to OpenAI, LM Studio, Ollama, LocalAI, or any OpenAI-compatible endpoint with the same R call.
-   Pipeline-friendly design: `gpt_column()` binds predictions back to the original tibble, honours tidy evaluation, and plays nicely with progress bars and parallel plans.
-   Built-in diagnostics: capture raw outputs, invalid rows, retry helpers, and `keep_unexpected_keys` controls for messy payloads.
-   Broader toolbox: one-off completions via `gpt()`, conversational state with `gpt_chat()`, text-to-speech using `gpt_tts()`, model discovery utilities, and cache helpers.

### When to use gptr

-   Extract structured fields (age, diagnosis, amounts, dates) from messy text.
-   Ask a model complex questions about a text (summaries/interpretation/etc..)
-   Turn meeting notes, tickets, or lab reports into tidy tables.
-   Build repeatable extraction pipelines that need validation and retry paths.

## New here? Start in 5 minutes

1.  **Install the package** (see below).
2.  **Configure a provider**:
    -   Cloud: set `OPENAI_API_KEY` in your `.Renviron` or session.
    -   Local: run LM Studio, Ollama, or LocalAI and pick a model name.
3.  **Sanity check** your setup with a tiny call using the provider you configured:

```{r, eval = FALSE}
library(gptr)
# OpenAI (requires OPENAI_API_KEY)
gpt("ping", provider = "openai", model = "gpt-4o-mini")

# Local backend (replace with your provider + model)
gpt("ping", provider = "ollama", model = "mistral:latest")
```

If you see a response, you're ready for structured extraction with `gpt_column()`.

## Installation

``` r
# Requires pak or remotes
pak::pak("FrancescoMonti-source/gptr")

# alternatively
remotes::install_github("FrancescoMonti-source/gptr")
```

Set an API key (e.g., `OPENAI_API_KEY`) or run a local OpenAI-compatible server before calling a model. With `provider = "auto"`, `gptr` prefers a detected local backend and falls back to cloud APIs when credentials are available.

### Configure credentials and defaults

Store credentials in `.Renviron` or your session so they do not get hard-coded:

```{r, eval = FALSE}
# ~/.Renviron (restart R after editing)
OPENAI_API_KEY="your-key-here"
```

You can also set defaults for a session:

```{r, eval = FALSE}
options(
  gptr.provider = "auto",
  gptr.openai_model = "gpt-4o-mini"
)
```

### Choose your backend

`gptr` can work with local or hosted providers. Pick the one that matches your setup:

| Provider | Best for | Notes |
| --- | --- | --- |
| `auto` | First-time users | Prefers local if running, otherwise uses OpenAI when credentials exist. |
| `openai` | Hosted models | Requires `OPENAI_API_KEY`. |
| `lmstudio` / `ollama` / `localai` | Local models | Run the server and pass the model name available on that backend. |
| `local` | Custom OpenAI-compatible server | Provide `base_url` or `backend` to pin. |

## Quick start

```{r, eval = FALSE}
library(gptr)
library(tibble)

notes <- tibble(
  id = 1:2,
  text = c(
    "Patient is 64 years old; dx: type 2 diabetes.",
    "72 y/o male diagnosed with hypertension."
  )
)

schema <- list(
  age = "integer",
  diagnosis = "character"
)

prompt <- "
You are an assistant specialisez in treating electronical health records. I'm gonna provide you a medical text and you task is to
extract the following fields as compact JSON on a single line:
{json_format}

Here is the text:
\"{text}\"

Respond with JSON only, nothing else."

res <- gpt_column(
  data     = notes,
  col      = text,
  keys     = schema,
  prompt   = prompt,
  provider = "ollama", # openai/local/ollama/lmstudio
  model    = "gemma3-4b" # chose among models you have at your disposal. Check list_models() documentation.
)

res
#> # A tibble: 2 x 4
#>      id text                                              age diagnosis    .invalid_rows
#>   <int> <chr>                                           <int> <chr>        <log>
#> 1     1 Patient is 64 years old; dx: type 2 diabetes.      64 diabetes     FALSE
#> 2     2 72 y/o male diagnosed with hypertension.           72 hypertension FALSE

```

`gpt_column()` adds the extracted variables, retains the original text, and (with `return_debug = TRUE`) appends `.raw_output` and `.invalid_detail` for auditing. The `".invalid_rows"` flags rows that failed validation so that you can retry them selectively.

## Common first-run issues

-   **"No backend available"**: start a local server (LM Studio/Ollama/LocalAI) or set `OPENAI_API_KEY`. Try `gpt("ping", provider = "openai", model = "gpt-4o-mini")` or `gpt("ping", provider = "ollama", model = "mistral:latest")` (swap in your provider/model) to confirm.
-   **"Model not found"**: run `list_models(provider = "auto")` to see what's available and pass a model name explicitly.
-   **Invalid JSON**: keep `return_debug = TRUE` and inspect `.raw_output` or `.invalid_detail` to tune your prompt or schema.

## A practical extraction checklist

1.  **Start with a minimal schema** (two or three fields) and a short prompt.
2.  **Run 5–10 real samples** to see where the model drifts.
3.  **Tighten the schema** (types or enumerations) and re-run.
4.  **Inspect invalid rows** with `return_debug = TRUE` and `patch_failed_rows()`.
5.  **Scale up** with progress backends or parallel plans once the prompt stabilizes.

## Workflow overview

```         
[gpt_column()] -- templated prompt --> [gpt()] -- tidy columns --> data + predictions
      |                                     |
      | text + keys' schema                 | raw JSON
      v                                     v
 build_prompt()                       tidy_json() + validation
 inject {text}/{json_format}          repair, coerce, align
```

For each row, `gpt_column()` trims the text (if asked to do so), renders the prompt, calls the model, repairs the response, validates it against the schema, and binds the structured values back onto the original tibble. All intermediate artefacts (raw output, validation detail, invalid row indexes) stay attached for debugging.

## Schema keys

`keys` is a named list that documents the shape of the JSON you expect back. Each entry can be:

-   A "type" string (`"integer"`, `"numeric"`, `"character"`, `"logical"`). Types declaration drive column coercion and inject type-based examples via glue() through `{json_format}`.
-   A vector of allowed values (regardless of the "type"). The parser accepts the listed values case-insensitively for strings.
-   A mix of the two: you can use types for some fields and enumerations for others. 

The schema is the single source of truth used for prompting, parsing, and validation. 

If you want to supply your own examples instead of the auto-generated hint, omit that key from `keys` and document it directly inside the prompt template.

## Prompt building and schema injection

`gpt_column()` renders a fresh prompt for every row. Under the hood it:

1.  Trims the row text and passes it, together with `keys`, to `build_prompt()`.
2.  Replaces `{text}` in your template with the row text.
3.  Computes `{json_format}` from `keys` so the model sees a compact schema reminder.
4.  Omits `{json_format}` entirely when `keys` is `NULL`.

Placeholders are filled with [`glue`](https://glue.tidyverse.org/). Glue evaluates anything inside braces, so escape literal braces with `{{` and `}}` or wrap code in `glue::glue_safe()` if needed.

```{r, eval = FALSE}
build_prompt(
  template,
  text = "Patient is 64 years old; dx: type 2 diabetes.",
  keys = schema
)
```

For full control, pass a function to `prompt`. It receives `(text, keys)` and should return a string:

```{r, eval = FALSE}
dynamic_prompt <- function(text, keys) {
  glue::glue("
  You are triaging clinical notes. Focus on age and diagnosis.
  {if (nchar(text) > 280) 'Summarise briefly before extracting.'}

  Text:
  \"{text}\"

  Return JSON using this schema:
  {jsonlite::toJSON(names(keys), auto_unbox = TRUE)}
  ")
}

gpt_column(data = notes, col = text, keys = schema, prompt = dynamic_prompt)
```

## Response validation pipeline

After the model replies, `gpt_column()` runs a deterministic pipeline:

1.  **Repair and parse** with `tidy_json()` so even slightly malformed JSON is recovered.
2.  **Align keys** using `json_keys_align()`, optional fuzzy matching (`auto_correct_keys`), and `keep_unexpected_keys`.
3.  **Validate values** by coercing to declared types and checking membership in allowed sets.
4.  **Shape the tibble** so every row has exactly the schema columns (missing values become `NA`).

When `keys` is `NULL`, you can still collect structured output: set `keep_unexpected_keys = TRUE` to keep a minified `.parsed_json` column, or use `relaxed = TRUE` to accept non-JSON responses.

## Debugging, retries, and auditing

-   `return_debug = TRUE` (default behavior) appends `.raw_output` and `.invalid_detail` columns to inspect model responses and validation errors.
-   `attr(result, "invalid_rows")` gives the row indexes that need attention.
-   `keep_unexpected_keys = TRUE` stores extra keys in `.extras_json` when you work with a fixed schema.
-   `patch_failed_rows()` retries only the invalid rows with the same or updated prompt.

```{r, eval = FALSE}
failed <- attr(res, "invalid_rows")

if (length(failed)) {
  res2 <- patch_failed_rows(
    data   = res,
    col    = text,
    id_col = id,
    prompt = template,
    keys   = schema,
    max_attempts = 2
  )
}
```

## Multiple ways to use gptr

`gptr` is not just `gpt_column()`. You can interact with models in a few different ways depending on your workflow:

-   **`gpt()` for single calls**: quick prompts or helper utilities.
-   **`gpt_chat()` for conversations**: keep and reset history between turns.
-   **`gpt_tts()` for text-to-speech**: generate audio and play it locally.

```{r, eval = FALSE}
gpt("Summarise this text in one sentence.")

gpt_chat("Give me three bullet points.")
gpt_chat("Now rewrite them as headlines.")
gpt_chat$reset()

audio <- gpt_tts("Hello from gptr!")
play(audio)
```

## Quality-of-life helpers

Use these helpers to inspect configuration, explore models, and manage caches:

```{r, eval = FALSE}
show_gptr_options()
options(gptr.provider = "ollama", gptr.ollama_model = "gemma3-4b")

list_models(provider = "auto")
refresh_models()
delete_models_cache()
```

## Other helpers

-   **Single call vs chat:** `gpt()` is stateless and perfect for small utilities; `gpt_chat()` maintains conversational history (reset with `gpt_chat$reset()`).
-   **Text-to-speech:** `gpt_tts("Hello from R!")` sends text to OpenAI TTS, saves an audio file, and returns a `gptr_audio` object (use `play(audio)` to open it).
-   **Model discovery:** `list_models(provider = "auto")` probes the selected backend, caching results; `refresh_models()` bypasses the cache. Use `delete_models_cache()` to clear entries.
-   **Options introspection:** `show_gptr_options()` prints every `gptr.*` option currently in effect so you can see provider defaults, timeouts, and cache behaviour.

## Advanced tuning

-   **Fuzzy key repair:** enable `auto_correct_keys = TRUE` (default) so `json_keys_align()` can fix typos when there is a unique close match. Control the algorithm with `fuzzy_model = "lev_ratio"` or `"lev"` and the tolerance via `fuzzy_threshold`.
-   **Type coercion:** leave `.coerce_types = TRUE` for schema-driven casts, or pass `coerce_when = list(field = "integer")` to override individual columns. Provide `na_values` to recognise additional sentinel strings.
-   **Relaxed parsing:** set `relaxed = TRUE` when `keys = NULL` and you want to accept scalar strings or partially structured replies. Combine with `keep_unexpected_keys = TRUE` to persist a `.parsed_json` column.
-   **Type inference:** when you omit `keys`, `infer_types = TRUE` attempts to guess column types from the parsed JSON.
-   **File/image inputs:** supply `file_path` or `image_path` and they are forwarded to `gpt()`; useful for multimodal providers that accept binary attachments.
-   **Progress backends:** `progress = TRUE` uses CLI spinners by default. Set `options(gptr.progress.backend = "progressr")` and load `progressr` for rich progress bars inside parallel plans or long runs.

All knobs and defaults can be inspected with `show_gptr_options()`, then overridden via `options(gptr.* = value)` in your session or `.Rprofile`.

## Provider support

`gptr` works with both hosted and local models via a consistent interface:

-   `provider = "auto"`: prefer a running local backend (LM Studio, Ollama, LocalAI). If none is detected and `OPENAI_API_KEY` is set, fall back to OpenAI.
-   `provider = "openai"`: call OpenAI's REST API directly. Set `model` (e.g., `"gpt-4o-mini"`).
-   `provider = "local"`: use an OpenAI-compatible server you control. Pin a backend with `backend = "lmstudio"`, `"ollama"`, or `"localai"`, or supply a custom `base_url`.
-   Shorthands `lmstudio`, `ollama`, and `localai` map to `provider = "local"` with the respective backend already set.

```{r, eval = FALSE}
gpt("ping", provider = "auto")
gpt("ping", provider = "lmstudio", model = "mistralai/mistral-7b-instruct-v0.3")
gpt("ping", provider = "openai", model = "gpt-4o-mini")
```

## FAQ

**Do I need to provide a schema?**  
No. If `keys = NULL`, you can still collect structured output with `keep_unexpected_keys = TRUE` (or `relaxed = TRUE` when you want to accept non-JSON replies). The trade-off is weaker validation. 

**How do I pick a model?**  
Start with `list_models(provider = "auto")` to see what your backend exposes, then set `model` explicitly to make runs reproducible.

**Where do I see the package defaults?**  
Run `show_gptr_options()` to print the full list of `gptr.*` options active in your session.

## Requirements

-   R \>= 4.1
-   Imports: `cli`, `httr`, `httr2`, `jsonlite`, `purrr`, `stringr`, `tibble`
-   Suggested: `furrr`, `progressr`, `pdftools`, `officer`, `mime`

## Contributing

Issues and pull requests are welcome. Please open an issue to discuss substantial changes before submitting a PR.

## License

MIT License - see `LICENSE.txt`.
