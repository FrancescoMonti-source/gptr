# gptr 0.4.9

* Renamed the structured extraction vocabulary to be more explicit: debug output now reports `.schema_mode` with `backend_schema` / `prompt_schema`, while `structured = "native"` / `"repair"` remain accepted as legacy aliases.
* Added `gptr.backend_schema_backends` as the preferred advanced opt-in option for local backends that truly support `response_format` / JSON schema, while keeping `gptr.native_structured_backends` as a compatibility alias.

# gptr 0.4.8

* Recentered the package around schema-based extraction, with `gpt_column()` now supporting an instruction-first prompt UX alongside the legacy raw `prompt` path.
* Structured extraction is now route-first: `gpt()` and `gpt_column()` share route resolution, native-vs-repair mode follows the chosen route, and hidden cross-provider model searching was removed from execution paths.
* Simplified ownership hotspots by pruning dead model discovery internals, deduplicating type coercion logic, and shrinking the `models_cache` test suite into a smaller public-first set.
* Refreshed docs and tests around the new extraction core, including synthetic real-world fixtures, prompt UX coverage, and clearer positioning of chat/TTS/model utilities as supporting features.

# gptr 0.4.4

* Overhauled model caching and fetching: consolidated helpers under cachem, `.fetch_models_live()` now handles OpenAI and local providers directly, `list_models()` caches by default with a new `refresh_models()` to force a live probe, and redundant base URL normalization was removed.
* Renamed internal helpers for clarity and streamlined provider lookup.
* `request_local()` is now internal.
* Dropped remote withr dependency and replaced typographic quotes.
* Added documentation for `assemble_models_df()` and expanded test coverage around model cache helpers.

# gptr 0.4.3
Removed legacy backend helpers and detection functions.

gpt():

Simplified provider/backend normalization.

Local backends now always use root host URLs (http://127.0.0.1:1234) rather than full paths.

Discovery and preflight handled through list_models() / refresh_models().

Local calls unified on .request_local(), which appends /v1/chat/completions.

Removed duplicate/obsolete code paths.

Cache: only models_cache.R remains the source of truth for probing and caching model IDs.

OpenAI path: untouched, continues to use .resolve_openai_defaults() and openai_send_request().

Docs/tests: update examples and tests to use list_models() instead of removed helpers.

# gptr 0.4.2

* Added a nice progress bar to gpt_column() showing time estimate, elapsed
, etc

# gptr 0.4.1

* gpt_chat$summarise() added:
    Summarises the current conversation history via gpt().
    By default replaces the history with a single user message prefixed "Chat summary :".
    With replace = FALSE, appends the summary as an assistant message while keeping full history.
* Dropped the old gpt_chat_summarise() helper (now redundant).
* Documentation updated to include summarise() in the method list.
* Cleaner API: all chat lifecycle operations (show_history, reset, save, load, replace_history, summarise) now live under gpt_chat$….
* Less duplicated code: summarisation leverages the main gpt() call path, ensuring provider/URL resolution and options work consistently.
* Tests updated → all passing ✅

# gptr 0.2.1

* Unified backend resolution centralizes all “auto” logic (provider/backend/base_url). gpt() now handles this centrally, so callers don’t need to guess.
* Shorthand providers. provider = "lmstudio"|"ollama"|"localai" now normalize to provider="local" + backend=... consistently.
* Predictable auto. provider="auto" chooses local when available; OpenAI when not (or when the model clearly targets OpenAI and no local is preferred).
* Explicit base URLs. For OpenAI, base_url must be the full endpoint (.../v1/chat/completions). Documented and tested.
* Safer HTTP config. Timeouts/retries handled at the request layer (no JSON payload leakage). Optional IPv4 forcing; clearer error surfacing in debug.
* gpt_column() pass-through fixed. Now correctly forwards provider, backend, model, base_url, and other ... to gpt().
* Parsing UX. Clearer outcomes when the model returns non-JSON; schema mode (keys) vs raw/relaxed modes behave as expected.
* Key alignment & coercion. Fuzzy key alignment and per-row type coercion kept; extras captured via .extras_json when requested.
* Tests. Added coverage for provider/model combos and explicit base_url behavior; docs/comments clarify expected routing.

## Migration notes

* Prefer provider="openai" or provider="lmstudio" explicitly when you want to pin a side.
* If you override OpenAI base_url, use the full path: https://api.openai.com/v1/chat/completions.
* In gpt_column(), set keys + {json_format} in the prompt when you expect strict JSON; otherwise use raw/relaxed mode.


# gptr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added print_raw argument to gpt() to return a compact raw skeleton.
* Introduced automatic provider detection ("auto") and explicit provider choices ("lmstudio", "ollama", "localai", etc.).
* Added caching to speed up repeated backend detection.
* Improved schema handling and error reporting in gpt_column().
* Added new tests and integration checks for LM Studio/Ollama backends.
