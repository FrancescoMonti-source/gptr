# gptr 0.2.1

* Unified backend resolution. New internal .resolve_provider_backend() centralizes all “auto” logic (provider/backend/base_url). gpt() now calls this, so callers don’t need to guess.
* Shorthand providers. provider = "lmstudio"|"ollama"|"localai" now normalize to provider="local" + backend=... consistently.
* Predictable auto. provider="auto" chooses local when available; OpenAI when not (or when the model clearly targets OpenAI and no local is preferred).
* Explicit base URLs. For OpenAI, base_url must be the full endpoint (.../v1/chat/completions). Documented and tested.
* Safer HTTP config. Timeouts/retries handled at the request layer (no JSON payload leakage). Optional IPv4 forcing; clearer error surfacing in debug.
* gpt_column() pass-through fixed. Now correctly forwards provider, backend, model, base_url, and other ... to gpt().
* Parsing UX. Clearer outcomes when the model returns non-JSON; schema mode (keys) vs raw/relaxed modes behave as expected.
* Key alignment & coercion. Fuzzy key alignment and per-row type coercion kept; extras captured via .extras_json when requested.
* Tests. Added coverage for provider/model combos and explicit base_url behavior; docs/comments clarify expected routing.

# gptr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added print_raw argument to gpt() to return a compact raw skeleton.
* Introduced automatic provider detection ("auto") and explicit provider choices ("lmstudio", "ollama", "localai", etc.).
* Added caching to speed up repeated backend detection.
* Improved schema handling and error reporting in gpt_column().
* Added new tests and integration checks for LM Studio/Ollama backends.
