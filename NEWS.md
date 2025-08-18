# gptr 0.2.0

* Added a `NEWS.md` file to track changes to the package.

* Added print_raw argument to gpt() to return a compact raw skeleton.

* Introduced automatic provider detection ("auto") and explicit provider choices ("lmstudio", "ollama", "localai", etc.).

* Added caching to speed up repeated backend detection.

* Improved schema handling and error reporting in gpt_column().

* Added new tests and integration checks for LM Studio/Ollama backends.
