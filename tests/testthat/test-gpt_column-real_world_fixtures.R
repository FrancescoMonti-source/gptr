load_real_world_fixture_tasks <- function() {
  env <- new.env(parent = baseenv())
  sys.source(testthat::test_path("fixtures", "real_world_extraction_cases.R"), envir = env)
  env$gptr_real_world_fixture_tasks
}

fixture_task <- function(task_id) {
  tasks <- load_real_world_fixture_tasks()
  idx <- vapply(tasks, function(task) identical(task$task_id, task_id), logical(1))
  tasks[[which(idx)[1L]]]
}

fixture_case_json <- function(case) {
  jsonlite::toJSON(case$expected, auto_unbox = TRUE, null = "null", na = "null")
}

run_fixture_case <- function(task,
                             case,
                             provider = "local",
                             structured = NULL,
                             response_json = fixture_case_json(case),
                             keys = task$keys) {
  seen_response_format <- "__unset__"
  data <- data.frame(text = case$text, stringsAsFactors = FALSE)

  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, ...) {
      seen_response_format <<- response_format
      response_json
    },
    .package = "gptr"
  )

  if (!is.null(structured)) {
    out <- rlang::inject(
      gpt_column(
        data = data,
        col = !!rlang::sym("text"),
        prompt = task$prompt,
        keys = keys,
        provider = provider,
        structured = structured,
        progress = FALSE,
        return_debug = TRUE
      )
    )
  } else {
    out <- rlang::inject(
      gpt_column(
        data = data,
        col = !!rlang::sym("text"),
        prompt = task$prompt,
        keys = keys,
        provider = provider,
        progress = FALSE,
        return_debug = TRUE
      )
    )
  }
  list(out = out, response_format = seen_response_format)
}

expect_case_matches <- function(out, case) {
  expected <- case$expected
  for (nm in names(expected)) {
    want <- expected[[nm]]
    got <- out[[nm]][[1L]]
    if (length(want) == 1L && is.na(want)) {
      testthat::expect_true(is.na(got), info = nm)
    } else {
      testthat::expect_identical(got, want, info = nm)
    }
  }
}

normalize_fixture_drug_type <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }

  tokens <- unlist(strsplit(tolower(trimws(as.character(x))), "\\s*;\\s*"))
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(NA_character_)
  }

  normalize_one <- function(token) {
    dplyr::case_when(
      token %in% c("cannabis", "thc", "marijuana", "hash", "hashish") ~ "cannabis",
      token %in% c("cocaine", "cocaine", "cocaïne") ~ "cocaine",
      token %in% c("heroine", "héroïne", "opioide", "opioïde", "opioides", "opioïdes", "opiace", "opiacé", "opiaces", "opiacés") ~ "opioides",
      token %in% c("amphetamine", "amphétamine", "amphetamines", "amphétamines", "methamphetamine", "méthamphétamine") ~ "amphetamines",
      token %in% c("mdma", "ecstasy", "ecstasie") ~ "mdma",
      token %in% c("hallucinogene", "hallucinogène", "hallucinogenes", "hallucinogènes", "lsd") ~ "hallucinogenes",
      token %in% c("ketamine", "kétamine") ~ "ketamine",
      token %in% c("autre", "unknown", "inconnu") ~ "autre",
      TRUE ~ NA_character_
    )
  }

  canonical <- unique(stats::na.omit(vapply(tokens, normalize_one, character(1))))
  if (!length(canonical)) {
    return(NA_character_)
  }

  canonical <- canonical[order(match(
    canonical,
    c("cannabis", "cocaine", "opioides", "amphetamines", "mdma", "hallucinogenes", "ketamine", "autre")
  ))]
  paste(canonical, collapse = "; ")
}

test_that("legacy-style gpt_column calls still work on redacted real-world fixtures", {
  tasks <- load_real_world_fixture_tasks()

  for (task in tasks) {
    for (case in task$rows) {
      res <- run_fixture_case(task, case, provider = "local")
      expect_case_matches(res$out, case)
      testthat::expect_false(res$out$.invalid_rows[[1]], info = paste(task$task_id, case$case_id))
      testthat::expect_identical(res$out$.schema_mode[[1]], "prompt_schema", info = paste(task$task_id, case$case_id))
      testthat::expect_null(res$response_format, info = paste(task$task_id, case$case_id))
    }
  }
})

test_that("real-world enum schemas reject out-of-schema values", {
  task <- fixture_task("d0840_dialysis_before_transplant")
  case <- task$rows[[1L]]

  invalid_json <- jsonlite::toJSON(
    list(
      receveur_dialyse_text = 1L,
      receveur_type_dialyse_text = "dialyse chronique",
      receveur_dialyse_text_resume = "Mention de dialyse avant la greffe"
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )

  res <- run_fixture_case(task, case, provider = "local", response_json = invalid_json)
  detail <- res$out$.invalid_detail[[1L]]
  bad_row <- detail[detail$key == "receveur_type_dialyse_text", , drop = FALSE]

  testthat::expect_identical(res$out$receveur_dialyse_text[[1L]], 1L)
  testthat::expect_true(is.na(res$out$receveur_type_dialyse_text[[1L]]))
  testthat::expect_true(res$out$.invalid_rows[[1L]])
  testthat::expect_identical(nrow(bad_row), 1L)
  testthat::expect_false(bad_row$allowed[[1L]])
  testthat::expect_identical(bad_row$action[[1L]], "coerced_to_na")
})

test_that("ambiguous real-world fixture cases can return nulls without invalidating the row", {
  dialysis_task <- fixture_task("d0840_dialysis_before_transplant")
  dialysis_case <- dialysis_task$rows[[3L]]
  dialysis_res <- run_fixture_case(dialysis_task, dialysis_case, provider = "local")

  expect_case_matches(dialysis_res$out, dialysis_case)
  testthat::expect_false(dialysis_res$out$.invalid_rows[[1L]])

  nephro_task <- fixture_task("d0840_nephropathie_initiale")
  nephro_case <- nephro_task$rows[[3L]]
  nephro_res <- run_fixture_case(nephro_task, nephro_case, provider = "local")

  expect_case_matches(nephro_res$out, nephro_case)
  testthat::expect_false(nephro_res$out$.invalid_rows[[1L]])
})

test_that("lifestyle fixture supports downstream drug-type normalization", {
  task <- fixture_task("dmo_lifestyle")
  case <- task$rows[[1L]]
  keys <- task$keys
  keys$drogue_type <- "character"

  response_json <- jsonlite::toJSON(
    list(
      tabac_statut = "actif",
      tabac_evidence_excerpt = "fume 10 cigarettes par jour",
      alcool_statut = "actif",
      alcool_evidence_excerpt = "consommation d'alcool encore active",
      drogue_statut = "actif",
      drogue_type = "cocaïne ; cannabis",
      drogue_evidence_excerpt = "usage recent de cocaïne et cannabis"
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )

  res <- run_fixture_case(task, case, provider = "local", response_json = response_json, keys = keys)

  testthat::expect_identical(res$out$tabac_statut[[1L]], "actif")
  testthat::expect_identical(res$out$alcool_statut[[1L]], "actif")
  testthat::expect_identical(res$out$drogue_statut[[1L]], "actif")
  testthat::expect_identical(normalize_fixture_drug_type(res$out$drogue_type[[1L]]), "cannabis; cocaine")
  testthat::expect_false(res$out$.invalid_rows[[1L]])
})

test_that("real-world fixtures honor structured mode selection", {
  task <- fixture_task("d0840_dialysis_before_transplant")
  case <- task$rows[[1L]]

  backend_schema_res <- run_fixture_case(task, case, provider = "openai", structured = "auto")
  testthat::expect_true(is.list(backend_schema_res$response_format))
  testthat::expect_identical(backend_schema_res$response_format$type, "json_schema")
  testthat::expect_identical(backend_schema_res$out$.schema_mode[[1L]], "backend_schema")
  expect_case_matches(backend_schema_res$out, case)

  prompt_schema_res <- run_fixture_case(task, case, provider = "ollama", structured = "prompt_schema")
  testthat::expect_null(prompt_schema_res$response_format)
  testthat::expect_identical(prompt_schema_res$out$.schema_mode[[1L]], "prompt_schema")
  expect_case_matches(prompt_schema_res$out, case)

  testthat::expect_error(
    run_fixture_case(task, case, provider = "local", structured = "backend_schema"),
    "Backend schema extraction is not available"
  )
})
