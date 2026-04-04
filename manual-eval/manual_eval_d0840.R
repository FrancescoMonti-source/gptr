# Local-only manual evaluation for D0840-derived extraction tasks.
#
# This script is intentionally self-contained so the manual benchmark stays
# stable even if the D0840 project changes later.
#
# Create the 1000-row pools manually once from D0840, then keep reusing them:
#
# Tabac pool example from D0840:
# saveRDS(
#   dplyr::select(
#     tabac_context,
#     PATID,
#     DATEACTE,
#     role,
#     n_docs_tabac,
#     ELTID_tabac_contexte,
#     text_tabac_llm
#   ) |>
#     dplyr::filter(!is.na(text_tabac_llm), nzchar(trimws(text_tabac_llm))) |>
#     dplyr::slice_sample(n = min(1000L, dplyr::n()), replace = FALSE),
#   file.path("path", "to", "gptr", "manual-eval", "tabac_eval_pool_1000.rds")
# )
#
# Surgical antecedents pool example from D0840:
# saveRDS(
#   dplyr::select(atcd_context, PATID, DATEACTE, role, text_atcd) |>
#     dplyr::filter(!is.na(text_atcd), nzchar(trimws(text_atcd))) |>
#     dplyr::slice_sample(n = min(1000L, dplyr::n()), replace = FALSE),
#   file.path("path", "to", "gptr", "manual-eval", "atcd_chir_eval_pool_1000.rds")
# )

task <- "tabac"
sample_n <- 25L
seed <- 42L
provider <- "ollama"
model <- "gemma3:4b"
save_run <- FALSE

.find_gptr_repo_root <- function() {
  source_file <- NULL
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    candidate <- frames[[i]]$ofile
    if (!is.null(candidate) && nzchar(candidate)) {
      source_file <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
      break
    }
  }

  candidate_roots <- unique(c(
    if (!is.null(source_file)) normalizePath(file.path(dirname(source_file), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(".", winslash = "/", mustWork = FALSE),
    normalizePath("..", winslash = "/", mustWork = FALSE)
  ))

  for (root in candidate_roots) {
    if (file.exists(file.path(root, "DESCRIPTION")) && file.exists(file.path(root, "R"))) {
      return(root)
    }
  }

  stop(
    "Could not locate the gptr repo root. Source this script from inside the gptr checkout.",
    call. = FALSE
  )
}

.repo_root <- .find_gptr_repo_root()
.manual_eval_dir <- file.path(.repo_root, "manual-eval")

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.repo_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(gptr)
}

.manual_eval_configs <- list(
  tabac = list(
    pool_path = file.path(.manual_eval_dir, "tabac_eval_pool_1000.rds"),
    text_col = "text_tabac_llm",
    required_cols = c("text_tabac_llm", "ELTID_tabac_contexte"),
    keys = list(
      tabac_actif = "integer",
      tabac_sevre = "integer",
      tabac_statut = "character",
      tabac_resume = "character"
    ),
    instruction = "Tu es un assistant d'extraction clinique.
Tu recois uniquement des extraits de dossier potentiellement pertinents pour le tabagisme autour d'une chirurgie.
Ta tache est de determiner le statut tabagique du patient au moment de la chirurgie.

Regles de decision:
- Base-toi uniquement sur les extraits fournis
- Cherche le statut du patient a la date de chirurgie, pas a une autre periode
- Ignore les mentions concernant la famille, les parents ou l'entourage
- Si le texte dit non-fumeur, jamais fume, absence de tabagisme: classe le patient comme non_fumeur
- Si le texte dit ex-fumeur, ancien fumeur, sevre, arret du tabac avant la chirurgie: classe le patient comme sevre
- Si le texte dit tabagisme actif, fumeur, non sevre, ou une consommation actuelle: classe le patient comme actif
- Si les extraits sont contradictoires ou insuffisants: classe le patient comme indetermine

Exemples de decision:
- 'Tabagisme actif : oui, 20 PA' => actif
- 'Ex-fumeur, sevre en 2010' => sevre
- 'Non-fumeur' => non_fumeur
- 'Pere fumeur' => ne concerne pas le patient"
  ),
  atcd_chir = list(
    pool_path = file.path(.manual_eval_dir, "atcd_chir_eval_pool_1000.rds"),
    text_col = "text_atcd",
    required_cols = c("text_atcd"),
    keys = list(
      atcd_chir_abdo_pelvienne = "integer",
      atcd_chir_abdo_pelvienne_type = "character",
      atcd_chir_voies_urinaires = "integer",
      atcd_chir_voies_urinaires_type = "character",
      atcd_chir_vasc = "integer",
      atcd_chir_vasc_type = "character"
    ),
    instruction = "Tu es un assistant d'extraction clinique.
Ta tâche est d'extraire uniquement les antécédents chirurgicaux anciens explicitement mentionnés dans le texte.
Il faut identifier de vraies chirurgies antérieures du patient, pas l'intervention actuelle ni les gestes mineurs.

Catégories à renseigner:
- atcd_chir_abdo_pelvienne: chirurgie abdominale ou pelvienne hors voies urinaires et hors chirurgie vasculaire
- atcd_chir_voies_urinaires: chirurgie urologique vraie du rein, des uretères, de la vessie, de l'urètre ou de la prostate
- atcd_chir_vasc: chirurgie vasculaire vraie artérielle ou veineuse

Exemples à compter:
- abdo/pelvienne: appendicectomie, cholécystectomie, césarienne, hystérectomie, ovariectomie, colectomie
- voies urinaires: néphrectomie, pyéloplastie, réimplantation urétérale, cure de reflux, cystectomie, prostatectomie, greffe rénale antérieure, transplantectomie
- vasculaire: chirurgie aortique, pontage, endartériectomie, thrombectomie, chirurgie iliaque, cave ou carotidienne

Ne pas compter:
- la transplantation rénale actuelle, la greffe actuelle, l'intervention du séjour actuel
- une mention isolée de 'greffe rénale' ou 'transplantation rénale' si le texte ne dit pas explicitement qu'il s'agit d'une chirurgie antérieure
- les fistules artério-veineuses de dialyse, leur création, révision, fermeture ou ligature
- les cathéters de dialyse et autres abords vasculaires de dialyse
- la pose ou l'ablation simple de sonde JJ, les néphrostomies, les cystoscopies, les urétéroscopies, les injections endoscopiques, les gestes purement endoscopiques
- les biopsies, ponctions, examens, imagerie, consultations

Règles de décision:
- Utilise uniquement les informations explicitement présentes dans le texte
- N'invente rien et n'interprète pas au-delà du texte
- Si le texte est ambigu ou insuffisant, retourne 0 et NA pour la catégorie concernée
- Si une chirurgie est mentionnée plusieurs fois, ne la compte qu'une fois
- Pour chaque champ *_type, retourne un libellé court, normalisé et clinique
- Si plusieurs chirurgies existent dans une même catégorie, sépare-les par ' | '

Exemple 1:
Texte: 'ATCD: appendicectomie, 2e greffe rénale, fistule artério-veineuse de dialyse'
Réponse:
{{
  \"atcd_chir_abdo_pelvienne\": 1,
  \"atcd_chir_abdo_pelvienne_type\": \"appendicectomie\",
  \"atcd_chir_voies_urinaires\": 1,
  \"atcd_chir_voies_urinaires_type\": \"greffe rénale antérieure\",
  \"atcd_chir_vasc\": 0,
  \"atcd_chir_vasc_type\": null
}}

Exemple 2:
Texte: 'Transplantation rénale ce jour avec pose de sonde JJ'
Réponse:
{{
  \"atcd_chir_abdo_pelvienne\": 0,
  \"atcd_chir_abdo_pelvienne_type\": null,
  \"atcd_chir_voies_urinaires\": 0,
  \"atcd_chir_voies_urinaires_type\": null,
  \"atcd_chir_vasc\": 0,
  \"atcd_chir_vasc_type\": null
}}"
  )
)

.stop_missing_cols <- function(data, cols, label) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "The %s manual-eval pool is missing required columns: %s",
        label,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

.read_manual_eval_pool <- function(config, label) {
  if (!file.exists(config$pool_path)) {
    stop(
      sprintf(
        "Missing manual-eval artifact: %s",
        normalizePath(config$pool_path, winslash = "/", mustWork = FALSE)
      ),
      call. = FALSE
    )
  }
  pool <- readRDS(config$pool_path)
  if (!is.data.frame(pool)) {
    stop(sprintf("The %s manual-eval artifact must contain a data frame.", label), call. = FALSE)
  }
  .stop_missing_cols(pool, config$required_cols, label)

  text_values <- trimws(as.character(pool[[config$text_col]]))
  valid <- !is.na(text_values) & nzchar(text_values)
  pool <- pool[valid, , drop = FALSE]
  rownames(pool) <- NULL
  if (nrow(pool) == 0L) {
    stop(sprintf("The %s manual-eval artifact has no non-empty rows.", label), call. = FALSE)
  }
  pool
}

.sample_manual_eval_pool <- function(pool, n, seed = NULL) {
  n <- as.integer(n)[1]
  if (is.na(n) || n < 1L) {
    stop("`sample_n` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(as.integer(seed)[1])
  }
  size <- min(n, nrow(pool))
  idx <- if (size == nrow(pool)) seq_len(nrow(pool)) else sample.int(nrow(pool), size = size)
  rownames(pool) <- NULL
  pool[idx, , drop = FALSE]
}

.sanitize_file_part <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "-", x)
  x <- gsub("-{2,}", "-", x)
  x <- sub("^-+", "", x)
  x <- sub("-+$", "", x)
  if (!nzchar(x)) "run" else x
}

task <- match.arg(task, choices = names(.manual_eval_configs))
config <- .manual_eval_configs[[task]]

manual_eval_pool <- .read_manual_eval_pool(config, task)
manual_eval_input <- .sample_manual_eval_pool(manual_eval_pool, n = sample_n, seed = seed)
manual_eval_call_input <- manual_eval_input
manual_eval_call_input$.manual_eval_text <- manual_eval_call_input[[config$text_col]]

manual_eval_result <- gpt_column(
  manual_eval_call_input,
  col = .manual_eval_text,
  keys = config$keys,
  instruction = config$instruction,
  provider = provider,
  model = model
)

if (".manual_eval_text" %in% names(manual_eval_result)) {
  manual_eval_result$.manual_eval_text <- NULL
}

manual_eval_run <- list(
  task = task,
  provider = provider,
  model = model,
  sample_n = nrow(manual_eval_input),
  seed = seed,
  created_at = as.character(Sys.time()),
  input = manual_eval_input,
  result = manual_eval_result
)

if (isTRUE(save_run)) {
  dir.create(file.path(.manual_eval_dir, "runs"), recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  run_path <- file.path(
    .manual_eval_dir,
    "runs",
    sprintf(
      "%s__%s__%s__%s.rds",
      .sanitize_file_part(task),
      .sanitize_file_part(provider),
      .sanitize_file_part(model),
      stamp
    )
  )
  saveRDS(manual_eval_run, run_path)
  message("Saved manual-eval run: ", normalizePath(run_path, winslash = "/", mustWork = FALSE))
}

message(
  sprintf(
    "Manual eval ready: task=%s, rows=%s, provider=%s, model=%s",
    task,
    nrow(manual_eval_input),
    provider,
    model
  )
)

invisible(manual_eval_run)
