gptr_real_world_fixture_tasks <- list(
  list(
    task_id = "d0840_dialysis_before_transplant",
    prompt = paste(
      "Tu es un assistant d'extraction clinique sur la transplantation renale.",
      "Tu recois uniquement des extraits autour de la chirurgie du receveur.",
      "",
      "Tu dois determiner si le receveur etait sous dialyse avant la greffe actuelle, et si oui de quel type.",
      "",
      "Tu dois extraire 3 informations:",
      "- receveur_dialyse_text",
      "- receveur_type_dialyse_text",
      "- receveur_dialyse_text_resume",
      "",
      "Definitions:",
      "- receveur_dialyse_text = 1 si les extraits disent clairement que le patient etait sous dialyse avant la greffe actuelle",
      "- receveur_dialyse_text = 0 si les extraits disent clairement que la greffe actuelle est pre-emptive / avant dialyse / patient non dialyse",
      "- receveur_dialyse_text = null si les extraits ne permettent pas de trancher",
      "- receveur_type_dialyse_text = une seule valeur parmi 'hemodialyse', 'dialyse peritoneale', 'mixte', null",
      "- receveur_dialyse_text_resume = resume clinique tres court de la preuve retenue",
      "",
      "Regles de decision:",
      "- Base-toi uniquement sur les extraits fournis",
      "- Concentre-toi sur la periode juste avant la greffe actuelle",
      "- Ignore les simples en-tetes de service ou de consultation s'il n'y a pas de preuve clinique que le patient est dialyse",
      "- Ignore les fistules, catheters ou abords de dialyse s'ils sont mentionnes seuls sans preuve explicite de dialyse en cours",
      "- Retourne uniquement un JSON valide",
      "",
      "Format attendu:",
      "{json_format}",
      "",
      "Texte:",
      "{text}",
      sep = "\n"
    ),
    keys = list(
      receveur_dialyse_text = c(0L, 1L),
      receveur_type_dialyse_text = c("hemodialyse", "dialyse peritoneale", "mixte"),
      receveur_dialyse_text_resume = "character"
    ),
    rows = list(
      list(
        case_id = "hemodialyse",
        text = paste(
          "Extraits sur la dialyse avant la greffe actuelle:",
          "2025-01-12 | CRH2AB | Patient en hemodialyse trihebdomadaire depuis 2022.",
          "2025-02-10 | CR2AAF | Toujours en hemodialyse au moment du bilan pre-greffe.",
          sep = "\n"
        ),
        expected = list(
          receveur_dialyse_text = 1L,
          receveur_type_dialyse_text = "hemodialyse",
          receveur_dialyse_text_resume = "Patient en hemodialyse avant la greffe actuelle"
        )
      ),
      list(
        case_id = "preemptive",
        text = paste(
          "Extraits sur la dialyse avant la greffe actuelle:",
          "2025-03-08 | CR2AAF | Greffe pre-emptive programmee avant toute dialyse chronique.",
          "2025-03-09 | CRH2AB | Patient non dialyse a ce jour.",
          sep = "\n"
        ),
        expected = list(
          receveur_dialyse_text = 0L,
          receveur_type_dialyse_text = NA_character_,
          receveur_dialyse_text_resume = "Greffe pre-emptive avant toute dialyse"
        )
      ),
      list(
        case_id = "ambiguous_service_only",
        text = paste(
          "Extraits sur la dialyse avant la greffe actuelle:",
          "2025-04-01 | CRH2AB | Consultation du service d'hemodialyse.",
          "2025-04-02 | CR2AAF | Fistule arterio-veineuse ancienne, sans preuve de dialyse en cours.",
          sep = "\n"
        ),
        expected = list(
          receveur_dialyse_text = NA_integer_,
          receveur_type_dialyse_text = NA_character_,
          receveur_dialyse_text_resume = NA_character_
        )
      )
    )
  ),
  list(
    task_id = "d0840_nephropathie_initiale",
    prompt = paste(
      "Tu es un assistant d'extraction clinique en transplantation renale.",
      "Tu recois uniquement des extraits d'antecedents autour de la chirurgie du receveur.",
      "",
      "Tu dois determiner la nephropathie initiale du receveur, c'est-a-dire la maladie renale causale ayant conduit a l'insuffisance renale terminale et a la transplantation.",
      "",
      "Tu dois extraire 2 informations:",
      "- receveur_nephropathie_initiale",
      "- receveur_nephropathie_initiale_resume",
      "",
      "Regles de decision:",
      "- Base-toi uniquement sur les extraits fournis",
      "- Cherche la cause de la maladie renale du receveur avant la transplantation actuelle",
      "- Ne retourne pas simplement 'insuffisance renale terminale' sans etiologie",
      "- Diabete seul ne suffit pas: retourne 'nephropathie diabetique' seulement si l'atteinte renale diabetique est explicite",
      "- Si les extraits disent explicitement que l'etiologie est indeterminee / inconnue, retourne 'indeterminee'",
      "- Si plusieurs etiologies possibles sont mentionnees sans cause clairement retenue, retourne null",
      "- Retourne uniquement un JSON valide",
      "",
      "Format attendu:",
      "{json_format}",
      "",
      "Texte:",
      "{text}",
      sep = "\n"
    ),
    keys = list(
      receveur_nephropathie_initiale = c(
        "maladie de Berger / nephropathie a IgA",
        "hyalinose segmentaire et focale",
        "polykystose",
        "nephropathie de reflux",
        "uropathie malformative",
        "nephropathie interstitielle",
        "nephropathie lupique",
        "nephropathie diabetique",
        "nephronophtise",
        "syndrome d'Alport",
        "glomerulonephrite chronique",
        "indeterminee"
      ),
      receveur_nephropathie_initiale_resume = "character"
    ),
    rows = list(
      list(
        case_id = "iga",
        text = paste(
          "Extraits sur la nephropathie initiale du receveur:",
          "2024-11-15 | CRH2AB | IR terminale secondaire a une nephropathie a IgA documentee sur biopsie.",
          "2025-01-03 | CR2AAF | Antecedent de maladie de Berger responsable de l'insuffisance renale terminale.",
          sep = "\n"
        ),
        expected = list(
          receveur_nephropathie_initiale = "maladie de Berger / nephropathie a IgA",
          receveur_nephropathie_initiale_resume = "Maladie de Berger responsable de l'insuffisance renale terminale"
        )
      ),
      list(
        case_id = "uropathie_malformative",
        text = paste(
          "Extraits sur la nephropathie initiale du receveur:",
          "2024-10-02 | CRH2AB | Insuffisance renale terminale sur reflux vesico-ureteral bilateral et hypoplasie renale.",
          "2025-01-18 | CR2AAF | Uropathie malformative de l'enfance avec nephropathie de reflux.",
          sep = "\n"
        ),
        expected = list(
          receveur_nephropathie_initiale = "uropathie malformative",
          receveur_nephropathie_initiale_resume = "Reflux vesico-ureteral avec hypoplasie renale"
        )
      ),
      list(
        case_id = "ambiguous_multiple_causes",
        text = paste(
          "Extraits sur la nephropathie initiale du receveur:",
          "2024-09-11 | CRH2AB | IRC terminale chez patient diabetique et hypertendu, etiologie renale non tranchee.",
          "2025-01-07 | CR2AAF | Plusieurs causes evoquees sans cause clairement retenue.",
          sep = "\n"
        ),
        expected = list(
          receveur_nephropathie_initiale = NA_character_,
          receveur_nephropathie_initiale_resume = NA_character_
        )
      )
    )
  ),
  list(
    task_id = "dmo_lifestyle",
    prompt = paste(
      "Tu es un assistant specialise dans l'interpretation de textes medicaux.",
      "Tu dois extraire uniquement les habitudes de vie du patient au moment du document,",
      "en distinguant consommation active, sevrage et absence explicite.",
      "N'utilise pas des informations hypothetique ou familiales.",
      "",
      "Retourne strictement un JSON valide avec exactement ces cles:",
      "{{",
      '  "tabac_statut": "actif | sevre | non_fumeur | NA",',
      '  "tabac_evidence_excerpt": "court extrait ou NA",',
      '  "alcool_statut": "actif | sevre | non_consommateur | NA",',
      '  "alcool_evidence_excerpt": "court extrait ou NA",',
      '  "drogue_statut": "actif | sevre | aucune | NA",',
      '  "drogue_type": "cannabis ; cocaine ; cannabis; cocaine ; opioides ; amphetamines ; mdma ; hallucinogenes ; ketamine ; autre | NA",',
      '  "drogue_evidence_excerpt": "court extrait ou NA"',
      "}}",
      "",
      "Regles:",
      "- `actif` = consommation actuelle ou recente explicitement decrite au moment du document.",
      "- `sevre` = sevre, arret anterieur, ex-consommateur.",
      "- `non_fumeur` / `non_consommateur` / `aucune` = absence explicite.",
      "- `ethylisme` ou `alcoolisme` comptent comme ancre forte pour l'alcool.",
      "- Pour `drogue_type`, retourne une ou plusieurs classes canoniques separees par `;`.",
      "- Si l'information n'est pas claire ou absente, retourne `NA`.",
      "",
      "Texte:",
      "{text}",
      sep = "\n"
    ),
    keys = list(
      tabac_statut = c("actif", "sevre", "non_fumeur"),
      tabac_evidence_excerpt = "character",
      alcool_statut = c("actif", "sevre", "non_consommateur"),
      alcool_evidence_excerpt = "character",
      drogue_statut = c("actif", "sevre", "aucune"),
      drogue_type = c(
        "cannabis",
        "cocaine",
        "cannabis; cocaine",
        "opioides",
        "amphetamines",
        "mdma",
        "hallucinogenes",
        "ketamine",
        "autre"
      ),
      drogue_evidence_excerpt = "character"
    ),
    rows = list(
      list(
        case_id = "active_multi_substances",
        text = paste(
          "Compte rendu de suivi nutritionnel.",
          "Le patient fume 10 cigarettes par jour.",
          "Consommation d'alcool encore active le week-end.",
          "Usage de cannabis et de cocaine rapporte lors des soirees recentes.",
          sep = " "
        ),
        expected = list(
          tabac_statut = "actif",
          tabac_evidence_excerpt = "fume 10 cigarettes par jour",
          alcool_statut = "actif",
          alcool_evidence_excerpt = "consommation d'alcool encore active",
          drogue_statut = "actif",
          drogue_type = "cannabis; cocaine",
          drogue_evidence_excerpt = "usage de cannabis et de cocaine"
        )
      ),
      list(
        case_id = "former_smoker_abstinent",
        text = paste(
          "Consultation de suivi.",
          "Ancien tabagisme arrete en 2018.",
          "Abstinent sur le plan alcoolique depuis plusieurs annees.",
          "Nie toute consommation de drogues.",
          sep = " "
        ),
        expected = list(
          tabac_statut = "sevre",
          tabac_evidence_excerpt = "ancien tabagisme arrete en 2018",
          alcool_statut = "sevre",
          alcool_evidence_excerpt = "abstinent sur le plan alcoolique",
          drogue_statut = "aucune",
          drogue_type = NA_character_,
          drogue_evidence_excerpt = "nie toute consommation de drogues"
        )
      ),
      list(
        case_id = "explicit_absence",
        text = paste(
          "Evaluation initiale.",
          "Patient jamais fumeur.",
          "Ne consomme pas d'alcool.",
          "Aucune drogue illicite rapportee.",
          sep = " "
        ),
        expected = list(
          tabac_statut = "non_fumeur",
          tabac_evidence_excerpt = "patient jamais fumeur",
          alcool_statut = "non_consommateur",
          alcool_evidence_excerpt = "ne consomme pas d'alcool",
          drogue_statut = "aucune",
          drogue_type = NA_character_,
          drogue_evidence_excerpt = "aucune drogue illicite rapportee"
        )
      )
    )
  )
)
