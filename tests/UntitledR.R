library(tidyverse)
library(readtext)

# read all .txt files in the following folder
files <- list.files("bilan preop/", full.names = TRUE, pattern = "\\.txt$")
# read the files and store them in a dataframe
df <- readtext(files, docid = "doc_id", text_field = "text", encoding = "UTF-8") %>%
    mutate(doc_id = (str_extract(doc_id, "\\d+"))) %>%
    arrange(doc_id)

df$text = str_sub(df$text,1,1000)

# use gptr to rewrite text into a new column text_rewritten
prompt_bilan_preop <- paste0(
    "Tu es un assistant data scientist spécialisé dans le traitement de données médicales.\n",
    "Ta tâche: réécrire le texte ci-dessous en assurant que:\n",
    "- le texte est anonymisé\n",
    "- aucune information n'est perdue\n\n",
    "Voici le texte : \"{text}\"\n\n",
    "Réponds uniquement avec un JSON une seule ligne, format : {json_format}"
)

x = gpt_column(
    data = df[1:5,],
    col = text,
    prompt = prompt_bilan_preop,
    keys = list(text_rewritten = "character")
)



