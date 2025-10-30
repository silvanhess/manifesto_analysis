# load packages ----------------------------------------------------------

library(manifestoR)
library(tidyverse)
library(countrycode)
library(conText)
library(quanteda)
library(text)
library(reticulate)

# set api key ------------------------------------------------------------

manifestoR::mp_setapikey("manifesto_apikey.txt")

# collect raw data -------------------------------------------------------

if (!dir.exists("data")) {
  dir.create("data")
}

if (!file.exists("data/maindataset.rds")) {
  maindataset <- manifestoR::mp_maindataset()
  saveRDS(maindataset, "data/maindataset.rds")
}
maindataset <- readRDS("data/maindataset.rds")

# Parties
if (!file.exists("project_data/parties.rds")) {
  parties <- manifestoR::mp_parties()
  saveRDS(parties, "data/parties.rds")
}
parties <- readRDS("data/parties.rds")

# get manifestos ---------------------------------------------------------

df_cleaned <-
  maindataset |>
  mutate(
    continent = countrycode(
      countryname,
      "country.name",
      "continent"
    )
  ) |>
  filter(
    continent == "Europe",
    date > 200000,
    parfam == 10
  ) |>
  left_join(
    parties,
    by = join_by(
      party == party
    )
  ) |>
  rename(
    countryname = countryname.x,
    country = country.x
  )

manifesto_ids <-
  df_cleaned |>
  select(party, date)

if (!file.exists("data/corpus.rds")) {
  df_corpus <- mp_corpus(manifesto_ids, as_tibble = TRUE)
  saveRDS(df_corpus, "data/corpus.rds")
}
df_corpus <- readRDS("data/corpus.rds")

# set Parameters ---------------------------------------------------------

anchors <- c("climate", "energy")
features <- c("denial", "elitist", "cultural", "national", "sovereignty")

# set up python ----------------------------------------------------------

# Initialize the installed Conda environment
text::textrpp_initialize(save_profile = TRUE)

# Compute sentence embeddings --------------------------------------------

# Compute embeddings
sentence_embeddings <- textEmbed(
  df_corpus$text,
  model = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
)$text_embeds

# Attach embeddings to sentences
df_corpus$embedding <- split(
  as.data.frame(sentence_embeddings),
  seq_len(nrow(sentence_embeddings))
)

# Get anchor and feature embeddings --------------------------------------

anchor_embeds <- textEmbed(
  anchors,
  model = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
)$text_embeds
feature_embeds <- textEmbed(
  features,
  model = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
)$text_embeds


# cosine similarity ------------------------------------------------------

get_cos <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

sentences_df <- sentences_df |>
  rowwise() |>
  mutate(
    anchor_sim = mean(sapply(1:nrow(anchor_embeds), function(i) {
      get_cos(unlist(embedding), anchor_embeds[i, ])
    })),
    feature_sim = mean(sapply(1:nrow(feature_embeds), function(i) {
      get_cos(unlist(embedding), feature_embeds[i, ])
    }))
  )

# Aggregate Results ------------------------------------------------------

party_results <- sentences_df %>%
  group_by(party) %>%
  summarize(
    mean_anchor_sim = mean(anchor_sim, na.rm = TRUE),
    mean_feature_sim = mean(feature_sim, na.rm = TRUE),
    .groups = "drop"
  )


# Visualization ----------------------------------------------------------

ggplot(
  party_results,
  aes(x = mean_anchor_sim, y = mean_feature_sim, label = party)
) +
  geom_point(size = 3, color = "#3366CC") +
  geom_text(vjust = -0.5, size = 4) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Semantic Association Between Climate/Energy and Populist Frames",
    subtitle = "Measured using ManifestoBERTa sentence embeddings",
    x = "Similarity to Anchors (climate, energy)",
    y = "Similarity to Features (denial, elitist, cultural, national, sovereignty)"
  )

top_sentences <- sentences_df %>%
  arrange(desc(anchor_sim)) %>%
  slice_head(n = 10) %>%
  select(party, sentence, anchor_sim, feature_sim)

print(top_sentences)
