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

# Main Dataset
if (!file.exists("data/maindataset.rds")) {
  maindataset <- manifestoR::mp_maindataset()
  saveRDS(maindataset, "data/maindataset.rds")
} else {
  maindataset <- readRDS("data/maindataset.rds")
}

# Parties
if (!file.exists("project_data/parties.rds")) {
  parties <- manifestoR::mp_parties()
  saveRDS(parties, "data/parties.rds")
} else {
  parties <- readRDS("data/parties.rds")
}


# clean dataset ---------------------------------------------------------

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

# get manifestos ---------------------------------------------------------

manifesto_ids <-
  df_cleaned |>
  select(party, date)

if (!file.exists("data/corpus.rds")) {
  corpus_original <- mp_corpus(manifesto_ids, as_tibble = TRUE)
  corpus_translated <- mp_corpus(
    manifesto_ids,
    as_tibble = TRUE,
    translation = "en"
  )
  df_corpus <- corpus_original |>
    left_join(
      corpus_translated,
      by = join_by(
        cmp_code,
        eu_code,
        pos,
        manifesto_id,
        party,
        date,
        language,
        annotations,
        translation_en
      )
    ) |>
    rename(text_original = text.x, text_translated = text.y)
  saveRDS(df_corpus, "data/corpus.rds")
} else {
  df_corpus <- readRDS("data/corpus.rds")
}

# clean manifesto dataset ------------------------------------------------

df_corpus_main_categories <- df_corpus |>
  mutate(
    main_category = str_extract(cmp_code, "([A-Za-z0-9])")
  ) |>
  group_by(main_category) |>
  summarise(count = n())

df_corpus_climate_energy <- df_corpus |>
  filter(str_detect(text_translated, "climate|energy"))

# to do: transform df in the way where one observation is a 24 words window of the keyword
# to achieve this glue all sentences of the same manifesto together and use tokens_context() Function from conText

# set Parameters ---------------------------------------------------------

anchors <- c("climate", "energy")
features <- c("denial", "elitist", "cultural", "national", "sovereignty")

# set up python ----------------------------------------------------------

reticulate::install_miniconda()
python_path <- readLines("python_path.txt")
reticulate::use_python(python_path)
reticulate::conda_create("textrpp_reticulate", packages = "python=3.9")
rpp_packages <- c(
  "torch==2.2.0",
  "transformers==4.38.0",
  "huggingface_hub==0.20.0",
  "numpy==1.26.0",
  "pandas==2.0.3",
  "nltk==3.8.1",
  "scikit-learn==1.3.0",
  "datasets==2.16.1",
  "evaluate==0.4.0",
  "accelerate==0.26.0",
  "bertopic==0.16.3",
  "jsonschema==4.19.2",
  "sentence-transformers==2.2.2",
  "flair==0.13.0",
  "umap-learn==0.5.6",
  "hdbscan==0.8.33",
  "scipy==1.10.1",
  "aiohappyeyeballs==2.4.4"
)

# Use Conda Forge for Package hdbscan
reticulate::conda_install(
  envname = "textrpp_reticulate",
  packages = "hdbscan",
  channel = "conda-forge"
)

# Install the Rest of the Packages normally
reticulate::conda_install(
  "textrpp_reticulate",
  packages = rpp_packages[!grepl("hdbscan", rpp_packages)],
  pip = TRUE
)

# Show available Conda environments
reticulate::conda_list()

# Initialize the installed Conda environment
text::textrpp_initialize(condaenv = "textrpp_reticulate", save_profile = TRUE)

# Test that textEmbed works
test_embedding <- textEmbed(
  "hello",
  model = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
) # this fails

# Try another Model
test_embedding <- textEmbed(
  "hello",
  model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
) # this works

# Compute sentence embeddings --------------------------------------------

# Compute embeddings
sentence_embeddings <- textEmbed(
  df_corpus$text,
  model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
)$text_embeds

# Attach embeddings to sentences
df_corpus$embedding <- split(
  as.data.frame(sentence_embeddings),
  seq_len(nrow(sentence_embeddings))
)

# Get anchor and feature embeddings --------------------------------------

anchor_embeds <- textEmbed(
  anchors,
  model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
)$text_embeds
feature_embeds <- textEmbed(
  features,
  model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
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
