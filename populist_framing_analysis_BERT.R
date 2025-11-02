# load packages ----------------------------------------------------------

library(manifestoR)
library(tidyverse)
library(countrycode)
library(conText)
library(quanteda)
library(text)
library(reticulate)
library(text2vec)

# load python environment ------------------------------------------------

textrpp_initialize(condaenv = "textrpp_reticulate", save_profile = TRUE)

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
# just for info for around how many windows i get later
# because i construct the windows in a different way later
# this number may vary +/- 20 %

df_corpus_glued_sentences <- df_corpus |>
  group_by(manifesto_id, party, date, language) |>
  summarise(
    text_original = str_c(text_original, collapse = " "),
    text_translated = str_c(text_translated, collapse = " "),
    .groups = "drop"
  )

# Create corpus ----------------------------------------------------------

the_corpus <- corpus(
  df_corpus_glued_sentences,
  docid_field = 'manifesto_id',
  text_field = 'text_translated',
  meta = c('party', 'date', 'language')
) # create corpus object from dataframe

the_toks <- tokens(
  the_corpus,
  remove_punct = T,
  remove_numbers = T,
  remove_symbols = T,
  remove_separators = T,
  include_docvars = T,
) # tokenize corpus

anchors <- c("climate", "energy")

toks_cont <- tokens_context(
  the_toks,
  pattern = anchors,
  window = 6L, # adjust for Performance from 6 to 24
  case_insensitive = T
) #create tokens context

df <- tibble(
  doc_id = docnames(toks_cont),
  text = sapply(as.list(toks_cont), function(x) paste(x, collapse = " ")),
  party = docvars(toks_cont, "party"),
  date = docvars(toks_cont, "date"),
  year = as.numeric(str_extract(date, "^.{4}"))
  # anchor = ifelse(str_detect(tokens, "climate"), "climate", "energy")
)

# check doube-occurence
# df |> filter(str_detect(tokens, "(?=.*climate)(?=.*energy)")) # there are also some co-occurences

# Compute embeddings --------------------------------------------

if (!file.exists("data/embeddings.rds")) {
  embeddings <- textEmbed(
    df$text,
    model = "sentence-transformers/all-MiniLM-L6-v2"
  )
  saveRDS(embeddings, "data/embeddings.rds")
} else {
  embeddings <- readRDS("data/embeddings.rds")
}

embedding_matrix <- as.matrix(embeddings$texts$texts)
embedding_matrix_enriched <- embedding_matrix |>
  bind_cols(df)

# Compute mean embedding across all documents (global anchor)
jmean_anchor_embedding <- colMeans(embedding_matrix)

# Compute mean embedding for each party
jmean_anchor_embedding_grouped <- embedding_matrix_enriched |>
  group_by(party) |>
  summarise(
    doc_id = first(doc_id),
    text = first(text),
    date = first(date),
    year = first(year),
    across(where(is.numeric), mean),
    .groups = "drop"
  )

# Transform back into Matrix Shape
anchor_embedding_matrix <- jmean_anchor_embedding_grouped |>
  select(-c(party, doc_id, text, date, year)) |>
  as.matrix()
row.names(anchor_embedding_matrix) <- jmean_anchor_embedding_grouped$party

# Feature embeddings -----------------------------------------------------

features <- c("denial", "elitist", "cultural", "national", "sovereignty")

if (!file.exists("data/feature_embeddings.rds")) {
  feature_embeddings <- textEmbed(
    text = features,
    model = "sentence-transformers/all-MiniLM-L6-v2"
  )
  saveRDS(feature_embeddings, "data/feature_embeddings.rds")
} else {
  feature_embeddings <- readRDS("data/feature_embeddings.rds")
}

feature_embeddings <- textEmbed(
  text = features,
  model = "sentence-transformers/all-MiniLM-L6-v2"
)

feature_matrix <- as.matrix(feature_embeddings$texts$texts)
rownames(feature_matrix) <- features

# Get Cosine Similarity --------------------------------------------------

cos_sims <- sim2(
  x = anchor_embedding_matrix, # mean anchor embeddings
  y = feature_matrix, # feature embeddings
  method = "cosine",
  norm = "l2"
)

cos_sim_results <- cos_sims |>
  as_tibble() |>
  mutate(
    party = row.names(cos_sims)
  ) |>
  select(party, everything()) |>
  pivot_longer(!party)

# group by party, calculate mean cosine similarity within group
agg_results <-
  cos_sim_results |>
  group_by(party) %>%
  summarize(value = mean(value))

# Visualize Results ------------------------------------------------------

ggplot(agg_results, aes(x = value)) +
  geom_dotplot() +
  labs(
    title = "Variance in Populist Framing",
    subtitle = "Climate-Relevant Manifesto Extracts of Nationalist and radical right Parties"
  )

if (!dir.exists("output")) {
  dir.create("output")
}

ggsave(
  filename = 'output/figure_1.jpg',
  device = 'jpg'
)
