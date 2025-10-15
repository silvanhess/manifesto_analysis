# load packages
library(manifestoR)
library(tidyverse)
library(countrycode)

# inspect manifesto project package
manifestoR::mp_setapikey("manifesto_apikey.txt")
versions <- manifestoR::mp_coreversions()
codebook <- manifestoR::mp_codebook()
maindataset <- manifestoR::mp_maindataset()
parties <- manifestoR::mp_parties()

# Create Dataset of Populist Parties in Europe after 2000
df_filtered <-
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


# download the documents
manifesto_ids <-
  df_filtered |>
  select(party, date)
# mutate(id = paste(party, date, sep = "_")) |>
# pull(id)

df_corpus <- mp_corpus(manifesto_ids, as_tibble = TRUE)
corpus <- mp_corpus(manifesto_ids)

write_csv(df_corpus, file = "data_prep/df_corpus.csv")
