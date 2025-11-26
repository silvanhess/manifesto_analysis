# load libraries ---------------------------------------------------------

library(tidyverse)
library(manifestoR)
library(openxlsx)
library(glue)

# import data ------------------------------------------------------------

policlim_full_qs <- read_csv("data/raw/policlim_full_qs_2025-17-06.csv")

# set api key ------------------------------------------------------------

manifestoR::mp_setapikey("manifesto_apikey.txt")

# clean original data -------------------------------------------------------------

policlim_cleaned <- policlim_full_qs |>
  filter(climate == 1, year >= 2015)

policlim_countries <- policlim_cleaned |>
  distinct(country) |>
  arrange(country) |>
  pull(country)

policlim_full_qs |>
  select(manifesto_id, original_text, prob_climate)

policlim_nationalist <- policlim_cleaned |>
  filter(parfam == 70)
write.xlsx(policlim_nationalist, "data/policlim_nationalist.xlsx")

# get manifesto project data ---------------------------------------------

ids <- policlim_cleaned |>
  select(party, edate) |>
  distinct() |>
  rename(date = edate) |>
  mutate(date = as.double(date)) |>
  drop_na()

mp_original <- mp_corpus(ids = ids, as_tibble = TRUE)
mp_original_translated <- mp_corpus(
  ids = ids,
  translation = "en",
  as_tibble = TRUE
)

mp_original_cleaned <- mp_original |>
  left_join(mp_original_translated, by = join_by(manifesto_id, pos)) |>
  select(
    manifesto_id,
    pos,
    original_text = text.x,
    translated_text = text.y
  )


# give row_numbers that start from 1 for each manifesto_id in the format 0001, 0002, 0003, etc.
# mp_original_cleaned <- mp_original |>
#   group_by(manifesto_id) |>
#   mutate(
#     row_number = row_number(),
#     #change format of the row number to 0001, 0002, 0003, etc.
#     row_number = str_pad(row_number, width = 4, side = "left", pad = "0")
#   ) |>
#   ungroup() |>
#   mutate(qs_id = glue("{manifesto_id}_{row_number}"))

# add translations -------------------------------------------------------

# policlim_translated <- policlim_full_qs |>
#   left_join(mp_original_cleaned, by = join_by(qs_id == qs_id)) |>
#   select(qs_id, original_text, text, climate)

# create windows ---------------------------------------------------------

# glue together 4 sentences before and after each climate sentence
policlim_windows <- policlim_full_qs |>
  group_by(manifesto_id) |>
  mutate(
    sentence_context = glue(
      "{lag(original_text, 4, default = '')} {lag(original_text, 3, default = '')} {lag(original_text, 2, default = '')} {lag(original_text, 1, default = '')} {original_text} {lead(original_text, 1, default = '')} {lead(original_text, 2, default = '')} {lead(original_text, 3, default = '')} {lead(original_text, 4, default = '')}"
    ),
    row_number = row_number()
  ) |>
  ungroup()

policlim_windows_cleaned <- policlim_windows |>
  # now flag all windows that contain a climate sentence
  mutate(
    climate_window = ifelse(
      lag(climate, 4, default = 0) == 1 |
        lag(climate, 3, default = 0) == 1 |
        lag(climate, 2, default = 0) == 1 |
        lag(climate, 1, default = 0) == 1 |
        climate == 1 |
        lead(climate, 1, default = 0) == 1 |
        lead(climate, 2, default = 0) == 1 |
        lead(climate, 3, default = 0) == 1 |
        lead(climate, 4, default = 0) == 1,
      1,
      0
    )
  ) |>
  # now filter for row row numbers 5, 15, 25, etc. to avoid overlapping windows
  filter(row_number %% 10 == 5) |>
  # now filter climate windows only
  filter(climate_window == 1) |>
  # filter manifestos from the last ten years and only nationalist parties
  filter(year >= 2015, parfam == 70)

write.xlsx(policlim_windows_cleaned, "data/policlim_windows_cleaned.xlsx")
