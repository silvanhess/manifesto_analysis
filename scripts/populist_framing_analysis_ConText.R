# load libraries ---------------------------------------------------------

library(tidyverse)

# import data ------------------------------------------------------------

policlim_full_qs <- read_csv("data/raw/policlim_full_qs_2025-17-06.csv")

# clean data -------------------------------------------------------------

policlim_cleaned <- policlim_full_qs |>
  filter(climate == 1, year >= 2015)

policlim_countries <- policlim_cleaned |>
  distinct(country) |>
  arrange(country) |>
  pull(country)

# TODO: get translated texts fom manifesto project
# TODO: analyze with ConText
