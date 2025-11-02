# load packages ----------------------------------------------------------

library(manifestoR)
library(tidyverse)
library(countrycode)

# Manifesto Project Overview ---------------------------------------------

manifestoR::mp_setapikey("manifesto_apikey.txt")
versions <- manifestoR::mp_coreversions()
codebook <- manifestoR::mp_codebook()
maindataset <- manifestoR::mp_maindataset()
parties <- manifestoR::mp_parties()

# Dataset Creation --------------------------------------------------------

# 1. Filter European manifestos after 2000
european_manifestos <- maindataset |>
  mutate(continent = countrycode(countryname, "country.name", "continent")) |>
  filter(continent == "Europe", date > 200000)

# 2. Keep only greens and populists, join party information
greens_populists <- european_manifestos |>
  filter(parfam %in% c(10, 70)) |>
  left_join(parties, by = join_by(party == party)) |>
  rename(countryname = countryname.x)

# 3. Keep only countries that have BOTH groups
greens_populists_filtered <- greens_populists |>
  group_by(countryname) |>
  filter(all(c(10, 70) %in% parfam)) |>
  ungroup()

# 4. Create election periods
greens_populists_periods <-
  greens_populists_filtered |>
  mutate(
    year = date %/% 100, # extract year from YYYYMM
    period = case_when(
      year >= 2000 & year <= 2004 ~ "2000-2004",
      year >= 2005 & year <= 2009 ~ "2005-2009",
      year >= 2010 & year <= 2014 ~ "2010-2014",
      year >= 2015 & year <= 2019 ~ "2015-2019",
      year >= 2020 & year <= 2025 ~ "2020-2025",
      TRUE ~ NA_character_
    )
  )
write_rds(greens_populists_periods, "data/prepgreens_populists_periods.rds")

greens_populists_wide <- greens_populists_periods |>
  select(countryname, parfam, period) |>
  group_by(countryname, parfam, period) |>
  summarise(manifesto_exists = n(), .groups = "drop") |>
  pivot_wider(
    names_from = period,
    values_from = manifesto_exists,
    values_fill = NA # fill missing periods with NA
  )
write_rds(greens_populists_wide, "data/greens_populists_wide.rds")
