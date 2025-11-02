# libraries --------------------------------------------------------------

library(tidyverse)

# load data --------------------------------------------------------------

populist <- read.csv("data/raw/The PopuList 3.0.csv", sep = ";") |>
    filter(populist == 1 & farright == 1)
