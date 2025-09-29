## Participant characteristics, grouping and randomization

library(tidyverse); library(readxl); library(reliefdata)

# Load the data
dat <- read_excel("./data/relief_age&height.xlsx") %>%
  mutate(participant = as.character(participant)) %>%
  print()


## Load data from other data sets to compile participant characteristics

## Weight from Hb data
weight <- reliefdata::relief_hb %>%
  filter(time == "pre") %>%
  select(participant, weight) %>%
  print()

## Weight values missing from Hb data are retreived in the DXA data
weight2 <- reliefdata::relief_bodycomp %>%
  filter(time == "pre") %>%
  select(participant, weight = weight.whole) %>%
  print()
