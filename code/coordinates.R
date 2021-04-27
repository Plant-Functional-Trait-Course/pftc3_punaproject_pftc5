## read in coordinate

# Load libraries ----------------------------------------------------------
source("code/load_libraries.R")
library("janitor")

coordinates <- read_excel("data/PU.10_PFTC3.10_2020_Peru_Coordinates.xlsx") %>%
  clean_names() %>%
  mutate(plot_id = as.character(plot_id)) %>%
  select(site, treatment, plot_id, burn_year, elevation, latitude, longitude, comment)
