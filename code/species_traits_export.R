# load libraries
source("code/load_libraries.R")
library("janitor")

## Read scripts for the three traits datasets ----
source(here::here("code/species_traits_pftc3.R")) #PFTC3 import and clean
source(here::here("code/species_traits_punaproject.R")) #Puna Project import and clean
source(here::here("code/species_traits_pftc5.R")) #PFTC5 import and clean
source("code/coordinates.R")

## Join all data ----

trait_data_peru <- bind_rows(trait_pftc3,
                             trait_puna,
                             trait_pftc5) %>%

  # make data long
  pivot_longer(cols = plant_height_cm:leaf_thickness_mm, names_to = "trait", values_to = "value") %>%
  filter(!is.na(value)) %>%

  mutate(flag = case_when(wet_flag == "Outlier_very_large_leaf" ~ "outlier",
                          area_flag == "Outlier_very_large_leaf" ~ "outlier",
                          area_flag == "Leaf too white_Area missing" ~ "scanning_too_white",
                          area_flag == "Area estimated" ~ "area_estimated")) %>%
  select(-area_flag, -dry_flag, -wet_flag) %>%
  left_join(coordinates, by = c("site", "treatment", "plot_id")) %>%
  select(-comment)

## Export data ----

# make new folder
dir.create("clean_data")

trait_data_peru %>%
  write_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv")


# End of Script ----
