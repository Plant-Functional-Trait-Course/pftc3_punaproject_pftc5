# load libraries
source("code/load_libraries.R")

## Read scripts for the three traits datasets ----
source(here::here("code/species_traits_pftc3.R")) #PFTC3 import and clean
source(here::here("code/species_traits_punaproject.R")) #Puna Project import and clean
source(here::here("code/species_traits_pftc5.R")) #PFTC5 import and clean

## Join all data ----

trait_data_peru <- bind_rows(trait_pftc3,
                             trait_puna,
                             trait_pftc5)

## Export data ----

#PFTC3
trait_pftc3 %>%
  distinct(project)

trait_pftc3 %>%
  write_csv("clean_data/PFTC3_Peru_2018_LeafTraits_clean.csv")

#PUNA PROJECT
trait_puna %>%
  distinct(project)

trait_puna %>%
  filter(id == "AZJ4672") %>%
  pull(dry_mass_g)

trait_puna %>%
  write_csv("clean_data/PunaProject_Peru_2019_LeafTraits_clean.csv")

#PFTC5
trait_pftc5 %>%
  distinct(project)

trait_pftc5 %>%
  filter(treatment != "OFF-PLOT")
write_csv("clean_data/PFTC5_Peru_2020_LeafTraits_clean.csv")

# PFTC3 - Puna Project - PFTC5
trait_data_peru %>%
  distinct(project)

#Sean...

trait_pftc5 %>%
  filter(treatment == "OFF-PLOT")
write_csv("clean_data/PFTC5_Peru_2020_Seans_LeafTraits_clean.csv")

# End of Script ----
