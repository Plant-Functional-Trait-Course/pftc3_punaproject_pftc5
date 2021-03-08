## PFTC3 2018 PERU ----

#' Cleaning and merging trait data with the species names corrected in 2020
#'
# Load libraries ----

library("readxl")
library("writexl")
library("tidyverse")
library("lubridate")
library("tpl")
library("PFTCFunctions")
library("janitor")
library("osfr")
library("here")

# 0RIGINAL ENVELOPE CODES ----

all_codes <- get_PFTC_envelope_codes(seed = 1)

#' Reading data from PFTC3
#'
#' Species data dictionary for PFTC3
#'

spp_trait_dictionary_2018 <- read_csv("data/PFTC3_Peru_2018_TaxonomicDictionary.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

#'
#' Trait data from PFTC3
#'

trait_2018 <- read_csv("data/PFTC3_Peru_2018_LeafTraits.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.)) %>%
  as_tibble()


###############################################################
#' Review unique IDs

trait_2018 %>%
  count(id) %>%
  distinct(n)


## Merging data sets: add species names cleaned ----
#'
#' Trait from PFTC3
#'

trait_pftc3 <- trait_2018 %>%
  left_join(spp_trait_dictionary_2018, by = c("treatment", "site", "plot_id", "taxon")) %>%
  mutate(project = "PFTC3",
         month = "March",
         genus = str_replace(name_2020, "(?s) .*", ""),
         species = str_remove(name_2020, paste0(genus, " ")),
         plot_id = as.character(plot_id),
         number_leaves_scan = as.character(number_leaves_scan),
         taxon_puna = NA_character_,
         date=as.character(date),
         dry_mass_total_g = as.numeric(dry_mass_total_g),
         number_leaves_scan = as.numeric(number_leaves_scan),
         wet_mass_g = wet_mass_g / nr_leaves,
         dry_mass_g = dry_mass_total_g / nr_leaves) %>%
  mutate(leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves,
         sla_cm2_g = leaf_area_cm2/dry_mass_g,
         ldmc = dry_mass_g/wet_mass_g) %>%
  select(country, project, id, year, month, date, site, treatment, plot_id,
         functional_group, family, name_2020, genus, species,
         individual_nr, plant_height_cm, nr_leaves, bulk, wet_mass_total_g,
         leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm,
         dry_mass_total_g, number_leaves_scan, leaf_area_total_cm2 ,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag, -c(7:10))

# End of Script ----
