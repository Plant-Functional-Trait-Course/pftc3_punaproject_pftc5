# Load libraries ----------------------------------------------------------

library("here")
library("readxl")
library("writexl")
library("tidyverse")
library("lubridate")
library("tpl")
library("PFTCFunctions")
library("janitor")

#'
#' Reading data species dictionary
#'

#' Species from 2018 PFTC3

spp_cover_dictionary_2018 <- read_csv("data/PFTC3_Peru_2018_TaxonomicDictionarycover.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.)) %>%
  mutate(taxon = str_to_sentence(taxon))

#' Species from 2019 Puna Project

spp_cover_dictionary_2019 <- read_csv("data/PunaProject_Peru_2019_TaxonomicDictionarycover.csv") %>%
  clean_names() %>%
  mutate(taxon = str_to_sentence(taxon),
         month = if_else(month == "Noviembre", "November", month)) %>%
  mutate_if(is.character, ~str_trim(.))

#############################################################################

#' Reading species coverage data
#'
#' Species cover from 2019 Puna Project
#'

spp_cover_2019 <- read_csv("data/PunaProject_Peru_2019_CommunityCover.csv") %>%
  clean_names() %>%
  mutate(taxon = str_to_sentence(taxon),
         month = if_else(month == "Noviembre", "November", month)) %>%
  mutate_if(is.character, ~str_trim(.))

#'
#' Species cover from 2018 PFTC3

spp_cover_2018 <- read_csv("data/PFTC3_Peru_2018_CommunityCover.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.)) %>%
  mutate(taxon = str_to_sentence(taxon)) %>%
  select(-c(family, genus, species, functional_group))

##########################################################################
#' Merging data with the species name corrected
#'
#'  PFTC3 data
#'

spp_clean_2018 <- spp_cover_2018 %>%
  left_join(spp_cover_dictionary_2018,
            by = c("site", "plot_id", "treatment", "taxon"))

#'
#' Puna Project
#'

spp_clean_2019 <- spp_cover_2019 %>%
  left_join(spp_cover_dictionary_2019,
            by = c("site", "month", "treatment", "plot_id", "taxon"))

##########################################################################
#' Cleaning data:
#' Based on the corrected species name *name_2020*
#'
#' PFTC3
#'

spp_clean_2018 <- spp_clean_2018 %>%
  group_by(site, treatment, plot_id, family, functional_group, name_2020) %>%
  summarise(cover = sum(cover),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(genus = str_replace(name_2020, "(?s) .*", ""),
         specie = str_remove(name_2020, paste0(genus, " "))) %>%
  select(site, treatment, plot_id, functional_group, family, genus, specie,
         taxon = name_2020, cover)


#'
#' Puna Project
#'


spp_clean_2019 <- spp_clean_2019 %>%
  group_by(month, site, treatment, plot_id, family, functional_group, name_2020) %>%
  summarise(cover = sum(cover),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(genus = str_replace(name_2020, "(?s) .*", ""),
         specie = str_remove(name_2020, paste0(genus, " "))) %>%
  select(month, site, treatment, plot_id, functional_group, family, genus, specie,
         taxon = name_2020, cover)

###################################################################################
# Merging PFTC3 and Puna Project

spp_clean_2018 <- spp_clean_2018 %>%
  mutate(year = 2018,
         project = "PFTC3",
         month = "March") %>%
  select(year, project, month, everything())


spp_clean_2019 <- spp_clean_2019 %>%
  mutate(year = 2019,
         project = "Puna") %>%
  select(year, project, everything())

# Here is the data for PFTC3 and PunaProject, merged

species_cover <- bind_rows(spp_clean_2018, spp_clean_2019)

# Convert to wide format for comparing species cover side by side by month

presence_absence_peru <- species_cover %>%
  select(-c(year, project, family, genus, specie)) %>%
  mutate(month = factor(month, levels = c("March", "April", "July", "November"))) %>%
  complete(month,
           nesting( site, treatment, plot_id, functional_group, taxon),
           fill = list(cover = 0)) %>%
###### Cover equal to 0 meaning that the specie where not registered
  pivot_wider(names_from = month,
              values_from = cover) %>%
  arrange(site, treatment, plot_id, functional_group, taxon)

presence_absence_peru


# Export ------------------------------------------------------------------


#PFTC3

spp_clean_2018 %>%
  write_csv("clean_data/PFTC3_Peru_2018_CommunityCover_clean.csv")

# Puna Project

spp_clean_2019 %>%
  write_csv("clean_data/PunaProject_Peru_2019_CommunityCover_clean.csv")

