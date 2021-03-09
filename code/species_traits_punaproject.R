## Puna Project 2019 PERU ----

#' Cleaning and merging trait data with the species names corrected in 2020
#'
# Load libraries ----------------------------------------------------------

# source this file if needed
#source("code/load_libraries.R")

# 0RIGINAL ENVELOPE CODES -

all_codes <- get_PFTC_envelope_codes(seed = 1)

## Read in data ----
#' Species data dictionary for 2019

spp_trait_dictionary_2019 <- read_csv("data/PunaProject_Peru_2019_TaxonomicDictionary.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

#'
#' Trait data from 2019 Puna Project
#'

trait_2019 <- read_csv("data/PunaProject_Peru_2019_LeafTraits.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

#' Review unique IDs

trait_2019 %>%
  count(id) %>%
  distinct(n)

## Merging data sets: add species names cleaned ----

# Trait from Puna Project

trait_puna <- trait_2019 %>%
  mutate(plot_id = as.character(plot_id)) %>%
  left_join(spp_trait_dictionary_2019,
            by = c("month", "site", "treatment", "plot_id", "taxon")) %>%
  mutate(course = "Puna",
         country = "PE",
         project = "T",
         gradient = 1,
         genus = str_replace(name_2020, "(?s) .*", ""),
         species = str_remove(name_2020, paste0(genus, " ")),
         plot_id = as.character(plot_id),
         treatment = if_else(site == 'QUE', "B", treatment),
         treatment = str_to_upper(treatment),
         # All this data need to be cheeked  by Aud
         area_flag = NA_character_,
         dry_flag = NA_character_,          #
         wet_flag = NA_character_,          #
         date = ymd(date)) %>%
  #TODO
  # Drymass was zero changed to NA to avoid problems
  mutate(dry_mass_total_g = if_else(id == "AZJ4672",
                              NA_real_,
                              dry_mass_total_g)) %>%
  # Cleaning trait values
  mutate(number_leaves_scan = ifelse(name_2020 %in% c("Baccharis genistelloides",
                                                      "Lycopodium thyoides",
                                                      "Lycopodium clavatum",
                                                      "Hypericum andinum" ), 1,
                                     number_leaves_scan)) %>%
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  mutate(leaf_area_total_cm2= ifelse(genus == "Sisyrinchium", leaf_area_total_cm2 * 2, leaf_area_total_cm2),
         leaf_thickness_1_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_1_mm / 2, leaf_thickness_1_mm),
         leaf_thickness_2_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_2_mm / 2, leaf_thickness_2_mm),
         leaf_thickness_3_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_3_mm / 2, leaf_thickness_3_mm)) %>%
  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / number_leaves_scan,
         dry_mass_g = dry_mass_total_g / number_leaves_scan,
         leaf_area_cm2 = leaf_area_total_cm2 / number_leaves_scan) %>%

    # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g)  %>%
  #Some of these species were not in the dictionary thus now have NA's
  mutate(functional_group = case_when(taxon == "Carex pichinchensis" ~ "Gramminoid",
                                      taxon == "Rinchospora machrochaeta fina" ~ "Gramminoid",
                                      taxon == "Rinchospora machrochaeta gruesa" ~ "Gramminoid",
                                      TRUE ~ functional_group),
         family = case_when(taxon == "Carex pichinchensis" ~ "Cyperaceae",
                            taxon == "Rinchospora machrochaeta fina" ~ "Cyperaceae",
                            taxon == "Rinchospora machrochaeta gruesa" ~ "Cyperaceae",
                            TRUE ~ family),
         species = case_when(taxon == "Carex pichinchensis" ~ "pichinchensis",
                             taxon == "Rinchospora machrochaeta fina" ~ "machrochaeta fina",
                             taxon == "Rinchospora machrochaeta gruesa" ~ "machrochaeta gruesa",
                             TRUE ~ species),
         genus = case_when(taxon == "Carex pichinchensis" ~ "Carex",
                           taxon == "Rinchospora machrochaeta fina" ~ "Rinchospora",
                           taxon == "Rinchospora machrochaeta gruesa" ~ "Rinchospora",
                           TRUE ~ genus),
         name_2020 = case_when(taxon == "Carex pichinchensis" ~ "Carex pichinchensis",
                               taxon == "Rinchospora machrochaeta fina" ~ "Rinchospora machrochaeta fina",
                               taxon == "Rinchospora machrochaeta gruesa" ~ "Rinchospora machrochaeta fina",
                               TRUE ~ name_2020)) %>%
  #Rename NB to BB
  mutate(treatment = if_else(treatment == "NB",
                             "BB",
                             treatment)) %>%
  # Reordering columns for matching with the other dataset
  select(country, course, project, id, year, month, date, gradient, site, treatment, plot_id,
         functional_group, family, name_2020, genus, species,
         individual_nr, plant_height_cm, nr_leaves, number_leaves_scan,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_mm = leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag)


# End of Script ---
