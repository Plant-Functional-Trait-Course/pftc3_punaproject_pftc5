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
  mutate_if(is.character, ~str_trim(.)) %>%
  # add missing combinations to dictionary
  bind_rows(tibble(month = c("April", "April", "July", "July"),
       site = c("QUE", "QUE", "ACJ", "ACJ"),
       treatment = c("B", "B", "NB", "NB"),
       plot_id = c("4", "5", NA_character_, NA_character_),
       functional_group = c(rep("Gramminoid", 4)),
       family = c(rep("Poaceae", 4)),
       taxon = c("Carex pichinchensis", "Carex pichinchensis", "Rinchospora machrochaeta fina", "Rinchospora machrochaeta gruesa"),
       name_2020 = c("Carex pichinchensis", "Carex pichinchensis", "Rhynchospora seedlings", "Rhynchospora seedlings"),
       notes = NA_character_))

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

# trait corrections
trait_correction <- read_excel(path = "data/trait_pftc_and_puna_corregido_LVB.xlsx") %>%
  select(id, wet_mass_corr = wet_mass_total_g, dry_mass_corr = dry_mass_total_g, leaf_area_corr = leaf_area_total_cm2, leaf_thickness_1_corr = leaf_thickness_1_mm, leaf_thickness_2_corr = leaf_thickness_2_mm, leaf_thickness_3_corr = leaf_thickness_3_mm, number_leaves_scan_paul)

trait_puna <- trait_2019 %>%
  mutate(plot_id = as.character(plot_id)) %>%
  left_join(spp_trait_dictionary_2019,
            by = c("month", "site", "treatment", "plot_id", "taxon")) %>%
  mutate(course = "Puna",
         country = "PE",
         project = "T",
         gradient = 1,
         # all combos are in the species dictionary, so this works!
         genus = str_replace(name_2020, "(?s) .*", ""),
         species = str_remove(name_2020, paste0(genus, " ")),
         taxon = paste(genus, species, sep = " "),
         plot_id = as.character(plot_id),
         treatment = if_else(site == 'QUE', "B", treatment),
         treatment = if_else(site == 'TRE' & treatment == "B", "NB", treatment),
         treatment = str_to_upper(treatment),
         # All this data need to be cheeked  by Aud
         area_flag = NA_character_,
         dry_flag = NA_character_,          #
         wet_flag = NA_character_,          #
         date = ymd(date)) %>%

  # join corrections
  left_join(trait_correction, by = c("id")) %>%
  mutate(number_leaves_scan_paul = as.numeric(ifelse(number_leaves_scan_paul == "no se encontro" | is.na(number_leaves_scan_paul), number_leaves_scan, number_leaves_scan_paul))) %>%

  mutate(wet_mass_total_g = wet_mass_corr,
         dry_mass_total_g = dry_mass_corr,
         leaf_area_total_cm2 = leaf_area_corr,
         leaf_thickness_1_mm = leaf_thickness_1_corr,
         leaf_thickness_2_mm = leaf_thickness_2_corr,
         leaf_thickness_3_mm = leaf_thickness_3_corr) %>%

  # Cleaning trait values
  mutate(number_leaves_scan_paul = ifelse(name_2020 %in% c("Baccharis genistelloides",
                                                      "Lycopodium thyoides",
                                                      "Lycopodium clavatum",
                                                      "Hypericum andinum" ), 1,
                                          number_leaves_scan_paul)) %>%
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  mutate(leaf_area_total_cm2= ifelse(genus == "Sisyrinchium", leaf_area_total_cm2 * 2, leaf_area_total_cm2),
         leaf_thickness_1_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_1_mm / 2, leaf_thickness_1_mm),
         leaf_thickness_2_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_2_mm / 2, leaf_thickness_2_mm),
         leaf_thickness_3_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_3_mm / 2, leaf_thickness_3_mm)) %>%
  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / number_leaves_scan_paul,
         dry_mass_g = dry_mass_total_g / number_leaves_scan_paul,
         leaf_area_cm2 = leaf_area_total_cm2 / number_leaves_scan_paul) %>%
  # Wet and dry mass do not make sense for these species
  mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
         wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
         leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) %>%

  # make dry mass NA if 0
  mutate(dry_mass_g = if_else(dry_mass_g == 0, NA_real_, dry_mass_g)) %>%

    # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g)  %>%
  distinct() %>%

  # remove problematic leaves
  mutate(wet_mass_g = if_else(ldmc > 1, NA_real_, wet_mass_g),
         dry_mass_g = if_else(ldmc > 1, NA_real_, dry_mass_g),
         ldmc = if_else(ldmc > 1, NA_real_, ldmc,),
         sla_cm2_g = if_else(ldmc > 1, NA_real_, sla_cm2_g),

         wet_mass_g = if_else(sla_cm2_g > 600, NA_real_, wet_mass_g),
         dry_mass_g = if_else(sla_cm2_g > 600, NA_real_, dry_mass_g),
         ldmc = if_else(sla_cm2_g > 600, NA_real_, ldmc),
         leaf_area_cm2 = if_else(sla_cm2_g > 600, NA_real_, leaf_area_cm2),
         sla_cm2_g = if_else(sla_cm2_g > 600, NA_real_, sla_cm2_g)) %>%

  # Reordering columns for matching with the other dataset
  select(country, course, project, id, year, month, date, gradient, site, treatment, plot_id,
         functional_group, family, taxon, genus, species,
         individual_nr, plant_height_cm,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_mm = leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag)

# End of Script ---
