#' Cleaning and merging trait data with the species names corrected on 2020
#'
# Load libraries ----------------------------------------------------------

library("readxl")
library("writexl")
library("tidyverse")
library("lubridate")
library("tpl")
library("PFTCFunctions")
library("janitor")
library("osfr")
library("here")

# 0RIGINAL ENVELOPE CODES -------------------------------------------------

all_codes <- get_PFTC_envelope_codes(seed = 1)

#' Reading data from PFTC3
#'
#' Species data dictionary for PFTC3
#'

spp_trait_dictionary_2018 <- read_csv("data/PFTC3_Peru_2018_TaxonomicDictionary.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

#' Species data dictionary for 2019

spp_trait_dictionary_2019 <- read_csv("data/PunaProject_Peru_2019_TaxonomicDictionary.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

#'
#' Trait data from PFTC3
#'

trait_2018 <- read_csv("data/PFTC3_Peru_2018_LeafTraits.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.)) %>%
  as_tibble()

#'
#' Trait data from 2019 Puna Project
#'

trait_2019 <- read_csv("data/PunaProject_Peru_2019_LeafTraits.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

###############################################################
#' Review unique IDs

trait_2018 %>%
  count(id) %>%
  distinct(n)

trait_2019 %>%
  count(id) %>%
  distinct(n)

###############################################################
#' Merging data sets: add species names cleaned
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
         number_leaves_scan = as.numeric(number_leaves_scan)) %>%
  select(country, project, id, year, month, date, site, treatment, plot_id,
         functional_group, family, name_2020, genus, species,
         individual_nr, plant_height_cm, nr_leaves, bulk, wet_mass_total_g,
         leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm,
         dry_mass_total_g, number_leaves_scan, leaf_area_total_cm2 ,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag, -c(7:10))


# Trait from Puna Project

trait_puna <- trait_2019 %>%
  mutate(plot_id = as.character(plot_id)) %>%
  left_join(spp_trait_dictionary_2019,
            by = c("month", "site", "treatment", "plot_id", "taxon")) %>%
  mutate(project = "Puna",
         country = "PE",
         genus = str_replace(name_2020, "(?s) .*", ""),
         species = str_remove(name_2020, paste0(genus, " ")),
         plot_id = as.character(plot_id),
         treatment = if_else(site == 'QUE', "B", treatment),
         treatment = str_to_upper(treatment),
         # All this data need to be cheeked  by Aud
         area_flag = NA_character_,
         dry_flag = NA_character_,          #
         wet_flag = NA_character_,          #
         date = as.character(date)) %>%
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
  # Wet and dry mass do not make sense for these species
  # mutate(Dry_Mass_g = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Dry_Mass_g),
  #       Wet_Mass_g = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Wet_Mass_g),
  #       Leaf_Area_cm2 = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Leaf_Area_cm2)) %>%
  # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g) %>%
  # Reordering columns for matching with the other dataset
  select(country, project, id, year, month, date, site, treatment, plot_id,
         functional_group, family, name_2020, genus, species,
         individual_nr, plant_height_cm, nr_leaves, bulk, wet_mass_total_g,
         leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm,
         dry_mass_total_g, number_leaves_scan, leaf_area_total_cm2,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag  )


########################################################################################
# PFTC5 2020 PERU

# Helpers -----------------------------------------------------------------

pn <- . %>% print(n = Inf)
source("code/taxon_correction.R")

# Get valid IDs -----------------------------------------------------------

all_codes2 <- get_PFTC_envelope_codes(seed = 6)

# Leaf traits -------------------------------------------------------------

trait_2020 <- read_csv("data/PFTC5_Peru_2020_LeafTraits.csv")  %>%
  clean_names() %>%
  mutate(project = str_to_lower(project),
         project = case_when(
           project == "t" ~ "trait",
           project == "s" ~ "sean",
           project == "r" ~ "trait" # R its for Rangild project intraspecific variability?
         ),
         id = str_to_upper(str_trim(id, side = "both")),
         site = str_to_upper(str_trim(site, side = "both")),
         site = if_else(site == "ACA", "ACJ", site),
         experiment = str_to_upper(experiment),
         experiment = if_else(experiment == "OFF=PLOT", NA_character_, experiment),
         genus = str_to_sentence(genus),
         species = str_to_lower(species),
         id = case_when(
           id == "CZQ3610" ~ "CYQ3610",
           id == "ADZ1845" ~ "ADY1845",
           id == "ADY3452" ~ "ADZ3452",
           id == "CZU0738" ~ "CYU0738",
           id == "CBN2433" ~ "CBM2433",
           id == "CGY2493" ~ "CGU2493",
           id == "BLF6180" ~ "BLE6180",
           id == "CQT3635" ~ "CQT3656",
           TRUE ~ id
         ),
         experiment = if_else(id == "AIR7210", "BB", experiment),
         leaf_thickness_1_mm = if_else(id == "AVH1607", 0.277, leaf_thickness_1_mm),
         leaf_thickness_1_mm = if_else(id == "BIX3249", 0.289, leaf_thickness_1_mm),
         leaf_thickness_2_mm = if_else(id == "BIX3249", 0.296, leaf_thickness_2_mm),
         leaf_thickness_2_mm = if_else(id == "AJA2506", 0.244, leaf_thickness_2_mm),
         leaf_thickness_3_mm = if_else(id == "ADB5258", 0.197, leaf_thickness_3_mm),
         leaf_thickness_3_mm = if_else(id == "CFN6705", 0.181, leaf_thickness_3_mm),
         leaf_thickness_2_mm = if_else(leaf_thickness_2_mm == -0.545, 0.545,
                                       leaf_thickness_2_mm),
         site = if_else(id == "BEA0992", "ACJ", site)) %>%
  # Check species and genus
  mutate(genus = plyr::mapvalues(genus, from = GenusDictionary2020$wrong,
                                 to = GenusDictionary2020$right, warn_missing = FALSE)) %>%

  mutate(species = tolower(species)) %>%
  mutate(species = plyr::mapvalues(species, from = SpeciesDictionary2020$wrong,
                                   to = SpeciesDictionary2020$right, warn_missing = FALSE)) %>%
  mutate(species = if_else(genus == "Carex" & species == "bonplandianum", "bonplandii", species),
         species = if_else(genus == "Lachemilla" & species == "umbellata", "orbiculata", species)) %>%
  mutate(genus = if_else(genus == "Calamagrostis" & species == "7", "Anatherostipa", genus),
         species = if_else(genus == "Anatherostipa" & species == "7", "hans-meyeri", species),
         species = if_else(genus == "Calamagrostis" & species == "macrochaeta", "cf. macrophylla", species),
         species = if_else(genus == "Vaccinium" & species == "bonplandianum", "floribundum", species),
         #TRE C Plot5 - lachemilla
         species = if_else(id %in% c("APS3068", "APO8208", "CNV5505", "CWL1543"), "vulcanica", species)) %>%
  # Fix Plant_Length_cm, Bulk_nr_leaves, Length_cm
  # move length to right column
  mutate(length_cm = if_else(!is.na(plant_length_cm) & genus %in% c("Hypericum", "Lycopodium"),
                             plant_length_cm, length_cm)) %>%
  # replace length with NA
  mutate(plant_length_cm = ifelse(!is.na(plant_length_cm) & genus %in% c("Halenia", "Oxalis", "Viola",
                                                                         "Vaccinium", "Hypericum",
                                                                         "Lycopodium", "Lachemilla",
                                                                         "Oreomyrhys", "Hypericum",
                                                                         "Lycopodium"), NA_real_,
                                  plant_length_cm)) %>%
  # move length to plant length for grasses
  mutate(plant_length_cm = if_else(!is.na(length_cm) & genus %in% c("Carex", "Paspalum"),
                                   length_cm, plant_length_cm)) %>%
  # remove length for vaccinium
  mutate(length_cm = if_else(!is.na(length_cm) & genus %in% c("Vaccinium", "Carex", "Paspalum"),
                             NA_real_, length_cm)) %>%

  mutate(taxon = paste(genus, species, sep = "_")) %>%
  # Fix wrong values
  mutate(wet_mass_g = if_else(id == "BXD6709", 0.109, wet_mass_g),
         wet_mass_g = if_else(id == "BEF1022", 0.281, wet_mass_g)) %>%
  # Wet mass flags
  mutate(wetflag = if_else(id %in% c("BDN3235", "CXX4125"), "Outlier_very_large_leaf", NA_character_)) %>%
  # add treatment (C, B, BB) for samples
  mutate(experiment = case_when(# QUE should be BB
                                id == "AQA0121" ~ "BB",
                                id == "BJT8862" ~ "BB",
                                #WAY sites - all should be C
                                id == 'CWD5069' ~ "C",
                                id == 'CML0422' ~ "C",
                                id == 'CMP2483' ~ "C",
                                str_detect(project,"sean") ~ "OFF-PLOT",
                                TRUE ~ experiment))

# Check dupes

trait_2020 %>%
  get_dupes()

trait_2020 %>%
  add_count(id) %>%
  distinct(n)

# Check valid IDs ---------------------------------------------------------

setdiff(select(trait_2020, hashcode = id), all_codes2)

# Leaf Area ---------------------------------------------------------------
# Import raw data files ---------------------------------------------------

read_plus <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

leafarea.raw  <- list.files(path = "data/",
                            pattern = "LeafArea",
                            full.names = T) %>%
  map_df(~read_plus(.)) %>%
  clean_names()

leafarea.raw <- leafarea.raw %>%
  mutate(filename = str_remove(filename, ".*/"),
         filename = str_remove(filename, ".csv")) %>%
  filter(!str_detect(id, "tif")) %>%
  mutate(id = str_remove(id, "[0-9]*$"),
         id = str_remove(id, ".jpeg.txt.Area"),
         id = case_when(
           id == "BVV6844 2" ~ "BVV6844",
           id == "CXX4125_1" ~ "CXX4125",
           id == "CXX4125_2" ~ "CXX4125",
           TRUE ~ id
         )) %>%
  filter(!id %in% c("2020-03-14-743-WAY_PAR0_3",
                    "2020-03-14-743_WAY_PABO_1_AT",
                    "2020-03-14-743_WAY_PABO_2_AT",
                    "CRE5297 2",
                    "BGX6274_2",
                    "out"))


# check length of IDs ------------------------------------------

leafarea.raw %>%
  mutate(idl = str_length(id)) %>%
  filter(idl != 7)

#Select data

leafarea <- leafarea.raw %>%
  select(id, leaf_area)

# CheCk wrong IDs --------------------------------------------------------

setdiff(select(leafarea, hashcode = id), all_codes2)

leafarea <- leafarea %>%
  # Remove black areas from the scans that are not leaves
  filter(!c(id == "BVL6152" & leaf_area %in% c(0.22, 0.005, 0.006)),
         !c(id == "BWJ5879" & leaf_area %in% c(0.159, 0.006, 0.006)),
         !c(id == "BXQ0542" & leaf_area %in% c(0.02)),
         !c(id == "BXU2822" & leaf_area %in% c(0.029)),
         !c(id == "CXC6033" & leaf_area %in% c(0.023, 0.127, 86.3)),
         !c(id == "CUM5583" & leaf_area %in% c(0.105)),
         !c(id == "BVP4602" & leaf_area %in% c(0.033))) %>%
  group_by(id) %>%
  summarise(leafarea_cm2 = sum(leaf_area),
            nleafscan = n(),
            .groups = "drop") %>%
  # not sure why, but code above does not work for this leaf
  mutate(leafarea_cm2 = if_else(id == "CXC6033", 2.62, leafarea_cm2))

# Check duplicated --------------------------------------------------------

leafarea %>%
  add_count(id) %>%
  summarise(sum(n)) == dim(leafarea)[1]

leafarea %>%
  add_count(id) %>%
  distinct(n)

# No duplicates IDs!!


# Correct leaves areas ----------------------------------------------------
# Select remarks form traits data

remark <- trait_2020 %>%
  select(id, remark)

# Correct leaf area, join leaf area and remarks

leafarea <- leafarea %>%
  left_join(remark, by = "id") %>%
  # Fix missing leaf area
  mutate(leafarea_cm2 = if_else(id %in% c("AFH6927", "AUV5625", "AVZ7947"), leafarea_cm2*5/2, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("AJT3939"), leafarea_cm2*2, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("AVX8706", "AWA4195"), leafarea_cm2*5/4, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("BCP9007"), leafarea_cm2*5/1, leafarea_cm2)) %>%
  mutate(leafarea_cm2 = ifelse(remark %in% c("double leaf area", "Leaf rolled, double leaf area",
                                             "rolled leaf, double leaf area", "rolled leaves,
                                             double area", "Rolled - take double area",
                                             "Rolled leaf multiple leaf area"),
                               leafarea_cm2*2, leafarea_cm2)) %>%
  # Flag data
  mutate(areaflag = if_else(id %in% c("AFH6927", "AUV5625", "AVZ7947", "AJT3939", "AVX8706", "AWA4195", "BCP9007"), "Area estimated", NA_character_),
         areaflag = if_else(id %in% c("AFL3747", "AJS0144", "AJZ6728", "AZE5327", "BBT3329"), "Leaf too white_Area missing", areaflag),
         areaflag = if_else(id %in% c("BDN3235", "CXX4125"), "Outlier_very_large_leaf", areaflag)) %>%
  select(-remark)


# Dry Mass -------------------------------------------------------------
# Import data

dry_mass <- read_csv("data/PFTC5_Peru_2020_DryWeight.csv") %>%
  clean_names() %>%
  # Fix wrong IDs
  mutate(id = case_when(
    id == "CKT177" ~ "CKT5177",
    id == "CZJ58985" ~ "CZJ5895",
    id == "CPH129" ~ "CPH1229",
    id == "BDU230" ~ "BDU0230 ",
    id == "SEAN1" ~ "SEAN1",
    id == "CYO124" ~ "CYO0124",
    id == "CYQ36610" ~ "CYQ3610",
    id == "CPN103" ~ "CPN1403",
    id == "CPR756" ~ "CPR0756",
    id == "CJKD0965" ~ "CKD0965",
    id == "ACL820" ~ "ACL0820",
    id == "APX53583" ~ "APX5383",
    id == "AEH515" ~ "AEH3515",
    id == "CND181" ~ "CND1841",
    id == "CGI70881" ~ "CGI7081",
    id == ".BOU3717" ~ "BOU3717",
    id == "ATW0.117" ~ "ATW0117",
    id == "BDU0230 " ~ "BDU0230",
    id == "BYZ5952" ~ "BYS5952",
    id == "BYI3484" ~ "BYI3448",
    id == "CYD9931" ~ "CDY9931",
    id == "CUB6632" ~ "CBU6632",
    id == "CDF2202" ~ "CDF2200",
    id == "BXM6498" ~ "BMX6498",
    id == "CKB3030" ~ "CKB3830",
    id == "CJM5916" ~ "CJM5919",
    id == "CYZ4252" ~ "CYX4252",
    id == "CYF4710" ~ "CFY4710",
    id == "AJT2939" ~ "AJT3939",
    id == "BXY3980" ~ "BWY3980",
    id == "BWN3403" ~ "BWM3403",
    id == "AVX8703" ~ "AVX8706",
    id == "BOH6123" ~ "BOH6129",
    id == "CYB2002" ~ "CYW2002",
    id == "BYL3650" ~ "BLY3650",
    id == "ADG5859" ~ "ADG5889",
    id == "BCJ9646" ~ "BCJ9446",
    id == "AXN6446" ~ "AXN6442",
    id == "CUC9131" ~ "CUC9130",
    id == "COU6998" ~ "CUO6998",
    id == "CFE3595" ~ "CFE2595",
    id == "BDK1065" ~ "BDK7065",
    id == "CGD2629" ~ "CGD1629",
    id == "BFN8407" ~ "BFV8407",
    id == "AHZ5218" ~ "AHZ5138",
    id == "BCW3456" ~ "BWC3456",
    id == "AZE5227" ~ "AZE5327",
    id == "AFN5707" ~ "AFM5707",
    id == "BOD7266" ~ "BOB7266",
    id == "AGZ7243" ~ "BGZ7243",
    id == "EGY8223" ~ "BGY8223",
    id == "APR6509" ~ "APV6509",
    id == "VIT7207" ~ "CIT7207",
    id == "BGR0051" ~ "AGR0051",
    id == "CGE7570" ~ "CGE7470",
    id == "CBF2923" ~ "CBV2923",
    id == "AJR6842" ~ "AJR6847",
    id == "CBD3438" ~ "CBD3435",
    id == "BZQ5357" ~ "BZQ3557",
    id == "CLU4763" ~ "CLJ4763",
    TRUE ~ id
  ),
  id = str_trim(id, side = "both")) %>%
  #omit duplicated IDs, have two different values
  filter(id != "APV6509") %>%
  rename(drymassflag = observations)


# IDs without dry mass ---------------------------------------------------

dry_mass %>%
  filter(is.na(dry_weight_g))

# Trait samples with missing dry mas -------------------------------------

setdiff(select(trait_2020, hashcode = id),
        select(dry_mass, hashcode = id))

# valid ids ---------------------------------------------------------------
setdiff(select(dry_mass, hashcode = id), all_codes2)


# . -----------------------------------------------------------------------
# Join data

trait_pftc5 <- trait_2020 %>%
  left_join(leafarea, by = "id") %>%
  left_join(dry_mass, by = "id") %>%
  select(id, day, project, site, experiment, plot_id, elevation, taxon, genus, species,
         individual_nr, leaf_nr, plant_height_cm, plant_length_cm, length_cm, bulk_nr_leaves,
         wet_mass_g, dry_mass_g =  dry_weight_g, leaf_thickness_1_mm, leaf_thickness_2_mm,
         leaf_thickness_3_mm, leafarea_cm2, nleafscan, remark, notes1, notes2, wetflag,
         areaflag, drymassflag)


# Clean species names

spp_trait_dictionary_2020 <- read_csv("data/PFTC5_Peru_2020_TaxonomicDictionary.csv")


trait_pftc5 <- trait_pftc5 %>%
  left_join(spp_trait_dictionary_2020, by = "taxon") %>%
  mutate(country = "PE",
         project = "PFTC5",
         month = "March",
         year = 2020,
         leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")),
                                          na.rm = TRUE)
  ) %>%
  select(wet_mass_total_g = wet_mass_g,
         leaf_area_total_cm2 = leafarea_cm2,
         dry_mass_total_g = dry_mass_g,
         # correction - this is now the number of leafs
         nr_leaves = bulk_nr_leaves,
         #this is leaf neumber per an individual
         leaf_nr = leaf_nr,
         dry_flag = drymassflag,
         wet_flag = wetflag,
         treatment = experiment,
         name_2020 = correct_taxon,
         number_leaves_scan = nleafscan,
         leaf_area_cm2 = leafarea_cm2,
         area_flag = areaflag,
         everything(),
         -c(genus, species)) %>%
  mutate(genus = sub("[[:space:]].*", "", name_2020),
         species = str_remove(name_2020, paste0(genus, " "))) %>%
  # Cleaning trait values
  #these species are bulk species so number of leaves 'standardised to 1
  mutate(nr_leaves = ifelse(name_2020 %in% c("Baccharis genistelloides",
                                             "Lycopodium thyoides",
                                             "Lycopodium clavatum",
                                             "Hypericum andinum"), 1,
                            nr_leaves)) %>%
  # TODO - each indiv needs at least 1 leaf
  mutate(nr_leaves = ifelse(is.na(nr_leaves),
                            1, nr_leaves)) %>%
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  mutate(leaf_area_total_cm2= ifelse(genus == "Sisyrinchium", leaf_area_total_cm2 * 2, leaf_area_total_cm2),
         leaf_thickness_1_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_1_mm / 2, leaf_thickness_1_mm),
         leaf_thickness_2_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_2_mm / 2, leaf_thickness_2_mm),
         leaf_thickness_3_mm = ifelse(genus == "Sisyrinchium", leaf_thickness_3_mm / 2, leaf_thickness_3_mm)) %>%
  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves,
         dry_mass_g = dry_mass_total_g / nr_leaves,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves) %>%
  # Wet and dry mass do not make sense for these species
  # mutate(Dry_Mass_g = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Dry_Mass_g),
  #       Wet_Mass_g = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Wet_Mass_g),
  #       Leaf_Area_cm2 = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, Leaf_Area_cm2)) %>%
  # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g) %>%
  # TODO
  #Create a 'binary' bulk column - assuming these were the bulk spp. but CHECK
  mutate(bulk = ifelse(name_2020 %in% c("Baccharis genistelloides",
                                        "Lycopodium thyoides",
                                        "Lycopodium clavatum",
                                        "Hypericum andinum"),
                       "bulk",
                       NA)) %>%
  # build date
  mutate(date = paste0("2020-03-", day),
         plot_id = as.character(plot_id),
         bulk = as.character(bulk)) %>%
  #reordering columns
  select(country, project, id, year, month, date, site, treatment, plot_id,
         functional_group, family, name_2020, genus, species,
         individual_nr, plant_height_cm, nr_leaves, bulk, wet_mass_total_g,
         leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm,
         dry_mass_total_g, number_leaves_scan, leaf_area_total_cm2,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag )

# Join all data -----------------------------------------------------------

trait_data_peru <- bind_rows(trait_pftc3,
                             trait_puna,
                             trait_pftc5) %>%
  #TODO
  mutate(treatment = case_when(
    year == 2020 & site == "ACJ" & treatment == "BB" ~ "NB",
    year == 2020 & site == "WAY" & treatment == "BB" ~ "B",
    year == 2020 & site == "TRE" & treatment == "BB" ~ "B",
    year == 2020 & site == "QUE" & treatment == "BB" ~ "B",
    TRUE ~ treatment
  ))


# Export data -------------------------------------------------------------

#PFTC3
trait_pftc3 %>%
  distinct(project)

trait_pftc3 %>%
  write_csv("clean_data/PFTC3_Peru_2018_LeafTraits_clean.csv")

#PUNA PROJECT
trait_puna %>%
  distinct(project)

trait_puna %>%
  write_csv("clean_data/PunaProject_Peru_2019_LeafTraits_clean.csv")

#PFTC5
trait_pftc5 %>%
  distinct(project)

trait_pftc5 %>%
  write_csv("clean_data/PFTC5_Peru_2020_LeafTraits_clean.csv")

# PFTC3 - Puna Project - PFTC5
trait_data_peru %>%
  distinct(project)


# End of Script ----
