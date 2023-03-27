# load libraries
source("code/load_libraries.R")
library("janitor")

## Read scripts for the three traits datasets ----
source("code/coordinates.R")
source("code/clean_chemical_traits.R")
source(here::here("code/species_traits_pftc3.R")) #PFTC3 import and clean
source(here::here("code/species_traits_punaproject.R")) #Puna Project import and clean
source(here::here("code/species_traits_pftc5.R")) #PFTC5 import and clean

source("code/taxon_correction.R")

## Join all data ----

trait_data_peru <- bind_rows(trait_pftc3,
                             trait_puna,
                             trait_pftc5) %>%
  mutate(functional_group = if_else(functional_group == "Gramminoid", "Graminoid", functional_group)) %>%
  #tnrs corrections across datasets
  # left_join(genus_tnrs, by = "genus") %>%
  # left_join(species_tnrs, by = "species") %>%
  left_join(GenusDictionary2020 |>
              rename(wrong_genus = wrong,
                     genus_new = right), by = c("genus" = "wrong_genus")) |>
  left_join(SpeciesDictionary2020 |>
              rename(wrong_species = wrong,
                     species_new = right), by = c("species" = "wrong_species")) |>
  mutate(genus = if_else(!is.na(genus_new), genus_new, genus),
         species = if_else(!is.na(species_new), species_new, species),
         taxon = if_else(!is.na(genus_new), paste(genus_new, species_new, sep = " "), taxon),
         season = if_else(month %in% c("July", "November"), "dry_season", "wet_season")) %>%
  select(-genus_new, -species_new) %>%

  # make data long
  pivot_longer(cols = plant_height_cm:dc13_permil, names_to = "trait", values_to = "value") %>%
  filter(!is.na(value)) %>%

  mutate(flag = case_when(wet_flag == "Outlier_very_large_leaf" ~ "outlier",
                          area_flag == "Outlier_very_large_leaf" ~ "outlier",
                          area_flag == "Leaf too white_Area missing" ~ "scanning_too_white",
                          area_flag == "Area estimated" ~ "area_estimated")) %>%
  select(-area_flag, -dry_flag, -wet_flag) %>%
  left_join(coordinates, by = c("site", "treatment", "plot_id")) %>%
  # not including flag, because the trait values are fine now, removed bad trait values
  select(year, season, month, site, treatment, plot_id, individual_nr, leaf_id, id, functional_group, family, taxon, trait, value, burn_year:longitude, course)

## Export data ----

# make new folder
dir.create("clean_data")

trait_data_peru %>%
  write_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean_newAH.csv")


# End of Script ----

# # check TNRS
library("TNRS")
dat <- trait_data_peru %>%
  distinct(taxon) %>%
  arrange(taxon) %>%
  rownames_to_column()
results_t <- TNRS(taxonomic_names = dat)
results_t %>% View()
results_t %>%
  filter(Taxonomic_status == "Synonym")
# Agrostis haenkeana -> Polypogon exasperatus
# Cyrtochilum mystacinum -> Cyrtochilum aureum
# Lucilia kunthiana -> Belloa kunthiana
results %>%
  filter(Taxonomic_status == "") # not relevant

# check the Plant List
library("Taxonstand")
sp_check_t <- TPL(dat$taxon)
sp_check_t %>% filter(Taxonomic.status == "Synonym")
sp_check_t %>% filter(Taxonomic.status == "Unresolved") # not relevant
sp_check_t %>% filter(Taxonomic.status == "") # not relevant
