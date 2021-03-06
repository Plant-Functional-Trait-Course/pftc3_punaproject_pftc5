# Load libraries ----------------------------------------------------------
source("code/load_libraries.R")
library("janitor")

source("code/coordinates.R")
source("code/tnrs_corrections.R")
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
         species = str_remove(name_2020, paste0(genus, " "))) %>%
  select(site, treatment, plot_id, functional_group, family, genus, species,
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
         species = str_remove(name_2020, paste0(genus, " "))) %>%
  select(month, site, treatment, plot_id, functional_group, family, genus, species,
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
         project = "Puna",
         treatment = if_else(site == "TRE" & treatment == "B", "NB", treatment)) %>%
  select(year, project, everything())


# Here is the data for PFTC3 and PunaProject, merged

species_cover_2018_2019 <- bind_rows(spp_clean_2018, spp_clean_2019) %>%
  mutate(cover = if_else(project == "Puna" & month == "April" & site == "WAY" & treatment == "C" & plot_id == 1 & genus == "Gnaphalium", 1, cover)) %>%
  rename(course = project)



# Checks
#import new corrections
# new_corrections <- read_excel(path = "data/species_cover_pftc_puna - corregido_LVB.xlsx") %>%
#   rename(course = project) %>%
#   mutate(treatment = if_else(site == "QUE", "B", treatment))

# anti_join with species_cover
# new_corrections %>% anti_join(species_cover_2018_2019, by = c("year", "course", "month", "site", "plot_id", "taxon", "cover"))
#treatment is wrong for TRE in the new_corrections, but when excluding treatment from the anti_join, everything matches! this is ok.

# species_cover_2018_2019 %>% anti_join(new_corrections, by = c("year", "month", "course", "site", "plot_id", "functional_group", "family", "genus", "species", "taxon")) %>% as.data.frame()
# 2 species (Achyrocline ramosissima, Carex sp8) that are missing in the new corrections.


# Convert to wide format for comparing species cover side by side by month
presence_absence_peru <- species_cover_2018_2019 %>%
  select(-c(year, course, family, genus, species)) %>%
  mutate(month = factor(month, levels = c("March", "April", "July", "November"))) %>%
  complete(month,
           nesting( site, treatment, plot_id, functional_group, taxon),
           fill = list(cover = 0)) %>%
###### Cover equal to 0 meaning that the specie where not registered
  pivot_wider(names_from = month,
              values_from = cover) %>%
  arrange(site, treatment, plot_id, functional_group, taxon)

presence_absence_peru

# 2020 PFTC5 ------------------------------------------------------------------

#' Species cover from 2020 PFTC5

spp_cover_2020 <- read_csv("data/PFTC5_2020_CommunityCover_raw.csv")  %>%

  ## edit characters in and around _cf_
  mutate(taxon = str_replace_all(taxon, "_", " ")) %>%
  mutate(taxon = str_replace(taxon, "cf", "cf.")) %>%

  ##taxanomic corrections
  mutate(taxon = case_when(
    #misspelling
    taxon == "Viola pymaea" ~ "Viola pygmaea",
    #misspelling
    taxon == "Agrostis trichoides" ~ "Agrostis trichodes",
    taxon == "Agrostis perenans" ~ "Agrostis perennans",
    #misspelling - CHECK
    taxon == "Lysipomia glanulifera" ~ "Lysipomia glandulifera",
    #misspelling - CHECK
    taxon == "Chusquea intipacarina" ~ "Chusquea intipaqariy",
    #misspelling - CHECK
    taxon == "Carex bomplandii" ~ "Carex bonplandii",
    #punctuation error
    taxon == "Hieracium cf.. mandonii" ~ "Hieracium cf. mandonii",
    #punctuation error
    taxon == "Diplostephium cf.. haenkei" ~ "Diplostephium cf. haenkei",
    #species change - was checked with Lucely
    taxon == "Calamagrostis cf.. macrophylla" ~ "Calamagrostis cf. amoena",
    #species change - was checked with Lucely
    taxon == "Geranium filipes" ~ "Geranium sessiliflorum",
    #species change - was checked with Lucely
    taxon == "Jamesonia alstonii" ~ "Jamesonia blepharum",
    #species change - was checked with Lucely
    taxon == "Bartsia inaequalis" ~ "Bartsia trichophylla",
    #species change - was checked with Lucely
    taxon == "Calamagrostis sp8" ~ "Anatherostipa hans-meyeri",
    #species change - was checked with Lucely
    taxon == "Calamagrostis sp7" ~ "Anatherostipa hans-meyeri",
    #species change - was checked with Lucely
    taxon == "Oreobolus sp1" ~ "Oreobolus goeppingeri",
    #species change - was checked with Lucely
    taxon == "Festuca 3 sharp" ~ "Festuca sp4",
    #species change - was checked with Lucely
    taxon == "Agrostis sp1" ~ "Agrostis trichodes",
    #species change - was checked with Lucely
    taxon == "Calamagrostis tricophylla" ~ "Calamagrostis cf. amoena",
    # typo
    taxon == "Agrostis sedlings" ~ "Agrostis seedling",
    taxon == "Rhynchospora seedlings" ~ "Rhynchospora seedling",
    TRUE ~ taxon)) %>%

  ##TIDY COLUMNS TO MATCH osf COLUMNS
  # split taxon into genus and species
  separate(taxon,
           into = c("genus", "species"),
           sep = "\\s", 2,
           remove = FALSE,
           # keeps cf. and species taxon together
           extra = "merge") %>%
  #create treatment and plot_id
  separate(plot,
           into = c("treatment", "plot_id"),
           #sperates before last character
           sep = -1, 2,
           remove = TRUE) %>%
  #add month
  mutate(month = rep("March",
                     nrow(.)),
         #add project
         course = rep("PFTC5",
                       nrow(.))) %>%
  #change '+' in cover to 0.5 and make numeric
  mutate(cover = as.numeric(case_when(cover == "+" ~ "0.5",
                                      TRUE ~ cover))) %>%
  # drop absent species (no cover value)
  filter(!cover == "")  %>%
  #add other cols from osf dataset - taxon, functional group and family
  left_join(.,
            species_cover_2018_2019  %>%
              select(taxon, functional_group, family),
            by = 'taxon') %>%

  #REMOVE ANY DUPLICATES
  distinct() %>%

  #ADDING FAMILY AND FUNCTIONAL GROUP FOR Festuca cf. andina.
  mutate(
    functional_group = case_when(
      taxon == "Festuca cf. andina" ~ "Gramminoid",
      taxon == "Bartsia trichophylla" ~ "Forb",
      taxon == "seedling unknown" ~ NA_character_,
      TRUE ~ functional_group
    ),
    family = case_when(
      taxon == "Festuca cf. andina" ~ "Poaceae",
      taxon == "Bartsia trichophylla" ~ "Orobanchaceae",
      TRUE ~ family
    )) %>%

  # fix treatment and stuff
  mutate(treatment = if_else(treatment == "BB", "NB", treatment)) %>%

  ##SELECT ONLY COLUMNS THAT ARE IN THE osf DATA
  select(year, course, month, site, treatment, plot_id, functional_group, family, genus, species, taxon, cover) %>%

  # REMOVE WHERE COVER = 0 i.e. absent
  filter(cover > 0) %>%

  # sum cover for the same species in the same plot
  group_by(across(c(-cover))) %>%
  summarise(cover = sum(cover, na.rm = TRUE))

#Adding 2020 species

species_cover <- bind_rows(species_cover_2018_2019, spp_cover_2020) %>%
  mutate(plot_id = as.character(plot_id),
         functional_group = if_else(functional_group == "Gramminoid", "Graminoid", functional_group)) %>%
  #tnrs corrections across datasets
  mutate(genus = case_when(taxon == "Agrostis haenkeana" ~ "Polypogon",
                           taxon == "Cyrtochilum mystacinum" ~ "Cyrtochilum",
                           taxon == "Lucilia kunthiana" ~ "Belloa",
                           TRUE ~ genus),
         species = case_when(taxon == "Agrostis haenkeana" ~ "exasperatus",
                             taxon == "Cyrtochilum mystacinum" ~ "aureum",
                             taxon == "Lucilia kunthiana" ~ "kunthiana",
                             TRUE ~ species),
         taxon = case_when(taxon == "Agrostis haenkeana" ~ "Polypogon exasperatus",
                           taxon == "Cyrtochilum mystacinum" ~ "Cyrtochilum aureum",
                           taxon == "Lucilia kunthiana" ~ "Belloa kunthiana",
                           TRUE ~ taxon)) %>%
  # join coordinates
  left_join(coordinates, by = c("site", "treatment", "plot_id")) %>%
  mutate(season = if_else(month %in% c("July", "November"), "dry_season", "wet_season")) %>%
  select(year, season, month, site, treatment, plot_id, family, functional_group, taxon, cover, burn_year:longitude, course)

# Export ------------------------------------------------------------------

# make new folder
dir.create("clean_data")

species_cover %>%
  write_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv")

# End of Script ----



# Check
# trait_data_peru %>%
#   distinct(course, site, treatment, plot_id, taxon) %>%
#   anti_join(species_cover %>% distinct(course, site, treatment, plot_id, taxon),
#             by = c("treatment", "site", "taxon")) %>% View()



# Check turf maps
species_cover %>%
  mutate(month_year = paste(month, year, sep = "_")) %>%
  filter(site == "ACJ", treatment == "C") %>%
  ggplot(aes(x = month_year, y = 1, fill = cover)) +
  geom_tile() +
  labs(y = "") +
  scale_fill_viridis_c() +
  facet_grid(taxon ~ plot_id) +
  theme(strip.text.y = element_text(angle = 360),
        axis.text.y = element_blank())

