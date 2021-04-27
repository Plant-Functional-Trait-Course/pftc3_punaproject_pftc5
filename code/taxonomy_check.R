# check TNRS
library("TNRS")
dat <- species_cover %>%
  distinct(taxon) %>%
  arrange(taxon) %>%
  rownames_to_column()
results <- TNRS(taxonomic_names = dat, matches = "best")
# get references from the search
metadata <- TNRS_metadata(bibtex_file = "data_paper/tnrs_citations.bib")
metadata$version
results %>% View()
results %>%
  filter(Taxonomic_status == "Synonym")
# species list to change
# Agrostis haenkeana -> Polypogon exasperatus
# Cyrtochilum mystacinum -> Cyrtochilum aureum
# Lucilia kunthiana -> Belloa kunthiana
results %>%
  filter(Taxonomic_status == "") # not relevant

# check the Plant List
library("Taxonstand")
TPL(c("Agrostis haenkeana", "Cyrtochilum mystacinum", "Lucilia kunthiana"))
sp_check <- TPL(dat$taxon)
sp_check %>% filter(Taxonomic.status == "Synonym")
sp_check %>% filter(Taxonomic.status == "Unresolved") # not relevant
sp_check %>% filter(Taxonomic.status == "") # not relevant

# check additional species from the plant list
TNRS(taxonomic_names = "Carex crinalis",matches = "all")



# make a species list

spList <- species_cover %>%
  distinct(functional_group, family, taxon) %>%
  mutate(Dataset = "community") %>%
  rbind(trait_data_peru %>%
          distinct(functional_group, family, taxon) %>%
          mutate(Dataset = "trait")) %>%
  mutate(Presence = "x") %>%
  pivot_wider(names_from = Dataset, values_from = Presence) %>%
  select(functional_group, family, taxon, community, trait) %>%
  arrange(functional_group, family, taxon)
spList

# check family names
family_check <- TNRS(spList$family)
family_check %>% filter(Family_score < 1)
# fixed these families
# Amarillidaceae Amaryllidaceae
# Euphobiaceae  Euphorbiaceae

taxon_check <- TNRS(spList$taxon)
# check typos in taxon
taxon_check %>%
  mutate(Name = c(Name_submitted == Accepted_species)) %>%
  select(Name_submitted,
         Name_matched,
         Accepted_species,
         Name,
         Name_matched_accepted_family,
         Accepted_family,
         Canonical_author,
         Accepted_name_author,
         Taxonomic_status) %>% View()
# fixing
# Senecio rhizomathus -> Senecio rhizomatus
# Bartisa tricophylla -> Bartisa trichophylla
# Agrostis perenans -> Agrostis perennans
# check typos in family
taxon_check %>%
  mutate(Name = c(Name_matched_accepted_family == Accepted_family)) %>%
  select(Name_submitted,
         Name_matched,
         Accepted_species,
         Name,
         Name_matched_accepted_family,
         Accepted_family,
         Canonical_author,
         Accepted_name_author,
         Taxonomic_status) %>% View()

dir("clean_data/")

spList %>%
  left_join(taxon_check %>%
              select(Name_submitted, Accepted_name_author), by = c("taxon" = "Name_submitted")) %>%
  rename("functional group" = "functional_group", "authority" = "Accepted_name_author") %>%
  select("functional group", family, authority, taxon, community, trait) %>%
  write_csv("clean_data/PFTC3_Puna_PFTC5_Peru_2018_2020_Full_species_list.csv")

