# Coordinates
source("code/coordinates.R")

coordinates %>%
  distinct(site, treatment, burn_year)

coordinates %>%
  group_by(site, treatment) %>%
  summarise(elev = mean(elevation, na.rm = TRUE),
            elev_se = sd(elevation, na.rm = TRUE)/sqrt(n()),
            lat = mean(latitude, na.rm = TRUE),
            lat_se = sd(latitude, na.rm = TRUE)/sqrt(n()),
            long = mean(longitude, na.rm = TRUE),
            long_se = sd(longitude, na.rm = TRUE)/sqrt(n()))

# Richness, evenness etc.
species_cover <- read_csv(file = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv")

# nr taxa
species_cover %>% distinct(taxon)

# nr observations
dim(species_cover)

## Calculate responses
diversity_index <- species_cover %>%
  group_by(year, month, site, elevation, treatment, plot_id) %>%
  summarise(richness = n(),
            diversity = diversity(cover),
            evenness = diversity/log(richness),
            sum_cover = sum(cover),
            graminoid_proportion = sum(cover[functional_group == "Graminoid"])/sum_cover,
            forb_proportion = sum(cover[functional_group == "Forb"])/sum_cover,
            woody_proportion = sum(cover[functional_group == "Woody"])/sum_cover) %>%
  pivot_longer(cols = c(richness:woody_proportion), names_to = "index", values_to = "value")

# overall richness
diversity_index %>%
  filter(index == "richness") %>%
  group_by() %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()))

# richness per treatment
diversity_index %>%
  filter(index == "richness") %>%
  group_by(treatment) %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()))

diversity_result <- diversity_index %>%
  filter(treatment %in% c("C", "B", "NB")) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB"))) %>%
  group_by(index) %>%
  nest(data = -c(index)) %>%
  mutate(model = map(data, ~lm(value ~ elevation * treatment, data = .x)),
         result = map(model, tidy)) %>%
  unnest(result)

dd <- diversity_index %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB"))) %>%
  filter(treatment %in% c("C", "B", "NB"),
         index == "richness")
summary(lm(value ~ elevation * treatment, dd))


# NMDS ORDINATION
cover_fat <- species_cover %>%
  select(-family, -functional_group, -c(burn_year:course)) %>%
  arrange(year, season, month) %>%
  pivot_wider(names_from = "taxon", values_from = "cover", values_fill = 0) %>%
  ungroup()

cover_fat_spp <- cover_fat %>% select(-(year:plot_id))

set.seed(32)
NMDS <- metaMDS(cover_fat_spp,
                noshare = TRUE,
                try = 30,
                trace = 0) #DNC
# Kontrollene vekt 0

fNMDS <- fortify(NMDS) %>%
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(year:plot_id))



### Vegetation height and structure
comm_structure <- read_csv("clean_data/PFTC3-Puna-Peru_2018-2019_CommunityStructure_clean.csv")

comm_structure %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC"))) %>%
  filter(variable %in% c("bryophyte_depth", "median_height")) %>%
  ggplot(aes(x = site, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable_class, scales = "free_y")

comm_structure %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC"))) %>%
  filter(variable %in% c("bryophyte_depth", "median_height")) %>%
  group_by(site, variable) %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()))

comm_structure %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC"))) %>%
  filter(variable == c("cover")) %>%
  ggplot(aes(x = site, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable_class, scales = "free_y")

comm_structure %>%
  filter(variable %in% c("cover", "median_height", "bryophyte_depth"),
         !variable_class %in% c("shrub_layer", "field_layer", "bottom_layer")) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB"))) %>%
  nest(data = -c(variable, variable_class)) %>%
  mutate(model = map(data, ~lm(value ~ elevation, data = .x)),
         result = map(model, glance)) %>%
  unnest(result) %>%
  filter(p.value <= 0.05) %>%
  select(variable, variable_class, statistic, p.value, df, df.residual)


### Biomass
biomass <- read_csv("clean_data/Puna_Peru_2019_Biomass_clean.csv")

biomass %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE"))) %>%
  filter(variable == "biomass", variable_class == "total")

biomass %>%
  filter(variable == "biomass") %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE"))) %>%
  ggplot(aes(x = site, y = value, colour = treatment)) +
  geom_point() +
  facet_wrap(~ variable_class, scales = "free_y")

biomass %>%
  filter(variable == "biomass") %>%
  nest(data = -c(variable_class)) %>%
  mutate(model = map(data, ~lm(value ~ elevation, data = .x)),
  #        fit = map(model, tidy)) %>%
  # unnest(fit) %>% filter(p.value < 0.05)
         result = map(model, glance)) %>%
  unnest(result) %>%
  filter(p.value <= 0.05) %>%
  select(variable_class, statistic, p.value, df, df.residual)


### Traits
traits_leaf <- read_csv(file = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv")
# traits_chem <- read_csv(file = "clean_data/")

#trait_data <- traits_leaf %>% bind_rows(traits_chem)

trait_data <- traits_leaf %>%
  mutate(value_trans = if_else(trait %in% c("dry_mass_g", "leaf_area_cm2", "plant_height_cm", "wet_mass_g"), log(value), value),
         trait_trans = case_when(trait == "dry_mass_g" ~ "dry_mass_g_log",
                                 trait == "wet_mass_g" ~ "wet_mass_g_log",
                                 trait == "leaf_area_cm2" ~ "leaf_area_cm2_log",
                                 trait == "plant_height_cm" ~ "plant_height_cm_log",
                                 TRUE ~ trait)) %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC")),
         treatment = factor(treatment, levels = c("C", "B", "NB", "BB")))


traits_leaf %>% distinct(taxon)
traits_leaf %>% distinct(id)

traits_leaf %>%
  group_by(site) %>%
  distinct(id) %>% count()

traits_leaf %>%
  group_by(treatment) %>%
  distinct(id) %>% count()


library(traitstrap)
species_cover <- species_cover %>%
  select(year, season, month, site, treatment, plot_id, taxon, cover) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB", "BB")),
         treatment2 = treatment)
traits <- trait_data %>%
  mutate(treatment2 = treatment) %>%
  select(year, season, month, site, treatment, plot_id, taxon, trait, value_trans, treatment2)
peru_trait_impute <- trait_impute(comm = species_cover,
             traits = traits,
             scale_hierarchy = c("year", "season", "month", "site", "plot_id"),
             taxon_col = "taxon",
             trait_col = "trait",
             value_col = "value_trans",
             abundance_col ="cover",
             treatment_col = "treatment",
             treatment_level = "site",
             min_n_in_sample = 5,
             other_col = "treatment2"
             )

autoplot(peru_trait_impute, other_col_how = "facet")

