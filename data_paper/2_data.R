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

species_cover |>
  group_by(year, site, treatment, plot_id) |>
  summarise(n = n()) |>
  ungroup() |>
  summarise(mean(n))

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

# run model
diversity_index %>%
  ungroup() |>
  filter(treatment %in% c("C", "B", "NB"),
         index %in% c("richness", "diversity", "evenness")) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB"))) %>%
  group_by(index) %>%
  nest(data = -c(index)) %>%
  mutate(model = map(data, ~lme(value ~ elevation * treatment, random = ~1|plot_id, data = .x)),
         result = map(model, tidy)) %>%
  unnest(result)


### Vegetation height and structure
comm_structure <- read_csv("clean_data/PFTC3-Puna-Peru_2018-2019_CommunityStructure_clean.csv")

comm_structure %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC"))) %>%
  filter(variable %in% c("bryophyte_depth", "median_height")) %>%
  group_by(site, variable) %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()))


dd <- comm_structure %>%
  filter(variable == "median_height")

fit <- lme(value ~ elevation * treatment, random = ~1|plot_id, data = dd)
summary(fit)

### Biomass
biomass <- read_csv("clean_data/Puna_Peru_2019_Biomass_clean.csv")

biomass %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE"))) %>%
  filter(variable == "biomass", variable_class == "total")

biomass %>%
  filter(variable == "biomass",
         variable_class %in% c("graminoids")) %>%
  nest(data = -c(variable_class)) %>%
  mutate(model = map(data, ~lme(value ~ elevation * treatment, random = ~1|plot_id, data = .x)),
         result = map(model, tidy)) %>%
  unnest(result)


### Traits
traits <- read_csv(file = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv")

trait_data <- traits %>%
  mutate(value_trans = if_else(trait %in% c("dry_mass_g", "leaf_area_cm2", "plant_height_cm", "wet_mass_g"), log(value), value),
         trait_trans = case_when(trait == "dry_mass_g" ~ "dry_mass_g_log",
                                 trait == "wet_mass_g" ~ "wet_mass_g_log",
                                 trait == "leaf_area_cm2" ~ "leaf_area_cm2_log",
                                 trait == "plant_height_cm" ~ "plant_height_cm_log",
                                 TRUE ~ trait)) %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC")),
         treatment = factor(treatment, levels = c("C", "B", "NB", "BB")))


trait_data %>% distinct(taxon)
trait_data %>% distinct(id)

trait_data %>%
  group_by(site) %>%
  distinct(id) %>% count()

trait_data %>%
  group_by(treatment) %>%
  distinct(id) %>% count()

# of which chem traits
trait_data |> filter(grepl("percent|permil|ratio", trait)) |> distinct(id)
trait_data |> filter(grepl("percent|permil|ratio", trait)) |> distinct(taxon)


library(traitstrap)
species_cover <- species_cover %>%
  select(year, season, month, site, treatment, plot_id, taxon, cover) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB", "BB")),
         treatment2 = treatment)
traits <- trait_data %>%
  mutate(treatment2 = treatment) %>%
  select(year, season, month, site, treatment, plot_id, taxon, trait, value_trans, treatment2)
peru_trait_impute <- trait_fill(comm = species_cover,
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

fortify(peru_trait_impute) |>
  ungroup() |>
  #filter(Trait == "CN_ratio") |>
  complete(.id, level, trait, fill = list(s = 0)) |>
  filter(level == "plot_id") |>
  group_by(trait) |>
  # prob = 0.25 gives 75% of the plots
  # also run prob = 0.5 for 50% of the plots
  summarise(q = quantile(s, prob = 0.5))


# climate
climate <- read_csv("clean_data/PFTC3_Puna_PFTC5_2019_2020_Climate_clean.csv")

climate |>
  group_by(site, treatment) |>
  summarise(air = mean(air_temperature))
