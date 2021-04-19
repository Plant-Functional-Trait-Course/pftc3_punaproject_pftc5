#### China Data paper

##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
#library("gridExtra")
library("lubridate")
library("patchwork")

theme_set(theme_bw())


## ----DiversityPlot
# Richness, evenness etc.
species_cover <- read_csv(file = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv")

## Calculate responses
diversity_index <- species_cover %>%
  group_by(year, month, site, elevation, treatment, plot_id) %>%
  summarise(richness = n(),
            diversity = diversity(cover),
            evenness = diversity/log(richness),
            sum_cover = sum(cover),
            graminoid_prop = sum(cover[functional_group == "Gramminoid"])/sum_cover,
            forb_prop = sum(cover[functional_group == "Forb"])/sum_cover,
            wood_prop = sum(cover[functional_group == "Woody"])/sum_cover) %>%
  pivot_longer(cols = c(richness:wood_prop), names_to = "index", values_to = "value")

diversity_index %>%
  filter(treatment %in% c("C", "B")) %>%
  ggplot(aes(x = elevation, y = index, colour = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x") +
  facet_wrap( ~ index, scales = "free") +
  theme_minimal()




## ----TraitDistribution
# Trait distributions
traits_leaf <- read_csv(file = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv")
# traits_chem <- read_csv(file = "clean_data/")

#trait_data <- traits_leaf %>% bind_rows(traits_chem)

trait_data <- traits_leaf %>%
  mutate(value_trans = if_else(trait %in% c("dry_mass_g", "leaf_area_cm2", "plant_height_cm", "wet_mass_g"), log(value), value),
         trait_trans = case_when(trait == "dry_mass_g" ~ "dry_mass_g_log",
                           trait == "wet_mass_g" ~ "wet_mass_g_log",
                           trait == "leaf_area_cm2" ~ "leaf_area_cm2_log",
                           trait == "plant_height_cm" ~ "plant_height_cm_log",
                           TRUE ~ trait))


trait_distibution <- ggplot(trait_data, aes(x = value_trans, fill = site)) +
  geom_density(alpha = 0.4) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~ trait, scales = "free") +
  theme_minimal()
trait_distibution


## ----TraitChecks
wet_vs_dry <- trait_data %>%
  select(-trait, -value) %>%
  filter(trait_trans %in% c("wet_mass_g_log", "dry_mass_g_log")) %>%
  pivot_wider(names_from = trait_trans, values_from = value_trans) %>%
  ggplot(aes(x = wet_mass_g_log, y = dry_mass_g_log, colour = treatment)) +
  geom_point(alpha = 0.3) +
  scale_colour_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ course) +
  theme_minimal()


area_vs_dry <- trait_data %>%
  select(-trait, -value) %>%
  filter(trait_trans %in% c("leaf_area_cm2_log", "dry_mass_g_log")) %>%
  pivot_wider(names_from = trait_trans, values_from = value_trans) %>%
  ggplot(aes(x = leaf_area_cm2_log, y = dry_mass_g_log, colour = treatment)) +
  geom_point(alpha = 0.3) +
  scale_colour_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ course) +
  theme_minimal()

trait_checks <- wet_vs_dry + area_vs_dry + patchwork::plot_layout(guides = "collect")
trait_checks



# ## ----Stuff
# traitsLong %>%
#   filter(!is.na(Value)) %>%
#   group_by(Traits) %>%
#   summarise(min = min(Value, na.rm = TRUE), max = max(Value, na.rm = TRUE))
#
# traitsLeaf %>%
#   group_by() %>%
#   summarise(min(Leaf_Thickness_Ave_mm, na.rm = TRUE), max(Leaf_Thickness_Ave_mm, na.rm = TRUE))
#
#
# ## ----OtherStuff
# # How many leaves
# traitsLeaf %>%
#   mutate(Wet_Mass_g.log = log(Wet_Mass_g),
#          Dry_Mass_g.log = log(Dry_Mass_g),
#          Leaf_Thickness_Ave_mm.log = log(Leaf_Thickness_Ave_mm),
#          Leaf_Area_cm2.log = log(Leaf_Area_cm2)) %>%
#   select(Date, Elevation, Site, Treatment, Taxon, Individual_number, Leaf_number, Wet_Mass_g.log, Dry_Mass_g.log, Leaf_Thickness_Ave_mm.log, Leaf_Area_cm2.log, SLA_cm2_g, LDMC) %>%
#   gather(key = Traits, value = Value, -Date, -Elevation, -Site, -Treatment, -Taxon, -Individual_number, -Leaf_number) %>%
#   filter(!is.na(Value)) %>%
#   #group_by(Treatment) %>%
#   summarise(n())
# # Total: 33'314
#
# traitsLeaf %>%
#   filter(Treatment != "LOCAL") %>%
#   group_by(Taxon) %>%
#   summarise(n())
#
#
# traitsChem %>%
#   select(Elevation, Site,  Treatment, Taxon, StoichLabel, P_percent, C_percent, N_percent, CN_ratio, dN15_percent, dC13_percent) %>%
#   gather(key = Trait, value = Value, C_percent, N_percent, CN_ratio, dN15_percent, dC13_percent, P_percent) %>%
#   filter(!is.na(Value)) %>%
#   group_by(Treatment) %>%
#   summarise(n())
# # Total obs: 3429
#
# # Nr leaves: 6671
# # Nr trait observations: 33314 + 3429 = 36743
#
# traitsChem %>%
#   group_by(Treatment) %>%
#   summarise(n())
# # Total leaves: 576, gradient: 265, experiment: 311
#
# traitsLeaf %>% distinct(Taxon)
# # Taxon: 193

## ----Climate figure

# Read in Tomst data
#temp <- read_csv(file = "climate/data_cleaned/China_2019_Climate_TomstLogger.csv")


# TomstOTC <- temp %>%
#   group_by(YearMonth, Variable, Treatment) %>%
#   summarise(MeanTemperature = mean(Temperature), SETemperature = sd(Temperature)/sqrt(n())) %>%
#   ungroup() %>%
#   mutate(Variable = str_remove(Variable, "Temperature")) %>%
#   ggplot(aes(x = YearMonth, y = MeanTemperature, ymin = MeanTemperature - SETemperature, ymax = MeanTemperature + SETemperature, colour = Treatment)) +
#   scale_colour_manual(name = "Treatment", values = c("grey", "purple"), labels=c("Control", "OTC")) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(width = 0.1) +
#   scale_x_date(date_labels = "%b", breaks = ymd(c("2019-09-15", "2019-10-15", "2019-11-15"))) +
#   labs(x = "", y = "Mean temperautre in °C") +
#   facet_wrap(~ Variable) +
#   theme(legend.position="top")



ClimatePlot <- AirTempPlot / (iButtonPlot + TomstOTC) + plot_annotation(tag_levels = "a")
ClimatePlot
#ggsave(ClimatePlot, filename = "ClimatePlot.pdf", height = 10, width = 10, dpi = 300)






## ----SpList
### SPECIES TABLE
biomass <- read_csv(file = "biomass/China_2016_Biomass_cleaned.csv")
traitsLeaf <- read_csv(file = "traits/data_cleaned/PFTC1.2_China_2015_2016_LeafTraits.csv")


spList <- cover_thin %>% distinct(speciesName) %>%
  mutate(Dataset = "community") %>%
  rbind(biomass %>% select(speciesName) %>%
          distinct() %>%
          mutate(Dataset = "biomass")) %>%
  rbind(traitsLeaf %>% select(Taxon) %>%
          rename("speciesName" = "Taxon") %>%
          distinct() %>%
          mutate(Dataset = "trait")) %>%
  mutate(Presence = "x") %>%
  pivot_wider(names_from = Dataset, values_from = Presence) %>%
  arrange(speciesName) %>%
  filter(!grepl("*Unkown|*mixed forb species|spp$|spp.$|sp$|sp1$|sp2$|sp4$", speciesName))
spList
#writexl::write_xlsx(spList, path = "China_FullSpeciesList.xlsx")

taxa %>% select(speciesName) %>%
  mutate(Dataset = "community") %>%
  rbind(biomass %>% select(speciesName) %>%
          distinct() %>%
          mutate(Dataset = "biomass")) %>%
  rbind(traitsLeaf %>% select(Taxon) %>%
          rename("speciesName" = "Taxon") %>%
          distinct() %>%
          mutate(Dataset = "trait")) %>%
  mutate(Presence = "x") %>%
  pivot_wider(names_from = Dataset, values_from = Presence) %>%
  arrange(speciesName) %>%
  filter(grepl("*Unkown|*mixed forb species|spp$|spp.$|sp$|sp1$|sp2$|sp4$", speciesName)) %>%
  gather(key = dataset, value = present, -speciesName) %>%
  filter(present == "x") %>%
  group_by(dataset) %>%
  count()


airtemp <- read_csv(file = "climate/data_cleaned/China_2013_2016_AirTemp.csv", col_names = TRUE)

# GDD
airtemp %>%
  mutate(year = year(dateTime),
         month = month(dateTime),
         day = day(dateTime)) %>%
  filter(year %in% c(2013)) %>%
  group_by(site, year, month, day) %>%
  summarise(mean = mean(Tair)) %>%
  filter(mean >= 5) %>%
  group_by(year, site) %>%
  summarise(n = n())

# Freezing days
airtemp %>%
  mutate(year = year(dateTime),
         month = month(dateTime),
         day = day(dateTime)) %>%
  filter(year %in% c(2013)) %>%
  group_by(site, year, month, day) %>%
  summarise(mean = mean(Tair)) %>%
  filter(mean < 0) %>%
  group_by(year, site) %>%
  summarise(n = n())


# Table 4
tbl(con, "turfEnvironment") %>%
  collect() %>%
  pivot_longer(cols = c(moss:litterThickness), names_to = "group", values_to = "value") %>%
  group_by(group) %>%
  summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)) %>%
  print(n = Inf)


tbl(con, "turfEnvironment") %>%
  collect() %>%
  pivot_longer(cols = c(moss:litterThickness), names_to = "group", values_to = "value") %>%
  ggplot(aes(x = factor(year), y = value)) +
  geom_boxplot() +
  facet_wrap(~ group, scales = "free")
