#### China Data paper

##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
#library("gridExtra")
library("lubridate")
library("patchwork")

# colours
source("data_paper/puna_colour_palette.R")

# set theme
theme_set(theme_bw())


## ----DiversityPlot
Gradient_plot_data <- diversity_index %>%
  filter(index %in% c("richness", "evenness")) %>%
  rename(variable = index) %>%

  # add community structure data
  bind_rows(
    comm_structure %>%
      filter(variable %in% c("cover", "median_height", "bryophyte_depth"),
             variable_class %in% c("forbs", "graminoids", "shrub", "litter", "vegetation", "bryophytes")) %>%
      mutate(variable = paste(variable, variable_class, sep = "_")) %>%
      select(-variable_class) %>%
      filter(variable != "cover_bryophytes")
  ) %>%
  # add biomass data
  bind_rows(
    biomass %>%
      filter(variable == "biomass",
             variable_class == "total") %>%
      select(-variable_class)
  ) %>%
  ungroup() %>%
  filter(treatment %in% c("C", "B", "NB")) %>%
  mutate(variable = case_when(variable == "cover_forbs" ~ "forb cover",
                              variable == "cover_graminoids" ~ "graminoid cover",
                              variable == "cover_shrub" ~ "shrub cover",
                              variable == "cover_litter" ~ "litter cover",
                              variable == "median_height_vegetation" ~ "vegetation height",
                              variable == "bryophyte_depth_bryophytes" ~ "bryophyte depth",
                              variable == "biomass" ~ "total biomass",
                              TRUE ~ variable),
         variable = factor(variable, levels = c("richness", "evenness", "forb cover", "graminoid cover", "shrub cover", "litter cover", "total biomass", "vegetation height", "bryophyte depth")),
         treatment = factor(treatment, levels = c("C", "B", "NB")))


res_GP <- Gradient_plot_data %>%
  nest(data = -c(variable)) %>%
  mutate(model = map(data, ~lm(value ~ elevation * treatment, data = .x)),
         result = map(model, tidy)) %>%
  unnest(result) %>%
  select(variable, term:p.value) %>%
  filter(term %in% c("elevation", "elevation:treatmentB", "elevation:treatmentNB")) %>%
  mutate(treatment = case_when(term == "elevation" ~ "C",
                               term == "elevation:treatmentB" ~ "B",
                               term == "elevation:treatmentNB" ~ "NB"))

Gradient_plot <- Gradient_plot_data %>%
  left_join(res_GP %>%
              select(variable, treatment, p.value),
            by = c("variable", "treatment")) %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB"))) %>%
  ggplot(aes(x = elevation, y = value, colour = treatment, linetype = p.value < 0.05, fill = treatment)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.2) +
  scale_colour_manual("Treatment", values = puna_treatment_colour$colour[1:3]) +
  scale_fill_manual("Treatment", values = puna_treatment_colour$colour[1:3]) +
  #scale_colour_viridis_d(option = "plasma", end = 0.8, direction = -1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Elevation m a.s.l", y = "") +
  guides(linetype = FALSE,
         fill = FALSE,
         colour = guide_legend(override.aes = list(fill = NA))) +
  facet_wrap( ~ variable, scales = "free_y",
              labeller = labeller(.default = capitalise))
Gradient_plot <- Gradient_plot + figure_theme
ggsave("Gradient_plot.jpeg", Gradient_plot, dpi = 150)

## ----NMDSOrdination
NMDS_ordination <- fNMDS %>%
  as_tibble() %>%
  mutate(treatment = factor(treatment, levels = c("C", "B", "NB", "BB")),
         site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE", "OCC")),
         season = if_else(season == "dry_season",
                          "Dry season",
                          "Wet season")) %>%
  ggplot(aes(x = NMDS1, y = NMDS2, colour = site, shape = treatment)) +
  geom_point() +
  scale_colour_manual("Treatment", values = puna_site_colour$colour) +
  scale_shape_manual("Site", values=c(16, 5, 6, 8)) +
  #scale_colour_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ season)
NMDS_ordination <- NMDS_ordination + figure_theme
ggsave("NMDS_ordination.jpeg", NMDS_ordination, dpi = 150, height = 5, width = 9)

## ----TraitDistribution
trait_distibution <- trait_data %>%
  mutate(trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                   trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                   trait == "ldmc" ~ "LDMC",
                   trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)",
                   trait == "wet_mass_g" ~ "Wet~mass~(g)",
                   trait == "dry_mass_g" ~ "Dry~mass~(g)",
                   trait == "leaf_area_cm2" ~ "Leaf~area~(cm^{2})")) %>%
  mutate(trait = factor(trait,
                        levels = c("Plant~height~(cm)", "Wet~mass~(g)", "Dry~mass~(g)",
                                   "Leaf~area~(cm^{2})", "Leaf~thickness~(mm)", "LDMC",
                                   "SLA~(cm^{2}/g)"))) %>%
  ggplot(aes(x = value_trans, fill = site, colour = site)) +
  geom_density(alpha = 0.5) +
  geom_density(alpha = 0.5, fill = NA) +
  scale_fill_manual("Site",
                    values = puna_site_colour$colour) +
  scale_colour_manual("Site",
                      values = puna_site_colour$colour) +
  #scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~ trait, scales = "free",
             labeller = label_parsed) +
  labs(x = "", y = "Density")
trait_distibution <- trait_distibution + figure_theme
ggsave("Trait_distribution.jpeg", trait_distibution, dpi = 150)


## ----TraitChecks
wet_vs_dry <- trait_data %>%
  select(-trait, -value) %>%
  filter(trait_trans %in% c("wet_mass_g_log", "dry_mass_g_log")) %>%
  pivot_wider(names_from = trait_trans, values_from = value_trans) %>%
  ggplot(aes(x = wet_mass_g_log, y = dry_mass_g_log, colour = treatment)) +
  geom_point(alpha = 0.3) +
  scale_colour_manual("Treatment",
                      values = puna_treatment_colour$colour) +
  #scale_colour_viridis_d(option = "plasma", end = 0.8, direction = -1) +
  labs(x = "log(wet mass g)", y = "log(dry mass g)", tag = "a)") +
  figure_theme


area_vs_dry <- trait_data %>%
  select(-trait, -value) %>%
  filter(trait_trans %in% c("leaf_area_cm2_log", "dry_mass_g_log")) %>%
  pivot_wider(names_from = trait_trans, values_from = value_trans) %>%
  ggplot(aes(x = leaf_area_cm2_log, y = dry_mass_g_log, colour = treatment)) +
  geom_point(alpha = 0.3) +
  scale_colour_manual("Treatment",
                      values = puna_treatment_colour$colour) +
  #scale_colour_viridis_d(option = "plasma", end = 0.8, direction = -1) +
  labs(x = bquote('log(leaf area '*cm^2*')'), y = "", tag = "b)") +
  figure_theme

trait_checks <- wet_vs_dry + area_vs_dry + patchwork::plot_layout(guides = "collect")
trait_checks
ggsave("Trait_checks.jpeg", trait_checks, dpi = 150)



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
