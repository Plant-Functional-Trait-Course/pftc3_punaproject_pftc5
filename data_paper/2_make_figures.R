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
  filter(index %in% c("richness", "diversity", "evenness")) %>%
  rename(variable = index) %>%

  # add community structure data
  bind_rows(
    comm_structure %>%
      filter(variable %in% c("cover", "median_height"),
             variable_class %in% c("graminoids", "vegetation")) %>%
      mutate(variable = paste(variable, variable_class, sep = "_")) %>%
      select(-variable_class)
  ) %>%
  ungroup() %>%
  filter(treatment %in% c("C", "B", "NB")) %>%
  mutate(variable = case_when(variable == "cover_graminoids" ~ "graminoid cover",
                              variable == "median_height_vegetation" ~ "vegetation height",
                              TRUE ~ variable),
         variable = factor(variable, levels = c("richness", "diversity","evenness", "graminoid cover", "vegetation height")),
         treatment = factor(treatment, levels = c("C", "B", "NB")))


res_GP <- Gradient_plot_data %>%
  nest(data = -c(variable)) %>%
  mutate(model = map(data, ~lme(value ~ elevation * treatment, random = ~ 1 | plot_id, data = .x)),
         result = map(model, tidy)) %>%
  unnest(result) %>%
  select(variable, term:p.value) %>%
  filter(!grepl("sd_", term)) |>
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
              labeller = labeller(.default = capitalise)) +
  theme_bw() +
  theme(text = element_text(size = 17),
        legend.position = "top")

Gradient_plot
ggsave("Gradient_plot.jpeg", Gradient_plot, dpi = 300, width = 10, height = 6)






## ----TraitDistribution
trait_distibution <- trait_data %>%
  mutate(trait = recode(trait,
                              c_percent = "C",
                              cn_ratio = "CN~ratio",
                              dc13_permil = "δC13",
                              dn15_permil = "δN15",
                              plant_height_cm = "Height~cm",
                              dry_mass_g = "Dry~mass~g",
                              ldmc = "LDMC",
                              leaf_area_cm2 = "Area~cm^{2}",
                              n_percent = "N",
                              np_ratio = "NP~ratio",
                              p_percent = "P",
                              sla_cm2_g = "SLA~cm^{2}/g",
                              leaf_thickness_mm = "Thickness~mm",
                              wet_mass_g = "Wet~mass~g")) |>
  mutate(trait = factor(trait,
                        levels = c("Height~cm", "Wet~mass~g", "Dry~mass~g",
                                   "Area~cm^{2}", "Thickness~mm", "LDMC",
                                   "SLA~cm^{2}/g", "C", "N","P","CN~ratio",
                                   "NP~ratio", "δC13", "δN15"))) %>%
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
  labs(x = "", y = "Density") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "top")
ggsave("Trait_distribution.jpeg", trait_distibution, dpi = 300, width = 8, height = 8)


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
#ggsave("Trait_checks.jpeg", trait_checks, dpi = 150)



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
# biomass <- read_csv(file = "biomass/China_2016_Biomass_cleaned.csv")
# traitsLeaf <- read_csv(file = "traits/data_cleaned/PFTC1.2_China_2015_2016_LeafTraits.csv")
#
#
# spList <- cover_thin %>% distinct(speciesName) %>%
#   mutate(Dataset = "community") %>%
#   rbind(biomass %>% select(speciesName) %>%
#           distinct() %>%
#           mutate(Dataset = "biomass")) %>%
#   rbind(traitsLeaf %>% select(Taxon) %>%
#           rename("speciesName" = "Taxon") %>%
#           distinct() %>%
#           mutate(Dataset = "trait")) %>%
#   mutate(Presence = "x") %>%
#   pivot_wider(names_from = Dataset, values_from = Presence) %>%
#   arrange(speciesName) %>%
#   filter(!grepl("*Unkown|*mixed forb species|spp$|spp.$|sp$|sp1$|sp2$|sp4$", speciesName))
# spList
# #writexl::write_xlsx(spList, path = "China_FullSpeciesList.xlsx")
#
# taxa %>% select(speciesName) %>%
#   mutate(Dataset = "community") %>%
#   rbind(biomass %>% select(speciesName) %>%
#           distinct() %>%
#           mutate(Dataset = "biomass")) %>%
#   rbind(traitsLeaf %>% select(Taxon) %>%
#           rename("speciesName" = "Taxon") %>%
#           distinct() %>%
#           mutate(Dataset = "trait")) %>%
#   mutate(Presence = "x") %>%
#   pivot_wider(names_from = Dataset, values_from = Presence) %>%
#   arrange(speciesName) %>%
#   filter(grepl("*Unkown|*mixed forb species|spp$|spp.$|sp$|sp1$|sp2$|sp4$", speciesName)) %>%
#   gather(key = dataset, value = present, -speciesName) %>%
#   filter(present == "x") %>%
#   group_by(dataset) %>%
#   count()
#
#
# airtemp <- read_csv(file = "climate/data_cleaned/China_2013_2016_AirTemp.csv", col_names = TRUE)
#
# # GDD
# airtemp %>%
#   mutate(year = year(dateTime),
#          month = month(dateTime),
#          day = day(dateTime)) %>%
#   filter(year %in% c(2013)) %>%
#   group_by(site, year, month, day) %>%
#   summarise(mean = mean(Tair)) %>%
#   filter(mean >= 5) %>%
#   group_by(year, site) %>%
#   summarise(n = n())
#
# # Freezing days
# airtemp %>%
#   mutate(year = year(dateTime),
#          month = month(dateTime),
#          day = day(dateTime)) %>%
#   filter(year %in% c(2013)) %>%
#   group_by(site, year, month, day) %>%
#   summarise(mean = mean(Tair)) %>%
#   filter(mean < 0) %>%
#   group_by(year, site) %>%
#   summarise(n = n())
#
#
# # Table 4
# tbl(con, "turfEnvironment") %>%
#   collect() %>%
#   pivot_longer(cols = c(moss:litterThickness), names_to = "group", values_to = "value") %>%
#   group_by(group) %>%
#   summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)) %>%
#   print(n = Inf)
#
#
# tbl(con, "turfEnvironment") %>%
#   collect() %>%
#   pivot_longer(cols = c(moss:litterThickness), names_to = "group", values_to = "value") %>%
#   ggplot(aes(x = factor(year), y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ group, scales = "free")
#
#
#
# ## Plotting climate figure
# climate <- read_csv(file = "clean_data/PFTC3_Puna_PFTC5_2019_2020_Climate_clean.csv")
# plot_temp <- aggregate(air_temperature~date(date_time)+site+treatment, data = climate[climate$error_flag == 0, ], mean)
# plot_temp$sd <- aggregate(air_temperature~date(date_time)+site+treatment, data = climate[climate$error_flag == 0, ], sd)[,4]
# colnames(plot_temp)[1] <- "date"
# #plot_temp <- plot_temp[(plot_temp$Site != "QUE"), ]
# ggplot(plot_temp, aes(x = date, y = air_temperature, fill = treatment)) +
#   geom_line(aes(col = treatment)) +
#   geom_ribbon(aes(ymin = air_temperature - sd/2, ymax = air_temperature + sd/2), col = NA, alpha = 0.3) +
#   labs(x = "", y = "Air Temperature [°C]") +
#   facet_wrap(~ site, scales = "free") +
#   theme_bw()
