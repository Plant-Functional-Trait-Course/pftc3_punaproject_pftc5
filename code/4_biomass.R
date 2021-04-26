# load libraries
source("code/load_libraries.R")
library("janitor")


biomass_raw <- read_csv("data/PU.6_Biomass_Dataset.csv") %>%
  clean_names()

biomass <- biomass_raw %>%
  # merge all comments
  unite(col = "remark",  all, x40, x41, x42, x43, x44, x45, x46, na.rm = TRUE, sep = "_") %>%
  select(-c(max_height_1:bryophyte_depth_5)) %>%
  mutate_all(funs(str_replace(., "\\+", "0.5"))) %>%
  mutate(across(c(cover_graminoids:drymass_litter), as.numeric),
         date_of_harvest = dmy(date_of_harvest)) %>%
  pivot_longer(cols = c(cover_graminoids:cover_open_soil, drymass_graminoids:drymass_litter), names_to = c("variable", "variable_class"), values_to = "value", names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(         variable_class = case_when(variable_class == "woody" ~ "shrub",
                                             variable_class == "open" ~ "bare_ground",
                                             TRUE ~ variable_class)) %>%
  bind_rows(biomass_raw %>%
              mutate(max_height_cm = rowMeans(select(., starts_with("max"))),
                     min_height_cm = rowMeans(select(., starts_with("min"))),
                     median_height_cm = rowMeans(select(., starts_with("median"))),
                     bryophyte_depth = rowMeans(select(., starts_with("bryophyte")))) %>%
              select(site, treatment, max_height_cm:bryophyte_depth) %>%
              pivot_longer(cols = c(max_height_cm:bryophyte_depth), names_to = "variable", values_to = "value") %>%
              mutate(variable = str_remove(variable, "\\_cm"),
                     variable_class = case_when(variable == "bryophyte_depth" ~ "bryophyte",
                                                TRUE ~ "vegetation"))) %>%
  select(site:date_of_harvest, variable:value, remark) %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE")))


# make new folder
dir.create("clean_data")

biomass %>%
  write_csv("clean_data/Puna_Peru_2019_Biomass_clean.csv")


# check
# ggplot(biomass, aes(x = site, y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ variable, scales = "free")
#
# biomass %>%
#   filter(variable == "drymass") %>%
#   ggplot(aes(x = site, y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ functional_group, scales = "free")
#
# biomass %>%
#   filter(variable == "cover") %>%
#   ggplot(aes(x = site, y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ functional_group, scales = "free")
