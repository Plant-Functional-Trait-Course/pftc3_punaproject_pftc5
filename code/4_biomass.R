# load libraries
source("code/load_libraries.R")
library("janitor")


biomass_raw <- read_csv("data/PU.6_Biomass_Dataset.csv") %>%
  clean_names()

biomass <- biomass_raw %>%
  unite(col = "remark",  all, x40, x41, x42, x43, x44, x45, x46, na.rm = TRUE, sep = "_") %>%
  select(-c(max_height_1:bryophyte_depth_5)) %>%
  mutate_all(funs(str_replace(., "\\+", "0.5"))) %>%
  mutate(across(c(cover_graminoids:drymass_litter), as.numeric),
         date_of_harvest = dmy(date_of_harvest)) %>%
  pivot_longer(cols = c(cover_graminoids:cover_open_soil, drymass_graminoids:drymass_litter), names_to = c("variable", "functional_group"), values_to = "value", names_sep = "_") %>%
  filter(!is.na(value)) %>%
  bind_rows(biomass_raw %>%
              select(site, treatment, max_height_1:bryophyte_depth_5) %>%
              pivot_longer(cols = c(max_height_1:bryophyte_depth_5), names_to = "variable", values_to = "value") %>%
              mutate(measurement = str_extract(variable, "\\d"),
                     variable = str_remove(variable, "_\\d"))) %>%
  mutate(site = factor(site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE")))

# check
ggplot(biomass, aes(x = site, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free")

biomass %>%
  filter(variable == "drymass") %>%
  ggplot(aes(x = site, y = value)) +
  geom_boxplot() +
  facet_wrap(~ functional_group, scales = "free")

biomass %>%
  filter(variable == "cover") %>%
  ggplot(aes(x = site, y = value)) +
  geom_boxplot() +
  facet_wrap(~ functional_group, scales = "free")
