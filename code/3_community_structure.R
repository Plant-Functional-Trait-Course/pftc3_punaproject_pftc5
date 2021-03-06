# Load libraries ----------------------------------------------------------
source("code/load_libraries.R")
library("janitor")

source("code/coordinates.R")

# 2018 data ------------------------------------------------------------------
comm_structure_raw_2018 <- read_csv("data/PFTC3_rawmetaCommunity_2018_Peru.csv") %>%
  clean_names() %>%
  mutate(max_height_cm = rowMeans(select(., starts_with("max"))),
         min_height_cm = rowMeans(select(., starts_with("min"))),
         median_height_cm = rowMeans(select(., starts_with("median")))) %>%
  select(-c(max_height_1_cm:median_height_5_cm)) %>%
  #change '+' in cover to 0.5 and make numeric
  mutate_all(funs(str_replace(., "\\+", "0.5"))) %>%
  mutate(across(c(cover_shrub_layer:cover_litter, max_height_cm:median_height_cm), as.numeric)) %>%
  mutate(treatment = case_when(treatment == "control" ~ "C",
                               treatment == "burned" ~ "B",
                               treatment == "double_burned" ~ "BB",
                               TRUE ~ treatment),
         month = "March",
         year = as.numeric(year)) %>%
  rename(plot_id = plot)

comm_structure_2018 <- comm_structure_raw_2018 %>%
  select(-c(day, photo, elevation, max_height_cm:median_height_cm, cover_shrub_layer:cover_bottom_layer)) %>%
  pivot_longer(cols = c(cover_forbs:cover_litter), names_to = c("variable" , "variable_class"), names_sep = "_", values_to = "value") %>%
  # add height data
  bind_rows(comm_structure_raw_2018 %>%
              select(site, year, month, plot_id, treatment, max_height_cm:median_height_cm) %>%
              pivot_longer(cols = c(max_height_cm:median_height_cm), names_to = "variable", values_to = "value") %>%
              mutate(variable = str_remove(variable, "\\_cm"),
                     variable_class = "vegetation")) %>%
  # add sum of cover
  bind_rows(comm_structure_raw_2018 %>%
              select(site, year, month, plot_id, treatment, cover_shrub_layer:cover_bottom_layer) %>%
              pivot_longer(cols = c(cover_shrub_layer:cover_bottom_layer), names_to = c("variable_class"), values_to = "value") %>%
              mutate(variable_class = str_remove(variable_class, "cover\\_"),
                     variable = "cover")) %>%
  mutate(variable_class = if_else(variable_class == "open", "bare_ground", variable_class),
         course = "PFTC3") %>%
  filter(!is.na(value))


# 2019 data ------------------------------------------------------------------
comm_structure_raw_2019 <- read_csv("data/PU.2_Community metadata_Dataset.csv") %>%
  clean_names() %>%
  filter(!is.na(site)) %>%
  # replace 0 in height/depth data with NA
  mutate(across(max_height_1:bryophyte_depth_5, ~replace(., . == 0 , NA_real_))) %>%
  mutate(max_height_cm = rowMeans(select(., starts_with("max")), na.rm = TRUE),
         min_height_cm = rowMeans(select(., starts_with("min")), na.rm = TRUE),
         median_height_cm = rowMeans(select(., starts_with("median")), na.rm = TRUE),
         bryophyte_depth = rowMeans(select(., starts_with("bryophyte")), na.rm = TRUE)) %>%
  select(-c(max_height_1:bryophyte_depth_5)) %>%
  #change '+' in cover to 0.5 and make numeric
  mutate_all(funs(str_replace(., "\\+", "0.5"))) %>%
  mutate(across(c(cover_graminoids:cover_solid_litter, max_height_cm:bryophyte_depth), as.numeric),
         # merge litter and solid litter because not available in pftc3 sampling
         cover_litter = cover_litter + cover_solid_litter,
         treatment = str_remove(plot, "\\d")) %>%
  select(-cover_solid_litter) %>%
  # add plot nr
  group_by(site, year, month, treatment) %>%
  arrange(site, year, month, treatment, plot) %>%
  mutate(plot_id = 1:n()) %>%
  ungroup() %>%
  #add plot_id
  select(-plot, -day) %>%
  mutate(site = case_when(site == "PILL" ~ "PIL",
                          TRUE ~ site),
         treatment = case_when(site == "TRE" & treatment == "B" ~ "NB",
                               site == "QUE" ~ "B",
                          TRUE ~ treatment),
         month = case_when(month == "4" ~ "April",
                           month == "7" ~ "July",
                           TRUE ~ month),
         year = as.numeric(year))

comm_structure_2019 <- comm_structure_raw_2019 %>%
  select(-c(max_height_cm:bryophyte_depth)) %>%
  pivot_longer(cols = c(cover_graminoids:cover_litter), names_to = c("variable" , "variable_class"), names_sep = "_", values_to = "value") %>%
  # add height data
  bind_rows(comm_structure_raw_2019 %>%
              select(site, year, month, plot_id, treatment, max_height_cm:bryophyte_depth) %>%
              pivot_longer(cols = c(max_height_cm:bryophyte_depth), names_to = "variable", values_to = "value") %>%
              mutate(variable = str_remove(variable, "\\_cm"),
                     variable_class = case_when(variable == "bryophyte_depth" ~ "bryophytes",
                                                  TRUE ~ "vegetation"))) %>%
  mutate(variable_class = case_when(variable_class == "open" ~ "bare_ground",
                                    variable_class == "woody" ~ "shrub",
                                      TRUE ~ variable_class),
         course = "Puna",
         plot_id = as.character(plot_id)) %>%
  filter(!is.na(value))


# 2020 data ------------------------------------------------------------------

comm_structure_raw_2020 <- read_csv("data/PFTC5_QUE_Community_metadata_Dataset.csv") %>%
  clean_names() %>%
  mutate(max_height_cm = rowMeans(select(., starts_with("max")), na.rm = TRUE),
         min_height_cm = rowMeans(select(., starts_with("min")), na.rm = TRUE),
         median_height_cm = rowMeans(select(., starts_with("median")), na.rm = TRUE),
         bryophyte_depth = rowMeans(select(., starts_with("bryophyte")), na.rm = TRUE),
         # merge litter and solid litter because not available in pftc3 sampling
         cover_litter2 = cover_litter + cover_solid_litter) %>%
  select(-c(max_height_1:bryophyte_depth_5, cover_solid_litter)) %>%
  mutate(treatment = "NB",
         plot_id = str_extract(plot, "\\d"),
         month = "March") %>%
  select(-plot, -day)


comm_structure_2020 <- comm_structure_raw_2020 %>%
  select(-c(max_height_cm:bryophyte_depth)) %>%
  pivot_longer(cols = c(cover_graminoids:cover_litter), names_to = c("variable" , "variable_class"), names_sep = "_", values_to = "value") %>%
  # add height data
  bind_rows(comm_structure_raw_2020 %>%
              select(site, year, month, plot_id, treatment, max_height_cm:bryophyte_depth) %>%
              pivot_longer(cols = c(max_height_cm:bryophyte_depth), names_to = "variable", values_to = "value") %>%
              mutate(variable = str_remove(variable, "\\_cm"),
                     variable_class = case_when(variable == "bryophyte_depth" ~ "bryophytes",
                                                TRUE ~ "vegetation"))) %>%
  mutate(variable_class = case_when(variable_class == "bare" ~ "rock",
                                    variable_class == "open" ~ "bare_ground",
                                    variable_class == "woody" ~ "shrub",
                                    TRUE ~ variable_class),
         course = "PFTC5")


# combine datasets ------------------------------------------------------------------

comm_structure <- bind_rows(comm_structure_2018,
                            comm_structure_2019,
                            comm_structure_2020) %>%
  left_join(coordinates %>% select(-comment), by = c("site", "treatment", "plot_id")) %>%
  mutate(season = if_else(month %in% c("July", "November"), "dry_season", "wet_season")) %>%
  select(year, season, month, site, treatment, plot_id, variable:value, burn_year, elevation, latitude, longitude, course)


# Export ------------------------------------------------------------------

# make new folder
dir.create("clean_data")

comm_structure %>%
  write_csv("clean_data/PFTC3-Puna-Peru_2018-2019_CommunityStructure_clean.csv")



