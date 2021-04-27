#### CREATE DATA DIC FOR READMEFILE ####

source("code/load_libraries.R")
#source("R/Rgathering/DownloadCleanData.R")

# get attribute table
attribute_table <- read_csv(file = "clean_data/PFTC3_Puna_PFTC5_data_dic.csv")

#***********************************************************************
### (i) COMMUNITY DATA

# read in data
comm <- read_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv")

range_comm <- comm %>%
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


comm_dic <- map_df(comm %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_comm, by = "Variable name") %>%
  left_join(attribute_table, by = c("Variable name")) %>%
  select("Variable name", "Variable type", Description, "Variable range or levels", "How measured", Unit)




#***********************************************************************
### (ii) COMMUNITY STRUCTURE

# read in data
comm_struc <- read_csv("clean_data/PFTC3-Puna-Peru_2018-2019_CommunityStructure_clean.csv")

range_comm_struc <- comm_struc %>%
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


comm_struc_dic <- map_df(comm_struc %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_comm_struc, by = "Variable name") %>%
  left_join(attribute_table, by = c("Variable name")) %>%
  select("Variable name", "Variable type", Description, "Variable range or levels", "How measured", Unit)



#***********************************************************************
### (iii) BIOMASS

# read in data
bio <- read_csv("clean_data/Puna_Peru_2019_Biomass_clean.csv")

range_bio <- bio %>%
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.Date), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


bio_dic <- map_df(bio %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_bio, by = "Variable name") %>%
  left_join(attribute_table, by = c("Variable name")) %>%
  select("Variable name", "Variable type", Description, "Variable range or levels", "How measured", Unit)




#***********************************************************************
### (iv) TRAITS

# read in data
leaf_traits <- read_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv")

range_traits <- leaf_traits %>%
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


trait_dic <- map_df(leaf_traits %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_traits, by = "Variable name") %>%
  left_join(attribute_table, by = c("Variable name")) %>%
  select("Variable name", "Variable type", Description, "Variable range or levels", "How measured", Unit)
