#########################
#### Chemical Traits ####
#########################

# For now this data is downloaded directly form google docs. When all data is collected, should be moved to OSF.




############################################################################
#### PHOSPHORUS DATA ####

p <- read_excel(path = "data/chemical_traits/PFTC_phosphorus_2018-2022.xlsx") %>%
  select(Batch, Site, ID, Sample_Mass, Sample_Absorbance, Volume_of_Sample_ml) |>
  tidylog::filter(Site == "Peru") |>
  tidylog::filter(!is.na(ID)) |>
  rename(Country = Site) |>
  tidylog::mutate(ID = case_when(ID == "BRG2476" ~ "BRG2478",
                                  ID == "CJD9841" ~ "CJD9041",
                                  TRUE ~ ID))

# Check IDs (all seem to be fine)
#all_codes <- get_PFTC_envelope_codes(seed = 1)
p |>
  filter(!ID %in% c("Hard Red Spring Wheat Flour", "Standard1", "Standard2")) |>
  anti_join(all_codes, by = c("ID" = "hashcode"))
# BRG2476 -> BRG2478
# CJD9841 -> CJD9041

# pull of standard, calculate R2, choose standard for absorbance curve, make regression and plot
standard_concentration <- tibble(Standard = c(0, 2, 4, 8, 12, 16),
                                 Concentration = c(0, 0.061, 0.122, 0.242, 0.364, 0.484))
Standard <- p %>%
  filter(ID %in% c("Standard1", "Standard2")) %>%
  group_by(Country, Batch, ID) %>%
  nest(standard = c(Sample_Absorbance)) %>%
  mutate(standard = map(standard, bind_cols, standard_concentration),
         # remove batch if Sample_Absorbance is NA; Sample has not been measured
         standard = map(standard, ~ filter(., !is.na(Sample_Absorbance))))

# Plot 2 Standard curves
# Standard %>%
#   unnest() %>%
#   ggplot(aes(x = Sample_Absorbance, y = Concentration, colour = ID)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(x = "Absorbance", y = expression(paste("Concentration ", mu, "g/ml"))) +
#   facet_wrap(~ Batch)



# Choose standard and make model
ModelResult <- Standard %>%
  mutate(correlation = map_dbl(standard, ~ if(length(.x$Sample_Absorbance > 1))
  {cor(.x$Sample_Absorbance, .x$Concentration, use = "pair")}
  else{NA_real_})) %>%
  group_by(Country, Batch) %>%
  slice(which.max(correlation)) %>%
  mutate(fit = map(standard, ~lm(Concentration ~ Sample_Absorbance, .)))


# Calculate Mean, sd, coeficiant variability for each leaf and flag data
p2 <- p %>%
  filter(!ID %in% c("Standard1", "Standard2"),
         # remove samples without mass
         !is.na(Sample_Mass)) %>%
  group_by(Country, Batch) %>%
  nest(data = c(ID:Volume_of_Sample_ml)) %>%
  # add estimate from model
  left_join(ModelResult %>% select(-ID), by = c("Country", "Batch"))


OriginalValues <- p2 %>%
  mutate(data = map2(.x = data, .y = fit, ~ mutate(.x, Sample_yg_ml = predict(.y, newdata = select(.x, Sample_Absorbance))))) %>%
  select(-Sample_Mass, -Volume_of_Sample_ml) |>
  unnest(data) %>%
  mutate(Pmass = Sample_yg_ml * Volume_of_Sample_ml,
         Pconc = Pmass / Sample_Mass * 100) %>%
  # Calculate mean, sd, coefficient of variation
  group_by(Batch, Country, ID) %>%
  mutate(meanP = mean(Pconc, na.rm = TRUE),
         sdP = sd(Pconc, na.rm = TRUE),
         CoeffVarP = sdP / meanP) %>%
  # flag data
  mutate(Flag_orig = ifelse(CoeffVarP >= 0.2, "flag", ""))


# wheat: check values, flag/remove, calculate convertion factor
RedWheatValue <- 0.137
CorrectionFactor <- OriginalValues %>%
  filter(ID %in% c("Hard Red Spring Wheat Flour")) %>%
  mutate(P_Correction = Pconc / RedWheatValue) %>%
  # Calculate mean, sd, coefficient of variation
  group_by(Batch, Country, ID) %>%
  summarise(Correction_Factor = mean(P_Correction, na.rm = TRUE)) %>%
  select(-ID)


# Use Correction Factor on data
Corrected_P <- OriginalValues %>%
  filter(!ID %in% c("Hard Red Spring Wheat Flour")) %>%
  tidylog::left_join(CorrectionFactor, by = c("Batch", "Country")) %>%
  mutate(Pconc_Corrected = Pconc * Correction_Factor) %>%
  # Calculate mean, sd, coefficient of variation
  group_by(Country, ID) %>%
  summarise(P_percent = mean(Pconc_Corrected, na.rm = TRUE),
            sdP_Corrected = sd(Pconc_Corrected, na.rm = TRUE),
            CoeffVarP_Corrected = sdP_Corrected / P_percent,
            N_replications = n()) %>%
  # flag data
  mutate(Flag = ifelse(CoeffVarP_Corrected >= 0.2, "coeff. of variation > 0.2", ""))


############################################################################
#### ISOTOPE DATA ####
# import CN and isotope data and merge
# Read isotope data
list_files <- dir(path = "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019/", pattern = "PERU.+\\.xlsx$", full.names = TRUE)

cn_isotopes <- list_files %>%
  set_names() %>%
  map(., ~ {sheetname <- excel_sheets(.x)
  sheetname <- if(length(sheetname) == 1) {
    sheetname
  } else {
    s <- str_subset(sheetname, fixed("REPORT("))
    if(length(s) == 0){s <- sheetname[1]}
    s
  }
  read_excel(.x, skip = 13, sheet = sheetname, na = c("NA", "REPEAT", "small", "See below", "See Below", "#WERT!", "4X"), col_types = "text")}) %>%
  map_df(~ {select(.,-c(...12:...17)) %>%
      slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>%
      filter(!is.na(...1)) %>% # removing empty rows
      rename(Samples_Nr = ...1, ID = `Sample ID`, Site = ...3, Row = R, Column = C, C_percent = `C%`, N_percent = `N%`, CN_ratio = `C/N`, dN15_permil = `δ15N ‰(ATM)`, dC13_permil = `δ13C ‰(PDB)`) %>%
      mutate(Column = as.character(Column))
  }, .id = "filename") |>

  # fix data for rerun
  mutate(Site = if_else(is.na(Site), "Peru", Site)) |>
  # fix repeats
  mutate(ID = case_when(filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-1).321-REPORT.xlsx" & Samples_Nr == "48" ~ "AHY9776",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "23" ~ "AUK0338",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "24" ~ "AUM5735",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "57" ~ "AYK7656",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "58" ~ "AYL5352",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "69" ~ "BBF8955",
                        filename == "data/chemical_traits/PFTC3_Puna_cn_isotopes_2018-2019//Enquist(21PERU-2).621-REPORT.xlsx" & Samples_Nr == "70" ~ "BBG5902",
                        TRUE ~ ID)) |>
  # remove empty rows and failures
  tidylog::filter(Samples_Nr != "REPEATS") |>
  tidylog::filter(!is.na(C_percent))


# check duplicates (no duplicates)
# cn_isotopes %>%
#   group_by(ID) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>%
#   arrange(ID) %>% print(n = Inf)


# Check ids
#setdiff(cn_isotopes$ID, all_codes$hashcode)

cn_data <- cn_isotopes %>%
  select(-Samples_Nr, -Row, -Column, -...11) |>
  # make traits numeric
  mutate(C_percent = as.numeric(C_percent),
         N_percent = as.numeric(N_percent),
         CN_ratio = as.numeric(CN_ratio),
         dN15_permil = as.numeric(dN15_permil),
         dC13_permil = as.numeric(dC13_permil)) |>
  mutate(ID = gsub("AU G3715", "AUG3715", ID),
         ID = gsub("BRG2476", "BRG2478", ID),
         ID = gsub("BYD8570", "BYD5870", ID),
         ID = gsub("BZI2250", "BZI2252", ID),
         ID = gsub("CYT2078", "CYT2076", ID),
         ID = gsub("DFO4823", "DFD4823", ID),
         ID = gsub("DFG3811", "DFO3811", ID),
         ID = gsub("EEJ6179", "EEJ6178", ID),
         ID = gsub("EGU6285", "EGU8285", ID),
         ID = gsub("EKE4630", "EKE4830", ID)) %>%
  rename(Country = Site)

# check valid IDs
# cn_data |>
#   anti_join(all_codes, by = c("ID" = "hashcode")) |>
#   distinct(ID)


############################################################################
### MERGE ###


cnp_data <- cn_data %>%
  tidylog::full_join(Corrected_P, by = c("ID", "Country")) %>%
  # might want to keep Flag_corrected, if there are any flags
  select(ID, Country, C_percent:dC13_permil, P_percent, Flag) %>%
  # filter unrealistic values
  tidylog::mutate(C_percent = if_else(C_percent > 65, NA_real_, C_percent),
         P_percent = if_else(P_percent < 0, NA_real_, P_percent)) %>%
  mutate(NP_ratio = N_percent / P_percent) |>
  select(id = ID, c_percent = C_percent, n_percent = N_percent, cn_ratio = CN_ratio, p_percent = P_percent, np_ratio = NP_ratio, dn15_permil = dN15_permil, dc13_permil = dC13_permil, Flag)
