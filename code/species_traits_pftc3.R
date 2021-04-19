## PFTC3 2018 PERU ----

#' Cleaning and merging trait data with the species names corrected in 2020
#'
# Load libraries ----
# source this file if needed
#source("code/load_libraries.R")

# 0RIGINAL ENVELOPE CODES ----
# for checking IDs
all_codes <- get_PFTC_envelope_codes(seed = 1)

#' Reading data from PFTC3
#'
#' Species data dictionary for PFTC3
#'

spp_trait_dictionary_2018 <- read_csv("data/PFTC3_Peru_2018_TaxonomicDictionary.csv") %>%
  clean_names() %>%
  mutate_if(is.character, ~str_trim(.))

# coordinates
coordinates_Peru_2020 <- read_excel("data/PU.10_PFTC3.10_2020_Peru_Coordinates.xlsx")


# Leaf area data
LeafArea.raw <- read_csv(file = "data/PFTC3_Peru_2018_Raw_LeafArea.csv")

#### LEAF AREA ####
LeafArea2018 <- LeafArea.raw %>%
  mutate(ID = substr(ID, 1, 7)) %>%

  # Remove non leaf parts of scan
  filter(!(ID == "BZW1631" &  LeafArea == 104.434)) %>% # remove large area, is black area on scan
  filter(!(ID == "EUO3529" & LeafArea == 0.192)) %>% # remove small part, cable on scan
  filter(!(ID == "AOF7984" & LeafArea == 0.187)) %>% # remove part, non leaf on scan

  # remove double scan (other leaf is BDJ1110, where ID is also wrong)
  filter(ID != "VJD1110") %>%

  # Fix leafID
  mutate(ID = gsub("ECV!179", "ECV1792", ID),
         ID = gsub("BAT0442", "BAT9442", ID),
         ID = gsub("BEZ1256", "BEZ1356", ID),
         ID = gsub("CSJ9255", "CZJ9255", ID),
         ID = gsub("EBO1873", "EBO1973", ID),
         ID = gsub("avs5435", "AVS5435", ID),
         ID = gsub("BCI3405", "BIC3405", ID),
         ID = gsub("BDJ1110", "BJD1110", ID),
         ID = gsub("bov5280", "BOV5280", ID),
         ID = gsub("BTK6387", "BTK6307", ID),
         ID = gsub("ADP5352", "ADP5320", ID),
         ID = gsub("AEB7575", "AEB7555", ID),
         ID = gsub("AEY3890", "AEY3990", ID),
         ID = gsub("AH01919", "AHO1919", ID),
         ID = gsub("AHF977", "AHY977", ID),
         ID = gsub("AIR5353", "AIH5353", ID),
         ID = gsub("ANP7254", "ANP7205", ID),
         ID = gsub("BSQ6672", "BQS6672", ID),
         ID = gsub("CDF7159", "CFD7159", ID),
         ID = gsub("GNK310.", "GNK3140", ID),
         ID = gsub("HBVO473", "HBV0473", ID),
         ID = gsub("egg2246", "EGG2246", ID)) %>%

  # Sum areas for each ID
  group_by(ID) %>%
  summarise(Area_cm2 = sum(LeafArea), NumberLeavesScan = n()) %>%

  # replace LeafArea with NA - empty or corrupted scan
  mutate(Area_cm2 = ifelse(ID == "AUB2092", NA, Area_cm2)) %>%
  add_row(ID = "BMB7274", Area_cm2 = NA) %>%
  add_row(ID = "EHP2066", Area_cm2 = NA) %>%
  add_row(ID = "FDF1809", Area_cm2 = NA)


#### LEAF TRAITS ####
files <- dir(path = "data/raw_traits_2018/", pattern = "\\.xlsx$", full.names = TRUE)
traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>%
  set_names(basename(.)) %>%
  map_df(read_excel, col_types = c("text", "date", "text", "numeric", rep("text", 4), rep("numeric", 8), "text", "numeric", "text"), .id = "file")

#' Review unique IDs
traits.raw %>%
  count(ID) %>%
  distinct(n)

# Merge traits and LeafArea  and clean data
traits <- traits.raw %>%

  ### FIX WROND LEAF ID's
  mutate(ID = gsub("WSY0063", "ESY0063", ID),
         ID = gsub("EEA7841", "EAA7841", ID),
         ID = gsub("EOP2126", "EOP2116", ID),
         ID = gsub("ELN57788", "ELN5788", ID),
         ID = gsub("EIH0038", "EHI0038", ID),
         ID = gsub("EFD9867", "EFP9867", ID),
         ID = gsub("EKB8895", "EBK8895", ID),
         ID = gsub("CJO6932", "COJ6932", ID),
         ID = gsub("DJV0218", "DJV0318", ID),
         ID = gsub("DMM5647", "DMM5645", ID),
         ID = gsub("DGE6973", "DGE6927", ID),
         ID = gsub("DCB", "DCB6902", ID),
         ID = gsub("DHG7363", "DHG7383", ID),
         ID = gsub("CMO0287", "CMO2587", ID),
         ID = gsub("CZV0169", "CZV0159", ID),
         ID = gsub("ELB6979", "ELB6970", ID),
         ID = gsub("FAG3940", "FAG3950", ID),
         ID = gsub("CQ06175", "CQO6175", ID),
         ID = gsub("DQO712", "DQO7122", ID),
         ID = gsub("CZZ7222", "CCZ7222", ID),
         ID = gsub("BDG9657", "BGD9657", ID),
         ID = gsub("DBE080", "DBE0880", ID),
         ID = gsub("CHK3202", "CHK3203", ID),
         ID = gsub("CFX6171", "CFX6172", ID),
         ID = gsub("BQT3166", "BQT1366", ID),
         ID = gsub("BLM4402", "BML4402", ID),
         ID = gsub("ALD", "ALD3826", ID),
         ID = gsub("GZM1878", "GMZ1878", ID),
         ID = gsub("ARR112", "ARR1112", ID),
         ID = gsub("GWV45096", "GWV4596", ID),
         ID = gsub("AWR2030", "AWR2040", ID),
         ID = gsub("DDC220", "DDC2200", ID),
         ID = gsub("AVE4872", "AVE4827", ID),
         ID = gsub("BQG5351", "BQH5351", ID),
         ID = gsub("EXP4335", "EXK4335", ID), # maybe UEX4335?
         ID = gsub("CTG2458", "CTG2468", ID),
         ID = gsub("AKB5462", "AKB5492", ID),
         ID = gsub("HKL5161", "HKL5191", ID)) %>%
  filter(!is.na(ID)) %>% # remove 46 lines that have ID as NA, empty rows

  # REMOVE DUPLICATE ENTRIES
  # Remove duplicate entries of envelopes, exact same information
  distinct() %>%

  #
  # Remove non real duplicates (but they are)
  filter(is.na(Experiment) | Experiment != "Sean") %>% #BTB2511
  filter(is.na(Comment) | Comment != "Might have been entered double") %>%
  filter(!(ID == "BFH6092" & is.na(Date))) %>% #BFH6092
  filter(!(ID == "CXN2517" & is.na(Date))) %>% #CXN2517
  filter(!(ID == "AGT5582" & is.na(Date))) %>% #AGT5582
  filter(!(ID == "DGA5662" & is.na(Date))) %>% #DGA5662
  filter(!(ID == "DGF0419" & is.na(Date))) %>% #DGF0419
  filter(!(ID == "DGT8067" & is.na(Date))) %>% #DGT8067
  filter(!(ID == "DGW6179" & is.na(Date))) %>% #DGW6179
  filter(!(ID == "DGY8804" & is.na(Date))) %>% #DGY8804
  filter(!(ID == "DIM3696" & is.na(Date))) %>% #DIM3696
  filter(!(ID == "DIS0103" & is.na(Date))) %>% #DIM3696
  filter(!(ID == "DIU7397" & is.na(Date))) %>% #DIU7397
  filter(!(ID == "DJP8187" & is.na(Date))) %>% #DJP8187
  filter(!(ID == "DJQ3777" & is.na(Date))) %>% #DJP8187
  filter(!(ID == "EDT8254" & is.na(Date))) %>% #EDT8254
  filter(!(ID == "EQX6665" & is.na(Date))) %>% #EQX6665
  filter(!(ID == "CXN2517" & is.na(Comment))) %>% #CXN2517
  filter(!(ID == "DEG8722" & is.na(Comment))) %>% #DEG8722


  # Remove double entry
  filter(!(ID == "ESK2822" & Species == "P"),
         !(ID == "DOR6678" & file == "LeafArea_Peru_2018_IO.xlsx")) %>%
  mutate(ID = ifelse((ID == "BTK6307" & Height_cm == 68), "", ID)) %>%

  ### FIX WRONG VARIABLES AND DATA
  # Dates (no such date found)
  mutate(Date = if_else(Date == ymd_hms("2018-05-15 00:00:00"), ymd_hms("2018-03-15 00:00:00"), Date)) %>%

  # Site name
  mutate(Site = plyr::mapvalues(Site, c("AJC", "PIL", "WAY", "ACJ", "TRE", "QUE", "Wayqecha", "Way", "QYE"), c("ACJ", "PIL", "WAY", "ACJ", "TRE", "QUE", "WAY", "WAY", "QUE"))) %>%
  mutate(Site = ifelse(ID == "AVX6287", "QUE", Site),
         Site = ifelse(ID == "AVY2694", "QUE", Site),
         Site = ifelse(ID == "DVA3114", "ACJ", Site),
         Site = ifelse(ID == "DNW0546", "PIL", Site),
         Site = ifelse(ID == "DOA7686", "PIL", Site),
         Site = ifelse(ID == "FAG3940", "PIL", Site)) %>%

  # Fix elevation (WAY:3100m; ACJ:3475m; PIL:3675m ; TRE:3715m; QUE:3888m)
  mutate(Elevation = ifelse(Site == "PIL" & Elevation %in% c(2675, 3400, 3600, 3647, 3650, 3670, 3700), 3675, Elevation)) %>% #probably add 3475, but it's ACJ elevation
  mutate(Elevation = ifelse(Site == "ACJ" & Elevation %in% c(3400, 3457, 3465, 3467, 3474, 3487, 3567, 3600, 3440), 3475, Elevation)) %>%
  mutate(Elevation = ifelse(Site == "TRE" & Elevation %in% c(3700, 3701, 3702, 3710), 3715, Elevation)) %>%
  mutate(Elevation = ifelse(Site == "QUE" & Elevation %in% c(3400, 3800, 3900), 3888, Elevation)) %>%
  mutate(Elevation = ifelse(ID == "EOR9773", 3675, Elevation),
         Elevation = ifelse(ID == "DNS2332", 3675, Elevation)) %>%
  mutate(Comment = ifelse(ID == "EOR9773", "Elevation wrong on envelope, assume Site was right", Comment),
         Comment = ifelse(ID == "DNS2332", "Elevation wrong on envelope, assume Site was right", Comment)) %>%
  mutate(Elevation = ifelse(ID %in% c("ARN1263", "AVY2694"), 3888, Elevation),
         Elevation = ifelse(ID == "EOX9894", 3100, Elevation),
         Elevation = ifelse(ID == "EYW8013", 3675, Elevation)) %>%

  # Project
  mutate(Project = ifelse(Project %in% c("Sean", "sean", "SEAN"), "Sean", Project)) %>%
  mutate(Project = ifelse(ID %in% c("EYX1643", "EOT2012", "EOR9773"), "Sean", Project)) %>%

  # missing experiment
  mutate(Experiment = ifelse(ID %in% c("EYX1643", "EOT2012"), "", Experiment)) %>%
  mutate(Experiment = ifelse(ID == "ETC9124", "C", Experiment)) %>%
  mutate(Experiment = plyr::mapvalues(Experiment,
                                      c("b", "B", "Burn", "BB", "C", "c", "E", "EARLY", "Early", "early", "early-E", "L", "LATE", "Late", "late", "missing experiment"),
                                      c("B", "B", "B", "BB", "C", "C","B", "B", "B", "B", "B", "C", "C", "C", "C", NA))) %>%
  mutate(Experiment = ifelse(ID == "DBI7438", "B", Experiment),
         Experiment = ifelse(ID == "AML7186", "C", Experiment),
         Experiment = ifelse(ID == "DHX2100", "C", Experiment),
         Experiment = ifelse(ID == "DMU7088", "B", Experiment),
         Experiment = ifelse(ID == "BEC2276", "B", Experiment),
         Experiment = ifelse(ID == "ELN5788", "C", Experiment),
         Experiment = ifelse(ID == "BJL6171", "B", Experiment),
         Plot = ifelse(ID == "BJL6171", 2, Plot),
         Individual_nr = ifelse(ID == "BJL6171", 3, Individual_nr),
         Experiment = ifelse(ID %in% c("FCY6830", "FCV3487"), "BB", Experiment),
         Experiment = ifelse(ID == "EZC2604", "BB", Experiment),
         Experiment = ifelse(ID == "EZW1995", "B", Experiment),
         Experiment = ifelse(ID == "CPJ9448", "B", Experiment)
  ) %>%

  # Plot
  mutate(Plot = ifelse(ID == "DBF4177", 3, Plot)) %>%

  # wrong individual number
  mutate(Individual_nr = ifelse(Site == "WAY" & Genus == "Eriosorus" & Experiment == "C" & Plot == 2 & Individual_nr == 6, 5, Individual_nr)) %>%
  mutate(Individual_nr = ifelse(Individual_nr == 15, 5, Individual_nr)) %>%


  ### JOIN TRAITS WITH LEAF AREA
  left_join(LeafArea2018, by = "ID") %>%


  ### MAKE DATA TALK TO OTHER PFTC DATA
  mutate(Bulk = ifelse(ID == "FAJ4238", "bulk", Bulk)) %>%

  # rename variables
  mutate(Dry_Mass_g = NA) %>% ### REMOVE THIS ONCE DRY MASS IS HERE!!!!
  rename(NrLeaves = Nr_leaves, Plant_Height_cm = Height_cm, Treatment = Experiment, PlotID = Plot, Wet_Mass_g = Wet_mass_g, Leaf_Thickness_1_mm = Leaf_thickness_1_mm, Leaf_Thickness_2_mm = Leaf_thickness_2_mm, Leaf_Thickness_3_mm = Leaf_thickness_3_mm, Leaf_Area_cm2 = Area_cm2) %>%

  # Create new variables
  mutate(Country = "PE",
         Year = 2018,
         Gradient = 1,
         Project = ifelse(is.na(Project), "T", Project),
         NrLeaves = ifelse(is.na(NrLeaves), 1, NrLeaves)) %>%

  # REMOVE ELEV, JOIN WITH COORD_2020
  select(-Elevation) %>%
  left_join(coordinates_Peru_2020 %>% select(-Comment), by = c("Site", "Treatment", "PlotID")) %>%

  ### CALCULATE AREA, SLA, etc.
  # Species with leaf number = > Leaf nr = 1 because of calculations below
  mutate(NrLeaves = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), 1, NrLeaves)) %>%

  # Fix wrong values
  mutate(Wet_Mass_g = ifelse(ID == "DKQ1911", NA, Wet_Mass_g),
         Leaf_Thickness_1_mm = ifelse(ID == "AUV6280", 0.136, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "AZC6125", 0.15, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "AFA0030", 0.181, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "HHX0541", 0.13, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "EAT7036", 0.09, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "FAE4104", 0.065, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "AWH2532", 0.172, Leaf_Thickness_1_mm),
         Leaf_Thickness_1_mm = ifelse(ID == "HHX0541", 0.013, Leaf_Thickness_1_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "HHX0541", 0.016, Leaf_Thickness_2_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "DTQ4241", 0.07, Leaf_Thickness_2_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "BUK1560", 0.031, Leaf_Thickness_2_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "BJQ2059", 0.346, Leaf_Thickness_3_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "BUX0491", 0.231, Leaf_Thickness_3_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "CAE9465", 0.228, Leaf_Thickness_3_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "DAE4308", 0.656, Leaf_Thickness_3_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "AMW1331", 0.445, Leaf_Thickness_3_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "DZL1521", 0.1, Leaf_Thickness_3_mm)) %>%

  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  mutate(Leaf_Area_cm2 = ifelse(Genus == "Sisyrinchium", Leaf_Area_cm2 * 2, Leaf_Area_cm2),
         Leaf_Thickness_1_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_1_mm / 2, Leaf_Thickness_1_mm),
         Leaf_Thickness_2_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_2_mm / 2, Leaf_Thickness_2_mm),
         Leaf_Thickness_3_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_3_mm / 2, Leaf_Thickness_3_mm)) %>%

  # Calculate average leaf thickness
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE)) %>%


  ### FIXING COMMENTS
  mutate(Comment = ifelse(ID == "ENF3830", paste(Comment, "_dirt"), Comment),
         Comment = ifelse(ID == "EHP2066", paste(Comment, "empty_scan", sep = ";_"), Comment),
         Comment = ifelse(ID == "AWE1352", "scan_missing", Comment),
         Comment = ifelse(ID == "CVV7522", "smallLeaf_NoWetMass", Comment),
         Comment = ifelse(is.na(Leaf_Area_cm2), paste(Comment, "Missing scan", sep = "; "), Comment),
         Comment = ifelse(Comment %in% c("CER2406", "HHD4231", "HHC7286", "AER8651", "BZH8056", "HGH0916", "AUO1998", "BRH4842", "AVZ7114", "AYO1679", "AFG9783", "DTF5574", "CZT2801"), paste(Comment, "ThinLeaf_value_from_envelope", sep = "; "), Comment),
         Comment = ifelse(Comment %in% c("EFM5927", "ENH3749", "ENG3094", "EZO8107"), paste(Comment, "ThickLeaf_value_from_envelope", sep = "; "), Comment),
         Comment = ifelse(ID %in% c("EFG2323", "FHL2051"), "NerveMeasured_tooThick", Comment)) %>%
  # Leaves with wrong treatment: PIL BB 5 does not exist
  mutate(Comment == ifelse(Site == "PIL" & Treatment == "BB" & PlotID == 5, "wrongTreatment", Comment)) %>%

  ### FLAG DATA
  ## AREAFLAG
  mutate(AreaFlag = ifelse(ID == "BOT1325", "DryLeaf_wrongArea", "")) %>%  # leaf was very dry, wrong area
  mutate(AreaFlag = ifelse(ID %in% c("ECN1410", "EPV0866", "FCC3736", "FDL7538"), "LeafOutside_wrongArea", "")) %>%   # Leaf on ruler or on edge - nothing one can do about it - area flag
  mutate(AreaFlag = ifelse(ID == "EMY0414", "TooLargeArea", "")) %>% # Lycopodiella, more than2 branches scanned
  # Part missing on scan - wrong area
  mutate(AreaFlag = ifelse(ID %in% c("CZL9321", "DUO6664", "DWL3144", "DWV2987", "EFU8488", "EPV0866", "EPW2330", "ERV6823", "ERW0817", "EUG2994", "HHV3850"), "Cut_wrongArea", "")) %>%

  # Empty or corrupted scan
  mutate(AreaFlag = ifelse(ID == "BMB7274", "EmptyScan_noArea", ""),
         AreaFlag = ifelse(is.na(Leaf_Area_cm2), paste(AreaFlag, "Missing scan", sep = "; "), AreaFlag)) %>%
  mutate(AreaFlag = ifelse(ID == "EHP2066", "CorruptedScan_noArea", "")) %>%
  mutate(AreaFlag = ifelse(ID == "FDF1809", "CorruptedScan_noArea", "")) %>%
  mutate(AreaFlag = ifelse(ID == "AUB2092", "ScannedOnWrongSide_noArea", "")) %>%

  ## DRYWEIGHTFLAG
  mutate(DryFlag = ifelse(ID == "EMY0414", "TooLargeWeight", "")) %>%  # Lycopodiella, more than2 branches scanned

  ## WETWEIGHTFLAG
  mutate(WetFlag = ifelse(ID == "EMY0414", "TooLargeWeight", ""),
         WetFlag = ifelse(ID == "CVV7522", "TooSmallWeight", ""),
         WetFlag = ifelse(ID == "DKQ1911", "TooSmallWeight", ""))   # Lycopodiella, more than2 branches scanned



#### FIX GRAMINOID NAMES (first round of corrections) ####
GraminoidsUpdate <- read_excel(path = "data/Peru_2018_unique_gramminoids_names_18-10-11.xlsx", col_names = TRUE)
GraminoidsUpdate <- GraminoidsUpdate %>%
  select(Genus, Species, Comment, merge_genus, merge_species_cons) %>%
  mutate(Species = replace(Species, Species == "NA", NA),
         Comment = replace(Comment, Comment == "NA", NA))

traits <- traits %>%
  left_join(GraminoidsUpdate, by = c("Genus", "Species", "Comment")) %>%
  mutate(Genus = ifelse(!is.na(merge_genus), merge_genus, Genus),
         Species = ifelse(!is.na(merge_species_cons), merge_species_cons, Species))


#### FIX FORB NAMES (first round of corrections) ####
traits <- traits %>%
  # Fix Genus names
  mutate(Genus = gsub("Achemilla|Alchemilla ", "Alchemilla", Genus),
         Genus = gsub("Belonathus|Belonauthus", "Belonanthus", Genus),
         Genus = gsub("Calamagostus", "Calamagrostis", Genus),
         Genus = gsub("Elaphoglossum ", "Elaphoglossum", Genus),
         Genus = gsub("Gaulteria", "Gaultheria", Genus),
         Genus = gsub("Gauphaliaum", "Gnaphalium", Genus),
         Genus = gsub("Gentranella", "Gentianella", Genus),
         Genus = gsub("Gamachaeta|Gauochata", "Gamochaeta", Genus),
         Genus = gsub("Geufiaua|Geutiana", "Gentiana", Genus),
         Genus = gsub("Hypocheris|Hypoehaens", "Hypochaeris", Genus),
         Genus = gsub("Hypsophila", "Hypsela", Genus),
         Genus = gsub("Lysopomia|Lysipania", "Lysipomia", Genus),
         Genus = gsub("Myconia|Mycomia", "Miconia", Genus),
         Genus = gsub("Nertera |Netera", "Nertera", Genus),
         Genus = gsub("Niphogetum", "Niphogeton", Genus),
         Genus = gsub("Orchio|Orquidede|Orchid", "Orchidaceae", Genus),
         Genus = gsub("Oritrophilum|Oritrophium|Orithrophium", "Oritrophium", Genus),
         Genus = gsub("Oerithales", "Oreithales", Genus),
         Genus = gsub("Perzia", "Perezia", Genus),
         Genus = gsub("Prenettya", "Pernettya", Genus),
         Genus = gsub("Pterichius|Pterichris", "Pterichis", Genus),
         Genus = gsub("Senecia", "Senecio", Genus),
         Genus = gsub("Werneria ", "Werneria", Genus),
         Genus = gsub("New_grass", "New grass", Genus)
  ) %>%

  mutate(Genus = ifelse(Genus == "Melpome", "Melpomene", Genus),
         Genus = ifelse(Genus == "Melpone", "Melpomene", Genus)) %>%

  mutate(Genus = ifelse((Genus == "Pernettya" & Species == "pungens"), "Perezia", Genus)) %>%


  # Fix Species names
  mutate(Species = ifelse(is.na(Species), "sp", Species)) %>%

  mutate(Species = gsub("sp.", "sp", Species),
         Species = gsub("erodifolia", "erodiifolia", Species),
         Species = gsub("genisteloides", "genistelloides", Species),
         Species = gsub("caesptosa", "caespitosa", Species),
         Species = gsub("audicola", "andicola", Species),
         Species = gsub("lamatus|lanata", "lanatus", Species),
         Species = gsub("cheilanthoides|cheilauthoides|chelianthoides", "cheilantoides", Species),
         Species = gsub("blomerata|glmoerata", "glomerata", Species),
         Species = gsub("sessilifolium", "sessiliflorum", Species),
         Species = gsub("audinus", "andinum", Species),
         Species = gsub("raraxacoides|taraxaciodes|taroxacoides", "taraxacoides", Species),
         Species = gsub("integrafolia", "integrifolia", Species),
         Species = gsub("hieraciodes", "hieracioides", Species),
         Species = gsub("macrophazea", "macrocephala", Species),
         Species = gsub("silverstris|sylvestris", "silvestris", Species),
         Species = gsub("aquilineum", "aquilinum", Species),
         Species = gsub("cuscoensis", "cuzcoensis", Species),
         Species = gsub("nugibena", "nubigena", Species),
         Species = ifelse((Genus == "Asteraceae" & Species == "hairy leaf"), "sphairyleaf", Species),
         Species = ifelse((Genus == "Chaptalia" & Species == "sp"), "cordata", Species),
         Species = ifelse((Genus == "Elaphoglossum" & Species == "narrow"), "spnarrow", Species),
         Species = ifelse((Genus == "Galium" & Species == "aparine"), "hypocarpium", Species),
         Species = ifelse((Genus == "Oritrophium" & Species == "hyeanacilolia"), "hieracioides", Species),
         Species = ifelse((Genus == "Oritrophium" & Species == "sp"), "hieracioides", Species),
         Species = ifelse((Genus == "Pterichis" & Species == "sp"), "silvestris", Species),
         Species = ifelse((Genus == "Senecio" & Species == "sp 2"), "sp2", Species),
         Species = ifelse((Genus == "Viola" & Species == "pygmae|pygmaeaa"), "pygmaea", Species),
         Species = ifelse((Genus == "Unknown" & Species == "hairy leaves"), "hairy leaf", Species),

         # Second round of changes in Iceland
         Species = gsub("cylindistachya", "cylindristachya", Species),
         Species = gsub("hispdus", "hispidus", Species),
         Species = gsub("cheilantoides", "cheilanthoides", Species),
         Species = gsub("cimericana", "americana", Species),
         Species = gsub("vaccinoides", "vaccinioides", Species),
         Species = gsub("ernesti", "ernestii", Species),
         Species = gsub("cermia", "cernua", Species),
         Species = ifelse((Genus == "Asteraceae" & Species == "hairy leaf"), "sp", Species),
         Species = ifelse((Genus == "Belonanthus" & Species == "sp"), "hispidus", Species),
         Species = ifelse((Genus == "Elaphoglossum" & Species == "wide"), "amphioxys", Species),
         Species = ifelse((Genus == "Eriosorus" & Species == "sp"), "cheilanthoides", Species),
         Species = ifelse((Genus == "Galium" & Species == "sp"), "hypocarpium", Species),
         Species = ifelse((Genus == "Gentiana" & Species == "sp"), "sedifolia", Species),
         Species = ifelse((Genus == "Gnaphalium" & Species == "racemosa"), "dombeyanum", Species),
         Species = ifelse((Genus == "Gnaphalium" & Species == "sp"), "dombeyanum", Species),
         Species = ifelse((Genus == "Hypericum" & Species == "floribundum"), "andinum", Species),
         Species = ifelse((Genus == "Hypericum" & Species == "sp"), "andinum", Species),
         Species = ifelse((Genus == "Hypochaeris" & Species == "sp"), "taraxacoides", Species),
         Species = ifelse((Genus == "Viola" & Species == "pygmae"), "pygmaea", Species),
         Species = ifelse((Genus == "Lycopodiella" & Species == "sp"), "cernua", Species),
         Species = ifelse((Genus == "Hypericum" & Species == "floribundum"), "andinum", Species),
         Species = ifelse((Genus == "Melpomene" & Species == "sp"), "moniliformis", Species),
         Species = ifelse((Genus == "Miconia" & Species == "sp"), "rotundifolia", Species),
         Species = ifelse((Genus == "Nertera" & Species == "sp"), "granadensis", Species),
         Species = ifelse((Genus == "Pernettya" & Species == "sp"), "prostrata", Species),
         Species = ifelse((Genus == "Puya" & Species == "sp"), "leptostachya", Species),
         Species = ifelse((Genus == "Vaccinium" & Species == "sp"), "floribundum", Species)
  ) %>%

  mutate(Species = ifelse(ID == "CHL7402", "sp", Species)) %>%
  mutate(Genus = ifelse(ID %in% c("CYC1709", "AMT5064", "AMS7010", "AMR3145", "AMV4004", "AMU6887", "EDH4767", "DVJ9425", "DUX6052", "DUQ4771", "DVN0309", "CYD2957"), "Pernettya", Genus)) %>%
  mutate(Species = ifelse(ID %in% c("CYC1709", "AMT5064", "AMS7010", "AMR3145", "AMV4004", "AMU6887", "EDH4767", "DVJ9425", "DUX6052", "DUQ4771", "DVN0309", "CYD2957"), "prostrata", Species)) %>%

  # Taxon
  mutate(Taxon = paste(Genus, Species, sep = " ")) %>%

  #these species are bulk species so number of leaves 'standardised to 1
  mutate(NrLeaves = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"),
                           1,
                           NrLeaves)) %>%
  # Sort
  select(ID, Country, Year, Project, Treatment, Site, Elevation, Latitude, Longitude, Gradient, PlotID, Taxon, Genus, Species, Date, Individual_nr, Plant_Height_cm, Wet_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Bulk, NrLeaves, NumberLeavesScan, AreaFlag, DryFlag, WetFlag, Comment)


#### JOIN DRY MASS ####
dryweigth <- read_csv(file = "data/PFTC3_Peru_2018_DryWeight.csv") %>%
  select(ID, Dry_Mass_g)

# Check if dryweight join
#dryweigth %>% anti_join(traits, by = "ID") %>% as.data.frame()

# trait corrections
trait_correction <- read_excel(path = "data/trait_pftc_and_puna_corregido_LVB.xlsx") %>%
  select(id, wet_mass_corr = wet_mass_total_g, dry_mass_corr = dry_mass_total_g, leaf_area_corr = leaf_area_total_cm2, leaf_thickness_1_corr = leaf_thickness_1_mm, leaf_thickness_2_corr = leaf_thickness_2_mm, leaf_thickness_3_corr = leaf_thickness_3_mm, number_leaves_scan_paul)


trait_pftc3 <- traits %>%
  left_join(dryweigth, by = "ID") %>%
  clean_names() %>%
  # join corrections
  left_join(trait_correction, by = c("id")) %>%
  mutate(number_leaves_scan_paul = as.numeric(ifelse(number_leaves_scan_paul == "no se encontro" | is.na(number_leaves_scan_paul), number_leaves_scan, number_leaves_scan_paul))) %>%

  mutate(wet_mass_total_g = wet_mass_corr,
         dry_mass_total_g = dry_mass_corr,
         leaf_area_total_cm2 = leaf_area_corr,
         leaf_thickness_1_mm = leaf_thickness_1_corr,
         leaf_thickness_2_mm = leaf_thickness_2_corr,
         leaf_thickness_3_mm = leaf_thickness_3_corr) %>%

  # # Calculate values on the leaf level (mostly bulk samples)
  # rename(wet_mass_total_g = wet_mass_g,
  #        dry_mass_total_g = dry_mass_g,
  #        leaf_area_total_cm2 = leaf_area_cm2) %>%
  mutate(wet_mass_g = wet_mass_total_g / number_leaves_scan_paul,
         dry_mass_g = dry_mass_total_g / number_leaves_scan_paul,
         leaf_area_cm2 = leaf_area_total_cm2 / number_leaves_scan_paul) %>%

  # make wet and dry mass NA if 0
  mutate(wet_mass_g = if_else(wet_mass_g == 0, NA_real_, wet_mass_g),
         dry_mass_g = if_else(dry_mass_g == 0, NA_real_, dry_mass_g)) %>%

  # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / wet_mass_g,
         ldmc = dry_mass_g / wet_mass_g,
         leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%

  # fix special cases
  mutate(sla_cm2_g = if_else(id == "HFW6441", NA_real_, sla_cm2_g)) %>%

  # # Wet and dry mass do not make sense for these species
  mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, dry_mass_g),
         wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, wet_mass_g),
         leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), NA_real_, leaf_area_cm2)) %>%

  #remove final duplicates
  distinct() %>%
  filter(project != "Sean") %>%

  # remove 5 leaves with wrong ldmc/sla values because of problems in wet/dry mass
  filter(!id %in% c("ASI4769", "AVO1141", "DSP7480", "BTW3395", "AVL0846")) %>%

  # remove because only height data
  filter(id != c("DXJ2730")) %>%

  # fix taxonomy
  left_join(spp_trait_dictionary_2018, by = c("treatment", "site", "plot_id", "taxon")) %>%
  mutate(course = "PFTC3",
         month = "March",
         genus = if_else(is.na(name_2020),
                         genus,
                         str_replace(name_2020, "(?s) .*", "")),
         #genus = str_replace(name_2020, "(?s) .*", ""),
         species = if_else(is.na(name_2020),
                           species,
                           str_remove(name_2020, paste0(genus, " "))),
         #species = str_remove(name_2020, paste0(genus, " ")),
         taxon = paste(genus, species, sep = " "),
         plot_id = as.character(plot_id),
         taxon_puna = NA_character_) %>%
  select(country, project, course, id, year, month, date, gradient, site, treatment, plot_id,
         functional_group, family, taxon, genus, species,
         individual_nr, plant_height_cm,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_mm, area_flag, dry_flag, wet_flag)

# End of Script ----

###############################################################


