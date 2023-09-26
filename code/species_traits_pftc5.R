## PFTC5 2020 PERU ----

# Load libraries ----------------------------------------------------------

# source this file if needed
#source("code/load_libraries.R")

# Dictionary -----------------------------------------------------------------

source("code/taxon_correction.R")

# Get valid IDs -----------------------------------------------------------

all_codes2 <- get_PFTC_envelope_codes(seed = 6)

# Leaf traits -------------------------------------------------------------

trait_2020 <- read_csv("data/PFTC5_Peru_2020_LeafTraits.csv") %>%
  clean_names() %>%
  mutate(project = str_to_lower(project),
         project = case_when(
           project == "T" ~ "trait",
           project == "S" ~ "sean",
           project == "R" ~ "trait", # R its for Rangild project intraspecific variability?
           project == "t" ~ "trait",
           project == "s" ~ "sean",
           project == "r" ~ "trait", # R its for Rangild project intraspecific variability?
           project == "Sean" ~ "sean",
           project == "Trait" ~ "trait",
           TRUE ~ project
         ),
         id = str_to_upper(str_trim(id, side = "both")),
         site = str_to_upper(str_trim(site, side = "both")),
         site = if_else(site == "ACA", "ACJ", site),
         experiment = str_to_upper(experiment),
         experiment = if_else(experiment == "OFF=PLOT", NA_character_, experiment),
         genus = str_to_sentence(genus),
         species = str_to_lower(species),
         id = case_when(
           id == "CZQ3610" ~ "CYQ3610",
           id == "ADZ1845" ~ "ADZ3452", #leaves switched, checked with envelopes
           id == "ADY3452" ~ "ADY1845", #leaves switched, checked with envelopes
           id == "CZU0738" ~ "CYU0738",
           id == "CBN2433" ~ "CBM2433",
           id == "CGY2493" ~ "CGU2493",
           id == "BLF6180" ~ "BLE6180",
           id == "CQT3635" ~ "CQT3656",
           TRUE ~ id
         ),
         experiment = if_else(id == "AIR7210", "BB", experiment),
         experiment = if_else(id == "BMP6395", "C", experiment),
         leaf_thickness_1_mm = if_else(id == "AVH1607", 0.277, leaf_thickness_1_mm),
         leaf_thickness_1_mm = if_else(id == "BIX3249", 0.289, leaf_thickness_1_mm),
         leaf_thickness_2_mm = if_else(id == "BIX3249", 0.296, leaf_thickness_2_mm),
         leaf_thickness_2_mm = if_else(id == "AJA2506", 0.244, leaf_thickness_2_mm),
         leaf_thickness_3_mm = if_else(id == "ADB5258", 0.197, leaf_thickness_3_mm),
         leaf_thickness_3_mm = if_else(id == "CFN6705", 0.181, leaf_thickness_3_mm),
         leaf_thickness_2_mm = if_else(leaf_thickness_2_mm == -0.545, 0.545,
                                       leaf_thickness_2_mm),
         #Fixing wrongly entered plant height
         plant_height_cm = if_else(id == "CXC6033", 2.3, plant_height_cm),
         plant_height_cm = if_else(id == "BVA6015", 20, plant_height_cm),
         plant_height_cm = if_else(id == "CWT5112", 3.9, plant_height_cm),
         plant_height_cm = if_else(id == "CEA4386", 8.9, plant_height_cm),
         plant_height_cm = if_else(id == "CEC4306", 7.1, plant_height_cm),
         plant_height_cm = if_else(id == "BGF6708", 10.1, plant_height_cm),
         plant_height_cm = if_else(id == "BJD7505", 15.0, plant_height_cm),
         plant_height_cm = if_else(id == "ADP7756", 10.8, plant_height_cm),
         plant_height_cm = if_else(id == "AGV6997", 39.4, plant_height_cm),
         plant_height_cm = if_else(id == "CXP8061", 74.6, plant_height_cm),
         plant_height_cm = if_else(id == "AUQ9983", 9.5, plant_height_cm),
         plant_height_cm = if_else(id == "AFQ4784", 28.3, plant_height_cm),
         plant_height_cm = if_else(id == "AJN5736", 2.8, plant_height_cm),
         plant_height_cm = if_else(id == "CDO4494", 2.0, plant_height_cm),
         plant_height_cm = if_else(id == "AAX4219", 11.7, plant_height_cm),
         plant_height_cm = if_else(id == "AEW0937", 11.7, plant_height_cm),
         plant_height_cm = if_else(id == "AZX9133", 24.8, plant_height_cm),
         plant_height_cm = if_else(id == "BHC0873", 26.5, plant_height_cm),
         plant_height_cm = if_else(id == "CJD9340", 34.2, plant_height_cm),
         plant_height_cm = if_else(id == "CLD8557", 37.3, plant_height_cm),
         plant_height_cm = if_else(id == "CJN4110", 13, plant_height_cm),
         plant_height_cm = if_else(id == "CEX5438", 2.6, plant_height_cm),
         plant_height_cm = if_else(id == "ACF7763", 9.6, plant_height_cm),
         plant_height_cm = if_else(id == "CJT3535", 34.2, plant_height_cm),
         plant_height_cm = if_else(id == "ABR6458", 18, plant_height_cm),
         plant_height_cm = if_else(id %in% c("CWY6193", "CWM3970"), 8.9, plant_height_cm),
         plant_height_cm = if_else(id %in% c("ASR6625", "ACU2880"), 7.5, plant_height_cm),
         plant_height_cm = if_else(id %in% c("BGV5645", "BHD6397"), 12.4, plant_height_cm),
         plant_height_cm = if_else(id %in% c("AVB4528", "AVJ3855", "AVE4471", "AVG6257"), 6.1, plant_height_cm),
         plant_height_cm = if_else(id %in%   c("AGI7187", "AGM1060", "AGQ6196"), 5.9, plant_height_cm),
         plant_height_cm = if_else(id %in% c("CJS1572", "CKF4385", "CIY4513", "CKJ4817", "CKR1084"), 120, plant_height_cm),
         plant_length_cm = if_else(id == "ABR6458", 20.5, plant_length_cm),
         #Fix wrong sites and wrong plot id
         site = if_else(id == "BEA0992", "ACJ", site),
         site = if_else(id %in% c("AKP1497", "AYS4952"), "QUE", site),
         plot_id = if_else(id == "BMP6395", 1, plot_id),
         plot_id = if_else(id == "CWF2802", 3, plot_id),
         #correcting leaf ID mistakes, or adding leaf ID where there should have been one
         leaf_nr = if_else(id == "BKW1860", 5, leaf_nr),
         leaf_nr = if_else(id == "ABS7148", 5, leaf_nr),
         leaf_nr = if_else(id == "CMH0413", 5, leaf_nr),
         leaf_nr = if_else(id == "BNT3287", 1, leaf_nr),
         leaf_nr = if_else(id == "BOO5640", 2, leaf_nr),
         leaf_nr = if_else(id == "BOB7266", 3, leaf_nr),
         leaf_nr = if_else(id == "BNX0682", 4, leaf_nr),
         leaf_nr = if_else(id == "BQF6005", 5, leaf_nr),
         leaf_nr = if_else(id == "ATJ5219", 2, leaf_nr),
         leaf_nr = if_else(id == "AAH3401", 3, leaf_nr),
         leaf_nr = if_else(id == "ATX9292", 4, leaf_nr),
         leaf_nr = if_else(id == "ADF3723", 5, leaf_nr),
         leaf_nr = if_else(id == "CPQ6887", 3, leaf_nr),
         leaf_nr = if_else(id == "CPM4170", 4, leaf_nr),
         leaf_nr = if_else(id == "ABL1039", 5, leaf_nr),
         leaf_nr = if_else(id == "CMT6881", 4, leaf_nr),
         #These are not ITV individuals and should not have leaf ID, removing leaf_nr info
         leaf_nr = if_else(id %in% c("CIG8658", "BPR5529", "AFR0503"), NA_real_, leaf_nr),
         #Correcting mistakes in individual number. Some are typos, some are duplicates that are given new names (often 11 or 12)
         individual_nr = if_else(id == "CPQ6887", 2, individual_nr),
         individual_nr = if_else(id == "CNW9172", 3, individual_nr),
         individual_nr = if_else(id == "CIG8658", 2, individual_nr),
         individual_nr = if_else(id == "BMC0849", 3, individual_nr),
         individual_nr = if_else(id == "CXX4125", 11, individual_nr),
         individual_nr = if_else(id == "BZH3536", 3, individual_nr),
         individual_nr = if_else(id == "APU4440", 12, individual_nr),
         #Here each line is one individual with several leaves that change to the same individual nr.
         individual_nr = if_else(id %in% c("CIP5336", "CNY7719", "CIT7207", "AFR0503"), 2, individual_nr),
         individual_nr = if_else(id %in% c("AYN2714", "ATY7432", "CEY7459", "AQF9992", "AQE5913"), 11, individual_nr),
         individual_nr = if_else(id %in% c("AQL4649", "AQK5616", "AQJ2521", "AQI6603", "AQM0288"), 12, individual_nr),
         individual_nr = if_else(id %in% c("CNP7038", "CNX3247", "COB5911"), 2, individual_nr),
         individual_nr = if_else(id %in% c("AJH6752", "CDQ3517", "CDU6030", "CDR9450", "CAS0411"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CDS3004", "CAW9605", "CDP3189"), 12, individual_nr),
         individual_nr = if_else(id %in% c("CZA2309", "CZF0686", "CZE7072", "CZJ5895", "CZN4729"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CDL6900", "CBF5268", "CBE4301", "CAA3576", "COY3821"), 12, individual_nr),
         individual_nr = if_else(id %in% c("CYF0619", "CXS5928", "CXW5856", "CYA3349", "CYE9151"), 11, individual_nr),
         individual_nr = if_else(id %in% c("BOJ2251", "BON0490", "BNO6456", "BNS6580", "BOA7727"), 11, individual_nr),
         individual_nr = if_else(id %in% c("AGG8353", "AGC5581", "AFY5122", "AFQ4784", "AFU4318"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CZI4416", "AGE7337", "AGI7187", "AGM1060", "AGQ6196"), 11, individual_nr),
         individual_nr = if_else(id %in% c("APQ5417", "APM3849", "CET3376", "CFB5184", "CEX5438"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CPB8831", "CPZ4857", "CQD3030", "CPJ4884", "CXC6033"), 11, individual_nr),
         individual_nr = if_else(id %in% c("BOM7625", "BMU4639", "BMY5426", "BLC7580", "AFV6468"), 21, individual_nr),
         individual_nr = if_else(id %in% c("BNT3287", "BOO5640", "BOB7266", "BNX0682", "BQF6005"), 11, individual_nr),
         individual_nr = if_else(id %in% c("BYH1355", "BYR4663", "BYO7151", "BYQ5420", "BYP4529"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CJV5222", "CJZ6289", "CKH7214", "CKD0965", "CKL1087"), 11, individual_nr),
         individual_nr = if_else(id %in% c("CXE6426", "CXA8168"), 12, individual_nr),
         individual_nr = if_else(id == "CPM4170", 2, individual_nr)) %>%
  # Check species and genus
  mutate(genus = plyr::mapvalues(genus, from = GenusDictionary2020$wrong,
                                 to = GenusDictionary2020$right, warn_missing = FALSE)) %>%

  mutate(species = tolower(species)) %>%
  mutate(species = plyr::mapvalues(species, from = SpeciesDictionary2020$wrong,
                                   to = SpeciesDictionary2020$right, warn_missing = FALSE)) %>%
  mutate(species = if_else(genus == "Carex" & species == "bonplandianum", "bonplandii", species),
         species = if_else(genus == "Lachemilla" & species == "umbellata", "orbiculata", species)) %>%
  mutate(genus = if_else(genus == "Calamagrostis" & species == "7", "Anatherostipa", genus),
         species = if_else(genus == "Anatherostipa" & species == "7", "hans-meyeri", species),
         species = if_else(genus == "Calamagrostis" & species == "macrochaeta", "cf. macrophylla", species),
         species = if_else(genus == "Vaccinium" & species == "bonplandianum", "floribundum", species),
         #TRE C Plot5 - lachemilla
         species = if_else(id %in% c("APS3068", "APO8208", "CNV5505", "CWL1543"), "vulcanica", species)) %>%
  #wrongly entered species
  mutate(genus = if_else(id == "AQE5913", "Halenia", genus),
         species = if_else(id == "AQE5913", "umbellata", species)) |>
  # Fix Plant_Length_cm, Bulk_nr_leaves, Length_cm
  # move length to right column
  mutate(length_cm = if_else(!is.na(plant_length_cm) & genus %in% c("Hypericum", "Lycopodium"),
                             plant_length_cm, length_cm)) %>%
  # replace length with NA
  mutate(plant_length_cm = if_else(!is.na(plant_length_cm) & genus %in% c("Halenia", "Oxalis", "Viola",
                                                                         "Vaccinium", "Hypericum",
                                                                         "Lycopodium", "Lachemilla",
                                                                         "Oreomyrhys", "Hypericum",
                                                                         "Lycopodium"), NA_real_,
                                  plant_length_cm)) %>%
  # move length to plant length for grasses
  mutate(plant_length_cm = if_else(!is.na(length_cm) & genus %in% c("Carex", "Paspalum"),
                                   length_cm, plant_length_cm)) %>%
  # remove length for vaccinium
  mutate(length_cm = if_else(!is.na(length_cm) & genus %in% c("Vaccinium", "Carex", "Paspalum"),
                             NA_real_, length_cm)) %>%

  mutate(taxon = paste(genus, species, sep = "_")) %>%
  # Fix wrong values
  mutate(wet_mass_g = if_else(id == "BXD6709", 0.109, wet_mass_g),
         wet_mass_g = if_else(id == "BEF1022", 0.281, wet_mass_g)) %>%
  # Wet mass flags
  mutate(wetflag = if_else(id %in% c("BDN3235", "CXX4125"), "Outlier_very_large_leaf", NA_character_)) %>%
  #TODO
  # add treatment (C, B, BB) for samples
  mutate(experiment = case_when(# QUE should be BB
    id == "AQA0121" ~ "BB",
    id == "BJT8862" ~ "BB",
    #WAY sites - all should be C
    id == 'CWD5069' ~ "C",
    id == 'CML0422' ~ "C",
    id == 'CMP2483' ~ "C",
    str_detect(project,"sean") ~ "OFF-PLOT",
    #all B were BB according to data doc...
    experiment == "B" ~ "BB",
    TRUE ~ experiment))

# Check dupes

trait_2020 %>%
  get_dupes()

trait_2020 %>%
  add_count(id) %>%
  distinct(n)


# Check valid IDs ---------------------------------------------------------

setdiff(select(trait_2020, hashcode = id), all_codes2)

# Leaf Area ---------------------------------------------------------------
# Import raw data files ---------------------------------------------------

read_plus <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

leafarea.raw  <- list.files(path = "data/raw_area_2020/",
                            pattern = "LeafArea",
                            full.names = T) %>%
  map_df(~read_plus(.)) %>%
  clean_names()

leafarea.raw <- leafarea.raw %>%
  mutate(filename = str_remove(filename, ".*/"),
         filename = str_remove(filename, ".csv")) %>%
  filter(!str_detect(id, "tif")) %>%
  mutate(id = str_remove(id, "[0-9]*$"),
         id = str_remove(id, ".jpeg.txt.Area"),
         id = case_when(
           id == "BVV6844 2" ~ "BVV6844",
           id == "CXX4125_1" ~ "CXX4125",
           id == "CXX4125_2" ~ "CXX4125",
           TRUE ~ id
         )) %>%
  filter(!id %in% c("2020-03-14-743-WAY_PAR0_3",
                    "2020-03-14-743_WAY_PABO_1_AT",
                    "2020-03-14-743_WAY_PABO_2_AT",
                    "CRE5297 2",
                    "BGX6274_2",
                    "out"))


# check length of IDs ------------------------------------------

leafarea.raw %>%
  mutate(idl = str_length(id)) %>%
  filter(idl != 7)

#Select data

leafarea <- leafarea.raw %>%
  select(id, leaf_area)

# CheCk wrong IDs --------------------------------------------------------

setdiff(select(leafarea, hashcode = id), all_codes2)

leafarea <- leafarea %>%
  # Remove black areas from the scans that are not leaves
  filter(!c(id == "BVL6152" & leaf_area %in% c(0.22, 0.005, 0.006)),
         !c(id == "BWJ5879" & leaf_area %in% c(0.159, 0.006, 0.006)),
         !c(id == "BXQ0542" & leaf_area %in% c(0.02)),
         !c(id == "BXU2822" & leaf_area %in% c(0.029)),
         !c(id == "CXC6033" & leaf_area %in% c(0.023, 0.127, 86.3)),
         !c(id == "CUM5583" & leaf_area %in% c(0.105)),
         !c(id == "BVP4602" & leaf_area %in% c(0.033))) %>%
  group_by(id) %>%
  summarise(leafarea_cm2 = sum(leaf_area),
            nleafscan = n(),
            .groups = "drop") %>%
  # not sure why, but code above does not work for this leaf
  mutate(leafarea_cm2 = if_else(id == "CXC6033", 2.62, leafarea_cm2))

# Check duplicated --------------------------------------------------------

leafarea %>%
  add_count(id) %>%
  summarise(sum(n)) == dim(leafarea)[1]

leafarea %>%
  add_count(id) %>%
  distinct(n)

# No duplicates IDs!!


# Correct leaves areas ----------------------------------------------------
# Select remarks form traits data

remark <- trait_2020 %>%
  select(id, remark)

# Correct leaf area, join leaf area and remarks

leafarea <- leafarea %>%
  left_join(remark, by = "id") %>%
  # Fix missing leaf area
  mutate(leafarea_cm2 = if_else(id %in% c("AFH6927", "AUV5625", "AVZ7947"), leafarea_cm2*5/2, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("AJT3939"), leafarea_cm2*2, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("AVX8706", "AWA4195"), leafarea_cm2*5/4, leafarea_cm2),
         leafarea_cm2 = if_else(id %in% c("BCP9007"), leafarea_cm2*5/1, leafarea_cm2)) %>%
  mutate(leafarea_cm2 = if_else(remark %in% c("double leaf area", "Leaf rolled, double leaf area",
                                             "rolled leaf, double leaf area", "rolled leaves,
                                             double area", "Rolled - take double area",
                                             "Rolled leaf multiple leaf area"),
                               leafarea_cm2*2, leafarea_cm2)) %>%

  # re-calculate area for leaves with large white area
  mutate(leafarea_cm2 = if_else(id == "BHQ5433", 29.24, leafarea_cm2),
         leafarea_cm2 = if_else(id == "BHU0841", 21.72, leafarea_cm2),
         leafarea_cm2 = if_else(id == "BCZ3166", 5.35, leafarea_cm2)) %>%

  # Flag data
  mutate(areaflag = if_else(id %in% c("AFH6927", "AUV5625", "AVZ7947", "AJT3939", "AVX8706", "AWA4195", "BCP9007"), "Area estimated", NA_character_),
         areaflag = if_else(id %in% c("AFL3747", "AJS0144", "AJZ6728", "AZE5327", "BBT3329"), "Leaf too white_Area missing", areaflag),
         areaflag = if_else(id %in% c("BDN3235", "CXX4125"), "Outlier_very_large_leaf", areaflag)) %>%
  select(-remark)


# Dry Mass -------------------------------------------------------------
# Import data

dry_mass <- read_csv("data/PFTC5_Peru_2020_DryWeight.csv") %>%
  clean_names() %>%
  # Fix wrong IDs
  mutate(id = case_when(
    id == "CKT177" ~ "CKT5177",
    id == "CZJ58985" ~ "CZJ5895",
    id == "CPH129" ~ "CPH1229",
    id == "BDU230" ~ "BDU0230 ",
    id == "SEAN1" ~ "SEAN1",
    id == "CYO124" ~ "CYO0124",
    id == "CYQ36610" ~ "CYQ3610",
    id == "CPN103" ~ "CPN1403",
    id == "CPR756" ~ "CPR0756",
    id == "CJKD0965" ~ "CKD0965",
    id == "ACL820" ~ "ACL0820",
    id == "APX53583" ~ "APX5383",
    id == "AEH515" ~ "AEH3515",
    id == "CND181" ~ "CND1841",
    id == "CGI70881" ~ "CGI7081",
    id == ".BOU3717" ~ "BOU3717",
    id == "ATW0.117" ~ "ATW0117",
    id == "BDU0230 " ~ "BDU0230",
    id == "BYZ5952" ~ "BYS5952",
    id == "BYI3484" ~ "BYI3448",
    id == "CYD9931" ~ "CDY9931",
    id == "CUB6632" ~ "CBU6632",
    id == "CDF2202" ~ "CDF2200",
    id == "BXM6498" ~ "BMX6498",
    id == "CKB3030" ~ "CKB3830",
    id == "CJM5916" ~ "CJM5919",
    id == "CYZ4252" ~ "CYX4252",
    id == "CYF4710" ~ "CFY4710",
    id == "AJT2939" ~ "AJT3939",
    id == "BXY3980" ~ "BWY3980",
    id == "BWN3403" ~ "BWM3403",
    id == "AVX8703" ~ "AVX8706",
    id == "BOH6123" ~ "BOH6129",
    id == "CYB2002" ~ "CYW2002",
    id == "BYL3650" ~ "BLY3650",
    id == "ADG5859" ~ "ADG5889",
    id == "BCJ9646" ~ "BCJ9446",
    id == "AXN6446" ~ "AXN6442",
    id == "CUC9131" ~ "CUC9130",
    id == "COU6998" ~ "CUO6998",
    id == "CFE3595" ~ "CFE2595",
    id == "BDK1065" ~ "BDK7065",
    id == "CGD2629" ~ "CGD1629",
    id == "BFN8407" ~ "BFV8407",
    id == "AHZ5218" ~ "AHZ5138",
    id == "BCW3456" ~ "BWC3456",
    id == "AZE5227" ~ "AZE5327",
    id == "AFN5707" ~ "AFM5707",
    id == "BOD7266" ~ "BOB7266",
    id == "AGZ7243" ~ "BGZ7243",
    id == "EGY8223" ~ "BGY8223",
    id == "APR6509" ~ "APV6509",
    id == "VIT7207" ~ "CIT7207",
    id == "BGR0051" ~ "AGR0051",
    id == "CGE7570" ~ "CGE7470",
    id == "CBF2923" ~ "CBV2923",
    id == "AJR6842" ~ "AJR6847",
    id == "CBD3438" ~ "CBD3435",
    id == "BZQ5357" ~ "BZQ3557",
    id == "CLU4763" ~ "CLJ4763",
    TRUE ~ id
  ),
  id = str_trim(id, side = "both")) %>%
  #omit duplicated IDs, have two different values
  filter(id != "APV6509") %>%
  rename(drymassflag = observations)


# IDs without dry mass ---------------------------------------------------

dry_mass %>%
  filter(is.na(dry_weight_g))

# Trait samples with missing dry mas -------------------------------------

setdiff(select(trait_2020, hashcode = id),
        select(dry_mass, hashcode = id))

# valid ids ---------------------------------------------------------------
setdiff(select(dry_mass, hashcode = id), all_codes2)


# . -----------------------------------------------------------------------
# Join data

trait_pftc5 <- trait_2020 %>%
  left_join(leafarea, by = "id") %>%
  left_join(dry_mass, by = "id") %>%
  select(id, day, project, site, experiment, plot_id, elevation, taxon, genus, species,
         individual_nr, leaf_nr, plant_height_cm, plant_length_cm, length_cm, bulk_nr_leaves,
         wet_mass_g, dry_mass_g = dry_weight_g, leaf_thickness_1_mm, leaf_thickness_2_mm,
         leaf_thickness_3_mm, leafarea_cm2, nleafscan, remark, notes1, notes2, wetflag,
         areaflag, drymassflag) %>%
  # remove Seans leaves (no more patience for this!)
  filter(project == "trait" | is.na(project)) %>%
  # fix wrong wet mass
  mutate(wet_mass_g = if_else(id == "BVM5909", 0.075, wet_mass_g),
         wet_mass_g = if_else(id == "BUJ1931", 0.069, wet_mass_g),
         wet_mass_g = if_else(id == "AKC4237", 0.063, wet_mass_g),
         # make plot_id a character
         plot_id = as.character(plot_id))




#   # fix wrong wet mass values
#   left_join(wet_mass_correction, by = "id") %>%
#   mutate(wet_mass_g = if_else(!is.na(wet_mass_envelope), wet_mass_envelope, wet_mass_g))
# filter(id == "AIS5021") %>% as.data.frame()
# Clean species names

spp_trait_dictionary_2020 <- read_csv("data/PFTC5_Peru_2020_TaxonomicDictionary.csv")

trait_pftc5 <- trait_pftc5 %>%
  left_join(spp_trait_dictionary_2020, by = "taxon") %>%
  mutate(country = "PE",
         course = "PFTC5",
         project = "T",
         gradient = 1,
         month = "March",
         year = 2020,
         leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")),
                                          na.rm = TRUE)
  ) %>%
  select(wet_mass_total_g = wet_mass_g,
         leaf_area_total_cm2 = leafarea_cm2,
         dry_mass_total_g = dry_mass_g,
         # correction - this is now the number of leafs
         nr_leaves = bulk_nr_leaves,
         #this is leaf number per an individual
         leaf_id = leaf_nr,
         dry_flag = drymassflag,
         wet_flag = wetflag,
         treatment = experiment,
         name_2020 = correct_taxon,
         number_leaves_scan = nleafscan,
         leaf_area_cm2 = leafarea_cm2,
         area_flag = areaflag,
         everything(),
         -c(genus, species)) %>%
  mutate(genus = sub("[[:space:]].*", "", name_2020),
         species = str_remove(name_2020, paste0(genus, " ")),
         taxon = paste(genus, species, sep = " ")) %>%
  # Cleaning trait values
  #these species are bulk species so number of leaves 'standardised to 1
  mutate(nr_leaves = if_else(name_2020 %in% c("Baccharis genistelloides",
                                             "Lycopodium thyoides",
                                             "Lycopodium clavatum",
                                             "Hypericum andinum"), 1,
                            nr_leaves)) %>%
  #TODO
  # each indiv needs at least 1 leaf
  mutate(nr_leaves = if_else(is.na(nr_leaves),
                             1,
                             nr_leaves)) %>%
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  mutate(leaf_area_total_cm2= if_else(genus == "Sisyrinchium", leaf_area_total_cm2 * 2, leaf_area_total_cm2),
         leaf_thickness_1_mm = if_else(genus == "Sisyrinchium", leaf_thickness_1_mm / 2, leaf_thickness_1_mm),
         leaf_thickness_2_mm = if_else(genus == "Sisyrinchium", leaf_thickness_2_mm / 2, leaf_thickness_2_mm),
         leaf_thickness_3_mm = if_else(genus == "Sisyrinchium", leaf_thickness_3_mm / 2, leaf_thickness_3_mm)) %>%
  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves,
         dry_mass_g = dry_mass_total_g / nr_leaves,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves) %>%
  # Wet and dry mass do not make sense for these species
  mutate(dry_mass_g = if_else(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
         wet_mass_g = if_else(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
         leaf_area_cm2 = if_else(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) %>%
  # Calculate SLA and LDMC
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g) %>%
  # TODO
  #Create a 'binary' bulk column - assuming these were the bulk spp. but CHECK
  mutate(bulk = if_else(name_2020 %in% c("Baccharis genistelloides",
                                        "Lycopodium thyoides",
                                        "Lycopodium clavatum",
                                        "Hypericum andinum"),
                       "bulk",
                       NA)) %>%

  # fix problematic leaves
  tidylog::mutate(wet_mass_g = if_else(ldmc > 1, NA_real_, wet_mass_g),
                  dry_mass_g = if_else(ldmc > 1, NA_real_, dry_mass_g),
                  ldmc = if_else(ldmc > 1, NA_real_, ldmc),

                  wet_mass_g = if_else(sla_cm2_g > 600, NA_real_, wet_mass_g),
                  dry_mass_g = if_else(sla_cm2_g > 600, NA_real_, dry_mass_g),
                  ldmc = if_else(sla_cm2_g > 600, NA_real_, ldmc),
                  leaf_area_cm2 = if_else(sla_cm2_g > 600, NA_real_, leaf_area_cm2),
                  sla_cm2_g = if_else(sla_cm2_g > 600, NA_real_, sla_cm2_g)) %>%


  # build date
  mutate(date = ymd(paste0("2020-03-", day)),
         plot_id = as.character(plot_id),
         bulk = as.character(bulk),

         treatment = if_else(treatment == "BB", "NB", treatment)) %>%

  #reordering columns
  select(country, course, project, id, year, month, date, gradient, site, treatment, plot_id,
         functional_group, family, taxon, genus, species,
         individual_nr, leaf_id, plant_height_cm,
         wet_mass_g, dry_mass_g, leaf_area_cm2, sla_cm2_g, ldmc,
         leaf_thickness_mm = leaf_thickness_ave_mm, area_flag, dry_flag, wet_flag )

# End of Script ---
