#### CREATE DATA DIC FOR READMEFILE ####

source("code/load_libraries.R")
#source("R/Rgathering/DownloadCleanData.R")
library(dataDocumentation)

# get attribute table
attribute_table <- read_csv(file = "clean_data/PFTC3_Puna_PFTC5_attribute_table.csv")


# community data
comm <- read_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv")

comm_dic <- make_data_dictionary(data = comm,
                     description_table = attribute_table,
                     table_ID = NA_character_)


# community structure data
comm_struc <- read_csv("clean_data/PFTC3-Puna-Peru_2018-2019_CommunityStructure_clean.csv")

comm_struc_dic <- make_data_dictionary(data = comm_struc,
                                 description_table = attribute_table,
                                 table_ID = "comm_structure")


# biomass
biomass <- read_csv("clean_data/Puna_Peru_2019_Biomass_clean.csv")

biomass_dic <- make_data_dictionary(data = biomass,
                                 description_table = attribute_table,
                                 table_ID = "biomass")

# traits
traits <- read_csv("clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_FunctionalTraits_clean.csv")

trait_dic <- make_data_dictionary(data = traits,
                                    description_table = attribute_table,
                                    table_ID = "traits") |>
  mutate(`Variable range or levels` = if_else(`Variable range or levels` == "13-Jan - SVH1234", "AAA0656 - SVH1234", `Variable range or levels`))

# climate
climate <- read_csv("clean_data/PFTC3_Puna_PFTC5_2019_2020_Climate_clean.csv")

climate_dic <- make_data_dictionary(data = climate,
                                  description_table = attribute_table,
                                  table_ID = "climate")

flux <- read_csv("clean_data/PFTC3_Puna_PFTC5_Peru_2018_2020_Cflux.csv")
respiration <- read_csv("clean_data/PFTC3_Puna_PFTC5_Peru_2018_2020_rsoil.csv")
water <- read_csv("clean_data/PFTC3_Puna_PFTC5_Peru_2018_2020_wflux.csv")

cflux_dic <- make_data_dictionary(data = flux,
                                    description_table = attribute_table,
                                    table_ID = "cflux")

resp_dic <- make_data_dictionary(data = respiration,
                                 description_table = attribute_table,
                                 table_ID = "rsoil")

water_dic <- make_data_dictionary(data = water,
                                 description_table = attribute_table,
                                 table_ID = "wflux")


write_xlsx(list(comm = comm_dic,
                comm_Struc = comm_struc_dic,
                biomass = biomass_dic,
                trait = trait_dic,
                climate = climate_dic,
                cflux = cflux_dic,
                resp = resp_dic,
                water = water_dic),
           path = "clean_data/PFTC3-Puna-PFTC5_Peru_2018-2020_data_dictionary.xlsx")
