## ----downloadData
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "gs8u6",
         file = "PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv",
         path = "clean_data",
         remote_path = "community")


#Download traits data from OSF
get_file(node = "gs8u6",
         file = "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv",
         path = "clean_data",
         remote_path = "traits")

#Download traits data from OSF
get_file(node = "gs8u6",
         file = "Puna_Peru_2019_Biomass_clean.csv",
         path = "clean_data",
         remote_path = "biomass")

#Download climate data from OSF
# get_file(node = "gs8u6",
#          file = "China_2013_2016_AirTemp.csv",
#          path = "clean_data",
#          remote_path = "Climate")

## ----
