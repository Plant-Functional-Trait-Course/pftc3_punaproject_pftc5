# Download raw data from OSF

source("code/load_libraries.R")

# Download raw data
download_PFTC_data(country = "Peru",
                   datatype = "meta",
                   path = "data")

download_PFTC_data(country = "Peru",
                   datatype = "raw_traits",
                   path = "data")

# Unzip files
zipFile <- "data/PFTC3_Peru_2018_Leaf_Traits.zip"
outDir <- "data/raw_traits_2018"
unzip(zipFile, exdir = outDir)

zipFile <- "data/PFTC5_Peru_2020_Raw_LeafArea.zip"
outDir <- "data/raw_area_2020"
unzip(zipFile, exdir = outDir)

zipFile <- "data/PFTC5_Peru_2020_Raw_Trait_Data.zip"
outDir <- "data/raw_traits_2020"
unzip(zipFile, exdir = outDir)

download_PFTC_data(country = "Peru",
                   datatype = "raw_community",
                   path = "data")

