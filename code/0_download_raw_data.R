# Download raw data from OSF

source("code/load_libraries.R")

# Download raw data
# coordinates
download_PFTC_data(country = "Peru",
                   datatype = "meta",
                   path = "data")

# traits
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

# community data
download_PFTC_data(country = "Peru",
                   datatype = "raw_community",
                   path = "data")

# climate
download_PFTC_data(country = "Peru",
                   datatype = "raw_climate",
                   path = "data/climate")

# Unzip files
zipFile <- "data/climate/tomst_2019/"
outDir <- "data/climate/tomst_2019/"
unzip(zipFile, exdir = outDir)

zipFile <- "data/climate/tomst_2020/"
outDir <- "data/climate/tomst_2020/"
unzip(zipFile, exdir = outDir)
