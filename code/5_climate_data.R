## Load libraries
source("code/load_libraries.R")
# source("code/functions/toms_read.R") # could not find this function anywhere

soil.moist <- function(rawsoilmoist, soil_temp, soilclass){
  #based on appendix A of Wild 2019 (https://www-sciencedirect-com.pva.uib.no/science/article/pii/S0168192318304118#sec0095) and https://www.tomst.com/tms/tacr/TMS3calibr1-11.xlsm
  # creating df with parameters for each soil type
  soilclass.df <- tibble(
    soil = c("sand", "loamy_sand_A", "loamy_sand_B", "sandy_loam_A", "sandy_loam_B", "loam", "silt_loam", "peat"),
    a = c(-3E-9, -1.9e-8, -2.3e-8, -3.8e-8, -9e-10, -5.1e-8, 1.7e-8, 1.23e-7),
    b = c(1.61192e-4, 2.6561e-4, 2.82473e-4, 3.39449e-4, 2.61847e-4, 3.97984e-4, 1.18119e-4, 1.44644e-4),
    c = c(-0.109956505, -0.154089291, -0.167211156, -0.214921782, -0.158618303, 0.291046437, -0.101168511, 0.202927906),
    AirCalib = rep(57.64530756, 8), # a constant across all soil types, don't know exactly what this does
    AirPuls = rep(56.88867311, 8), # a constant across all soil types, don't know exactly what this does
    DilVol = rep(-59.72975311, 8) # a constant across all soil types, don't know exactly what this does
  )

  #filtering soilclass.df based on which soilclass was entered in the function
  soilclass.df <- soilclass.df %>%
    filter(
      soil == soilclass
    )

  #calculating the volumetric soil moisture with the parameters corresponding to the soil class and the raw soil moisture from the logger
  volmoist = (soilclass.df$a * rawsoilmoist^2) + (soilclass.df$b * rawsoilmoist) + soilclass.df$c

  #temperature correction
  temp_ref <- 24
  delta_air <- 1.91132689118083
  delta_water <- 0.64108
  delta_dil <- -1.270246891 # this is delta-water - delta_air
  # we don't know what this does or what the variables do, but the result is the same as in excel
  temp_corr <- rawsoilmoist + ((temp_ref-soil_temp) * (delta_air + delta_dil * volmoist))
  # volumetric soil moisture with temperatue correction
  volmoistcorr <- with(soilclass.df,
                       ifelse(rawsoilmoist>AirCalib,
                              (temp_corr+AirPuls+DilVol*volmoist)^2*a+(temp_corr+AirPuls+DilVol*volmoist)*b+c,
                              NA))
  return(volmoistcorr)
  # return(volmoist) #let's just use the soil moisture without temperature correction for now
}

## Read in files
files <- dir(path = "data/climate/raw_climate", pattern = ".*\\.csv$", full.names = TRUE, recursive = TRUE)

## Read in the id mapping file
id_map <- read_xlsx("data/climate/raw_climate/id_toms_puna.update.xlsx")

## Function to read in data
temp_raw <- map_df(set_names(files), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "filename")

## Merge individual logger data into one big data frame
climate <- temp_raw %>%
  # rename column names
  rename(id = X1,
         date_time = X2,
         time_zone = X3,
         soil_temperature = X4,
         ground_temperature = X5,
         air_temperature = X6,
         raw_soilmoisture = X7,
         shake = X8,
         error_flag = X9) %>%
  mutate(date_time = ymd_hm(date_time)) %>%
  # extract logger id from filename
  mutate(logger_id = as.numeric(str_extract_all(filename, "\\d{8}")))  %>%
  mutate(volumetric_soilmoisture = soil.moist(rawsoilmoist = raw_soilmoisture, soil_temp = soil_temperature, soilclass = "peat")) # according to https://doi.org/10.1029/2010GB003787 the organic layer of the soil (which our loggers are measuring in) consists of peat in these regions

## attach all logger-specific, unchanging data descriptors to big climate data frame
climate <- cbind(climate, id_map[match(climate$logger_id, id_map$.id), c(-1,-ncol(id_map))])

## remove duplicate rows (some of the 2019 data sheets extend into 2020)
climate <- climate[duplicated(climate[,c(3,11)]) == FALSE,]

## identifying and flagging parts of time-series where loggers were not in the field, we observe a huge jump in raw_soilmoisture quantiles between .25 and .26. This indicates, to us, that this is where real data was recorded
climate$error_flag[climate$raw_soilmoisture < mean(quantile(climate$raw_soilmoisture, c(.25, .26)))] <- 1

## extend error flags of soil moisture by one day to be conservative about what data to trust
Extend_days <- aggregate(date(date_time) ~ logger_id, data = climate[climate$error_flag == 0,], min) # find first date for which no error flag is thrown for each logger individually
colnames(Extend_days)[2] <- "date"
Extend_days <- rbind(Extend_days, data.frame(logger_id = Extend_days$logger_id, date = Extend_days$date+1)) # extend minimum date by 1 day and retain initial day
Extend_flags <- paste(climate$logger_id, date(climate$date_time), sep="_") %in% paste(Extend_days$logger_id, Extend_days$date, sep="_") # find the matching rows (which should be applied an error flag to)
climate$error_flag[Extend_flags] <- 1

## QUE treatment fix (all treatments at QUE are Burned, we just don't know the plotid for each loggerid)
climate[climate$Site == "QUE", "Treatment"] <- "B"

## DATA SAVING
write.csv(climate, file = "data/climate/Puna_Climate_clean.csv")

## Plotting
plot_temp <- aggregate(air_temperature~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], mean)
plot_temp$sd <- aggregate(air_temperature~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], sd)[,4]
colnames(plot_temp)[1] <- "date"
plot_temp <- plot_temp[(plot_temp$Site != "QUE"), ]

ggplot(plot_temp, aes(x = date, y = air_temperature, fill = Treatment)) +
  geom_line(aes(col = Treatment)) +
  geom_ribbon(aes(ymin=air_temperature-sd/2, ymax=air_temperature+sd/2), col = NA, alpha=0.3) +
  facet_wrap(~ Site, scales = "free") + theme_bw() + labs(x = "", y = "Air Temperature [Â°C]")
ggsave(filename="data/climate/AirTemp.png", width = 16, height = 9)
