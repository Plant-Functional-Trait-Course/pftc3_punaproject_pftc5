## Load libraries
source("code/load_libraries.R")
source("code/coordinates.R")

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
  mutate(logger_id = as.numeric(str_extract_all(filename, "\\d{8}"))) %>%
  ## attach all logger-specific, unchanging data descriptors to big climate data frame
  left_join(id_map, by = "logger_id") %>%
  # remove data before starting date
  filter(date_time > starting_date) %>%
  # remove all data from QUE, has been in the field from 13.3 afternoon - 15.3 afternoon, trustworthy data is from 14.3.2021 01:00:00 - 15.3.2021 12:00:00 (ish). This is very little compared to other sites. Chuck it!
  filter(site != "QUE") %>%
  # remove not needed cols
  select(-id, -c(Latitude:data_collected_2020)) %>%
  ## remove duplicate rows (some of the 2019 data sheets extend into 2020)
  group_by(date_time, logger_id, site, treatment, plot_id, soil_temperature, ground_temperature, air_temperature, raw_soilmoisture) %>%
  mutate(n = 1:n()) %>%
  filter(n != 2) %>%
  mutate(treatment = if_else(site == "TRE" & treatment == "B", "NB", treatment),
         plot_id = as.character(plot_id)) %>%
  left_join(coordinates, by = c("site", "treatment", "plot_id")) %>%
  select(date_time, site, treatment, plot_id, air_temperature, ground_temperature, soil_temperature, raw_soilmoisture, burn_year, elevation, latitude, longitude, logger_id)

## DATA SAVING
write_csv(climate, file = "clean_data/PFTC3_Puna_PFTC5_2019_2020_Climate_clean.csv")


# checks
# climate %>%
#   filter(site %in% c("ACJ"),
#          treatment %in% c("NB"),
#          date_time > "2020-07-01 01:00:00" & date_time < "2020-07-30 01:00:00"
#          ) %>%
#   ggplot(aes(x = date_time, y = air_temperature)) +
#   geom_line() +
#   facet_wrap(~ logger_id)


# ## attach all logger-specific, unchanging data descriptors to big climate data frame
# climate <- cbind(climate, id_map[match(climate$logger_id, id_map$.id), c(-1,-ncol(id_map))])
#
# ## remove duplicate rows (some of the 2019 data sheets extend into 2020)
# climate <- climate[duplicated(climate[,c(3,11)]) == FALSE,]

## identifying and flagging parts of time-series where loggers were not in the field, we observe a huge jump in raw_soilmoisture quantiles between .25 and .26. This indicates, to us, that this is where real data was recorded
# climate$error_flag[climate$raw_soilmoisture < mean(quantile(climate$raw_soilmoisture, c(.25, .26)))] <- 1
#
# ## extend error flags of soil moisture by one day to be conservative about what data to trust
# Extend_days <- aggregate(date(date_time) ~ logger_id, data = climate[climate$error_flag == 0,], min) # find first date for which no error flag is thrown for each logger individually
# colnames(Extend_days)[2] <- "date"
# Extend_days <- rbind(Extend_days, data.frame(logger_id = Extend_days$logger_id, date = Extend_days$date+1)) # extend minimum date by 1 day and retain initial day
# Extend_flags <- paste(climate$logger_id, date(climate$date_time), sep="_") %in% paste(Extend_days$logger_id, Extend_days$date, sep="_") # find the matching rows (which should be applied an error flag to)
# climate$error_flag[Extend_flags] <- 1

# ## QUE treatment fix (all treatments at QUE are Burned, we just don't know the plotid for each loggerid)
# climate[climate$Site == "QUE", "Treatment"] <- "B"




## Plotting Air Temp
plot_temp <- aggregate(air_temperature~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], mean)
plot_temp$sd <- aggregate(air_temperature~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], sd)[,4]
colnames(plot_temp)[1] <- "date"
Elev_mean <- aggregate(Elevation~Site, data = climate[climate$error_flag == 0, ], mean)
plot_temp$Elevation <- round(Elev_mean$Elevation[match(plot_temp$Site, Elev_mean$Site)], 0)
plot_temp$Title <- paste0(plot_temp$Site, " (~", plot_temp$Elevation, "m)")
plot_temp <- plot_temp[(plot_temp$Site != "QUE"), ]


ggplot(plot_temp, aes(x = date, y = air_temperature, fill = Treatment)) +
  geom_line(aes(col = Treatment)) +
  geom_ribbon(aes(ymin=air_temperature-sd/2, ymax=air_temperature+sd/2), col = NA, alpha=0.3) +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Air Temperature [°C]")
ggsave(filename="data/climate/AirTemp.png", width = 16, height = 9)


ggplot(plot_temp, aes(y = air_temperature, x = Treatment, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Mean Air Temperature [°C]")
ggsave(filename="data/climate/AirTempBox.png", width = 16, height = 9)
aggregate(air_temperature~Site+Treatment, data = plot_temp, mean)

ggplot(plot_temp, aes(y = sd, x = Treatment, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Standard Deviation of Air Temperature [°C]")
ggsave(filename="data/climate/AirTempSD.png", width = 16, height = 9)
aggregate(sd~Site+Treatment, data = plot_temp, mean)

## Plotting SoilMoi
plot_temp <- aggregate(volumetric_soilmoisture~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], mean)
plot_temp$sd <- aggregate(volumetric_soilmoisture~date(date_time)+Site+Treatment, data = climate[climate$error_flag == 0, ], sd)[,4]
colnames(plot_temp)[1] <- "date"
Elev_mean <- aggregate(Elevation~Site, data = climate[climate$error_flag == 0, ], mean)
plot_temp$Elevation <- round(Elev_mean$Elevation[match(plot_temp$Site, Elev_mean$Site)], 0)
plot_temp$Title <- paste0(plot_temp$Site, " (~", plot_temp$Elevation, "m)")
plot_temp <- plot_temp[(plot_temp$Site != "QUE"), ]


ggplot(plot_temp, aes(x = date, y = volumetric_soilmoisture, fill = Treatment)) +
  geom_line(aes(col = Treatment)) +
  geom_ribbon(aes(ymin=volumetric_soilmoisture-sd/2, ymax=volumetric_soilmoisture+sd/2), col = NA, alpha=0.3) +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Soil Moisture [% vol.]")
ggsave(filename="data/climate/VolMoi.png", width = 16, height = 9)


ggplot(plot_temp, aes(y = volumetric_soilmoisture, x = Treatment, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Mean Soil Moisture [% vol.]")
ggsave(filename="data/climate/VolMoiBox.png", width = 16, height = 9)
aggregate(volumetric_soilmoisture~Site+Treatment, data = plot_temp, mean)

ggplot(plot_temp, aes(y = sd, x = Treatment, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Title, scales = "free_x") + theme_bw() + labs(x = "", y = "Daily Standard Deviation of Soil Moisture [% vol.]")
ggsave(filename="data/climate/VolMoiSD.png", width = 16, height = 9)
aggregate(sd~Site+Treatment, data = plot_temp, mean)
