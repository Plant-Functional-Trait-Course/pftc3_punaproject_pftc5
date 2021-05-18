## Load libraries
source("code/load_libraries.R")
# source("code/functions/toms_read.R") # could not find this function anywhere

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
  mutate(logger_id = as.numeric(str_extract_all(filename, "\\d{8}")))

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

climate <- as_tibble(climate) %>%
  select(date_time, site = Site, treatment = Treatment, plot_id = PlotID, air_temperature, ground_temperature, soil_temperature, raw_soilmoisture, elevaiton = Elevation, latitude = Latitude, longitude = Longitude, logger_id, error_flag)

## DATA SAVING
write_csv(climate, file = "data/climate/PFTC3_Puna_PFTC5_2019_2020_Climate_clean.csv")

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
