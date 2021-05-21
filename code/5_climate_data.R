## Load libraries
source("code/load_libraries.R")
source("code/coordinates.R")

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
  select(date_time, site, treatment, plot_id, air_temperature, ground_temperature, soil_temperature, raw_soilmoisture, burn_year, elevation, latitude, longitude, logger_id, error_flag)

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



## Plotting
plot_temp <- aggregate(air_temperature~date(date_time)+site+treatment, data = climate[climate$error_flag == 0, ], mean)
plot_temp$sd <- aggregate(air_temperature~date(date_time)+site+treatment, data = climate[climate$error_flag == 0, ], sd)[,4]
colnames(plot_temp)[1] <- "date"
#plot_temp <- plot_temp[(plot_temp$Site != "QUE"), ]
ggplot(plot_temp, aes(x = date, y = air_temperature, fill = treatment)) +
  geom_line(aes(col = treatment)) +
  geom_ribbon(aes(ymin = air_temperature - sd/2, ymax = air_temperature + sd/2), col = NA, alpha = 0.3) +
  labs(x = "", y = "Air Temperature [°C]") +
  facet_wrap(~ site, scales = "free") +
  theme_bw()


dat <- climate %>%
  mutate(date = ymd(date_time)) %>%
  group_by(date, site, treatment) %>%
  summarise(mean = mean(air_temperature),
            sd = sd(air_temperature))
ggplot(dat, aes(x = date, y = mean, fill = treatment)) +
  geom_line(aes(col = treatment)) +
  geom_ribbon(aes(ymin = mean - sd/2, ymax = mean + sd/2), col = NA, alpha = 0.3) +
  labs(x = "", y = "Air Temperature [°C]") +
  facet_wrap(~ site, scales = "free") +
  theme_bw()
ggsave(filename="AirTemp.png", width = 16, height = 9)
