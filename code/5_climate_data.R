### get climate data

source("code/load_libraries.R")
source("code/functions/toms_read.R")

### Read in files
files <- dir(path = "data/climate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)


# Function to read in data
temp_raw <- map_df(set_names(files), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "filename")

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


ggplot(climate, aes(x = date_time, y = air_temperature)) +
  geom_line() +
  facet_wrap(~ logger_id)


