
### THIS SCRIPT LOADS AND TRANSFORMS THE DATASET INTO TWO WORKING DATASETS
### ONE HORIZONTAL FOR ANALYSIS AND ONE VERTICAL FOR PLOTTING

# Load Stations Locations
stations <- read.csv('data_input/Weather Stations List.csv', sep = ',', stringsAsFactors = FALSE)
# str(stations)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Station Locations imported'))

# Load the file weather in data frame weather
weather <- read_excel('data_input/weather.xlsx')
weather$date <- as.Date(weather$date)
weather$temp_gap <- weather$temp_max-weather$temp_min
weather_param <- names(weather)[names(weather) != 'date']
weather_param <- c(sort(weather_param[!grepl('temp',weather_param)]),
                   sort(weather_param[grepl('temp',weather_param)]))
# str(weather)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Weather data imported'))

# Create reference list of pollutants
color1 <- 'coral'
color2 <- 'orange'
pollutants <- data.frame(param_index = c(8, 1, 14, 9, 30, 43, 6, 35, 37, 44, 7, 12, 39, 10, 38, 42, 20),
                         pollutant = c('NO2', 'SO2', 'O3', 'PM2.5', 'BEN', 'CH4', 'CO', 'EBE', 'MXY', 'NMHC', 'NO', 'NOx', 'OXY','PM10', 'PXY', 'TCH', 'TOL'),
                         color = c(color1, color1, color1, color1, color2, color2, color2, color2, color2, color2, color2, color2, color2, color2, color2, color2, color2))
pollutants$color <- as.character(pollutants$color)
# str(pollutants)

# Create vectors with year and month to read data files
years <- seq(from = 11, to = 16, by = 1)
months <- seq(from = 1, to = 12, by = 1)

# Load the files in list temp_pollution with additional columns year and month
temp_pollution <- list()
increment <- 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    temp_pollution[[increment]] <- read.csv(paste0('data_input/workgroup data/hourly_data_',years[i],'_',months[j],'.csv'), sep=',', stringsAsFactors = FALSE)
    temp_pollution[[increment]] <- cbind(temp_pollution[[increment]],
                                         year = rep(paste0('20',years[i]), times = nrow(temp_pollution[[increment]])),
                                         month = rep(paste0(months[j]), times = nrow(temp_pollution[[increment]])))
    increment <- increment + 1
  }
}
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Pollution files imported.. Starting treatment..'))

# Create data frame pollution_dataframe with all data combined
pollution <- data.frame()
for (i in 1:length(temp_pollution)){
  pollution <- rbind(pollution, temp_pollution[[i]])
}
pollution$parameter <- as.factor(pollution$parameter)
pollution$station <- as.factor(pollution$station)
# print(levels(pollution$station))
# str(pollution)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'All pollution files combined..'))

# Add pollutant and date columns
pollution$date <- paste(pollution$year, pollution$month, pollution$day, sep='-')
pollution$date <- as.Date(pollution$date)
# str(pollution)
pollution <- merge(pollution, pollutants, by.x = 'parameter', by.y = 'param_index')
pollution$parameter <- NULL
pollution$color <- NULL
# str(pollution)

# List the pollutants used in the dataset
list_pollutants <- as.vector(aggregate(pollution[,'value'], FUN = sum, by = list(pollutant = pollution$pollutant), na.rm = T)[,'pollutant'])
list_pollutants <- list_pollutants[match(pollutants$pollutant,list_pollutants)]
list_pollutants <- list_pollutants[!is.na(list_pollutants)]
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Pollutants in the dataset: ',paste(unlist(list_pollutants), collapse = ', ')))

# Convert factors in pollutant to columns
pollution <- spread(pollution, pollutant, value)
# str(pollution)

# # Aggregate average per day with station details
# pollution_daily <- aggregate(pollution_dataframe[,list_pollutants],
#                              FUN = mean,
#                              by = list(station = pollution_dataframe$station,
#                                        date = pollution_dataframe$date),
#                              na.rm = TRUE)
# str(pollution_daily)

# Aggregate average per day and across stations
pollution_daily_h <- aggregate(pollution[,list_pollutants],
                             FUN = mean,
                             by = list(date = pollution$date),
                             na.rm = TRUE)
# str(pollution_daily_h)

# Add month and week indexes
pollution_daily_h$month <- as.Date(cut(pollution_daily_h$date, breaks = 'month'))
pollution_daily_h$week <- as.Date(cut(pollution_daily_h$date, breaks = 'week', start.on.monday = TRUE))
# str(pollution_daily_h)

# Add weather information
pollution_daily_h <- merge(pollution_daily_h, weather, by.x = 'date', by.y = 'date')
# str(pollution_daily_h)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Weather info added to dataset..'))

pollution_daily_v <- gather(pollution_daily_h, c(list_pollutants,weather_param), key = variable, value = value)
# str(pollution_daily_v)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Pollution daily data ready to use!'))

# Export to RDS files and RData file for RMarkdown
saveRDS(pollution_daily_h, 'data_output/pollution_daily_h.rds')
saveRDS(pollution_daily_v, 'data_output/pollution_daily_v.rds')
save(list = c('pollution_daily_h',
              'pollution_daily_v',
              'list_pollutants',
              'weather',
              'pollutants',
              'weather_param',
              'stations'),
     file = 'data_output/RMarkdown_Objects.RData'
     )

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
            'Pollution daily files exported!'))

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Working dataset ready!'))


