library(data.table)
library(ggplot2)

#importing runoff values
runoff_info <- readRDS('./data/raw/runoff_eu_day.rds')
stations_info <- readRDS('./data/stations_info_raw.rds')
head(runoff_info)
str(runoff_info)
summary(runoff_info) #-999 values in Value (NA)

#setting the column names and formats to match stations_info 
colnames(runoff_info) <- c('ID', 'Date', 'Value')
runoff_info[, ID := factor(ID)]
runoff_info[, Date := as.Date(Date)]
saveRDS(runoff_info, './data/runoff_info_raw.rds')

#checking on a random station
station_runoff <- runoff_info[ID == 6935146]
ggplot(data = station_runoff,                         #obvious negative NA's
       aes(x = Date, y = Value)) +
  geom_point() +
  geom_line()

#checking the data
length(unique(runoff_info$ID)) #202 stations, but in stations_info we had 208
#checking on the NA's for each station
missing_values <- runoff_info[Value < 0, .(missing = .N), by = ID]
missing_values
sum(missing_values$missing) #52 141 missing values in total in 103 stations

#checking the NA's compared to the total size
#adding the number of measurements for each station (size)
sample_size <- runoff_info[, .(size = .N), by = ID]
stations_info <- merge(stations_info, sample_size, by = "ID", all = TRUE)
#adding the amount of missing values for each station
stations_info <- missing_values[stations_info, on = 'ID']
stations_info[is.na(missing), missing := 0]
#changing the missing column to a precentage
stations_info[, missing := missing / size]
stations_info[, missing := round(missing, 3)]
#checking the precentages
unique(stations_info$missing) #we have two high amounts of NA's, 53% and 11%
stations_info[missing==0.530, unique(ID)] #station 6970250 RU
stations_info[missing==0.114, unique(ID)] #station 6744500 RO
#looking at the data from the stations that have a lot of NA's (DON'T USE)
station_runoff <- runoff_info[ID == 6970250]
ggplot(data = station_runoff,
       aes(x = Date, y = Value)) +
  geom_point() +
  geom_line()

station_runoff <- runoff_info[ID == 6744500]
ggplot(data = station_runoff,
       aes(x = Date, y = Value)) +
  geom_point() +
  geom_line()
stations_info <- stations_info[-(ID == 6970250)]
stations_info <- stations_info[-(ID == 6744500)]
#removing the missing values
runoff_info <- runoff_info[Value >= 0]

#in which year did stations start and stop measuring runoff, to determine range
station_time <- runoff_info[, .(start = min(year(Date)), 
                                end = max(year(Date))), by = ID]
table(station_time$end) #between 2010 and 2016
hist(station_time$start) #between 1911 and 1920 maybe
#we are looking for changes before and after 1980
min_year <- 1980 - 30 #1950, most stations started recording before, so great
max_year <- 1980 + 30 #2010, most stations stopped recording after, so great
#removing stations that don't fit the year criteria
stations_info <- stations_info[station_time, on  = 'ID']
stations_info <- stations_info[start <=  min_year & end >= max_year & 
                                 size >= 30 * 2 * 365]
length(unique(stations_info$ID)) #now we only have 173 stations
country_info_new <- summary(stations_info$Country) #lost some countries too
#cleaning the runoff data as well
runoff_info <- runoff_info[ID %in% stations_info$ID]
summary(runoff_info)

saveRDS(stations_info, './data/stations_info_edited.rds')
saveRDS(runoff_info, './data/runoff_values_edited.rds')
saveRDS(country_info_new, './data/country_info_new.rds')
