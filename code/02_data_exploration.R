library(data.table)
library(ggplot2)
library(sf)
library(mapview)

runoff_info <- readRDS('./data/runoff_values_edited.rds')
stations_info <- readRDS('./data/stations_info_edited.rds')

#making yearly runoff
runoff_info[, year:=year(Date)]
runoff_year <- runoff_info[, .(Value = mean(Value)), by = .(year, ID)]
#looking at some summary statistics for yearly runoff for each station
runoff_yearly_stats <- runoff_year[, .(mean_year = round(mean(Value), 0),
                                      sd_year = round(sd(Value), 0),
                                      min_year = round(min(Value), 0),
                                      max_year = round(max(Value), 0),
                                      ID = ID), by = .(year)]
head(runoff_yearly_stats, 4)
summary(runoff_yearly_stats) # 10 NA's in sd in 6340120
#the same thing, but only for the years 
runoff_year_sum <- runoff_info[, .(Value = mean(Value)), by = .(year)]
runoff_yearly_stats_sum <- runoff_year_sum[, .(mean_year = round(mean(Value), 0),
                                               min_year = round(min(Value), 0), 
                                               max_year = round(max(Value), 0)), 
                                           by = .(year)]
saveRDS(runoff_yearly_stats_sum, './data/runoff_yearly_stats_sum')
ggplot(runoff_yearly_stats_sum, aes(year, mean_year)) +
  geom_line() +
  theme_bw() +
  ylab('Mean runoff')
#finding the ranges for low and high runoff values
runoff_yearly_stats <- runoff_yearly_stats[, max_range := max_year - mean_year]
runoff_yearly_stats <- runoff_yearly_stats[, min_range := mean_year - min_year]
#dividing the ranges into before 1980 and after
runoff_before <- runoff_yearly_stats[year<1980, .(max_range_before = mean(max_range)), by = ID]
runoff_after <- runoff_yearly_stats[year>=1980, .(max_range_after = mean(max_range)), by = ID]
runoff_max_change <- merge(runoff_before, runoff_after, by = 'ID')
#finding the difference in the ranges before and after and plotting it
runoff_max_change <- runoff_max_change[, max_diff := ((max_range_after - max_range_before) /
                                                       max_range_before) * 100, by = ID]
ggplot(data = runoff_max_change, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = max_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
saveRDS(runoff_max_change, './data/runoff_max_change')
#seeing it on a map
runoff_max_change[, change := factor('Increase')]
runoff_max_change[max_diff < 0, change := factor('Decrease')]
runoff_max_change <- merge(runoff_max_change, stations_info[,c('Lat', 'Lon', 'ID')], by = 'ID')
stations_coords <- st_as_sf(runoff_max_change,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "change")
stat_map
#for low values as well
runoff_before <- runoff_yearly_stats[year<1980, .(min_range_before = mean(min_range)), by = ID]
runoff_after <- runoff_yearly_stats[year>=1980, .(min_range_after = mean(min_range)), by = ID]
runoff_min_change <- merge(runoff_before, runoff_after, by = 'ID')
runoff_min_change <- runoff_min_change[, min_diff := ((min_range_after - min_range_before) /
                                                        min_range_before) * 100, by = ID]
ggplot(data = runoff_min_change, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = min_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
saveRDS(runoff_min_change, './data/runoff_min_change')
#on a map
runoff_min_change[, change := factor('Increase')]
runoff_min_change[min_diff < 0, change := factor('Decrease')]
runoff_min_change <- merge(runoff_min_change, stations_info[,c('Lat', 'Lon', 'ID')], by = 'ID')
stations_coords <- st_as_sf(runoff_min_change,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "change")
stat_map
#another statistics for classification
runoff_yearly_class <- runoff_year[, .(mean_year = round(mean(Value), 0),
                                       sd_year = round(sd(Value), 0),
                                       min_year = round(min(Value), 0),
                                       max_year = round(max(Value), 0)), by = .(ID)]

#first classification is based on avg yearly runoff
runoff_stats_class <- runoff_yearly_class[, .(ID, mean_year)]
runoff_stats_class[, runoff_class := factor('low')]
runoff_stats_class[mean_year >= 100 & mean_year < 1000, runoff_class := factor('medium')]
runoff_stats_class[mean_year >= 1000, runoff_class := factor('high')]
#plot shows the reason for categories
ggplot(runoff_stats_class, aes(x = ID, y = mean_year, color=runoff_class)) +
  geom_point() +
  theme_bw()
#merging classes with stations info to get the full picture
stations_info <- merge(stations_info, runoff_stats_class, by = "ID")
stations_info
#adding the changes to the stations
stations_info <- merge(stations_info, runoff_max_change[, c('ID', 'max_diff')], by = 'ID') 
stations_info <- merge(stations_info, runoff_min_change[, c('ID', 'min_diff')], by = 'ID') 
#plotting the changes for the runoff groups 
ggplot(data = stations_info, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = runoff_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Runoff class")
ggplot(data = stations_info, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = runoff_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Runoff class")

#second classification based on the location, map
stations_info[, location_class := factor('West EU')]
stations_info[(Lon >= 5 & Lon <= 20) & Lat < 55, location_class := factor('Central EU')]
stations_info[(Lon >= 3 & Lon <= 30) & Lat >= 55, location_class := factor('North EU')]
stations_info[(Lon > 30 & Lat > 45) | (Lon > 20 & Lat < 50), location_class := factor('East EU')]
#on a map
stations_coords <- st_as_sf(stations_info,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "location_class")
stat_map
#plotting the changes for the location groups 
ggplot(data = stations_info, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = location_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Location class")
ggplot(data = stations_info, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = location_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Location class")

#third classification based on altitude, map
stations_info[, altitude_class := factor('Unknown')] #lots of NA's
stations_info[Alt < 150, altitude_class := factor('Low')]
stations_info[Alt >= 150 & Alt < 300, altitude_class := factor('Medium')]
stations_info[Alt >= 300 & Alt < 600, altitude_class := factor('High')]
stations_info[Alt >= 600, altitude_class := factor('Very high')]
#categories
ggplot(stations_info, aes(x = ID, y = Alt, color=altitude_class)) +
  geom_point() +
  theme_bw()
#on a map
stations_coords <- st_as_sf(stations_info,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "altitude_class")
stat_map
#plotting the changes for the altitude groups 
ggplot(data = stations_info, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = altitude_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Altitude class")
ggplot(data = stations_info, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = altitude_class))+
  geom_bar(stat = "identity")+
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980', x='Stations', y='Change')+
  scale_fill_discrete(name = "Altitude class")

#statistics for runoff by country
stations_runoff_merged <- merge(stations_info, runoff_info, on = "ID")
runoff_country_stats <- stations_runoff_merged[, .(mean_runoff = round(mean(Value), 0),
                                sd_day = round(sd(Value), 0),
                                min_day = round(min(Value), 0),
                                max_day = round(max(Value), 0)), by = Country]
runoff_country_stats
summary(runoff_country_stats)
#mean runoff for each country
ggplot(runoff_country_stats, aes(x = Country, y = mean_runoff)) +
  geom_point() +
  theme_bw()
#getting mean yearly runoff for each country
runoff_year_country <- stations_runoff_merged[, .(Value = mean(Value)), by = .(Country, year)]
#interesting patterns for countries
ggplot(runoff_year_country, aes(x = year, y = Value)) +
  geom_point() +
  facet_wrap(~Country, scales="free") +
  theme_bw()
#plotting the changes for the countries
to_plot <- stations_runoff_merged[, .(Country, min_diff, max_diff)]
to_plot <- to_plot[, .(min_diff = mean(min_diff), max_diff = mean(max_diff)), by = Country]
ggplot(data = to_plot, 
       aes(x = reorder(Country, max_diff), y = max_diff, fill = max_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title='Changes in high runoff values after 1980', x='Countries', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
ggplot(data = to_plot, 
       aes(x = reorder(Country, min_diff), y = min_diff, fill = min_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title='Changes in low runoff values after 1980', x='Countries', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
  
#dividing the data into summer and winter
runoff_month <- runoff_info[, month := month(Date)]
runoff_month[month == 12 | month == 1 | month == 2, season := 'winter']
runoff_month[month == 3 | month == 4 | month == 5, season := 'spring']
runoff_month[month == 6 | month == 7 | month == 8, season := 'summer']
runoff_month[month == 9 | month == 10 | month == 11, season := 'autumn']
runoff_month[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

runoff_winter <- runoff_month[season == 'winter', .(Value = mean(Value)), by = .(ID, year)]
runoff_summer <- runoff_month[season == 'summer', .(Value = mean(Value)), by = .(ID, year)]

runoff_winter_stats <- runoff_winter[, .(mean_winter = round(mean(Value), 0),
                                       sd_winter = round(sd(Value), 0),
                                       min_winter = round(min(Value), 0),
                                       max_winter = round(max(Value), 0),
                                       ID = ID), by = .(year)]
head(runoff_winter_stats, 4)
summary(runoff_winter_stats)
#finding the ranges for low and high runoff values
runoff_winter_stats <- runoff_winter_stats[, max_range := max_winter - mean_winter]
runoff_winter_stats <- runoff_winter_stats[, min_range := mean_winter - min_winter]
#dividing the ranges into before 1980 and after
runoff_before <- runoff_winter_stats[year<1980, .(max_range_before = mean(max_range)), by = ID]
runoff_after <- runoff_winter_stats[year>=1980, .(max_range_after = mean(max_range)), by = ID]
runoff_max_change_winter <- merge(runoff_before, runoff_after, by = 'ID')
#finding the difference in the ranges before and after and plotting it 
runoff_max_change_winter <- runoff_max_change_winter[, max_diff := ((max_range_after - max_range_before) /
                                                                      max_range_before)*100, by = ID]
ggplot(data = runoff_max_change_winter, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = max_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980 in winter', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))

#for low values as well
runoff_before <- runoff_winter_stats[year<1980, .(min_range_before = mean(min_range)), by = ID]
runoff_after <- runoff_winter_stats[year>=1980, .(min_range_after = mean(min_range)), by = ID]
runoff_min_change_winter <- merge(runoff_before, runoff_after, by = 'ID')
runoff_min_change_winter <- runoff_min_change_winter[, min_diff := ((min_range_after - min_range_before) /
                                                                      min_range_before)*100, by = ID]
ggplot(data = runoff_min_change_winter, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = min_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980 in winter', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))

#same for summer
runoff_summer_stats <- runoff_summer[, .(mean_summer = round(mean(Value), 0),
                                         sd_summer = round(sd(Value), 0),
                                         min_summer = round(min(Value), 0),
                                         max_summer = round(max(Value), 0),
                                         ID = ID), by = .(year)]
head(runoff_summer_stats, 4)
summary(runoff_summer_stats)
#finding the ranges for low and high runoff values
runoff_summer_stats <- runoff_summer_stats[, max_range := max_summer - mean_summer]
runoff_summer_stats <- runoff_summer_stats[, min_range := mean_summer - min_summer]
#dividing the ranges into before 1980 and after
runoff_before <- runoff_summer_stats[year<1980, .(max_range_before = mean(max_range)), by = ID]
runoff_after <- runoff_summer_stats[year>=1980, .(max_range_after = mean(max_range)), by = ID]
runoff_max_change_summer <- merge(runoff_before, runoff_after, by = 'ID')
#finding the difference in the ranges before and after and plotting it
runoff_max_change_summer <- runoff_max_change_summer[, max_diff := ((max_range_after - max_range_before) /
                                                                      max_range_before)*100, by = ID]
ggplot(data = runoff_max_change_summer, 
       aes(x = reorder(ID, max_diff), y = max_diff, fill = max_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in high runoff values after 1980 in summer', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
#for low values as well
runoff_before <- runoff_summer_stats[year<1980, .(min_range_before = mean(min_range)), by = ID]
runoff_after <- runoff_summer_stats[year>=1980, .(min_range_after = mean(min_range)), by = ID]
runoff_min_change_summer <- merge(runoff_before, runoff_after, by = 'ID')
runoff_min_change_summer <- runoff_min_change_summer[, min_diff := ((min_range_after - min_range_before) /
                                                                      min_range_before)*100, by = ID]
ggplot(data = runoff_min_change_summer, 
       aes(x = reorder(ID, min_diff), y = min_diff, fill = min_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in low runoff values after 1980 in summer', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))

#plotting overall differences on a map
#for summer, first merging the lows and highs and finding a mean difference
runoff_change_summer <- merge(runoff_max_change_summer[, c('ID', 'max_diff')], 
                              runoff_min_change_summer[, c('ID', 'min_diff')], by = 'ID')
runoff_change_summer <- runoff_change_summer[, overall_diff := (max_diff + min_diff)/2]
runoff_change_summer[overall_diff > 0, change := factor('Increase')]
runoff_change_summer[overall_diff < 0, change := factor('Decrease')]
#plotting on a map
runoff_overall_summer <- merge(runoff_change_summer, stations_info[, c('Lon', 'Lat', 'ID')], by = 'ID')
stations_coords <- st_as_sf(runoff_overall_summer,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "change")
stat_map
#plotting on a plot
ggplot(data = runoff_overall_summer, 
       aes(x = reorder(ID, overall_diff), y = overall_diff, fill = overall_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in runoff values after 1980 in summer', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))
#doing the same for winter
runoff_change_winter <- merge(runoff_max_change_winter[, c('ID', 'max_diff')], 
                              runoff_min_change_winter[, c('ID', 'min_diff')], by = 'ID')
runoff_change_winter <- runoff_change_winter[, overall_diff := (max_diff + min_diff)/2]
runoff_change_winter[overall_diff > 0, change := factor('Increase')]
runoff_change_winter[overall_diff < 0, change := factor('Decrease')]
#plotting on a map
runoff_overall_winter <- merge(runoff_change_winter, stations_info[, c('Lon', 'Lat', 'ID')], by = 'ID')
stations_coords <- st_as_sf(runoff_overall_winter,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "change")
stat_map
#plotting on a plot
ggplot(data = runoff_overall_winter, 
       aes(x = reorder(ID, overall_diff), y = overall_diff, fill = overall_diff > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.text.y=element_blank()) +
  labs(title='Changes in runoff values after 1980 in winter', x='Stations', y='Change')+
  scale_fill_discrete(name = "Change", labels = c("Decrease", "Increase"))

saveRDS(runoff_overall_summer, './data/runoff_overall_summer.rds')
saveRDS(runoff_overall_winter, './data/runoff_overall_winter.rds')
saveRDS(stations_info, './data/stations_info_final.rds')
