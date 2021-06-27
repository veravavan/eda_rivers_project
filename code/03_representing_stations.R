library(data.table)
library(ggplot2)
library(sf)
library(mapview)

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
stations_info <- readRDS('./data/stations_info_final.rds')
runoff_info <- readRDS('./data/runoff_values_edited.rds')

#getting the stations
five_stations <- stations_info[ID %in% c(6742900, 6340120, 6401601, 6335060, 6233680)]
five_runoff <- runoff_info[ID %in% c(6742900, 6340120, 6401601, 6335060, 6233680)]
#map of the stations
stations_coords <- st_as_sf(five_stations,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap')
stat_map
#setting years, months and seasons
five_runoff[, year := year(Date)]
five_runoff[, month := month(Date)]
five_runoff[month == 12 | month == 1 | month == 2, season := 'winter']
five_runoff[month == 3 | month == 4 | month == 5, season := 'spring']
five_runoff[month == 6 | month == 7 | month == 8, season := 'summer']
five_runoff[month == 9 | month == 10 | month == 11, season := 'autumn']
five_runoff[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
five_month <- five_runoff[, .(Value = mean(Value)), by = .(month, ID)]
five_month <- merge(five_month, five_stations[, c('ID', 'Station')], by = 'ID')
saveRDS(five_month, './data/five_month.rds')
#plot of monthly runoff
ggplot(five_month, aes(x=month, y=Value)) +
  geom_line() +
  facet_wrap(~Station, scales='free') +
  theme_bw() +
  ylab('Runoff value')
#plotting boxplots comparing years
five_winter <- five_runoff[season == 'winter', .(Value = mean(Value)), by = .(ID, year)]
five_summer <- five_runoff[season == 'summer', .(Value = mean(Value)), by = .(ID, year)]
saveRDS(five_summer, './data/five_summer.rds')
saveRDS(five_winter, './data/five_winter.rds')
year_thres <- 1980
to_plot <- rbind(cbind(five_winter, season = factor('winter')), 
                 cbind(five_summer, season = factor('summer'))) 
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot <- to_plot[year >= 1950]
saveRDS(to_plot, './data/five_win_sum.rds')
#boxplot
ggplot(to_plot, aes(season, Value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~ID, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

#plotting slopes for summer and winter
ggplot(five_summer, aes(x = year, y = Value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~ID, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(five_winter, aes(x = year, y = Value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~ID, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

