library(data.table)
library(mapview) 
library(sf)

#importing info about the stations
stations_info <- readRDS('./data/raw/runoff_eu_info.rds')
head(stations_info)
str(stations_info)
summary(stations_info) #43 NA's in altitude

stations_info[, Station := factor(abbreviate(Station))]
stations_info[, ID := factor(ID)]
stations_info[, River := factor(River)]
stations_info[, Country := factor(Country)]
stations_info[, Continent := factor(Continent)]
stations_info[, Lat := round(Lat, 3)]
stations_info[, Lon := round(Lon, 3)]
stations_info[, Alt := round(Alt, 0)]
saveRDS(stations_info, './data/stations_info_raw.rds')

#checking if the data is correct
nrow(stations_info) #208 stations
unique(stations_info$Continent) #all stations are in europe
length(unique(stations_info$Country))  #19 countries
length(unique(stations_info$River)) #153 rivers
min(stations_info$N.Years) #at least 80 years of measuring
max(stations_info$N.Years) #max 208 years of measuring

#everything looks fine, now checking the specifics

#how many stations per country
nstations <- stations_info[, .(Stations = length(unique(ID))), by = Country]
#how many rivers per country
nrivers <- stations_info[, .(Rivers = length(unique(River))), by = Country]
#merging these two data tables 
country_info <- merge(nstations, nrivers)

#how many stations per river
nstat_river <- stations_info[, .(Stations = length(unique(Station)), Countries = length(unique(Country))), by = River]
nstat_river <- nstat_river[order(Stations, decreasing=TRUE),]
saveRDS(nstat_river, './data/rivers_info.rds')
#making a map of the stations to inspect lon, lat and alt
stations_coords <- st_as_sf(stations_info,
                            coords = c('Lon', 'Lat'),
                            crs = 4326)
stat_map <- mapview(stations_coords, map.types = 'OpenTopoMap', zcol = "Alt")
stat_map #use the code in the report, don't save

#how many years of measuring per river on average
nyears_country <- stations_info[, .(Avg.Years = round(mean(N.Years))), by = Country]
#merging with the previous data table country_info
country_info <- merge(country_info, nyears_country)
saveRDS(country_info, './data/country_info.rds')

#big cluster around Germany+Switzerland, then Sweden+Norway+Finland
#but a lot of NA's in Sweden???