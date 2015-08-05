
library(ggplot2)
library(animint)
library(dplyr)
library(magrittr)
library(lubridate)
library(stringr)
library(USAboundaries)
library(rgdal)
library(spatstat)
library(rgeos)

rivers <- readOGR(dsn="Data/", layer="TCEQ_SEGMENTS_LINE_2012") 
rivers.data <- rivers@data
rivers <- gSimplify(rivers, .025, topologyPreserve = T) 
rivers <- SpatialLinesDataFrame(rivers, rivers.data)
rivers <- merge(rivers %>% fortify(), data.frame(rivers.data, id=0:(nrow(rivers.data)-1))) 
rivers <- rivers %>% group_by(id) %>%
  mutate(npts = length(id),
         isRiver = sum(str_detect(SEG_NAME, "[Rr]iver") | SHAPE_LEN>1)>0) %>%
  filter(isRiver)

lakes <- readOGR(dsn="Data/", layer="TCEQ_SEGMENT_POLY_2012") 
lakes.data <- lakes@data
lakes <- gSimplify(lakes, .05, topologyPreserve = T) 
lakes <- SpatialPolygonsDataFrame(lakes, lakes.data)
lakes <- merge(lakes %>% fortify(), data.frame(lakes.data, id=0:(nrow(lakes.data)-1))) 
lakes <- lakes %>% group_by(id) %>%
  mutate(npts = length(id),
         isBig = sum(SHAPE_Area>.05)>0) %>%
  filter(isBig)


weather <- read.csv("Data/TexasWeather.csv", na.strings="-9999", stringsAsFactors=F)
names(weather) <- c("station", "station.name", "date", 
                    "multiday.precip.total", "number.of.days", 
                    "precipitation", "snow.depth", 
                    "temp.max", "temp.min", "temp.obs", 
                    "wind.movement", 
                    "wtype.snow", "wtype.dust", "wtype.fog", 
                    "wtype.glz", "wtype.hail", "wtype.heavyfog", 
                    "wtype.highwind", "wtype.sleet",
                    "wtype.smoke", "wtype.thunder", "wtype.tornado")

weather <- weather %>% 
  select(1:22) %>% 
  mutate(date = ymd(date), 
         temp.max = temp.max/10*1.8+32,  # Convert temps to F
         temp.min = temp.min/10*1.8+32, 
         temp.obs = temp.obs/10*1.8+32,
         precipitation = precipitation/10/10*.39370,
         station.type = str_sub(station, 1, 5),
         station = str_sub(station, 7, -1)) # Convert precip to inches

weather <- weather %>% filter(!is.na(precipitation) | !is.na(temp.min))

# colSums(apply(weather, 2, is.na))

stations <- read.fwf("Data/stations.txt", widths=c(11, 9, 10, 7, 3, 31, 4, 4, 6), na.strings="", stringsAsFactors=F, header=F, comment.char="!")
names(stations) <- c("station", "latitude", "longitude", "elevation", "state", "station.name", "network", "wmo.id")
stations <- stations %>% select(station,station.name, elevation,latitude,longitude) %>% unique() %>%
  filter(station%in%weather$station)

weather <- merge(weather, stations[,-2], stringsAsFactors=F)

floor.week <- function(x){
  tmp <- ymd("2015-01-01") 
  week(tmp) <- week(x)
  tmp
}

statesummary <- weather %>% 
  mutate(
    week = floor.week(date),
    min.day = week,
    max.day = week + days(7)
  ) %>%
  group_by(week, station) %>%
  summarize(
    precip = sum(precipitation[!is.na(precipitation) & !is.nan(precipitation) & !is.infinite(precipitation)])
  ) %>% ungroup %>%
  group_by(week) %>% 
  summarize(
    precip.mean = mean(precip),
    precip.median = median(precip),
    precip.max = max(precip),
    station.max = station[which.max(precip)]
  )

weathersummary <- weather %>% 
  mutate(lat = round(latitude/2, digits = 1)*2,
         long = round(longitude/2, digits = 1)*2,
         week = floor.week(date)) %>%
  group_by(lat, long, station, week) %>% 
  mutate(precip.wk = sum(precipitation, na.rm=T),
         min.day = week,
         max.day = week + days(7)) %>%
  ungroup() %>% 
  group_by(lat, long, date, week) %>% 
  summarize(elevation = mean(elevation, na.rm=T),
            temp.max = mean(temp.max, na.rm=T),
            temp.min = mean(temp.min, na.rm=T),
            temp.obs = mean(temp.obs, na.rm=T),
            precip.wk = mean(precip.wk, na.rm=T),
            idx = which.min((lat-latitude)^2+(long-longitude)^2),
            station = station[idx],
            station.name = station.name[idx],
            latitude = latitude[idx],
            longitude = longitude[idx],
            precip = mean(precipitation, na.rm=T),
            min.day = min(min.day, na.rm=T),
            max.day = max(max.day, na.rm=T)
            ) %>%
  group_by(station) %>% 
  mutate(max.temp.gap = max(diff(date[!is.na(temp.min)])),
         month.span = length(unique(month(date)))) %>%
  filter(max.temp.gap<15 & month.span>6) %>% 
  ungroup()

weathersummary$station.name %<>% str_replace_all("( \\d{1,}\\.?\\d{1,}?( [NSEW]{0,3})?)|( TX US)|( TEXAS)|( INTERNATIONAL)|( MUNICIPAL)|( REGIONAL)", "")

stations <- weathersummary %>% select(station, station.name, latitude, longitude) %>% unique()

tx <- us_states(states="Texas") %>% fortify()

stations.plot <- ggplot() + 
  geom_path(data=rivers, aes(x=long, y=lat, group=group), color="grey", size=.5) + 
  geom_polygon(data=lakes, aes(x=long, y=lat, group=group), fill="grey", size=.5) + 
  geom_path(data=tx, aes(x=long, y=lat, group=group)) + 
  geom_point(data=stations, aes(x=longitude, y=latitude, clickSelects=station), size=4, fill="transparent", color="black", alpha=.75) + 
  geom_text(data=stations, aes(x=-100, y=37, label=station.name, showSelected=station)) + 
  theme_minimal() + 
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank()) + 
  theme_animint(width=500, height=450) + 
  ggtitle("Weather Stations")
# 
# temp.plot <- ggplot() + 
#   geom_tallrect(data=unique(select(weathersummary, min.day, max.day, week)), 
#                 aes(xmin=min.day, xmax=max.day, clickSelects=week), alpha=.1, fill="black", color="black") +  
#   geom_text(data=weathersummary, aes(x=median(weathersummary$date), y=0, label=sprintf("Station: %s", str_replace(station.name, "( (\\d{1,}\\.\\d{1,} [NSEW]{0,3})? TX US)|( TEXAS)|( INTERNATIONAL)|( MUNICIPAL)|( AIRPORT)|( REGIONAL)", "")), showSelected=station)) + 
#   geom_line(data=weathersummary, aes(x=date, y=temp.min, showSelected=station)) + 
#   xlab("") + ylab("Temperature (F)") + ggtitle("Measured Temperature") + theme_bw()

precip.plot <- ggplot() + 
  geom_tallrect(data=unique(select(weathersummary, min.day, max.day, week)), 
                aes(xmin=min.day, xmax=max.day, clickSelects=week), alpha=.1, fill="black", color="black") +
  geom_text(data=weathersummary, aes(x=median(weathersummary$date), y=10, label=sprintf("Station: %s", str_replace(station.name, "( (\\d{1,}\\.\\d{1,} [NSEW]{0,3})? TX US)|( TEXAS)|( INTERNATIONAL)|( MUNICIPAL)|( AIRPORT)|( REGIONAL)", "")), showSelected=station)) + 
  geom_segment(data=subset(weathersummary, !is.infinite(precip)), aes(x=date, xend=date, y=precip, yend=0, showSelected=station)) + 
  xlab("") + ylab("Precipitation (in)") + ggtitle("Average Regional Precipitation") + theme_bw()

precip.map <- ggplot() + 
  geom_path(data=rivers, aes(x=long, y=lat, group=group), color="grey", size=.5) + 
  geom_polygon(data=lakes, aes(x=long, y=lat, group=group), fill="grey", size=.5) + 
  geom_path(data=tx, aes(x=long, y=lat, group=group)) + 
  geom_point(data=weathersummary, aes(x=longitude, y=latitude, fill=precip.wk, showSelected=week, color=precip.wk, clickSelects=station), size=4) +
  geom_point(data=weathersummary, aes(x=longitude, y=latitude, showSelected=week, clickSelects=station), color="black", size=4, alpha=.5, fill="transparent") +
  scale_fill_gradient2("Weekly Rainfall (in)", limits=c(0, 11), low="gray90", high="steelblue4", mid="steelblue1") + 
  scale_color_gradient2("Weekly Rainfall (in)", limits=c(0, 11), low="gray90", high="steelblue4", mid="steelblue1") + 
  theme_minimal() + 
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank()) + 
  theme_animint(width=600, height=450)

animint2dir(
  plot.list=list(
    stations=stations.plot, 
    # temp=temp.plot, 
    precip=precip.plot, 
    precipMap = precip.map, 
    first=list(station="US1TXED0022")), 
  out.dir="Plots/")
