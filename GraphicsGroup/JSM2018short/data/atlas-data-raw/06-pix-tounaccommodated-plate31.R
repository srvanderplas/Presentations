library(tidyverse)
church <- read.csv("./data/atlas-data-clean/denominations-1874.csv")
churchPixel <- read.csv("./data/atlas-data-raw/church_accomodations-w-percent-unacomodated-area.csv")
churchPixel <- churchPixel %>%
  separate(Square.Outside, into=c("out.width", "out.height"), sep="x", convert=TRUE) %>%
  separate(Square.Inside, into=c("in.width", "in.height"), sep="x", convert=TRUE) %>%
  mutate(
    Total.Area = out.width*out.height,
    Border.Area = Total.Area - in.width*in.height,
    UAPixperc = Border.Area/Total.Area*100
  )


anti_join(churchPixel, church, by=c("State.Territory"="Area.name"))
anti_join(church, churchPixel, by=c("Area.name"="State.Territory"))
churchPixel <- churchPixel %>%
  left_join(church, by=c("State.Territory"="Area.name"))
churchPixel <- churchPixel %>% mutate(
  UAEstperc = Unaccommodated/Total.above10.est*100
)

write.csv(churchPixel, "./data/atlas-data-clean/church_pixel.csv", row.names=FALSE)

