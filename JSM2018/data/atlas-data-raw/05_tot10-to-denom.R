library(tidyverse)
denominations <- read.csv("./data/atlas-data-raw/denominations-1874.csv")
totals <- read.csv("./data/atlas-data-clean/ages-ipums.csv")
totals <- totals %>% mutate(
  Area.name = as.character(Area.name),
  Area.name = replace(Area.name, Area.name=="Louisianna", "Louisiana")
)

denominations %>% anti_join(totals[,c("STATEICP", "Area.name", "Total.above10.est")],
                            by=c("State"="Area.name"))

anti_join(totals[,c("STATEICP", "Area.name", "Total.above10.est")], denominations,
                            by=c("Area.name"="State"))

denominations <- denominations %>%  left_join(totals[,c("STATEICP", "Area.name", "Total.above10.est")],
                            by=c("State"="Area.name"))
denominations <- denominations %>% mutate(
  Unaccommodated = pmax(0, Total.above10.est - Total)
) %>% unique()

write.csv(denominations, file="./data/atlas-data-clean/denominations-1874.csv", row.names=FALSE)
