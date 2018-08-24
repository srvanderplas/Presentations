library(tidyverse)
pixels2 <- read.csv("./data/atlas-data-raw/pixel-values.csv")
pixels2$Unaccounted <- (451^2 - (451-2*pixels2$Unaccounted)^2)/451

px1 <- pixels2 %>% select(State, Agriculture, Manufacturing, Trade, Service, School, Unaccounted) %>% gather(key="Occupation", value="Pixels", -State, factor_key=TRUE)
px1 <- px1 %>% group_by(State) %>% mutate(State.Pixels=sum(Pixels))
px1 <- px1 %>% mutate(PixPercent = Pixels/State.Pixels*100) %>% arrange(State)

occ4 <- occ3 %>% select(Area.name, Occupation, Sex, Number) %>%
  spread(key="Sex", value="Number") %>%
  mutate(Number = Male + Female) %>%
  group_by(Area.name) %>%
  mutate(Total = sum(Number),
         CensusPercent = Number/Total*100) %>%
  select(-Male, -Female, -Total)

px2 <- full_join(px1, occ4, by=c("State"="Area.name", "Occupation"))

write.csv(px2, file="./data/atlas-data-clean/px2.csv", row.names=FALSE)
