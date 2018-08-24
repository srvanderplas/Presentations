library(tidyverse)
occupation <- read.csv("./data/atlas-data-raw/occupation.csv")
occupation$State <- as.character(occupation$Area.name)
occupation$Territory <- as.logical(gsub(".* Territory", "TRUE", occupation$State))
occupation$Area.name <- gsub(" Territory", "", occupation$Area.name)
occupation$Area.name <- gsub(" Department", "", occupation$Area.name)
occupation$Area.name <- gsub(" Terr.", "", occupation$Area.name)


occ2 <- occupation %>% gather(key="Occupation.Gender", value="Number", 2:11, factor_key = TRUE)
occ2 <- occ2 %>% select(State, Total.Population, Occupation.Gender, Number) %>%
  separate(Occupation.Gender, into=c("Occupation", "Gender"))
occ2 <- occ2 %>% mutate(
  Occupation = factor(Occupation, levels= c("Agriculture", "Manufacturing",
                                            "Trade", "Service", "School")),
  Gender = factor(Gender, levels=c("Male", "Female"))
)
occ2 <- occ2 %>% group_by(State) %>% mutate(Total.Employed=sum(Number))
occ2$Area.Name <- gsub(" Territory","", occ2$State)
occ2$Area.Name <- gsub(" Terr.","", occ2$Area.Name)
write.csv(occ2, "./data/atlas-data-clean/occ2.csv", row.names=FALSE)
