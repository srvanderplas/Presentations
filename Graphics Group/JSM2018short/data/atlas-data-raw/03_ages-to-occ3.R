library(tidyverse)
occupation <- read.csv("./data/atlas-data-raw/occupation.csv")
occupation$State <- as.character(occupation$Area.name)
occupation$Territory <- as.logical(gsub(".* Territory", "TRUE", occupation$State))
occupation$Area.name <- gsub(" Territory", "", occupation$Area.name)
occupation$Area.name <- gsub(" Department", "", occupation$Area.name)
occupation$Area.name <- gsub(" Terr.", "", occupation$Area.name)


ages <- read.csv("./data/atlas-data-clean/ages-ipums.csv")
ages <- ages %>% mutate(
  STATEICP = replace(STATEICP, STATEICP == "Louisianna", "Louisiana"),
  STATEICP = replace(as.character(STATEICP), STATEICP %in% c("South Dakota", "North Dakota"), "Dakota")
)
ages <- ages %>% group_by(STATEICP) %>% summarize(total=sum(total), above10 = sum(above10), above11 = sum(above11))

ages.sex <- read.csv("./data/atlas-data-clean/ages-sex-ipums.csv")
ages.sex <- ages.sex %>% mutate(
  STATEICP = replace(STATEICP, STATEICP == "Louisianna", "Louisiana"),
  STATEICP = replace(as.character(STATEICP), STATEICP %in% c("South Dakota", "North Dakota"), "Dakota")
) %>% group_by(STATEICP, SEX) %>% summarize(above10=sum(above10), above11=sum(above11))

ages.sex.10 <- ages.sex %>% select(-above11) %>%
  spread(key=SEX, value=above10)
names(ages.sex.10)[2:3] <- c("above10.Female", "above10.Male")
ages.sex.11 <- ages.sex %>% select(-above10) %>%
  spread(key=SEX, value=above11)
names(ages.sex.11)[2:3] <- c("above11.Female", "above11.Male")
ages.sex <- left_join(ages.sex.10, ages.sex.11)


# all good!
# anti_join(occupation, ages, by=c("Area.name"="STATEICP")) %>% count(State)
occupation2 <- left_join(occupation, ages, by=c("Area.name"="STATEICP"))
occupation2$Total.Above10.Est <- with(occupation2, above10/total*Total.Population)
occupation2$Total.Above11.Est <- with(occupation2, above11/total*Total.Population)
occupation3 <- left_join(occupation2, ages.sex, by=c("Area.name"="STATEICP"))
occupation3 <- occupation3 %>% mutate(
  Total.Above10.Female = above10.Female/above10*Total.Above10.Est,
  Total.Above10.Male = above10.Male/above10*Total.Above10.Est,
  Total.Above11.Female = above11.Female/above11*Total.Above11.Est,
  Total.Above11.Male = above11.Male/above11*Total.Above11.Est,
  Employed.Female = Agriculture.Female+Manufacturing.Female+Trade.Female+Service.Female+School.Female,
  Employed.Male = Agriculture.Male+Manufacturing.Male+Trade.Male+Service.Male+School.Male,
  Unaccounted.Female = Total.Above11.Female - Employed.Female,
  Unaccounted.Male = pmax(Total.Above11.Male - Employed.Male,0),
  Unaccounted.Female.10 = Total.Above10.Female - Employed.Female,
  Unaccounted.Male.10 = pmax(Total.Above10.Male - Employed.Male,0)
)

occ3 <- occupation3 %>% gather(key="Occupation.Sex", value="Number", c(2:11,30:31)) %>% separate(Occupation.Sex, into=c("Occupation", "Sex"))
occ3 <- occ3 %>% mutate(
  Occupation = factor(Occupation, levels= c("Agriculture", "Manufacturing",
                                            "Trade", "Service", "School", "Unaccounted")),
  Sex = factor(Sex, levels=c("Male", "Female"))
)
write.csv(occ3, file="./data/atlas-data-clean/occ3.csv", row.names=FALSE)
