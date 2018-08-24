library(tidyverse)

# This data is from IPUMS and cannot be released in full form due to licensing restrictions
ipums <- read.csv("./data/atlas-data-raw/ipums-1870.csv")
ages <- ipums %>%
  group_by(STATEICP) %>%
  summarize(total = n(), above10 = sum(AGE>=10), above11 = sum(AGE >=11))
ages.sex <- ipums %>%
  group_by(STATEICP, SEX) %>%
  summarize(total = n(), above10 = sum(AGE>=10), above11=sum(AGE >=11))

totals <- read.csv("./data/atlas-data-raw/state-population.csv")
anti_join(ages, totals, by=c("STATEICP"="State")) # the Dakotas ...

ages <- ages %>%
  mutate(
    STATEICP = as.character(STATEICP),
    STATEICP = replace(STATEICP, STATEICP %in% c("North Dakota", "South Dakota"), "Dakota")
  )
ages <- ages %>%
  group_by(STATEICP) %>%
  summarize(total = sum(total), above10 = sum(above10), above11 = sum(above11))
anti_join(ages, totals, by=c("STATEICP"="State")) # the Dakotas are fixed now ...


ages <- left_join(ages, totals, by=c("STATEICP"="State"))
summary(ages$total/ages$Total.Population)
ages %>%
  ggplot(aes(x = total/Total.Population, y = STATEICP)) + geom_point()
ages <- ages %>% mutate(
  Total.above10.est = Total.Population/total*above10,
  Total.above11.est = Total.Population/total*above11
)

write.csv(ages, file="./data/atlas-data-clean/ages-ipums.csv", row.names=FALSE)
write.csv(ages.sex, file="./data/atlas-data-clean/ages-sex-ipums.csv", row.names=FALSE)
