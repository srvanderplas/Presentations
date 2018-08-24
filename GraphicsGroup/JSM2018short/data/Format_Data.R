# This script is written to be run from within the writeup subdirectory (e.g. during the Rnw compilation process)

library(digest)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(munsell)
library(stringr)

# ---- Code to anonymize identifiable data ----
# # Read in original data
# amazon_header <- read_csv("../data/user-data-confidential/Frame Study - Amazon_February 9, 2018_13.18.csv", n_max = 1, col_names = F) %>% as.character()
# amazon <- read_csv("../data/user-data-confidential/Frame Study - Amazon_February 9, 2018_13.18.csv", skip = 3, col_names = amazon_header)
# amazon$source <- "amazon"
# reddit_header <- read_csv("../data/user-data-confidential/Frame Study - reddit-final.csv", n_max = 1, col_names = F) %>% as.character()
# reddit <- read_csv("../data/user-data-confidential/Frame Study - reddit-final.csv", skip = 3, col_names = reddit_header)
# reddit$source <- "reddit"
#
# # Function to hash sensitive information
# digestvec <- function(x) {
#   lapply(x, function(y) digest(as.character(y))) %>% unlist()
# }
#
# # Function to anonymize all sensitive fields and filter out unnecessary columns
# anon_data <- function(df) {
#   df %>%
#     select(-matches("Click|Page")) %>%
#     mutate_at(.vars = vars(matches("MTurkCode|IP|Recipient|IPAddress")), .funs = funs(digestvec)) %>%
#     # Fuzz IP address-based location
#     mutate_at(.vars = vars(matches("Location")), ~. + rnorm(length(.), sd = ***CENSORED***)) %>%
#     magrittr::set_names(stringr::str_replace(names(.), "Location", "Approx"))
# }
#
# # Anonymize data and write to csv
# amazon_anon <- anon_data(amazon)
# reddit_anon <- anon_data(reddit)
#
# write_csv(amazon_anon, "../data/user-data-anon/Amazon.csv")
# write_csv(reddit_anon, "../data/user-data-anon/Reddit.csv")
#
# # Extract demographic information
# read_demographics <- function(file) {
#   responses <- read.csv(file)
#   responses <- responses[-(1:2),]
#   library(tidyverse)
#   responses <- responses %>% rename(
#     Age=Q56,
#     Gender = Q520,
#     Education=Q524
#   )
#   responses <- filter(responses, Finished %in% c("True", "TRUE", "Finished"))
#   responses %>% select(Age, Gender, Education, Duration = Duration..in.seconds.)
# }
#
# amazon <- read_demographics("../data/user-data-confidential/Frame Study - Amazon_February 9, 2018_13.18.csv")
# reddit <- read_demographics("../data/user-data-confidential/Frame Study - reddit-final.csv")
# amazon$source <- "amazon"
# reddit$source <- "reddit"
# demographics <- rbind(amazon, reddit)
# write.csv(demographics, file="../data/user-data-anon/demographics.csv", row.names=FALSE)

# ---- Reproducible Pipeline ----
# datapath <- "../data"

# Clean up Anonymized Data
coltypes <- paste0("TTccidlTc____dd__c", paste0(rep("c", 56), collapse = ""), "_", "c")
amazon <- read_csv(file.path(datapath, "user-data-anon/Amazon.csv"), col_types = coltypes)
reddit <- read_csv(file.path(datapath, "user-data-anon/Reddit.csv"), col_types = coltypes)
demographics <- read_csv(file.path(datapath, "user-data-anon/demographics.csv"))

user_responses <- bind_rows(amazon, reddit)

responses <- user_responses %>% rename(
  Age = Q56,
  Gender = Q520,
  Education = Q524
)
responses <- filter(responses, Finished)


percentages <- responses %>%
  dplyr::select(IPAddress, ResponseId, source, starts_with("Q")) %>%
  gather(key = question, value = howmuch, -IPAddress, -ResponseId, -source)
percentages$howmuch <- str_replace(percentages$howmuch, "\\.{2}", "") %>%
  str_replace("~", "") %>%
  as.numeric()

percentages <- percentages %>% filter(howmuch > 0)
percentages <- percentages %>% filter(howmuch < 100)
idx <- which(percentages$howmuch < 1)
percentages$howmuch[idx] <- percentages$howmuch[idx]*100

pl <- read.csv(file.path(datapath, "/study-setup/PlotLabels.csv"))
percentages <- percentages %>%
  left_join(select(pl, "Question", "perc", "Type", "Frame", "isFrame"), by=c("question"="Question"))

# Define errors and which observations are included
percentages <- percentages %>%
  na.omit() %>%
  mutate(
    rel.error = (howmuch-perc)/perc,
    diff.error = howmuch-perc,
    log.error = abs(log(howmuch) - log(perc)),
    includeObs = log.error < log(5)
  )

percentages$frameframe <- 0
percentages <- percentages %>% mutate(
  frameframe = replace(frameframe, Frame & !isFrame, 1),
  frameframe = replace(frameframe, Frame & isFrame, 2)
)
percentages$frameframe <- c("Unframed", "Framed-inside", "Framed-frame")[percentages$frameframe+1]
percentages$frameframe <- factor(percentages$frameframe,
                                 levels=c("Unframed", "Framed-inside", "Framed-frame"))

write_csv(percentages, file.path(datapath, "user-data-anon/Cleaned_Data.csv"))

# ---- Statistical Atlas Data Sets ----
px2 <- read_csv(file.path(datapath, "atlas-data-clean/px2.csv"))
occ2 <- read_csv(file.path(datapath, "atlas-data-clean/occ2.csv"))
occ3 <- read_csv(file.path(datapath, "atlas-data-clean/occ3.csv"))
church <- read_csv(file.path(datapath, "atlas-data-clean/denominations-1874.csv"))
churchPixel <- read_csv(file.path(datapath, "atlas-data-clean/church_pixel.csv"))


# Set factor levels
occ2 <- occ2 %>% mutate(
  Occupation = factor(Occupation, levels =
                        c("Agriculture", "Manufacturing",
                          "Trade", "Service", "School")),
  Gender = factor(Gender, levels = c("Male", "Female")),
  State = as.character(State),
  Area.Name = as.character(Area.Name)
)

# Set factor levels, recode territories
occ3 <- occ3 %>% mutate(
  Occupation = factor(Occupation, levels =
                        c("Agriculture", "Manufacturing",
                          "Trade", "Service", "School", "Unaccounted")),
  Sex = factor(Sex, levels = c("Male", "Female")),
  State = as.character(State),
  Area.name = as.character(Area.name)
) %>%
  mutate(State = str_replace_all(
    State,
    c("(Arizona|New Mexico|Utah|Colorado) Territory" = "Southwest Territories",
      "(Idaho|Wyoming|Washington|Montana|Dakota) Territory" = "Northwest Territories")))

# Recode territories
stateorder <- unique(occ3$State)
stateorder <- c(stateorder[-which(stateorder %in% c("District of Columbia",
                                                    "Northwest Territories",
                                                    "Southwest Territories"))],
                c("District of Columbia", "Northwest Territories", "Southwest Territories"))
occ3$State <- factor(occ3$State, levels = stateorder, ordered = T)

# Recode denominations
cl <- church %>% gather(key = Denomination,
                        value = Number,
                        Baptist:Universalist, Unaccommodated)
cl <- cl %>% mutate(Denomination =
                      reorder(Denomination, Number, na.rm = TRUE))

cl$Denomination <- factor(cl$Denomination,
                          c("Unaccommodated", levels(cl$Denomination)[-20]))
levels(cl$Denomination) <- gsub("\\.", " ", levels(cl$Denomination))


cl_data <- cl %>% filter(as.character(STATEICP)==as.character(State)) %>%
  group_by(STATEICP) %>% nest()

cl_data$data <- cl_data$data %>% purrr::map(.f = function(x) {
  x %>%  arrange(desc(Number)) %>% mutate(
    group = c(as.character(Denomination[1:5]), rep("Other", n() - 5))
  ) %>% group_by(group) %>% summarize(Number = sum(Number, na.rm=TRUE))
})

cl_data <- cl_data %>% unnest()

cl_data <- cl_data %>% group_by(group) %>% mutate(Total = sum(Number, na.rm=TRUE))
cl_data <- cl_data %>% ungroup(group) %>% mutate(
  group = reorder(group, -Total, na.rm=TRUE)
)

# colours for church denominations
cols = c(brewer.pal(n = 12, name = "Paired")[c(1,3,5,7,9,11)], "grey80")

colRGB <- t(col2rgb(cols))/256
colMNSL <- rgb2mnsl(colRGB)
colMNSL <- c(colMNSL,
             darker(colMNSL, steps = 2),
             darker(colMNSL, steps = 4))

colHEX <- mnsl(colMNSL)
colHEX <- colHEX[rep(0:6, each = 3) + c(1, 8, 15)]
colHEX <- c(colHEX, "grey60")

# put other and unaccommodated last:
levels <- levels(cl_data$group)
cl_data$group <- factor(cl_data$group,
                        levels=c(setdiff(levels, c("Unaccommodated", "Other")),
                                 "Other", "Unaccommodated"))
cl_data$colour <- colHEX[as.numeric(cl_data$group)]
cl_data$colour <- with(cl_data, replace(colour, group=="Unaccommodated", "grey60"))

# ---- Auxiliary Data ----
# Need to document where census data comes from
census <- read_csv(file.path(datapath, "other/census-2016.csv"))
census <- census %>% gather(Education, count, -1)


# Save to a single data object
save(percentages, demographics, census, colHEX, px2, occ2, occ3, church, churchPixel,
     file = file.path(datapath, "Processed_Data.Rdata"))
