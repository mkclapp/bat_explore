## code to explore SonoBat output
## authors: M Clapp, M Boynton
## date created: 10 april 2018
## date last edited: 18 april 2018
# added code to read in multiple files 

rm(list = ls()) # this line of code removes EVERYTHING from the working environment

library(tidyverse)
library(lubridate)

# adding code to read multiple files here

allsites <- list.files ("data", full.names = TRUE, pattern = ".txt")
t <- do.call("rbind", lapply(allsites, read_tsv)) #t for 'total' sites

#a1 <- read_tsv("data/AMPHIT1_July_shorts_output.txt", col_names=TRUE)

# always take a glimpse to make sure your data are read in correctly

glimpse(t)
# summary(a1)

# rename columns to get rid of symbols...fix this later when we know true column significances
names(t) <- c("Filename", "HiF", "LoF", "ManualID", "AccpSpp", "Maj", "Accp", "TildaSpp", "Primary", "Secondary", "Tert", "Quat",
                        "Fc_mean", "Fc_stdev", "Dur_mean", "Dur_stdev", "calls_sec", "HiF_mean", "LoF_mean", "UpprSlp_mean", "LwrSlp_mean", 
                        "TotSlp_mean", "PrecedingInt_mean", "ParentDir", "NextDirUp", "filelength", "V", "AccpQuality", "NumCalls")

glimpse(t) # check column names

# create columns for date and time using the 'separate' function to split the latent info in the Filename column

t <- t %>% 
  separate(Filename, c("Site", "Date", "Time"), sep = "_", remove = FALSE) 

t <- t %>% 
  separate(Time, c("Time", "fileext"), sep=".w")
t$fileext <- NULL

t <- t %>% 
  unite(Timestamp, Date, Time, sep = " ", remove = TRUE)

class(t$Timestamp)

# format Timestamp to POSIXct format

t$Timestamp <- ymd_hms(t$Timestamp)

class(t$Timestamp) #check class now

# filter out all non-bat entries
detect <- t %>% 
  filter(HiF == "1" | LoF == "1")

detect %>%
  filter(HiF == "1" & LoF == "1") # this confirms that we have the entries for which both HiF and LoF = 1

summary(detect$Fc_mean)


sum(detect$HiF == "1")
sum(detect$LoF == "1")

# check the results for number of files with bats

nrow(detect)

## SOME VARIABLES CALLED BELOW NOW WRONG/RENAMED...below is old code from same process as above but on just a1 (amphitheater 1)
# # create columns for date and time using the 'separate' function to split the latent info in the Filename column
# 
# a1 <- a1 %>% 
#   separate(Filename, c("Site", "Date", "Time"), sep = "_", remove = FALSE) 
# 
# a1 <- a1 %>% 
#   separate(Time, c("Time", "fileext"), sep=".w")
# a1$fileext <- NULL
# 
# a1 <- a1 %>% 
#   unite(Timestamp, Date, Time, sep = " ", remove = TRUE)
# 
# class(a1$Timestamp)
# 
# # format Timestamp to POSIXct format
# 
# a1$Timestamp <- ymd_hms(a1$Timestamp)
# 
# 
# # filter out all non-bat entries
# amphit_detect <- a1 %>% 
#   filter(HiF == "1" | LoF == "1")
# 
# amphit_detect %>%
#   filter(HiF == "1" & LoF == "1") # this confirms that we have the entries for which both HiF and LoF = 1
# 
# summary(amphit_detect$`Fc mean`)
# 
# 
# sum(amphit_detect$HiF == "1")
# sum(amphit_detect$LoF == "1")

## below is unfinished code from previous edit?

#summary(amphit_detect$)

## attempting to filter data that we need to manually ID. Multiple different scenarios need to be checked for manual ID.
# Check if both HiF = 1 and LoF = 1
CheckMe <- detect %>%
  filter(HiF == "1" & LoF == "1") 
# check how many files we are going to need to manual ID
nrow(CheckMe)

## attempting to assign ID to data by using best-choice ID column, then next best, then next best...
# how to look in a column in a certain object when that column is used in multiple objects
detect["AccpSpp"]

if (detect["AccpSpp"] =='"x"') ID <- detect["TildaSpp"] else
  ID <- detect["AccpSpp"]