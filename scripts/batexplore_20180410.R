## code to explore SonoBat output
## authors: M Clapp, M Boynton
## date created: 10 april 2018
## date edited: 18 april 2018: added code to read in multiple files by M Boynton
## last date edited: 20 april 2018 M Clapp: annotated some existing code, added sections, 

rm(list = ls()) # this line of code removes EVERYTHING from the working environment


# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(chron)

# READ IN, CLEAN UP SONOVET OUTPUT FILES ----------------------------------

allsites <- list.files ("data", full.names = TRUE, pattern = ".txt")
t <- do.call("rbind", lapply(allsites, read_tsv)) #t for 'total' sites
# TO DO: check the above to make sure all the files bound correctly-- the code above returns warnings
# always take a glimpse to make sure your data are read in correctly
glimpse(t)

# a1 <- read_tsv("data/AMPHIT1_July_shorts_output.txt", col_names=TRUE) #AMPHIT1 ONLY
# summary(a1)

# rename columns to get rid of symbols...fix this later when we know true column significances
names(t) <- c("Filename", "HiF", "LoF", "ManualID", "AccpSpp", "Maj", "Accp", "TildeSpp", "Primary", "Secondary", "Tert", "Quat",
                        "Fc_mean", "Fc_stdev", "Dur_mean", "Dur_stdev", "calls_sec", "HiF_mean", "LoF_mean", "UpprSlp_mean", "LwrSlp_mean", 
                        "TotSlp_mean", "PrecedingInt_mean", "ParentDir", "NextDirUp", "filelength", "V", "AccpQuality", "NumCalls")
glimpse(t) # check column names

# change columns with numbers to class 'numeric'
t[,13:23] <- lapply(t[,13:23], as.numeric) # 'lapply' is taking columns 13-23 of the t dataframe and applying the function 'as.numeric' to them
summary(t) #confirm that it worked-- summary can now return summary stats on the numeric columns

# create columns for date and time using the 'separate' function to split the latent info in the Filename column
# the second 'separate' command is a janky way to do this but i don't feel like learning regular expressions rn
t <- t %>% 
  separate(Filename, c("Site", "Date", "Time"), sep = "_", remove = TRUE) %>%
  separate(Time, c("Time", "fileext"), sep=".w")
t$fileext <- NULL # this deletes this column
# change class of Time column
t$Time <- as.numeric(t$Time)
t$Time <- chron(times=t$Time, out.format = "h:m:s") # for some reason, changing the Time column to numeric allowed chron to work
t$Time <- as.POSIXct(t$Time, format = "hhmmss")
#change class of date column 
t$Date <- ymd(t$Date)

# UPDATE: we may not need this timestamp column anymore now that the above worked
# 'unite' creates a new column called "Timestamp" that includes both the Date and Time columns we created above
# We did this because it's easier to coerce a full date-timestamp into POSIXct format than it is with a time-only stamp
# I'm sure there's a way to change the class of time objects but I couldn't figure it out quickly -MKC
t <- t %>% 
  unite(Timestamp, Date, Time, sep = " ", remove = FALSE)
class(t$Timestamp) 

# format Timestamp to correct class
t$Timestamp <- ymd_hms(t$Timestamp)
class(t$Timestamp) #check class now

# FINALLY create separate columns for date and time and coerce to date/time class
t <- t %>%
  separate(Timestamp, c("Date", "Time"), sep = " ", remove = FALSE)
lapply(t, class) # returns class for all columns
t$Date <- ymd(t$Date)
t$Time <- chron(times=t$Time)

# FILTER DATA -------------------------------------------------------------
# filter out all non-bat entries and create a new dataframe called 'detect' with only entries that definitely contain bat passes
# from SonoBat documentation: "For rudimentary tallying of bat passes, SonoBat separates bat species into low and high frequency acoustic clades. Even if unable to perform a species identification, if recognized as a bat, SonoBat will add a digit in this column"
# entries with a 1 in both columns means that the classifier found passes in both classes within the .wav file-- there could be two bats in those files
detect <- t %>% 
  filter(HiF == "1" | LoF == "1")

# this confirms that we also have the entries for which both HiF and LoF = 1 ('count' returns 715 entries of this case)
detect %>%
  count(HiF == "1" & LoF == "1") 

sum(detect$HiF == "1")
sum(detect$LoF == "1")

# check the results for number of files with bats

nrow(detect)

# (SOME OLD CODE) ---------------------------------------------------------
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


# FILTERING DATA FOR MANUAL ID --------------------------------------------

## attempting to filter data that we need to manually ID. Multiple different scenarios need to be checked for manual ID.
# Check if both HiF = 1 and LoF = 1
CheckMe <- detect %>%
  filter(HiF == "1" & LoF == "1") 
# check how many files we are going to need to manual ID
nrow(CheckMe)

## attempting to assign ID to data by using best-choice ID column, then next best, then next best...
# how to look in a column in a certain object when that column is used in multiple objects
detect["AccpSpp"]

if (detect["AccpSpp"] =='"x"') ID <- detect["TildeSpp"] else
  ID <- detect["AccpSpp"]


# PLOTS -------------------------------------------------------------------

# HiF and LoF over time, by Site

# detect$HiF <- as.numeric(detect$HiF)
# detect$LoF <- as.numeric(detect$LoF)

freq_group <- detect %>%
  select(Date, Time, Site, HiF, LoF) %>%
  group_by(Date, Site) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1")) %>%
  
  ggplot(data = freq_group) +
  geom_point(alpha = 0.5, color = "red", aes(x=Date, y=nHiF)) +
  geom_point(alpha = 0.5, color = "blue", aes(x=Date, y=nLoF)) +
  facet_wrap(~Site, nrow = 3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per night",
       title = "Bat Call Activity at Fishless versus Fish-containing Lakes") 

freq_group %>%
  group_by(Time, Site) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1")) %>%
  
  ggplot() +
  geom_point(alpha = 0.5, color = "red", aes(x=Date, y=nHiF)) +
  geom_point(alpha = 0.5, color = "blue", aes(x=Date, y=nLoF)) +
  facet_wrap(~Site, nrow = 3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per night",
       title = "Bat Call Activity at Fishless versus Fish-containing Lakes") 
