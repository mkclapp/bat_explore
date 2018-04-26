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
t <- do.call("rbind", lapply(X = allsites, FUN = read_tsv, col_names = FALSE, skip = 1)) #t for 'total' sites
# TO DO: check the above to make sure all the files bound correctly-- the code above returns warnings
# always take a glimpse to make sure your data are read in correctly
glimpse(t)

# a1 <- read_tsv("data/AMPHIT1_July_shorts_output.txt", col_names=TRUE) #AMPHIT1 ONLY
# summary(a1)


# TO-DO LIST --------------------------------------------------------------
## UPDATE COLUMN NAMES NOW 04242018 //
## write an interactive code for column name descriptions //
## GOOGLE 'SECTION' CTRL+SHIFT+R //
## ADD MULT GUESSES TO CHECKME //
## BE CONSERVATIVE WITH ID //
## PROBABLY GET RID OF ID IF/THEN SITUATION //
## REVIEW CODE/REVIEW PLOTTING PROCESS
## PLAY W/ SPRICH_SITE
## INSECT LITERATURE
## FEEDBACK FROM LEILA REGARDING HI=1&LO=1 EXAMPLES
## FIX GROUP_BY(CHECKME)?


# COLUMN NAMES ------------------------------------------------------------

# rename columns to get rid of symbols...fix this later when we know true column significances
names(t) <- c("Filename", "HiF", "LoF", "ManualID", "AccpSpp", "Maj", "Accp", "TildeSpp", "Primary", "Secondary", "Tert", "Quat",
              "Fc_mean", "Fc_stdev", "Dur_mean", "Dur_stdev", "calls_sec", "HiF_mean", "LoF_mean", "UpprSlp_mean", "LwrSlp_mean", 
              "TotSlp_mean", "PrecedingInt_mean", "ParentDir", "NextDirUp", "filelength", "V", "AccpQuality", "NumCalls")
glimpse(t) # check column names
# These col descriptors never found in docs: TildeSpp, HiF_mean, LoF_mean, UpprSlp_mean, LwrSlp_mean
# TildeSpp may be like the former col name 'Clssnif<Thr'?
# AccpQuality is likely the former col name 'AccpQualForTally' and could be USEFUL to understand freq and decision choices made by SonoBat

# Create Interactive Code for user-friendlyness
#trying the comment function, etc.
library(lattice)
describeattempt <- left_join(names(t), descriptions)
comment(t) <- columns

descriptions <- c("name of file imported",
                  "1 if call is primarily high frequency",
                  "1 if call is primarily low frequency",
                  "not listed -- optional for if we added a manual ID",
                  "accepted species ID",
                  "the # of calls in the majority of vote decision//accepted species ID",
                  "? -- # of calls in the vote decision that reached a species decision and entered in the vote decision",
                  "not listed -- a likely but uncertain species ID",
                  "the best guess//species ID with the greatest # of calls",
                  "2nd best species ID guess",
                  "3rd best species ID guess",
                  "4th best species ID guess",
                  "mean frequency of the most characteristic frequency",
                  "standard deviation of the most characteristic frequency",
                  "mean call duration (msec) of the calls included in the sequence decision",
                  "standard deviation of the duration of the calls included in the sequence decision",
                  "mean calls per second (long description in Word doc",
                  "mean high frequency",
                  "mean low frequency",
                  "not listed",
                  "not listed",
                  "not listed",
                  "not listed?",
                  "parent directory",
                  "next directory up from parent directory",
                  "file length",
                  "SonoBat version",
                  "bat pass acceptable quality setting -- a compromise b/n getting all the bats vs getting excess noise",
                  "max # of calls considered per file")
columns <- data.frame(names(t), descriptions)

# NUMBERS = NUMERIC -------------------------------------------------------

# change columns with numbers to class 'numeric'
t[,13:23] <- lapply(t[,13:23], as.numeric) # 'lapply' is taking columns 13-23 of the t dataframe and applying the function 'as.numeric' to them
summary(t) #confirm that it worked-- summary can now return summary stats on the numeric columns


# DATE/TIME FORMATTING ----------------------------------------------------
# create columns for date and time using the 'separate' function to split the latent info in the Filename column
# the second 'separate' command is a janky way to do this but i don't feel like learning regular expressions rn
t <- t %>% 
  separate(Filename, c("Site", "Date", "Time"), sep = "_", remove = TRUE) %>%
  separate(Time, c("Time", "fileext"), sep=".w")
t$fileext <- NULL # this deletes this column

# create a Timestamp column because it's easy to coerce to the date/time class
t <- t %>% 
  unite(Timestamp, Date, Time, sep = " ", remove = FALSE)
class(t$Timestamp) 

# format Timestamp to correct class
t$Timestamp <- ymd_hms(t$Timestamp)
class(t$Timestamp) #check class now, should be POSIXct

# FINALLY create separate columns for date and time and coerce to date/time class
t <- t %>%
  separate(Timestamp, c("Date", "Time"), sep = " ", remove = FALSE)
lapply(t, class) # returns class for all columns
t$Date <- ymd(t$Date)
t$Time <- chron(times=t$Time)
class(t$Time)

# add column for HOUR (to later investigate call activity by hour of night)
t <- t %>%
  mutate(hr = substr(Time, 1,2))

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



# MANUAL ID --------------------------------------------

## attempting to filter data that we need to manually ID. Multiple different scenarios need to be checked for manual ID.
# Check if both HiF = 1 and LoF = 1
CheckMe1 <- detect %>%
  filter(HiF == "1" & LoF == "1") 
# check if it worked
nrow(CheckMe1)
# sort by site and date
CheckMe1 %>%
  group_by(Site, Date)
# create a table to send to Leila
write.csv(CheckMe1, file = "docs/CheckMeHiLo", na = "NA", append = FALSE, col.names = append)

# check if there are multiple guesses
CheckMe2 <- detect %>%
  filter(Primary != AccpSpp)
# check 
nrow(CheckMe2)
# sort by site and date
CheckMe2 %>%
  group_by(Site, Date)

# check if there are multiple guesses even though the Primary was "accepted"
CheckMe3 <- detect %>%
  filter(Primary == AccpSpp & Accp != Maj)
# check 
nrow(CheckMe3)
# sort by site and date
CheckMe3 %>%
  group_by(Site, Date)

# combine all the files we need to manual ID:
CheckMe <- bind_rows(CheckMe1, CheckMe2, CheckMe3)
# check that the # of variables in all 3 'CheckMe#'s got into the final CheckMe
nrow(CheckMe1) + nrow(CheckMe2) + nrow(CheckMe3)
# sort by site and date
CheckMe %>%
    group_by(Site, Date) #THIS IS NOT WORKING -- IT'S JUST STACKING ALL THE CHECKME'S
#^use 'melt' in reshape2 package?

# ## attempting to assign ID to data by using best-choice ID column, then next best, then next best...
# # how to look in a column in a certain object when that column is used in multiple objects
# detect["AccpSpp"]
# unique(detect$AccpSpp)
# 
# if (detect["AccpSpp"] =='"x"') ID <- detect["TildeSpp"] else
#   ID <- detect["AccpSpp"]

# number of species per site
sprich_site <- detect %>%
  select(Site, Date, Time, AccpSpp) %>%
  group_by(Site) %>%
  summarise(sprich = n_distinct(AccpSpp)) %>%
  ggplot() +
  geom_col(aes(x=Site, y=sprich))

# PLOTS -------------------------------------------------------------------

# HiF and LoF over time, by Site

# detect$HiF <- as.numeric(detect$HiF)
# detect$LoF <- as.numeric(detect$LoF)

freq_group <- detect %>%
  select(Date, Time, Site, HiF, LoF) %>%
  group_by(Date, Site) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))
  
  ggplot(freq_group) +
  geom_point(alpha = 0.5, aes(x=Date, y=nHiF, color = "red")) +
  geom_point(alpha = 0.5, aes(x=Date, y=nLoF, color = "blue")) +
  facet_wrap(~Site, nrow = 3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per night",
       title = "Bat Call Activity at Fishless versus Fish-containing Lakes") +
    scale_colour_manual(name = NULL,
                        values =c('red'='red','blue'='blue'), 
                        labels = c('low-frequency', 'high-frequency')) 

by_time <- detect %>%
  group_by(hr, Site) %>%
  filter(hr != 10) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))

by_time_2 <- detect %>%
  group_by(hr) %>%
  filter(hr != 10) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))
  
  ggplot(by_time) +
  geom_point(alpha = 0.5, aes(x=hr, y=nHiF, color = "red")) +
  geom_point(alpha = 0.5, aes(x=hr, y=nLoF, color = "blue")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "hour of night (24-hour time)", 
       y = "number of bat passes",
       title = "Bat Passes by Hour") +
    scale_colour_manual(name = 'acoustic clade',
                        values =c('red'='red','blue'='blue'), 
                        labels = c('low-frequency', 'high-frequency')) +
    theme(legend.position = c(0.8, 0.8))
