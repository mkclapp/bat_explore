## code to explore SonoBat output
## authors: M Clapp, M Boynton
## date created: 21 Jan 2019
## date edited: 21 Jan 2019: reads new SonoBat output generated 1/16/2019 

rm(list = ls()) # this line of code removes EVERYTHING from the working environment

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(chron)

# TO-DO LIST --------------------------------------------------------------


# NEW data 1/25/2019 from Leila -------------------------------------------

t <- read_csv(file = "./data/Shorts_Combined_CumSonoBatch_v430.csv")
glimpse(t)
# change columns with numbers to class 'numeric'
t[,11:20] <- lapply(t[,11:20], as.numeric) # 'lapply' is taking columns 13-23 of the t dataframe and applying the function 'as.numeric' to them

summary(t) #confirm that it worked-- summary can now return summary stats on the numeric columns
t[,32:34] <- lapply(t[,32:34], as.numeric)

# TEMPORARY potential solution-- changing NA's to 0's 
t[is.na(t)] <- 0

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
detect[is.na(detect)] <- 0
# this confirms that we also have the entries for which both HiF and LoF = 1 ('count' returns 715 entries of this case)
detect %>%
  count(HiF == "1" & LoF == "1") 

sum(detect$HiF == "1") # number of high-frequency bat passes
sum(detect$LoF == "1") # number of low-frequency bat passes
sum(detect$SppAccp != "0")
# for the moment, we're going to filter out the double ones
# no need to do this, means that there are likely two different bats in the same file, 
# one might just not be good enough to ID past "HiF" or "LoF"
detect$dbl <- ifelse(detect$HiF =="1" & detect$LoF == "1", "dbl", "not.dbl") #omg I did it
detect2 <- detect %>% filter(dbl=="not.dbl")

# PLOTS -------------------------------------------------------------------

# HiF and LoF over time, by Site
# amphitheater only
amph <- detect %>% filter(Site=="AMPHIT2") %>%
  select(Date, Time, Site, HiF, LoF, SppAccp) %>%
  group_by(Date) %>%
  summarise(nHiF = sum(HiF == 1), nLoF = sum(LoF == 1))

ggplot(amph) +
  geom_point(alpha = 0.5, aes(x=Date, y=nHiF, color = "red")) +
  geom_point(alpha = 0.5, aes(x=Date, y=nLoF, color = "blue")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per night",
       title = "Bat Call Activity at Amphitheater Lake") +
  scale_colour_manual(name = NULL,
                      values =c('red'='red','blue'='blue'), 
                      labels = c('low-frequency', 'high-frequency')) 

# detect$HiF <- as.numeric(detect$HiF)
# detect$LoF <- as.numeric(detect$LoF)

freq_group <- detect %>%
  select(Date, Time, hr, Site, HiF, LoF, `Fc mean`, SppAccp, Prob) %>%
  filter(SppAccp != "0" & Prob > 0.98) 

nighttally <- freq_group %>%
  group_by(Date, Site) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))

ggplot(nighttally) +
  geom_point(alpha = 0.5, aes(x=Date, y=nHiF, color = "red")) +
  geom_point(alpha = 0.5, aes(x=Date, y=nLoF, color = "blue")) +
  facet_wrap(~Site, nrow = 3) +
  theme_minimal() + 
  labs(x = "date", 
       y = "number of bat passes per night",
       title = "") +
  scale_colour_manual(name = NULL,
                      values =c('red'='red','blue'='blue'), 
                      labels = c('low-frequency', 'high-frequency')) +
  theme(plot.title = element_text(family = "Helvetica", size=24),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_text(family = "Helvetica", size=12, angle=50, hjust=1, vjust=1),
        axis.text.y = element_text(family = "Helvetica", size=12, angle=0),
        strip.text.x = element_text(size = 16))

# by time of night
by_time <- freq_group %>%
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

# patterns by species (HUGE grain of salt right now bc no manual vetting has occurred yet)

unique(freq_group$SppAccp)
spp <- freq_group %>% group_by(Date, Site, SppAccp) %>%
  summarise(daytot = n()) %>%
  separate(Site, c("Basin", "Fish"), sep=-1, remove=FALSE) %>%
  mutate(month = as.character(month(Date)))

spp %>% 
  filter(`Fc mean` < 30) %>%
  ggplot() +
  geom_bar(stat="identity", position="dodge", alpha = 0.8, aes(x=Date, y=daytot, fill=SppAccp)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per species",
       title = "Bat Passes by Night, AMPHITHEATER") +
  # theme(legend.position = c(0.8, 0.8)) +
  facet_wrap(~Site, nrow=2)

