## code to explore SonoBat output
## authors: M Clapp, M Boynton
## date created: 21 Jan 2019
## date edited: 21 Jan 2019: reads new SonoBat output generated 1/16/2019 

rm(list = ls()) # this line of code removes EVERYTHING from the working environment

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(chron)
library(lme4)
#library(MASS)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# TO-DO LIST --------------------------------------------------------------
file_names <- list.files("data/SonoBatch_Output", full.names = TRUE, pattern = ".txt")
main <- read.csv("data/SonoBatch_Output/Shorts_Combined_CumSonoBatch_v430.csv")
d <- do.call("rbind",lapply(file_names,read.delim))
colnames(d)
colnames(main)

t <- rbind(d, main)

# NEW data 1/25/2019 from Leila -------------------------------------------

#t <- read_csv(file = "./data/Shorts_Combined_CumSonoBatch_v430.csv")
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
  separate(Filename, c("Site", "Date", "Time"), sep = "_", remove = FALSE) %>%
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

# detect2 removes entries w/ both HiF and LoF checked (these are likely artifacts from noise)
detect2 <- detect %>% filter(dbl=="not.dbl") %>% 
  separate(Site, c("Basin", "Treatment"), sep=-1, remove=FALSE)
  
detect2$Treatment <- factor(ifelse(gsub('[^12]', '', detect2$Site) == "2", "fish", "fishless"))

# PLOTS -------------------------------------------------------------------
# HiF and LoF over time, by Site
# all 3 basins ------------------------------------------------------------

freq_group <- detect2 %>%
  #select(Path, Date, Time, hr, Basin, Site, Treatment, HiF, LoF, Fc.mean, SppAccp, X.Prob, X.Spp, X1st, X2nd) %>%
  filter(Fc.mean < 50, SppAccp!=0 | X.Spp !=0 | X1st !=0) 

nighttally <- freq_group %>%
  group_by(Date, Basin, Site, Treatment) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))

qualhi <- freq_group %>%
  filter(HiF==1 & SppAccp != "") # this goes from 20,000 to 175 if I only accept entries where SppAccp has an entry
quallo <- freq_group %>%
  filter(LoF==1 & SppAccp != "")

index <- rbind(qualhi, quallo)
colnames(index)
colnames(main)

index <- index %>% select(-hr, -dbl, -Site, -Basin, -Treatment, -Timestamp, -Date, -Time)

write_csv(index, "data/indexed_samples_for_LH_20200814.csv")

# for presentation!
labels <- c(AMPHIT = "Amphitheater", CENTER = "Center", EASTLA = "East Lake")

ggplot(data=nighttally, aes(x=Date, y=nHiF, color=Treatment)) +
  geom_jitter() + 
  scale_color_manual(values = c(cbPalette[2], cbPalette[6])) +
  labs(x = "Date", 
       y = "# of bat passes per night",
       title = "Nightly Activity by High-Frequency Bats") +
  #theme_dark() +
  theme_minimal() +
  theme(plot.title = element_text(family = "Helvetica", size=16),
        axis.title = element_text(family = "Helvetica", size=12), 
        axis.text.x = element_text(family = "Helvetica", size=12, angle=50, hjust=1, vjust=1),
        axis.text.y = element_text(family = "Helvetica", size=12, angle=0),
        strip.text.x = element_text(size=16, face="italic",color = "gray50"),
        #legend.position = NULL +
        panel.background = element_rect(fill = "black", color  =  NA), 
        panel.border = element_rect(fill = NA, color = "white"),
        plot.background = element_rect(color = "black", fill = "black")) +
  facet_wrap(~Basin, nrow=3, labeller=labeller(Basin=labels))

ggplot(data=nighttally, aes(x=Date, y=nLoF, color=Treatment)) +
  geom_jitter() + 
  scale_color_manual(values = c(cbPalette[2], cbPalette[6])) +
  labs(x = "Date", 
       y = "# of bat passes per night",
       title = "Nightly Activity by Low-Frequency Bats") +
  #theme_dark() +
  theme_minimal() +
  theme(plot.title = element_text(family = "Helvetica", size=16),
        axis.title = element_text(family = "Helvetica", size=12), 
        axis.text.x = element_text(family = "Helvetica", size=12, angle=50, hjust=1, vjust=1),
        axis.text.y = element_text(family = "Helvetica", size=12, angle=0),
        strip.text.x = element_text(size=16, face="italic", color = "gray50"),
        #legend.position = "top" +
       panel.background = element_rect(fill = "black", color  =  NA),  
     panel.border = element_rect(fill = NA, color = "white"),
       plot.background = element_rect(color = "black", fill = "black")) +
  facet_wrap(~Basin, nrow=3, labeller=labeller(Basin=labels))
  

ggplot(data=nighttally, aes(x=Date, y=nLoF, color=Treatment)) +
  geom_jitter() +
  theme_dark() +
  facet_wrap(~Basin, nrow=3)

# amphitheater, july

amph.jul <- freq_group %>% filter(Basin=="AMPHIT", month(Date)==7) %>% 
  group_by(Date, Basin, Site, Treatment) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF=="1"))

ggplot(amph.jul, aes(x=Date, y=nHiF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()

ggplot(amph.jul, aes(x=Date, y=nLoF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()

# center basin, june
center.jun <- freq_group %>% filter(Basin=="CENTER", month(Date)==6 & month(Date)==7) %>% 
  group_by(Date, Basin, Site, Treatment) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF=="1"))

ggplot(center.jun, aes(x=Date, y=nHiF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()

ggplot(center.jun, aes(x=Date, y=nLoF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()

# east lake, june and july (missing data from EASTLA2 in July; broken SM)
east <- freq_group %>% filter(Basin=="EASTLA", month(Date)==6 | month(Date)==7) %>% 
  group_by(Date, Basin, Site, Treatment) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF=="1"))

ggplot(east, aes(x=Date, y=nHiF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()

ggplot(east, aes(x=Date, y=nLoF, color=Treatment)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_minimal()


# some exploration of the distribution of the data
head(nighttally)

ggplot(nighttally) +
  #geom_point(alpha = 0.6, color='red',aes(x=Date, y=nHiF)) +
  geom_point(alpha = 0.6, aes(x=Date, y=nHiF, color=Treatment)) +
  facet_wrap(~Basin, nrow = 3) +
  theme_minimal() + 
  labs(x = "date", 
       y = "number of low-frequency bat passes per night",
       title = "") +
  # scale_colour_manual(name = 'acoustic clade',
  #                     values =c('red'='red','darkblue'='darkblue'), 
  #                     labels = c('high-frequency', 'low-frequency')) +
  theme(plot.title = element_text(family = "Helvetica", size=24),
        axis.title = element_text(family = "Helvetica", size=18), 
        axis.text.x = element_text(family = "Helvetica", size=12, angle=50, hjust=1, vjust=1),
        axis.text.y = element_text(family = "Helvetica", size=12, angle=0),
        strip.text.x = element_text(size = 16))

# by time of night
by_time <- freq_group %>%
  group_by(hr, Site) %>%
  filter(hr != 10, hr != "08") %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))

by_time_2 <- detect2 %>%
  group_by(hr) %>%
  filter(hr != 10, hr !=08) %>%
  summarise(nHiF = sum(HiF == "1"), nLoF = sum(LoF == "1"))

by_time %>% filter(nHiF <750) %>%
ggplot() +
  geom_jitter(aes(x=hr, y=nHiF, color = "red")) +
  geom_jitter(aes(x=hr, y=nLoF, color = "blue")) +
  theme_dark() + 
  theme(axis.text.x = element_text(size = 20, hjust = 1),
        axis.text.y = element_text(size=20),
        axis.title = element_text(family = "Helvetica", size=18),
        legend.position = "none",
        panel.background = element_rect(fill = "black", color  =  NA),  
        panel.border = element_rect(fill = NA, color = "white"),
        plot.background = element_rect(color = "black", fill = "black")) +
  labs(x = "hour of night (24-hour time)", 
       y = "# bat passes",
       title = NULL) +
  scale_colour_manual(name = 'acoustic clade',
                      values =c('red'='red','blue'='blue'), 
                      labels = c('low-frequency', 'high-frequency')) +
  theme(legend.position =0)


## modelly stuff (not organized yet) ---------------------------------------

library(MASS)
nighttally$jday <- yday(nighttally$Date)
# These are count data, so we might conventionally start with a statistical model based on the Poisson distribution. 
#First, it can be useful to see if the data are overdispersed, or have excess 0's (zero inflation) or both.

# Compare data  to a Poisson distribution
qqplot(nighttally$nHiF, rpois(10000, lambda=mean(nighttally$nHiF))) # not really enough data
qqplot(nighttally$nLoF, rpois(10000, lambda=mean(nighttally$nLoF))) # hmm looks potentially overdispersed compared to what a poisson dist would look like
# Compare data to a negative binomial distribution
z <- fitdistr(nighttally$nHiF, densfun="negative binomial")
qqplot(nighttally$nHiF, rnegbin(10000, mu=z$estimate[2], theta=z$estimate[1])) # more like dis

# Q: are the data obviously overdispersed relative to the Poisson distribution? 

# If so, is the overdispersion of counts mainly due to among-site variation? First is there a lot of variation among sites? 
boxplot(nLoF~Site, nighttally)

# As a null model, we could just fit a glm with no site effects:
m0 <- glm.nb(nHiF~1, data=nighttally)
summary(m0)
# Next, as we've done before, to add a random intercept for the grouping variable "site". 
#This time, though, we will use a Negative Binomial distribution (because of above), rather than a Gaussian / normal distribution.

m1 <- glm.nb(nHiF~Treatment, data=nighttally)
summary(m1)
qqnorm(resid(m1))
plot(fitted(m1)~nighttally$nHiF)
plot(resid(m1)~fitted(m1))

# add julian day to model

nighttally$jday <- yday(nighttally$Date)

m2 <- glm.nb(nHiF~Treatment + (1|jday), data=nighttally)
summary(m2)
anova(m1,m2)

m3 <- glm.nb(nHiF~Treatment + Basin + (1|jday), data=nighttally)
summary(m3)
anova(m0, m3)

# is basin actually important?
m4 <- update(m3, . ~ . - Basin)
anova(m3, m4)
# honestly dunno
#lets make some predictions

library("effects")
plot(allEffects(m3), ask = FALSE, rescale = FALSE)
# predicting values [UNDER CONSTRUCTION!!!!!]
est <- cbind(Estimate = coef(m3), confint(m3))
exp(est)

est4 <- cbind(Estimate = coef(m4), confint(m4))
exp(est4)

newdata2 <- data.frame(
  jday = rep(seq(from = min(nighttally$jday), to = max(nighttally$jday), length.out = 300), 2),
  Treatment = factor(rep(1:2, each = 300), levels = 1:2, labels =
                  levels(nighttally$Treatment)),
  Basin = factor(rep(1:3, each=100), levels = 1:3, labels = levels(nighttally$Basin)))

newdata2 <- cbind(newdata2, predict(m3, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  BatPasses <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(jday, BatPasses)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Treatment), alpha = .25) +
  geom_line(aes(colour = Treatment), size = 2) +
  labs(x = "jday", y = "Predicted Bat Passes")
#m4 <- glm.nb(nHiF~Treatment+(1|Basin) + (1|jday), data = nighttally)
#for some reason I can't add Basin as a random effect whateverrrrr

#Poisson models down here
# m3 <- glmer(nLoF~Treatment + (1|Basin), data=nighttally, family="poisson")
# summary(m3)
# 
# m4 <- glmer(nLoF~Treatment+Basin+(1|Site), data=nighttally, family="poisson")
# summary(m4)
# 
# anova(m2,m4)
# library(lme4)

hist(nighttally$nHiF, breaks = 50)
hist(nighttally$nLoF, breaks=50)

m1 <- glmer(nLoF ~ Treatment + (1|Basin/Site), family = poisson, data = nighttally)
summary(m1)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(m1)




# patterns by species -----------------------------------------------------

# patterns by species (HUGE grain of salt right now bc no manual vetting has occurred yet)

unique(freq_group$SppAccp)
spp <- freq_group %>% group_by(Date, Site, SppAccp) %>%
  summarise(daytot = n()) %>%
  separate(Site, c("Basin", "Fish"), sep=-1, remove=FALSE) %>%
  mutate(month = as.character(month(Date)))

spp %>% 
  ggplot() +
  geom_bar(stat="identity", position="dodge", alpha = 0.8, aes(x=Date, y=daytot, fill=SppAccp)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "date", 
       y = "number of bat passes per species",
       title = "Bat Passes by Night, AMPHITHEATER") +
  # theme(legend.position = c(0.8, 0.8)) +
  facet_wrap(~Site, nrow=2)

