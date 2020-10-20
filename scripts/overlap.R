# code to learn R package 'overlap' (Ridout and Linkie 2009, 2011)

install.packages('overlap')
install.packages("suncalc")
library(overlap)
library(suncalc)
library(lubridate)
data("kerinci")
head(kerinci)

range(kerinci$Time)
timeRad <- kerinci$Time * 2 * pi

tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
densityPlot(tig2, rug=T)

sun_t <- getSunlightTimes(as.Date(t$Date), lat=36.73, lon=-118.36, tz = "America/Los_Angeles") 
moon_t <- getMoonTimes(as.Date(t$Date), lat=36.73, lon=-118.36, tz = "America/Los_Angeles") 
moon_p <- getMoonIllumination(as.Date(t$Date))

bat <- freq_group %>% dplyr::select(Site, Basin, Treatment, Date, Time, HiF, LoF, SppAccp, Fc.mean)
bat$TimeDec <- as.numeric((bat$Time*86400)/86400)
head(bat)

#comparing kernel density of HiF bats at fishless versus fish-containing lakes

timeRad <- bat$TimeDec * 2 * pi
batHi_NF <- timeRad[bat$HiF == 1 & bat$Treatment=="fishless"]
batHi_F <- timeRad[bat$HiF == 1 & bat$Treatment=="fish"]

batLo_NF <- timeRad[bat$LoF == 1 & bat$Treatment=="fishless"]
batHi_F <- timeRad[bat$LoF == 1 & bat$Treatment=="fish"]

densityPlot(batHi_NF, rug=T, xcenter = 'midnight', add = T, col="red")
densityPlot(batLo_NF, rug=T, xcenter = 'midnight', col="blue")

# TODO: do sunrise/sunset times by site location (split apply combine by site?)
sunrise <- as.hms(sun_t$sunrise)
sunset <- as.hms(sun_t$sunset)
