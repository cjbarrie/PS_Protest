library(haven)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(cowplot)

#This uses latest version of the MMD data updated Feb 7, 2019--data only goes up to middle of 2018.

dat <- read_dta("data/tseries/mmALL_020619_v15.dta")
dat$event <- 1
datsums <- dat %>%
  group_by(year) %>%
  filter(year<=2017) %>%
  summarise(events = sum(event))

mmdts <- ggplot(datsums, aes(year, events)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") + scale_y_continuous(breaks=c(400, 600, 800)) +
  scale_x_continuous(breaks=c(1990, 1999, 2008, 2017)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# png("figures/mmdts.png", 
#     width = 400, height = 300, units='mm', res = 300)
# mmdts
# dev.off()
    
#SCAD AFRICA

dat <- read.csv("data/tseries/SCAD2018Africa_Final.csv")

dat$event <- 1
datsums <- dat %>%
  group_by(eyr) %>%
  summarise(events = sum(event))

scadats <- ggplot(datsums, aes(eyr, events)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") +
  scale_x_continuous(breaks=c(1990, 1999, 2008, 2017)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

#SCAD LATIN AMERICA

dat <- read.csv("data/tseries/SCAD2018LatinAmerica_Final.csv")

dat$event <- 1
datsums <- dat %>%
  group_by(eyr) %>%
  summarise(events = sum(event))

scadlats <- ggplot(datsums, aes(eyr, events)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# MMAD

dat <- read.csv("data/tseries/events.csv")

dat$date <- as.Date(dat$event_date)
dat$year <- format(as.Date(dat$date, format="%Y/%m/%d"),"%Y")

dat$event <- 1
datsums <- dat %>%
  group_by(year) %>%
  filter(year>2002) %>%
  summarise(events = sum(event))

datsums$group <- 1
datsums$year <- as.numeric(datsums$year)
#Our transformation function
scaleFUN <- function(x) sprintf("%.0f", x)

mmadts <- ggplot(datsums, aes(year, events, group=group)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_x_continuous(breaks = c(2003, 2007,2011,2015), labels = scaleFUN)


#ACLED

dat <- read.csv("data/tseries/1997-01-01-2019-12-17.csv")

datbeg <- dat %>%
  filter(year>=1997 & year <=2000)
unique(datbeg$country)
begcnt <- unique(datbeg$country)

#filter to events that are in countries that have been meaasured from 2000
datbeg <- dat %>%
  filter(country %in% begcnt)

dat <- datbeg
dat$event <- 1
datsums <- dat %>%
  group_by(year) %>%
  summarise(events = sum(event))

acledts <- ggplot(datsums, aes(year, events)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") + scale_y_continuous(breaks=c(5000, 12500, 20000)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_x_continuous(breaks = c(1997,2004,2011,2018), labels = scaleFUN)


#POLCON

dat <- read.csv("data/tseries/PEA_Data_v1.csv")

dat$date <- as.Date(dat$doc_publdate)
dat$year <- format(as.Date(dat$date, format="%Y/%m/%d"),"%Y")

dat$event <- 1
dat <- dat[!is.na(dat$year),]
datsums <- dat %>%
  group_by(year) %>%
  summarise(events = sum(event))

datsums$group <- 1
datsums$year <- as.numeric(datsums$year)
#Our transformation function
scaleFUN <- function(x) sprintf("%.0f", x)


polconts <- ggplot(datsums, aes(year, events, group=group)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Events") + xlab("") + scale_y_continuous(breaks=c(1250, 1750, 2250)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

#VDEM

datsums <- read.csv("data/tseries/v-Dem_cademmobsums.csv")
datsums$events <- datsums$v2cademmob_ord
datsums <- datsums %>%
  filter(year>=1990)
vdts <- ggplot(datsums, aes(year, events)) +
  geom_line() + theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  ylab("Event scale") + xlab("") + scale_y_continuous(breaks=c(150,210,270)) +
  scale_x_continuous(breaks=c(1990, 1999, 2008, 2017)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y  = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
###################################

plot_grid(mmdts, mmadts, scadats, vdts, acledts, polconts,
          labels = c("MMD", "MMAD", "SCAD Africa", "V-Dem", "ACLED","POLCON"), nrow=3,
          label_fontfamily = "Helvetica", label_fontface = "bold")

png("figures/tsplot.png", 
    width = 400, height = 300, units='mm', res = 300)
plot_grid(mmdts, mmadts, scadats, vdts, acledts, polconts,
          labels = c("MMD", "MMAD", "SCAD Africa", "V-Dem", "ACLED","POLCON"), nrow=3,
          label_fontfamily = "Helvetica", label_fontface = "bold", label_size = 18)
dev.off()
