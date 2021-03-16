library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(stringr)
library(ggthemes)
library(cowplot)
library(scales)

################  ALL POLITICAL SCIENCE

load("data/analysis/bib_records.RData")

journals <- unique(M_all$SO)
psjournals <- journals[c(1,3,5,7,8,10,11,15)]
socjournals <- setdiff(journals, psjournals)



MALL <- rbind(MAPSR, MAJPS,MJOP,MCPS,MBJPS,MWP,MIO)

#get rid of weird data.frame/bibliometrixDB class by converting to and from matrix class
MALLm <- as.matrix(MALL)
MALL <- as.data.frame(MALLm)

M <- MALL %>%
  filter(!is.na(AB))

tidy_abs <- M %>% 
  unnest_tokens(ngram, AB, token = "ngrams", n = 2)

tidy_abs$pword1 <- grepl("\\bprotest\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword2 <- grepl("\\bprotestor\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword3 <- grepl("\\bprotestors\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword4 <- grepl("\\bsocial movement\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword5 <- grepl("\\bsocial movements\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword6 <- grepl("\\bcontentious politics\\b", tidy_abs$ngram, ignore.case = T)
# tidy_abs$pword7 <- grepl("\\bnonviolence\\b", tidy_abs$ngram, ignore.case = T)
# tidy_abs$pword8 <- grepl("\\bnonviolent resistance\\b", tidy_abs$ngram, ignore.case = T)

tidy_abs$pword <- ifelse(tidy_abs$pword1==T|
                           tidy_abs$pword2==T|
                           tidy_abs$pword3==T|
                           tidy_abs$pword4==T|
                           tidy_abs$pword5==T|
                           tidy_abs$pword6==T,1,0)

tidy_abs$ngram[tidy_abs$pword==1] <- "pword"

term_counts <- tidy_abs %>% 
  dplyr::group_by(PY) %>% 
  dplyr::count(ngram, sort = TRUE)

pword_counts <- term_counts %>%
  filter(ngram=="pword")

yr_term_counts <- term_counts %>%
  extract(PY, "year", convert = TRUE) %>%
  complete(year, ngram, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(n))

g1all <- yr_term_counts %>%
  filter(ngram  == "pword") %>%
  ggplot(aes(year, (n/year_total)*100)) +
  #geom_bar(stat = "identity", alpha =.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  xlab("Year") +
  ylab("% protest words in journal abstracts") +
  ggtitle("") +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))

g1all

MALL <- MALL %>%
  filter(!is.na(PY))

MALL$protest <- grepl("\\bprotest\\b", MALL$AB, ignore.case = T)
MALL$protests <- grepl("\\bprotests\\b", MALL$AB, ignore.case = T)
MALL$protestor <- grepl("\\bprotestor\\b", MALL$AB, ignore.case = T)
MALL$protestors <- grepl("\\bprotestors\\b", MALL$AB, ignore.case = T)
#note there are no obs. for protestor and protestors so can remove
MALL$sm <- grepl("\\bsocial movement\\b", MALL$AB, ignore.case = T)
MALL$sms <- grepl("\\bsocial movements\\b", MALL$AB, ignore.case = T)
MALL$cp <- grepl("\\bcontentious politics\\b", MALL$AB, ignore.case = T)

MALL$protestb <- MALL$protest*1
MALL$protestsb <- MALL$protests*1
MALL$smb <- MALL$sm*1
MALL$smsb <- MALL$sms*1
MALL$cpb <- MALL$cp*1

MALL$pword <- ifelse(MALL$protestb+MALL$protestsb+MALL$smb+MALL$smsb+MALL$cpb >0, 1,0)

MALL$article <- 1
MALLsums <- MALL %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(sum_particles = sum(pword),
            pct_particles = (sum(pword)/sum(article))*100)

g1allsums <- ggplot(MALLsums) +
  geom_bar(aes(PY, pct_particles), stat = "identity") +
  xlab("Year") +
  ylab("% protest articles") +
  ggtitle("") +
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2020)) +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))

g1allsums


############# All soc.

MALL <- rbind(MASR, MAJS,MSF, MBJS,MESR,MSP,MSMR)
#get rid of weird data.frame/bibliometrixDB class by converting to and from matrix class
MALLm <- as.matrix(MALL)
MALL <- as.data.frame(MALLm)
M <- MALL %>%
  filter(!is.na(AB))

tidy_abs <- M %>% 
  unnest_tokens(ngram, AB, token = "ngrams", n = 2)

tidy_abs$pword1 <- grepl("\\bprotest\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword2 <- grepl("\\bprotestor\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword3 <- grepl("\\bprotestors\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword4 <- grepl("\\bsocial movement\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword5 <- grepl("\\bsocial movements\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword6 <- grepl("\\bcontentious politics\\b", tidy_abs$ngram, ignore.case = T)
# tidy_abs$pword7 <- grepl("\\bnonviolence\\b", tidy_abs$ngram, ignore.case = T)
# tidy_abs$pword8 <- grepl("\\bnonviolent resistance\\b", tidy_abs$ngram, ignore.case = T)

tidy_abs$pword <- ifelse(tidy_abs$pword1==T|
                           tidy_abs$pword2==T|
                           tidy_abs$pword3==T|
                           tidy_abs$pword4==T|
                           tidy_abs$pword5==T|
                           tidy_abs$pword6==T,1,0)

tidy_abs$ngram[tidy_abs$pword==1] <- "pword"

term_counts <- tidy_abs %>% 
  dplyr::group_by(PY) %>% 
  dplyr::count(ngram, sort = TRUE)

pword_counts <- term_counts %>%
  filter(ngram=="pword")

yr_term_counts <- term_counts %>%
  extract(PY, "year", convert = TRUE) %>%
  complete(year, ngram, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(n))

g2all <- yr_term_counts %>%
  filter(ngram  == "pword") %>%
  ggplot(aes(year, (n/year_total)*100)) +
  #geom_bar(stat = "identity", alpha =.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  xlab("Year") +
  ylab("% protest words in journal abstracts") +
  ggtitle("") +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))

g2all

MALL <- MALL %>%
  filter(!is.na(PY))

MALL$protest <- grepl("\\bprotest\\b", MALL$AB, ignore.case = T)
MALL$protests <- grepl("\\bprotests\\b", MALL$AB, ignore.case = T)
MALL$protestor <- grepl("\\bprotestor\\b", MALL$AB, ignore.case = T)
MALL$protestors <- grepl("\\bprotestors\\b", MALL$AB, ignore.case = T)
#note there are no obs. for protestor and protestors so can remove
MALL$sm <- grepl("\\bsocial movement\\b", MALL$AB, ignore.case = T)
MALL$sms <- grepl("\\bsocial movements\\b", MALL$AB, ignore.case = T)
MALL$cp <- grepl("\\bcontentious politics\\b", MALL$AB, ignore.case = T)

MALL$protestb <- MALL$protest*1
MALL$protestsb <- MALL$protests*1
MALL$smb <- MALL$sm*1
MALL$smsb <- MALL$sms*1
MALL$cpb <- MALL$cp*1

MALL$pword <- ifelse(MALL$protestb+MALL$protestsb+MALL$smb+MALL$smsb+MALL$cpb >0, 1,0)

MALL$article <- 1
MALLsums <- MALL %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(sum_particles = sum(pword),
            pct_particles = (sum(pword)/sum(article))*100)

g2allsums <- ggplot(MALLsums) +
  geom_bar(aes(PY, pct_particles), stat = "identity") +
  xlab("Year") +
  ylab("% protest articles") +
  ggtitle("") +
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))

g2allsums

g <- plot_grid(g1all,g1allsums, labels = c("","","",""), nrow = 1)
g <- add_sub(g, "Political science journals", x = 0.5, hjust = 0.5, size=20, fontface = "bold")
plot_grid(g)

g1 <- plot_grid(g2all,g2allsums, labels = c("","","",""), nrow = 1)
g1 <- add_sub(g1, "Sociology journals", x = 0.5, hjust = 0.5, size=20,fontface = "bold")
plot_grid(g, g1, labels= "AUTO", nrow=2)

png("/Users/christopherbarrie/Dropbox/pd_projects/PS_protest/figures/absplot.png", 
    width = 400, height = 300, units='mm', res = 300)
plot_grid(g, g1, labels= "AUTO", nrow=2)
dev.off()

#######

#facetting by journal
MALL <- rbind(MAPSR, MAJPS,MJOP,MCPS,MBJPS,MWP,MIO, MASR, MAJS,MSF, MBJS,MESR,MSP,MSMR)
#get rid of weird data.frame/bibliometrixDB class by converting to and from matrix class
MALLm <- as.matrix(MALL)
MALL <- as.data.frame(MALLm)
M <- MALL %>%
  filter(!is.na(AB))

tidy_abs <- M %>% 
  unnest_tokens(ngram, AB, token = "ngrams", n = 2)

tidy_abs$pword1 <- grepl("\\bprotest\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword2 <- grepl("\\bprotestor\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword3 <- grepl("\\bprotestors\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword4 <- grepl("\\bsocial movement\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword5 <- grepl("\\bsocial movements\\b", tidy_abs$ngram, ignore.case = T)
tidy_abs$pword6 <- grepl("\\bcontentious politics\\b", tidy_abs$ngram, ignore.case = T)

tidy_abs$pword <- ifelse(tidy_abs$pword1==T|
                           tidy_abs$pword2==T|
                           tidy_abs$pword3==T|
                           tidy_abs$pword4==T|
                           tidy_abs$pword5==T|
                           tidy_abs$pword6==T,1,0)

tidy_abs$ngram[tidy_abs$pword==1] <- "pword"

term_counts <- tidy_abs %>% 
  dplyr::group_by(PY, SO) %>% 
  dplyr::count(ngram, sort = TRUE)

pword_counts <- term_counts %>%
  filter(ngram=="pword")

yr_term_counts <- term_counts %>%
  extract(PY, "year", convert = TRUE) %>%
  complete(year, ngram, fill = list(n = 0)) %>%
  group_by(year, SO) %>%
  mutate(year_total = sum(n))

yr_term_counts$SO[yr_term_counts$SO=="AMERICAN JOURNAL OF POLITICAL SCIENCE"] <-  "AJPS"
yr_term_counts$SO[yr_term_counts$SO=="AMERICAN JOURNAL OF SOCIOLOGY"] <-  "AJS"
yr_term_counts$SO[yr_term_counts$SO=="AMERICAN POLITICAL SCIENCE REVIEW"] <-  "APSR"
yr_term_counts$SO[yr_term_counts$SO=="AMERICAN SOCIOLOGICAL REVIEW"] <-  "ASR"
yr_term_counts$SO[yr_term_counts$SO=="BRITISH JOURNAL OF POLITICAL SCIENCE"] <-  "BJPS"
yr_term_counts$SO[yr_term_counts$SO=="BRITISH JOURNAL OF SOCIOLOGY"] <-  "BJS"
yr_term_counts$SO[yr_term_counts$SO=="COMPARATIVE POLITICAL STUDIES"] <-  "CPS"
yr_term_counts$SO[yr_term_counts$SO=="INTERNATIONAL ORGANIZATION"] <-  "IO"
yr_term_counts$SO[yr_term_counts$SO=="EUROPEAN SOCIOLOGICAL REVIEW"] <-  "ESR"
yr_term_counts$SO[yr_term_counts$SO=="JOURNAL OF POLITICS"] <-  "JOP"
yr_term_counts$SO[yr_term_counts$SO=="SOCIAL FORCES"] <-  "SF"
yr_term_counts$SO[yr_term_counts$SO=="SOCIAL PROBLEMS"] <-  "SP"
yr_term_counts$SO[yr_term_counts$SO=="SOCIOLOGICAL METHODS & RESEARCH"] <-  "SMR"
yr_term_counts$SO[yr_term_counts$SO=="WORLD POLITICS"] <-  "WP"

yr_term_counts$PS <- ifelse(yr_term_counts$SO=="AJPS"|
                              yr_term_counts$SO=="APSR"|
                              yr_term_counts$SO=="BJPS"|
                              yr_term_counts$SO=="CPS"|
                              yr_term_counts$SO=="JOP"|
                              yr_term_counts$SO=="WP"|
                              yr_term_counts$SO=="IO", 1,0)
##polisci

yr_term_countsps <- yr_term_counts %>%
  filter(PS==1)

psbyart <- yr_term_countsps %>%
  filter(ngram  == "pword") %>%
  ggplot(aes(year, (n/year_total)*100)) +
  #geom_bar(stat = "identity", alpha =.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  xlab("Year") +
  ylab("% protest words in abstracts") +
  scale_y_continuous(labels=comma) +
  ggtitle("") +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20)) +
  facet_wrap(~SO,scales = "free")

psbyart

png("/Users/christopherbarrie/Dropbox/pd_projects/PS_protest/figures/psbyart.png", 
    width = 400, height = 300, units='mm', res = 300)
psbyart
dev.off()

yr_term_countsoc <- yr_term_counts %>%
  filter(PS==0)

socbyart <- yr_term_countsoc %>%
  filter(ngram  == "pword") %>%
  ggplot(aes(year, (n/year_total)*100)) +
  #geom_bar(stat = "identity", alpha =.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
  xlab("Year") +
  ylab("% protest words in abstracts") +
  scale_y_continuous(labels=comma) +
  ggtitle("") +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20)) +
  facet_wrap(~SO,scales = "free")
  
socbyart  

png("/Users/christopherbarrie/Dropbox/pd_projects/PS_protest/figures/socbyart.png", 
    width = 400, height = 300, units='mm', res = 300)
socbyart
dev.off()

##by article count
#MALL <- rbind(MAPSR, MAJPS,MJOP,MCPS,MBJPS,MIO,MWP,MASR, MAJS,MSF, MBJS,MESR,MSP,MSMR)

MALLm <- as.matrix(MALL)
MALL <- as.data.frame(MALLm)
MALL <- MALL %>%
  filter(!is.na(PY))

MALL$protest <- grepl("\\bprotest\\b", MALL$AB, ignore.case = T)
MALL$protests <- grepl("\\bprotests\\b", MALL$AB, ignore.case = T)
MALL$protestor <- grepl("\\bprotestor\\b", MALL$AB, ignore.case = T)
MALL$protestors <- grepl("\\bprotestors\\b", MALL$AB, ignore.case = T)
#note there are no obs. for protestor and protestors so can remove
MALL$sm <- grepl("\\bsocial movement\\b", MALL$AB, ignore.case = T)
MALL$sms <- grepl("\\bsocial movements\\b", MALL$AB, ignore.case = T)
MALL$cp <- grepl("\\bcontentious politics\\b", MALL$AB, ignore.case = T)

MALL$protestb <- MALL$protest*1
MALL$protestsb <- MALL$protests*1
MALL$smb <- MALL$sm*1
MALL$smsb <- MALL$sms*1
MALL$cpb <- MALL$cp*1

MALL$pword <- ifelse(MALL$protestb+MALL$protestsb+MALL$smb+MALL$smsb+MALL$cpb >0, 1,0)

MALL$article <- 1

MALL$SO[MALL$SO=="AMERICAN JOURNAL OF POLITICAL SCIENCE"] <-  "AJPS"
MALL$SO[MALL$SO=="AMERICAN JOURNAL OF SOCIOLOGY"] <-  "AJS"
MALL$SO[MALL$SO=="AMERICAN POLITICAL SCIENCE REVIEW"] <-  "APSR"
MALL$SO[MALL$SO=="AMERICAN SOCIOLOGICAL REVIEW"] <-  "ASR"
MALL$SO[MALL$SO=="BRITISH JOURNAL OF POLITICAL SCIENCE"] <-  "BJPS"
MALL$SO[MALL$SO=="BRITISH JOURNAL OF SOCIOLOGY"] <-  "BJS"
MALL$SO[MALL$SO=="COMPARATIVE POLITICAL STUDIES"] <-  "CPS"
MALL$SO[MALL$SO=="EUROPEAN SOCIOLOGICAL REVIEW"] <-  "ESR"
MALL$SO[MALL$SO=="INTERNATIONAL ORGANIZATION"] <-  "IO"
MALL$SO[MALL$SO=="JOURNAL OF POLITICS"] <-  "JOP"
MALL$SO[MALL$SO=="SOCIAL FORCES"] <-  "SF"
MALL$SO[MALL$SO=="SOCIAL PROBLEMS"] <-  "SP"
MALL$SO[MALL$SO=="SOCIOLOGICAL METHODS & RESEARCH"] <-  "SMR"
MALL$SO[MALL$SO=="WORLD POLITICS"] <-  "WP"

MALL$PS <- ifelse(MALL$SO=="AJPS"|
                    MALL$SO=="APSR"|
                    MALL$SO=="BJPS"|
                    MALL$SO=="CPS"|
                    MALL$SO=="IO"|
                    MALL$SO=="JOP"|
                    MALL$SO=="WP", 1,0)


MALLps <- MALL %>%
  filter(PS==1)

MALLsums <- MALLps %>%
  dplyr::group_by(PY, SO) %>%
  dplyr::summarise(sum_particles = sum(pword),
            pct_particles = (sum(pword)/sum(article))*100)
psfba <- ggplot(MALLsums) +
  geom_bar(aes(PY, pct_particles), stat = "identity") +
  xlab("Year") +
  ylab("% protest articles") +
  ylim(0,8) +
  ggtitle("") +
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015)) +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20)) + 
  facet_wrap(~SO)

psfba

png("/Users/christopherbarrie/Dropbox/pd_projects/PS_protest/figures/psfba.png", 
    width = 400, height = 300, units='mm', res = 300)
psfba
dev.off()

MALLsoc <- MALL %>%
  filter(PS==0)

MALLsums <- MALLsoc %>%
  dplyr::group_by(PY, SO) %>%
  dplyr::summarise(sum_particles = sum(pword),
            pct_particles = (sum(pword)/sum(article))*100)
socfba <- ggplot(MALLsums) +
  geom_bar(aes(PY, pct_particles), stat = "identity") +
  xlab("Year") +
  ylab("% protest articles") +
  ylim(0,8) +
  ggtitle("") +
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015)) +
  theme_tufte(base_family = "GillSans") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20)) + 
  facet_wrap(~SO)

socfba

png("/Users/christopherbarrie/Dropbox/pd_projects/PS_protest/figures/socfba.png", 
    width = 400, height = 300, units='mm', res = 300)
socfba
dev.off()
  