library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(ggthemes)
library(cowplot)
library(scales)

################  ALL POLITICAL SCIENCE

load("data/analysis/bib_records.RData")
# load("data/analysis/bib_precords.RData")

journals <- unique(M_all$SO)
psjournals <- journals[c(1,3,5,7,9,10,14)]
socjournals <- setdiff(journals, psjournals)

journals_list <- list(psjournals, socjournals)
gplots <- list()

for (i in seq_along(journals_list)) {
  
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  M <- socps_journals %>%
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
    dplyr::group_by(PY) %>% 
    dplyr::count(ngram, sort = TRUE)
  
  yr_term_counts <- term_counts %>%
    extract(PY, "year", convert = TRUE) %>%
    complete(year, ngram, fill = list(n = 0)) %>%
    group_by(year) %>%
    mutate(year_total = sum(n))
  
   gplots[[i]] <- yr_term_counts %>%
    filter(ngram  == "pword") %>%
    ggplot(aes(year, (n/year_total)*100)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
    xlab("Year") +
    ylab("% protest words in journal abstracts") +
    ggtitle("") +
    theme_tufte(base_family = "Helvetica") + 
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=15))
  
}

g1all <- gplots[[1]]
g2all <- gplots[[2]]

gplotsums <- list()
for (i in seq_along(journals_list)) {
  
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  MALL <- socps_journals %>%
    filter(!is.na(AB),
           !is.na(PY))
  
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
  
  gplotsums[[i]] <- ggplot(MALLsums) +
    geom_bar(aes(PY, pct_particles), stat = "identity") +
    xlab("Year") +
    ylab("% protest articles") +
    ggtitle("") +
    scale_x_discrete(breaks=c(2000, 2005, 2010, 2020)) +
    theme_tufte(base_family = "Helvetica") + 
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=15))
  
}

g1allsums <- gplotsums[[1]]
g2allsums <- gplotsums[[2]]

g <- plot_grid(g1all,g1allsums, labels = c("","","",""), nrow = 1)
g <- add_sub(g, "Political science journals", x = 0.5, hjust = 0.5, size=20, fontface = "bold")
plot_grid(g)

g1 <- plot_grid(g2all,g2allsums, labels = c("","","",""), nrow = 1)
g1 <- add_sub(g1, "Sociology journals", x = 0.5, hjust = 0.5, size=20,fontface = "bold")
plot_grid(g, g1, labels= "AUTO", nrow=2)

png("figures/absplot.png", 
    width = 400, height = 300, units='mm', res = 300)
plot_grid(g, g1, labels= "AUTO", nrow=2)
dev.off()

#######

#facetting by journal

journals_list <- list(psjournals, socjournals)
gplotsfac <- list()

for (i in seq_along(journals_list)) {
  
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  M <- socps_journals %>%
    filter(!is.na(AB))
  
  M$shortitle[M$SO=="AMERICAN JOURNAL OF POLITICAL SCIENCE"] <-  "AJPS"
  M$shortitle[M$SO=="AMERICAN JOURNAL OF SOCIOLOGY"] <-  "AJS"
  M$shortitle[M$SO=="AMERICAN POLITICAL SCIENCE REVIEW"] <-  "APSR"
  M$shortitle[M$SO=="AMERICAN SOCIOLOGICAL REVIEW"] <-  "ASR"
  M$shortitle[M$SO=="BRITISH JOURNAL OF POLITICAL SCIENCE"] <-  "BJPS"
  M$shortitle[M$SO=="BRITISH JOURNAL OF SOCIOLOGY"] <-  "BJS"
  M$shortitle[M$SO=="COMPARATIVE POLITICAL STUDIES"] <-  "CPS"
  M$shortitle[M$SO=="INTERNATIONAL ORGANIZATION"] <-  "IO"
  M$shortitle[M$SO=="EUROPEAN SOCIOLOGICAL REVIEW"] <-  "ESR"
  M$shortitle[M$SO=="JOURNAL OF POLITICS"] <-  "JOP"
  M$shortitle[M$SO=="SOCIAL FORCES"] <-  "SF"
  M$shortitle[M$SO=="SOCIAL PROBLEMS"] <-  "SP"
  M$shortitle[M$SO=="SOCIOLOGICAL METHODS & RESEARCH"] <-  "SMR"
  M$shortitle[M$SO=="WORLD POLITICS"] <-  "WP"
  
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
    dplyr::group_by(PY, shortitle) %>% 
    dplyr::count(ngram, sort = TRUE)
  
  yr_term_counts <- term_counts %>%
    extract(PY, "year", convert = TRUE) %>%
    complete(year, ngram, fill = list(n = 0)) %>%
    group_by(year) %>%
    mutate(year_total = sum(n))
  
  gplotsfac[[i]] <-  yr_term_counts %>%
    filter(ngram  == "pword") %>%
    ggplot(aes(year, (n/year_total)*100)) +
    #geom_bar(stat = "identity", alpha =.8) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", linetype="dashed", color = "red", alpha=.15) +
    xlab("Year") +
    ylab("% protest words in abstracts") +
    scale_y_continuous(labels=comma) +
    ggtitle("") +
    theme_tufte(base_family = "Helvetica") + 
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=20),
          strip.text.x = element_text(size = 20)) +
    facet_wrap(~shortitle,scales = "free")
  
}

psbyart <- gplotsfac[[1]]
png("figures/psbyart.png", 
    width = 400, height = 300, units='mm', res = 300)
psbyart
dev.off()

socbyart <- gplotsfac[[2]]
png("figures/socbyart.png", 
    width = 400, height = 300, units='mm', res = 300)
socbyart
dev.off()

#facetting by article count

gplotsfacall <- list()

for (i in seq_along(journals_list)) {
  
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  M <- socps_journals %>%
    filter(!is.na(AB),
           !is.na(PY)) 
  
  M$shortitle[M$SO=="AMERICAN JOURNAL OF POLITICAL SCIENCE"] <-  "AJPS"
  M$shortitle[M$SO=="AMERICAN JOURNAL OF SOCIOLOGY"] <-  "AJS"
  M$shortitle[M$SO=="AMERICAN POLITICAL SCIENCE REVIEW"] <-  "APSR"
  M$shortitle[M$SO=="AMERICAN SOCIOLOGICAL REVIEW"] <-  "ASR"
  M$shortitle[M$SO=="BRITISH JOURNAL OF POLITICAL SCIENCE"] <-  "BJPS"
  M$shortitle[M$SO=="BRITISH JOURNAL OF SOCIOLOGY"] <-  "BJS"
  M$shortitle[M$SO=="COMPARATIVE POLITICAL STUDIES"] <-  "CPS"
  M$shortitle[M$SO=="INTERNATIONAL ORGANIZATION"] <-  "IO"
  M$shortitle[M$SO=="EUROPEAN SOCIOLOGICAL REVIEW"] <-  "ESR"
  M$shortitle[M$SO=="JOURNAL OF POLITICS"] <-  "JOP"
  M$shortitle[M$SO=="SOCIAL FORCES"] <-  "SF"
  M$shortitle[M$SO=="SOCIAL PROBLEMS"] <-  "SP"
  M$shortitle[M$SO=="SOCIOLOGICAL METHODS & RESEARCH"] <-  "SMR"
  M$shortitle[M$SO=="WORLD POLITICS"] <-  "WP"
  
  M$protest <- grepl("\\bprotest\\b", M$AB, ignore.case = T)
  M$protests <- grepl("\\bprotests\\b", M$AB, ignore.case = T)
  M$protestor <- grepl("\\bprotestor\\b", M$AB, ignore.case = T)
  M$protestors <- grepl("\\bprotestors\\b", M$AB, ignore.case = T)
  #note there are no obs. for protestor and protestors so can remove
  M$sm <- grepl("\\bsocial movement\\b", M$AB, ignore.case = T)
  M$sms <- grepl("\\bsocial movements\\b", M$AB, ignore.case = T)
  M$cp <- grepl("\\bcontentious politics\\b", M$AB, ignore.case = T)
  
  M$protestb <- M$protest*1
  M$protestsb <- M$protests*1
  M$smb <- M$sm*1
  M$smsb <- M$sms*1
  M$cpb <- M$cp*1
  
  M$pword <- ifelse(M$protestb+M$protestsb+M$smb+M$smsb+M$cpb >0, 1,0)
  
  M$article <- 1
  
  Msums <- M %>%
    dplyr::group_by(PY, shortitle) %>%
    dplyr::summarise(sum_particles = sum(pword),
                     pct_particles = (sum(pword)/sum(article))*100)
  
  gplotsfacall[[i]] <- ggplot(Msums) +
    geom_bar(aes(PY, pct_particles), stat = "identity") +
    xlab("Year") +
    ylab("% protest articles") +
    ylim(0,8) +
    ggtitle("") +
    scale_x_discrete(breaks=c(2000, 2005, 2010, 2015)) +
    theme_tufte(base_family = "Helvetica") + 
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=20),
          strip.text.x = element_text(size = 20)) + 
    facet_wrap(~shortitle)
  
}

psfba <- gplotsfacall[[1]]
png("figures/psfba.png", 
    width = 400, height = 300, units='mm', res = 300)
psfba
dev.off()

socfba <- gplotsfacall[[2]]
png("figures/socfba.png", 
    width = 400, height = 300, units='mm', res = 300)
socfba
dev.off()