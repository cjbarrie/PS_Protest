library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(ggthemes)
library(cowplot)
library(scales)

################  PROTEST WORDS PER ABSTRACT

load("data/analysis/bib_records.RData")

journals <- unique(M_all$SO)
psjournals <- journals[c(1, 3, 5, 7, 9, 10, 14)]
socjournals <- setdiff(journals, psjournals)

pwords <-
  c(
    "protest",
    "protests",
    "protestor",
    "protestors",
    "social movement",
    "social movements",
    "contentious politics"
  )
pwords <- paste0("\\b", pwords, "\\b",  collapse = "|")


journals_list <- list(psjournals, socjournals)
gplots <- list()

for (i in seq_along(journals_list)) {
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  M <- socps_journals %>%
    filter(!is.na(AB),!is.na(PY))
  
  tidy_abs <- M %>%
    unnest_tokens(ngram, AB, token = "ngrams", n = 2)
  
  term_counts <- tidy_abs %>%
    dplyr::group_by(PY) %>%
    dplyr::count(ngram, sort = TRUE)
  
  term_counts$pword <- as.integer(grepl(pwords, 
                                          x = term_counts$ngram, ignore.case = T))
  
  yr_term_counts <- term_counts %>%
    extract(PY, "year", convert = TRUE) %>%
    complete(year, ngram, fill = list(n = 0)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(year_total = sum(n)) %>%
    dplyr::filter(pword==1) %>%
    dplyr::summarise(sum_p = sum(n),
                     year_total= min(year_total)) %>%
    na.omit()
  
  gplots[[i]] <- yr_term_counts %>%
    filter(year <= 2020) %>%
    ggplot(aes(year, (sum_p / year_total) * 100)) +
    geom_point(alpha = 0.8) +
    geom_smooth(
      method = "lm",
      linetype = "dashed",
      color = "red",
      alpha = .15
    ) +
    ylim(-.05, .25) +
    xlab("Year") +
    ylab("% protest words in journal abstracts") +
    ggtitle("") +
    theme_tufte(base_family = "Helvetica") +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15))
  
}

g1all <- gplots[[1]]
g2all <- gplots[[2]]

gplotsums <- list()
for (i in seq_along(journals_list)) {
  jlist <- journals_list[[i]]
  
  socps_journals <- M_all %>%
    filter(SO %in% jlist)
  
  MALL <- socps_journals %>%
    filter(!is.na(AB),!is.na(PY))
  
  MALL$pword <- as.integer(grepl(pwords, 
                                 x = MALL$AB, ignore.case = T))

  MALLsums <- MALL %>%
    dplyr::mutate(article = 1) %>%
    dplyr::group_by(PY) %>%
    dplyr::summarise(sum_particles = sum(pword),
                     pct_particles = (sum(pword) / sum(article)) * 100)
  
  gplotsums[[i]] <- MALLsums %>%
    filter(PY <= 2020) %>%
    ggplot() +
    geom_bar(aes(PY, pct_particles), stat = "identity") +
    xlab("Year") +
    ylab("% protest articles") +
    ggtitle("") +
    theme_tufte(base_family = "Helvetica") +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15))
  
}

g1allsums <- gplotsums[[1]]
g2allsums <- gplotsums[[2]]

g <- plot_grid(g1all,
               g1allsums,
               labels = c("", "", "", ""),
               nrow = 1)
g <-
  add_sub(
    g,
    "Political science journals",
    x = 0.5,
    hjust = 0.5,
    size = 20,
    fontface = "bold"
  )
plot_grid(g)

g1 <- plot_grid(g2all,
                g2allsums,
                labels = c("", "", "", ""),
                nrow = 1)
g1 <-
  add_sub(
    g1,
    "Sociology journals",
    x = 0.5,
    hjust = 0.5,
    size = 20,
    fontface = "bold"
  )

png(
  "figures/absplot.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
plot_grid(g, g1, labels = "AUTO", nrow = 2)
dev.off()

#######

#facetting by journal

M_all$shortitle[M_all$SO == "AMERICAN JOURNAL OF POLITICAL SCIENCE"] <-
  "AJPS"
M_all$shortitle[M_all$SO == "AMERICAN JOURNAL OF SOCIOLOGY"] <-  "AJS"
M_all$shortitle[M_all$SO == "AMERICAN POLITICAL SCIENCE REVIEW"] <-
  "APSR"
M_all$shortitle[M_all$SO == "AMERICAN SOCIOLOGICAL REVIEW"] <-  "ASR"
M_all$shortitle[M_all$SO == "BRITISH JOURNAL OF POLITICAL SCIENCE"] <-
  "BJPS"
M_all$shortitle[M_all$SO == "BRITISH JOURNAL OF SOCIOLOGY"] <-  "BJS"
M_all$shortitle[M_all$SO == "COMPARATIVE POLITICAL STUDIES"] <-  "CPS"
M_all$shortitle[M_all$SO == "INTERNATIONAL ORGANIZATION"] <-  "IO"
M_all$shortitle[M_all$SO == "EUROPEAN SOCIOLOGICAL REVIEW"] <-  "ESR"
M_all$shortitle[M_all$SO == "JOURNAL OF POLITICS"] <-  "JOP"
M_all$shortitle[M_all$SO == "SOCIAL FORCES"] <-  "SF"
M_all$shortitle[M_all$SO == "SOCIAL PROBLEMS"] <-  "SP"
M_all$shortitle[M_all$SO == "SOCIOLOGICAL METHODS & RESEARCH"] <-  "SMR"
M_all$shortitle[M_all$SO == "WORLD POLITICS"] <-  "WP"

journals_list <- list(psjournals, socjournals)
gplotsfac <- list()

for (i in seq_along(journals_list)) {
  jlist <- journals_list[[i]]
  
  M <- M_all %>%
    filter(SO %in% jlist,
           !is.na(AB),!is.na(PY))
  
  tidy_abs <- M %>%
    unnest_tokens(ngram, AB, token = "ngrams", n = 2)
  
  term_counts <- tidy_abs %>%
    dplyr::group_by(PY, shortitle) %>%
    dplyr::count(ngram, sort = TRUE)
  
  term_counts$pword <- as.integer(grepl(pwords, 
                                        x = term_counts$ngram, ignore.case = T))

  yr_term_counts <- term_counts %>%
    extract(PY, "year", convert = TRUE) %>%
    complete(year, ngram, fill = list(n = 0)) %>%
    dplyr::group_by(year, shortitle) %>%
    dplyr::summarise(year_total = sum(n))
  
  yr_term_counts_p <- term_counts %>%
    extract(PY, "year", convert = TRUE) %>%
    complete(year, ngram, fill = list(n = 0)) %>%
    dplyr::group_by(year, shortitle) %>%
    dplyr::mutate(year_total = sum(n)) %>%
    dplyr::filter(pword==1) %>%
    dplyr::summarise(sum_p = sum(n)) %>%
    na.omit()
  
  yr_term_countsm <- left_join(yr_term_counts, yr_term_counts_p, 
                               by = c("year", "shortitle"))
  
  yr_term_countsm$sum_p[is.na(yr_term_countsm$sum_p)] <- 0

  gplotsfac[[i]] <-  yr_term_countsm %>%
    ggplot(aes(year, (sum_p / year_total) * 100)) +
    geom_point(alpha = 0.8) +
    geom_smooth(
      method = "lm",
      linetype = "dashed",
      color = "red",
      alpha = .15
    ) +
    xlab("Year") +
    ylab("% protest words in abstracts") +
    scale_y_continuous(labels = comma) +
    ggtitle("") +
    theme_tufte(base_family = "Helvetica") +
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      strip.text.x = element_text(size = 20)
    ) +
    facet_wrap( ~ shortitle, scales = "free")
  
}

psbyart <- gplotsfac[[1]]
psbyart
png(
  "figures/psbyart.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
psbyart
dev.off()

socbyart <- gplotsfac[[2]]
png(
  "figures/socbyart.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
socbyart
dev.off()

#facetting by article count

gplotsfacall <- list()

for (i in seq_along(journals_list)) {
  jlist <- journals_list[[i]]
  
  M <- M_all %>%
    filter(SO %in% jlist,
           !is.na(AB),!is.na(PY))
  
  M$pword <- as.integer(grepl(pwords, 
                                 x = M$AB, ignore.case = T))
  
  MALLsums <- M %>%
    dplyr::mutate(article = 1) %>%
    dplyr::group_by(PY, shortitle) %>%
    dplyr::summarise(sum_particles = sum(pword),
                     pct_particles = (sum(pword) / sum(article)) * 100)
  
  gplotsfacall[[i]] <- ggplot(MALLsums) +
    geom_bar(aes(PY, pct_particles), stat = "identity") +
    xlab("Year") +
    ylab("% protest articles") +
    ylim(0, 8) +
    ggtitle("") +
    scale_x_discrete(breaks = c(2000, 2005, 2010, 2015)) +
    theme_tufte(base_family = "Helvetica") +
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      strip.text.x = element_text(size = 20)
    ) +
    facet_wrap( ~ shortitle)
  
}

psfba <- gplotsfacall[[1]]
png(
  "figures/psfba.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
psfba
dev.off()

socfba <- gplotsfacall[[2]]
png(
  "figures/socfba.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
socfba
dev.off()