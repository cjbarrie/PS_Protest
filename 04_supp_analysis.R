library(tidyverse)
library(bibliometrix)
library(tidytext)
library(ggthemes)

################
load("data/analysis/bib_records_supp.RData")

tidy_kwds <- M_all_supp %>% 
  select(AU, TI, SO, PY, ID) %>%
  mutate(kwd = tolower(ID)) %>%
  unnest_tokens(word, kwd, token = stringr::str_split, pattern = "; ")

term_counts <- tidy_kwds %>%
  dplyr::group_by(PY) %>%
  dplyr::count(word, sort = TRUE)

term_counts$outword <- as.integer(grepl("\\battitudes\\b|\\bconsequences\\b|\\boutcomes\\b|\\bpublic-opinion\\b|\\bimpact\\b", 
                                             x = term_counts$word))

yr_term_counts <- term_counts %>%
  extract(PY, "year", convert = TRUE) %>%
  complete(year, word, fill = list(n = 0)) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(year<=2020) %>%
  dplyr::mutate(year_total = sum(n)) %>%
  dplyr::filter(outword==1) %>%
  dplyr::summarise(sum_out = sum(n),
            year_total= min(year_total)) %>%
  na.omit()

ggplot(yr_term_counts, aes(year, sum_out / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% outcome-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica")
