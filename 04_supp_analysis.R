library(tidyverse)
library(bibliometrix)
library(tidytext)
library(ggthemes)
library(cowplot)

################
load("data/analysis/bib_records_supp.RData")

journals <- unique(M_all_supp$SO)
pjournals <- journals[c(5, 9)]
npjournals <- setdiff(journals, pjournals)


# Moby and SMS

tidy_kwds <- M_all_supp %>% 
  select(AU, TI, SO, PY, ID) %>%
  filter(SO %in% pjournals) %>%
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

mobouts <- ggplot(yr_term_counts, aes(year, sum_out / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% outcome-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica")

ggsave("figures/mobout.png", width=400, height = 300, dpi=300, units="mm")

# International soc.

pwords <-
  c(
    "protest",
    "protestation",
    "protesta",
    "protesta",
    "protest",
    "protests",
    "protestations",
    "proteste",
    "protestas",
    "proteste",
    "protestor",
    "protestataire",
    "manifestante",
    "manifestante",
    "demonstrantin",
    "protestors",
    "protestataires",
    "manifestanti",
    "manifestantes",
    "demonstrantinnen",
    "social movement",
    "mouvement social",
    "movimento sociale",
    "movimiento social",
    "soziale bewegung",
    "social movements",
    "mouvements sociaux",
    "movimenti sociali",
    "movimientos sociales",
    "soziale bewegungen",
    "contentious politics",
    "política contenciosa"
  )
pwords <- paste0("\\b", pwords, "\\b",  collapse = "|")


M <- M_all_supp %>%
  filter(!is.na(AB),!is.na(PY), 
         SO %in% npjournals)

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

g3all <- yr_term_counts %>%
  filter(year <= 2020) %>%
  ggplot(aes(year, (sum_p / year_total) * 100)) +
  geom_point(alpha = 0.8) +
  geom_smooth(
    method = "lm",
    linetype = "dashed",
    color = "red",
    alpha = .15
  ) +
  ylim(0, .25) +
  xlab("Year") +
  ylab("% protest words in journal abstracts") +
  ggtitle("") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))



MALL <- M_all_supp %>%
  filter(!is.na(AB),!is.na(PY), 
         SO %in% npjournals)

MALL$pword <- as.integer(grepl(pwords, 
                               x = MALL$AB, ignore.case = T))

MALLsums <- MALL %>%
  dplyr::mutate(article = 1) %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(sum_particles = sum(pword),
                   pct_particles = (sum(pword) / sum(article)) * 100)

g3allsums <- MALLsums %>%
  filter(PY <= 2020) %>%
  ggplot() +
  geom_bar(aes(PY, pct_particles), stat = "identity") +
  xlab("Year") +
  ylab("% protest articles") +
  ggtitle("") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))


g <- plot_grid(g3all,
               g3allsums,
               nrow = 2, labels = "AUTO")
g <-
  add_sub(
    g,
    "International sociology journals",
    x = 0.5,
    hjust = 0.5,
    size = 20,
    fontface = "bold"
  )

png(
  "figures/isjplot.png",
  width = 400,
  height = 300,
  units = 'mm',
  res = 300
)
plot_grid(g, nrow = 1, labels = "")
dev.off()