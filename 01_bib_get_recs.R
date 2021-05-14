library(bibliometrix)
library(tidyverse)
library(plyr)

# NB: at the moment the bib_records data *includes* EJPR and *excludes* IO; whereas the bib_precords data
# for the network and conceptual structure map analyses *does not include* EJPR and *includes* IO. This was
# done for purpose of seeing if EJPR interested before redoing network analyses as well as simple counts of
# articles. So before final submission, the data needs to be harmonized in terms of which journals are being
# included for each part of the analysis.

file.ls <- list.files(path = "data/raw/")
file.ls <- paste0("data/raw/", file.ls)

M_all <- data.frame()
for (i in seq_along(file.ls)) {
  D <- file.ls[[i]]
  M <- convert2df(D, dbsource = "isi", format = "plaintext")
  M_all <- rbind.fill(M_all, M)
}

M_all <- M_all %>%
  distinct(UT, .keep_all = T)

save(M_all, file = "data/analysis/bib_records.RData")

# get protest articles

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

M_all_p <- M_all %>%
  mutate(abstract = tolower(AB),
         particle = as.integer(grepl(pwords, 
                                     x = abstract, ignore.case = T))) %>%
  filter(particle == 1)

save(M_all_p, file = "data/analysis/bib_precords.RData")
write_csv(M_all_p, "data/output/particles.csv")
