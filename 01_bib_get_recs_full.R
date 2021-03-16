library(bibliometrix)
library(tidyverse)
library(plyr)

file.ls <- list.files(path="data/raw/")
file.ls <- paste0("data/raw/", file.ls)

M_all <- data.frame()
for (i in seq_along(file.ls)) {
  D <- file.ls[[i]]
  M <- convert2df(D, dbsource = "isi", format = "plaintext")
  M_all <- rbind.fill(M_all, M)
}

pwords <- c("protest", "protests", "protestor", "protestors", "social movement", "social movements", "contentious politics")
pwords <- paste0("\\b",pwords, "\\b")

M_all_p <- M_all %>%
  mutate(abstract = tolower(AB)) %>%
  filter(str_detect(abstract, paste(pwords, collapse = "|")))

save(M_all_p, file = "data/analysis/bib_precords.RData")
