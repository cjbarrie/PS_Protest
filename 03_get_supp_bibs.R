library(bibliometrix)
library(tidyverse)
library(plyr)

file.ls <- list.files(path = "data/raw_supp/")
file.ls <- paste0("data/raw_supp/", file.ls)

M_all_supp <- data.frame()
for (i in seq_along(file.ls)) {
  D <- file.ls[[i]]
  M <- convert2df(D, dbsource = "isi", format = "plaintext")
  M_all_supp <- rbind.fill(M_all_supp, M)
}

save(M_all_supp, file = "data/analysis/bib_records_supp.RData")
