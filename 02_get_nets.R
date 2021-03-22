library(tidyverse)
library(ggrepel)
library(bibliometrix)
library(tidytext)
library(tidyr)
library(stringr)
library(ggthemes)
library(igraph)
library(gridExtra)
library(ggforce)
library(cowplot)
library(data.table)
library(xtable)

################
load("data/analysis/bib_precords.RData")

M <- M_all_p %>%
  filter(!is.na(AB) & !is.na(AB) & PY<=2020)

# M <- M %>%
#   filter(!is.na(AB) & PY!="2020")

NetMatrix <-
  biblioNetwork(M,
                analysis = "co-citation",
                network = "references",
                sep = ";")
net <-
  networkPlot(
    NetMatrix,
    normalize = NULL,
    weighted = NULL,
    n = 160,
    Title = "Co-citation",
    type = "fruchterman",
    size = 5,
    size.cex = T,
    remove.multiple = TRUE,
    labelsize = 0.8,
    label.n = 30,
    label.cex = F,
    cluster = "walktrap"
  )
unique(net$cluster_res$cluster)
unique(net$cluster_res$vertex)
class(net$cluster_obj) #igraph object
#this gives the list of vertices
g1 <- net$graph
g1
#get graphml file for Gephi
write.graph(g1, "data/output/prot.graphml", format = "graphml")

#another look at keyword frequency
#here i take all keywords in polisci then soc., I filter by lowest frequency of outcome word (3 inPS; 7 in soc.),
#then denominate by total remaining keywords in dataframe to get meaasure of frequency that can compare between soc. and PS.

journals <- unique(M$SO)
psjournals <- journals[c(1, 3,5,7,9,10,14)]
socjournals <- setdiff(journals, psjournals)

MpsALL <- M %>%
  filter(SO %in% psjournals)
MsocALL <- M %>%
  filter(SO %in% socjournals)

results <- biblioAnalysis(MpsALL, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
pskeyw <- as.data.frame(results$ID)
colnames(pskeyw) <- c("word", "freqps")
pskeyw$freqps <- pskeyw$freqps / nrow(pskeyw)

results <- biblioAnalysis(MsocALL, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
sockeyw <- as.data.frame(results$ID)
colnames(sockeyw) <- c("word", "freqsoc")
sockeyw$freqsoc <- sockeyw$freqsoc / nrow(sockeyw)

allkeyw <- merge(pskeyw, sockeyw, by = "word")
allkeyw <-
  allkeyw %>% remove_rownames %>% column_to_rownames(var = "word")
allkeyws <- subset(allkeyw, freqps > .006)
allkeyws$Category <- "Non-outcomes"
allkeyws$Category[rownames(allkeyws) %in% c("ATTITUDES",
                                            "CONSEQUENCES",
                                            "OUTCOMES",
                                            "PUBLIC-OPINION",
                                            "IMPACT")] <- "Outcomes"
cbbPalette <- c(Outcomes = "red", `Non-outcomes` = "grey80")

alpha <- ifelse(allkeyws$Category == "Outcomes", 0.9, 0.65)

g2 <- ggplot(allkeyws, aes(freqps, freqsoc)) +
  geom_point(aes(color = Category), data = allkeyws[allkeyws$Category == "Outcomes",]) +
  geom_point(aes(color = Category), data = allkeyws[allkeyws$Category == "Non-outcomes",]) +
  scale_colour_manual(values = cbbPalette) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Keyword frequency (political science)") +  ylab("Keyword frequency (sociology)") +
  geom_abline(intercept = 0) +
  geom_label_repel(
    label = rownames(allkeyws),
    color = ifelse(allkeyws$Category == "Outcomes", "red", "grey80"),
    force = 10,
    alpha = alpha
  ) +
  xlim(0, .05) + ylim(0, .05) +
  theme(
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    aspect.ratio = .75,
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
g2
png(
  "figures/keywdsn.png",
  width = 300,
  height = 250,
  units = 'mm',
  res = 300
)
g2
dev.off()

#look at most cited papers in soc and polisci

cmat <-
  cocMatrix(
    MpsALL,
    Field = "CR",
    type = "sparse",
    sep = ";",
    binary = TRUE
  )
topPS <-
  as.data.frame(sort(Matrix::colSums(cmat), decreasing = TRUE)[1:20])
write.csv(topPS, "data/output/topPS.csv", row.names = T)

cmat <-
  cocMatrix(
    MsocALL,
    Field = "CR",
    type = "sparse",
    sep = ";",
    binary = TRUE
  )
topSOC <-
  as.data.frame(sort(Matrix::colSums(cmat), decreasing = TRUE)[1:20])
write.csv(topSOC, "data/output/topSOC.csv", row.names = T)


topPS <- setDT(topPS, keep.rownames = TRUE)[]
colnames(topPS) <- c("Top_PS", "cites_PS")
topSOC <- setDT(topSOC, keep.rownames = TRUE)[]
colnames(topSOC) <- c("Top_SOC", "cites_PS")

topPS$Top_PS <- as.character(topPS$Top_PS)
topSOC$Top_SOC <- as.character(topSOC$Top_SOC)
topb <- cbind(topSOC, topPS)


print(xtable(topb, type = "latex", digits = 0), file = "data/output/topb.tex")
print(xtable(topSOC, type = "latex", digits = 0), file = "data/output/topsoc.tex")
print(xtable(topPS, type = "latex", digits = 0), file = "data/output/topps.tex")