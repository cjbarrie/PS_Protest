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
load("data/analysis/bib_precords_f.RData")

#co-citation of references: Co-citation of two articles occurs when both are cited in a third 
#article. Thus, co-citation is the counterpart of bibliographic coupling (Aria and Cuccurullo 2013).
#Below we get a network plot using the Fruchterman-Reingold layout. It is clustered into three
#communtiies when we specify an n of 150 (the number of vertices we plot). Here, it is
#using the cluster = "walktrap" option from Pascal Pons, Matthieu Latapy: Computing communities 
#in large networks using random walks, http://arxiv.org/abs/physics/0512106. This splits into 
#clusters using node betweeness centrality on the basis of a random walk technique. Basically 
#this takes the top 150 most frequently cited articles, splits these into 3 clusters
#and then sized node proportional to in-degree/degree-centrality. The edges link two bibliographic entries that are
#are both cited in a third article (i.e., one of our ~400 articles). This network therefore
#picks up both network centrality (number of edges flowing into article) and common 
#types of co-citation, which helps visualize clustered communities of co-citations.  In plain
#English, citations are clustered alongside other citations that often appear in the same paper.
#So Tilly will reguilarly appear alongside Tarrow, while Chenoweth will often appear alongside 
#Skocpol

M <- M %>%
  filter(!is.na(AB) & PY!="2020")

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = "; ")
net <- networkPlot(NetMatrix,  normalize = NULL, weighted=NULL, n = 150, Title = "Co-citation", 
                   type = "fruchterman",size=5,size.cex=T,remove.multiple=TRUE,
                   labelsize=0.8,label.n=20,label.cex=F, cluster= "walktrap")
unique(net$cluster_res$cluster)
unique(net$cluster_res$vertex)
class(net$cluster_obj) #igraph object
#this gives the list of vertices 
g1 <- net$graph
g1
#get graphml file for Gephi
write.graph(g1, "data/output/prot.graphml", format="graphml")

#conceptual structure
#note the documents argument is only for the factorial map -- it tells you which documents are contributing to each cluster
#mindegree is for the minimum number of occurrences of a word for it to make it onto the map
#cluster = "auto" finds the number of clusters automatically
#CA versus MCA is correspondences analysis or multiple correspondence analysis
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=8, 
                          clust="3", stemming=FALSE, labelsize=15, documents=10) 
CAg <- CS$graph_terms
#view labellings
CAg$labels

#change fill and shape and color to "Cluster"
CAg$labels$colour <- "Cluster"
CAg$labels$fill <- "Cluster"
CAg$labels$shape <- "Cluster"
CAg$labels$label <- ""

g <- CAg + theme_minimal(base_family = "Helvetica") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.key = element_rect(colour = "black", size = 0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.spacing.x = unit(1, "cm"),
        legend.key.size = unit(.75, "cm"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15)) +
  ggtitle("") + ylab("") + xlab("") +
  guides(fill = guide_legend(override.aes = list(alpha = 0)))


outcomes <- CAg$data
outcomes <- outcomes %>%
  filter(name=="attitudes"|
           name=="public-opinion"|
           name=="consequences"|
           name=="outcomes"|
           name=="impact")

shape <- data.frame(
  x = c(-.78,-.14, .7, .8, -.33),
  y = c(-.28, .69, .59,-.87, -1.54)
)

shape$name <- ""

g1 <- ggplot(outcomes, aes(x,y, label=name)) + theme_minimal(base_family = "Helvetica") + 
  geom_point() +  ggtitle("") + ylab("") + xlab("") + 
  guides(fill = guide_legend(override.aes = list(alpha = 0))) +
  geom_text_repel(data=subset(outcomes, name=="attitudes"|name=="consequences"),
                  color = "red",
                  size=5, nudge_x = .5, nudge_y =.5) +
  geom_text_repel(data=subset(outcomes, name=="outcomes"|name=="public-opinion"),
                  color = "red",
                  size=5, nudge_x = -.4, nudge_y = -.5) +
  geom_text_repel(data=subset(outcomes, name=="impact"),
                color = "red",
                size=5, nudge_x = -.3, nudge_y = .3) +
  geom_polygon(data=shape, fill="red", color="red", alpha=.2) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        aspect.ratio = .75)
g1
#another look at keyword frequency
#here i take all keywords in polisci then soc., I filter by lowest frequency of outcome word (3 inPS; 7 in soc.), 
#then denominate by total remaining keywords in dataframe to get meaasure of frequency that can compare between soc. and PS.

journals <- unique(M$SO)
psjournals <- journals[c(1:7)]
socjournals <- setdiff(journals, psjournals)


MALL<- M

MpsALL <- MALL %>%
  filter(SO %in% psjournals)
MsocALL <- MALL %>%
  filter(SO %in% socjournals)

results <- biblioAnalysis(MpsALL, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
pskeyw <- as.data.frame(results$ID)
colnames(pskeyw) <- c("word", "freqps")
pskeyw$freqps <- pskeyw$freqps/397

results <- biblioAnalysis(MsocALL, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
sockeyw <- as.data.frame(results$ID)
colnames(sockeyw) <- c("word", "freqsoc")
sockeyw$freqsoc <-sockeyw$freqsoc/602

allkeyw <- merge(pskeyw,sockeyw, by="word")
allkeyw <- allkeyw %>% remove_rownames %>% column_to_rownames(var="word")
allkeyws <- subset(allkeyw, freqps>.007)
allkeyws$Category <- "Non-outcomes"
allkeyws$Category[rownames(allkeyws) %in% c("ATTITUDES", "CONSEQUENCES", 
                                            "OUTCOMES", "PUBLIC-OPINION", "IMPACT")] <- "Outcomes"
cbbPalette <- c(Outcomes = "red", `Non-outcomes` = "grey80")

alpha <- ifelse(allkeyws$Category=="Outcomes", 0.9, 0.65)

g2 <- ggplot(allkeyws, aes(freqps, freqsoc)) +
  geom_point(aes(color = Category), data = allkeyws[allkeyws$Category == "Outcomes", ]) +
  geom_point(aes(color = Category), data = allkeyws[allkeyws$Category == "Non-outcomes", ]) +
  scale_colour_manual(values = cbbPalette) +
  theme_tufte(base_family = "Helvetica") + 
  xlab("Keyword frequency (political science)") +  ylab("Keyword frequency (sociology)") +
  geom_abline(intercept = 0) + 
  geom_label_repel(label=rownames(allkeyws),
                   color = ifelse(allkeyws$Category=="Outcomes", "red", "grey80"),
                   force=10,
                   alpha=alpha) +
  xlim(0, .05) + ylim(0, .05) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        aspect.ratio = .75,
        legend.position = "bottom",
        legend.direction = "horizontal")
g2
png("figures/keywds.png", 
    width = 300, height = 250, units='mm', res = 300)
g2
dev.off()

#or

lhs <- g
rhs <- plot_grid(g1, g2, labels = c('B', 'C'), label_size = 20, ncol=2)

plot_grid(lhs, rhs, labels = c('A', ''), label_size = 20, ncol = 1)

png("figures/concmapall.png", 
    width = 500, height = 480, units='mm', res = 300)
plot_grid(lhs, rhs, labels = c('A', ''), label_size = 20, ncol = 1)
dev.off()

#look at most cited papers in soc and polisci

cmat <- cocMatrix(MpsALL, Field = "CR", type = "sparse", sep = ";",binary = TRUE)
topPS <- as.data.frame(sort(Matrix::colSums(cmat), decreasing = TRUE)[1:20])
write.csv(topPS, "output/topPS.csv",row.names = T)

cmat <- cocMatrix(MsocALL, Field = "CR", type = "sparse", sep = ";",binary = TRUE)
topSOC <- as.data.frame(sort(Matrix::colSums(cmat), decreasing = TRUE)[1:20])
write.csv(topSOC, "output/topSOC.csv",row.names = T)


topPS <- setDT(topPS, keep.rownames = TRUE)[]
colnames(topPS) <-c("Top_PS", "cites_PS")
topSOC <- setDT(topSOC, keep.rownames = TRUE)[]
colnames(topSOC) <-c("Top_SOC", "cites_PS")

topPS$Top_PS <- as.character(topPS$Top_PS)
topSOC$Top_SOC <- as.character(topSOC$Top_SOC)
topb <-cbind(topSOC, topPS)


print(xtable(topb, type = "latex",digits=0), file = "output/topb.tex")
print(xtable(topSOC, type = "latex", digits=0), file = "output/topsoc.tex")
print(xtable(topPS, type = "latex", digits=0), file = "output/topps.tex")

