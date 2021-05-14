library(gtrendsR)
library(ggplot2)
library(ggthemes)
library(plotly)
library(ggrepel)

#View(countries)

psh <- gtrends(c("protest"), 
               time = "2007-01-01 2020-12-31", 
               gprop = "news",
               hl = "en")

pshdf <- as.data.frame(psh$interest_over_time)
pshdf$datenum <- as.numeric(pshdf$date)
pshdf$date <- as.Date(pshdf$date)
pshdf$year <- format(as.Date(pshdf$date, format="%d/%m/%Y"),"%Y")

pshdf$event[pshdf$year==2011&pshdf$hits==27] <- "Arab Spring"
pshdf$event[pshdf$year==2016&pshdf$hits==23] <- "anti-Trump protests"
pshdf$event[pshdf$year==2019&pshdf$hits==23] <- "Hong Kong/Iran/Lebanon protests"
pshdf$event[pshdf$year==2020&pshdf$hits==42] <- "George Floyd protests"
pshdf$event[pshdf$year==2020&pshdf$hits==100] <- "George Floyd protests"


#get rid of 0 rows
pshdf <- pshdf[-c(0:7),]

g <- ggplot(pshdf, aes(date, hits, label=event)) +
  geom_line(size = 0.5) +
  xlab("Date") + ylab("News interest") +
  geom_point(color=ifelse(pshdf$hits>=23, "red", "NA")) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_tufte(base_family = "Helvetica") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15)) +
  geom_text_repel(data=subset(pshdf, event=="Arab Spring"),
                  color = "red",
                  size=5, hjust=-.5, nudge_y =-5) +
  geom_text_repel(data=subset(pshdf, event=="anti-Trump protests"),
                  color = "red",
                  size=5, hjust=1, nudge_y =6) +
  geom_text_repel(data=subset(pshdf, event=="Hong Kong/Iran/Lebanon protests"),
                  color = "red",
                  size=5, hjust=-4, nudge_y =5) + 
  geom_text_repel(data=subset(pshdf, event=="George Floyd protests"),
                color = "red",
                size=5, hjust=-4, nudge_y =5)


png("figures/gtrendsprot.png", 
    width = 400, height = 300, units='mm', res = 300)
ggplot(pshdf, aes(date, hits, label=event)) +
  geom_line(size = 0.5) +
  xlab("Date") + ylab("News interest") +
  geom_point(color=ifelse(pshdf$hits>=23, "red", "NA")) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_tufte(base_family = "Helvetica") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15)) +
  geom_text_repel(data=subset(pshdf, event=="Arab Spring"),
                  color = "red",
                  size=5, hjust=-.5, nudge_y =-5) +
  geom_text_repel(data=subset(pshdf, event=="anti-Trump protests"),
                  color = "red",
                  size=5, hjust=1, nudge_y =6) +
  geom_text_repel(data=subset(pshdf, event=="Hong Kong/Iran/Lebanon protests"),
                  color = "red",
                  size=5, hjust=-4, nudge_y =5) + 
  geom_text_repel(data=subset(pshdf, event=="George Floyd protests"),
                  color = "red",
                  size=5, hjust=-4, nudge_y =5)

dev.off()

##########################################

#make news fig.

library(pdftools)
library(magick)
library(gridExtra)
library(grid)
library(cowplot)

time <- image_read("figures/time_poy.jpg")
time <- image_convert(time)
grob0 <- rasterGrob(time)

guard <- image_read("figures/guardian_dop.jpg")
guard <- image_convert(guard)
grob1 <- rasterGrob(guard)

tw <- image_read("figures/tw_aop.jpg")
tw <- image_convert(tw)
grob2 <- rasterGrob(tw)

ng <- image_read("figures/floyd.png")
ng <- image_convert(ng)
grob3 <- rasterGrob(ng)

g1 <- grid.arrange(grob0, grob1, grob2, grob3, ncol=4)

plot_grid(g1, g, labels = "AUTO", nrow = 2)

png("figures/gtrendsprot.png", 
    width = 400, height = 300, units='mm', res = 300)
plot_grid(g1, g, labels = "AUTO", nrow = 2, label_size = 20)
dev.off()
