library(tidyverse)
library("readxl")
library(latex2exp)
library(ggrepel)

theme.info <- theme(plot.title = element_text(size=16, hjust=0.5),
                    axis.title = element_text(size=14),
                    axis.text = element_text(size=14))

data <- read_excel("C:\\Users\\chris\\Desktop\\NBA-Data-and-Models\\Visualizations\\Points vs TS Scatterplots\\12-11-2019\\sportsref_download.xlsx")

data$PPG <- round(data$PTS/data$G, digits = 3)

data$`TS%` <- data$PTS/(2 * (data$FGA + 0.44*data$FTA))*100

sorted_data <- data[order(-data$PPG),]

sorted_data <- sorted_data[1:50,]

std_dev <- sd(data$`TS%`, na.rm=TRUE)

league_avg_ts <- 55.9

ggplot(sorted_data, aes(x=PPG, y=`TS%`)) + geom_point() + 
  geom_hline(yintercept=league_avg_ts, linetype="dashed", size = 1, color="red") +
  geom_hline(yintercept=c(league_avg_ts + std_dev, league_avg_ts - std_dev), linetype="dashed", size = 1, color="green") +
  geom_label_repel(aes(label = Player),
                   box.padding   = 0.4, 
                   point.padding = 0.4,
                   segment.color = 'grey50') +
  scale_x_continuous(breaks = seq(round(min(sorted_data$PPG), 0), round(max(sorted_data$PPG), 0), by = 2.5)) +
  scale_y_continuous(breaks = seq(round(min(sorted_data$`TS%`)-5, 0), round(max(sorted_data$`TS%`+5), 0), by = 2.5)) +
  ggtitle("PPG vs. TS% for the Top 50 Scorers") +
  annotate(geom="text", label=TeX("League Average TS% + $\\sigma$"), x=36, y=league_avg_ts + std_dev+0.1, vjust=-1)+
  annotate(geom="text", label="League Average TS%", x=36, y=league_avg_ts+0.1, vjust=-1)+
  annotate(geom="text", label=TeX("League Average TS% - $\\sigma$"), x=36, y=league_avg_ts - std_dev+0.1, vjust=-1)+
  theme.info 
