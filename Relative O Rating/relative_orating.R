library(tidyverse)
library("readxl")
library(latex2exp)
library(ggrepel)

theme.info <- theme(plot.title = element_text(size=16, hjust=0.5),
                    axis.title = element_text(size=14),
                    axis.text = element_text(size=14))

season_data <- read_excel("C:\\Users\\chris\\Desktop\\NBA-Data-and-Models\\Relative O Rating\\season_stats.xlsx")

team_data <- read_excel("C:\\Users\\chris\\Desktop\\NBA-Data-and-Models\\Relative O Rating\\team_stats.xlsx")

team_data$`reFG%` <- rep(0, times=nrow(team_data))
team_data$rORtg <- rep(0, times=nrow(team_data))

team_data$Tm <- str_remove(team_data$Tm, "[*]")
team_data$Team <- paste(team_data$Tm, team_data$Season)

for(i in 1:nrow(team_data)){
  team_data$rORtg[i] <- team_data$ORtg[i] - season_data$ORtg[season_data$Season == team_data$Season[i]]
  team_data$`reFG%`[i] <- (team_data$`eFG%`[i] - season_data$`eFG%`[season_data$Season == team_data$Season[i]])*100
}

ggplot(team_data, aes(x=rORtg, y=`reFG%`)) + geom_point() + 
  geom_label_repel(data= . %>% 
                  mutate(label = ifelse(rORtg >= 6 & `reFG%` > 2,
                                           Team, "")),
                  aes(label = label), 
                   box.padding   = 0.4, 
                   point.padding = 0.4,
                   segment.color = 'grey50') +
  ggtitle("rORtg vs. reFG% for All Teams in NBA History") +
  theme.info 
