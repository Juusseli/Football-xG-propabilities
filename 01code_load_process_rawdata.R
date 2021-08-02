## Loading R packages and source the "getshots" customized own function
source("getshots.R")
library(jsonlite)
library(tidyverse)
library(ggsoccer)
library(ggplot2)

## Getting processed shots data from JSON files of 5 leagues and join them (previous downloading datasets is required)
shotsEN <- get_shots("events/events_European_Championship.json", "EN")
shotsFR <- get_shots("events/events_France.json", "FR")
shotsGE <- get_shots("events/events_Germany.json", "GE")
shotsIT <- get_shots("events/events_Italy.json", "IT")
shotsSP <- get_shots("events/events_Spain.json", "SP")

shots <- tilastot %>% mutate(is_goal = ifelse(tilastot$laukaisu == 'Goal', 1, 0))


tilastot<-veikkausss



# Viz 1: all unblocked shots
ggplot(shots_2007_2019, aes(x = xCordAdjusted, y = yCordAdjusted)) +
        annotate_pitch(colour = "white",
                       fill   = "black",
                       limits = FALSE) +
        theme_pitch() +
        theme(plot.background = element_rect(fill = "black"),
              title = element_text(colour = "white")) +
        coord_flip(xlim = c(0, 51),
                   ylim = c(0, 101)) +
        geom_jitter(aes(fill = factor(goal, levels = c("1", "0"))),
                    alpha = 0.3, shape = 21, size = 0.8) +
        facet_wrap(~goal, nrow = 1) +
        scale_fill_manual(values = c("red", "#00BFFF")) +
        scale_colour_manual(values = c("red", "#00BFFF")) +
        theme(legend.position = c(0.75, 1.12), legend.direction = "horizontal",
              legend.text = element_text(color = "white", size = 8, face = "plain"),
              legend.background = element_rect(fill = "black"),
              legend.key = element_rect(fill = "black"),
              strip.background=element_rect(fill = "black"),
              strip.text = element_text(colour = "black"),
              plot.title = element_text(hjust = 0.07, face = "plain"),
              plot.subtitle = element_text(hjust = 0.07, size = 10, face = "italic"),
              plot.caption = element_text(hjust = 0.95),
              plot.margin = margin(1, 0.2, 0.5, 0.2, "cm")) +
        labs(fill = "Goal?", caption = "@Juuso Heiskanen") +
        guides(fill = guide_legend(override.aes = list(alpha = 0.8, size = 2), reverse=T)) +
        ggtitle("Unblocked open play shots", "Veikkausliiga 2017-2020")

ggsave("shots_plot.png", width = 10, height = 5)


ggplot(data = shots_2007_2019, aes(x = xCordAdjusted, y = yCordAdjusted))+geom_jitter(aes(fill = factor(goal, levels = c("1", "0"))),
                                                                            alpha = 0.3, shape = 21, size = 0.8)

shots<- transform(shots, x = as.numeric(as.character(x)))
shots<- transform(shots, y = as.numeric(as.character(y)))

shots$y <- shots$y * 0.4

shots$pvm <- format(as.Date(shots$pvm, format = "%d/%m/%Y"), "%Y-%m-%d")


#shots <- shots[-c(3337, 3339, 5975, 14701, 14432, 9354, 5137),]





shots <- data.frame(shots,do.call(rbind,strsplit(shots$aika,":")))

shots<- transform(shots, X1 = as.numeric(as.character(X1)))

shots<- transform(shots, X2 = as.numeric(as.character(X2)))

shots$aika <- (shots$X1*60)+shots$X2

shots <- shots %>% arrange(pvm, gameid, paika, aika)

write_rds(shots, "all_unblocked_shots.rds")

#shots <- filter(shots, laukaisu != "Shot.blocked")
#shots <- filter(shots, laukaisu != "Own.goal")

