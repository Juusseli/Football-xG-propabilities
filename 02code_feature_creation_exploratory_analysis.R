## Loading R packages
library(tidyverse)
library(DataExplorer)


## Loading RDS file with unblocked shots (already processed data)
shots <- readRDS("all_unblocked_shots.rds")

## Feature edition/creation
shotsB <- shots %>%
  mutate(eventSec2 = ifelse(paika == "2", aika + 2700, aika),
         time_prev = ifelse(gameid == lag(gameid) & paika == lag(paika) & home.away == lag(home.away), aika - lag(aika), -1),
         time_prev = ifelse(is.na(time_prev), -1, time_prev),
         x_meter =105 - y * 105/100,
         y_meter = x * 68/100,
         distance_to_goal_line = sqrt((105 - x_meter)^2 + (32.5 - y_meter)^2),
         angle_to_goal = atan( (7.32 * (105 - x_meter) ) / ( (105 - x_meter)^2 + (32.5 - y_meter)^2 - (7.32/2)^2 )) * 180/pi)

data_to_mod <- shotsB %>%
        dplyr::select(is_goal, aika, paika, y, x, 
                      time_prev, distance_to_goal_line, angle_to_goal) %>%
        mutate(is_goal = factor(is_goal),
               paika = factor(paika))

write_rds(data_to_mod, "data_to_mod.rds")


## Exploratory analysis
plot_correlation(data_to_mod)
ggsave("corr_plot.png", width = 10, height = 5)

plot_bar(data_to_mod, ncol = 2)
ggsave("bars_plot.png", width = 10, height = 5)

plot_density(data_to_mod, ncol = 3)
ggsave("dens_plot.png", width = 10, height = 5)

