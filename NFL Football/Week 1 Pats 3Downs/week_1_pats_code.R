# Patriots 2019 3rd Downs

library(tidyverse)
library(nflfastR)
library(gifski)
library(gganimate)
library(png)

nfl <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds"))

gif_data <- nfl %>%
  filter(posteam == "NE") %>%
  filter(down == 3) %>%
  select(play_id, game_date, week, defteam, play_type, air_yards, yards_after_catch, pass_location, ydstogo, yards_gained, run_location, run_gap, rusher) %>%
  mutate(pass_location = ifelse(is.na(pass_location), "run", pass_location)) %>%
  mutate(xend = ifelse(pass_location == "left", runif(261, -25, -8),
                       ifelse(pass_location == "right", runif(261, 8, 25), runif(261, -8, 8)))) %>%
  mutate(result = ifelse(play_type == "pass" & is.na(yards_after_catch), "incomplete", 
                         ifelse(yards_gained < ydstogo, "fail", "success"))) %>%
  mutate(play_type = ifelse(play_type == "pass", "throw", play_type)) %>%
  mutate(id = seq.int(1, 261))
  
pats_gif <- 
  gif_data %>%
  filter(play_type %in% c("throw", "run")) %>%
  ggplot() +
  geom_segment(x = -27, y = 20, xend = 27, yend = 20, color = "blue", size = .8, alpha = 0.2) +
  geom_segment(x = -27, y = 30, xend = 27, yend = 30, color = "white", size = .8) +
  geom_segment(x = -27, y = 40, xend = 27, yend = 40, color = "white", size = .8) +
  geom_segment(x = -27, y = 15, xend = 27, yend = 15, color = "white", size = .1) +
  geom_segment(x = -27, y = 25, xend = 27, yend = 25, color = "white", size = .1) +
  geom_segment(x = -27, y = 35, xend = 27, yend = 35, color = "white", size = .1) +
  geom_segment(x = -27, y = 45, xend = 27, yend = 45, color = "white", size = .1) +
  geom_vline(xintercept = c(-28, 28), color = "white", size = 7) +
  geom_segment(aes(x = -27, y = ydstogo + 20, xend = 27, yend = ydstogo + 20), color = "yellow", alpha = 0.99, size = .85) +
  geom_segment(aes(x = 0, y = 13, xend = xend,
                   yend = ifelse(play_type == "throw" & yards_gained == 0, air_yards + 20, yards_gained + 20),
                   linetype = play_type,
                   color = result,
                   group = id),
               size = 2.8) +
  scale_color_manual(values = c("orangered2", "rosybrown", "blue")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "green4"),
        legend.position = c(0.922, 0.827),
        legend.spacing.y = unit(units = "cm", 0),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(color = "black")) +
    coord_cartesian(ylim = c(12.5, 47)) +
    labs(color = "Result", linetype = "Play Type",
         title = "Every Patriots Offensive 3rd Down from the 2019-2020 Season") +
  transition_states(id) +
  shadow_mark(alpha = alpha * 0, size = size * 0.1)

animate(pats_gif, nframes = 476)
anim_save("week_1_pats_third_downs.gif")

nfl %>%
  filter(down == 3) %>%
  filter(play_type %in% c("run", "pass")) %>%
  count(posteam) %>%
  arrange(desc(n))
