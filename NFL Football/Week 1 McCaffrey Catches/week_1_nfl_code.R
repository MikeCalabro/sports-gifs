# week 1 nfl code

library(tidyverse)
library(nflfastR)
library(gifski)
library(gganimate)
library(png)

nfl <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds"))

gif_data <- nfl %>%
  filter(receiver == "C.McCaffrey") %>%
  filter(play_type == "pass") %>% 
  filter(!yards_gained == 0) %>%
  select(game_date, week, defteam, play_type, air_yards, yards_after_catch, pass_location, yards_gained) %>%
  arrange(game_date) %>%
  mutate(xloc = ifelse(pass_location == "left", runif(115, -22, -8),
                       ifelse(pass_location == "right", runif(115, 8, 22), runif(115, -8, 8)))) %>%
  pivot_longer(cols = c(air_yards,yards_after_catch), names_to = "yard_type", values_to = "yards") %>%
  mutate(x = ifelse(yard_type == "air_yards", 0, xloc)) %>%
  mutate(y = ifelse(yard_type == "air_yards", 0, lag(yards) + 7)) %>%
  mutate(xend = ifelse(yard_type == "air_yards", xloc, xloc + runif(115, -5, 5))) %>%
  mutate(yend = ifelse(yard_type == "air_yards", yards, yards_gained)) %>%
  mutate(play_id = seq.int(1, 230)) %>%
  mutate(catch_number = ceiling(play_id/2)) %>%
  mutate(play_id = ifelse(yard_type == "air_yards",
                          glue::glue("Date: {game_date}  -  Week {week} vs {defteam}  -  Catches: {catch_number}"),
                          glue::glue("Date: {game_date}  -  Week {week} vs {defteam}  -  Catches: {catch_number}")) )


cmac_gif <- gif_data %>%
  ggplot() +
  geom_vline(xintercept = c(-6, 6), color = "white", size = 1.4, linetype = "dotted") +
  geom_text(aes(x = -15, y = 20, label = "20", angle = -90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = 15, y = 20, label = "20", angle = 90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = -15, y = 30, label = "30", angle = -90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = 15, y = 30, label = "30", angle = 90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = -15, y = 40, label = "40", angle = -90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = 15, y = 40, label = "40", angle = 90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = -15, y = 48, label = "0", angle = -90), color = "white", size = 14, family = "serif") +
  geom_text(aes(x = 15, y = 48, label = "5", angle = 90), color = "white", size = 14, family = "serif") +
  geom_segment(x = -27, y = 20, xend = 27, yend = 20, color = "blue", size = .8, alpha = 0.2) +
  geom_segment(x = -27, y = 30, xend = 27, yend = 30, color = "yellow", size = .8) +
  geom_segment(x = -27, y = 40, xend = 27, yend = 40, color = "white", size = .8) +
  geom_segment(x = -27, y = 15, xend = 27, yend = 15, color = "white", size = .1) +
  geom_segment(x = -27, y = 25, xend = 27, yend = 25, color = "white", size = .1) +
  geom_segment(x = -27, y = 35, xend = 27, yend = 35, color = "white", size = .1) +
  geom_segment(x = -27, y = 45, xend = 27, yend = 45, color = "white", size = .1) +
  geom_vline(xintercept = c(-28, 28), color = "white", size = 7) +
  geom_segment(aes(x = x, y = y + 13, xend = xend, yend = yend + 20,
                   linetype = ifelse(yard_type == "air_yards", "solid", "dotted"), 
                   group = catch_number),
               color = "turquoise1", size = 2.5, alpha = 1) +
  coord_cartesian(ylim = c(13, 47)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "green4")) +
  transition_states(play_id, state_length = 0) +
  shadow_mark(alpha = alpha*0.13) +
  labs(title = "Every Christian McCaffrey Reception From The 2019-2020 Season",
       caption = "{previous_state}")

animate(cmac_gif, nframes = 230)
anim_save("week_1_mccaffrey_catches.gif")

