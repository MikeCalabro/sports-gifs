# Week 1 Mlb Code

library(tidyverse)
library(baseballr)
library(gifski)
library(png)
library(gganimate)

playerid_lookup("Sale", "Chris") %>%
  select(mlbam_id)

sale_2018 <- scrape_statcast_savant(start_date = "2018-04-01",
                                    end_date = "2018-10-01",
                                    playerid = 519242,
                                    player_type = "pitcher")

sale_strikeouts_2018 <- sale_2018 %>%
  filter(events == "strikeout") %>%
  select(game_date, player_name, plate_x, plate_z, pitch_name, events)

sale_strikeouts_2018 <- sale_strikeouts_2018 %>%
  arrange(game_date) %>%
  mutate(so_id = seq.int(1, nrow(sale_strikeouts_2018)))

sale_gif_data <- sale_strikeouts_2018 %>%
  mutate(best_id = glue::glue("Date: {game_date}  -  Strikeouts: {so_id}")) 

sale_gif_2018 <-
  # Data I am using
  sale_gif_data %>%
  filter(!pitch_name == "null") %>%
  # Creating a plot
  ggplot() +
  # Data shown on plot
  geom_point(aes(x = plate_x, y = plate_z, color = pitch_name, group = seq_along(best_id)), shape = 21, size = 3.5, stroke = 2) +
  geom_segment(aes(x = plate_x + ((1.1-plate_x)/3),
                   y = plate_z + ((5.5-plate_z)/3),
                   xend = plate_x,
                   yend = plate_z,
                   color = pitch_name,
                   group = seq_along(best_id)), alpha = 0.6, size = 1.5) +
  geom_segment(aes(x = 1.1,
                   y = 5.5,
                   xend = plate_x,
                   yend = plate_z,
                   color = pitch_name,
                   group = seq_along(best_id)), alpha = 0.6, size = 0.3) +
  geom_point(aes(x = 1.1, y = 5.5, color = pitch_name), size = 2) +
  # Drawing the Strikezone
  geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "gray") +
  geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "gray") +
  geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "gray") +
  geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "gray") +
  # Drawing foul lines
  # geom_segment(aes(x = 0, y = -0.4, xend = 3.3, yend = 0.2), color = "gray") +
  # geom_segment(aes(x = 0, y = -0.4, xend = -3.3, yend = 0.2), color = "gray") +
  # Drawing home plate
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), color = "black") +
  geom_segment(aes(x = -1, y = 0, xend = -1.1, yend = -0.2), color = "black") +
  geom_segment(aes(x = 1, y = 0, xend = 1.1, yend = -0.2), color = "black") +
  geom_segment(aes(x = 1.1, y = -0.2, xend = 0, yend = -0.4), color = "black") +
  geom_segment(aes(x = -1.1, y = -0.2, xend = 0, yend = -0.4), color = "black") +
  # Dimensions of the plot
  ylim(-1, 5.6) +
  xlim(-5, 5) +
  # Removing all graph-looking elements from plot
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.85, 0.7)) +
  # Animation Control
  transition_states(best_id,
                    state_length = 0,
                    wrap = FALSE) +
  enter_fly(x_loc = 1.1, y_loc = 5.5) +
  enter_fade(alpha = 0.5) +
  enter_grow() +
  shadow_mark(size = 0.6, alpha = alpha*0.01, past = TRUE) +
  labs(title = "Chris Sale 2018 Season Strikeouts",
       subtitle = "{closest_state}",
       color = "")

animate(sale_gif_2018, fps = 2000, detail = 10, nframes = 520, end_pause = 20)
anim_save("week_1_sale_strikeout.gif")