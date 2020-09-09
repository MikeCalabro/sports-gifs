# Fernando Tatis HRs Week 2 MLB

library(tidyverse)
library(baseballr)
library(gifski)
library(png)
library(gganimate)

playerid_lookup("Tatis", "Fernando") 

tatis_2020 <- scrape_statcast_savant(start_date = "2020-07-01",
                                    end_date = "2020-09-08",
                                    playerid = 665487,
                                    player_type = "batter")

tatis_gif_data <- tatis_2020 %>%
  filter(events == "home_run") %>%
  select(game_date, pitch_name, events, plate_x, plate_z, launch_angle, launch_speed, hit_distance_sc, hc_x, hc_y, des) %>%
  arrange(game_date) %>%
  mutate(birds_angle = ifelse(str_detect(des, "right center field"), runif(15, 50, 80), 
                         ifelse(str_detect(des, "left center field"), runif(15, 100, 115), 
                                ifelse(str_detect(des, "right field"), runif(15, 50, 65), 
                                       ifelse(str_detect(des, "left field"), runif(15, 115, 135), runif(15, 80, 100)))))) %>%
  select(1:2, 4:8, 12) %>%
  mutate(id = glue::glue("Date: {game_date} - Homeruns: {seq.int(1, 15)} - Data Source: Statcast | MLB.com"))

tatis_gif_data

infield_x <- c(0, -107,-80, -60, -40, -20, 0, 20, 40, 60, 80, 107, 0)
infield_y <- c(-7, 100, 120, 135, 145, 152, 155, 152, 145, 135, 120, 100, -7)
infield <- data.frame(infield_x, infield_y)

wall_x <- c(0, 237, 130, 90, 50, 10, 0, -10, -50, -90, -130, -237, 0)
wall_y <- c(-7, 230, 310, 328, 340, 345, 346, 345, 340, 328, 310, 230, -7)
wall <- data.frame(wall_x, wall_y)

tatis_gif <- 
  tatis_gif_data %>%
  ggplot() +
  geom_polygon(data = wall, aes(x = wall_x, y = wall_y), fill = "green4") +
  geom_polygon(data = infield, aes(x = infield_x, y = infield_y), fill = "orange4") +
  geom_segment(aes(x = 0, y = 0, xend = 230, yend = 230), color = "white") +
  geom_segment(aes(x = 0, y = 0, xend = -230, yend = 230), color = "white") +
  geom_segment(aes(x = .97*(hit_distance_sc * cos(birds_angle * pi / 180)),
                   y = .97*(hit_distance_sc * sin(birds_angle * pi / 180)),
                   xend = (hit_distance_sc * cos(birds_angle * pi / 180)),
                   yend = (hit_distance_sc * sin(birds_angle * pi / 180)),
                   color = pitch_name,
                   group = id),
               size = 1) +
  geom_point(aes(x = hit_distance_sc * cos(birds_angle * pi / 180),
                 y = hit_distance_sc * sin(birds_angle * pi / 180),
                 group = id,
                 color = pitch_name),
             shape = 21,
             fill = "white",
             size = 1.9) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray90"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.11, .185),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 1),
        legend.box.background = element_rect(color = "black")) +
  labs(title = "Every Fernando Tatis Homerun From The 2020 MLB Season (So Far!)", color = "", caption = "{next_state}") +
  scale_y_continuous(limits = c(-10, 430)) +
  coord_cartesian(ylim = c(-7, 429)) +
  transition_states(id, wrap = FALSE, state_length = 0) +
  shadow_mark() +
  enter_fly(x_loc = 0, y_loc = 0) +
  enter_grow(size = 3.6) +
  enter_fade(alpha = 0.85)

animate(tatis_gif, nframes = 165, end_pause = 15)
anim_save("week_2_tatis_hr.gif")
