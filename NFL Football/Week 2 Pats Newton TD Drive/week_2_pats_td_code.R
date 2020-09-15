# Patriots Drive

library(tidyverse)
library(nflfastR)
library(gifski)
library(transformr)
library(gganimate)
library(png)

nfl <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds"))

pats_td_drive <- nfl %>%
  filter(posteam == "NE") %>%
  filter(drive == 12) %>%
  select(qtr,
         yardline_100, 
         yards_gained,
         play_type,
         desc) %>%
  mutate(play_number = seq.int(1,9)) %>%
  filter(!play_type == "extra_point")

#pats_drive_gif <- 
 pats_td_drive %>%
  ggplot() +
  geom_curve(aes(x = -yardline_100,
                 y = play_number + 0.2,
                 xend = -yardline_100 + yards_gained,
                 yend = play_number + 0.2,
                 group = play_number,
                 color = play_type),
             size = 3,
             arrow = arrow(),
             curvature = 0) +
   geom_text(aes(label = desc,
                x = -yardline_100,
                y = play_number - 0.2,
                group = play_number),
            vjust = 0.6, hjust = 0, size = 3) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.background = element_rect(fill = "green4", color = "green4"),
        panel.background = element_rect(fill = "green4")) +
  xlim(-100, 10) +
  coord_cartesian(xlim = c(-75, 10)) #+
  transition_states(play_number, wrap = FALSE) +
  enter_fly(x_loc = -100) +
  shadow_mark()

animate(pats_drive_gif, width = 1000)
