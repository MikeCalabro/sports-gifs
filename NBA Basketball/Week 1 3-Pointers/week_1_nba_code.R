library(ballr)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

seasons <- (1977:2020)

nba <- map_df(seasons, function(x) {
        NBAPerGameStatistics(season = x) %>%
          mutate(season = x)
})

write_rds(nba, path = "data/nba_data")

nba <- read_rds("data/nba_data")

nba <- nba %>%
  mutate(total_3pt_attempts = g*x3pa)

gif_data <- nba %>%
  count(season, wt = total_3pt_attempts) %>%
  mutate(lag_3pa = lag(n)) %>%
  filter(!is.na(lag_3pa))
  
three_pa_gif <-
  gif_data %>%
  ggplot() +
  geom_segment(aes(x = season-1, y = lag_3pa, xend = season, yend = n, color = n), size = 1.5) +
  geom_vline(xintercept = c(1980, 1999, 2012, 2020), linetype = "dotted") +
  geom_label(aes(x = 1980.4, y = 25000, label = "3pt Line Introduced"), size = 3, hjust = 0) +
  geom_label(aes(x = 1998.6, y = 55000, label = "Lockout #1"), size = 3, hjust = 1) +
  geom_label(aes(x = 2011.6, y = 70000, label = "Lockout #2"), size = 3, hjust = 1) +
  geom_label(aes(x = 2019.6, y = 100000, label = "COVID"), size = 3, hjust = 1) +
  scale_color_gradient2(low = "blue", mid = "orange", high = "yellow", midpoint = 53000) +
  labs(y = "Three Point Attempts",
       title = "The Monumental Rise in NBA 3-Point Attempts Since 1980") +
  scale_x_continuous(breaks = c(1980, 1999, 2012, 2020)) +
  scale_y_continuous(breaks = c(25000, 50000, 75000, 100000)) +
  coord_cartesian(ylim = c(0, 105000), xlim = c(1977, 2021), expand = FALSE) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  transition_states(season) +
  shadow_wake(wake_length = 1) 


animate(three_pa_gif, nframes = 70, fps = 20, detail = 1.3)

anim_save("week_1_nba_3pa.gif")
