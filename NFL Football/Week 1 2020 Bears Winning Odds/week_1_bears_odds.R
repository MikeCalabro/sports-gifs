# Bears winning odds throughout week 1 win over Lions

library(tidyverse)
library(nflfastR)
library(gifski)
library(transformr)
library(gganimate)
library(png)
library(ggimage)

nfl <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds"))

logos <- teams_colors_logos %>%
  select(team_abbr, team_logo_wikipedia)

det_chi_data <- 
  nfl %>%
  filter(home_team == "DET") %>%
  select(home_team, away_team, home_wp, away_wp, game_seconds_remaining, qtr) %>%
  filter(!is.na(home_wp)) %>%
  distinct() %>%
  mutate(id = seq.int(1,167, by = 1)) %>%
  pivot_longer(cols = (home_wp:away_wp), names_to = "team_abbr", values_to = "wp") %>%
  mutate(team_abbr = ifelse(team_abbr == "home_wp", "DET", "CHI")) %>%
  select(3:7) %>%
  mutate(qtr_x = case_when(
    qtr == 1 ~ -3150,
    qtr == 2 ~ -2250,
    qtr == 3 ~ -1350,
    qtr == 4 ~ -450
  )) %>%
  mutate(img_x = case_when(
    qtr == 1 ~ -3600,
    qtr == 2 ~ -2700,
    qtr == 3 ~ -1800,
    qtr == 4 ~ -900
  )) %>%
  mutate(img_y = case_when(
    qtr == 1 & team_abbr == "DET" ~ 55,
    qtr == 2 & team_abbr == "DET" ~ 63,
    qtr == 3 & team_abbr == "DET" ~ 81,
    qtr == 4 & team_abbr == "DET" ~ 94,
    qtr == 1 & team_abbr == "CHI" ~ 45,
    qtr == 2 & team_abbr == "CHI" ~ 37,
    qtr == 3 & team_abbr == "CHI" ~ 19,
    qtr == 4 & team_abbr == "CHI" ~ 6
  )) %>%
  mutate(qtr = glue::glue("QTR {qtr}")) %>%
  mutate(wp = wp*100) %>%
  left_join(logos) %>%
  mutate(lag_wp = lag(wp, 2)) %>%
  mutate(lag_game_seconds_remaining = lag(game_seconds_remaining, 2)) %>%
  filter(!is.na(lag_wp))

det_chi_data[331,6] = 600
det_chi_data[332,6] = 600

det_chi_data[331,7] = 85
det_chi_data[332,7] = 85

det_chi_data[331,8] = 0
det_chi_data[332,8] = 100



bears_color <- teams_colors_logos %>%
  filter(team_abbr == "CHI") %>%
  select(team_color2) %>%
  deframe()

lions_color <- teams_colors_logos %>%
  filter(team_abbr == "DET") %>%
  select(team_color) %>%
  deframe()

det_chi_gif <- 
det_chi_data %>%
  ggplot() +
  geom_segment(aes(x = -lag_game_seconds_remaining, y = lag_wp,
                   xend =  -game_seconds_remaining, yend =  wp,
                   color = team_abbr),
               size = 2) +
  geom_vline(xintercept = c(-2700, -1800, -900), alpha = 0.4) +
  geom_label(aes(label = qtr, x = qtr_x, y = 108), size = 6.5) +
  geom_image(aes(x = img_x, y = img_y, image = team_logo_wikipedia), size = 0.08) +
  scale_x_continuous(limits = c(-3800, 900), breaks = c(-2700, -1800, -900)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25)) +
  coord_cartesian(xlim = c(-3700, 100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  labs(y = "Win Probability (%)",
       color = "Team",
       title = "Bears vs Lions Live Win Probability -  NFL Week 1 2020",
       subtitle = "Data Source: nflfastR") +
  scale_color_manual(values = c(bears_color, lions_color)) +
  transition_states(id, state_length = 0) +
  shadow_mark(size = size * 0.7)

animate(det_chi_gif, nframes = 200, end_pause = 35)
anim_save("week_1_bears_lions_odds.gif")


