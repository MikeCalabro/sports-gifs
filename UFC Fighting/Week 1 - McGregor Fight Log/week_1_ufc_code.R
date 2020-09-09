# Week 1 UFC Code

library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(lubridate)

# Reads in the UFC Data, downloaded from Kaggle
ufc <- read_csv("data/ufc-master.csv") %>%
  clean_names()

ufc_full <- ufc %>%
  mutate(winner_name = ifelse(winner == "Red", r_fighter,
                              ifelse(winner == "Blue", b_fighter, "Draw"))) %>%
  mutate(fight_length = round(total_fight_time_secs / 300, 2)) %>%
  mutate(year = strtoi(sprintf("20%s", substr(date, nchar(date)-1, nchar(date)))))

conor_data <- ufc_full %>%
  filter(r_fighter == "Conor McGregor" | b_fighter == "Conor McGregor") %>%
  select(r_fighter, b_fighter, date, year, winner, winner_name, fight_length, finish, finish_details) %>%
  mutate(date = mdy(date)) %>%
  mutate(finish = case_when(
    finish == "KO/TKO" ~ "KO",
    finish == "M-DEC" ~ "DEC",
    finish == "U-DEC" ~ "DEC",
    finish == "S-DEC" ~ "DEC",
    finish == "SUB" ~ "SUB"
  )) %>%
  mutate(opponent = ifelse(r_fighter == "Conor McGregor", b_fighter, r_fighter))

conor_gif <- 
  conor_data %>%
  ggplot(aes(x = fight_length, y = factor(date), color = ifelse(winner_name == "Conor McGregor", "Win", "Loss"))) +
  geom_text(aes(label = finish), size = 7, nudge_x = 0.33) +
  geom_segment(aes(x = 0, y = factor(date), xend = fight_length, yend = factor(date)), size = 7) +
  labs(color = "",
       title = "Conor McGregor UFC Career Fight Log",
       subtitle = "Fight Date: {next_state}") +
  scale_x_continuous("Rounds Lasted", limits = c(-3.5, 5.5), breaks = (0:5)) +
  scale_y_discrete(labels =  rev(conor_data$opponent)) +
  theme_classic() +
  theme(axis.text = element_text(size = 9),
        axis.title.y = element_blank(),
        legend.position = c(0.9, 0.42)) +
  coord_cartesian(xlim = c(0, 5.5)) +
  transition_states(date, transition_length = 2, state_length = 0.4, wrap = FALSE) +
  shadow_mark() +
  enter_recolor(color = "black") +
  enter_fly(x_loc = -3.5)  +
  enter_fade(alpha = 0.6)

animate(conor_gif, nframes = 220, width = 600, height = 500, end_pause = 20)
anim_save("week_1_mcgregor_fights.gif")
