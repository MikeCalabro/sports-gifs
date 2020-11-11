# UFC FIGHT HISTORY GIF MAKER

fighter_name <- "Jon Jones"

library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(lubridate)
library(glue)

# Reads in the UFC Data, downloaded from Kaggle
ufc <- read_csv("data/ufc-new.csv") %>%
  clean_names()

ufc_full <- ufc %>%
  mutate(winner_name = ifelse(winner == "Red", r_fighter,
                              ifelse(winner == "Blue", b_fighter, "Draw"))) %>%
  mutate(fight_length = round(total_fight_time_secs / 300, 2)) %>%
  mutate(year = strtoi(sprintf("20%s", substr(date, nchar(date)-1, nchar(date)))))

fighter_data <- function(name) {
  data <- ufc_full %>%
    filter(r_fighter == name | b_fighter == name) %>%
    select(r_fighter, b_fighter, date, year, winner, winner_name, fight_length, finish, finish_details) %>%
    mutate(date = mdy(date)) %>%
    mutate(finish = case_when(
      finish == "KO/TKO" ~ "KO",
      finish == "M-DEC" ~ "DEC",
      finish == "U-DEC" ~ "DEC",
      finish == "S-DEC" ~ "DEC",
      finish == "SUB" ~ "SUB"
    )) %>%
    mutate(opponent = ifelse(r_fighter == name, b_fighter, r_fighter)) %>%
    arrange(desc(date))
  
  return(data)
} 

data <- fighter_data(fighter_name)


fighter_plot <- function(name) {
  data <- fighter_data(name) 
  fig_size <- ceiling(60/nrow(data)) + 1
  text_nudge <- (45 - nrow(data))/100
  plot <- data %>%
    ggplot(aes(x = fight_length, y = factor(date), color = ifelse(winner_name == name, "Win", "Loss"))) +
    geom_segment(aes(x = 0.03, y = factor(date), xend = fight_length + 0.06, yend = factor(date)), size = fig_size + 3, color = "#ECECEC") +
    geom_segment(aes(x = 0.03, y = factor(date), xend = fight_length + 0.03, yend = factor(date)), size = fig_size + 1, color = "gray") +
    geom_segment(aes(x = 0, y = factor(date), xend = fight_length, yend = factor(date)), size = fig_size) +
    geom_text(aes(label = finish, fontface = "bold"), size = fig_size, nudge_x = text_nudge + 0.02, color = "#DCDCDC") +
    geom_text(aes(label = finish, fontface = "bold"), size = fig_size, nudge_x = text_nudge) +
    labs(color = "",
         title = glue::glue("{name} UFC Career Fight Log"),
         subtitle = "Fight Date: {next_state}",
         caption = "DATA: Kaggle - Ultimate UFC Dataset") +
    scale_x_continuous("Rounds Lasted", limits = c(-3.5, 5.5), breaks = (0:5)) +
    scale_y_discrete(labels =  rev(data$opponent)) +
    theme_classic() +
    theme(axis.text = element_text(size = 9),
          axis.title.y = element_blank(),
          legend.position = c(0.9, 0.12)) +
    coord_cartesian(xlim = c(0, 5.5)) +
    scale_color_manual(values = c("green3")) 
  
  return(plot)
}

plot <- fighter_plot(fighter_name)

plot

fights_gif <- plot +
  transition_states(date, transition_length = 2, state_length = 0.4, wrap = FALSE) +
  shadow_mark() +
  enter_recolor(color = "black") +
  enter_fly(x_loc = -3.5)  +
  enter_fade(alpha = 0.6)

animate(fights_gif, nframes = 400, width = 850, height = 650, end_pause = 50)

