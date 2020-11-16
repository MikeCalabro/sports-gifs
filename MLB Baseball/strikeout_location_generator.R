# Strikeout Location Generator

library(tidyverse)
library(baseballr)
library(glue)

pitcher_name <- "Gerrit Cole"
year <- 2017

player_id_finder <- function(name) {
  player_name <- str_to_title(name)
  player_firstname <- strsplit(player_name, "\\s+")[[1]][1]
  player_lastname <- strsplit(player_name, "\\s+")[[1]][2]
  id <- playerid_lookup(last_name = player_lastname, first_name = player_firstname) %>%
    arrange(desc(mlb_played_first)) %>%
    top_n(1) %>%
    select(mlbam_id)
  return(id)
}

data_finder <- function(name, season) {
data <- scrape_statcast_savant(start_date = glue::glue("{season}-04-01"),
                                    end_date = glue::glue("{season}-10-01"),
                                    playerid = player_id_finder(name),
                                    player_type = "pitcher") %>%
  filter(events == "strikeout") %>%
  select(game_date, player_name, plate_x, plate_z, pitch_name, events)
return(data)
}

# Making the Plot
# Data I am using
data_finder(pitcher_name, year) %>%
  filter(!pitch_name == "null") %>%
  # Creating a plot
  ggplot() +
  # Drawing the Strikezone
  geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "black") +
  geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "black") +
  geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "black") +
  geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "black") +
  # Drawing home plate
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), color = "black") +
  geom_segment(aes(x = -1, y = 0, xend = -1.1, yend = -0.2), color = "black") +
  geom_segment(aes(x = 1, y = 0, xend = 1.1, yend = -0.2), color = "black") +
  geom_segment(aes(x = 1.1, y = -0.2, xend = 0, yend = -0.4), color = "black") +
  geom_segment(aes(x = -1.1, y = -0.2, xend = 0, yend = -0.4), color = "black") +
  # Data shown on plot
  geom_point(aes(x = plate_x, y = plate_z, fill = pitch_name), shape = 21, size = 2.5, stroke = 0.5, color = "black") +
  # Dimensions of the plot
  ylim(-1, 4.5) +
  xlim(-5, 5) +
  labs(title = glue::glue("Every {pitcher_name} Strikeout From {year}")) +
  # Removing all graph-looking elements from plot
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.85, 0.7),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0, face = "bold"))

data <- data_finder(pitcher_name, year)

data %>%
  count(pitch_name) %>%
  arrange(desc(n))
