# Points per drive

library(tidyverse)
library(nflfastR)
library(gifski)
library(transformr)
library(gganimate)
library(png)
library(DBI)

# Choosing the seasons I wish to add to my data table
seasons <- 2015:2019

# Creating the data table "nfl"
# Combining tables from each season from guga31bb's Github
nfl <- map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})


nfl_drives_2015 <- nfl %>%
  select(season,
         posteam,
         drive,
         drive_start_yard_line,
         fixed_drive_result) %>%
  filter(!is.na(posteam)) %>%
  filter(!is.na(fixed_drive_result)) %>%
  mutate(side_of_field = str_trim(str_sub(drive_start_yard_line, 1, 3))) %>%
  mutate(yard_line = as.integer(str_sub(drive_start_yard_line, -2, -1))) %>%
  mutate(yard_line_100 = ifelse(side_of_field == posteam, 100 - yard_line, yard_line)) %>%
  distinct()


drives_plot_data <- nfl_drives_2015 %>%
  group_by(season, yard_line_100, fixed_drive_result) %>%
  summarise(count = n()) %>%
  group_by(season, yard_line_100) %>%
  mutate(total = sum(count)) %>%
  mutate(share = count / total)


gif_data <- drives_plot_data %>%
  mutate(points_per_drive = ifelse(fixed_drive_result == "Touchdown", count*7/total, 
                                   ifelse(fixed_drive_result == "Field goal", count*3/total, 0))) %>%
  group_by(season, yard_line_100) %>%
  summarise(total = total, ppd = sum(points_per_drive)) %>%
  distinct() %>%
  filter(yard_line_100 > 49) 


gif_data %>%
  ggplot() +
  geom_point(aes(x = yard_line_100, y = ppd, size = total, color = factor(season))) +
  geom_text(aes(label = season, x = 75, y = 3.25, color = factor(season)), size = 9) +
  geom_smooth(aes(x = yard_line_100, y = ppd, size = total, color = factor(season)),
              method = "lm", se = FALSE) +
  theme(legend.position = "none") +
  transition_states(season, 
                    transition_length = 1,
                    state_length = 4.5) +
  labs(x = "Yard Line Where Drive Began",
       y = "Points Per Drive",
       title = "Points Per Drive by Drive Starting Position 2015-2019") +
  exit_fade() +
  enter_fade()

anim_save("week_2_points_per_drive.gif")
