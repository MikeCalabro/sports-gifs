# Week 1 - Tour Money Winners

library(tidyverse)
library(gganimate)
library(png)
library(gifski)
library(janitor)
library(scales)

pga <- read_csv("pgaTourData.csv") %>%
  clean_names()

players <- deframe(pga %>%
                     count(player_name,
                           wt = money,
                           sort = TRUE) %>%
                     head(20) %>%
                     select(player_name))

pga_spread <- pga %>%
  filter(player_name %in% players) %>%
  select(player_name, year, money) %>%
  spread(year, money)

pga_spread[is.na(pga_spread)] = 0

y_2019 <- c(5534619, 2637480, 4358849, 7785286, 2124192, 1558014, 6294690, 3945810, 2440221, 4084541, 4690572, 3122936, 500000, 2669938, 5013084, 730806, 1397370, 500000, 3593844, 3199615)

gif_data <- pga_spread %>%
  mutate(`2011` = `2010` + `2011`) %>%
  mutate(`2012` = `2011` + `2012`) %>%
  mutate(`2013` = `2012` + `2013`) %>%
  mutate(`2014` = `2013` + `2014`) %>%
  mutate(`2015` = `2014` + `2015`) %>%
  mutate(`2016` = `2015` + `2016`) %>%
  mutate(`2017` = `2016` + `2017`) %>%
  mutate(`2018` = `2017` + `2018`) %>%
  arrange(desc(`2018`)) %>%
  cbind(y_2019) %>%
  mutate(y_2019 = `2018` + y_2019) %>%
  pivot_longer((`2010`:y_2019), names_to = "year", values_to = "purse") %>%
  group_by(year) %>%
  mutate(rank = as.numeric(min_rank(-purse))) %>%
  ungroup() %>%
  mutate(rank = ifelse(player_name == "Justin Thomas" & rank == 18, 20, rank)) %>%
  mutate(rank = -1*rank + 21) %>%
  mutate(year = ifelse(year == "y_2019", "2019", year))

gif_data[181:183,4] = 2


winnings_gif <- ggplot(data = gif_data) + # this is the data
  geom_tile(
    mapping = aes( # these aesthetics change with the data
      x = rank,
      y = purse / 2,
      height = purse,
      fill = as.factor(player_name) # group is not needed
      # colour = as.factor(country)
    ),
    width = 0.9, # these aesthetics are constant
    alpha = 1 # hide bars for rank > 10
  ) +
  
  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
  # leads to weird artifacts in text spacing.
  geom_text(
    mapping = aes(
      x = rank,
      y = 0,
      label = player_name
    ),
    vjust = 0.2,
    hjust = 1
  ) +
  
  coord_flip(
    clip = "off",
    expand = FALSE,
    xlim = c(0.45, 20.55) # show rank 1 to 10
  ) +
  scale_y_continuous(labels = dollar_format(), limits = c(0, 60000000)) +
  scale_fill_discrete() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title = "PGA Golfer's Cumulative Winnings Through {closest_state}",
       x = "",
       y = "Tounament Winnings") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.ticks.y = element_blank(), # These relate to the axes post-flip
    axis.text.y = element_blank(), # These relate to the axes post-flip
    plot.margin = margin(1, 3, 1, 4, "cm")
  ) +
  transition_states(year, state_length = 0.1, transition_length = 2, wrap = FALSE) +
  ease_aes("cubic-in-out")

animate(winnings_gif, width = 700, height = 500, nframes = 150, end_pause = 15)
anim_save("week_1_winnings.gif")  
