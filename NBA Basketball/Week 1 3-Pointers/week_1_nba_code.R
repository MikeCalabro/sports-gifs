library(ballr)
library(tidyverse)

seasons <- (1980:2020)

nba <- map_df(seasons, function(x) {
        NBAPerGameStatistics(season = x) %>%
          mutate(season = x)
})

write_rds(nba, path = "nba_data")

nba <- nba %>%
  mutate(total_3pt_attempts = g*x3pa)

nba %>%
  count(season, wt = total_3pt_attempts) %>%
  ggplot(aes(x = season, y = n)) +
  geom_bar(stat = "identity") +
  labs(y = "Three Point Attempts")
