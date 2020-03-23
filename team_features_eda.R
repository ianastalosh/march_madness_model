# Team play styles

# Cluster teams based on:
## FT's per 100 possessions
## 3PA per 100 possesssions
## (3PM per 100 possessions? maybe it should be possessions?)

# field goals attempted - offensive rebounds + turnovers + (0.4 x free throws attempted) 

# Configs
FREE_THROW_POSSESSION_PARAM = 0.44

advanced_metrics_added = combined_data %>%
                            mutate(possessions = FGA - OR + TO + (FREE_THROW_POSSESSION_PARAM * FTA),
                                   fta_per_100 = 100 * FTA/possessions,
                                   ftm_per_100 = 100 * FTM/possessions,
                                   ft_perc_per_100 = ifelse(fta_per_100 != 0, ftm_per_100/fta_per_100, 0),
                                   `3pm_per_100` = 100 * `FGM3`/possessions,
                                   `3pa_per_100` = 100 * `FGA3`/possessions,
                                   `3pt_perc_per_100` = `3pm_per_100` / `3pa_per_100`,
                                   tov_per_100 = 100 * TO/possessions,
                                   efg_perc = (FGM + 0.5 * FGM3)/FGA,
                                   true_shooting_perc = Score/(2 * (FGA + FREE_THROW_POSSESSION_PARAM * FTA)),
                                   threes_and_frees = ft_perc_per_100 * fta_per_100 + 3 * `3pt_perc_per_100` * `3pa_per_100`)

# Scatterplots
ggplot(advanced_metrics_added, aes(x = fta_per_100, y = `3pt_perc_per_100`)) + 
  geom_point(alpha = 0.05) + 
  labs(title = "FT per 100 Possessions vs 3PA per 100 Possessions",
       x = "FTA per 100 Possessions",
       y = "3PT Percentage per 100 Possessions") + 
  facet_wrap(~winner)


ggplot(advanced_metrics_added, aes(x = ft_perc_per_100, y = `3pt_perc_per_100`)) + 
  geom_point(alpha = 0.02) + 
  labs(title = "FT perc per 100 Possessions vs 3P perc per 100 Possessions",
       x = "FT perc per 100 Possessions",
       y = "3PT perc per 100 Possessions") + 
  facet_wrap(~winner)

ggplot(advanced_metrics_added, aes(x = fta_per_100, y = `3pa_per_100`)) + 
  geom_point(alpha = 0.05) + 
  labs(title = "FT per 100 Possessions vs 3PA per 100 Possessions",
       x = "FTA per 100 Possessions",
       y = "3PA per 100 Possessions") + 
  facet_wrap(~winner)

# Density Plots
ggplot(advanced_metrics_added, aes(x = fta_per_100, group = as.factor(winner))) + 
  geom_density(aes(fill = as.factor(winner)), alpha = 0.95) + 
  labs(title = "FT per 100 Possessions Density",
       x = "FTA per 100 Possessions",
       y = "Density") 

ggplot(advanced_metrics_added, aes(x = ft_perc_per_100, group = as.factor(winner))) + 
  geom_density(aes(fill = as.factor(winner)), alpha = 0.95) + 
  labs(title = "FT perc per 100 Possessions Density",
       x = "FT perc per 100 Possessions",
       y = "Density") 

ggplot(advanced_metrics_added, aes(x = `3pa_per_100`, group = as.factor(winner))) + 
  geom_density(aes(fill = as.factor(winner)), alpha = 0.95) + 
  labs(title = "3PA per 100 Possessions Density",
       x = "3PA per 100 Possessions",
       y = "Density") 

ggplot(advanced_metrics_added, aes(x = `3pt_perc_per_100`, group = as.factor(winner))) + 
  geom_density(aes(fill = as.factor(winner)), alpha = 0.95) + 
  labs(title = "3pt Perc per  Possessions Density",
       x = "3pt perc per 100 Possessions",
       y = "Density") 

ggplot(advanced_metrics_added, aes(x = threes_and_frees, group = as.factor(winner))) + 
  geom_density(aes(fill = as.factor(winner)), alpha = 0.95) + 
  labs(title = "Points from Threes and Frees per 100 Possessions",
       x = "Threes and Frees Points",
       y = "Density") 
