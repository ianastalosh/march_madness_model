# Create rolling values

# Configs
FREE_THROW_POSSESSION_PARAM = 0.44
ROLLING_WINDOW = 5 

# Functions
lagged_rollmean = function(column_name, window) {
  lag(rollmean(column_name, window, align = "right", fill = NA))
}

offensive_stats = combined_data %>%
                    mutate(possessions = FGA - OR + TO + (FREE_THROW_POSSESSION_PARAM * FTA),
                           opp_possessions = opp_FGA - opp_OR + opp_TO + (FREE_THROW_POSSESSION_PARAM * opp_FTA),
                           pts_scored_per_100 = 100 * Score/possessions,
                           pts_allowed_per_100 = 100 * opp_Score/possessions,
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

# Advanced metrics over time
team_averages = advanced_metrics_added %>% 
                  arrange(TeamID, Season, DayNum) %>% 
                  group_by(TeamID) %>%
                  mutate(fta_per_100_avg = lagged_rollmean(fta_per_100, ROLLING_WINDOW), 
                         ftm_per_100_avg = lagged_rollmean(ftm_per_100, ROLLING_WINDOW), 
                         ft_perc_per_100_avg = lagged_rollmean(ft_perc_per_100, ROLLING_WINDOW), 
                         `3pm_per_100_avg` = lagged_rollmean(`3pm_per_100`, ROLLING_WINDOW), 
                         `3pa_per_100_avg` = lagged_rollmean(`3pa_per_100`, ROLLING_WINDOW), 
                         `3pt_perc_per_100_avg` = lagged_rollmean(`3pt_perc_per_100`, ROLLING_WINDOW), 
                         tov_per_100_avg = lagged_rollmean(tov_per_100, ROLLING_WINDOW), 
                         efg_perc_avg = lagged_rollmean(efg_perc, ROLLING_WINDOW), 
                         true_shooting_perc_avg = lagged_rollmean(true_shooting_perc, ROLLING_WINDOW), 
                         threes_and_frees_avg = lagged_rollmean(threes_and_frees, ROLLING_WINDOW)) %>% 
                group_by(opp_TeamID) %>% 
                arrange(opp_TeamID, Season, DayNum) %>% 
                mutate(opp_fta_per_100_avg = lagged_rollmean(opp_fta_per_100, ROLLING_WINDOW), 
                       opp_ftm_per_100_avg = lagged_rollmean(opp_ftm_per_100, ROLLING_WINDOW), 
                       opp_ft_perc_per_100_avg = lagged_rollmean(opp_ft_perc_per_100, ROLLING_WINDOW), 
                       opp_3pm_per_100_avg = lagged_rollmean(`opp_3pm_per_100`, ROLLING_WINDOW), 
                       opp_3pa_per_100_avg = lagged_rollmean(`opp_3pa_per_100`, ROLLING_WINDOW), 
                       opp_3pt_perc_per_100_avg = lagged_rollmean(`opp_3pt_perc_per_100`, ROLLING_WINDOW), 
                       opp_tov_per_100_avg = lagged_rollmean(opp_tov_per_100, ROLLING_WINDOW), 
                       opp_efg_perc_avg = lagged_rollmean(opp_efg_perc, ROLLING_WINDOW), 
                       opp_true_shooting_perc_avg = lagged_rollmean(opp_true_shooting_perc, ROLLING_WINDOW), 
                       opp_threes_and_frees_avg = lagged_rollmean(opp_threes_and_frees, ROLLING_WINDOW)) %>%
                ungroup() %>% 
                arrange(Season, DayNum) %>%
                filter(!is.na(fta_per_100_avg)) %>% 
                mutate(location = case_when(location == "H" ~ 1,
                                            location == "N" ~ 0, 
                                            location == "A" ~ -1))
