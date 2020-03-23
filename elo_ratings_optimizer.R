# Elo optimizer
K_FACTOR_RANGE = seq(from = 5, to = 50, by = 5)
SEASON_REVERSION_RANGE = seq(from = 0.05, to = 0.95, by = 0.05)
ELO_SEARCH_GRID = expand_grid(K_FACTOR_RANGE, SEASON_REVERSION_RANGE)

OUTPUT_K_COL = "k_factor"
OUTPUT_SEASON_REVERSION_COL = "season_reversion_factor"
OUTPUT_LOG_LOSS = "log_loss"


measure_elo_accuracy = function(elo_start_burnin, start_test, end_test, k_factor, season_reversion_factor) {
  
  # Compute elo ratings
  run_elo = produce_elo_ratings(elo_start_burnin, end_test, k_factor, season_reversion_factor)
  
  # Get elo history
  elo_team_history = run_elo$elo_results %>% 
                      filter(season >= elo_start_burnin) %>% 
                      select(season, day_num, team_id, prematch_elo_rating, result)
  
  # Get match test schedule
  test_data = total_season_data %>% 
                filter(Season >= start_test) %>% 
                select(Season, DayNum, WTeamID, LTeamID, WScore, LScore)
  
  # Rename column so I can join both the winning and losing team elos
  wteam_data = elo_team_history %>% select(season, day_num, team_id, WTeamELO = prematch_elo_rating)
  lteam_data = elo_team_history %>% select(season, day_num, team_id, LTeamELO = prematch_elo_rating)

  # Join elo to match results to get log loss 
  joined_with_elo = test_data %>% 
                      left_join(wteam_data, by = c("Season" = "season", 
                                                    "DayNum" = "day_num",
                                                    "WTeamID" = "team_id")) %>% 
                      left_join(lteam_data, by = c("Season" = "season", 
                                                   "DayNum" = "day_num",
                                                   "LTeamID" = "team_id")) %>% 
                      mutate(WEloDiff = WTeamELO - LTeamELO,
                             WWinProb = (1)/(10^(-WEloDiff/400) + 1),
                             LogLoss = log(WWinProb))
  
  # Return the log loss
  average_log_loss = mean(joined_with_elo$LogLoss)
  
  # Return both the full elo history and the average log loss
  # TODO Decide if you actually need to output the full elo results
  full_results = list(elo_output = run_elo, log_loss = average_log_loss)
  return(full_results)
  
}

# Create function to do grid search
run_elo_grid_search = function(elo_start_burnin, start_test, end_test, k_range, season_reversion_range) {
  
  # Create the grid to actually search
  search_grid = expand_grid(k_range, season_reversion_range)
  
  # Create empty matrix to store results
  output_column_names = c(OUTPUT_K_COL, OUTPUT_SEASON_REVERSION_COL, OUTPUT_LOG_LOSS)
  
  elo_log_loss_results = matrix(0, nrow = nrow(search_grid), ncol = length(output_column_names), 
                                dimnames = list(1:nrow(search_grid), output_column_names))
  
  # Iterate through loop
  for (combo_index in 1:nrow(search_grid)) {
    
    # Get the input parameters
    current_k = search_grid[combo_index, 1] %>% unlist()
    current_season_reversion_factor = search_grid[combo_index, 2] %>% unlist()
    
    print(paste("The time is", Sys.time()))
    print(paste("The current k is", current_k, "and the current season reversion factor is", current_season_reversion_factor))
    
    # Get the log loss for this particular combination
    current_elo = measure_elo_accuracy(elo_start_burnin, start_test, end_test, current_k, current_season_reversion_factor)
    current_log_loss = current_elo$log_loss
    
    # Put the results into the matrix
    elo_log_loss_results[combo_index, OUTPUT_K_COL] = current_k
    elo_log_loss_results[combo_index, OUTPUT_SEASON_REVERSION_COL] = current_season_reversion_factor
    elo_log_loss_results[combo_index, OUTPUT_LOG_LOSS] = current_log_loss
    
    print(paste("The log loss for this combination is", current_log_loss))
    
  }
  
  # Return the results
  return(elo_log_loss_results)
  
}

# Small search (to make sure it works)
# elo_grid_search_results = run_elo_grid_search(2014, 2015, 2019, c(5, 25, 45), c(0.05, 0.5, 0.95))

# Full search
## The grid currently has 190 rows. One row, with burnin from 2009, takes ~6 minutes. So, looking at ~19 hours total. 
elo_grid_search_results = run_elo_grid_search(2009, 2015, 2019, K_FACTOR_RANGE, SEASON_REVERSION_RANGE)

