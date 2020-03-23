
# Elo model
COL_GAME_ID = "game_id"
COL_SEASON = "season"
COL_DAY_NUM = "day_num"
COL_TEAM_ID = "team_id"
COL_PREVIOUS_ELO = "prematch_elo_rating"
COL_RESULT = "result"
COL_NEW_ELO = "postmatch_elo_rating"
ELO_BASE_VALUE = 1500
ELO_SEASON_MEAN_REVERSION = 0.75
ELO_K_FACTOR = 20
ELO_START_BURN_IN = 2009
ELO_FIRST_SEASON = 2015
ELO_LAST_SEASON = 2019

# Want it to look like:
# game_id / team / previous_elo_rating / result / post_elo_rating

generate_elo_win_probability = function(team1_elo, team2_elo) {
  
  # Get the difference in elos
  elo_diff = team1_elo - team2_elo
  
  # Return win probability
  (1)/(10^(-elo_diff/400) + 1)
  
}

compute_elo_for_single_game = function(game_row, k_factor, reference_elo) {
  
  # Extract information from row
  game_id = game_row$game_id
  season = game_row$Season
  day_num = game_row$DayNum
  winning_team_id = game_row$WTeamID
  losing_team_id = game_row$LTeamID
  final_score_difference = game_row$WScore - game_row$LScore
  
  # Get elo values before game
  w_pre_elo = reference_elo %>% filter(team_id == winning_team_id) %>% select(elo_value) %>% unlist() %>% unname()
  l_pre_elo = reference_elo %>% filter(team_id == losing_team_id) %>% select(elo_value) %>% unlist() %>% unname()
  
  # Probability of each team winning
  pr_w_elo = generate_elo_win_probability(w_pre_elo, l_pre_elo)
  pr_l_elo = generate_elo_win_probability(l_pre_elo, w_pre_elo)
  
  # Get new ratings
  w_post_elo = w_pre_elo + k_factor * (1 - pr_w_elo)
  l_post_elo = l_pre_elo + k_factor * (0 - pr_l_elo)
  
  # Update reference data frame
  reference_elo$elo_value[reference_elo$team_id == winning_team_id] = w_post_elo
  reference_elo$elo_value[reference_elo$team_id == losing_team_id] = l_post_elo
  
  winner_row = data.frame(game_id = game_id, 
                          season = season,
                          day_num = day_num,
                          team_id = winning_team_id, 
                          prematch_elo_rating = w_pre_elo,
                          result = final_score_difference, 
                          postmatch_elo_rating = w_post_elo)
  
  loser_row = data.frame(game_id = game_id, 
                         season = season,
                         day_num = day_num,
                         team_id = losing_team_id, 
                         prematch_elo_rating = l_pre_elo,
                         result = -final_score_difference, 
                         postmatch_elo_rating = l_post_elo)
  
  output = list(winner = winner_row,
                loser = loser_row,
                updated_reference_elo = reference_elo)
  
  return(output)
  
}

revert_elo_to_mean = function(elo_values, reversion_to_mean_factor) {
  
  # Should move the elo value closer to the base value by the factor specified
  updated_elo_values = elo_values %>% 
                        mutate(elo_value = case_when(elo_value >= ELO_BASE_VALUE ~ elo_value - reversion_to_mean_factor * (elo_value - ELO_BASE_VALUE),
                                                     elo_value < ELO_BASE_VALUE ~ elo_value + reversion_to_mean_factor * (ELO_BASE_VALUE - elo_value)))
  
  return(updated_elo_values)
  
}

produce_elo_ratings = function(start_season, end_season, k_factor, season_reversion_factor) {
  
  season_names = start_season:end_season
  
  # Get only relevant season data
  total_data = total_season_data %>% 
                filter(Season %in% season_names)
  
  # Iterate through each season and get game by game elo adjustments
  season_elo_results = vector(mode = "list", length = length(season_names))
  names(season_elo_results) = season_names
  
  # Initialize starting elo values
  unique_ids = unique(combined_data$TeamID) 
  reference_elo = data.frame(team_id = unique_ids, elo_value = ELO_BASE_VALUE)
  elo_data = list(reference_values = reference_elo)
  
  for (season_index in 1:length(season_names)) {
    
    # Get current season
    current_season = season_names[season_index]
    
    season_data = total_season_data %>% 
                    filter(Season == current_season)
    
    # Create empty matrix to fill with weekly elo flucs
    elo_columns = c(COL_GAME_ID, COL_SEASON, COL_DAY_NUM, COL_TEAM_ID, COL_PREVIOUS_ELO, COL_RESULT, COL_NEW_ELO)
    elo_results = matrix(0, nrow = 2 * nrow(season_data), ncol = length(elo_columns)) %>% as.data.frame()
    colnames(elo_results) = elo_columns
    elo_results$game_id = as.character(elo_results$game_id)
    
    print(paste("Getting elo for", current_season, "season."))
    
    for (game_index in 1:nrow(season_data)) {
      
      current_game = season_data[game_index, ]
      elo_from_game = compute_elo_for_single_game(current_game, k_factor, elo_data$reference_values)
      elo_results[2 * game_index - 1, 1:length(elo_columns)] = elo_from_game$winner %>% unlist()
      elo_results[2 * game_index, 1:length(elo_columns)] = elo_from_game$loser %>% unlist()
      elo_data$reference_values = elo_from_game$updated_reference_elo
      
      if (game_index %% 1000 == 0) {
        print(game_index)
      }
      
    }
    
    # Add results to empty list
    season_elo_results[[season_index]] = elo_results
    
    # If this is not the last season, revert these values to the mean
    if (season_index != length(season_names)) {
    elo_data$reference_values = revert_elo_to_mean(elo_data$reference_values, season_reversion_factor)
    }
    
  }
  
  compiled_results = bind_rows(season_elo_results)
  
  # Return list with both full history and also final values
  full_results = list(elo_results = compiled_results, final_elo_values = elo_data$reference_values)
  
  return(full_results)
  
}


elo_with_burnin = produce_elo_ratings(ELO_START_BURN_IN, ELO_LAST_SEASON, ELO_K_FACTOR, ELO_SEASON_MEAN_REVERSION)

elo_team_history = elo_with_burnin$elo_results %>% 
                    filter(season >= ELO_FIRST_SEASON)

elo_final_value = elo_with_burnin$final_elo_values
