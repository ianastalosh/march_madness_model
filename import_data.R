
#### IMPORT DATA ----

# Team and players
players_data = read_csv("data/MPlayers.csv")
teams_data = read_csv("data/MDataFiles_Stage1/MTeams.csv")

# Regular season and tournament data 
reg_season_data = read_csv("data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv") %>% 
                    mutate(ncaa_tournament = 0,
                           game_id = paste(Season, DayNum, WTeamID, LTeamID, sep = "-"))

ncaa_tournament_data = read_csv("data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv") %>% 
                        mutate(ncaa_tournament = 1,
                               game_id = paste(Season, DayNum, WTeamID, LTeamID, sep = "-"))

total_season_data = bind_rows(reg_season_data, ncaa_tournament_data)

# Rankings
massey_data = read_csv("data/MDataFiles_Stage1/MMasseyOrdinals.csv")

# NCAA Tournament specific data
ncaa_tournament_seeds = read_csv("data/MDataFiles_Stage1/MNCAATourneySeeds.csv") %>% 
  mutate(region = substr(Seed, start = 1, stop = 1),
         seed_only = as.numeric(substr(Seed, start = 2, stop = 3)))


#### FORMAT AND MERGE NECESSARY DATA FRAMES
# How do I want to format the data - ideally should be team, opponent, 

winners_data = total_season_data %>% 
                rename(location = WLoc) %>% 
                rename_at(vars(starts_with("L")), funs(str_replace(., "L", "opp_"))) %>% 
                rename_at(vars(starts_with("W")), funs(str_replace(., "W", ""))) %>%
                mutate(winner = 1)

losers_data = total_season_data %>% 
                mutate(location = case_when(WLoc == "N" ~ "N",
                                            WLoc == "H" ~ "A",
                                            WLoc == "A" ~ "H")) %>% 
                select(-WLoc) %>% 
                rename_at(vars(starts_with("L")), funs(str_replace(., "L", ""))) %>% 
                rename_at(vars(starts_with("W")), funs(str_replace(., "W", "opp_"))) %>%
                mutate(winner = 0)

doubled_combined_data = bind_rows(winners_data, losers_data) %>% 
                          mutate(score_difference = Score - opp_Score) 

# Add tournament seeds
team_seed = ncaa_tournament_seeds %>% select(Season, TeamID, seed = seed_only)
opp_team_seed = ncaa_tournament_seeds %>% select(Season, opp_TeamID = TeamID, opp_seed = seed_only)

combined_data = doubled_combined_data %>% 
                  left_join(team_seed, by = c("Season", "TeamID")) %>% 
                  left_join(opp_team_seed, by = c("Season", "opp_TeamID")) %>%
                  mutate(seed = ifelse(ncaa_tournament == 0, NA, seed),
                         opp_seed = ifelse(ncaa_tournament == 0, NA, opp_seed))
