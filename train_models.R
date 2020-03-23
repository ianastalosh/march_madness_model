
# Features come from 'team_features_rolling_averages.R'
TEST_SEASON = 2019
TRAIN_VAL_PROPORTION = 0.8

# Create feature list for easy testing
XGBOOST_FEATURES = c("location",
                     "fta_per_100_avg", 
                     "ft_perc_per_100_avg",
                     "3pa_per_100_avg",
                     "3pt_perc_per_100_avg",
                     "tov_per_100_avg", 
                     "efg_perc_avg",
                     "true_shooting_perc_avg",
                     "threes_and_frees_avg",
                     "opp_fta_per_100_avg", 
                     "opp_ft_perc_per_100_avg",
                     "opp_3pa_per_100_avg",
                     "opp_3pt_perc_per_100_avg",
                     "opp_tov_per_100_avg", 
                     "opp_efg_perc_avg",
                     "opp_true_shooting_perc_avg",
                     "opp_threes_and_frees_avg",
                     "seed", 
                     "opp_seed")

full_train_data = team_averages %>% ungroup() %>% filter(Season < TEST_SEASON) %>% select(XGBOOST_FEATURES, winner)
number_of_rows = nrow(full_train_data)

set.seed(7)
train_indicies = sample(1:number_of_rows, 0.8 * number_of_rows)

train_set_x = full_train_data[train_indicies, XGBOOST_FEATURES] %>% as.matrix()
train_set_y = full_train_data[train_indicies, "winner"] %>% as.matrix()

val_set_x = full_train_data[-train_indicies, XGBOOST_FEATURES] %>% as.matrix()
val_set_y = full_train_data[-train_indicies, "winner"] %>% as.matrix()

train_dmat = xgb.DMatrix(data = train_set_x, label = train_set_y)
val_dmat = xgb.DMatrix(data = val_set_x, label = val_set_y)

param = list(max_depth = 6, 
             eta = 0.1, 
             objective = "binary:logistic", 
             eval_metric = "logloss")

first_model = xgb.train(data = train_dmat, 
                        params = param, 
                        nrounds = 10000,
                        early_stopping_rounds = 10,
                        watchlist = list(train = train_dmat, validation = val_dmat))


test_data = team_averages %>% ungroup() %>% filter(Season == TEST_SEASON, ncaa_tournament == 1) %>% select(XGBOOST_FEATURES) %>% as.matrix()
probabilities = predict(first_model, test_data)


