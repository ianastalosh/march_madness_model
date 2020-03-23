# March Madness Model
Repo for the Kaggle 2020 March Madness Competition, but progress was stopped after the competition was suspended.

The data is obtained from the Kaggle competition link at https://www.kaggle.com/c/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/data. A folder called 'data' needs to be created in the repo which contains the data, and the scripts run off that.

Currently, the scripts:
- Import the data
- Format the data so it's not in the Winning Team / Losing Team format into Team 1 / Team 2 format, doubling the number of rows and adding a column if that team won or not.
- Creating an elo system for college basketball
- An optimizer on that elo system to find the optimal k-value and season to season reversion factor. 
- I also played around with trying to cluster based on the number of 2's, 3's and free throws attempted by a team. I tried to build soem features based on these with little success.

No 'final' model incorporating all of these was created as the competition was suspended. My plan was to either:
1. Develop a neural net or XGBoost model using these team features. The probability from this model would be blended with the probability from the elo, with those weightings being determined by some further analysis.
2. Add elo as a feature to that neural net or XGBoost model.

The code may not be clean, but I'm putting it here so I don't lose it while backing up my computer. 
