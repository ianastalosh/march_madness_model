
#### LOAD PACKAGES ----
library(tidyverse)
library(janitor)
library(zoo)

#### RUN SCRIPTS ----

# Import data
source("import_data.R")

# Train models
source("elo_ratings.R")



# TODO Optimize k-factor and season reversion to mean factor
# TODO Clustering around team style, ie. 
  ## number of 2pt attempts per game
  ## number of 3pt attempts per game (as percentage of total)
  ## number of free throws per game 
  ## number of rebounds per game?
  ## then use this clustering as a feature
  ## find metric on advanced stats
# TODO Create ensemble prediction method using classifier + elo
# TODO Create rolling train/test set to measure accuracy and performance. 