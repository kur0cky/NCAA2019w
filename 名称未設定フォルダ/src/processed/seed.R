library(tidyverse)

tourney_seeds <- read_csv("data/WDataFiles/WNCAATourneySeeds.csv")
set.seed(1)
seed_tmp <- tourney_seeds %>% 
  transmute(Season, 
            TeamID,
            # seed = as.integer(str_sub(Seed, 2,3)) < 9,
            seed = as.integer(str_sub(Seed, 2,3))
            )


write_csv(seed_tmp, "data/processed/seed.csv")
rm(seed_tmp); gc()
