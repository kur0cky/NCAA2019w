library(tidyverse)

tmp <- read_csv("data/processed/massey_3yr.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/WSampleSubmissionStage2.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            massey_3yr_diff = massey_3yr.x - massey_3yr.y,
            # massey_r.x,
            # massey_r.y
            # massey_o_diff = massey_o.x - massey_o.y,
            # massey_d_diff = massey_d.x - massey_d.y,
  )

write_csv(fe, "data/features/massey_3yr_fe.csv")
rm(tmp, fe);gc()
