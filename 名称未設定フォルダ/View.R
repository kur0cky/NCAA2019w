library(tidyverse)
seeds <- read_csv("data/processed/seed.csv")
team_name <- read_csv("data/wdatafiles/wteamspellings.csv")
teams <- read_csv("data/wdatafiles/wteams.csv")
sub2 <- read_csv("data/submit/first.csv")

sub2 %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(teams, by = c("team1" = "TeamID")) %>% 
  left_join(teams, by = c("team2" = "TeamID")) %>% 
  left_join(seeds, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seeds, by = c("Season", "team2" = "TeamID")) %>% 
  # filter(seed.y %in% c(1:4,13:16), seed.x %in% c(1:4,13:16)) %>% 
  filter(seed.x + seed.y == 17) %>% 
  select(-ID) %>% 
  View()
