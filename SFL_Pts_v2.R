library(nflfastR)
library(tidyverse)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

nfl_positions <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL_rosters/master/nfl_positions_2011_2020.csv"))

data <- data %>% 
  left_join(nfl_positions, by = c("passer_id" = "player_id")) %>% 
  rename(
    passer_position = position
  ) %>% 
  left_join(nfl_positions, by = c("receiver_id" = "player_id")) %>% 
  rename(
    receiver_position = position
  ) %>% 
  left_join(nfl_positions, by = c("rusher_id" = "player_id")) %>% 
  rename(
    rusher_position = position) %>% 
  select(-c('player.x', 'player.y', 'player'))



rushing_yards <- .1
rushing_tds <- 6
rushing_fd <- 1
fum_lost <- -2

receiving_yards <- .1
receiving_tds <- 6
receptions <- 1
receiving_fd <- 1

passing_yards <- .05
passing_fd <- .25
passing_td <- 4
interception <- -2

SFL_Scoring <- c(rushing_yards,
                 fum_lost,
                 rushing_tds,
                 rushing_fd,
                 receiving_yards,
                 receiving_tds,
                 receptions,
                 receiving_fd,
                 passing_yards,
                 passing_fd,
                 passing_td,
                 interception
)

ru_data <- data %>% 
  filter(season_type=='REG', play_type== 'run') %>% 
  group_by(posteam, player_name=rusher_player_name) %>% 
  summarize(
    ru_yds=sum(yards_gained), fum=sum(fumble_lost), ru_TD=sum(rush_touchdown), ru_fd=sum(first_down)
  ) %>% 
  ungroup() %>% 
  arrange(-ru_yds)

re_data <- data %>%  
  filter(season_type=='REG', play_type == 'pass', !is.na(receiver_player_name))%>%
  group_by(posteam, player_name=receiver_player_name) %>%
  summarize(
    re_yds = sum(yards_gained), re_TD=sum(pass_touchdown),Receptions = round(sum(complete_pass),0), re_fd=sum(first_down)
  ) %>% 
  ungroup() %>% 
  arrange(-re_yds) %>%
  mutate_if(is.numeric, round, 2)

pa_data <- data %>%  
  filter(season_type=='REG', play_type == 'pass', !is.na(passer_player_name))%>%
  group_by(posteam, player_name=passer_player_name) %>%
  summarize(
    pa_yds = sum(yards_gained),pa_fd=sum(first_down_pass), pa_td=sum(pass_touchdown), int=sum(interception), fum=sum(fumble_lost)
  ) %>% 
  ungroup() %>% 
  arrange(-pa_yds) %>%
  mutate_if(is.numeric, round, 2)

ru_re_data <- left_join(ru_data, re_data, by = c("posteam", "player_name"))
fan_data <- left_join(ru_re_data, pa_data, by = c("posteam", "player_name"))

fan_data[is.na(fan_data)] <- 0

fan_data <- fan_data %>% 
  mutate(fan_pts=rowSums(sweep(fan_data[,3:14],MARGIN=2,SFL_Scoring, '*'))) %>% 
  arrange(-fan_pts)

fan_data %>% 
  head() %>% 
  ggplot(aes(x=player_name, y=fan_pts) +
  geom_col()

