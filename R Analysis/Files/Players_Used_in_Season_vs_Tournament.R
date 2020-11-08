source("R Analysis\\Files\\Read_Files_R.R")

library(dplyr)
library(ggplot2)

#make a data frame with all players used in the tournament
#only include players who played at least 10% of their team's tournament minutes
ncaa_tourney_players <- tourney_players %>%
  select(everything()) %>% filter(!is.na(Seed), MIN. >= 10)

#make a data frame with all players used in the regular season
#only include players who played at least 10% of their team's regular season minutes
season_players <- players %>%
  select(everything()) %>% filter(MIN. >= 10)

#create data frame with the number of players each team used in their tournament games
t_players_used <- ncaa_tourney_players %>% 
  group_by(Team.ID) %>%
  mutate(num_players = n_distinct(Player.ID)) %>%
  filter(Year != 2008) %>%
  distinct(Team.ID, Team.Name, Year, Seed, G, num_players)

#create data frame with the number of players each team used in their season games
players_used <- season_players %>% 
  group_by(Team.ID) %>%
  mutate(num_players = n_distinct(Player.ID)) %>%
  filter(Year != 2008) %>%
  distinct(Team.ID, Team.Name, Year, num_players)

#join regular season and tournament players used tables by team
all_players_used <- left_join(players_used, t_players_used, by = c('Team.ID', 'Team.Name', 'Year'))

#rename columns in all_players_used to be more descriptive
all_players_used <- select(all_players_used, c(Team.ID, Team.Name, Year, num_players.x, num_players.y))
all_players_used <- na.omit(all_players_used)
all_players_used <- rename(all_players_used, s_players_used = num_players.x,t_players_used = num_players.y)

#plot players used in the regular season vs the tournament by team
all_players_used %>% ggplot(aes(x = s_players_used, y = t_players_used)) +
  geom_point(alpha = 0.05, aes(col = 'blue')) + 
  geom_smooth(method = 'lm')

#same plot as above but with a box plot
all_players_used %>% ggplot(aes(x = s_players_used, y = t_players_used)) +
  geom_boxplot(aes(group = s_players_used, fill = s_players_used))

#calculate the correlation between players used during the season and the tournament
cor(all_players_used$s_players_used, all_players_used$t_players_used)
