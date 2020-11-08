source("R Analysis\\Files\\Read_Files_R.R")

library(dplyr)
library(ggplot2)
library(cowplot)

#create data frame with players whose teams played in the tournament
#only include players who played at least 10% of their team's minutes
ncaa_tourney_players <- tourney_players %>%
  select(everything()) %>% filter(!is.na(Seed), MIN. >= 10)

#create data frame with all players who played at least 10% of their team's minutes
season_players <- players %>%
  select(everything()) %>% filter(MIN. >= 10)

#create data frame with the number of players each team used in their tournament games
t_players_used <- ncaa_tourney_players %>% 
  group_by(Team.ID) %>%
  mutate(num_players = n_distinct(Player.ID)) %>%
  filter(Year != 2008) %>%
  distinct(Team.ID, Team.Name, Year, Seed, G, num_players)

#calculate the average number of players all teams used in the tournament
tourney_avg <- mean(t_players_used$num_players)

#create data frame with the number of players each team used in their season games
players_used <- season_players %>% 
  group_by(Team.ID) %>%
  mutate(num_players = n_distinct(Player.ID)) %>%
  filter(Year != 2008) %>%
  distinct(Team.ID, Team.Name, Year, num_players)

#calcualte the average number of players all teams used in the season
season_avg <- mean(players_used$num_players)

#plot the number of players used by teams during tournament games
#vs the frequency of teams using each number of players
t_plot <- ggplot(t_players_used, aes(factor(num_players))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = 'blue') +
  labs(title = 'Number of Players Used in Tournament',
       subtitle = 'Minimum 10% of Team Minutes Played',
       y = 'Frequency', x = 'Number of Players') + 
  scale_x_discrete(breaks = seq(5, 15), limits = factor(seq(5, 15)))

#same plot as above but for players used during regular season games
s_plot <- ggplot(players_used, aes(factor(num_players))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = 'red') +
  labs(title = 'Number of Players Used in Season',
       subtitle = 'Minimum 10% of Team Minutes Played',
       y = 'Frequency', x = 'Number of Players') +
  scale_x_discrete(breaks = seq(5, 15), limits = factor(seq(5, 15)))

#align the plots with the players used in the season
#above the players used in the tournament
plot_grid(s_plot, t_plot, ncol = 1, align = 'v')

#print the season and tournament average players used by teams
cat("Mean players used during season:", season_avg)
cat("Mean players used during tournament:", tourney_avg)

#teams play fewer players in the tournament than during the regular season
