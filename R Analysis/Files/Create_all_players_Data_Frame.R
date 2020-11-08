source("R Analysis\\Files\\Read_Files_R.R")

library(dplyr)
library(ggplot2)
library(gridExtra)

#filter out 2008 stats for season and tourney players
all_players <- players %>% 
  select(c(Player.ID:MIN.)) %>%
  filter(Year != 2008)

all_tourney_players <- tourney_players %>%
  select(everything()) %>%
  filter(Year != 2008, !is.na(Seed))

# connect tourney stats with season stats for each player
all_players <- inner_join(x = all_players, y = all_tourney_players, 
                          by = c("Player.ID", "Player.Name", "Team.ID", 
                                 "Team.Name", "Year"))

# fix the column names to be more descriptive
all_players <- rename(all_players, season_games = 'G.x', season_min = 'MIN..x', 
                      tourney_games = 'G.y', tourney_min = 'MIN..y')

#connect team stats to basic team info
all_teams <- inner_join(x = eff_metrics, y = team_info, by = NULL)

# filter out teams from 2008 and that didn't make the NCAA Tournament
all_teams <- all_teams %>% 
  select(c(Team.ID:Seed, Conference:L)) %>%
  filter(Year != 2008, !is.na(Seed))

# add team info to the player data frame
all_players <- inner_join(x = all_players, y = all_teams, by = NULL)

all_players <- rename(all_players, team_games = 'Games')

# calculate how many games each team played in the tournament
team_tournament_games <- tourney_players %>% 
  group_by(Team.ID) %>%
  mutate(team_games_played = max(G)) %>%
  filter(Year != 2008, !is.na(Seed)) %>%
  distinct(Team.ID, Team.Name, Year, Seed, team_games_played)

# add this to the all_players data frame and name it team_tourney_games
all_players <- inner_join(x = all_players, y = team_tournament_games, by = NULL)
all_players <- rename(all_players, team_tourney_games = 'team_games_played')

# calculate the ratio of minutes to % of games for season and tourney
# calcluate the height in inches of players
all_players <- all_players %>% 
  mutate(s_min_ratio = season_min / (season_games / team_games),
         t_min_ratio = tourney_min / (tourney_games / team_tourney_games),
         height = as.integer(Feet) * 12 + as.integer(Inches),
         Seed = factor(Seed),
         Conference = factor(Conference),
         Class = factor(Class))

# has set up the all_players data frame with the columns needed