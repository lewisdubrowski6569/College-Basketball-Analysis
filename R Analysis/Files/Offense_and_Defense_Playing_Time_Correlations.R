source("R Analysis\\Files\\Create_all_players_Data_Frame.R")

# plots offense and defense compared to tournament minutes played
all_players %>% ggplot(aes(x = O.PRPG, y = tourney_min)) +
  geom_point(alpha = 0.5, aes(col = factor(Year))) +
  geom_smooth(method = 'lm', formula = y ~ x)

all_players %>% ggplot(aes(x = D.PRPG, y = tourney_min)) +
  geom_point(alpha = 0.5, aes(col = factor(Year))) +
  geom_smooth(method = 'lm', formula = y ~ x)

# creates linear regression model for offense vs. tourney mins
offense <- lm(all_players$tourney_min ~ all_players$O.PRPG)
summary(offense)

#calculate the correlation between players' minutes played in the tournament
#and their offensive points above replacement per game
tourney_off_correlation <- cor(all_players$tourney_min, all_players$O.PRPG)

# creates linear regression model for defense vs. tourney mins
defense <- lm(all_players$tourney_min ~ all_players$D.PRPG)
summary(defense)

#calculate the correlation between players' minutes played in the tournament
#and their defensive points above replacement per game
tourney_def_correlation <- cor(all_players$tourney_min, all_players$D.PRPG)

# creates linear regression model for offense vs. regular season mins
offense <- lm(all_players$season_min ~ all_players$O.PRPG)
summary(offense)

# calculates correlation between players' minutes played in the regular season
#and their offensive points above replacement per game
season_off_correlation <- cor(all_players$season_min, all_players$O.PRPG)

# creates linear regression model for defense vs. regular season mins
defense <- lm(all_players$season_min ~ all_players$D.PRPG)
summary(defense)

# calculates correlation between players' minutes played in the regular season
#and their defensive points above replacement per game
season_def_correlation <- cor(all_players$season_min, all_players$D.PRPG)

# defense is more correlated with playing time than offense
tourney_off_correlation
tourney_def_correlation

season_off_correlation
season_def_correlation