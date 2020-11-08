source("R Analysis\\Files\\Read_Files_R.R")

#returns the average number of players 
#who played x% of their team's minutes played in the tournament
t_means <- function(x){
  #creates data frame of all players who played at least x% of their 
  #team's tournament minutes played
  df <- tourney_players %>%
    select(everything()) %>% filter(!is.na(Seed), MIN. >= x)

  #creates a data frame of the number of players who meet the above criteria on each team
  used <- df %>% 
    group_by(Team.ID) %>%
    mutate(num_players = n_distinct(Player.ID)) %>%
    filter(Year != 2008) %>%
    distinct(Team.ID, Team.Name, Year, Seed, G, num_players)
  
  #returns the average number of players per team who meet the above criteria
  mean(used$num_players)
}

#returns the average number of players 
#who played x% of their team's minutes played in the regular season
s_means <- function(x){
  #creates data frame of all players who played at least x% of their 
  #team's regular season minutes played
  df <- players %>%
    select(everything()) %>% filter(MIN. >= x)
  
  #creates a data frame of the number of players who meet the above criteria on each team
  used <- df %>% 
    group_by(Team.ID) %>%
    mutate(num_players = n_distinct(Player.ID)) %>%
    filter(Year != 2008) %>%
    distinct(Team.ID, Team.Name, Year, num_players)
  
  #returns the average number of players per team who meet the above criteria
  mean(used$num_players)
}

#creates a list of integers from 10-100 counting by 10s
intervals <- seq(10, 100, 10)

#calculate tourney_means and season_means for each number in intervals
tourney_means <- sapply(intervals, t_means)
season_means <- sapply(intervals, s_means)

#print the exact tourney means and season means values calculated above
tourney_means
season_means #final value in season_means is NaN because no players played 100%
              #of their team's regular season minutes

#plot the numbers in intervals vs
#the average number of players that meet minutes played criteria
plot(intervals, tourney_means, type='l', col = 'red',
     main = expression("Season and Tournament Avg. Players Used 
      By Teams vs. Pct. of Team Min. Played"),
     xlab = 'Minimum Pct. of Team Min. Played by Players',
     ylab = 'Avg. Players Meeting Minimum Min. Played for all Teams')
lines(intervals, season_means, col = 'green')
legend('topright', legend = c('Tournament', 'Season'),
       col = c('red', 'green'), lty=1)
points(intervals, tourney_means, col = 'red')
points(intervals, season_means, col = 'green')

