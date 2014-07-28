# More R Code for Sabermetrics Problem Set

setwd('C:/Users/dsbmac/Documents/Professional Development/Sabermetrics/data')

retro <- read.csv('retrosheet_columns.csv')
str(retro)
table(retro$Field, retro$Key)

# We are now going to use R to analyze some data from the retrosheet database. We are going to use the csv from the previous section "game_statistics.csv".
# To start, use either the file.choose() command or just the read.csv() command to import "game_statistics.csv". Sample versions of code for this could include:
  
game_statistics <- read.csv('game_statistics.csv')

str(game_statistics)

#Please enter the following code in order to not have to retype "game_statistics" whenever we use a column in the game_statistics data frame:

attach(game_statistics)

game_statistics$total_runs <- with(game_statistics, home_score + visitor_score)

total_time_data <- game_statistics[c('game_minutes', 'total_runs', 'outs')]

require('lattice')

splom(total_time_data)

projected_minutes <- lm(game_minutes ~ total_runs + outs)
summary(projected_minutes)


# we will start by creating a new dummy variable which assigns a value of 1 to any game involving the Red Sox and a 0 otherwise:

game_statistics$RedSox_playing <- with(game_statistics, ifelse(home == 'BOS' | visitor == 'BOS', 1, 0))
RedSox_games <- game_statistics[game_statistics$RedSox_playing == 1,]

x = subset(game_statistics, RedSox_playing == 1, select=c(total_runs))
mean(x$total_runs)

str(x)
mean(game_statistics$total_runs)


hist(RedSox_games$total_runs, breaks=40, main="Breaks=40")

# umpires Brian Gorman, Jim Joyce, Dale Scott, and Tim Welke.
game_statistics$BrianGorman <- with(game_statistics, ifelse(hp_ump_name == 'Brian Gorman', 1, 0))
game_statistics$JimJoyce <- with(game_statistics, ifelse(hp_ump_name == 'Jim Joyce', 1, 0))
game_statistics$DaleScott <- with(game_statistics, ifelse(hp_ump_name == 'Dale Scott', 1, 0))
game_statistics$TimWelke <- with(game_statistics, ifelse(hp_ump_name == 'Tim Welke', 1, 0))

# Now, create 4 new data frames which consist of the games umpired by Brian Gorman, Jim Joyce, Dale Scott, and Tim Welke.

BrianGorman_games <- game_statistics[game_statistics$BrianGorman == 1,]
JimJoyce_games <- game_statistics[game_statistics$JimJoyce == 1,]
DaleScott_games <- game_statistics[game_statistics$DaleScott == 1,]
TimWelke_games <- game_statistics[game_statistics$TimWelke == 1,]

# Select all of the umpires who gave up more than league average number of total runs per game.

#league avg
mean(game_statistics$total_runs)

mean(BrianGorman_games$total_runs)
mean(JimJoyce_games$total_runs)
mean(DaleScott_games$total_runs)
mean(TimWelke_games$total_runs)

# run a multivariate regression to predict the amount of total runs per game by looking at whether the Red Sox were playing and whether any of Brian Gorman, Jim Joyce, Dale Scott, and Tim Welke was the home plate umpire. 
projected_runs = lm(total_runs ~ RedSox_playing + BrianGorman + JimJoyce + DaleScott
                    + TimWelke, data=game_statistics)

summary(projected_runs)

# Additionally, once you find the coefficients of the regression, please find the 95% confidence intervals of each coefficient estimate in the regression as such:

confint(projected_runs, level = 0.95)
