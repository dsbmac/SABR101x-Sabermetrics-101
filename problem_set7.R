# RStudio Walkthrough Problem Set

setwd('C:/Users/dsbmac/Documents/Professional Development/Sabermetrics/data')

win_estimators <- read.csv('win_estimators.csv')

summary(win_estimators)

mean(win_estimators$WPct)
mean(win_estimators$R)

sd(win_estimators$WPct)

1/162

a = 690.4
x = a - sqrt(a^2 / (1/(.500-1/162)-1))
x

estimators <- win_estimators[c('RperG', 'RAperG', 'WPct','Cook_WPct',  'Soolman_WPct',  'Kross_WPct', 'Smyth_WPct', 'BJames_Pythag_WPct', 'BJames_Pythag_WPctII')]

require('lattice')

splom(estimators, xlab='Win Estimators')

# To start, we are going to look at just the teams who finished higher than the 95th percentile in win percentage (a.k.a the outlier winners).  quantile() function gives you any percentile you specify, and we use this as a threshold to filter data into our new data frame, top_winners.

wpct_95th_pct = quantile(win_estimators$WPct, .95)

top_winners = win_estimators[win_estimators$WPct >= wpct_95th_pct, ]
summary(top_winners)
mean(top_winners)

splom(top_winners, xlab='top_winners')
str(top_winners)
cor(top_winners$BJames_Pythag_WPct)

cor(top_winners$WPct, top_winners$BJames_Pythag_WPctII)

cor(win_estimators$WPct, win_estimators$BJames_Pythag_WPctII)