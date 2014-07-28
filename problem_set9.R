# Park Factors in R

setwd('C:/Users/dsbmac/Documents/Professional Development/Sabermetrics/data')

# Now, we are going to find the Rockies park factors for 2013 for home runs. To do so, we first have to download our file. We have given you the code for 2 alternative ways to access the file in R.

rox <- read.csv('coors_park_factors.csv')

# After this, we need to create home and away data frames as such:

park <- subset(rox, home == "COL")

away <- subset(rox, visitor == "COL")

# Finally, you will need to find the ratios for home and away home runs at Coors:
  
park_ratio <- sum(park$home_hr + park$visitor_hr) / sum(park$home_ab + park$visitor_ab)

away_ratio <- sum(away$home_hr + away$visitor_hr) / sum(away$home_ab + away$visitor_ab)

pf = 100 * park_ratio / away_ratio
pf


### Once again, access the file in R. We saved the data frame as pf_all. You will need the plyr package for this function. Additionally, since our data from 2009-2013 includes the Marlins as 'FLO' and 'MIA', execute the following code to make sure we label all Marlins games as 'MIA':

require(plyr)

pf_all <- read.csv('park_factors.csv')

pf_all <- within(pf_all, levels(home)[levels(home)=='FLO'] <- 'MIA')

pf_all <- within(pf_all, levels(visitor)[levels(visitor)=='FLO'] <- 'MIA')

# function pf_stat_teams takes in a statistic, a data frame, and a year and returns a data frame with all of the teams' park factor for the specified statistic for the specified year:

pf_stat_teams <- function(stat, data, season_year=2013) {

  # To start, we will filter our data frame by the correct year.
  data <- subset(data, year == season_year)

  # We next build strings for accessing proper PF stat, followed by filtering out unnecessary columns.
  home_stat = paste("home", stat, sep="_")

  visitor_stat = paste("visitor", stat, sep="_")
  
  pf_stat = paste("pf", stat, season_year, sep="_")
  
  cols = c(home_stat, visitor_stat, "home_ab", "visitor_ab")
  
  # We continue by separating by home/away as we did in the Rockies HR Park Factor for 2013, but now we group by team as well. Additionally, we calculate sums of desired stat and AB in this step.
  
  park_sums <- ddply(data, .(home), colwise(sum, cols))
  
  away_sums <- ddply(data, .(visitor), colwise(sum, cols))
  
  # We now calculate stat/AB frequency ratios.
  
  park_sums$park_ratio <- (park_sums[[home_stat]] + park_sums[[visitor_stat]]) / (park_sums[["home_ab"]] + park_sums[["visitor_ab"]])
  
  away_sums$away_ratio <- (away_sums[[home_stat]] + away_sums[[visitor_stat]]) / (away_sums[["home_ab"]] + away_sums[["visitor_ab"]])
  
  # We proceed by merging together park and visitor ratios. Notice how the merge() function and the by statement inside of it as a parameter behave like a JOIN and ON in SQL.
  
  pf <- merge(park_sums, away_sums, by.x="home", by.y="visitor")
  
  # Now, we calculate the ratio of ratios to get the final PF.
  
  pf[[pf_stat]] <- with(pf, pf$park_ratio / pf$away_ratio)
  
  # Finally, we clean up the dataframe, and return this version.
  
  pf <- rename(pf, replace=c("home" = "team"))
  
  pf <- subset(pf, select = c("team", pf_stat))
  
  return(pf)

}

# With this function, we can see the entire league's park factors for any statistic for any year. We provide below example code to find the 2013 home run park factors:

hr_2013 <- pf_stat_teams("hr", pf_all, season_year=2013)
doubles_2010 <- pf_stat_teams("2b", pf_all, season_year=2010)
bb_2011 <- pf_stat_teams("bb", pf_all, season_year=2011)

str(bb_2010)

attach(bb_2011)

doubles_2010[order(pf_2b_2010),]
bb_2011[order(pf_bb_2011),]
