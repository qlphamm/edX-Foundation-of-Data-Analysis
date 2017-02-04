#__________________ START OF PRE-LAB __________________

# Subset data for just the United States
us <- world[world$Country.Code == "USA",]

# Select the years from 1990
us_select <- us[us$year >= 1990, ]

# Create a new variable in our datset called internet.mil to make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

# Create a new variable in our dataset called time that represents "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10,]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)
l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)


#__________________ START OF LAB __________________

# Subset data for just Denmark since 1990
denmark <- world[world$Country =='Denmark',]
denmark_90 <- denmark[denmark$year >= 1990,]

# Create a variable that represents proportion of the population using the internet (internet users divided by population).
prop_internet <- denmark_90$internet.users/denmark_90$population

# Create a new variable that is "years since 1990". 
time_90 <- denmark_90$year - 1990

# Determine the best-fitting model (exponential or logistic) for internet usage in each country from 1990 onward.
plot(time_90, prop_internet, xlab = 'Year since 1990', ylab = 'Proportion of population using internet')
expFit(time_90, prop_internet, xlab = 'Year since 1990', ylab = 'Proportion of population using internet')
logisticFit(time_90, prop_internet, xlab = 'Year since 1990', ylab = 'Proportion of population using internet')

#__________________ START OF PROBLEM SET __________________
brazil <- world[world$Country=='Brazil',]
brazil_95 <- brazil[brazil$year>= 1995,]
time_95 <- brazil_95$year - 1995

plot(time_95, brazil_95$mobile.users, xlab = 'Time Since 1995', ylab = 'Number of mobile users')
tripleFit(time_95, brazil_95$mobile.users, xlab = 'Time Since 1995', ylab = 'Number of mobile users')
logisticFitPred(time_95, brazil_95$mobile.users, 30, xlab = 'Time Since 1995', ylab = 'Number of mobile users')
