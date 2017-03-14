# edX-Foundation-of-Data-Analysis
#R Scripts for UTAustinX's UT.7.11x edX course Foundation of Data Analysis

## PART 1 
plot (bull$YearsPro, bull$BuckOuts12)
abline(lm(bull$BuckOuts12 ~ bull$YearsPro))
plot(bull$Events12, bull$BuckOuts12, xlab = "Number of 2012 Events", ylab = "Number of 2012 Buckouts", main = "Bull Rider Scatterplot")
abline(lm(bull$BuckOuts12 ~ bull$Events12))

#---------START OF PRELAB QUESTIONS---------

#Subset for riders that participated in at least one event in 2013
new_bull <- bull[bull$Events13  > 0 ,]

# Visualize and describe the first variable of interest
hist(new_bull$Rides13)
fivenum(new_bull$Rides13)
mean(new_bull$Rides13)
sd(new_bull$Rides13)
median(new_bull$Rides13)

# Visualize and describe the second variable of interest
hist(new_bull$Top10_13)
fivenum(new_bull$Top10_13)
mean(new_bull$Top10_13)
sd(new_bull$Top10_13)
median(new_bull$Top10_13)

# Create a scatterplot
plot(new_bull$Rides13,new_bull$Top10_13)

# Add line of best fit
abline(lm(new_bull$Top10_13~new_bull$Rides13))

# Calculate the correlation coefficient
cor(new_bull$Rides13,new_bull$Top10_13)

# Create a correlation matrix
vars <- c("Top10_13", "Rides13")
cor(new_bull[,vars])

#identify a specific record
which(new_bull$Top10_13==2 & new_bull$Rides13==22)
new_bull[4,]


#---------LAB QUESTIONS---------
new_bull12 <- bull[bull$Events12 > 0,]

#Make a histogram to visualize the distribution of Earnings for 2012.
hist(new_bull12$Earnings12, xlab = 'Earning' , main = 'Riders Earning in 2012')
median(new_bull12$Earnings12)
max(new_bull12$Earnings12)

#Make a correlation matrix for Earnings12, RidePer12 and CupPoints12.
vars <- c("Earnings12", "RidePer12", "CupPoints12")
cor(new_bull12[,vars])

#Make a Scatterplot of Earnings and Ride Percentage
plot(new_bull12$Earnings12, new_bull12$RidePer12)

#Create a Scatterplot of Earnings and Cup Points
plot(new_bull12$Earnings12, new_bull12$CupPoints12, xlab = "Earning in 2012", ylab = "Cup Points at the end of 2012")
abline(lm(new_bull12$CupPoints12 ~ new_bull12$Earnings12))

# identify the outlier
which(new_bull12$Earnings12 == max(new_bull12$Earnings12))

#Subset the data to remove the outlier
nooutlier <- new_bull12[new_bull12$Earnings12 < 1000000 ,]

#rerun the correlation matrix and the scatterplots
vars <- c("Earnings12", "RidePer12", "CupPoints12")
cor(nooutlier[,vars])
plot(nooutlier$Earnings12, nooutlier$RidePer12)
abline(lm(nooutlier$RidePer12 ~ nooutlier$Earnings12))



## PART 2

age <- 2014 - bull$YearBorn
hist(age)
t.test(age, mu = 30)
t.test(age, muu = 30, alternative = 'less')

#---------START OF PRE-LAB----------
#1. Create a data frame for the US bull riders, and then calculate the sample mean and standard deviation for the weight of the bull-riders.
#2. Create a histogram to visualize the distribution of bull-riders' weights.  
#3. Confirm the assumptions of a one-sample t-test
#4. Run the t-test and interpret the results.
  
#Select bull riders from the US
USA <-bull[bull$Country=="USA",]

# Summarize the bull rider weights
mean(USA$Weight)
sd(USA$Weight)

# Visualize the weight distribution
hist(USA$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)', xlim=c(100,200))

# Run the single sample t-test
t.test(USA$Weight, mu=190)
sd(USA$Weight)


#---------START OF LAB----------
riders5 <- bull[bull$Events14 >= 5,]  
hist(riders5$RidePer14, xlab = 'Average Ride Percentage', xlim=c(0,0.6))
mean(riders5$RidePer14)
sd(riders5$RidePer14)
t.test(riders5$RidePer14, mu = 0.5)
