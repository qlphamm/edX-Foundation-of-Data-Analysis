mean(survey$age)
sd(survey$age)
hist(survey$age)
sample(survey$age,30)
myxbar <- rep(NA, 1000)
for (i in 1:1000) {
  mysamp <-sample(survey$age, size = 30)
  myxbar[i] <- mean(mysamp)
}
myxbar
hist(myxbar)
mean(myxbar)

#--------------START OF PRE-LAB---------------------

# Calculate the population parameters
hist(survey$name_letters)
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

# Draw 1,000 samples of n=5 and find the mean of each sample
xbar5 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$name_letters, size = 5)
  xbar5[i] <- mean(x)
}
xbar5

# Graph the histogram of 1,000 sample means.
hist(xbar5, xlim = c(2,10))

# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$name_letters)/sqrt(5)

#Repeat for samples of size n=15
xbar15 <- rep(NA,1000)
for (i in 1:1000) {
  x <- sample(survey$name_letters, size = 15)
  xbar15[i] <- mean(x)
}
xbar15
hist(xbar15, xlim = c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)

#Repeat for samples of size n=25
xbar25 <- rep(NA,1000)
for (i in 1:1000) {
  x <- sample(survey$name_letters, size = 25)
  xbar25[i] <- mean(x)
}
xbar25
hist(xbar25, xlim = c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)

mean(survey$name_letters)
sd(survey$name_letters)/sqrt(25)

#-----------------------START OF LAB-------------------------
# Compare the sample statistics:  
# Draw 1,000 samples of size n=5 from the population data.  Calculate the mean of each sample. 
# Graph these 1,000 sample means in a histogram and examine the shape.
# Calculate the mean and standard deviation of the sampling distribution.
# Repeat this process for samples of size n=15 and n=25.
# Compare the results you get to the predictions of the Central Limit Theorem.

samp5 <- rep(NA,1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 5)
  samp5[i] <- mean(x)
}
samp5
hist(samp5, xlim = c(60,95))
mean(samp5)
sd(samp5)

samp15 <- rep(NA,1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 15)
  samp15[i] <- mean(x)
}
samp15
hist(samp15, xlim = c(60,95))
mean(samp15)
sd(samp15)

samp25 <- rep(NA,1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 25)
  samp25[i] <- mean(x)
}
samp25
hist(samp25, xlim = c(60,95))
mean(samp25)
sd(samp25)

mean(survey$happy)
sd(survey$happy)/sqrt(25)

#---------START OF PROBLEM SET---------------#

hist(survey$austin)
mean(survey$austin)

aust10 <- rep(NA,1000)
for (i in 1:1000) {
  x <-sample(survey$austin, size = 10)
  aust10[i] <- mean(x)
}
hist(aust10)
mean(aust10)
sd(aust10)
