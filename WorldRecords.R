########## START OF PRELAB ##########
#Primary Research Question: How has the men’s shotput world record changed over time?  What about the women’s world record?

#Subset the data
menshot <- WR[WR$Event=='Mens Shotput',]
womenshot <- WR[WR$Event=='Womens Shotput',] 

#Create scatterplots
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)

########## START OF LAB ##########
#Create a subset of the data that contains World Record cases for the men’s and women's Mile event.
mensmile <- WR[WR$Event=='Mens Mile',]
womensmile <- WR[WR$Event == 'Womens Mile',]

#Create a scatterplot  
plot(mensmile$Year,mensmile$Record, xlab = 'Year', ylab = 'Record in Seconds', main = 'World Record Time for Men\'s Mile Event')
plot(womensmile$Year,womensmile$Record, xlab = 'Year', ylab = 'Record in Seconds', main = "World Record Time for Women\' Mile Event")

#Run a linear model for each event 
linFit(mensmile$Year, mensmile$Record)
linFit(womensmile$Year, womensmile$Record)

########## START OF PROBLEM SET ##########
#Create a new dataframe taht contains  the world record cases in the men's pole vault event in years 1970 and later. 
menspole <- WR[WR$Event=='Mens Polevault',]
menspole70 <- menspole[menspole$Year>=1970,]

#What is the standing world record height (in meters) for men's pole vault?
max(menspole$Record)

#Create a scatterplot showing the men's pole vault records since 1970 as a function of year. Fit a linear model to the data.
plot(menspole70$Year, menspole70$Record, xlab = 'Year', ylab = 'Record in Meters')
linFit(menspole70$Year, menspole70$Record)
