#_________ LAB QUESTIONS _________#
#Subset the data for male artists only#
acl <- AustinCityLimits
male <- acl[acl$Gender=='M',]

#Marginal distrubution for Genre and Grammy
table(male$Genre)
table(male$Grammy) #How many male artists won a Grammy? How many male artists did not win a Grammy?

#Contingency Table
twoway  <- table(male$Genre, male$Grammy)
twoway

#Make a bar chart to better visualize how many artists in each Genre received a Grammy.
barplot(twoway, legend=T, beside=T)

#Which genre had the greatest number of Grammy wins?
male_grammy <- male[male$Grammy=='Y',]
table(male_grammy$Genre)

#Marginal Probability
prop.table(table(male$Genre))
prop.table(table(male$Grammy))

#Probability of winning Grammy given the genre
prop.table(twoway,1)


#__________ PROBLEM SET ____________#
# subset the data for popular artists (100k likes or more)
popular <- acl[acl$Facebook.100k==1,]

# distribution of popular artists by age group
barplot(table(popular$Age.Group))

#Contingency Table
twoway2 <- table(acl$Facebook.100k,acl$Age.Group)
twoway2
prop.table(twoway2,2)
