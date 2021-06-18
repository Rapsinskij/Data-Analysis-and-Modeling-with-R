#read data from yelp.csv and store in variable 'd'
d <- read.table("C:\\Users\\Jason\\Desktop\\CS 373\\hw1\\yelp.csv", sep = ",", header = TRUE, 
                        quote = "\"", comment.char="")

#3.a print a summary of the data using summary()
summary(d)
#3.b print names of the columns using names()
names(d)

#4A.a plot a histogram of checkin data
checkinData <- d$checkins
hist(checkinData, main="Histogram of Checkin Data")

#4A.b plot a logarithmic histogram of checkin data
logCheckins <- log(d$checkins)
hist(logCheckins, main="Logarithmic Histogram of Checkin Data")

#4A.c create a density plot of checkin data
checkinDensity <- density(checkinData)
plot(checkinDensity, main = "Density plot of Checkin Data")

#4B.a plot a logarithmic histogram of checkin data with breaks=50
hist(logCheckins, main = "Logarithmic Histogram of Checkin Data with 50 breaks",
     breaks=50)

#4B.b create a density plot of checkin data with adjust = 0.5
checkinDensity <- density(checkinData, adjust=0.5)
plot(checkinDensity, main = "Density plot of Checkin Data with adjust=0.5")

#4B.d plot a logarithmic histogram of checkin data with breaks=50
#   and freq = FALSE
hist(logCheckins, main = "Logarithmic Histogram of Checkin Data with 50 breaks
      and freq=FALSE",
     breaks=50, freq=FALSE)

#4C create a frequency barplot for the state attribute
stateData <- table(d$state)
stateNames <- names(stateData)
barplot(stateData, main = "Barplot of State Frequency", names.arg = stateNames)

#5A.a transform alcohol and noiseLevel to ordered numeric features
#   by using factor()
alcoholLevels <-factor(d$alcohol, 
                       levels=c("full_bar", "beer_and_wine", "none", ""))
alcoholInts <- as.integer(alcoholLevels)

noiseLevels <- factor(d$noiseLevel, 
                      levels=c("quiet", "average", "loud", "very_loud", ""))
noiseInts <- as.integer(noiseLevels)

#5A.b append new columns to the original table
d <- cbind(d, alcoholInts)
d <- cbind(d, noiseInts)

#5B.a compute quantiles for the reviewCount attribute
quantile(d$reviewCount)
#5B.b create a subset that is the first quantile of reviewCount
firstQuantile <- subset(d, d$reviewCount <=8)
#5B.c create a summary of the first quantile and compare 
summary(firstQuantile)

#6A.a create a scatterplot with the following attributes
#     stars, reviewCount, checkins, longitude, and latitude
scatterData <- data.frame(d$stars, d$reviewCount, d$checkins, 
                         d$longitude, d$latitude)
pairs(~ d$stars + d$reviewCount + d$checkins + d$longitude + 
        d$latitude, data= scatterData)

#6B.a use cor() to calculate the pairwise correlation for all pairs
#   I couldn't get a cleaner implementation to work
cor(d$stars, d$reviewCount)
cor(d$stars, d$checkins)
cor(d$stars, d$longitude)
cor(d$stars, d$latitude)
cor(d$reviewCount, d$checkins)
cor(d$reviewCount, d$longitude)
cor(d$reviewCount, d$latitude)
cor(d$checkins, d$longitude)
cor(d$checkins, d$latitude)
cor(d$longitude, d$latitude)

#6C.a use boxplot() to model checkins, reviewcount, longitude, and latitude vs.
#     the 'goodforgroups' attribute
good<- subset(d, d$goodForGroups==TRUE)
notGood <- subset(d, d$goodForGroups==FALSE)
boxplot(good$checkins, notGood$checkins, 
        main = "Good for Groups or Not vs Checkin Frequency",
        names = c("Good for Groups","Not Good for Groups"),
        ylab = "Frequency")
boxplot(good$reviewCount, notGood$reviewCount, 
        main = "Good for Groups or Not vs Review Count",
        names = c("Good for Groups","Not Good for Groups"),
        ylab = "Frequency")
boxplot(good$longitude, notGood$longitude, 
        main = "Good for Groups or Not vs Longitude",
        names = c("Good for Groups","Not Good for Groups"),
        ylab = "Frequency")
boxplot(good$latitude, notGood$latitude, 
        main = "Good for Groups or Not vs Latitude",
        names = c("Good for Groups","Not Good for Groups"),
        ylab = "Frequency")

#6C.c Checking interquartile ranges for checkins, reviewCount, Longitude, and
#     Latitude for 'goodforgroups'=TRUE and FALSE
quantile(good$checkins)
quantile(notGood$checkins)
quantile(good$reviewCount)
quantile(notGood$reviewCount)
quantile(good$longitude)
quantile(notGood$longitude)
quantile(good$latitude)
quantile(notGood$latitude)

#7A compare attire with presence of goodForKids after excluding null options
dKids <- subset(d, d$goodForKids!="")

uAttire <- subset(dKids, dKids$attire=="")
uAttireKids <- subset(uAttire, uAttire$goodForKids==TRUE)
uProb <- 100*as.double(nrow(uAttireKids))/as.double(nrow(uAttire))

cAttire <- subset(dKids, dKids$attire=="casual")
cAttireKids <- subset(cAttire, cAttire$goodForKids==TRUE)
cProb <- 100*as.double(nrow(cAttireKids))/as.double(nrow(cAttire))

dAttire <- subset(dKids, dKids$attire=="dressy")
dAttireKids <- subset(dAttire, dAttire$goodForKids==TRUE)
dProb <- 100*as.double(nrow(dAttireKids))/as.double(nrow(dAttire))

fAttire <- subset(dKids, dKids$attire=="formal")
fAttireKids <- subset(fAttire, fAttire$goodForKids==TRUE)
fProb <- 100*as.double(nrow(fAttireKids))/as.double(nrow(fAttire))

data <- table(c(uProb, cProb, dProb, fProb))
dataNames <- c("Unspecified", "Casual", "Dressy", "Formal")
barplot(c(uProb, cProb, dProb, fProb), 
        main = "Barplot of Percent of restaurants that are Good For Kids 
        vs Resturant Dress Codes",
        names.arg = dataNames,
        ylab = "Percent of Restaurants")

#7B compare attire with reviewCount
badKid <- subset(d, d$goodForKids==FALSE)
unspecifiedKid <- subset(d, is.na(d$goodForKids))
goodKid <- subset(d, d$goodForKids==TRUE)

boxplot(badKid$stars, unspecifiedKid$stars, goodKid$stars, 
        main = "Stars vs Kid-Friendly Restaurants",
        names = c("Bad for Kids","Unspecified", "Good For Kids"),
        ylab = "Stars")
