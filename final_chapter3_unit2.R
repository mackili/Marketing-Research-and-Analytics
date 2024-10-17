store.df <- read.csv("http://goo.gl/QPDdMl")  # load data file 
summary(store.df)


############
# descriptives

#discrete variables 

#A basic way to describe discrete data is with frequency counts. 
#table() counts the observed prevalence of each value that occurs in a variable (vector or column in a data frame)

#How many times, Product 1 was on sale at which price 
table(store.df$p1price)

#most functions produce objects that can be stored for further use
p1.table <- table(store.df$p1price)
p1.table

#str shows that the object is a special type called table
str(p1.table)

#data can be passed to plot()
plot(p1.table)

#how often product 1 was promoted at each price?

#The function table() productes two-way cross tabs when a second variable is included:

table(store.df$p1price, store.df$p1prom)

plot(table(store.df$p1price, store.df$p1prom))


#fraction of times product 1 is on promotion at each price point
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])

#continuous variables

# functions

#extremes
min(store.df$p1sales)
max(store.df$p2sales)

#central dendency
mean(store.df$p1prom) #arithmetic mean
median(store.df$p2sales)

#dispersion
var(store.df$p1sales) #variance around mean
sd(store.df$p1sales) #standard deviation (sqrt(var(x)))
IQR(store.df$p1sales) #interquartile range, 75th-25th percentile
mad(store.df$p1sales) #median absolute deviation (a robust variance estimator)

quantile(store.df$p1sales, probs=c(0.25,0.50,0.75))   

quantile(store.df$p1sales, probs=c(0.05, 0.95))  # central 90%
quantile(store.df$p1sales, probs=0:10/10)  #to find every 10th percentile, a sequence can be used

## Build a summary
##Summary of sales for product 1 and 2 based on median and interquartile range
##into a data frame that is easier to read


mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

#there a various built in summary functions:

#basic summary()
#describe from package psych
#apply()


## Descriptive summaries
summary(store.df)
summary(store.df$Year) # also works for single vectors
summary(store.df, digits=2) #digits=2 means 2 significant positions, not 2 decimal places

install.packages("psych")  #only run once
library(psych)

#describe() reports a variety of statistics for each variable
#especially useful for summarizing Likert type data 

describe(store.df)
describe(store.df[ , c(2, 4:9)])


############
# apply()

#apply(x=DATA, MARGIN=MARGIN, FUN=FUNCTION)
#along rows-->MARGIN=1
#along columns -->MARGIN=2

#some examples
apply(store.df[, 2:9], MARGIN=2, FUN=mean)
apply(store.df[, 2:9], 1, mean)
apply(store.df[, 2:9], 2, sum)
apply(store.df[, 2:9], 2, sd)
apply(store.df[, 2:9], 2, function(x) { mean(x) - median(x) } )

## creating a summary data frame using apply()

mysummary2.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary2.df) <- c("Median Sales", "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5] # names from the data frame
mysummary2.df[, "Median Sales"] <- apply(store.df[, 4:5], 2, median)
mysummary2.df[, "IQR"]          <- apply(store.df[, 4:5], 2, IQR)
mysummary2.df

############
# single variable visualization

#histograms

hist(store.df$p1sales) # basic version

## prettier version

#improvement 1: title and axis labels

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",    # add labels
     xlab="Product 1 Sales (Units)",
     ylab="Count" )           

#improvement 2: change of bin-size("breaks=") and color
#hint: available colors can been found using colors()

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,             # more columns 
     col="lightblue")       # color the bars


# improvement 3: relativ instead of absolute frequency (makes plot comparable with different samples sizes)  
# replacement of x-axis

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",             # it's no longer the count!
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want; side=1 for x, side=2 for y, side=3 for top axis, side=4 for right axis


# improvement 4: adding curves to the histogram
# density() estimates density values for a vector, 
# lines() adds elements to the current plot


hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want


lines(density(store.df$p1sales, bw=20),    # "bw= ..." adjusts the smoothing
      type="l", col="darkred", lwd=2)      # lwd = line width


############
# boxplots and stripcharts

## boxplot

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)


#extreme values at 1.5*times the width of the box|--------25thpercentile|__|75hpercentile------------|extreme values at 1.5*times the with of the box 


#boxplots are useful to compare distributions by some other factor.

#How do different stores compare on sales of product 2?
#Note: Tilde (~) separates dependent variable from explanatory variable

boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
     ylab="Store", xlab="Weekly unit sales", las=1,           #las=1 forces axes to have text in horizontal direction
     main="Weekly Sales of P2 by Store")


# Do Sales differ in relation to in-store promotion?
# using data=...

boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
     ylab="P2 promoted in store?", xlab="Weekly sales",
     main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))



### qq check for normality
# graphical method to evaluate a distribution for normality
# Many common statistics (e.g., correlation coefficient) are interpreted under the assumption that 
# the data is normally distributed 

#Check if the sales of P1 are normally distributed

qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

#If models assume normally distributed data, transforming the data might help.
#e.g., logarithmic distributions are often used in marketing

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

#another useful univariate plot:
### empirical cumulative distribution function (ecdf) plot

plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab="Cumulative Proportion",
     xlab=c("P1 weekly sales, all stores", "90% of weeks sold <= 171 units"),
     yaxt="n")
axis(side=2, at=seq(0, 1, by=0.1), las=1, 
    labels=paste(seq(0,100,by=10), "%", sep=""))
#note: paste() concatenates vectors after converting to character 

# add lines for 90%
# abline() adds a straight line to a plot
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)

#Two ways of break out data by factors and summarize it
### by() and aggregate()

#What are the mean sales of P1 by store?
#by(data=DATA, INDICES=INDICES, FUN=FUNCTION)

by(store.df$p1sales, store.df$storeNum, mean)


#what are the mean sales of P1 by store and year

by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

#output of by() not well suited for reuse
#aggregate() returns a nicely formatted data frame

#What is the sum of sales of P1 by country

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)



##########
# How to plot data on a  
# world map 

# aggregate the average per-store sales by country
p1sales.sum <- aggregate(store.df$p1sales, 
                         by=list(country=store.df$country), sum)

install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)

# create map
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2", 
                                   nameJoinColumn = "country")

mapCountryData(p1sales.map, nameColumnToPlot="x", 
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"), 
               catMethod="fixedWidth", addLegend=TRUE)


