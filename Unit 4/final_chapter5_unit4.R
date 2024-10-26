#### load the data
seg.df <- read.csv("https://goo.gl/qw303p",stringsAsFactors = TRUE)
summary(seg.df)



# descriptives

# descriptive statistics for a specific segment can be obtained by indexing

#mean income for segment "moving up"
mean(seg.df$income[seg.df$Segment == "Moving up"])

#mean income for segment "moving up" who do not subscribe

mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe=="subNo"])

#more efficient for multiple groups:
#by(data, INDICES,FUN)
#by divides data into groups for each unique value in indices and applies FUN to each group

by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

#better alternative with structured output (data frame), that can be used for further operations
#note: needs a list for even a single factor

aggregate(seg.df$income, list(seg.df$Segment), mean)

#we will use this output to add another variable to our data set that contains the segment mean:

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]


library(car)
some(seg.df)

#How did this work?
seg.df$Segment
seg.income.mean[seg.df$Segment, ]
seg.income.mean[seg.df$Segment, 2]

# clear that variable
seg.df$segIncome <- NULL


#### formula version
#R provides a standard way to describe relationships among
# variables through formula specification.
#
#A formula uses the tilde (~) operator to separate response variables on the left
#from explanatory variables on the right. We will use this in different applications

aggregate(income ~ Segment, data=seg.df, mean)

##########

#Descriptives for two-way groups


#Cross -tabulation is a common task in marketing, e.g., separating customers
#into groups according to two or more factors
# two-way data aggregation
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)

#Resulting output can be assigned to a data frame object and indexed
agg.data <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
agg.data[2, ]
agg.data[2, 3]


# Count of factor level occurence by factor
# can be obtained by using the table expression
# with() evaluates the expression in a data environment
# same as table(seg.df$Segment, seg.df$ownHome)

with(seg.df, table(Segment, ownHome))

#suppose we want a breakdown of the number of kids in each household (kids)
#by segment. Here kids will be treated as factor and not as a number

with(seg.df, table(kids, Segment))

# To obtain the total number of kids in each segment:

#alternative 1:

aggregate(kids ~ Segment, data=seg.df, sum)

#alternative 2:
seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)

#alternative 3:
seg.tab <- with(seg.df, table(kids, Segment))
colSums(seg.tab*0:7)


#### visualize counts by group

#We want to plot the proportion of subscribers for each segment 
#to understand which segments use the subscription service

library(lattice)


# histogram by 1 factor
histogram(~subscribe | Segment, data=seg.df)

#histogram understands formula notation including conditioning on a factor, i.e.,
#the plot is separated into multiple panes.
#Note that there is no response variable before the tilde (~)


# the get counts instead of proportions, and some visual options
histogram(~subscribe | Segment, data=seg.df, type="count", 
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))

# We can condition on more than one factor. Factors can be added in the conditiong part 
#of the formula with "+"
# histogram by 2 factors
histogram(~subscribe | Segment + ownHome, data=seg.df)


# use prop.table to get just "yes" proportion
# prop.table give the proportions for each cell with respect to the entire table (default)
# or just rows (margin=1) or colums (margin=2)

prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)

barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ], 
          xlab="Subscriber proportion by Segment", col="darkolivegreen")

barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ], 
         xlab="Subscriber proportion by Segment", col="darkolivegreen", xlim=(c(0,.25)))


#### visualize continuous data by group
# e.g., how can we plot income by segment?

## bar chart for continuous variable, the "spreadsheet" way to graph it
# aggregate our data
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
# plot it
library(lattice)

barchart(income~Segment, data=seg.mean, col="grey")

# How do we split this out further by home ownership?

#data aggregation
seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
# then plot it
# groups= to add the grouping variable 
# auto.key produces legend for grouping variable
# some color improvements

barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=rainbow(2)))   # try rainbow, topo.colors, heat.colors, cm.colors


barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=cm.colors(2)))   # try rainbow, topo.colors, heat.colors, cm.colors


barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=terrain.colors(2)))   # try rainbow, topo.colors, heat.colors, cm.colors


#more informative plot for comparing values of continuous data is a box-and-whiskers plot or boxplot

# base graphics way to do this

boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)


# even better option for box-and-whiskers plot is bwplot() 
# gives more options, especially for multiway breakouts ("conditioning")
# Note: opposite direction in formula than usual "Segment ~ income"

library(lattice)

bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")

# add conditioning variable
bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, 
       xlab="Income")



##### Statistical models and tests
### chi-square tests

#a simple example

#creating sample data

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)

#interpretation: evaluates the likelihood of seeing such a result under the
#null hypothesis that the data were randomly sampled from a large population where the values
#1-4 are equally distributed
#85.2% chance of seeing the result if the null hypothesis is true -->data suggest no real differences
#in frequency between the four cells

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)

#--> reject null hypothesis with 95% confidence

#test is also sensitive to sample size
#e.g.:

tmp.tab <- tmp.tab/5
tmp.tab
chisq.test(tmp.tab)

#return to data set

# one way chi-square in our data
chisq.test(table(seg.df$Segment))



#Is subscription status independent from home ownership?
# two-way chi-square
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# two-way chi-square without correction (matches traditional formula)
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=FALSE)

# two-way with simulation to establish p value
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=TRUE, B=100000)

#Testing of proportions
#if observations have only two possible values, we can consider them as a binomial variable

#Consider we observe 12 times event A and 8 times event B, can we conclude that this differs significantly 
#from an equal representation of events (50/50)?

### binom.test(successes, trials, probability): probability is the true likelihood to test for

binom.test(12, 20, p=0.5)
#p-value >0.05 or 50% value within confidence intervall -->null hypothsis cannot be rejected

#same proportion, but larger sample

binom.test(120, 200, p=0.5)

#-->significant!


### t-test
#Compares the mean of one sample against the mean of another sample (or against a specific value, e.g., 0)
#Question: Is household income different among those who own a home and those who do not?

#Many tests assume that the data follows a normal distribution.
#Skewness or outliers violate those assumptions and might lead to an inaccurate test

hist(seg.df$income)
with(seg.df, hist(income[ownHome=="ownYes"]))
with(seg.df, hist(income[ownHome=="ownNo"]))

# then the t-tests
t.test(income ~ ownHome, data=seg.df)

#p-value of 0.001195 --> null hypothesis (no difference in income) can be rejected
#95% confidence intervall for the difference between -3007 and -12080, i.e.,
#if data is representative for a larger population, we have 95% confidence that the group difference is
#between those values

#is there a significan difference in the subset of travellers?

t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))

#we saw a significant difference in income based on home ownership, but no significant difference within Travellers.
#How can we locate where the difference lies --> testing of multiple group means using ANOVA


### ANOVA
seg.aov.own <- aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)

seg.aov.seg <- aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

# two-way aov
anova(aov(income ~ Segment + ownHome, data=seg.df)) #main effects only

#Could it be that home ownership is related to income in some segments but not in others? -->Interaction effect
anova(aov(income ~ Segment * ownHome, data=seg.df)) #main effects and interactions


### Visualize ANOVA group means

install.packages("multcomp")
library(multcomp)

# create an aov model. 
seg.aov <- aov(income ~ Segment, data=seg.df)

#problem for glht() plotting because of the intercept term. Intercept term corresponds
#to Moving up Segment. Difficult for decision makers
glht(seg.aov)

# make new AOV without intercept, as a *convenience for plotting* (not for modeling)
# it helps with plotting because it keeps all segments on same scale
seg.aov <- aov(income ~ -1 + Segment, data=seg.df)

glht(seg.aov)

par(mar=c(6,10,2,2))   # adjusts margins to preserve axis labels
plot(glht(seg.aov), 
     xlab="Income", main="Average Income by Segment (95% CI)")


