#reference: chapter 4 - relationships between continuous variables

cust.df <- read.csv("http://goo.gl/PmPkaG") # load data file 
str(cust.df)
summary(cust.df)

cust.df$cust.id <- factor(cust.df$cust.id) # id is not a number (integer), but a factor!
str(cust.df)
summary(cust.df)

# Basic scatterplot

plot(x=cust.df$age, y=cust.df$credit.score) #produces a quick plot, useful when exploring data for yourself

plot(cust.df$age, cust.df$credit.score,                             #creating a nice looking plot by adding arguments
     col="blue",                                                    #adding color to the datapoints
     xlim=c(15, 55), ylim=c(500, 900),                              #rescale x- and y- axis
     main="Active Customers as of June 2014",                       #adding a title
     xlab="Customer Age (years)", ylab="Customer Credit Score ")    #labeling the axes
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted") #adds a horizontal line at the mean credit score
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")          #adds a vertical line at the mean age
#other information you might want to add: points(), lines(), legend()

#plot produces low-level graphics to visualize the data, depending on the data type
plot(cust.df$email) #email is coded as factor!


#Question: Do customers who buy more online buy less in stores?

#starting with a scatterplot
plot(cust.df$store.spend, cust.df$online.spend, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.7)                                              #rescales the symbol size
#relationship?

hist(cust.df$store.spend, 
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     main="Customers as of June 2014", 
     xlab="Prior 12 months online sales ($)", 
     ylab="Count of customers")
#problem of skewed data - typical distribution of spending and transaction counts in customer data


#Question: Propensity to buy online vs. in store related to email efforts?
#i.e. adding another dimension (information email yes/no) to the plot
#Using color-coding - black, open circles for "email: no"; green, solid circles for "email: yes"

my.col <- c("black", "green3") 
my.pch <- c(1, 19) # R's symbols for solid and open circles (see ?points)

head(cust.df$email)                          #email is a factor!
as.numeric(head(cust.df$email))              #transformation to numeric 
my.col[as.numeric(head(cust.df$email))]      #using these numbers as index, we get the matching colors
my.col[head(cust.df$email)]                  #R understands what we want and converts the factor to numbers itself

#simple color coding
plot(cust.df$store.spend, cust.df$online.spend,
     col=(cust.df$email))                       #what would happen if we define col=("black","red")?

#creating a nice looking plot, using color coding
plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email], pch=my.pch[cust.df$email],         #col:  black vs green ; pch: open vs solid circle
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )

#adding a legend: X=position, info by "legend="; paste combines text/numbers/...
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
       col=my.col, pch=my.pch)
#not linked to dataset, be careful!

#Still hard to interpret, because of skewed data
#Possible solution: Plotting on a log-scale

plot(cust.df$store.spend+1, cust.df$online.spend+1,      #'+1' prevents an error message / omitted data
     log="xy", cex=0.7,                                  # log="x";log="y";log="xy" for log-scaled axes
     col=my.col[cust.df$email], pch=my.pch[cust.df$email],
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
       col=my.col, pch=my.pch)

#So: Do customers who buy more online buy less in stores? Does emailing have an impact?


# Combining plots in a single graphics object: Multi-panel plots
par(mfrow=c(2, 2))                                                     #begins in row 1 and moves from left to right
plot(cust.df$distance.to.store, cust.df$store.spend, main="store")
plot(cust.df$distance.to.store, cust.df$online.spend, main="online")
plot(cust.df$distance.to.store, cust.df$store.spend+1, log="xy", 
     main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend+1, log="xy", 
     main="online, log")

#Of course you can "upgrade" each plot individually by all arguments that work for a single plot
# Any relationships observable?

par(mfrow=c(1, 1))       #setting R back to single plot layout


#scatterplot matrix

# Using a scatterplot matrix to analyse relationships within your dataset for all possible combinations
# pairs creates scatterplot. formula allows you to transform variables (e.g. "log(online.spend)
# Variables after the "~" symbol will be plotted. Variables have to be separated by the "+".
pairs(formula = ~ age + credit.score + email +
                  distance.to.store + online.visits + online.trans + 
                  online.spend + store.trans + store.spend,
      data=cust.df)                 #specifying the data here allows you to list the column names only

pairs(cust.df[ , c(2:10)])  #creates the same plot - shorter, but does not allow transformations and is less robust

#What (obvious) relationsships do we see?


#Other packagaes offer different visualizations containing additional information

# scatterplotMatrix() - car package (Companion to Applied Regression). 
# Adds smoothed line and univariate histograms/density plots on the diagonal

library(car)   # install if needed (Tools->Install packages->search for "car")

scatterplotMatrix(formula = ~ age + credit.score + email +
                    distance.to.store + online.visits + online.trans + 
                    online.spend + store.trans + store.spend, 
                  data=cust.df, diagonal="histogram")             #specify here what should be provided on diagonal

# warnings because email cannot be smoothed - Discrete variables not ideally visualized by scatterplots
# green line shows linear fit; red line shows smoothed fit and confidence interval
# Interpretation?


# gpairs() - Generalized Pair Plots
# Allows better visualization for discrete and continuous variables 

# install.packages("gpairs")  # only run once
library(gpairs)
gpairs(cust.df[ , c(2:10)])


# Scatterplots are useful for first inspections, but for detailed analyses you need correlation coefficients

# Correlations
cov(cust.df$age, cust.df$credit.score)   #depends on the scale of the variables

cor(cust.df$age, cust.df$credit.score)   #r=[-1,1]

cov(cust.df$age, cust.df$credit.score)/
  (sd(cust.df$age)*sd(cust.df$credit.score))   #standardization results in same output

# What is a significant r?
# in physics or medicine: 0.9 or even 0.99+ required
# Cohen's Rule of Thumb (psychology): 0.1=small/weak ; 0.3=medium ; 0.5+=large/strong
# But: Only true for normally distributed variables!

#Is it statistically significant?
cor.test(cust.df$age, cust.df$credit.score)
# confidence interval does NOT include the 0 - so there is a statistically significant relationship (p-value<0.05)


# To  calculate all possible correlation coefficients we produce a correlation matrix
cor(cust.df[, c(2, 3, 5:12)])
# Why do we exclude email here?
# NA's in sat.service and sat.selection -> no correlation coefficient is calculated
# interpretation?


# only use observations with full information set
cor(cust.df[,c(2, 3, 5:12)], use="complete.obs")


# graphics help with the scanning of the matrix
library(corrplot)    # for correlation plot
library(gplots)      # for color interpolation

# NOTE: if using RStudio, it can be helpful to "Clear All" plots if one
# appears too small or too large; 
# this is a remnant of par(mfrow=..) settings as above and similar settings

corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),
               upper="ellipse", tl.pos="lt", 
               upper.col = colorpanel(50, "red", "gray60", "blue4"))
#try with different settings and arguments

# Important sidenote: Correlation does not imply causation! 
# see e.g. http://www.tylervigen.com/spurious-correlations


# Correlation coefficients only true for linear relationships 

set.seed(49931)                      #to reproduce random values
x <- runif(1000, min=-10, max=10)    # creates a uniform distribution of 1000 observations between -10 and 10
cor(x, x^2)                          
# correlation is 0, but we have a perfect (but non-linear) relationship
plot(x,x^2)


# Problem: Many relationships in marketing are non-linear --> Transformations needed

# linear correlation between distance to store and store spendings
cor(cust.df$distance.to.store, cust.df$store.spend)    
# using the inverse gives a stronger relationship
cor(1/cust.df$distance.to.store, cust.df$store.spend)
# using the square root an even stronger association
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)  # interpretation?


# transformations should be made before making scatterplots
par(mfrow = c(1,2))
plot(cust.df$distance.to.store, cust.df$store.spend)            #the 'original' plot
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)    #the plot, using a more plausible association
par(mfrow = c(1,1))

# so what is the 'correct' transformation (see slides)?

# Box-Cox transformation
# of course you do not try to find an optimal lambda by hand
library(car)
powerTransform(1/cust.df$distance.to.store) #gives us the best lambda to make our distance normally distributed

lambda <- coef(powerTransform(1/cust.df$distance.to.store))  #coef allows us to save just the number
bcPower(cust.df$distance.to.store, lambda)                   #creates the transformed variable

#comparing the results between transformed and untransformed variable by histograms
par(mfrow=c(1,2))
hist(cust.df$distance.to.store, 
     xlab="Distance to Nearest Store", ylab="Count of Customers", 
     main="Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda),
     xlab="Box-Cox Transform of Distance", ylab="Count of Customers", 
     main="Transformed Distribution")

powerTransform(cust.df$age) #age is already nearly normally distributed -> lambda close to 1
lambda <- coef(powerTransform(cust.df$age))

hist(cust.df$age, xlab="Age", ylab="Count of Customers", 
     main="Original Distribution")
hist(bcPower(cust.df$age, lambda),
     xlab="Box-Cox Transform of Age", ylab="Count of Customers", 
     main="Transformed Distribution")
par(mfrow = c(1,1))

#once we have ensured normally distributed variables, we can compute the correlation coefficients

l.dist  <- coef(powerTransform(cust.df$distance.to.store))  #transforming distance to store (lambda)
l.spend <- coef(powerTransform(cust.df$store.spend+1))      #transforming store spendings (lambda)

cor(bcPower(cust.df$distance.to.store, l.dist), 
    bcPower(cust.df$store.spend+1, l.spend))                #correlation between transformed variables

#again: you should do these transformations before calculating the correlations


#so far we analysed mainly continuous variables - what about ordinal scaled variables?
# Comparison of customer satisfaction with service and product selection
#start with univariate analysis - e.g. by boxplots
boxplot(cust.df$sat.service,main="Customer Satisfaction with Service")
boxplot(cust.df$sat.selection,main="Customer Satisfaction with Selection")

#creating a scatterplot
plot(cust.df$sat.service, cust.df$sat.selection, 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")
#problem?

#jitter adds noise variance to each data point
plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection), 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")

#for detailed analyses of relationships between ordinally scaled variables use  e.g. polychoric() or baysem()


