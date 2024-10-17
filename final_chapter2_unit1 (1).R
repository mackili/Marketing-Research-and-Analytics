# Language Fundamentals
x <- c(2, 4, 6, 8)   # this is a comment; x gets values 2,4,6,8
x

# R differentiates between data and functions
# basic data objects in R: vectors, lists, data frames 

# vectors are a one-dimensional collection of data points of a similar kind (numbers, text,..)
# c() indicates that you are entering elements of a vector

xNum  <- c(1, 3.14159, 5, 7)
xLog  <- c(TRUE, FALSE, TRUE, TRUE)
xChar <- c("foo", "bar", "boo", "far")
xMix  <- c(1, TRUE, 3, "Hello, world!") 
xNum

#vectors can be appended to one another with c()

x2 <- c(x, x)
x2

#overall view of an object can be obtained with summary() functions,
#results depend on the object type

summary(xNum)
summary(xChar)

#"Indexing" denotes particular elements of a data structure.
#Vectors are indexed with square brackets []

xNum[2]

# R applies operators across entire vectors

x2 + 1 
x2 * pi 

#Caution: R recycles the elements to match a longer set. Check with length() 

(x+cos(0.5)) * x2  

length(x)
length(x2)

# when a vector is created, R automatically assigns a data type or class (e.g., logical, integer, double, character)
# in case of "mixed" values, it takes the most general format

c(1, 2, 3.5) 
xMix


xNum[1]
xMix[1]
xNum[1] + 1

xMix[1] + 1    # error

#we can force variables to be numeric
as.numeric(xMix[1])+1

#many variable types can be coerced to another.
#information about the data type can be obtained by using str()


str(xNum)
str(xChar)
str(xMix)

# to find help, use ? and ??
?as.numeric
??anova


#integer sequences can be created with the : operator

xSeq <- 1:10
xSeq

#caution: be careful of operator precedence
#":" is applied before many other operations

1:5*2

#use parentheses, when in doubt
1:(5*2)

#sequences can be used for indexing inside []
xNum
xNum[2:4]
myStart <- 2
xNum[myStart:sqrt(myStart+7)]

#more complex sequences can be generated using "seq" and "rep"

seq(from=-5, to=28, by=4)
rep(c(1,2,3), each=3)
rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1))

#items can be excluded by using negativ indices

xSeq
xSeq[-5:-7]

#result of an vector operation is itself a vector (can be reused)
xNum[2:4]
xSub <- xNum[2:4]
xSub

#indexing also works with a vector of logical variables 
xNum
xNum[c(FALSE, TRUE, TRUE, TRUE)]

xNum[xNum > 3]


# missing and interesting values
# missing values are important in statistics and a special constant is used: NA

# can be used to create an object where values are are filled in later
my.test.scores <- c(91, NA, NA)

#mathematical operations on a value of NA become NA
mean(my.test.scores)
max(my.test.scores)

#3 ways to omit NAs

#1
#many commands include an argument to ignore missing values: na.rm=TRUE

mean(my.test.scores, na.rm=TRUE)
max(my.test.scores, na.rm=TRUE)

#2
#na.omit() removes NA values
mean(na.omit(my.test.scores))

#3
#is.na() tests for NA
is.na(my.test.scores)
my.test.scores[!is.na(my.test.scores)]

#Caution: Using -999 or similar numbers should be avoided. 
#Commonly used in data sets, replace after loading data with:

my.test.scores <- c(91, -999, -999)
mean(my.test.scores)
my.test.scores[my.test.scores < -900] <- NA
mean(my.test.scores, na.rm=TRUE)

#further important constants for Infinity and "not a number"
log(c(-1,0,1))


# lists
# lists are collections of objects of any type

#two objects with different types can be combined usding list():
str(xNum)
str(xChar)

xList <- list(xNum, xChar)
xList

#objects inside a list retain the types they had:
str(xList)

#lists are indexed with double brackets 
summary(xList[[1]])

#to run a function on all list objects, use lapply(OBJECT, FUNCTION)

#to build a summary for each element in the list:
lapply(xList, summary)

#a name can be assigned to each element in a list

#2 ways to assign the name

#1
xList <- list(xNum, xChar)
names(xList) <- c("itemnum", "itemchar")     # method 1

#2
xList <- list(itemnum=xNum, itemchar=xChar)  # method 2
names(xList)

#different possibilities to index a list

#1
xList[[1]]
#2
xList$itemnum
#3
xList[["itemnum"]]


# data frames
# most important object type to hold data sets and to provide data to statistical functions and models
# general structure: rectangular object with columns as variables and rows as observations.

#to construct a data frame, data.frame() is used. Input must be a set of vectors of the same length
x.df <- data.frame(xNum, xLog, xChar)
x.df
 
# data frames may be indexed using [ROW, COLUMN]
x.df[2,1]
x.df[1,3]

#by default, character data is converted into a nominal factor

#to prevent conversion, the option stringsAsFactors=FALSE is added to the data.frame()
x.df <- data.frame(xNum, xLog, xChar, stringsAsFactors=FALSE)
x.df[1,3]

#indexing data frames:

x.df[2, ]  # all of row 2
x.df[ ,3]  # all of column 3

x.df[2:3, ] 
x.df[ ,1:2] 
x.df[-3, ]  # omit the third observation
x.df[, -2]  # omit the second column

#indexing a data frame returns an object. Resulting data type will suit the data:

str(x.df[2,1])
str(x.df[, 2])
str(x.df[c(1,3), ])    # use c() to get rows 1 and 3 only

#data frames can also be indexed by using the column name
x.df$xNum



# create more interesting data

# warning!!
rm(list=ls())    # caution, deletes all objects; see below


store.num <- factor(c(3, 14, 21, 32, 54))   # store id
store.rev <- c(543, 654, 345, 678, 234)     # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34)       # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
(store.df <- data.frame(store.num, store.rev, store.visits,
                        store.manager, stringsAsFactors=F))  # F = FALSE
#note: putting parentheses around a whole expression leads to evaluation of the resulting object



#some basic evaluations
#note: $ notation works the same as with lists

store.df$store.manager
mean(store.df$store.rev)
cor(store.df$store.rev, store.df$store.visits)

summary(store.df)


# loading and saving data

# (1) single variables in R, (2) csv

#(1)

save(store.df, file="store-df-backup.RData")
rm(store.df)     # caution, first ensure 'save' worked
mean(store.df$store.rev)    # error
load("store-df-backup.RData")
mean(store.df$store.rev)     # works now

#multiple variables can be combined using list=c()
save(list=c("store.df","store.visits"), file="store-df-backup.RData")

#caution: loading variables overwrites exisitng with the same name without warning:
store.df <- 5
store.df
load("store-df-backup.RData")
store.df

# Works on Windows:
#save(store.df, file="C:\\Documents and Settings\\user\\My Documents\\R\\store-df-backup.RData")

# Works on Mac OSX, Linux and Windows:
#save(store.df, file="~/Documents/R/store-df-backup.RData")

#working directory can be changed using setwd()
getwd()
setwd("~/Documents/R")   # tilde is handled on UNIX-like systems, only works if directory exists
getwd()

#(2) csv files: used for moving data between R, databases, excel,...
#write.csv(OBJECT, file="FILENAME", row.names=FALSE). row.names=FALSE removes an extra, unnamed column containing labels
#for each row


#test the command without a filename to send output to screen
write.csv(store.df, row.names=FALSE)

#writing and reading:
write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")  # "file=" is optional

#assigning it to an object (data frame)
store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)  # "file=" is optional
store.df2$store.num <- factor(store.df2$store.num)

#compare old and new file

#compare by element
store.df == store.df2

#more convenient:
all.equal(store.df, store.df2)
