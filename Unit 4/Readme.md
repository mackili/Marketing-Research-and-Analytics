Unit 4
================
Maciej Kilijański

# Comparing groups: statistical tests

Loading and summarizing the data:

``` r
seg.df <- read.csv("https://goo.gl/qw303p",stringsAsFactors = TRUE)
summary(seg.df)
```

    ##       age           gender        income            kids        ownHome   
    ##  Min.   :19.26   Female:157   Min.   : -5183   Min.   :0.00   ownNo :159  
    ##  1st Qu.:33.01   Male  :143   1st Qu.: 39656   1st Qu.:0.00   ownYes:141  
    ##  Median :39.49                Median : 52014   Median :1.00               
    ##  Mean   :41.20                Mean   : 50937   Mean   :1.27               
    ##  3rd Qu.:47.90                3rd Qu.: 61403   3rd Qu.:2.00               
    ##  Max.   :80.49                Max.   :114278   Max.   :7.00               
    ##   subscribe         Segment   
    ##  subNo :260   Moving up : 70  
    ##  subYes: 40   Suburb mix:100  
    ##               Travelers : 80  
    ##               Urban hip : 50  
    ##                               
    ## 

## Descriptive statistics

> Descriptive statistics for a specific segment can be obtained by
> indexing

Mean income for segment `moving up`

``` r
mean(seg.df$income[seg.df$Segment == "Moving up"])
```

    ## [1] 53090.97

Mean income for segment `moving up` who do not subscribe

``` r
mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe=="subNo"])
```

    ## [1] 53633.73

More efficient for multiple groups: `by(data, INDICES,FUN)` by divides
data into groups for each unique value in indices and applies FUN to
each group

``` r
by(seg.df$income, seg.df$Segment, mean)
```

    ## seg.df$Segment: Moving up
    ## [1] 53090.97
    ## ------------------------------------------------------------ 
    ## seg.df$Segment: Suburb mix
    ## [1] 55033.82
    ## ------------------------------------------------------------ 
    ## seg.df$Segment: Travelers
    ## [1] 62213.94
    ## ------------------------------------------------------------ 
    ## seg.df$Segment: Urban hip
    ## [1] 21681.93

``` r
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
```

    ## : Moving up
    ## : subNo
    ## [1] 53633.73
    ## ------------------------------------------------------------ 
    ## : Suburb mix
    ## : subNo
    ## [1] 54942.69
    ## ------------------------------------------------------------ 
    ## : Travelers
    ## : subNo
    ## [1] 62746.11
    ## ------------------------------------------------------------ 
    ## : Urban hip
    ## : subNo
    ## [1] 22082.11
    ## ------------------------------------------------------------ 
    ## : Moving up
    ## : subYes
    ## [1] 50919.89
    ## ------------------------------------------------------------ 
    ## : Suburb mix
    ## : subYes
    ## [1] 56461.41
    ## ------------------------------------------------------------ 
    ## : Travelers
    ## : subYes
    ## [1] 58488.77
    ## ------------------------------------------------------------ 
    ## : Urban hip
    ## : subYes
    ## [1] 20081.19

Better alternative with structured output (data frame), that can be used
for further operations

> note: needs a list for even a single factor

``` r
aggregate(seg.df$income, list(seg.df$Segment), mean)
```

    ##      Group.1        x
    ## 1  Moving up 53090.97
    ## 2 Suburb mix 55033.82
    ## 3  Travelers 62213.94
    ## 4  Urban hip 21681.93

We will use this output to add another variable to our data set that
contains the segment mean:

``` r
seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
head(seg.df)
```

    ##        age gender   income kids ownHome subscribe    Segment segIncome
    ## 1 47.31613   Male 49482.81    2   ownNo     subNo Suburb mix  55033.82
    ## 2 31.38684   Male 35546.29    1  ownYes     subNo Suburb mix  55033.82
    ## 3 43.20034   Male 44169.19    0  ownYes     subNo Suburb mix  55033.82
    ## 4 37.31700 Female 81041.99    1   ownNo     subNo Suburb mix  55033.82
    ## 5 40.95439 Female 79353.01    3  ownYes     subNo Suburb mix  55033.82
    ## 6 43.03387   Male 58143.36    4  ownYes     subNo Suburb mix  55033.82

``` r
library(car)
```

``` r
some(seg.df)
```

    ##          age gender     income kids ownHome subscribe    Segment segIncome
    ## 6   43.03387   Male 58143.3633    4  ownYes     subNo Suburb mix  55033.82
    ## 131 23.97756   Male 18106.8369    0   ownNo     subNo  Urban hip  21681.93
    ## 143 22.34075   Male 16341.0906    0   ownNo     subNo  Urban hip  21681.93
    ## 156 66.89691   Male 54060.5906    0  ownYes     subNo  Travelers  62213.94
    ## 160 60.69423   Male  -693.9989    0  ownYes     subNo  Travelers  62213.94
    ## 180 57.39567 Female 37797.9068    0  ownYes     subNo  Travelers  62213.94
    ## 209 60.56487   Male 62932.7986    0   ownNo     subNo  Travelers  62213.94
    ## 245 39.42243 Female 57265.3026    0   ownNo     subNo  Moving up  53090.97
    ## 268 34.42021 Female 49235.0167    2   ownNo     subNo  Moving up  53090.97
    ## 273 38.87477 Female 44811.7814    5  ownYes     subNo  Moving up  53090.97

How did this work?

``` r
head(seg.df$Segment)
```

    ## [1] Suburb mix Suburb mix Suburb mix Suburb mix Suburb mix Suburb mix
    ## Levels: Moving up Suburb mix Travelers Urban hip

``` r
head(seg.income.mean[seg.df$Segment, ])
```

    ##        Group.1        x
    ## 2   Suburb mix 55033.82
    ## 2.1 Suburb mix 55033.82
    ## 2.2 Suburb mix 55033.82
    ## 2.3 Suburb mix 55033.82
    ## 2.4 Suburb mix 55033.82
    ## 2.5 Suburb mix 55033.82

``` r
head(seg.income.mean[seg.df$Segment, 2])
```

    ## [1] 55033.82 55033.82 55033.82 55033.82 55033.82 55033.82

Clear that variable

``` r
seg.df$segIncome <- NULL
```

### Formula version

R provides a standard way to describe relationships among variables
through formula specification.

A formula uses the tilde (\~) operator to separate response variables on
the left from explanatory variables on the right. We will use this in
different applications

``` r
aggregate(income ~ Segment, data=seg.df, mean)
```

    ##      Segment   income
    ## 1  Moving up 53090.97
    ## 2 Suburb mix 55033.82
    ## 3  Travelers 62213.94
    ## 4  Urban hip 21681.93

### Descriptives for two-way groups

Cross -tabulation is a common task in marketing, e.g., separating
customers into groups according to two or more factors \> two-way data
aggregation

``` r
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
```

    ##      Segment ownHome   income
    ## 1  Moving up   ownNo 54497.68
    ## 2 Suburb mix   ownNo 54932.83
    ## 3  Travelers   ownNo 63188.42
    ## 4  Urban hip   ownNo 21337.59
    ## 5  Moving up  ownYes 50216.37
    ## 6 Suburb mix  ownYes 55143.21
    ## 7  Travelers  ownYes 61889.12
    ## 8  Urban hip  ownYes 23059.27

``` r
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)
```

    ##       Segment ownHome subscribe   income
    ## 1   Moving up   ownNo     subNo 55402.89
    ## 2  Suburb mix   ownNo     subNo 54579.99
    ## 3   Travelers   ownNo     subNo 65852.54
    ## 4   Urban hip   ownNo     subNo 21604.16
    ## 5   Moving up  ownYes     subNo 49898.85
    ## 6  Suburb mix  ownYes     subNo 55354.86
    ## 7   Travelers  ownYes     subNo 61749.71
    ## 8   Urban hip  ownYes     subNo 23993.93
    ## 9   Moving up   ownNo    subYes 50675.70
    ## 10 Suburb mix   ownNo    subYes 63753.97
    ## 11  Travelers   ownNo    subYes 48091.75
    ## 12  Urban hip   ownNo    subYes 20271.33
    ## 13  Moving up  ownYes    subYes 51359.44
    ## 14 Suburb mix  ownYes    subYes 52815.13
    ## 15  Travelers  ownYes    subYes 62944.64
    ## 16  Urban hip  ownYes    subYes 19320.64

> Resulting output can be assigned to a data frame object and indexed

``` r
agg.data <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
agg.data[2, ]
```

    ##      Segment ownHome   income
    ## 2 Suburb mix   ownNo 54932.83

``` r
agg.data[2, 3]
```

    ## [1] 54932.83

Count of factor level occurence by factor can be obtained by using the
table expression `with()` evaluates the expression in a data environment
same as `table(seg.df$Segment, seg.df$ownHome)`

``` r
with(seg.df, table(Segment, ownHome))
```

    ##             ownHome
    ## Segment      ownNo ownYes
    ##   Moving up     47     23
    ##   Suburb mix    52     48
    ##   Travelers     20     60
    ##   Urban hip     40     10

Suppose we want a breakdown of the number of kids in each household
(kids) by segment. Here kids will be treated as factor and not as a
number

``` r
with(seg.df, table(kids, Segment))
```

    ##     Segment
    ## kids Moving up Suburb mix Travelers Urban hip
    ##    0        13         11        80        17
    ##    1        17         36         0        17
    ##    2        18         22         0        11
    ##    3        13         19         0         4
    ##    4         5          7         0         1
    ##    5         3          3         0         0
    ##    6         0          2         0         0
    ##    7         1          0         0         0

> To obtain the total number of kids in each segment:

-   Alternative 1:

``` r
aggregate(kids ~ Segment, data=seg.df, sum)
```

    ##      Segment kids
    ## 1  Moving up  134
    ## 2 Suburb mix  192
    ## 3  Travelers    0
    ## 4  Urban hip   55

-   Alternative 2:

``` r
seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)
```

    ##  Moving up Suburb mix  Travelers  Urban hip 
    ##        134        192          0         55

-   Alternative 3:

``` r
seg.tab <- with(seg.df, table(kids, Segment))
colSums(seg.tab*0:7)
```

    ##  Moving up Suburb mix  Travelers  Urban hip 
    ##        134        192          0         55
