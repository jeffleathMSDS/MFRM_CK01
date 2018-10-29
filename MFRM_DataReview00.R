## Jeff's review of MFRM Check Clearing Data
## Sourced from JL Data Lake
## Check Dates between 3/1/2018 and 10/2/2018
## Only CLEARED Checks


## add librarys
library(readr)
library(ggplot2)
library(scales)
library(plyr)
library(ddply)

## read in data file


checks <- read_csv("C:/Users/jeffl/OneDrive - Mattress Firm/DB/CheckClearings/qry_CR_00_CLEARED00.csv", 
col_types = cols(Amount_Orig = col_double(), 
AsOf = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckDate = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckNbr = col_character(),
StatusDate = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


## review elements of data
str(checks)

## summary info for data
summary(checks)


## summary info for days to clear
summaryL1 <-summary(checks$Days2CLR)
summaryL1


##historgram
hist(checks$Days2CLR)

summaryL2AS <- ddply(checks, .(checks$AsOf),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2AS

## After reviewing the raw data, it is clear there is 'noise' in the data
# Reviewing the variance of Days to Clear, it is very long tailed (to the right)
# The outlier data (driven by unusual business circumstances) could skew the forecasted
# data, which needs to be conservative in nature
# I will subset the data, to remove outliers, defined as Days to Clear > DC

DC <- summaryL2AS$mean + summaryL2AS$sd*2
DC

## I will compile the eliminated records in this variable for review
checksexcluded <-subset(checks, checks$Days2CLR > DC)
summary(checksexcluded)


## Moving forward with a cleaner data set
checks2 <-subset(checks, checks$Days2CLR < DC)
summary(checks2)

summaryL1 <-summary(checks2$Days2CLR)
summaryL1

summaryL2 <- ddply(checks2, .(ConsGroup),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2

summaryL2v <- ddply(checks2, .(VendorID),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2v

summaryL2d <- ddply(checks2, .(CkDay),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2d


## quantile for days to clear
quantL1 <-quantile(checks2$Days2CLR)
quantL1



## scatterplot:  Check Amount by Days to Clear
p1 <-plot(checks2$Amount_Orig, checks2$Days2CLR,
          cex = .5,col = "purple",
          main = "Days to Clear by Check Amount
          (P1)",
          ylim=c(0,DC),
          
          xlab = "Check Amount",
          ylab= "Days to Clear"
          )
p1

# Kernel density plots for Days to Clear
# grouped by number of gears (indicated by color)
## limiting days 2 clear to 60

p2 <- qplot(checks2$Days2CLR, data=checks2, geom="density",
      fill=checks2$ConsGroup, alpha=I(.5), 
      main="Density: Days to Clear by Group
      (P2)",
      xlab="Days to Clear",
      ylab="Density")
p2


#find correlations
# basic scatterplot
p3 <-ggplot(checks2, aes(x=checks2$ConsGroup, y=checks2$Days2CLR)) + 
  geom_point()
p3

p4 <-ggplot(checks2, aes(x=checks2$Days2CLR, y=checks2$ConsGroup)) + 
  geom_point()
p4

p5 <-ggplot(checks2, aes(x=checks2$Amount_Orig, y=checks2$Days2CLR)) + 
  geom_point()
p5



# find r
cor(checks2$Amount_Orig, checks2$Days2CLR)


# regression code from Stats Ch9
checklm <- lm(Days2CLR~Amount_Orig, data = checks2)
checklm

summary(checklm)

confint(checklm)
