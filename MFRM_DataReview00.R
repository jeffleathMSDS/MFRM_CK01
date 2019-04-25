## Jeff's review of MFRM Check Clearing Data
## Sourced from JL Data Lake
## 
## Only CLEARED Checks
## Need to change source data

## add librarys
library(readr)
library(ggplot2)
library(scales)
library(plyr)
library(ddply)
library(dplyr)
library(tibbletime)


##
## set working directory

setwd("H:/AA/MFRM_CK01")


## read in data file

checks <- read_csv("source/qry_CR_00_CLEARED00.csv", 
col_types = cols(Amount_Orig = col_double(), 
AsOf = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckDate = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckNbr = col_character(),
StatusDate = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


## review elements of data
str(checks)

## summary info for data
summary(checks)

## exclude Checks with Days2Clear <=0

checks2 <-checks %>%
  filter(Days2CLR > "0")
  #filter(Days2CLR > "0" & ConsGroup == "Rent" & CheckDate == as.Date("01/01/2019"))

## summary info for data
summary(checks2)

## summary info for days to clear
summaryL1 <-summary(checks2$Days2CLR)
summaryL1


##historgram
hist(checks2$Days2CLR)

summaryL2AS <- ddply(checks2, .(checks2$AsOf),
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
checksexcluded <-subset(checks2, checks2$Days2CLR > DC)
summary(checksexcluded)

## Export excluded records
write.table(checksexcluded, file="C:/Users/jeffl/OneDrive - Mattress Firm/DB/CheckClearings/tbl_excluded01.csv",sep=",",row.names=FALSE)



## Moving forward with a cleaner data set
checks3 <-subset(checks, checks$Days2CLR < DC)
summary(checks3)

## Export Checks2
write.table(checks3, file="C:/Users/jeffl/OneDrive - Mattress Firm/DB/CheckClearings/tbl_checks2.csv",sep=",",row.names=FALSE)




summaryL1 <-summary(checks3$Days2CLR)
summaryL1

summaryL2 <- ddply(checks3, .(ConsGroup),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2

summaryL2v <- ddply(checks3, .(VendorID),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2v

summaryL2d <- ddply(checks3, .(CkDay),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2d

summaryL2Lg <- ddply(checks3, .(Large),
                    summarise,
                    N = length(Days2CLR),
                    mean=mean(Days2CLR),
                    sum=sum(Days2CLR),
                    sd   = sd(Days2CLR),
                    se   = sd / sqrt(N))
summaryL2Lg


## summarize check amounts by vendor
summaryL2cD <- ddply(checks3, .(VendorID),
                    summarise,
                    N = length(Amount_Orig),
                    mean=mean(Amount_Orig),
                    min=min(Amount_Orig),
                    max=max(Amount_Orig),
                    sum=sum(Amount_Orig),
                    sd   = sd(Amount_Orig),
                    se   = sd / sqrt(N))
summaryL2cD


## quantile for days to clear
quantL1 <-quantile(checks3$Days2CLR)
quantL1



## scatterplot:  Check Amount by Days to Clear
p1 <-plot(checks3$Amount_Orig, checks3$Days2CLR,
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

p2 <- qplot(checks3$Days2CLR, data=checks3, geom="density",
      fill=checks3$ConsGroup, alpha=I(.5), 
      main="Density: Days to Clear by Group
      (P2)",
      xlab="Days to Clear",
      ylab="Density")
p2


#find correlations
# basic scatterplot


p4 <-ggplot(checks3, aes(x=checks3$Days2CLR, y=checks3$ConsGroup)) + 
  geom_point()
p4

p5 <-ggplot(checks3, aes(x=checks3$Amount_Orig, y=checks3$Days2CLR)) + 
  geom_point()
p5

# One Way Anova (Completely Randomized Design)
fit <- aov(Days2CLR ~ ConsGroup, data=checks3)
fit
plot(fit)

# find r
r1 <-cor(checks3$Amount_Orig, checks3$Days2CLR)
r1



# regression test1
checklm <- lm(Days2CLR~AD+CA+Co+DI+EM+ME+OT+OV+PA+PR+RE+SU+TA+Large, data = checks3)
checklm
summary(checklm)
confint(checklm)

# regression test2
checklm2 <- lm(Days2CLR~DI+EM+ME+OT+OV+RE+TA+Large, data = checks3)
checklm2
summary(checklm2)
confint(checklm2)


# regression test3
checklm3 <- lm(Days2CLR~DI+EM+ME+OT+RE+TA+Large, data = checks3)
checklm3
summary(checklm3)
confint(checklm3)
