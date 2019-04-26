## Jeff's review of MFRM Check Clearing Data
## Sourced from JL Data Lake
## 
## Only CLEARED Checks
## 

## load librarys
library(readr) #import data
library(ggplot2) # viz
library(scales) # viz
library(dplyr) # data handling in df
library(plyr)
library(tibble) # data frame handling
library(tibbletime) # time formatting addon to tibble
library(DAAG) # Model Cross Validation, unique package
library(MASS) # Stepwise Regression, unique package
library(leaps) # Regression Subset Selection
library(car) # Companion to Applie Regression
library(caret) # model accuracy testing


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
coefficients(checklm)
fitted(checklm)
residuals(checklm)
anova(checklm)
vcov(checklm)
influence(checklm)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(checklm)


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


# compare models
fit1 <- checklm
fit2 <- checklm3
anova(fit1, fit2)

# K-fold cross-validation
cv.lm(checks3, checklm, m=3) # 3 fold cross-validation

# Stepwise Regression
fit <- lm(Days2CLR~AD+CA+Co+DI+EM+ME+OT+OV+PA+PR+RE+SU+TA+Large, data = checks3)
step <- stepAIC(fit, direction="both")
step$anova # display results

# All Subsets Regression
attach(checks3)
leaps<-regsubsets(Days2CLR~AD+CA+Co+DI+EM+ME+OT+OV+PA+PR+RE+SU+TA+Large, data = checks3,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
subsets(leaps, statistic="rsq")

## Test and Train

#sample size

smp_size <- floor(0.75 * nrow(checks3))
smp_size

## set the seed to make your partition reproducible
set.seed(7)
train_ind <- sample(seq_len(nrow(checks3)), size = smp_size)

train <- checks3[train_ind,]
test <- checks3[-train_ind,]


##define training control
train_control <-trainControl(method = "boot", 100)
# train the model
model <-train(Days2CLR~., data = checks3, trControl=train_control, method="nb")
#summarize results
print(model)

