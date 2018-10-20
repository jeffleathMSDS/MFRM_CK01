## Jeff's review of MFRM Check Clearing Data
## Sourced from JL Data Lake
## Check Dates between 3/1/2018 and 10/2/2018
## Only CLEARED Checks


## add librarys
library(readr)
library(ggplot2)
library(scales)
library(plyr)

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

## quantile for days to clear
quantL1 <-quantile(checks$Days2CLR)
quantL1


## find std dev for each cons group
STDDEVL1 <-ddply(checks,.(ConsGroup),colwise(sd))
STDDEVL1

## scatterplot:  Check Amount by Days to Clear
p1 <-plot(checks$Amount_Orig, checks$Days2CLR,
          cex = .5,col = "purple",
          main = "Days to Clear by Check Amount
          (limited to 60)",
          ylim=c(0,60),
          
          xlab = "Check Amount",
          ylab= "Days to Clear"
          )
p1

# Kernel density plots for Days to Clear
# grouped by number of gears (indicated by color)
## limiting days 2 clear to 60

p2 <- qplot(checks$Days2CLR, data=checks, geom="density",
      fill=checks$ConsGroup, alpha=I(.5), 
      main="Density: Days to Clear by Group",
      xlab="Days to Clear",
      xlim=c(0,60),
      ylab="Density")
p2