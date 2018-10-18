## Jeff's review of MFRM Check Clearing Data
## Sourced from JL Data Lake
## Check Dates between 3/1/2018 and 10/2/2018
## Only CLEARED Checks


## add librarys
library(readr)
library(ggplot2)


## readin data file


checks <- read_csv("C:/Users/jeffl/OneDrive - Mattress Firm/DB/CheckClearings/qry_CR_00_CLEARED00.csv", 
col_types = cols(Amount_Orig = col_double(), 
AsOf = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckDate = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
CheckNbr = col_character(),
StatusDate = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

str(checks)
summary(checks)
plot((checks$Amount_Orig, checks$Days2CLR))