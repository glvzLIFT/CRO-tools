## #########################################################################
## MDE Calculator
## 
## Author: Michael Galvez
## #########################################################################



## Set working directory
setwd(choose.dir())


## Load libraries
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(stringr)


## Set MDE baseline values
baselineDailyTraffic <- 683
baselineCR <- 0.0228
treatments <- 2

## Setup Data Frame for Weeks 2 through 6 
mdeCalc <- data.frame(weeks=integer())
for (i in 1:5) {
  mdeCalc[i,1] = i+1
}  
# End for loop


## Add traffic (n) for Weeks 2 through 6 
mdeCalc %<>% mutate(trafficPerVariant = baselineDailyTraffic*7*weeks/treatments)


## Add p1 column with the same baseline CR
mdeCalc$p1 = baselineCR


## For each row, calculate minimum target CR (p2) using power.prop.test
for (i in 1:nrow(mdeCalc)) {
    p2 <- power.prop.test( n = unlist(mdeCalc[i,2]), p1 = unlist(mdeCalc[i,3]), sig.level = 0.05, power = 0.8,  alternative = c("two.sided"), strict = TRUE)
    mdeCalc[i,'targetCR'] = p2[[3]]
}    
# End for loop


## For each row, calculate MDE using % change
for (i in 1:nrow(mdeCalc)) {
  mdeCalc[i,'MDE'] = (mdeCalc[i,4]-mdeCalc[i,3])/mdeCalc[i,3]*100
}    
# End for loop


## For each row, calculate Total Sample Size
for (i in 1:nrow(mdeCalc)) {
  mdeCalc[i,'Sample Size'] = (mdeCalc[i,2]*treatments)
}    
# End for loop

write.csv(mdeCalc,file = "MDE-estimates.csv")
