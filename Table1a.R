# Table1a. R

# Prepared by Suhas Monday, August 10, 2015

library(data.table) # to generate tables
library(dplyr) # to perform aggregations easily
library(xtable) # to generate latex table from R primitive
library(reshape2) # For melting and recasting data 
library(tidyr) # Another reshaping package

# I generate a data.table from DEVCON8a - note that this is merely a property that can be checked with a class() command
DEVCON8a <- data.table(DEVCON8a)

# I generate extract from "students" sub-set - the newly crerated variables will need to be added before preparing a data.table for the 
# entire "students' set.

Count <- function(x) base::length(which(complete.cases(x) == TRUE)) 




students1a <- DEVCON8a[, .(ST08Q01,ST115Q01,HISEI)]

tb1a1 <- summarise_each(test, funs(mean(.,na.rm=TRUE)))


students1b2 <- summarise_each(students1a,funs(sd(.,na.rm=TRUE)))


students1b3 <- summarise_each(students1a,funs(Count(.)))



students1b1
students1b2
students1b3

t1 <- rbind(round(students1b1,4),round(students1b2,4))
t1
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MEAN(St.Dev.)"))


#### Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- students1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1

flax<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
seq <- seq(2,6,by=2) # I will need to use 2,124 for the actual version as there are 124 variables
setnames(flax,"MEAN(St.Dev.)","MS")  # Just to avoid a problematic name

flax[c(2,4,6), c("Variable","V1"):=""] # this eliminates the values
# flax[c(seq-1),V1:=""]
flax

# I convert MS column into string variable so I can add () to every second row of MS
flax[, MS:=as.character(MS)]
flax

flax[c(seq),MS:=paste0("(",MS,")")]
flax

# Output looks like this
#Variable        MS    V1
#1:  ST08Q01    1.4843 47701
#2:           (0.7485)      
#3: ST115Q01    1.2388 47676
#4:           (0.5273)      
#5:    HISEI   38.6841 39518
#6:          (22.6257)   


print(xtable(flax),include.rownames = FALSE) # this generates the latex table input

