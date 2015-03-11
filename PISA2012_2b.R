# PISA2012_2b.R

# Prepared, Thursday, March 5, 2015

# Revised, Wednesday, March 11, 2015

# I replicate Fryer-Levitt sort of.
 

library(intsvy) # For PISA analysis with PVs and BRRs
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(foreign) # To import or export file
library(dplyr) # for many things


# Base regression
R0 <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8,export=FALSE)
R0

mean0 <- pisa.mean.pv(pvlabel="SCIE",by="VIETNAM", data=DEVCON8b,weight="W_FSTUWT")
mean0

# How many cases were here ?
T1 <- DEVCON8[, c("PV1SCIE", "VIETNAM", "W_FSTUWT")]
NROW(na.omit(T1)) # All 48483 of course

# I want to separate the questionnaire common items and rotated items - first look at common items only.



# Form A

# 1 Student Effort
# 1A Class time in hours per week                                                     
# I introduce modified version of dataset
DEVCON8b <- DEVCON8
DEVCON8b$SHRS <- (DEVCON8$SMINS)/60
DEVCON8b$MHRS <- (DEVCON8$MMINS)/60
DEVCON8b$LHRS <- (DEVCON8$LMINS)/60

R1A <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM",
                      "SHRS","MHRS","LHRS"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R1A

mean1A <- t(sapply(DEVCON8b[c("SHRS","MHRS","LHRS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# How many cases were here ?
T1A <- DEVCON8b[, c("SHRS", "MHRS", "LHRS")]
NROW(na.omit(T1A)) # There are 24098 cases in the data


# SO I should run my first regression on this same data as well.
DEVCON8b2 <- DEVCON8b[complete.cases(T1A),]

# Now run R1 regression again
# Base regression repeated with DEVCON8b2
R0b <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b2,export=FALSE)
R0b


# I have switched database to DEVCON8b2 of 24098 cases

# 1B Time outside of class in hours
R1B <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM",
                      "SHRS","MHRS","LHRS",
                      "ST57Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b2,export=FALSE)
R1B

mean1B <- t(sapply(DEVCON8b2[c("ST57Q01")], function(x) 
  unlist(t.test(x~DEVCON8b2$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1B

# How many cases were here ?
T1B <- DEVCON8b2[, c("ST55Q01","ST55Q02","ST55Q03","ST55Q04")]
NROW(na.omit(T1B)) # There are 19692 cases in the data

DEVCON8b3 <- DEVCON8b2[complete.cases(T1B),]

# Now for tuition classes 1C

DEVCON8b3$TUT_L[DEVCON8b3$ST55Q01==1] <- 0
DEVCON8b3$TUT_L[DEVCON8b3$ST55Q01==2] <- 1
DEVCON8b3$TUT_L[DEVCON8b3$ST55Q01==3] <- 3
DEVCON8b3$TUT_L[DEVCON8b3$ST55Q01==4] <- 5
DEVCON8b3$TUT_L[DEVCON8b3$ST55Q01==5] <- 8

DEVCON8b3$TUT_M[DEVCON8b3$ST55Q02==1] <- 0
DEVCON8b3$TUT_M[DEVCON8b3$ST55Q02==2] <- 1
DEVCON8b3$TUT_M[DEVCON8b3$ST55Q02==3] <- 3
DEVCON8b3$TUT_M[DEVCON8b3$ST55Q02==4] <- 5
DEVCON8b3$TUT_M[DEVCON8b3$ST55Q02==5] <- 8

DEVCON8b3$TUT_S[DEVCON8b3$ST55Q03==1] <- 0
DEVCON8b3$TUT_S[DEVCON8b3$ST55Q03==2] <- 1
DEVCON8b3$TUT_S[DEVCON8b3$ST55Q03==3] <- 3
DEVCON8b3$TUT_S[DEVCON8b3$ST55Q03==4] <- 5
DEVCON8b3$TUT_S[DEVCON8b3$ST55Q03==5] <- 8

DEVCON8b3$TUT_X[DEVCON8b3$ST55Q04==1] <- 0
DEVCON8b3$TUT_X[DEVCON8b3$ST55Q04==2] <- 1
DEVCON8b3$TUT_X[DEVCON8b3$ST55Q04==3] <- 3
DEVCON8b3$TUT_X[DEVCON8b3$ST55Q04==4] <- 5
DEVCON8b3$TUT_X[DEVCON8b3$ST55Q04==5] <- 8

T1C <- DEVCON8b3[, c("TUT_L","TUT_M","TUT_S","TUT_X")]
NROW(na.omit(T1C)) # There are 19692 cases in the data

mean1C <- t(sapply(DEVCON8b3[c("TUT_L","TUT_M","TUT_S","TUT_X")], function(x) 
  unlist(t.test(x~DEVCON8b3$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1C

R1C <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X"),
                   weight="W_FSTUWT",
                   data=DEVCON8b3,export=FALSE)
R1C



# 1D Truancy Skip Class and Late - This is common question so same n should
# maintain, but actually one loses cases

T1D <- DEVCON8b3[, c("ST08Q01","ST09Q01", "ST115Q01")]
NROW(na.omit(T1D)) # There are 19532 cases in the data - 160 cases lost
DEVCON8b4 <- DEVCON8b3[complete.cases(T1D),]

DEVCON8b4$LATES[DEVCON8b4$ST08Q01==1] <- 0
DEVCON8b4$LATES[DEVCON8b4$ST08Q01==2] <- 1.5
DEVCON8b4$LATES[DEVCON8b4$ST08Q01==3] <- 3.5
DEVCON8b4$LATES[DEVCON8b4$ST08Q01==4] <- 6

DEVCON8b4$TRUANT[DEVCON8b4$ST09Q01==1] <- 0
DEVCON8b4$TRUANT[DEVCON8b4$ST09Q01==2] <- 1.5
DEVCON8b4$TRUANT[DEVCON8b4$ST09Q01==3] <- 3.5
DEVCON8b4$TRUANT[DEVCON8b4$ST09Q01==4] <- 6

DEVCON8b4$SKIPC[DEVCON8b4$ST115Q01==1] <- 0
DEVCON8b4$SKIPC[DEVCON8b4$ST115Q01==2] <- 1.5
DEVCON8b4$SKIPC[DEVCON8b4$ST115Q01==3] <- 3.5
DEVCON8b4$SKIPC[DEVCON8b4$ST115Q01==4] <- 6


R1D <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM",
                      "SHRS","MHRS","LHRS",
                      "ST57Q01",
                      "TUT_L","TUT_M","TUT_S","TUT_X",
                      "LATES","TRUANT","SKIPC"),
                  weight="W_FSTUWT",
                  data=DEVCON8b4,export=FALSE)
R1D

mean1D <- t(sapply(DEVCON8b4[c("LATES","TRUANT","SKIPC")], function(x) 
  unlist(t.test(x~DEVCON8b4$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1D


# I have finished block 1 here with 19532 observations - that's not too bad


# 2 Student Mathematics Aptitude - I just use ST53Q04

T2A <- DEVCON8b4[, c("ST53Q01","ST53Q02","ST53Q03","ST53Q04")]
NROW(na.omit(T2A)) # There are now 18188 cases in the data
DEVCON8b5 <- DEVCON8b4[complete.cases(T2A),]

# I recode  a new variable from ST53 which I take as positive attitude to study

DEVCON8b5$MATHWORKa[DEVCON8b5$ST53Q01==1] <- 1
DEVCON8b5$MATHWORKa[DEVCON8b5$ST53Q01==2] <- 1
DEVCON8b5$MATHWORKa[DEVCON8b5$ST53Q01==3] <- 0

DEVCON8b5$MATHWORKb[DEVCON8b5$ST53Q02==1] <- 1
DEVCON8b5$MATHWORKb[DEVCON8b5$ST53Q02==2] <- 1
DEVCON8b5$MATHWORKb[DEVCON8b5$ST53Q02==3] <- 0

DEVCON8b5$MATHWORKc[DEVCON8b5$ST53Q03==1] <- 1
DEVCON8b5$MATHWORKc[DEVCON8b5$ST53Q03==2] <- 1
DEVCON8b5$MATHWORKc[DEVCON8b5$ST53Q03==3] <- 0

DEVCON8b5$MATHWORKd[DEVCON8b5$ST53Q04==1] <- 0
DEVCON8b5$MATHWORKd[DEVCON8b5$ST53Q04==2] <- 1
DEVCON8b5$MATHWORKd[DEVCON8b5$ST53Q04==3] <- 1

mean2A <- t(sapply(DEVCON8b5[c("MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd")], function(x) 
  unlist(t.test(x~DEVCON8b5$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean2A

#3A Pre-School
T3A <- DEVCON8b5[, c("PRESCHOOL")]
NROW(na.omit(T3A)) # There are now 18039 cases in the data
DEVCON8b6 <- DEVCON8b5[complete.cases(T3A),]

mean3A <- t(sapply(DEVCON8b6[c("PRESCHOOL")], function(x) 
  unlist(t.test(x~DEVCON8b6$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean3A


R3A <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X",
                       "LATES","TRUANT","SKIPC",
                      "MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd",
                      "PRESCHOOL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b6,export=FALSE)
R3A

# 4A Home support - SES and Parents Ed
T4A <- DEVCON8b6[, c("HISEI","HISCED","HOMEPOS","HEDRES","ICTRES")]
NROW(na.omit(T4A)) # There are now 14514 cases in the data
DEVCON8b7 <- DEVCON8b6[complete.cases(T4A),]

mean4A <- t(sapply(DEVCON8b7[c("HISEI","HISCED","HOMEPOS","HEDRES","ICTRES")], function(x) 
  unlist(t.test(x~DEVCON8b7$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean4A

R4A <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X",
                       "LATES","TRUANT","SKIPC",
                       "MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd",
                       "PRESCHOOL",
                       "HISEI","HISCED","HOMEPOS","HEDRES","ICTRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b7,export=FALSE)
R4A

#5A Gender

# Also want to check if student is female or not
DEVCON8b7$FEMALE[DEVCON8b8$ST04Q01==1] <- 1 #  - gender data has no further missing cases
DEVCON8b7$FEMALE[DEVCON8b8$ST04Q01==2] <- 0


T5A <- DEVCON8b7[, c("PCGIRLS","FEMALE")]
NROW(na.omit(T5A)) # There are now 14347 cases in the data
DEVCON8b8 <- DEVCON8b7[complete.cases(T5A),]


mean5A <- t(sapply(DEVCON8b8[c("PCGIRLS","FEMALE")], function(x) 
  unlist(t.test(x~DEVCON8b8$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean5A

R5A <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X",
                       "LATES","TRUANT","SKIPC",
                       "MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd",
                       "PRESCHOOL",
                       "HISEI","HISCED","HOMEPOS","HEDRES","ICTRES",
                       "PCGIRLS","FEMALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b8,export=FALSE)
R5A

#6A Teacher Quantity 
T6A <- DEVCON8b8[, c("STRATIO")]
NROW(na.omit(T6A)) # There are now 13528 cases in the data
DEVCON8b9 <- DEVCON8b8[complete.cases(T6A),]

mean6A <- t(sapply(DEVCON8b9[c("STRATIO")], function(x) 
  unlist(t.test(x~DEVCON8b9$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean6A

R6A <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X",
                       "LATES","TRUANT","SKIPC",
                       "MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd",
                       "PRESCHOOL",
                       "HISEI","HISCED","HOMEPOS","HEDRES","ICTRES",
                       "PCGIRLS","FEMALE",
                       "STRATIO"),
                   weight="W_FSTUWT",
                   data=DEVCON8b9,export=FALSE)
R6A


#7A Teacher Quality 

T7A <- DEVCON8b9[, c("PROPCERT","PROPQUAL")]
NROW(na.omit(T7A)) # There are now 11628 cases in the data
DEVCON8b10 <- DEVCON8b9[complete.cases(T7A),]

mean7A <- t(sapply(DEVCON8b10[c("PROPCERT","PROPQUAL")], function(x) 
  unlist(t.test(x~DEVCON8b10$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean7A
R7A <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01",
                       "TUT_L","TUT_M","TUT_S","TUT_X",
                       "LATES","TRUANT","SKIPC",
                       "MATHWORKa","MATHWORKb","MATHWORKc","MATHWORKd",
                       "PRESCHOOL",
                       "HISEI","HISCED","HOMEPOS","HEDRES","ICTRES",
                       "PCGIRLS","FEMALE",
                       "STRATIO",
                       "PROPCERT","PROPQUAL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b10,export=FALSE)
R7A

#8A 

T8A <- DEVCON8b10[, c("SC25Q01")]
NROW(na.omit(T8A)) # There are now 11364 cases in the data
DEVCON8b11 <- DEVCON8b10[complete.cases(T8A),]




R0F <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b10,export=FALSE)
R0F






# How many cases were here ?
T2A <- DEVCON8b4[, c("PERSEV", "OPENPS")]
NROW(na.omit(T2A)) # There are 6741  cases in the data (I lost nearly half the cases)



#2B Other attitude (math related)
R2B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS",
                       "ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06",
                       "LATES","TRUANT","SKIPC",
                       "PERSEV", "OPENPS","MATWKETH","MATHEFF","MATBEH","MATINTFC"),
                   weight="W_FSTUWT",
                   data=DEVCON8b4,export=FALSE)
R2B

mean2B <- t(sapply(DEVCON8b4[c("PERSEV", "OPENPS","MATWKETH","MATHEFF","MATBEH","MATINTFC")], function(x) 
  unlist(t.test(x~DEVCON8b4$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean2B

# How many cases were here ?
T2B <- DEVCON8b4[, c("MATWKETH","MATHEFF","MATBEH","MATINTFC")]
NROW(na.omit(T2B)) # There are 6541  cases in the data (I lost nearly half the cases)

BLIX <- DEVCON8b2[complete.cases(T1B),]
BLAX <- BLIX[, c("NEWID","ST55Q01","ST55Q02","ST55Q03","ST55Q04")]
write.csv(BLAX, "blax.csv")





BLIX <- DEVCON8b[,c("VIETNAM","NEWID", "SHRS","MHRS","LHRS")]
BLAX <- na.omit(BLIX)
BLAX$CNT <- as.numeric(substr(BLAX$NEWID, 1, 1))
table(BLAX$CNT)

summary(DEVCON8b)

BLIX <- which(is.na(DEVCON8b$PV1SCIE))
GLIX <- which(is.na(DIX$PERSEV))
BLAX <- -BLIX
GLAX <- -GLIX
FIX <- DIX[GLAX,]

  ydata[!complete.cases(mydata),]
newdata <- na.omit(mydata) 






TEST <- DEVCON8b[,c(2, 3:4)]
