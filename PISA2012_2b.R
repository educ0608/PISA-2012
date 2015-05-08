# PISA2012_2b.R

# Prepared, Thursday, March 5, 2015

# Revised, Friday, May 8, 2015

# I replicate Fryer-Levitt sort of.
 

library(intsvy) # For PISA analysis with PVs and BRRs
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(foreign) # To import or export file
library(dplyr) # for many things
library(gmodels) # For PROC FREQ like tables
library(psych) # for rescaling variables to given mean and sd


# How many cases ?
T0 <- DEVCON8a[, c("VIETNAM")]
N0<- NROW(na.omit(T0)) # 48483 data points - we have scores on these students for sure

# What is the mean score
mean0 <- pisa.mean.pv(pvlabel="SCIE",by="VIETNAM", data=DEVCON8a,weight="W_FSTUWT")
mean0


# The baseline regression - I will need to report this one. 
R0 <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0

# Result:-
#Estimate Std. Error t value
#(Intercept)   393.86       2.25  175.00
#VIETNAM       134.56       4.91   27.41
#R-squared      30.75       1.96   15.66


# I want to separate the questionnaire common items and rotated items - first look at common items only.

# Let's start with the School Questionnaire, it is easier


# SC01 - Private or Public School
# __________________________________________________________________________________________
# This is the first question, but does not seem an ideal place to start for the topic at hand
# I will follow the order of the questionnaire to begin with, then choose my specification and experiment 
# with alternative directions in selection

#How many non-missing values ?
T1b <- DEVCON8a[, c("SC01Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48262  data points 
(N0-N1)

# I need to run the regression on data excluding the missing values
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Convert into 0 1 variable
DEVCON8b$PRIVATESCL[DEVCON8b$SC01Q01==2] <- 1
DEVCON8b$PRIVATESCL[DEVCON8b$SC01Q01==1] <- 0

CrossTable(DEVCON8b$PRIVATESCL, DEVCON8b$VIETNAM) 
# 83% students in public schools as compared to 91.8% for Vietnam

mean1B <- t(sapply(DEVCON8b[c("PRIVATESCL")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1B
# 17% in G7 8.2% in Vietnam

# Supposedly, private schools do better - let's see.
R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "PRIVATESCL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Result:- 
#Estimate Std. Error t value
#(Intercept)   396.91       2.36  168.03
#VIETNAM       132.33       4.76   27.77
#PRIVATESCL     -9.93       5.31   -1.87
#R-squared      31.01       2.02   15.37


# SC02 - Funding sources
# __________________________________________________________________________________________

# I am interested in effect of Revenue from Fees - skin in the game argument
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC02Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 # 41,090
N0-N1 # 7393 NAs

# I need to run the regression on data excluding the missing values
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1B <- t(sapply(DEVCON8b[c("SC02Q02")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1B

# Rev 25.74% in G7 schools; 16.39% in Vietnam

# Supposedly, more skin in the game is better, also could be a resource story  - let's see.
R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SC02Q02"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
#Estimate Std. Error t value
#(Intercept)   391.98       2.60  150.79
#VIETNAM       133.96       5.02   26.67
#SC02Q02         0.17       0.07    2.39
#R-squared      31.87       1.99   16.03


# SC03 - City size
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC03Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,427
N0-N1 # 56 NAs

# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# First I have to generate a series of dummy variables 
DEVCON8b$DUM_VILLAGE <- ifelse(DEVCON8b$SC03Q01==1,1,0)
DEVCON8b$DUM_SMLTOWN <- ifelse(DEVCON8b$SC03Q01==2,1,0)
DEVCON8b$DUM_TOWN    <- ifelse(DEVCON8b$SC03Q01==3,1,0)
DEVCON8b$DUM_CITY    <- ifelse(DEVCON8b$SC03Q01==4,1,0)
DEVCON8b$DUM_LRGCITY <- ifelse(DEVCON8b$SC03Q01==5,1,0)

mean1A <- t(sapply(DEVCON8b[c("DUM_VILLAGE", "DUM_SMLTOWN","DUM_TOWN","DUM_CITY","DUM_LRGCITY")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
#DUM_VILLAGE                0.1252316               0.45159896  0.000000e+00  -43.095024
#DUM_SMLTOWN                0.2136940               0.24567848  2.415052e-06   -4.719694
#DUM_TOWN                   0.2364126               0.08102852 5.183229e-223   32.828771
#DUM_CITY                   0.2521408               0.19922213  1.784455e-16    8.257506
#DUM_LRGCITY                0.1725211               0.02247191  0.000000e+00   48.085440


# Regression against excluded category village

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "DUM_SMLTOWN","DUM_TOWN","DUM_CITY","DUM_LRGCITY"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

#Estimate Std. Error t value
#(Intercept)   372.18       4.00   93.06
#VIETNAM       140.87       5.01   28.10
#DUM_SMLTOWN    10.53       5.93    1.78
#DUM_TOWN       31.55       5.62    5.61
#DUM_CITY       42.19       6.30    6.70
#DUM_LRGCITY    70.55       7.46    9.45
#R-squared      37.55       2.10   17.88

# SC04  - Competing for students 
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC04Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,412
N0-N1 # 71 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
CrossTable(DEVCON8b$SC04Q01, DEVCON8b$VIETNAM, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 
# Not much difference in proportions and I am not sure about the options.



# SC05  - Class Size CLSIZE
# __________________________________________________________________________________________
# How many non-missing values ?
# Shows no missing on SC05Q01, but has 56 and 568 for codes 98 and 99 rather than 1 to 9 eligible codes
# I use the CLSIZE variable already provided
T1b <- DEVCON8a[, c("CLSIZE")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,850
N0-N1 # 633 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("CLSIZE")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#CLSIZE                 34.99832                 42.48982       


# Supposedly, smaller classes are better

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "CLSIZE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   345.20      10.45   33.05
# VIETNAM       125.11       5.17   24.20
# CLSIZE          1.37       0.27    4.99
# R-squared      32.66       2.11   15.51

# SC07, SC09, SC10 and SC11Q01 - Students and Teachers and certified/qualified teachers
# __________________________________________________________________________________________
# How many non-missing values ?
# Shows no missing on SC05Q01, but has 56 and 568 for codes 98 and 99 rather than 1 to 9 eligible codes
# I use the CLSIZE variable already provided
T1b <- DEVCON8a[, c("SCHSIZE", "STRATIO", "PROPCERT","PROPQUAL")]
N1 <- NROW(na.omit(T1b)) 
N1 # 33,239
N0-N1 # 15,244 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
table(DEVCON8a$CNT)
table(DEVCON8b$CNT)


mean1A <- t(sapply(DEVCON8b[c("SCHSIZE", "STRATIO", "PROPCERT","PROPQUAL")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value  statistic.t
# SCHSIZE              1105.5566041             1274.8595506  7.013317e-54 -15.56346909
# STRATIO                19.5718936               19.0372493  6.830416e-08   5.39990196
# PROPCERT                0.6246824                0.7893719 2.321196e-139 -25.77301953
# PROPQUAL                0.8770705                0.8773723  9.441337e-01  -0.07007841   


# Supposedly, better qualified teachers are better, larger/smaller schools 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SCHSIZE", "STRATIO", "PROPCERT","PROPQUAL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   355.69      12.25   29.02
# VIETNAM       120.99       4.73   25.57
# SCHSIZE         0.03       0.00   10.67
# STRATIO        -0.96       0.31   -3.10
# PROPCERT       16.73       4.94    3.38
# PROPQUAL       20.70      11.53    1.80
# R-squared      41.85       1.92   21.78


# SC11Q02-03, SC13 - I use RATCMP15 and COMPWEB 
# __________________________________________________________________________________________
# How many non-missing values ?
# Shows no missing on SC05Q01, but has 56 and 568 for codes 98 and 99 rather than 1 to 9 eligible codes
# I use the CLSIZE variable already provided
T1b <- DEVCON8a[, c("RATCMP15","COMPWEB")]
N1 <- NROW(na.omit(T1b)) 
N1 # 42,132
N0-N1 # 6,351 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
table(DEVCON8a$CNT)
table(DEVCON8b$CNT)


mean1A <- t(sapply(DEVCON8b[c("RATCMP15","COMPWEB")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# RATCMP15                0.4109337                0.2289965 4.617841e-154   27.270392
# COMPWEB                 0.7516232                0.7834181  4.503828e-09   -5.875541


# Clearly, these variables stand for other thnings -

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "RATCMP15","COMPWEB"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

#Estimate Std. Error t value
#(Intercept)   352.77       4.73   74.57
#VIETNAM       119.81       6.78   17.67
#RATCMP15        2.34       6.04    0.39
#COMPWEB        51.09       6.46    7.90
#R-squared      29.52       2.08   14.20

# SC14 Shortages
# __________________________________________________________________________________________
# How many non-missing values ?
# Shows no missing on SC05Q01, but has 56 and 568 for codes 98 and 99 rather than 1 to 9 eligible codes
# I use the CLSIZE variable already provided
T1b <- DEVCON8a[, c("SCMATBUI", "SCMATEDU", "TCSHORT")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,017
N0-N1 # 466 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
table(DEVCON8a$CNT)
table(DEVCON8b$CNT)


mean1A <- t(sapply(DEVCON8b[c("SCMATBUI", "SCMATEDU", "TCSHORT")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# SCMATBUI               -0.6367538               -0.4012056 2.156028e-52  -15.371776
# SCMATEDU               -0.8119099               -0.5023066 3.221799e-93  -20.806293
# TCSHORT                 0.4751680                0.4180102 1.166150e-03    3.248502

# Clearly, these variables stand for other thnings -

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SCMATBUI", "SCMATEDU", "TCSHORT"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   403.98       3.09  130.80
# VIETNAM       129.69       4.94   26.27
# SCMATBUI       -3.68       2.27   -1.62
# SCMATEDU       15.55       2.55    6.10
# TCSHORT         1.91       1.89    1.01
# R-squared      33.46       1.88   17.77


# SC15 Ability Grouping in Math classes as ABGMATH - is not different/is unclear
# __________%**&%*&%*%*&______________(*^(%*$&$&%*_______________________________%*%*&%*&#%#%()()__________________________


# SC16 Extra-curricular activities
# __________________________________________________________________________________________
# How many non-missing values ?
# Shows no missing on SC05Q01, but has 56 and 568 for codes 98 and 99 rather than 1 to 9 eligible codes
# I use the CLSIZE variable already provided
T1b <- DEVCON8a[, c("SC16Q02","SC16Q06")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,017
N0-N1 # 466 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

DEVCON8b$EXC1_BAND[DEVCON8b$SC16Q01==1] <- 1
DEVCON8b$EXC1_BAND[DEVCON8b$SC16Q01==2] <- 0

DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==1] <- 1
DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==2] <- 0

DEVCON8b$EXC3_NEWS[DEVCON8b$SC16Q03==1] <- 1
DEVCON8b$EXC3_NEWS[DEVCON8b$SC16Q03==2] <- 0

DEVCON8b$EXC4_VOLU[DEVCON8b$SC16Q04==1] <- 1
DEVCON8b$EXC4_VOLU[DEVCON8b$SC16Q04==2] <- 0

DEVCON8b$EXC5_MCLUB[DEVCON8b$SC16Q05==1] <- 1
DEVCON8b$EXC5_MCLUB[DEVCON8b$SC16Q05==2] <- 0

DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==1] <- 1
DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==2] <- 0

DEVCON8b$EXC7_CHESS[DEVCON8b$SC16Q07==1] <- 1
DEVCON8b$EXC7_CHESS[DEVCON8b$SC16Q07==2] <- 0

DEVCON8b$EXC8_ICTCB[DEVCON8b$SC16Q08==1] <- 1
DEVCON8b$EXC8_ICTCB[DEVCON8b$SC16Q08==2] <- 0

DEVCON8b$EXC9_ARTCB[DEVCON8b$SC16Q09==1] <- 1
DEVCON8b$EXC9_ARTCB[DEVCON8b$SC16Q09==2] <- 0

DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==1] <- 1
DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==2] <- 0

DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==1] <- 1
DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==2] <- 0


# Something intricate is happening here; When you include the two variables for which VN is better, 
# namely 2 school play and 6 math competition, separately, you get big + effect and reduction in dummy
# of course funny happens when you put only one of them and both together - math competition on its own
# it is big and becomes much smaller- washed out when you bring in school play; A good question to ask
# is why MATHCOMP washes out and not PLAY in this case ? I need to think about that a bit.
# the other 9 activities, when you include, the more positive the coefficients, the bigger the VN deficit
# eg. just BAND adds 15 points to the deficit - clearly this is not a band effect, it captures all that
# happens in those kinds of schools, of which there are not as many in Vietnam ! 
# When you add three togethe - MATHCOMP,PLAY and BAND, MATHCOMP gets washed out even more, and it is clear
# that gap adding effect of BAND more than makes up for gap reducing effect of PLAY and MATHCOMP

CrossTable(DEVCON8b$EXC1_BAND, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 
CrossTable(DEVCON8b$EXC2_PLAY, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 
CrossTable(DEVCON8b$EXC6_MATHCOMP, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

mean1A <- t(sapply(DEVCON8b[c("EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# SCMATBUI               -0.6367538               -0.4012056 2.156028e-52  -15.371776
# SCMATEDU               -0.8119099               -0.5023066 3.221799e-93  -20.806293
# TCSHORT                 0.4751680                0.4180102 1.166150e-03    3.248502

# Clearly, these variables stand for other things -

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B


R1B2 <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "EXC1_BAND","EXC3_NEWS","EXC5_MCLUB","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B2

R1B3 <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "EXC2_PLAY","EXC4_VOLU","EXC6_MATHCOMP","EXC11_UNICORN"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B3


# Estimate Std. Error t value
# (Intercept)   375.93       2.95  127.52
# VIETNAM       146.05       5.19   28.12
# EXC1_BAND      35.39       4.45    7.95
# R-squared      34.42       1.83   18.85

#Estimate Std. Error t value
#(Intercept)   374.48       2.88  130.20
#VIETNAM       124.25       5.13   24.24
#EXC2_PLAY      34.92       4.40    7.94
#R-squared      34.17       1.82   18.73

#Estimate Std. Error t value
#(Intercept)     385.60       3.22  119.61
#VIETNAM         131.51       5.20   25.31
#EXC6_MATHCOMP    13.75       4.73    2.91
#R-squared        31.43       1.93   16.26

#Estimate Std. Error t value
#(Intercept)     371.55       3.32  111.98
#VIETNAM         123.43       5.21   23.67
#EXC6_MATHCOMP     5.76       4.41    1.31
#EXC2_PLAY        33.76       4.51    7.48
#R-squared        34.32       1.82   18.85

#(Intercept)     366.13       3.34  109.71
#VIETNAM         135.17       5.60   24.13
#EXC6_MATHCOMP     2.69       4.18    0.64
#EXC2_PLAY        23.96       4.58    5.23
#EXC1_BAND        24.91       4.41    5.65
#R-squared        35.78       1.75   20.44

#Estimate Std. Error t value
#(Intercept)     342.02       9.65   35.46
#VIETNAM         140.00       5.57   25.15
#EXC1_BAND        16.16       4.45    3.63
#EXC2_PLAY        15.95       4.93    3.23
#EXC3_NEWS        15.66       4.24    3.70
#EXC4_VOLU        17.35       5.59    3.10
#EXC5_MCLUB       13.02       5.00    2.61
#EXC6_MATHCOMP    -1.44       4.16   -0.35
#EXC7_CHESS       -3.46       5.68   -0.61
#EXC8_ICTCB        7.00       4.46    1.57
#EXC9_ARTCB        8.89       5.44    1.63
#EXC10_SPORT      -5.31       8.53   -0.62
#EXC11_UNICORN     5.59       5.93    0.94
#R-squared        38.50       1.78   21.61

#Estimate Std. Error t value
#(Intercept)   354.02       8.65   40.91
#VIETNAM       148.21       4.96   29.89
#EXC1_BAND      23.04       4.39    5.25
#EXC3_NEWS      17.52       4.41    3.97
#EXC5_MCLUB     13.62       4.91    2.78
#EXC7_CHESS     -4.32       5.80   -0.75
#EXC8_ICTCB     10.00       4.58    2.19
#EXC9_ARTCB     11.11       5.31    2.09
#EXC10_SPORT     3.16       9.70    0.33
#R-squared      37.55       1.84   20.43


#Estimate Std. Error t value
#(Intercept)     344.04       6.08   56.55
#VIETNAM         121.98       5.11   23.87
#EXC2_PLAY        30.15       4.38    6.88
#EXC4_VOLU        22.73       5.38    4.22
#EXC6_MATHCOMP     5.26       4.21    1.25
#EXC11_UNICORN    13.95       6.22    2.24
#R-squared        35.53       1.89   18.85


# SC18 Use of assessment appears to have 47000s data and constructed ASSESS only has 20,085 non NAs
# sumpin goin'on 'ere....
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC18Q01","SC18Q02","SC18Q03","SC18Q04","SC18Q05","SC18Q06","SC18Q07","SC18Q08")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,323
N0-N1 # 2160 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# I generate dichotomous equivalents 

DEVCON8b$AS1_PARENTS[DEVCON8b$SC18Q01==1] <- 1
DEVCON8b$AS1_PARENTS[DEVCON8b$SC18Q01==2] <- 0

DEVCON8b$AS2_PROMOTE[DEVCON8b$SC18Q02==1] <- 1
DEVCON8b$AS2_PROMOTE[DEVCON8b$SC18Q02==2] <- 0

DEVCON8b$AS3_GROUPING[DEVCON8b$SC18Q03==1] <- 1
DEVCON8b$AS3_GROUPING[DEVCON8b$SC18Q03==2] <- 0

DEVCON8b$AS4_NATCOMP[DEVCON8b$SC18Q04==1] <- 1
DEVCON8b$AS4_NATCOMP[DEVCON8b$SC18Q04==2] <- 0

DEVCON8b$AS5_YEARLY[DEVCON8b$SC18Q05==1] <- 1
DEVCON8b$AS5_YEARLY[DEVCON8b$SC18Q05==2] <- 0

DEVCON8b$AS6_TEACHER[DEVCON8b$SC18Q06==1] <- 1
DEVCON8b$AS6_TEACHER[DEVCON8b$SC18Q06==2] <- 0

DEVCON8b$AS7_CURRICLM[DEVCON8b$SC18Q07==1] <- 1
DEVCON8b$AS7_CURRICLM[DEVCON8b$SC18Q07==2] <- 0

DEVCON8b$AS8_OTHSCHLS[DEVCON8b$SC18Q08==1] <- 1
DEVCON8b$AS8_OTHSCHLS[DEVCON8b$SC18Q08==2] <- 0


CrossTable(DEVCON8b$SC18Q02, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 


# How many non-missing values ?
T1b <- DEVCON8b[, c("AS1_PARENTS","AS2_PROMOTE","AS3_GROUPING","AS4_NATCOMP","AS5_YEARLY","AS6_TEACHER","AS7_CURRICLM","AS8_OTHSCHLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,323
N0-N1 # 2160 NAs

mean1A <- t(sapply(DEVCON8b[c("AS1_PARENTS","AS2_PROMOTE","AS3_GROUPING","AS4_NATCOMP","AS5_YEARLY","AS6_TEACHER","AS7_CURRICLM","AS8_OTHSCHLS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
#AS1_PARENTS                 0.9678611                0.9929421  1.228993e-64  -17.086950
#AS2_PROMOTE                 0.8986277                0.9516031  2.416195e-54  -15.649322
#AS3_GROUPING                0.6599323                0.7418834  1.081125e-34  -12.358847
#AS4_NATCOMP                 0.6932077                0.8804194 1.352332e-268  -36.481928
#AS5_YEARLY                  0.9105644                0.9802380 9.187169e-176  -28.796442
#AS6_TEACHER                 0.7728438                0.9913289  0.000000e+00  -89.638694
#AS7_CURRICLM                0.8987290                0.9140956  2.961035e-04   -3.620679
#AS8_OTHSCHLS                0.6544596                0.8681186 1.629320e-317  -40.011256

# Appears a lot of high 80s and 90s - rarely would be informative, even 99.3%  - seems to not be useful

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "AS1_PARENTS","AS2_PROMOTE","AS3_GROUPING","AS4_NATCOMP","AS5_YEARLY","AS6_TEACHER","AS7_CURRICLM","AS8_OTHSCHLS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

#(Intercept)    374.23      18.91   19.78
#VIETNAM        131.94       5.04   26.19
#AS1_PARENTS      9.91      25.52    0.39
#AS2_PROMOTE      3.45       6.76    0.51
#AS3_GROUPING     4.18       4.46    0.94
#AS4_NATCOMP     15.75       6.89    2.29
#AS5_PARENTS      6.83       7.46    0.92
#AS6_PROMOTE     -5.57       5.03   -1.11
#AS7_GROUPING    -0.67       8.63   -0.08
#AS8_NATCOMP     -9.73       6.43   -1.51
#R-squared       31.75       2.08   15.24

# Only one is significant AS4_NATCOMP - shows highest (88%) in VN; 
CrossTable(DEVCON8b$AS4_NATCOMP, DEVCON8b$CNT,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

# I try the variable by itself
# How many non-missing values ?
T1b <- DEVCON8b[, c("AS4_NATCOMP")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,853
N0-N1 # 630 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

DEVCON8b$AS4_NATCOMP[DEVCON8b$SC18Q04==1] <- 1
DEVCON8b$AS4_NATCOMP[DEVCON8b$SC18Q04==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("AS4_NATCOMP")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# AS4_NATCOMP                0.6959687                0.8804194 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "AS4_NATCOMP"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   385.38       5.11   75.45
# VIETNAM       131.61       4.96   26.54
# AS4_NATCOMP    12.90       5.47    2.36
# R-squared      31.28       2.03   15.40


# SC19 Transparency of achievement data
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC19Q01", "SC19Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,541
N0-N1 # 942 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==1] <- 1
DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==2] <- 0

DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==1] <- 1
DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==2] <- 0


CrossTable(DEVCON8b$SC19Q02, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

mean1A <- t(sapply(DEVCON8b[c("SCORE_PUBLIC","SCORE_AUTHRITS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#SCORE_PUBLIC                  0.3384482                0.7553038 
#SCORE_AUTHRITS                0.7972169                0.8173018 


# I shall say this will be a good positive result
R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SCORE_PUBLIC","SCORE_AUTHRITS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# I knew it, I knew it !
# Estimate Std. Error t value
# (Intercept)      376.33       6.02   62.53
# VIETNAM          120.12       5.18   23.19
# SCORE_PUBLIC      28.88       4.62    6.25
# SCORE_AUTHRITS    11.53       6.41    1.80
# R-squared         33.00       2.19   15.09

# S20 # School Remedial classes Q01 Do you have Q05 For what purpose
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC20Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,853 for 20Q01 and 32833 for both
N0-N1 # 630 NAs for 20Q01  is okay but 15560 NAs for 19Q02 seems too many to me. 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

CrossTable(DEVCON8b$SC20Q01, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==1] <- 1
DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("SCL_EXTR_CL")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
# SCL_EXTR_CL                0.6501142                0.9590643 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SCL_EXTR_CL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

#Estimate Std. Error t value
#(Intercept)   377.34       2.58  145.98
#VIETNAM       128.22       5.39   23.80
#SCL_EXTR_CL    24.01       3.91    6.15
#R-squared      32.07       1.88   17.05


# S22 # School climate - using generated indices STUDCLIM and TEACCLIM
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("STUDCLIM","TEACCLIM")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,019 
N0-N1 # 464 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("STUDCLIM","TEACCLIM")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#STUDCLIM               0.03627657               0.03092815 
#TEACCLIM              -0.20352550              -0.09032238 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "STUDCLIM","TEACCLIM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
#Estimate Std. Error t value
#(Intercept)   392.29       2.19  179.32
#VIETNAM       136.20       4.85   28.07
#STUDCLIM        3.59       2.41    1.49
#TEACCLIM        0.20       3.13    0.06
#R-squared      31.12       1.96   15.91


# S24 # Parental expectations
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC24Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,285 
N0-N1 # 1198 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

CrossTable(DEVCON8b$SC24Q01, DEVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

# Need to make dummy variables
DEVCON8b$PARENT_EXT <- ifelse(DEVCON8b$SC24Q01==1,1,0)
DEVCON8b$PARENT_MED <- ifelse(DEVCON8b$SC24Q01==2,1,0)
DEVCON8b$PARENT_NEG <- ifelse(DEVCON8b$SC03Q01==3,1,0)

mean1A <- t(sapply(DEVCON8b[c("PARENT_EXT","PARENT_MED","PARENT_NEG")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#PARENT_EXT                0.2624817               0.39105806  
#PARENT_MED                0.4248264               0.51001416  
#PARENT_NEG                0.2264570               0.07586486 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "PARENT_EXT","PARENT_MED","PARENT_NEG"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   390.51       4.74   82.36
# VIETNAM       132.44       4.91   26.96
# PARENT_EXT     13.95       5.65    2.47
# PARENT_MED     -1.01       5.70   -0.18
# PARENT_NEG      6.14       5.05    1.22
# R-squared      31.44       2.14   14.68


# S25 # Parental activities 
# __________________________________________________________________________________________

# I don't want to mess with DEVCON8a, sort of master file
DEVCON8b <- DEVCON8a

# May generate some spurious data, but essentially it is safe to put 0 for na in this specific case of SC25
DEVCON8b$SC25Q01[is.na(DEVCON8a$SC25Q01)]  <- 0
DEVCON8b$SC25Q02[is.na(DEVCON8a$SC25Q02)]  <- 0
DEVCON8b$SC25Q03[is.na(DEVCON8a$SC25Q03)]  <- 0
DEVCON8b$SC25Q04[is.na(DEVCON8a$SC25Q04)]  <- 0
DEVCON8b$SC25Q05[is.na(DEVCON8a$SC25Q05)]  <- 0
DEVCON8b$SC25Q06[is.na(DEVCON8a$SC25Q06)]  <- 0
DEVCON8b$SC25Q07[is.na(DEVCON8a$SC25Q07)]  <- 0
DEVCON8b$SC25Q08[is.na(DEVCON8a$SC25Q08)]  <- 0
DEVCON8b$SC25Q09[is.na(DEVCON8a$SC25Q09)]  <- 0
DEVCON8b$SC25Q10[is.na(DEVCON8a$SC25Q10)]  <- 0
DEVCON8b$SC25Q11[is.na(DEVCON8a$SC25Q11)]  <- 0
DEVCON8b$SC25Q12[is.na(DEVCON8a$SC25Q12)]  <- 0

mean1A <- t(sapply(DEVCON8b[c("SC25Q01","SC25Q02","SC25Q03","SC25Q04","SC25Q05","SC25Q06","SC25Q07","SC25Q08","SC25Q09","SC25Q10",
                              "SC25Q11","SC25Q12")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.          mean in group 0         mean in group 1       p.value statistic.t
# SC25Q01                33.082022                 43.06211  2.510169e-62  -16.866584
# SC25Q02                46.778988                 48.44919  2.878031e-03   -2.981705
# SC25Q03                33.635329                 47.87435 9.889980e-132  -25.068590
# SC25Q04                44.653084                 50.81113  1.978941e-28  -11.115806
# SC25Q05                11.860789                 12.71522  2.578993e-02   -2.229931
# SC25Q06                14.854937                 13.21188  6.294937e-06    4.520129
# SC25Q07                 7.456976                 11.82339  1.101489e-26  -10.749665
# SC25Q08                11.917872                 38.95201  0.000000e+00  -44.800170
# SC25Q09                 9.809436                 18.71425  7.218298e-79  -19.112208
# SC25Q10                35.998075                 23.92299 1.783757e-101   21.790621
# SC25Q11                22.461400                59.941117  0.000000e+00  -57.980393
# SC25Q12                 4.059324                 2.080056  9.972566e-24   10.082187

# 1 and 3 are tiger moms
# Let's see the addition
DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

mean1A <- t(sapply(DEVCON8b[c("TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# TIGERMOM                   52.42340                 62.82434  8.773441e-63  -16.922853
# VOLUMOM                    34.41764                 39.09760  8.810321e-15   -7.775012
# TEACHMOM                   11.91787                 38.95201  0.000000e+00  -44.800170
# FUNDMOM                    22.46140                 59.94112  0.000000e+00  -57.980393
# COUNCILMOM                 35.99807                 23.92299 1.783757e-101   21.790621

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   380.98       4.23   90.05
# VIETNAM       110.87       6.01   18.45
# TIGERMOM        0.05       0.06    0.81
# VOLUMOM         0.01       0.08    0.18
# TEACHMOM       -0.06       0.09   -0.68
# FUNDMOM         0.40       0.06    6.14
# COUNCILMOM     -0.24       0.06   -3.68
# R-squared      30.04       2.15   13.98

# S26 # teacher morale - using generated indices TCMORALE
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("TCMORALE")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,361 
N0-N1 # 121 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("TCMORALE")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCMORALE               0.04139685                -0.300932 6.47916e-143    26.07069

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "TCMORALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
Estimate Std. Error t value
# (Intercept)   391.40       2.31  169.79
# VIETNAM       139.33       4.81   28.95
# TCMORALE        7.61       1.96    3.89
# R-squared      31.42       1.91   16.44

# Almost funny result - I guess Vietnamese principals are in self-improvement mode and non-braggiong mode,
# so the score on this index is much lower 0.04 compared to -0.30 for Vietnam, but the score is important in general
# so the gap would have increased because of this. 

# S27-29 # teacher focus - using generated indices TCFOCST
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("TCFOCST")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,381 
N0-N1 # 102 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("TCFOCST")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCFOCST                0.4931784                 0.132143

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "TCFOCST"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   392.19       2.91  134.94
# VIETNAM       135.89       4.90   27.73
# TCFOCST         2.56       2.26    1.13
# R-squared      30.83       1.95   15.83

# S30  teacher practice measured through student achievement
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC30Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,007 
N0-N1 # 476 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8b$TCM_STUASS[DEVCON8b$SC30Q01==1] <- 1
DEVCON8b$TCM_STUASS[DEVCON8b$SC30Q01==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("TCM_STUASS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCM_STUASS                0.8733971                0.9820528


R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TCM_STUASS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   361.03       7.52   48.02
# VIETNAM       126.42       5.75   21.98
# TCM_STUASS     24.46       7.76    3.15
# R-squared      28.12       2.19   12.85

# S30  teacher practice measured through teacher peer review of lesson plans, ass. instr., lessons
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC30Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,029
N0-N1 # 454 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8b$TCM_PEER[DEVCON8b$SC30Q02==1] <- 1
DEVCON8b$TCM_PEER[DEVCON8b$SC30Q02==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("TCM_PEER")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCM_PEER                0.7868354                0.8406937 

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TCM_PEER"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   368.96       6.51   56.68
# VIETNAM       128.87       5.64   22.84
# TCM_PEER       16.27       6.92    2.35
# R-squared      27.93       2.22   12.59

# S30  teacher practice measured through principal or senior staff class observation
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC30Q03")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,022
N0-N1 # 461 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8b$TCM_OBSER[DEVCON8b$SC30Q03==1] <- 1
DEVCON8b$TCM_OBSER[DEVCON8b$SC30Q03==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("TCM_OBSER")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCM_OBSER                0.7951141                0.9788264

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TCM_OBSER"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   378.89       4.21   90.06
# VIETNAM       127.57       5.77   22.12
# TCM_OBSER       5.04       4.96    1.02
# R-squared      27.28       2.25   12.14

# S30  teacher practice measured through inspector or other staff external to school obser
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC30Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,960
N0-N1 # 523 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector observation (INSPE)
DEVCON8b$TCM_INSPE[DEVCON8b$SC30Q04==1] <- 1
DEVCON8b$TCM_INSPE[DEVCON8b$SC30Q04==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("TCM_INSPE")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCM_INSPE                0.5877538                0.8685219 

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TCM_INSPE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   387.85       2.45  158.48
# VIETNAM       130.29       6.12   21.28
# TCM_INSPE      -7.98       4.45   -1.79
# R-squared      27.68       2.33   11.89

# S31  teacher incentives
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,276
N0-N1 # 2207 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# I will generate an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
SC31DAT <- DEVCON8b[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(SC31DAT, "SC31DAT.csv")
# Generated Winsteps output using Winsteps control+data file SC31a.txt
# Person file Output read back into R
SC31OUT.rda <- read.csv("C:/Country/Vietnam/Data/PISA/WINSTEPS/SC31DATOUT.csv")
# I merge back to the PISA data, except now I have to give it a c suffix.
# merge school and student datasets 
DEVCON8c <- merge(DEVCON8b,SC31OUT.rda,by="NEWID")
DEVCON8c$TCH_INCENTV <- rescale(DEVCON8c$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

mean1A <- t(sapply(DEVCON8c[c("TCH_INCENTV")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# TCH_INCENTV              -0.03169114                0.2687061

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8c,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   383.07       2.54  150.89
# VIETNAM       126.98       5.94   21.38
# TCH_INCENTV     7.49       2.73    2.74
# R-squared      27.84       2.21   12.60

# S32  School selection in admission
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC32Q01", "SC32Q03","SC32Q04","SC32Q05","SC32Q06","SC32Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,443
N0-N1 # 1040 
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# I will generate an OECD style rasch index that measures slectivity - too many factors means more selectivity
# One would expect to have positive correlation and low incidence for Vietnam so does not help with the gap
SC32DAT <- DEVCON8b[,c("NEWID","W_FSCHWT","SC32Q01", "SC32Q03","SC32Q04","SC32Q05","SC32Q06","SC32Q07")]
write.csv(SC32DAT, "SC32DAT.csv")
# Generated Winsteps output using Winsteps control+data file SC32a.txt
# Person file Output read back into R
SC32OUT.rda <- read.csv("C:/Country/Vietnam/Data/PISA/WINSTEPS/SC32DATOUT.csv")
DEVCON8c <- merge(DEVCON8b,SC32OUT.rda,by="NEWID")
DEVCON8c$SCL_SELECT <- rescale(DEVCON8c$WMLE_SC32, mean = 0, sd = 1,df=FALSE)

mean1A <- t(sapply(DEVCON8c[c("SCL_SELECT")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# SCL_SELECT              -0.03005038                0.2581987

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "SCL_SELECT"),
                   weight="W_FSTUWT",
                   data=DEVCON8c,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   382.32       2.54  150.27
# VIETNAM       128.44       5.41   23.73
# SCL_SELECT      5.71       2.31    2.47
# R-squared      27.98       2.22   12.63

# S33  School Autonomy and Teacher Participation - use indices SCHAUTON and TCHPARTI 
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SCHAUTON","TCHPARTI")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,474
N0-N1 # 9
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("SCHAUTON","TCHPARTI")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#SCHAUTON               -0.2701432                -1.047833       0    54.39068
#TCHPARTI               -0.2445780                -1.645921       0   139.24173

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "SCHAUTON","TCHPARTI"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   380.12       2.34  162.44
# VIETNAM       145.33       6.31   23.04
# SCHAUTON        1.68       2.31    0.73
# TCHPARTI        7.57       2.03    3.73
# R-squared      28.58       2.10   13.61


# S34  Leadership - There are 21 items, not all used in indices
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("LEADCOM","LEADINST","LEADPD","LEADTCH")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,118
N0-N1 # 365
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("LEADCOM","LEADINST","LEADPD","LEADTCH")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# LEADCOM                0.22250657               0.09037187  1.978252e-33   12.100226
# LEADINST               0.07304997              -0.04654632  9.789512e-17    8.329552
# LEADPD                 0.24451626              -0.04553364 9.328304e-101   21.665445
# LEADTCH                0.30658759              -0.28096952  0.000000e+00   42.041512

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "LEADCOM","LEADINST","LEADPD","LEADTCH"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   380.55       2.76  137.90
# VIETNAM       131.63       6.47   20.35
# LEADCOM         6.59       4.20    1.57
# LEADINST       -0.61       3.96   -0.15
# LEADPD         -2.92       3.07   -0.95
# LEADTCH         6.16       3.57    1.73
# R-squared      28.35       2.24   12.64


# S35  Professional Development mathematics - for math teachers I skip all staff
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON8a[, c("SC35Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,187
N0-N1 # 2296
# Data set with non NAs 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("SC35Q02")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# SC35Q02                   40.242                 49.81999

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "SC35Q02"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   372.14       3.34  111.49
# VIETNAM       126.26       5.90   21.38
# SC35Q02         0.26       0.07    3.91
# R-squared      28.86       2.13   13.52

# S39  Quality assurance/improvment activities - I SKIP IT
# __________________________________________________________________________________________
#

# S40  Curriculum standardization policies  - I skip it
# __________________________________________________________________________________________
#

# S44  Transferring students to other school for 6 reasons - I SKIP IT
# __________________________________________________________________________________________
#

# S45-S51  Financial Education - I SKIP IT
# __________________________________________________________________________________________
#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Now for the student questionnaires

# ST01 - Grade - I omit as an analytical variable - it seems to me to be a symbolic or nominal variable that mistakenly
# looks like a numerical or analytical one, driven by nomenclature and age at entry, we already capture that aspect
# through Preschool and Age variable 
#_______________________________________________________________

#How many non-missing values ?
T1b <- DEVCON8a[, c("ST01Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48483  data points 

CrossTable(DEVCON8a$ST01Q01, DEVCON8a$VIETNAM) 

# ST03 - converted into AGE
#_______________________________________________________________

#How many non-missing values ?
T1b <- DEVCON8a[, c("AGE")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48454  data points 
(N0-N1)

# I need to run the regression on data excluding the missing values
DEVCON8b <- DEVCON8a[complete.cases(T1b),]


mean1A <- t(sapply(DEVCON8b[c("AGE")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# AGE                 15.82227                  15.7704 1.372301e-32    11.95726

# I don't think that a slight younger age 15.77 as compared to 15.82 really matters, though shows statistically siginficant
# difference in mean values


R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "AGE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   443.04      44.90    9.87
# VIETNAM       128.28       5.62   22.81
# AGE            -3.77       2.91   -1.30
# R-squared      27.32       2.25   12.12

# ST04
#_______________________________________________________________
# Gender - convert to 01 Female variable

# How many non-missing values ?
T1b <- DEVCON8a[, c("ST04Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 48,483
N0-N1 # No NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# I generate dichotomous equivalents 

DEVCON8b$FEMALE[DEVCON8b$ST04Q01==1] <- 1
DEVCON8b$FEMALE[DEVCON8b$ST04Q01==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("FEMALE")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1   p.value statistic.t
# FEMALE                0.5271115                0.5339786 0.3584798  -0.9183345

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   385.74       2.72  141.61
# VIETNAM       128.17       5.67   22.60
# FEMALE         -4.79       1.92   -2.50
# R-squared      27.28       2.26   12.04

# Note that Females do much better in Language ! 

# Base Specification for READ
R1B <- pisa.reg.pv(pvlabel="READ", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8a,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   403.06       2.46  163.78
# VIETNAM       105.16       5.03   20.89
# R-squared      19.61       1.81   10.85

# And now for our test
R1B <- pisa.reg.pv(pvlabel="READ", 
                   x=c("VIETNAM",
                       "FEMALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
# Estimate Std. Error t value
# (Intercept)   386.46       2.88  134.21
# VIETNAM       104.35       4.91   21.25
# FEMALE         32.51       1.84   17.68
# R-squared      22.89       1.77   12.93


# ST05 - Attended Pre-school ?
#_______________________________________________________________

# How many non-missing values ?
T1b <- DEVCON8a[, c("ST05Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,119
N0-N1 # 1,364 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# PRESCHOOL                0.7866136                0.9115743 2.124727e-161   -27.73917
R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "PRESCHOOL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   363.44       3.27  111.20
# VIETNAM       123.91       4.88   25.41
# PRESCHOOL      45.36       3.60   12.61
# R-squared      35.57       1.70   20.97

# ST06 - Age at Pre School 
#_______________________________________________________________

# How many non-missing values ?
T1b <- DEVCON8a[, c("ST06Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,188
N0-N1 # 2,295 NAs
# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("ST06Q01")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# ST06Q01                 6.123829                 6.169105 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "ST06Q01"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   496.89       8.62   57.61
# VIETNAM       133.51       4.73   28.21
# ST06Q01       -16.29       1.40  -11.65
# R-squared      32.53       1.93   16.82


# ST07 - Grade Repetition 
#_______________________________________________________________
# 22,123 cases (nearly half missing for Upper Sec)
# Missing values convert to 0s 


T1b <- DEVCON8a[, c("ST07Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 44,698
N0-N1 # 3,785 NAs

T1b <- DEVCON8a[, c("ST07Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 # 44,002
N0-N1 # 4,481 NAs

T1b <- DEVCON8a[, c("ST07Q03")]
N1 <- NROW(na.omit(T1b)) 
N1 # 26,630
N0-N1 # 22,123 NAs

# Replace NAs with 1 (assume not repeaters)

DEVCON8b <- DEVCON8a  # Set the raw data back to base case

DEVCON8b$ST07Q01[is.na(DEVCON8b$ST07Q01)] <- 1 
DEVCON8b$ST07Q02[is.na(DEVCON8b$ST07Q02)] <- 1 
DEVCON8b$ST07Q03[is.na(DEVCON8b$ST07Q03)] <- 1 

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)


BLIX <- DEVCON8b[, c("STIDSTD", "ST07Q01", "ST07Q02", "ST07Q03", "REP1","REP2","REP3","REPEATER")]

CrossTable(DEVCON8b$REPEATER, DEVCON8b$VIETNAM, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE) 

mean1A <- t(sapply(DEVCON8b[c("REPEATER")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# REPEATER                0.1926064                0.0681589 

# Nearly 3 times as many student repeaters in G-7 countries. 
# CrossTable shows that of 8721 repaters in the sample, only 338 are Vietnamese  (4% as compared to 10% for whole sample)

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "REPEATER"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

Estimate Std. Error t value
(Intercept)   404.23       2.18  185.35
VIETNAM       128.66       4.42   29.12
REPEATER      -58.22       2.90  -20.05
R-squared      36.14       1.79   20.14

# The repeater impact is really driven outside, since so little repetition takes place in Vietnam. So partly the results in Vietnam 
# are explained by lack of repetition.


# ST08 - Truancy - 3 questions 
#_______________________________________________________________

T1b <- DEVCON8a[, c("ST08Q01","ST09Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 47,397
N0-N1 # 1086 NAs

# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("ST08Q01","ST09Q01","ST115Q01")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# ST08Q01                  1.517384                 1.187664  0.000000e+00    43.27607
# ST09Q01                  1.215726                 1.101719  8.519048e-88    20.12180
# ST115Q01                 1.256666                 1.075834 5.246634e-244    34.45785

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "ST08Q01","ST09Q01","ST115Q01"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   430.85       3.62  118.99
# VIETNAM       130.58       4.68   27.90
# ST08Q01        -7.10       1.30   -5.44
# ST09Q01       -18.28       1.95   -9.38
# ST115Q01       -3.94       1.89   -2.09
# R-squared      32.39       1.96   16.50



# ST11 - Family Structure - use index variable FAMSTRUC instead
#_______________________________________________________________

T1b <- DEVCON8a[, c("FAMSTRUC")]
N1 <- NROW(na.omit(T1b)) 
N1 # 39,747
N0-N1 # 8736 NAs

# Data set with non NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

CrossTable(DEVCON8b$FAMSTRUC, DEVCON8b$VIETNAM, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

# Does not seem worthwhile to construct dummies and clearly cannot use the FAMSTRC 1 2 3 as it is (78% are G-7 2s; 86% in VN)

#ST12 and ST16 Parents Occupation - I choose index variable HISEI  Parents highest level of occupation 
#_______________________________________________________________

T1b <- DEVCON8a[, c("HISEI")]
N1 <- NROW(na.omit(T1b)) 
N1 # 39,518
N0-N1 # 8,965 NAs  - Quite a bit ! If we use, we will need to examime more closely. 
DEVCON8b <- DEVCON8a[complete.cases(T1b),]


mean1A <- t(sapply(DEVCON8b[c("HISEI")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# HISEI                 40.41093                  26.5859 

# Prediction time - given such difference, if HISEI is meaningful at all, and the missing values do not bias the data,
# the dummy variable coefficient will increase


R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "HISEI"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   362.70       3.10  116.98
# VIETNAM       139.23       4.44   31.34
# HISEI           0.98       0.06   15.86
# R-squared      36.77       1.92   19.17

# ST13-14 and ST17-18 Parents education - MISCED and FISCED - at some point I want to relate daughters 
# especially to mothers - the result will be much stronger, I am sure of it. 
#_______________________________________________________________

T1b <- DEVCON8a[, c("MISCED","FISCED")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,061
N0-N1 # 2,422 NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]


mean1A <- t(sapply(DEVCON8b[c("MISCED","FISCED")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# MISCED                 3.120145                 2.174103 4.621116e-282    37.69513
# FISCED                 3.374157                 2.437850 5.287798e-263    36.30819

# This will be the same result as HISEI - wish we had the Tiger Mom variable on the whole sample ! 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "MISCED","FISCED"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   366.15       2.78  131.90
# VIETNAM       138.67       4.43   31.33
# MISCED          6.68       0.58   11.44
# FISCED          3.79       0.54    6.99
# R-squared      34.75       1.79   19.39

# Father's Job status - is a component of ESCS - but so is also occupation and education - and Alabania should be dropped
#_______________________________________________________________

T1b <- DEVCON8a[, c("ESCS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 43,400
N0-N1 # 5,083 NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("ESCS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# ESCS                -1.109775                -1.838403 

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "ESCS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   427.37       2.39  178.83
# VIETNAM       139.68       4.20   33.27
# ESCS           21.35       1.12   19.13
# R-squared      38.42       1.76   21.82

# ESCS takes all the fun away - I would rather at least include the key 
# elements together, ST15, 16 and ST19 - job statsus - there is no separate
# index - HISEU should be sufficient for effects other than eductaion of parents

# Immigrant background, language and so on are not issues of importance (ST20,21,25)

# ST26, ST27 - WEALTH CULTPOS HEDRES
#________________________________________________________
T1b <- DEVCON8a[, c("WEALTH","CULTPOS","HEDRES")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,812
N0-N1 # 1,671 NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

mean1A <- t(sapply(DEVCON8b[c("WEALTH","CULTPOS","HEDRES")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# WEALTH                -1.4669886               -2.1443732 5.428400e-287   38.201655
# CULTPOS               -0.1466573               -0.2367472  4.138649e-09    5.887337
# HEDRES                -0.7471896               -1.0814277 3.574865e-113   23.045154

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "WEALTH","CULTPOS","HEDRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   434.83       2.52  172.29
# VIETNAM       135.41       4.17   32.49
# WEALTH         12.79       1.10   11.66
# CULTPOS        -3.90       0.87   -4.50
# HEDRES         13.80       0.93   14.88
# R-squared      39.52       1.69   23.45

# ST28
#________________________________________________________

T1b <- DEVCON8a[, c("ST28Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 46,570
N0-N1 # 1,913 NAs
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
# Recode ordinal variables to cardinal
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==1]  <- 5
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==2]  <- 15
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==3]  <- 60
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==4]  <- 150
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==5]  <- 350
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==6]  <- 500

mean1A <- t(sapply(DEVCON8b[c("BOOK_N")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# BOOK_N                 53.00375                 50.91723

R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   389.32       2.04  191.14
# VIETNAM       134.20       4.83   27.76
# BOOK_N          0.11       0.01    8.47
# R-squared      32.02       1.88   16.99


