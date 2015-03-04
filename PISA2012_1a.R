# PISA2012_1a.R

# Prepared, Friday, February 20, 2015

# Revised, Monday, March 2, 2015

library(foreign) # to import and export data from R
library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(HMISC)# What can I say about this Man Friday/factotum
library(TDMR)# Need this for tuning data mining in R - eg. detect column of constants in dataframe


# Import PISA data into R and generate Colombia and Vietnam extracts

student.rda <- read.dta("C:/Country/Vietnam/Data/PISA/RDATA/stu.dta")
STU12_CO_VN <- subset(student.rda,cnt==c("COL","VNM"))

school.rda <- read.dta("C:/Country/Vietnam/Data/PISA/RDATA/sch.dta")
SCH12_CO_VN <- subset(school.rda,cnt==c("COL","VNM"))

# Extract Vietnam and Colombia data
SCH12_CO_VN <- subset(SCH12_CO_VN,cnt== "COL" | cnt== "VNM")

# In need variable names in upper case for later
# application of INTSVY package
names(STU12_CO_VN) <- toupper(names(STU12_CO_VN))
names(SCH12_CO_VN) <- toupper(names(SCH12_CO_VN))

# merge school and student datasets 
PISA_CO_VN <- merge(STU12_CO_VN,SCH12_CO_VN,by="SCHOOLID")
# generate COL and VN extracts
PISA_CO <- subset(PISA_CO_VN,CNT.x== "COL")
PISA_VN <- subset(PISA_CO_VN,CNT.x== "VNM")

# Save the files
save(SCH12_CO_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/SCH12_CO_VN.rda")
save(STU12_CO_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/STU12_CO_VN.rda")
save(PISA_CO_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/PISA_CO_VN.rda")
save(PISA_CO, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/PISA_CO.rda")
save(PISA_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/PISA_VN.rda")


# Get summary of output -schools
stargazer(SCH12_CO_VN,
          type="latex", out="C:/Country/Vietnam/Data/PISA/LATEX/PISA2012_1a.rdata.pretex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

# Get summary of output -students
stargazer(STU12_CO_VN,
          type="latex", out="C:/Country/Vietnam/Data/PISA/LATEX/PISA2012_1a.rdata.pretex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

attach(STU12_CO_VN)
# Get Kernel Plots comparisons - Math Science Reading
# Need to convert country names to factors
CNT.f <- factor(CNT, levels=c("COL", "VNM"),labels = c("Colombia", "Vietnam"))

# SCIENCE
# I need nbins to be 0 as I am using a smoothing factor h
sm.density.compare(PV1SCIE,lwd=2,CNT.f,lty=c(2,1),col=c("blue","red"),
                   nbins=0,h=35,xlab="Science Score",weights=W_FSTUWT)
# Draw reference line for OECD average
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("blue","red")
legend(0,0.003, levels(CNT.f),lty=c(2,1), lwd=2, col=c("blue","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)

# MATHEMATICS
sm.density.compare(PV5MATH,lwd=2,CNT.f,lty=c(2,1),col=c("green","red"),
                   nbins=0,h=35,xlab="Mathematics Score",weights=W_FSTUWT)
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("green","red")
legend(0,0.003, levels(CNT.f),lty=c(2,1), lwd=2, col=c("green","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)


# READING
sm.density.compare(PV2READ,lwd=2,CNT.f,lty=c(2,1),col=c("purple","red"),
                   nbins=0,h=35,xlab="Reading Score",weights=W_FSTUWT)
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("purple","red")
legend(0,0.003, levels(CNT.f),lty=c(2,1), lwd=2, col=c("purple","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)


# Get ECDF- separately for VN and CO


# Generate Vietnam and Colombia extracts
vnpisa <- subset(PISA_CO_VN,CNT == "VNM")
copisa <- subset(PISA_CO_VN,CNT == "COL")


# ("ASSESS","COGACT","LMINS","MMINS","SMINS",

# Generate Colombia extract

# Now to plot ECDFs of sets of explanatory variables



plot(ecdf(copisa$ASSESS), verticals=TRUE, pch=46, col="black", lty=2, lwd=2, main=NULL, ylab="", xlab="")
plot(ecdf(vnpisa$ASSESS), verticals=TRUE, pch=46, col="red",lty=1,lwd=2, add=2, main=NULL, ylab="", xlab="")
legend(locator(1),levels(CNT.f),lty=c(2,1), lwd=2, col=c("black","red"),bty="n")
text(locator(1),'ASSESS',font=2,1)

summary(vnsch)


# Examine analysis of variance
results_vn <- lmer(PV1MATH ~1 + (1|stratum) + (1|stratum:SCHOOLID), data=vnstu,weights=W_FSTUWT)
summary(results_vn)


# Examine analysis of variance
results_co <- lmer(PV1MATH ~1 + (1|stratum) + (1|stratum:SCHOOLID), data=costu)
summary(results_co)


# I want to do t-test comparisons of the indices generated in PISA database

# I use grep on colnames to determine the order - fortunately they are all contiguously ordered 
# in the schools database ! 

grep("abgmath",colnames(SCH12_CO_VN))
grep("teacclim",colnames(SCH12_CO_VN))

# This tells me the order is 256-288 - I don't automate it for easier understanding of code

grep("clcuse1",colnames(STU12_CO_VN))
grep("ancsubnorm",colnames(STU12_CO_VN))

# This tells me the order is 404-500 - I don't automate it for easier understanding of code

# I initially thought of making two sperate dataframes, but it is not required 
indices_scl <- SCH12_CO_VN[,names(SCH12_CO_VN)[256:288]]
indices_stu <- STU12_CO_VN[,names(SCH12_CO_VN)[404:500]]

# I use sapply to apply t.test function many times

# First I do for the schools dataset, then I do for the students

comp_scl <- t(sapply(SCH12_CO_VN[256:288], function(x) 
  unlist(t.test(x~SCH12_CO_VN$CNT,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))
# I wanted to know what is the format of the output - it is a matrix
class(comp_scl)
# I give more reasonable names 
colnames(comp_scl) <- c("MEAN_COL","MEAN_VNM","P_VALUE", "T-STATISTIC", "CONFINT1", "CONFINT2") 
# I select those indices where significant difference at 5% level of significance
comp_scl_sig <- comp_scl[(which(abs(comp_scl[,4]) > 1.96)),]
# I export to MS-Excel
write.xlsx(comp_scl_sig, "C:/Country/Vietnam/PISA WP/comp_scl_sig.xlsx") 
# And generate input for Latex
xtable(comp_scl_sig)

# Now repeat above steps (except for class()) for STU
# Some variables are apparently all constant or all NAs so gives an error message !

# can not run :-( comp_stu <- t(sapply(STU12_CO_VN[404:500], function(x) 
#  unlist(t.test(x~STU12_CO_VN$CNT,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))

# I find out and eliminate all vars that are all NAs
DELS <- which((sapply(STU12_CO_VN, function(x)all(is.na(x)))))
STU12_CO_VN <- STU12_CO_VN[, -DELS] 
# delete 134 variables, but still has non-numerical variables in the choice range


# First I need to convert the CNT variable to a numeric one - as I am going to have to eliminate
# all non-numeric variables - I generate a new CNT_vn variable that has value 0 for col and 1 for vn

STU12_CO_VN$CNT_n[STU12_CO_VN$CNT=="COL"] <- 1
STU12_CO_VN$CNT_n[STU12_CO_VN$CNT=="VNM"] <- 2

# STU12_CO_VN now has 503 variables
# Then, I need to get rid of any non-numeric and non-logical vectors
temp1 <- STU12_CO_VN[,sapply(STU12_CO_VN,is.numeric)] # 476 out of 503 variables, none logical

# Second, I check for contant columns 
allsame1 <- tdmPreFindConstVar(temp1)
allsame1 # I find two - oecd and easy - I need to delete them from temp1
#Note the - selection !
temp1 <- subset(temp1, select=c(-EASY,-OECD))
# I will also get rid of two identifying variables on qres I do not really know
temp1 <- subset(temp1, select=c(-QUESTID, -BOOKID))
# Check the results - should be NULL now
allsame1 <- tdmPreFindConstVar(temp1)
allsame1
rm(allsame1) # Just needed to identify and then confirm NULL


# I need to  bring back the factor for grouping. 
temp1$CNT.f <- factor(temp1$CNT_n, levels=c(1,2),labels = c("Colombia", "Vietnam"))
table(temp1$CNT.f)


# I try to find out the locations of the variables I need
grep("CLCUSE1",colnames(temp1))
grep("ANCSUBNORM",colnames(temp1))

# I get 272 and 337 - and try to apply this
comp_stu <- t(sapply(temp1[272:337], function(x) 
 unlist(t.test(x~temp1$CNT.f,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))

colnames(comp_stu) <- c("MEAN_COL","MEAN_VNM","P_VALUE", "T-STATISTIC", "CONFINT1", "CONFINT2") 
# I select those indices where significant difference at 5% level of significance
comp_stu_sig <- comp_stu[(which(abs(comp_stu[,4]) > 1.96)),]
# I export to MS-Excel
write.xlsx(comp_stu_sig, "C:/Country/Vietnam/PISA WP/comp_stu_sig.xlsx") 
# And generate input for Latex
xtable(comp_stu_sig)






comp_stu <- t(sapply(temp1[c(389:400)], function(x) 
    unlist(t.test(x~temp1$CNT.f,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))
  
tests1 <- lapply(seq(1,ncol(Gf), by=2), 
                 function (x){t.test(Gf[,x],Gf[,x+1],paired=FALSE)})
print(tests1)
table(STU12_CO_VN[,c(450)])

CNT_f <- factor(STU12_CO_VN$CNT_n)
