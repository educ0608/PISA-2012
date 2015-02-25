# PISA2012_1a.R

# Prepared, Friday, February 20, 2015

# Revised, Wedneday, February 25, 2015

library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models

# Extract Vietnam and Colombia data

SCH12_CO_VN <- subset(sch,cnt== "COL" | cnt== "VNM")
STU12_CO_VN <- subset(stu,cnt==c("COL","VNM"))


# Save the files
save(SCH12_CO_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/SCH12_CO_VN.rda")
save(STU12_CO_VN, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/STU12_CO_VN.rda")

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
cnt.f <- factor(cnt, levels=c("COL", "VNM"),labels = c("Colombia", "Vietnam"))

# SCIENCE
# I need nbins to be 0 as I am using a smoothing factor h
sm.density.compare(pv1scie,lwd=2,cnt.f,lty=c(2,1),col=c("blue","red"),
                   nbins=0,h=35,xlab="Science Score",weights=w_fstuwt)
# Draw reference line for OECD average
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("blue","red")
legend(0,0.003, levels(cnt.f),lty=c(2,1), lwd=2, col=c("blue","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)

# MATHEMATICS
sm.density.compare(pv5math,lwd=2,cnt.f,lty=c(2,1),col=c("green","red"),
                   nbins=0,h=35,xlab="Mathematics Score",weights=w_fstuwt)
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("green","red")
legend(0,0.003, levels(cnt.f),lty=c(2,1), lwd=2, col=c("green","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)


# READING
sm.density.compare(pv2read,lwd=2,cnt.f,lty=c(2,1),col=c("purple","red"),
                   nbins=0,h=35,xlab="Reading Score",weights=w_fstuwt)
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
colfill<-c("purple","red")
legend(0,0.003, levels(cnt.f),lty=c(2,1), lwd=2, col=c("purple","red"),bty="n")
text(100,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)


# Get ECDF- separately for VN and CO

# Generate Vietnam extract
vnstu <- subset(STU12_CO_VN,cnt == "VNM")
vnsch <- subset(SCH12_CO_VN,cnt == "VNM")

# Generate Colombia extract
costu <- subset(STU12_CO_VN,cnt == "COL")
cosch <- subset(SCH12_CO_VN,cnt == "COL")

# Now to plot ECDFs of sets of explanatory variables
# HOME POSSESSIONS
plot(ecdf(vnstu$homepos), verticals=TRUE, pch=46, col="red",lty=2,lwd=2)
plot(ecdf(costu$homepos), verticals=TRUE, pch=46,add=T, lwd=2)

summary(vnsch)


# Examine analysis of variance
results_vn <- lmer(pv1math ~1 + (1|stratum) + (1|stratum:schoolid), data=vnstu,weights=w_fstuwt)
summary(results_vn)


# Examine analysis of variance
results_co <- lmer(pv1math ~1 + (1|stratum) + (1|stratum:schoolid), data=costu)
summary(results_co)


