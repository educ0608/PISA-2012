# PISA2012_1c.R

# Prepared, Sunday, March 1, 2015

# Revised, Monday, March 2, 2015

library(intsvy) # For PISA analysis with PVs and BRRs
library(xlsx)# To generate MS-Excel output

# Oaxaca Blinder for MATHEMATICS


#Mathematics Regression
R_CM <- pisa.reg.pv(pvlabel="MATH", 
x=c("ASSESS","COGACT","LMINS","MMINS","SMINS",
    "BELONG","STUDREL","STUDCLIM","TEACCLIM",
    "LEADCOM","LEADPD","LEADTCH",
    "SCMATBUI","SCMATEDU","COMPWEB","RATCMP15",
    "SCHAUTON","SCHSEL","RESPCUR","RESPRES",
    "CREACTIV","MACTIV","STRATIO","SMRATIO",
    "PROPCERT","PROPQUAL",
    "TCHBEHFA","TCHBEHSO","TCHBEHTD","TEACHSUP",
    "TCMORALE","TCFOCST",
    "ATSCHL","ATTLNACT", 
    "EXAPPLM","EXPUREM","FAMCONC","PCGIRLS",
    "WEALTH","HOMEPOS","CULTPOS","HEDRES",
    "ICTRES","OUTHOURS","ANXMAT","SCMAT",
    "HISCED","HISEI"), 
     data=PISA_CO,
     export=FALSE)

R_VM <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("ASSESS","COGACT","LMINS","MMINS","SMINS",
                        "BELONG","STUDREL","STUDCLIM","TEACCLIM",
                        "LEADCOM","LEADPD","LEADTCH",
                        "SCMATBUI","SCMATEDU","COMPWEB","RATCMP15",
                        "SCHAUTON","SCHSEL","RESPCUR","RESPRES",
                        "CREACTIV","MACTIV","STRATIO","SMRATIO",
                        "PROPCERT","PROPQUAL",
                        "TCHBEHFA","TCHBEHSO","TCHBEHTD","TEACHSUP",
                        "TCMORALE","TCFOCST",
                        "ATSCHL","ATTLNACT", 
                        "EXAPPLM","EXPUREM","FAMCONC","PCGIRLS",
                        "WEALTH","HOMEPOS","CULTPOS","HEDRES",
                        "ICTRES","OUTHOURS","ANXMAT","SCMAT",
                        "HISCED","HISEI"), 
                    data=PISA_VN,
                    export=FALSE)

BETA_CM <- R_CM[1:49,1]  # as matrix
BETA_VM <- R_VM[1:49,1]

# Now for the X_ part
spec1 <- c("ASSESS","COGACT","LMINS","MMINS","SMINS",
           "BELONG","STUDREL","STUDCLIM","TEACCLIM",
           "LEADCOM","LEADPD","LEADTCH",
           "SCMATBUI","SCMATEDU","COMPWEB","RATCMP15",
           "SCHAUTON","SCHSEL","RESPCUR","RESPRES",
           "CREACTIV","MACTIV","STRATIO","SMRATIO",
           "PROPCERT","PROPQUAL",
           "TCHBEHFA","TCHBEHSO","TCHBEHTD","TEACHSUP",
           "TCMORALE","TCFOCST",
           "ATSCHL","ATTLNACT", 
           "EXAPPLM","EXPUREM","FAMCONC","PCGIRLS",
           "WEALTH","HOMEPOS","CULTPOS","HEDRES",
           "ICTRES","OUTHOURS","ANXMAT","SCMAT",
           "HISCED","HISEI")

X_CMj <- apply(PISA_CO[,spec1], 2, mean, na.rm = TRUE)

X_VMj <- apply(PISA_VN[,spec1], 2, mean, na.rm = TRUE)

X_CM <- append(1,X_CMj)
X_VM <- append(1,X_VMj)

# SPEC 1

SDIFF_SYS1 <- (BETA_VM - BETA_CM)
SDIFF_SYS <- (X_CM)%*%(SDIFF_SYS1)

SDIFF_END1 <- (X_VM - X_CM)
SDIFF_END <- SDIFF_END1%*%BETA_VM



# SPEC 2

SDIFFJ_SYS1 <- (BETA_CM-BETA_VM)
SDIFFJ_SYS <- (X_VM)%*%(SDIFFJ_SYS1)

SDIFFJ_END1 <- (X_VM - X_CM)
SDIFFJ_END <- (SDIFFJ_END1)%*%(BETA_CM)

R_CM
R_VM


SDIFF_SYS
SDIFF_END


SDIFFJ_SYS
SDIFFJ_END









