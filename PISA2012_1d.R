# PISA2012_1d.R

# Prepared, Sunday, March 1, 2015

# Revised, Monday, March 2, 2015

library(intsvy) # For PISA analysis with PVs and BRRs
library(xlsx)# To generate MS-Excel output

# Oaxaca Blinder for MATHEMATICS


#Reading Regression
R_CR <- pisa.reg.pv(pvlabel="READ", 
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

R_VR <- pisa.reg.pv(pvlabel="READ", 
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

BETA_CR <- R_CR[1:49,1]  # as matrix
BETA_VR <- R_VR[1:49,1]

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

X_CRj <- apply(PISA_CO[,spec1], 2, mean, na.rm = TRUE)

X_VRj <- apply(PISA_VN[,spec1], 2, mean, na.rm = TRUE)

X_CR <- append(1,X_CRj)
X_VR <- append(1,X_VRj)

# SPEC 1

SDIFF_SYS1 <- (BETA_VR - BETA_CR)
SDIFF_SYS <- (X_CR)%*%(SDIFF_SYS1)

SDIFF_END1 <- (X_VR - X_CR)
SDIFF_END <- SDIFF_END1%*%BETA_VR



# SPEC 2

SDIFFJ_SYS1 <- (BETA_CR-BETA_VR)
SDIFFJ_SYS <- (X_VR)%*%(SDIFFJ_SYS1)

SDIFFJ_END1 <- (X_VR - X_CR)
SDIFFJ_END <- (SDIFFJ_END1)%*%(BETA_CR)

R_CR
R_VR


SDIFF_SYS
SDIFF_END


SDIFFJ_SYS
SDIFFJ_END









