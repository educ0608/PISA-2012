# PISA2012_1b.R

# Prepared, Sunday, March 1, 2015

# Revised, Monday, March 2, 2015

library(intsvy) # For PISA analysis with PVs and BRRs
library(xlsx)# To generate MS-Excel output

# Oaxaca Blinder for SCIENCE


#Science Regression
R_CS <- pisa.reg.pv(pvlabel="SCIE", 
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

R_VS <- pisa.reg.pv(pvlabel="SCIE", 
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

DF_BETA_CS <- subset(R_CS, select="Estimate")  # as dataframe

BETA_CS <- R_CS[1:49,1]  # as matrix
BETA_VS <- R_VS[1:49,1]

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

X_CSj <- apply(PISA_CO[,spec1], 2, mean, na.rm = TRUE)

X_VSj <- apply(PISA_VN[,spec1], 2, mean, na.rm = TRUE)

X_CS <- append(1,X_CSj)
X_VS <- append(1,X_VSj)

# SPEC 1

SDIFF_SYS1 <- (BETA_VS - BETA_CS)
SDIFF_SYS <- (X_CS)%*%(SDIFF_SYS1)

SDIFF_END1 <- (X_VS - X_CS)
SDIFF_END <- SDIFF_END1%*%BETA_VS



# SPEC 2

SDIFFJ_SYS1 <- (BETA_CS-BETA_VS)
SDIFFJ_SYS <- (X_VS)%*%(SDIFFJ_SYS1)

SDIFFJ_END1 <- (X_VS - X_CS)
SDIFFJ_END <- (SDIFFJ_END1)%*%(BETA_CS)

R_CS
R_VS


SDIFF_SYS
SDIFF_END


SDIFFJ_SYS
SDIFFJ_END









