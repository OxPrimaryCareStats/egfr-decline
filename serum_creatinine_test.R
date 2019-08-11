#========================#
#                        #
#### SERUM CREATININE ####
#                        #
#========================#

# TODO: Units are not currently being assessed in this file. Check whether they need to be.
# TODO: Condiser using shift/lag in the catepillar.

# NB: Jason says the second date should be used to assign albuminuria status.
# NB: There are multiple tests per day for some people. The lowest result from each day is currenlty used.

### Entity Types ###
# 165 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Serum Creatinine"
# 204 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Urea - Blood"
# 213 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Blood Glucose"
# 239 - [1] TQU                                   - "Urea and Electrolytes"
# 257 - [1] TQU                                   - "Hand X-Ray"
# 266 - [1] TQU                                   - "Skull X-Ray"
# 288 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Other Laboratory Tests"
# 333 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Serum Electrolytes"
# 363 - [1] TQU                                   - "Lipoprotein Electrophoresis"
# 480 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Other Lab Result Information"

#############################
### Merge in Lookup Files ###
#############################

### Read Creatinine & eGFR Tests into R ###
setwd(test)
creat <- read_fst("serum_creatinine_test_all.fst", as.data.table = T)
setnames(creat, grep(".date$", names(creat), value = T), "event.date")
setnames(creat, "data2", "value")
creat <- creat[enttype %in% c(165, 288)] # Use only relevant or vague numerical entity types.
creat <- creat[, .(patid, event.date, data1, value, data3)] # Drop useless vars
creat <- creat[complete.cases(event.date, value)] # Drop observations with missing dates or values.
creat[, event.date := as.IDate(event.date, "%d%b%Y")] # Convert dates.
creat <- unique(creat) # Drop duplicated rows.

### Merge in Lookup Files ###
## Operator ##
setnames(creat, "data1", "code")
creat <- OPR[creat, on = "code"]
creat <- creat[, code := NULL]

## Specimen Unit of Measure ##
setnames(creat, "data3", "code")
creat <- SUM[creat, on = "code"]
creat <- creat[, code := NULL]

### Data Cleaning ###
setcolorder(creat, c("patid", "event.date", "operator", "value", "specimen.unit.of.measure"))
creat <- creat[is.na(operator) | operator %in% c("=", "Data Not Entered")]

### Drop Biologically Implausible Values ###
creat <- creat[value >= 17 & value <= 450]

###########################
### Convert to SI Units ###
###########################

# 6-Part MDRD SI Units = "umol/L"
# 6-Part MDRD Conventional Units = "mg/dL"
# 4-Part MDRD SI Units = "umol/L"
# 4-Part MDRD Conventional Units = "mg/dL"
# CKD-EPI Units = "mg/dL"

creat <- creat[, .(patid, event.date, value)]
setnames(creat, "value", "creat.umol.l")
creat <- creat[, creat.mg.dl := creat.umol.l / 88.4]

setorder(creat, patid, event.date, -creat.mg.dl)
creat <- creat[, .SD[1], by = .(patid, event.date)] # Take lowest stage (highest eGFR) per day.

#############################
### Merge in Patient File ###
#############################

setwd(core)
patient.practice <- read_fst("patient&practice.fst", as.data.table = T)
patient.practice <- patient.practice[, .(patid, gender, ethnicity, dat1.date, eliL.date, dob)]
creat <- creat[patient.practice, on = "patid"]; rm(patient.practice)

creat <- creat[event.date >= dat1.date & event.date <= eliL.date] # Drop in-eligibile data.
creat[, age := as.double(difftime(event.date, dob, units = "days")) / 365.25]

######################
### Calculate eGFR ###
######################

## 4-Part MDRD ##
#creat[, egfr.mdrd := 186 * (creat.umol.l * 0.0114)^-1.154 * age^-0.203 * ifelse(gender == "Female", 0.742, 1) * ifelse(ethnicity != "Black" | is.na(ethnicity), 1, 1.212)] # SI
creat[, egfr.mdrd := 186 * creat.mg.dl^-1.154 * age^-0.203 * ifelse(gender == "Female", 0.742, 1) * ifelse(ethnicity != "Black" | is.na(ethnicity), 1, 1.212)] # Conventional Units

## CKD-EPI ##
creat[, egfr.ckdepi := 141 * pmin(creat.mg.dl / ifelse(gender == "Female", 0.7, 0.9), 1)^ifelse(gender == "Female", -0.329, -0.411) * pmax(creat.mg.dl / ifelse(gender == "Female", 0.7, 0.9), 1)^-1.209 * 0.993^age * ifelse(gender == "Female", 1.018, 1) * ifelse(ethnicity != "Black" | is.na(ethnicity), 1, 1.159)]

#creat <- creat[(egfr.ckdepi <= 200 & egfr.mdrd <= 200) | is.na(egfr.mdrd) | is.na(egfr.ckdepi)), ] # Dump implausible values

### Output eGFR File ###
egfr <- creat[, .(patid, event.date, egfr.ckdepi, egfr.mdrd)]
setnames(egfr, "event.date", "egfr.date")
setwd(temp)
write_fst(egfr, "egfr_test.fst"); rm(egfr)

############################
### Calculate eGFR Stage ###
############################

### 4-Part MDRD ###
creat[, stage.mdrd := cut(egfr.mdrd,
                          breaks = c(seq(0, 60, 15), 90, Inf),
                          labels = c("G5", "G4", "G3b", "G3a", "G2", "G1"))]
creat[, stage.mdrd := ordered(stage.mdrd, levels = c("G1", "G2", "G3a", "G3b", "G4", "G5"))]

### CKD-Epi ###
creat[, stage.ckdepi := cut(egfr.ckdepi,
                            breaks = c(seq(0, 60, 15), 90, Inf),
                            labels = c("G5", "G4", "G3b", "G3a", "G2", "G1"))]
creat[, stage.ckdepi := ordered(stage.ckdepi, levels = c("G1", "G2", "G3a", "G3b", "G4", "G5"))]

### Drop Serum Creatinine Measures ###
creat <- creat[, .(patid, event.date, stage.mdrd, stage.ckdepi)]

# Should probably be a save line here!

###################################
### The Very Hungry Caterpillar ###
###################################

creat <- creat[, .(patid, event.date, stage.mdrd)] # Keep only useful columns
setnames(creat, "stage.mdrd", "stage")
setkey(creat, patid, event.date, stage)

### Guestimate Loop Upper Bound ###
n <- max(creat[, .N, by = patid][, N])

# Maybe this would be better served as a while loop!
creat.list <- list()
pb <- txtProgressBar(0, n, style = 3)
for (i in 1:n) {
  creat[, tdiff := event.date - event.date[1], by = patid] # Create tdiff
  
  ## Keep Values 90 Days Apart ##
  creat.temp <- creat[tdiff == 0 | tdiff >= 90] # Keep only values 90 days apart.
  creat.temp[, tdiff := NULL] # Drop "tdiff"
  
  ## Keep 1st 90-Day Couplet per Patient ##
  creat.temp.a <- creat.temp[, .SD[1], by = patid] # data.table command.
  creat.temp.b <- creat.temp[, .SD[2], by = patid] # data.table command.
  creat.temp <- rbind(creat.temp.a, creat.temp.b); rm(creat.temp.a, creat.temp.b)
  setorder(creat.temp, patid, event.date, na.last = T)
  
  ## Reshape Wide ##
  creat.temp[, event.date := event.date[1], by = patid] # Make all dates = 1st date.
  creat.temp[, test.num := rep(c("test1", "test2")) , by = patid] # Create reshape "key".
  creat.temp <- dcast(data = creat.temp,
                      formula = patid + event.date ~ test.num,
                      value.var = "stage") # Reshape.
  creat.temp <- creat.temp[complete.cases(test1, test2)] # Drop entries with no 2nd entry.
  if(!{nrow(creat.temp) >= 1}){
    break # No second tests for comparison.
  }
  
  ## Establish CKD Stage at 1st Date ##
  creat.temp[, stage := ordered(NA, levels = c("G1", "G2", "G3a", "G3b", "G4", "G5"))]
  creat.temp[test2 >= test1, stage := test1] # Leaves NAs unclassified
  creat.temp <- creat.temp[complete.cases(stage)] # Drop patients without consistent dianoses.
  creat.temp <- creat.temp[, .(patid, event.date, stage)]
  
  ## Save Object to List ##
  creat.list[[i]] <- creat.temp; rm(creat.temp)
  
  ## Drop First Row Per Group ##
  creat.drop <- creat[, .SD[1], by = patid]
  creat <- fsetdiff(creat, creat.drop); rm(creat.drop)
  creat <- creat[, .(patid, event.date, stage)]
  setTxtProgressBar(pb, i)
}
close(pb)
rm(creat, creat.temp, i, n, pb)

####################################
### Aggregate Caterpillar Output ###
####################################

creat <- rbindlist(creat.list); rm(creat.list)
creat[, stage := factor(as.character(stage), c("G1", "G2", "G3a", "G3b", "G4", "G5"))]

##############################################
### Establish Stage per QOF Financial Year ###
##############################################

### Keep Last Value of the QOF Financial Year ###
creat[, year := year(event.date)]
creat[month(event.date) <= 3, year := year - 1L]
setorder(creat, patid, -event.date)
creat <- creat[, .SD[1], by = .(patid, year)]
creat <- creat[, .(patid, year, stage)]
setnames(creat, "stage", "ckdstage.test")
setkey(creat, patid, year)

### Save Output ###
setwd(temp)
write_fst(creat, "ckd_stage.fst"); rm(creat)