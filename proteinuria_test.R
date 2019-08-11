#===================#
#                   #
#### ALBUMINURIA ####
#                   #
#===================#

# NB: Jason says the second date should be used to assign albuminuria status.
# NB: Using set operations to remove the last row per group is much faster than group_by(patid) %>% slice(-1) or data.table's equivalent functions.
# NB: There are multiple tests per day for some people. The lowest result from each day is currenlty used.

# TODO: Should 0 values for numerators (albumin or protein) be treated specially, irrespective of denominators?
# TODO: This doesn't currently reflect the guidelines, which states that ACRs between 3 mg/mmol & 70 mg/mmol require a subsequent test to determine the presence of albuminuria, whereas readings >=70 mg/mmol do not. No mention of temporal separation between test results is mentioned.
# TODO: Check with Tim/Dan as to whether 24hr creatinine outputs are of any use? Should we try to standardise 24hr protein/albumin outputs to these to create PCRs and ACRs?

### Read in Code-Lists ###
setwd(codelists)
medcodes <- fread("albuminuria.txt", na.strings = "")
medcodes <- medcodes[, .(medcode, readterm)]

medcodes.alb <- medcodes[grepl("albumin", readterm, ignore.case = T)]
medcodes.alb <- as.vector(medcodes.alb$medcode)

medcodes.pro <- medcodes[grepl("protein", readterm, ignore.case = T)]
medcodes.pro <- as.vector(medcodes.pro$medcode)

medcodes.ucreat <- medcodes[!grepl("albumin|protein", readterm, ignore.case = T)]; rm(medcodes)
medcodes.ucreat <- as.vector(medcodes.ucreat$medcode)

### Entity Types ###
# 152 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Albumin"
# 166 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Creatinine Clearance"
# 204 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Urea - Blood"
# 227 - [1] TQU                                   - "Mid-Stream Specimen of Urine"
# 229 - [1] TQU                                   - "Pregnancy Test"
# 239 - [1] TQU                                   - "Urea & Electrolytes"
# 240 - [1] TQU                                   - "Urine Test"
# 263 - [1] TQU                                   - "Ribs X-Ray"
# 264 - [1] TQU                                   - "Shoulder X-Ray"
# 275 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "HbA1c - Diabetic Control"
# 281 - [1] TQU                                   - "Blood Group Antibodies"
# 286 - [1] TQU                                   - "Urinalysis Glucose"
# 287 - [1] TQU                                   - "Urinalysis Protein"
# 288 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Other Laboratory Tests"
# 315 - [1] TQU                                   - "Chemical Function Tests"
# 340 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Urine Biochemistry"
# 403 - [1] TQU                                   - "Urine Cytology"
# 431 - [1] TQU                                   - "Urine Dipstick for Protein"
# 435 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Urine Microalbumin"
# 438 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Other Biochemistry Test"
# 467 - [1] TQU                                   - "Procedures, Specimens & Samples"
# 469 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Albumin:Creatinine Ratio"
# 480 - [1] OPR [2] value [3] SUM [4] TQU [7] POP - "Other Lab Result Information"
# 481 - [1] TQU                                   - "Biochemical Screening Tests"

### Define Regex Search Terms ###
## Rates ##
ps <- "^ps$|/ps$"
ns <- "^ns$|/ns$"
us <- "^us$|/us$"
ms <- "^ms$|/ms$"
s <- "^s$|/s$|^second$|/second$"
min <-"^min$|/min$|^minute$|/minute$"
hr <- "^h$|/h$|^hour$|/hour$|^hr$|/hr$"
hr12 <- "^12hrs$|/12hrs$"
day <- "^d$|/d$|^day$|/day$"
week <- "^week$|/week$"
month <- "^month$|/month$"
year <- "^year$|/year$"
rates <- paste(ps, ns, us, ms, s, min, hr, hr12, day, week, month, year, sep = "|"); rm(ps, ns, us, ms, s, min, hr, hr12, day, week, month, year)

## Weights ##
pg <- "^pg$|^pg/"
ng <- "^ng$|^ng/"
ug <- "^ug$|^ug/"
mg <- "^mg$|^mg/"
g <- "^g$|^g/"
kg <- "^kg$|^kg/"
stone <- "^stone$"
weights <- paste(pg, ng, ug, mg, g, kg, stone, sep = "|"); rm(pg, ng, ug, mg, g, kg, stone)

### Read Test Data into R ###
setwd(test)
alb.pro.ucreat <- read_fst("proteinuria_test_all.fst", as.data.table = T)

### Data Cleaning ###
setnames(alb.pro.ucreat, "data2", "value")
alb.pro.ucreat <- alb.pro.ucreat[enttype %in% c(275, 288, 340, 435, 438, 469, 480)] # Use relevant or vague numerical entity types.
alb.pro.ucreat[, event.date := as.IDate(event.date, "%d%b%Y")] # Convert dates.
alb.pro.ucreat <- alb.pro.ucreat[complete.cases(event.date, value)] # Drop rows with missing dates|values.
alb.pro.ucreat <- alb.pro.ucreat[, .(patid, event.date, enttype, medcode, readterm, data1, value, data3)]
alb.pro.ucreat <- unique(alb.pro.ucreat) # Drop duplicate rows.

### Merge in Lookup Files ###
## Operator ##
setnames(alb.pro.ucreat, "data1", "code")
alb.pro.ucreat <- OPR[alb.pro.ucreat, on = "code"]
alb.pro.ucreat <- alb.pro.ucreat[, code := NULL]

## Specimen Unit of Measure ##
setnames(alb.pro.ucreat, "data3", "code")
alb.pro.ucreat <- SUM[alb.pro.ucreat, on = "code"]
alb.pro.ucreat <- alb.pro.ucreat[, code := NULL]

### Further Data Cleaning ###
alb.pro.ucreat <- alb.pro.ucreat[, .(patid, event.date, enttype, medcode, readterm, operator, value, specimen.unit.of.measure)]
alb.pro.ucreat <- alb.pro.ucreat[is.na(operator) | operator %in% c("=", "Data Not Entered")] # Drop non-specific values.
alb.pro.ucreat <- alb.pro.ucreat[, .(patid, event.date, enttype, medcode, readterm, value, specimen.unit.of.measure)] # Drop operators.

#################
### Filtering ###
#################

### Identification via Medical Codes ###
alb <- alb.pro.ucreat[medcode %in% medcodes.alb]; rm(medcodes.alb)
pro <- alb.pro.ucreat[medcode %in% medcodes.pro]; rm(medcodes.pro)
ucreat <- alb.pro.ucreat[medcode %in% medcodes.ucreat]; rm(medcodes.ucreat, alb.pro.ucreat)

### Filtering via Specimen Unit of Measure ###
acr <- alb[specimen.unit.of.measure %in% c("mg/mmol", "g/mol", "No Data Entered", "mg/mmol(creat)", "ratio", "1/1", "%")]
aer <- alb[grepl(rates, specimen.unit.of.measure, ignore.case = T)]
alb <- alb[specimen.unit.of.measure %in% c("mg/L", "g/L")]
pcr <- pro[specimen.unit.of.measure %in% c("mg/mmol", "g/mol", "No Data Entered", "mg/mmol(creat)", "ratio", "1/1", "%")]
per <- pro[grepl(rates, specimen.unit.of.measure, ignore.case = T)]
pro <- pro[specimen.unit.of.measure %in% c("mg/L", "g/L")]
ucreat <- ucreat[specimen.unit.of.measure %in% c("mmol/L", "umol/L")]

### Change/Convert Units ###
acr[, specimen.unit.of.measure := "mg/mmol(creat)"] # Everything is actually "mg/mmol(creat)".

aer[, value := mapply(time.denom, value, specimen.unit.of.measure, "day")]
aer[, specimen.unit.of.measure := gsub(rates, "/day", specimen.unit.of.measure)]
aer[, value := mapply(weight.num, value, specimen.unit.of.measure, "mg")]
aer[, specimen.unit.of.measure := gsub(weights, "mg/", specimen.unit.of.measure)]
aer <- aer[specimen.unit.of.measure == "mg/day"]

alb[, value := ifelse(specimen.unit.of.measure == "g/L", value * 1000, value)]
alb[, specimen.unit.of.measure := ifelse(specimen.unit.of.measure == "g/L", "mg/L", specimen.unit.of.measure)]

pcr[, specimen.unit.of.measure := "mg/mmol(creat)"] # Everything is actually "mg/mmol(creat)".

per[, value := mapply(time.denom, value, specimen.unit.of.measure, "day")]
per[, specimen.unit.of.measure := gsub(rates, "/day", specimen.unit.of.measure)]
per[, value := mapply(weight.num, value, specimen.unit.of.measure, "mg")]
per[, specimen.unit.of.measure := gsub(weights, "mg/", specimen.unit.of.measure)]
per <- per[specimen.unit.of.measure == "mg/day"]

pro[, value := ifelse(specimen.unit.of.measure == "g/L", value * 1000, value)]
pro[, specimen.unit.of.measure := ifelse(specimen.unit.of.measure == "g/L", "mg/L", specimen.unit.of.measure)]

ucreat[, value := ifelse(specimen.unit.of.measure == "umol/L", value * 1000, value)]
ucreat[, specimen.unit.of.measure := ifelse(specimen.unit.of.measure == "umol/L", "mmol/L", specimen.unit.of.measure)]

rm(rates)
rm(weights)

### Drop Biologically Implausible Values ###
acr <- acr[value <= 150]
alb <- alb[value <= 500]

pcr <- pcr[value <= 800]
pro <- pro[value <= 4000]

ucreat <- ucreat[value <= 50]

#################
### Compiling ###
#################

### Pre-Merge Renaming ###
alb <- alb[, .(patid, event.date, value, specimen.unit.of.measure)]
setnames(alb, c("value", "specimen.unit.of.measure"), c("value.alb", "units.alb"))

pro <- pro[, .(patid, event.date, value, specimen.unit.of.measure)]
setnames(pro, c("value", "specimen.unit.of.measure"), c("value.pro", "units.pro"))

ucreat <- ucreat[, .(patid, event.date, value, specimen.unit.of.measure)]
setnames(ucreat, c("value", "specimen.unit.of.measure"), c("value.ucreat", "units.ucreat"))

### Merge ALbumin/Protein-Based Measures with Creatinine-Based Ones ###
alb.ucreat <- ucreat[alb, nomatch = 0, on = c("patid", "event.date")]; rm(alb) # Inner join.
pro.ucreat <- ucreat[pro, nomatch = 0, on = c("patid", "event.date")]; rm(pro, ucreat) # Inner join.

### Calculate Albumin:Creatinine Ratios ###
alb.ucreat[, value := value.alb / value.ucreat]
alb.ucreat[, specimen.unit.of.measure := "mg/mmol(creat)"]
alb.ucreat <- alb.ucreat[, .(patid, event.date, value, specimen.unit.of.measure)]

### Calculate Protein:Creatinine Ratios ###
pro.ucreat[, value := value.pro / value.ucreat]
pro.ucreat[, specimen.unit.of.measure := "mg/mmol(creat)"]
pro.ucreat <- pro.ucreat[, .(patid, event.date, value, specimen.unit.of.measure)]

### Drop Biologically Implausible Values ###
alb.ucreat <- alb.ucreat[value <= 150]
pro.ucreat <- pro.ucreat[value <= 800]

### Pre-Merge Renaming ###
acr <- acr[, .(patid, event.date, value, specimen.unit.of.measure)]
setnames(acr, "value", "value.natural")

pcr <- pcr[, .(patid, event.date, value, specimen.unit.of.measure)]
setnames(pcr, "value", "value.natural")

setnames(alb.ucreat, "value", "value.calculated")

setnames(pro.ucreat, "value", "value.calculated")

### Merge Natural & Calcualted ACR & PCR Values ###
acr <- merge(acr, alb.ucreat, all = T, by = c("patid", "event.date", "specimen.unit.of.measure")); rm(alb.ucreat)
acr[, value := value.calculated]
acr[, value := ifelse(complete.cases(value.natural), value.natural, value)]
acr <- acr[, .(patid, event.date, value, specimen.unit.of.measure)]

pcr <- merge(pcr, pro.ucreat, all = T, by = c("patid", "event.date", "specimen.unit.of.measure")); rm(pro.ucreat)
pcr[, value := value.calculated]
pcr[, value := ifelse(complete.cases(value.natural), value.natural, value)]
pcr <- pcr[, .(patid, event.date, value, specimen.unit.of.measure)]

###################################
### Calculate Albuminuria Stage ###
###################################

acr[, stage := ifelse(value < 3, 1L, NA)] # A1
acr[, stage := ifelse(value >= 3 & value <= 30, 2L, stage)] # A2
acr[, stage := ifelse(value > 30, 3L, stage)] # A3
acr[, stage := factor(stage, levels = 1:3, labels = c("A1", "A2", "A3"))]
acr <- acr[, .(patid, event.date, stage)]

pcr[, stage := ifelse(value < 15, 1L, NA)] # A1
pcr[, stage := ifelse(value >= 15 & value <= 50, 2L, stage)] # A2
pcr[, stage := ifelse(value > 50, 3L, stage)] # A3
pcr[, stage := factor(stage, levels = 1:3, labels = c("A1", "A2", "A3"))]
pcr <- pcr[, .(patid, event.date, stage)]

aer[, stage := ifelse(value < 30, 1L, NA)] # A1
aer[, stage := ifelse(value >= 30 & value <= 300, 2L, stage)] # A2
aer[, stage := ifelse(value > 300, 3L, stage)] # A3
aer[, stage := factor(stage, levels = 1:3, labels = c("A1", "A2", "A3"))]
aer <- aer[, .(patid, event.date, stage)]

per[, stage := ifelse(value < 150, 1L, NA)] # A1
per[, stage := ifelse(value >= 150 & value <= 500, 2L, stage)] # A2
per[, stage := ifelse(value > 500, 3L, stage)] # A3
per[, stage := factor(stage, levels = 1:3, labels = c("A1", "A2", "A3"))]
per <- per[, .(patid, event.date, stage)]

alb.stage <- rbind(acr, pcr, aer, per); rm(acr, pcr, aer, per)

### Ensure Each Patient Has One Estimate Per Day ###
setkey(alb.stage, patid, event.date, stage)
alb.stage <- alb.stage[, .SD[1], by = .(patid, event.date)] # Take lowest albuminuria value per day.

setwd(temp)
write_fst(alb.stage, "proteinuria_test.fst")

#############################
### Merge in Patient File ###
#############################

setwd(core)
patient.practice <- read_fst("patient&practice.fst", as.data.table = T)
patient.practice <- patient.practice[, .(patid, dat1.date, eliL.date)]
alb.stage <- alb.stage[patient.practice, on = "patid"]; rm(patient.practice)

alb.stage <- alb.stage[event.date >= dat1.date & event.date <= eliL.date] # Drop in-eligibile data.
alb.stage <- alb.stage[, .(patid, event.date, stage)]

##############################################
### Establish Stage per QOF Financial Year ###
##############################################

### Keep Last Value of the QOF Financial Year ###
alb.stage[, year := year(event.date)]
alb.stage[, year := ifelse(month(event.date) <= 3, year - 1L, year)]
setorder(alb.stage, patid, -event.date)
alb.stage <- alb.stage[, .SD[1], by = .(patid, year)]
alb.stage <- alb.stage[, .(patid, year, stage)]
setnames(alb.stage, "stage", "albstage.test")
setkey(alb.stage, patid, year)

### Save Output ###
setwd(temp)
write_fst(alb.stage, "proteinuria_stage.fst"); rm(alb.stage)