#=============#
#             #
#### SETUP ####
#             #
#=============#

################################
### Patient & Practice Files ###
################################

### Load Patient Data into R ###
setwd(base)
patient.practice <- read_fst("patient&practice.fst", as.data.table = T)

### Pseudo-Random Sample ###
set.seed(10)
if(datacut == "10k") {
  patient.practice <- patient.practice[sample(.N, 10e3)]
}
if(datacut == "50k") {
  patient.practice <- patient.practice[sample(.N, 50e3)]
}
if(datacut == "100k") {
  patient.practice <- patient.practice[sample(.N, 100e3)]
}

### Data Cleaning ###
setnames(patient.practice, "deathdate", "death.date")
patient.practice[, gender := ifelse(gender == 2, 1L, 0L)]
patient.practice[, gender := factor(gender, levels = 0:1, labels = c("Male", "Female"))]
patient.practice[, dob := paste("01", "01", yob + 1800L, sep = "/")]
vars <- c("dob", "frd", "crd", "tod", "death.date", "lcd", "uts")
setDTthreads(1L)
patient.practice[, (vars) := mclapply(.SD, as.IDate, format = "%d/%m/%Y"), .SDcols = vars]; rm(vars)
setDTthreads(0L)
patient.practice <- patient.practice[tod <= as.IDate("2016-01-01") | is.na(tod)] # Data entry errors.
patient.practice <- patient.practice[, .(patid, pracid, region, gender, dob, frd, crd, tod, death.date, lcd, uts)]

###########################
### Practice SHA Region ###
###########################

### Merge Datasets ###
setnames(patient.practice, "region", "code")
setkey(patient.practice, patid, code)
patient.practice <- PRG[patient.practice, on = "code"]

### Data Cleaning ###
setnames(patient.practice, "practice.region", "region")
regions <- c("London", "East Midlands", "East of England", "North East", "North West", "South Central", "South East Coast", "South West", "West Midlands", "Yorkshire & The Humber", "Northern Ireland", "Scotland", "Wales")
patient.practice[, region := factor(region, regions, regions)]; rm(regions)
patient.practice <- patient.practice[, .(patid, pracid, region, gender, dob, frd, crd, tod, death.date, lcd, uts)]
setkey(patient.practice, patid, pracid)

##########################
### ONS Mortality Data ###
##########################

### Load ONS Mortality Data into R ###
setwd(linkage)
death <- read_fst("death_patient.fst", as.data.table = T)

### Data Cleaning ###
setnames(death, names(death), gsub("_", "\\.", names(death)))
death <- death[, c("dod", "dod.partial") := .(as.IDate(dod, "%d/%m/%Y"), as.IDate(dod.partial, "%d/%m/%Y"))]
death <- death[match.rank <= 3, ] # Revise this strategy!
death <- death[, .(patid, pracid, dod)]
setkey(death, patid, pracid)

### Merge Patient & Mortality Data ###
patient.practice <- death[patient.practice, on = c("patid", "pracid")]; rm(death)

#################
### Ethnicity ###
#################

# 1 = "White"
# 2 = "Asian"
# 3 = "Black"
# 4 = "Mixed"
# 5 = "Other"

### Load Ethnicity Data into R ###
setwd(base)
ethnicity <- read_fst("ethnic_group.fst", as.data.table = T)

### Data Cleaning ###
setnames(ethnicity, names(ethnicity), gsub("_", "\\.", names(ethnicity)))
ethnicity[, ethnic.group := factor(ethnic.group, labels = c("White", "Asian", "Black", "Mixed", "Other"))]

### Merge Patient & Ethnicity Data ###
patient.practice <- ethnicity[patient.practice, on = "patid"]; rm(ethnicity)

##############################
### Hospital Episode Stats ###
##############################

### Load HES Data into R ###
setwd(linkage)
hes.basic <- read_fst("hes_patient_data.fst", as.data.table = T)
hes.int <- read_fst("hes_diagnosis_hosp_integrated.fst", as.data.table = T)

### Merge HES Files ###
setkey(hes.basic, patid)
setkey(hes.int, patid)
hes <- merge(hes.basic, hes.int, by = "patid", all = T); rm(hes.basic, hes.int)

### Data Cleaning ###
setnames(hes, names(hes), tolower(gsub("_", "\\.", names(hes))))
setnames(hes, "gen.ethnicity", "ethnic.group.hes")
hes <- hes[match.rank <= 3] # Revise this strategy.
hes[, discharged := as.IDate(discharged, format = "%d/%m/%Y")]
hes <- hes[, .(patid, pracid, ethnic.group.hes, discharged, icd)]

### Generate HES Ethnicity Variable ###
# 1 = "White"; 2 = "Asian"; 3 = "Black"; 4 = "Mixed"; 5 = "Other"
hes[, ethnicity.hes := as.integer(NA)]
hes[ethnic.group.hes == "White", ethnicity.hes := 1L]
hes[ethnic.group.hes %in% c("Bangladeshi", "Chinese", "Indian", "Oth_Asian", "Pakistani"), ethnicity.hes := 2L]
hes[ethnic.group.hes %in% c("Bl_Afric", "Bl_Carib", "Bl_Other"), ethnicity.hes := 3L]
hes[ethnic.group.hes == "Mixed", ethnicity.hes := 4L]
hes[ethnic.group.hes == "Other", ethnicity.hes := 5L]
hes[, ethnicity.hes := factor(ethnicity.hes, labels = c("White", "Asian", "Black", "Mixed", "Other"))]
hes <- hes[, .(patid, pracid, ethnicity.hes, discharged, icd)]
hes <- unique(hes)
setkey(hes, patid, pracid, discharged)

##########################################
### Hospital Episode Stats - Ethnicity ###
##########################################

### Create HES Ethnicity Subset ###
hes.ethnicity <- hes[, .(patid, pracid, ethnicity.hes)]
setkey(hes.ethnicity, patid, pracid)
hes.ethnicity <- unique(hes.ethnicity)

### Merge Patient & HES Ethnicity Data ###
patient.practice <- hes.ethnicity[patient.practice, on = c("patid", "pracid")]; rm(hes.ethnicity)

### Reconcile Ethnicity Differences ###
patient.practice[, ethnicity := ifelse(is.na(ethnic.group), ethnicity.hes, ethnic.group)]
patient.practice[, ethnicity := factor(ethnicity, labels = c("White", "Asian", "Black", "Mixed", "Other"))]
patient.practice <- patient.practice[, c("ethnicity.hes", "ethnic.group") := NULL]
setkey(patient.practice, patid, pracid)

##################
### IMD Scores ###
##################

### Load IMD Scores into R ###
setwd(linkage)
imd <- read_fst("patient_imd.fst", as.data.table = T)

### Data Cleaning ###
setnames(imd, names(imd), gsub("_", "\\.", names(imd)))
setnames(imd, "imd2010.5", "imd")
imd[, imd := factor(imd)]
setkey(imd, patid, pracid)

### Merge Patient & IMD Data ###
patient.practice <- imd[patient.practice, on = c("patid", "pracid")]; rm(imd)

#####################
### Dialysis Data ###
#####################

### Load Renal Dialysis Data into R ###
setwd(base)
dialysis <- read_fst("renal_dialysis.fst", as.data.table = T)

### Data Cleaning ###
dialysis[, dialysis.date := as.IDate(dialysis.date, "%d%b%Y")]
dialysis <- dialysis[, .(patid, dialysis.date)]
setkey(dialysis, patid)

### Merge Patient & Dialysis Data ###
patient.practice <- dialysis[patient.practice, on = "patid"]; rm(dialysis)

#########################################
### Hospital Episode Stats - Dialysis ###
#########################################

### Create HES Dialysis Subset ###
hes.dialysis <- hes[complete.cases(discharged)]
hes.dialysis <- hes.dialysis[, .(patid, pracid, discharged, icd)]
setkey(hes.dialysis, patid, pracid, icd)

### Read ICD-10CM Code-Lists into R ###
setwd(codelists)
dialysis.icdcodes <- fread("renal_dialysis_icd-10.txt", na.strings = "")

### Data Cleaning ###
dialysis.icdcodes <- dialysis.icdcodes[, .(code, description)]
setnames(dialysis.icdcodes, "code", "icd")
setkey(dialysis.icdcodes, icd)

### Merge Code-Lists with HES Data ###
hes.dialysis <- dialysis.icdcodes[hes.dialysis, nomatch = 0, on = "icd"]; rm(dialysis.icdcodes) # Inner join.

### Keep First HES Exclusion Date per Patient ###
setkey(hes.dialysis, patid, discharged)
hes.dialysis <- hes.dialysis[, .SD[1], by = patid]

### Pre-Merge Edits ###
setnames(hes.dialysis, "discharged", "dialysis.date.hes")
hes.dialysis <- hes.dialysis[, .(patid, pracid, dialysis.date.hes)]
setkey(hes.dialysis, patid, pracid)

### Merge Patient & HES Data ###
patient.practice <- hes.dialysis[patient.practice, on = c("patid", "pracid")]; rm(hes.dialysis)

######################################
### Renal Replacement Therapy Data ###
######################################

### Load Renal Replacement Data into R ###
setwd(base)
renal.replacement <- read_fst("renal_replacement.fst", as.data.table = T)

### Data Cleaning ###
setnames(renal.replacement, "renalreplacementdate", "rrt.date")
renal.replacement[, rrt.date := as.IDate(rrt.date, "%d%b%Y")]
renal.replacement <- renal.replacement[, .(patid, rrt.date)]
setkey(renal.replacement, patid)

### Merge Patient & Dialysis Date ###
patient.practice <- renal.replacement[patient.practice, on = "patid"]; rm(renal.replacement)

##########################################################
### Hospital Episode Stats - Renal Replacement Therapy ###
##########################################################

### Create HES RRT Subset ###
hes.rrt <- hes[complete.cases(discharged)]
hes.rrt <- hes.rrt[, .(patid, pracid, discharged, icd)]

### Read ICD-10CM Code-Lists into R ###
setwd(codelists)
rrt.icdcodes <- data.table(read.delim("renal_replacement_icd-10.txt", stringsAsFactors = F))

### Data Cleaning ###
setnames(rrt.icdcodes, "code", "icd")
rrt.icdcodes <- rrt.icdcodes[, .(icd, description)]

### Merge Code-Lists with HES Data ###
hes.rrt <- rrt.icdcodes[hes.rrt, nomatch = 0, on = "icd"]; rm(rrt.icdcodes)

### Keep First HES Exclusion Date per Patient ###
setkey(hes.rrt, patid, discharged)
hes.rrt <- hes.rrt[, .SD[1], by = patid]

### Pre-Merge Edits ###
setnames(hes.rrt, "discharged", "rrt.date.hes")
hes.rrt <- hes.rrt[, .(patid, pracid, rrt.date.hes)]
setkey(hes.rrt, patid, pracid)

### Merge Patient & HES Data ###
patient.practice <- hes.rrt[patient.practice, on = c("patid", "pracid")]; rm(hes, hes.rrt)

#######################
### Pregnancy Dates ###
#######################

### Load Pregnancy Data into R ###
setwd(base)
preg <- read_fst("preg.fst", as.data.table = T)

### Data Cleaning ###
setnames(preg, "p1date", "p1.date")
preg[, p1.date := as.IDate(p1.date, "%d%b%Y")]
preg <- preg[, .(patid, p1.date)]
setkey(preg, patid)

### Merge Patient & Pregnancy Dates ###
patient.practice <- preg[patient.practice, on = "patid"]; rm(preg)

######################################
### Reconcile CPRD & Linkage Dates ###
######################################

### Select the Earliest Event Dates from CPRD & Linkages ###
patient.practice[, rrt.date := pmin(rrt.date, rrt.date.hes, na.rm = T)]
patient.practice[, dialysis.date := pmin(dialysis.date, dialysis.date.hes, na.rm = T)]
patient.practice[, death.date := pmin(death.date, dod, na.rm = T)]

### Drop Useless Variables ###
patient.practice <- patient.practice[, grep("\\.hes$|^dod$", names(patient.practice), value = T) := NULL]

#############################
### Generate Date Windows ###
#############################

### Study Censor Dates ###
patient.practice[, censor.date := pmin(p1.date, rrt.date, dialysis.date, death.date, qofL.date, na.rm = T)]

### First & Last Eligible Data Windows ###
patient.practice[, dat1.date := pmax(frd, crd, uts, na.rm = T)] # Data eligibility
patient.practice[, eli1.date := dat1.date + 365L] # Patient eligibility
patient.practice[, eliL.date := pmin(tod, lcd, censor.date, na.rm = T)] # Patient & data eligibility.

### Drop Ineligible Patients ###
patient.practice <- patient.practice[eli1.date <= eliL.date]

### Repair Broken IDates ###
vars <- c("dob", "dat1.date", "eli1.date", "eliL.date")
setDTthreads(1L)
patient.practice[, (vars) := mclapply(.SD, as.integer), .SDcols = vars]
patient.practice[, (vars) := mclapply(.SD, as.IDate, origin = "1970-01-01"), .SDcols = vars]
setDTthreads(0L)
rm(vars)

###############################
### Save Final File Version ###
###############################

### Specify Variables to Keep ###
patient.practice <- patient.practice[, .(patid, pracid, region, gender, ethnicity, imd, dob, dat1.date, eli1.date, eliL.date, death.date)]

### Write Data to Disk ###
setwd(core)
write_fst(patient.practice, "patient&practice.fst")

##################################
### Create Long Format Dataset ###
##################################

### Initial Assignment ###
patient.practice.long <- patient.practice; rm(patient.practice)

### Calculate Amount of FUP per Year ###
for (i in year(qof1.date):( year(qofL.date) - 1L) ) {
  print(paste("April", i, "to", "March", i + 1L), quote = F) # Progress indicator
  patient.practice.long[, temp := as.IDate(paste(i + 1L, "04", "01", sep = "-")) - as.IDate(paste(i, "04", "01", sep = "-"))]
  patient.practice.long[eli1.date >= as.IDate(paste(i, "04", "01", sep = "-")) & eli1.date <= as.IDate(paste(i + 1L, "04", "01", sep = "-")), temp := temp - as.integer( eli1.date - as.IDate(paste(i, "04", "01", sep = "-")) )]
  patient.practice.long[eliL.date >= as.IDate(paste(i, "04", "01", sep = "-")) & eliL.date <= as.IDate(paste(i + 1L, "04", "01", sep = "-")), temp := temp - as.integer( as.IDate(paste(i + 1L, "04", "01", sep = "-")) - eliL.date )]
  patient.practice.long[eli1.date > as.IDate(paste(i + 1L, "04", "01", sep = "-")) | eliL.date < as.IDate(paste(i, "04", "01", sep = "-")), temp := 0L]
  setnames(patient.practice.long, "temp", paste(i))
}; rm(i)

### Reshape the Data to Long Format ###
patient.practice.long <- melt(patient.practice.long,
                              id.vars = grep("[a-zA-Z]+", names(patient.practice.long), value = T),
                              measure.vars = grep("^[1-9]+", names(patient.practice.long), value = T),
                              variable.name = "year",
                              value.name = "fup",
                              variable.factor = F)
patient.practice.long[, year := as.integer(year)]
setkey(patient.practice.long, patid, year)

### Remove Ineligible Data ###
patient.practice.long <- patient.practice.long[fup != 0L] # Years patients had no fup.
patient.practice.long[, fup := ifelse(year %in% seq(2003L, 2015L, 4L), fup/366L, fup/365)]

### Generate Patient Age Variable ###
patient.practice.long[, age := (as.IDate(paste(year, "04", "01", sep = "-")) - dob)/365.25]
patient.practice.long[, dob := NULL]
patient.practice.long[, age.cat := cut(age,
                                       breaks = c(-Inf, 40, 50, 60, 70, 80, 90, Inf),
                                       labels = c("18-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                                       right = F)]

### Write Data to Disk ###
setwd(core)
write_fst(patient.practice.long, "patient&practice_long.fst")

patient.year <- patient.practice.long[, .(patid, year)]; rm(patient.practice.long)
setwd(core)
write_fst(patient.year, "patient_year.fst"); rm(patient.year); gc()
