#==============================#
#                              #
#### DATASET COMPILE MARKOV ####
#                              #
#==============================#

#############################
### Read In & Merge Files ###
#############################

### Read in Patient-Level Data ###
setwd(core)
dt <- read_fst("patient&practice.fst", as.data.table = T)

### Read in Albuminuria Stage Data ###
setwd(temp)
alb.stage <- read_fst("proteinuria_test.fst", as.data.table = T)
alb.stage <- dt[alb.stage, on = "patid"]
alb.stage <- alb.stage[event.date <= eli1.date]
setorder(alb.stage, patid, -event.date)
alb.stage <- alb.stage[, .SD[1], by = "patid"]
alb.stage <- alb.stage[, .(patid, stage)]
setnames(alb.stage, "stage", "albstage0")

### Read in Exposures & Therapies Data ###
setwd(temp)
exp.dt <- read_fst("exp.fst", as.data.table = T)
drug.dt <- read_fst("drug.fst", as.data.table = T)

### Read in Test Data ###
setwd(temp)
egfr <- read_fst("egfr_test.fst", as.data.table = T)

### Merge Datasets ###
dt <- alb.stage[dt, on = "patid"]; rm(alb.stage)
dt <- exp.dt[dt, on = "patid"]; rm(exp.dt)
dt <- drug.dt[dt, on =  "patid"]; rm(drug.dt)
dt <- egfr[dt, on = "patid"]; rm(egfr)
dt[, egfr.mdrd := NULL]

### Drop Subjects Without Tests ###
dt <- dt[complete.cases(egfr.ckdepi)]

#######################
### Data Management ###
#######################

### Drop Redundant Date Variables ###
dt <- dt[, "eli1.date" := NULL]

### Convert Dates to Presence/Absence ###
dates <- grep("date", names(dt), value = T)
dates <- dates[!grepl("^dat1|^death|^egfr|^eliL", dates)]
for(i in 1:length(dates)){
  print(paste("Converting", dates[i], "..."), quote = F)
  setnames(dt, dates[i], "datevar")
  dt[, datevar := ifelse(datevar <= egfr.date & !is.na(datevar), 1L, 0L)]
  setnames(dt, "datevar", dates[i])
  setnames(dt, dates[i], gsub(".date", "", dates[i]))
}; rm(dates, i)

### Create eGFR Stages ###
dt[, egfr.stage := cut(egfr.ckdepi, c(-Inf, 15, 30, 45, 60, 90, Inf),
                       c("G5", "G4", "G3b", "G3a", "G2", "G1"),
                       right = F)]
dt[, egfr.stage := factor(egfr.stage,
                          c("G1", "G2", "G3a", "G3b", "G4", "G5"))]
dt[, egfr.stage := as.character(egfr.stage)]

### Append Death Dates to eGFR Data ###
death <- copy(dt)
setorder(death, patid, -egfr.date)
death <- death[, .SD[1], by = patid]
death <- death[complete.cases(death.date)]
death[, egfr.stage := "Dead"]
death[, egfr.date := death.date]
death[, death.date := NULL]
dt[, death.date := NULL]
dt[, egfr.date := as.IDate(as.integer(egfr.date))] # Repair broken IDates.
death[, egfr.date := as.IDate(as.integer(egfr.date))] # Repair broken IDates.
dt <- rbind(dt, death); rm(death)
dt[, egfr.stage := factor(egfr.stage,
                          c("G1", "G2", "G3a", "G3b", "G4", "G5", "Dead"))]
setkey(dt, patid, egfr.date)

### Centre & Rescale Continuous Variables ###
dt <- dt[, age := as.integer(egfr.date - dob)/365.25]

### Sorting ###
print(object.size(dt), units = "MB")
first.vars <- c("patid", "pracid", "region", "gender", "age", "ethnicity", "imd")
test.vars <- c("egfr.date", "egfr.ckdepi", "egfr.stage")
setcolorder(dt, c(first.vars, names(dt)[!{names(dt) %in% c(first.vars, test.vars)}], test.vars)); rm(first.vars, test.vars)
setkey(dt, region, pracid, patid, egfr.date)

### Exclusions ###
dt[, count := .N, by = patid]
dt <- dt[count >= 3]
dt[, count := NULL]

dt_alb0 <- dt[is.na(albstage0)]
dt_alb1 <- dt[albstage0 == "A1"]
dt_alb2 <- dt[albstage0 == "A2"]
dt_alb3 <- dt[albstage0 == "A3"]

### Write Data Sets to Disk ###
setwd(core)
system.time({
  write_fst(dt_alb0, "analysis_data_markov_alb0.fst")
  write_fst(dt_alb1, "analysis_data_markov_alb1.fst")
  write_fst(dt_alb2, "analysis_data_markov_alb2.fst")
  write_fst(dt_alb3, "analysis_data_markov_alb3.fst")
})

### Tidy Up ###
rm(list = ls(pattern = "^dt")); gc()