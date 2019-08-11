#===============================#
#                               #
#### DATASET COMPILE (DATES) ####
#                               #
#===============================#

####################
### File Purpose ###
####################

# This file consists of a series of dates only.
# It should only be used with great caution, i.e. only if you really know what you're doing!

#############################
### Read In & Merge Files ###
#############################

### Read in Patient-Level Data ###
setwd(core)
dt <- read_fst("patient&practice.fst", as.data.table = T)

### Read in Albuminuria Stage Data ###
setwd(temp)
alb.stage <- read_fst("proteinuria_test.fst", as.data.table = T)
setorder(alb.stage, patid, stage, event.date)
alb.stage <- alb.stage[, .SD[1], by = c("patid", "stage")]
alb.stage <- dcast(alb.stage, patid ~ stage, value.var = "event.date")
setnames(alb.stage, grep("A[1-3]", names(alb.stage), value = T), paste0("albstage", 1:3, ".date"))

### Read in Exposures & Therapies Data ###
setwd(temp)
add.dt <- read_fst("add.fst", as.data.table = T)
exp.dt <- read_fst("exp.fst", as.data.table = T)
drug.dt <- read_fst("drug.fst", as.data.table = T)

### Read in Test Data ###
setwd(temp)
egfr <- read_fst("egfr_test.fst", as.data.table = T)

### Merge Datasets ###
dt <- alb.stage[dt, on = "patid"]; rm(alb.stage)
dt <- add.dt[dt, on = "patid"]; rm(add.dt)
dt <- exp.dt[dt, on = "patid"]; rm(exp.dt)
dt <- drug.dt[dt, on =  "patid"]; rm(drug.dt)
dt <- egfr[dt, on = "patid"]; rm(egfr)

#######################
### Data Management ###
#######################

### Centre & Rescale Continuous Variables ###
dt <- dt[, age := as.integer(egfr.date - dob)/365.25]

### Sorting ###
print(object.size(dt), units = "MB")
first.vars <- c("patid", "pracid", "region", "gender", "age", "ethnicity", "imd")
test.vars <- c("egfr.date", "egfr.ckdepi", "egfr.mdrd")
setcolorder(dt, c(first.vars, names(dt)[!{names(dt) %in% c(first.vars, test.vars)}], test.vars)); rm(first.vars, test.vars)
setkey(dt, region, pracid, patid, egfr.date)

### Write Dataset to Disk ###
## Garbage Collection ##
gc()

## R *.rds File ##
setwd(core)
system.time(
  saveRDS(dt, "analysis_data_date.rds")
)

## FST *.fst File ##
setwd(core)
system.time(
  write_fst(dt, "analysis_data_date.fst")
)

### Tidy Up ###
rm(dt); gc()