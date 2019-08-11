#========================#
#                        #
#### EXPOSURES (DATE) ####
#                        #
#========================#


#########################
### Load Patient Data ###
#########################

setwd(core)
exp.dt <- read_fst("patient&practice.fst", as.data.table = T)
exp.dt <- exp.dt[, .(patid, dat1.date, eliL.date)]
setkey(exp.dt, patid)

#################
### Exposures ###
#################

setwd(clinical)
exp.list <- list.files(pattern = "\\.fst$") # Create a vector of all the exposures.
exp.list <- exp.list[!{grepl("^ckd", exp.list)}] # Drop ckd for now.
exp.list <- exp.list[!grepl("^bmi", exp.list)] # Drop bmi.
exp.list <- gsub("_clinical_all\\.fst$", "", exp.list) # Drop filename endings.
exp.list <- sort(exp.list) # Alphabetic sorting.
exp.list

for (i in 1:length(exp.list)) {
  print(paste("Processing", exp.list[i], "..."), quote = F) # Progress indicator.
  temp.dt <- read_fst(paste0(exp.list[i], "_clinical_all.fst"), as.data.table = T)
  setnames(temp.dt, grep("date$", names(temp.dt)), "event.date") # Create a common date name.
  temp.dt[, event.date := as.IDate(event.date, format = "%d%b%Y")] # Convert dates.
  setkey(temp.dt, patid, event.date) # Set key (sort) for fast joins.
  temp.dt <- temp.dt[, c("patid", "event.date"), with = F] # Drop redundant variables.
  
  exp.dt <- temp.dt[exp.dt, on = "patid"]; rm(temp.dt) # Merge data tables.
  exp.dt[event.date < dat1.date | event.date > eliL.date, event.date := NA]
  exp.dt[, event.date := as.IDate(event.date, origin = "1970-01-01")]
  setnames(exp.dt, "event.date", paste0(exp.list[i], ".date")) # Renames variable.
  setkey(exp.dt, patid)
}; rm(exp.list, i)

### Load CKD Data into R ###
setwd(clinical)
ckd <- read_fst("ckd_clinical_all.fst", as.data.table = T)
setkey(ckd, patid)

### Data Cleaning ###
vars <- grep("^ckd1stage", names(ckd), value = T)
ckd[, (vars) := mclapply(.SD, as.IDate, format = "%d%b%Y"), .SDcols = vars]; rm(vars)
setnames(ckd,
         grep("^ckd1stage", names(ckd), value = T),
         paste0("ckdstage", c("1", "2", "3", "3a", "3b", "4", "5"), ".date"))
ckd[, ckdstage3.date := pmin(ckdstage3.date, ckdstage3a.date, ckdstage3b.date, na.rm = T)]
ckd <- ckd[, .(patid, ckdstage1.date, ckdstage2.date, ckdstage3.date, ckdstage4.date, ckdstage5.date)]

exp.dt <- ckd[exp.dt, on = "patid"]; rm(ckd) # Merge data tables.

### Check CKD Date Eligibility ###
vars <- grep("^ckdstage", names(exp.dt), value = T)
for(i in 1:length(vars)){
  setnames(exp.dt, vars[i], "ckd.date")
  exp.dt[ckd.date < dat1.date | ckd.date > eliL.date, ckd.date := NA]
  
  ## Repair Broken IDates ##
  exp.dt[, ckd.date := as.integer(ckd.date)]
  exp.dt[, ckd.date := as.IDate(ckd.date, origin = "1970-01-01")]
  
  setnames(exp.dt, "ckd.date", vars[i])
}; rm(vars, i)

setkey(exp.dt, patid) # Set key (sort) for fast joins.

### Drop Eligibility Dates ###
exp.dt[, c("dat1.date", "eliL.date") := NULL]

##########################
### Write Data to Disk ###
##########################

setwd(temp)
write_fst(exp.dt, "exp.fst"); rm(exp.dt); gc()