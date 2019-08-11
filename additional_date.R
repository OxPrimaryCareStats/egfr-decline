#=========================#
#                         #
#### ADDITIONAL (DATE) ####
#                         #
#=========================#

# NB: This file takes each patient's recorded values in the additional files and matches up the most recent value prior to study entry.

#########################
### Load Patient Data ###
#########################

setwd(core)
add.dt <- read_fst("patient&practice.fst", as.data.table = T)
add.dt <- add.dt[, .(patid, dat1.date, eli1.date, eliL.date)]
setkey(add.dt, patid)

##################
### Additional ###
##################

setwd(additional)
add.list <- list.files(pattern = "\\.fst$") # Create a vector of all the exposures.
add.list <- gsub("_additional_all\\.fst$", "", add.list) # Drop filename endings.
add.list <- sort(add.list) # Alphabetic sorting.
add.list

for (i in 1:length(add.list)) {
  print(paste("Processing", add.list[i], "..."), quote = F) # Progress indicator.
  temp.dt <- read_fst(paste0(add.list[i], "_additional_all.fst"), as.data.table = T)
  setnames(temp.dt, grep("date$", names(temp.dt)), "event.date") # Create a common date name.
  setnames(temp.dt, grep(add.list[i], names(temp.dt)), "event") # Create common event marker.
  setkey(temp.dt, patid, event.date)
  temp.dt <- temp.dt[, c("patid", "event.date", "event"), with = F]
  
  add.dt <- temp.dt[add.dt, on = "patid"]; rm(temp.dt)
  add.dt <- add.dt[event.date >= dat1.date & event.date <= eli1.date] # Eligible data.
  setorder(add.dt, patid, -event.date)
  add.dt <- add.dt[, .SD[1], by = patid]
  add.dt <- add.dt[, c("patid", "event"), with = F]
  setnames(add.dt, "event", add.list[i])
  setkey(add.dt, patid)
}; rm(add.list, i)

##########################
### Write Data to Disk ###
##########################

setwd(temp)
write_fst(add.dt, "add.fst"); rm(add.dt); gc()