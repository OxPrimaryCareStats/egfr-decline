#========================#
#                        #
#### THERAPIES (DATE) ####
#                        #
#========================#

#########################
### Load Patient Data ###
#########################

setwd(core)
drug.dt <- read_fst("patient&practice.fst", as.data.table = T)
drug.dt <- drug.dt[, .(patid, dat1.date, eliL.date)]
setkey(drug.dt, patid)

#################
### Therapies ###
#################

### Jeff's Classifications ###
jeff.tab <- rbindlist(
  list(
    list("acei", 1L, 0L, 1L, 0L),
    list("anthranilic", 1L, 0L, 0L, 0L),
    list("arb", 1L, 0L, 1L, 0L),
    list("arylalkanoic", 1L, 0L, 0L, 0L),
    list("azathioprine", 1L, 0L, 0L, 1L),
    list("butazone", 1L, 0L, 0L, 0L),
    list("ciclosporin", 1L, 0L, 0L, 1L),
    list("cox2", 1L, 0L, 0L, 0L),
    list("cyclophosphamide", 0L, 0L, 0L, 1L),
    list("digoxin", 0L, 1L, 0L, 0L),
    list("diuretics", 1L, 0L, 1L, 0L),
    list("darones", 0L, 0L, 1L, 0L),
    list("enolic", 1L, 0L, 0L, 0L),
    list("ethambutol", 0L, 1L, 0L, 0L),
    list("gold", 1L, 0L, 0L, 0L),
    list("hydroxychloroquine", 1L, 0L, 0L, 1L),
    list("indoleacetic", 1L, 0L, 0L, 0L),
    list("leflunomide", 0L, 0L, 0L, 1L),
    list("lithium", 1L, 1L, 0L, 0L),
    list("mesalazine", 1L, 0L, 0L, 0L),
    list("methotrexate", 1L, 0L, 0L, 1L),
    list("mycophenolatemofetil", 1L, 0L, 0L, 1L),
    list("oralanticoagulants_other", 1L, 0L, 0L, 0L),
    list("penicillamine", 1L, 0L, 0L, 0L),
    list("rosuvastatin", 0L, 0L, 0L, 0L), # Potentially damages kidney if rhabdomyolysis occurs.
    list("sulfasalazine", 1L, 0L, 0L, 0L),
    list("warfarin", 1L, 0L, 0L, 0L)
  )
)
names(jeff.tab) <- c("drug", "damages.kidney", "kidney.excretes", "affects.serum.k", "post.transplant")

### Keep Only Relevant Drugs ###
jeff.tab <- jeff.tab[damages.kidney == 1L | kidney.excretes == 1L | affects.serum.k == 1L]
drug.list <- jeff.tab[, drug]; rm(jeff.tab)
drug.list <- sort(drug.list, decreasing = T) # Reverse alphabetical sorting.
drug.list

setwd(therapy)
for (i in 1:length(drug.list)) {
  print(paste("Processing", drug.list[i], "..."), quote = F) # Progress indicator.
  temp.dt <- read_fst(paste0(drug.list[i], "_therapy_all.fst"), as.data.table = T)
  setnames(temp.dt, grep("date$", names(temp.dt)), "script.date") # Create a common date name.
  temp.dt[, script.date := as.IDate(script.date, format = "%d%b%Y")] # Convert dates.
  setkey(temp.dt, patid, script.date) # Set key (sort) for fast joins.
  temp.dt <- temp.dt[, .SD[1], by = patid] # Use only one perscription per patient per year.
  temp.dt <- temp.dt[, .(patid, script.date)] # Keep only these variables.
  
  drug.dt <- temp.dt[drug.dt, on = "patid"]; rm(temp.dt) # Merge data tables.
  drug.dt[script.date < dat1.date | script.date > eliL.date, script.date := NA]
  setnames(drug.dt, "script.date", paste0(drug.list[i], ".date")) # Renames variable.
  setkey(drug.dt, patid) # Set key (sort) for fast joins.
}; rm(drug.list, i); gc()
setkey(drug.dt, patid)

### Group Similar Drug Classes ###
## Other Immunosuppressants ##
drug.dt[, immunosupressants.other.date := pmin(ciclosporin.date, hydroxychloroquine.date, mycophenolatemofetil.date, penicillamine.date, sulfasalazine.date, azathioprine.date, na.rm = T)]
drug.dt[, paste(c("ciclosporin", "hydroxychloroquine", "mycophenolatemofetil", "penicillamine", "sulfasalazine", "azathioprine"), "date", sep = ".") := NULL]
## NSAIDs ##
drug.dt[, nsaids.date := pmin(anthranilic.date, arylalkanoic.date, butazone.date, cox2.date, enolic.date, indoleacetic.date, na.rm = T)]
drug.dt[, c("anthranilic.date", "arylalkanoic.date", "butazone.date", "cox2.date", "enolic.date", "indoleacetic.date") := NULL]
## Oral Anticoagulants ##
drug.dt[, oacs.date := pmin(oralanticoagulants_other.date, warfarin.date, na.rm = T)]
drug.dt[, c("oralanticoagulants_other.date", "warfarin.date") := NULL]

### Repair Broken IDates ###
vars <- c("immunosupressants.other.date", "nsaids.date", "oacs.date")
drug.dt[, (vars) := mclapply(.SD, as.integer), .SDcols = vars]
drug.dt[, (vars) := mclapply(.SD, as.IDate, origin = "1970-01-01"), .SDcols = vars]
rm(vars)

### Drop Eligibility Dates ###
drug.dt[, c("dat1.date", "eliL.date") := NULL]

##########################
### Write Data to Disk ###
##########################

setwd(temp)
write_fst(drug.dt, "drug.fst"); rm(drug.dt); gc()