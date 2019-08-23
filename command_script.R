#======================#
#                      #
#### COMMAND SCRIPT ####
#                      #
#======================#

### Clear the Environment ###
rm(list = ls()); gc()

### General Options ###
options(pkgType = switch(Sys.info()[["sysname"]],
                         Windows = "win.binary",
                         Linux  = "source",
                         Darwin = "mac.binary.el-capitan"))
options(scipen = 10)

### Update Installed Packages ###
suppressWarnings(
  update.packages(repos = "https://cloud.r-project.org", ask = F, checkBuilt = T)
)

### Load Libraries ###
suppressPackageStartupMessages(library(fst)) # Fast Data I/O
suppressPackageStartupMessages(library(parallel)) # Parallelisation.
suppressPackageStartupMessages(library(data.table)) # Data management.
suppressPackageStartupMessages(library(msm)) # Modelling.
suppressPackageStartupMessages(library(msmtools)) # Modelling.

### Parallelisation Opions ###
options(mc.cores = detectCores())
setDTthreads(detectCores())
options(fst_threads = detectCores())

### Specify Datacut ###
datacut <- "all" # "10k" | "50k" | "100k" | "all"

### Specify Study Dates ###
qof1.date <- as.IDate("2005-04-01") # Start of the financial year.
qofL.date <- as.IDate("2014-03-31") # End of the financial year.

## Define Directory Paths ##
mnt.pt <- switch(Sys.info()[["sysname"]],
                 Windows = "X:",
                 Linux  = "/mnt/PHC_CPRD/data",
                 Darwin = "/Volumes/PHC_CPRD/data")

## Code & Code-List Directories ##
if(.Platform[["OS.type"]] == "unix"){
  git <- "~/Git/eGFR Decline"
  codelists <- "~/Git/Code Lists"
} else {
  git <- file.path(mnt.pt, "CPRD_14_150R", "scripts", "Ben")
  codelists <- file.path(mnt.pt, "CPRD_14_150R", "codelists")
}

## 1ry Data Directories ##
proj.dir <- file.path(mnt.pt, "CPRD_14_150R"); rm(mnt.pt)
additional <- file.path(proj.dir, "additional")
base <- file.path(proj.dir, "base")
clinical <- file.path(proj.dir, "clinical")
linkage <- file.path(proj.dir, "linkages")
lookup <- file.path(proj.dir, "lookup")
test <- file.path(proj.dir, "test")
therapy <- file.path(proj.dir, "therapy")

## 2ry Data Directories ##
scratch.dir <- file.path(proj.dir, "scratch"); rm(proj.dir)
core <- file.path(scratch.dir, "Core")
output <- file.path(scratch.dir, "Output")
temp <- file.path(scratch.dir, "Temp"); rm(scratch.dir)

### Merge & Trim Core CPRD Files ###
setwd(git)
source("functions.R", echo = T)
setwd(git)
source("lookup.R", echo = T)
setwd(git)
source("setup.R", echo = T)
setwd(git)
source("exposures_date.R", echo = T)
setwd(git)
source("therapies_date.R", echo = T)
setwd(git)
source("serum_creatinine_test.R", echo = T)
setwd(git)
source("proteinuria_test.R", echo = T)
setwd(git)
source("dataset_compile_markov.R", echo = T)
setwd(git)
source("modelling_hmm_unmeas.R", echo = T)
setwd(git)
source("modelling_hmm_normo.R", echo = T)
setwd(git)
source("modelling_hmm_micro.R", echo = T)
setwd(git)
source("modelling_hmm_macro.R", echo = T)