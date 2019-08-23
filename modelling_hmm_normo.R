#=======================================#
#                                       #
#### HMM FOR NORMOALBUMINURIA COHORT ####
#                                       #
#=======================================#

### Import Data ###
setwd(core)
DT <- read_fst("analysis_data_markov_alb1.fst", as.data.table = T)

DT[, visit := 1:.N, by = patid]  
DT[, DM := diabetes[1], by = patid]
DT[, CA := anycancer[1], by = patid]
DT[, HF := hf[1], by = patid]
DT[, STR := stroketia[1], by = patid]
DT[, HT := hypertension[1], by = patid]
DT[, IHD := ihd[1], by = patid]
DT[, CRD := chronic_renal_disease[1], by = patid]
DT[, PVD := pvd[1], by = patid]
DT[, SEX := ifelse(gender == "Male", 0, 1)]
DT[, AGE60 := age - 60, by = patid]  # updated covariate 

## Recode the outcome
DT[, y := 0L]
DT[egfr.stage == "Dead", y := 6L]
DT[egfr.stage == "G1", y := 1L]
DT[egfr.stage == "G2", y := 1L]
DT[egfr.stage == "G3a", y := 2L]
DT[egfr.stage == "G3b", y := 3L]
DT[egfr.stage == "G4", y := 4L]
DT[egfr.stage == "G5", y := 5L]

## There are duplicated results in this file 
DT[, dup := duplicated(age), by = patid]

## check these 
pt <- DT[dup == T & egfr.stage == "Dead", patid]

DT[patid == 17344343, .(patid, visit, egfr.stage, egfr.date, age, dup)]  

DT <- DT[patid == 17344343 & visit == 5, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 17344343 & visit == 6, dup := F]

DT <- DT[patid == 1587628 & visit == 4, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 1587628 & visit == 5, dup := F]

DT <- DT[patid == 2533628 & visit == 4, ':='(egfr.date = egfr.date - 1, age = age - (1/365.25))]
DT <- DT[patid == 2533628 & visit == 5, dup := F]  

DT <- DT[patid == 12019615 & visit == 21, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 12019615& visit == 22, dup := F]  

DT <- DT[patid == 1586095 & visit == 4, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 1586095 & visit == 5, dup := F]  

DT <- DT[patid == 6818645 & visit == 14, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 6818645 & visit == 15, dup := F]

DT <- DT[patid == 32328460 & visit == 8, ':='(egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 32328460 & visit == 9, dup := F]

DT <- DT[patid == 6074530 & visit == 7, ':=' (egfr.date = egfr.date - 1, age = age - (1/365.25))]  
DT <- DT[patid == 6074530 & visit == 8, dup := F]

DT[dup == T & egfr.stage == "Dead", .(patid)]
View(devDT[patid == 17344343, .(patid, time, y, visit)])

## This needs to be done here - after the adjustments made above. 
DT[,time:=(egfr.date - egfr.date[1])/365.25, by = patid] 

## only one observation 
DT <- DT[!{patid == 6317409}]

## remove all other duplicates
DT <- DT[!{dup == T}]

### Split Sample for Validation Purposes ###
set.seed(101)
uniqid <- unique(DT$patid)
ndevDT <- ceiling(length(uniqid)*0.5)
sids <- sample(uniqid, ndevDT)
devDT <- DT[patid %in% sids]
testDT <- DT[!{patid %in% sids}]

### Export Validation Data ###
setwd(core)
save("testDT", file = "nalb.testdata.RData")

# randomly sample starting values for qmatrix 
qst <- runif(9,0,0.2)
qr1 <- c(0,qst[1],0,0,0,qst[2])  
qr2 <- c(0,0,qst[3],0,0,qst[4])  
qr3 <- c(0,0,0,qst[5],0,qst[6]) 
qr4 <- c(0,0,0,0,qst[7],qst[8]) 
qr5 <- c(0,0,0,0,0,qst[9]) 
qr6 <- c(0,0,0,0,0,1)
Qi <- rbind(qr1, qr2, qr3, qr4, qr5, qr6)
names <- c("G1/2", "G3a", "G3b","G4","G5","Death")
dimnames(Qi) <- list(names, names)
Qi

## Error matrix - all close errors are possible i.e. G2 can be observed as G1 or G3a but not any further. 
est <- runif(14,0,0.2)
er1 <- c(0,est[1],est[2],0,0,0)  
er2 <- c(est[3],0,est[4],est[5],0,0)
er3 <- c(est[6],est[7],0,est[8],est[9],0)
er4 <- c(0,est[10],est[11],0,est[12],0)
er5 <- c(0,0,est[13],est[14],0,0)
er6 <- c(0,0,0,0,0,0)
Ei <- rbind(er1, er2, er3, er4, er5, er6) 
dimnames(Ei) <- list(names, names)
Ei

## drop out those with G1 at baseline 
devDT[, fy := egfr.ckdepi[1], by = patid]

## these guys 
devDT[, mean(fy > 90)]
devDT <- devDT[fy <= 90]

initprobs <- devDT[time <= 1, prop.table(table(y))]
initprobs[6] <- 0
initprobs

## simplifying the covariates on transactions deeley 
covcon <- c(1,2,1,2,1,2,1,2,2)
constraints <- list(AGE60 = covcon, SEX = covcon, HF = covcon, CA = covcon)

### HMM ###
nalb.model <- msm(y ~ time,
                  subject = patid,
                  data = devDT,
                  qmatrix = Qi,
                  ematrix = Ei,
                  deathexact = 6,
                  hessian = T, # switch off to start estimating
                  constraint = constraints,
                  # opt.method = "minqa",
                  # method = "CG",
                  # pci = c(4, 8), # for piecewise  -constant intensities
                  control = list(fnscale = 12e3, maxit = 2e3, trace = 1),
                  covariates = ~ AGE60 + SEX + HF + CA,
                  # fixedpars = T,
                  initprobs = initprobs)

### Matrices ###
pmatrix.msm(nalb.model)
sojourn.msm(nalb.model)
hazard.msm(nalb.model)

### Export Model Object ###
setwd(core)
save("nalb.model", file = "nalb.model.noG1s.RData")

View(devDT[1034:1035, .(patid, time, y), which = F])

# Sensitivity analysis no G1/G2 at baseline  ---------------------------------
devDT[,staget0 := y[1], by = patid]
devDTmG2 <- devDT[staget0 != 1]
initprobs <- devDTmG2[time <= 1, prop.table(table(y))]
initprobs[6] <- 0

## simplifying the covariates on transactions deeley 
covcon <- c(1,2,1,2,1,2,1,2,2)
constraints <- list(AGE60 = covcon, SEX = covcon, HF = covcon, CA = covcon)

nalbG2m.model <- msm(y ~ time,
                  subject = patid,
                  data = devDTmG2,
                  qmatrix = Qi,
                  ematrix = Ei,
                  deathexact = 6,
                  hessian = T, # switch off to start estimating
                  constraint = constraints,
                  # opt.method = "minqa",
                  # method = "CG",
                  # pci = c(4, 8), # for piecewise  -constant intensities
                  control = list(fnscale = 12e3, maxit = 2e3, trace = 1),
                  covariates = ~ AGE60 + SEX + HF + CA,
                  # fixedpars = T,
                  initprobs = initprobs)

### Matrices ###
pmatrix.msm(nalbG2m.model)
sojourn.msm(nalbG2m.model)
hazard.msm(nalbG2m.model)

### Export Model Object ###
setwd(core)
save("nalbG2m.model", file = "nalb.modelG2minus.RData")

# Sensitivity analysis on only G1/2 at baseline ---------------------------
devDTG12 <- devDT[staget0 == 1,]
initprobs <- devDTG12[time <= 1, prop.table(table(y))]
initprobs[6] <- 0

## simplifying the covariates on transactions deeley 
covcon <- c(1,2,1,2,1,2,1,2,2)
constraints <- list(AGE60 = covcon, SEX = covcon, HF = covcon, CA = covcon)

statetable.msm(state = y, subject = patid, data = devDTG12)

View(head(devDTG12))

nalbG12.model <- msm(y ~ time,
                     subject = patid,
                     data = devDTG12,
                     qmatrix = Qi,
                     ematrix = Ei,
                     deathexact = 6,
                     hessian = T, # switch off to start estimating
                     constraint = constraints,
                     # opt.method = "minqa",
                     # method = "CG",
                     # pci = c(4, 8), # for piecewise  -constant intensities
                     control = list(fnscale = 12e3, maxit = 2e3, trace = 1),
                     covariates = ~ AGE60 + SEX + HF + CA,
                     # fixedpars = T,
                     initprobs = initprobs)

### Matrices ###
pmatrix.msm(nalbG12.model)
sojourn.msm(nalbG12.model)
hazard.msm(nalbG12.model)

### Export Model Object ###
setwd(core)
save("nalbG12.model", file = "nalb.modelG12v1.RData")