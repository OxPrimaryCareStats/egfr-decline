#=======================================#
#                                       #
#### HMM FOR MICROALBUMINURIA COHORT ####
#                                       #
#=======================================#

### Import Data ###
setwd(core)
DT <- read_fst("analysis_data_markov_alb2.fst", as.data.table = T)

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

## E.g. 
DT <- DT[!{patid == 6282581 & visit == 7}]  # G4 and Dead on same day
DT <- DT[!{patid == 11124617 & visit == 19}]
DT <- DT[!{patid == 9706651 & visit == 7}]

DT <- DT[!{patid == 13567095 & visit == 3}]
DT <- DT[!{patid == 31736525 & visit == 6}]
DT <- DT[!{patid == 28582031 & visit == 7}]
DT <- DT[!{patid == 26064544 & visit == 24}]

DT[, dup := duplicated(age), by = patid]
## check 
DT[dup == T & egfr.stage == "Dead", patid]

## removing this patient
DT[patid == 3965147, .(patid, egfr.ckdepi, egfr.stage, age, visit, dup)]
DT <- DT[!{patid == 3965147}]

## remove the duplicates
DT <- DT[!{dup == T}]

## Calculate the difference in time between measurements 
## needs to be in years not days  for the model to run
DT[, time := (egfr.date - egfr.date[1])/365.25, by = patid]

### Split Sample for Validation Purposes ###
set.seed(101)
uniqid <- unique(DT$patid)
ndevDT <- ceiling(length(uniqid)*0.5)
sids <- sample(uniqid, ndevDT)
devDT <- DT[patid %in% sids]
testDT <- DT[!{patid %in% sids}]

### Export Validation Data ###
setwd(core)
save("testDT", file = "micro.testdata.RData")

# randomly sample starting values for qmatrix 
qst <- runif(9,0,0.2)
qr1 <- c(0,qst[1],0,0,0,qst[2])  
qr2 <- c(0,0,qst[3],0,0,qst[4])  
qr3 <- c(0,0,0,qst[5],0,qst[6]) 
qr4 <- c(0,0,0,0,qst[7],qst[8]) 
qr5 <- c(0,0,0,0,0,qst[9]) 
qr6 <- c(0,0,0,0,0,1)
Qi <- rbind(qr1, qr2, qr3, qr4, qr5, qr6)
names <- c("G1/2", "G3a", "G3b", "G4", "G5", "Death")
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

initprobs <- devDT[time <= 1, prop.table(table(y))]
initprobs[6] <- 0

## simplifying the covariates on transactions deeley 
covcon <- c(1,2,1,2,1,2,1,2,2)
constraints <- list(AGE60 = covcon, SEX = covcon, HF = covcon, CA = covcon)

### HMM ###
micro.model <- msm(y ~ time,
                  subject = patid,
                  data = devDT,
                  qmatrix = Qi,
                  ematrix = Ei,
                  deathexact = 6,
                  hessian = T, # switch off to start estimating
                  constraint = constraints,
                  # opt.method = "minqa",
                  # method = "CG",
                  # pci = c(4, 8), # for piecewise-constant intensities
                  control = list(fnscale = 12e3, maxit = 2e3, trace = 1),
                  covariates = ~ AGE60 + SEX + HF + CA,
                  # fixedpars = T,
                  initprobs = initprobs)

### Matrices ###
pmatrix.msm(micro.model)
sojourn.msm(micro.model)
hazard.msm(micro.model)

### Export Model Object ###
setwd(core)
save("micro.model", file = "micro.modelv1.RData")