#=================================#
#                                 #
#### HMM FOR UNMEASURED COHORT ####
#                                 #
#=================================#

### Import Data ###
setwd(core)
DT <- read_fst("analysis_data_markov_alb0.fst", as.data.table = T)

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

## remove the duplicates
DT[, dup := duplicated(age), by = patid]
DT <- DT[!{dup == T}]
dim(DT)

## Calculate the difference in time between measurements 
## needs to be in years not days  for the model to run
DT[, time := (egfr.date - egfr.date[1])/365.25, by = patid]

## Exclusions Drop duplicate obs - e.g. stage 4 an dead on same date 
DT <- DT[!{8366}, , which = F]
DT <- DT[!{8591}, , which = F]

## Only one observation 
# View(DT[patid==15329336])
DT <- DT[!{patid == 8791109}]
DT <- DT[!{patid == 15329336}]

## unfeasible jumps in state
DT <- DT[!{patid == 45030670 & egfr.stage == "G1"}]
DT <- DT[!{patid == 32691676 & egfr.stage == "G5"}]

### Split Sample for Validation Purposes ###
set.seed(101)
uniqid <- unique(DT$patid)
ndevDT <- ceiling(length(uniqid)*0.5)
sids <- sample(uniqid, ndevDT)
devDT <- DT[patid %in% sids]
testDT <- DT[!{patid %in% sids}]

### Export Validation Data ###
setwd(core)
save("testDT", file = "unmeas.testdata.RData")

### Check the model first 
uniqid <- unique(devDT$patid)
length(uniqid)
sids <- sample(uniqid, 75000)
devDTs <- devDT[patid %in% sids]

## take out the people with only on observation 
## first redo count variable 
devDTs[, count := .N, by = patid]
devDTs <- devDTs[count > 1]

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
Ei <- rbind(er1, er2, er3, er4,  er5, er6) 
dimnames(Ei) <- list(names, names)
Ei

initprobs <- devDT[time <= 1, prop.table(table(y))]
initprobs[6] <- 0

## simplifying the covariates on transactions deeley 
covcon <- c(1,2,1,2,1,2,1,2,2)
constraints <- list(AGE60 = covcon, SEX = covcon, HF = covcon, CA = covcon)

### HMM ###
unmeas.model <- msm(y ~ time,
                   subject = patid,
                   data = devDTs,
                   qmatrix = Qi,
                   ematrix = Ei,
                   deathexact = 6,
                   hessian = T, # switch off to start estimating
                   constraint = constraints,
                   # opt.method = "minqa",
                   method = "CG",
                   # pci = c(4, 8), # for piecewise-constant intensities
                   control = list(fnscale = 2e4, maxit = 2e3, trace = 1),
                   covariates = ~ AGE60 + SEX + HF + CA,
                   # fixedpars = T,
                   initprobs = initprobs)

### Matrices ###
pmatrix.msm(unmeas.model)
sojourn.msm(unmeas.model)
hazard.msm(unmeas.model)

### Export Model Object ###
setwd(core)
save("unmeas.model", file = "unmeas.modelvmax.RData")