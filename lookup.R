#====================#
#                    #
#### LOOKUP FILES ####
#                    #
#====================#

################################
### Load Lookup Files into R ###
################################

setwd(lookup)
OPR <- fread("OPR.txt", na.strings = "") # Operator
setnames(OPR, names(OPR), tolower(names(OPR)))
setnames(OPR, names(OPR), gsub(" ", ".", names(OPR)))
setkey(OPR, code)

PRG <- fread("PRG.txt", na.strings = "") # Practice Region
setnames(PRG, names(PRG), tolower(names(PRG)))
setnames(PRG, names(PRG), gsub(" ", ".", names(PRG)))
setkey(PRG, code)

#POP <- fread("POP.txt", na.strings = "") # Population Base
#setnames(POP, names(POP), tolower(names(POP)))
#setnames(POP, names(POP), gsub(" ", ".", names(POP)))
#setkey(POP, code)

SUM <- fread("SUM.txt", na.strings = "") # Specimen Unit of Measure
setnames(SUM, names(SUM), tolower(names(SUM)))
setnames(SUM, names(SUM), gsub(" ", ".", names(SUM)))
setkey(SUM, code)

TQU <- fread("TQU.txt", na.strings = "") # Test Qualifier
setnames(TQU, names(TQU), tolower(names(TQU)))
setnames(TQU, names(TQU), gsub(" ", ".", names(TQU)))
setkey(TQU, code)