# Statistical models for the deterioration of kidney function in a primary care population: A retrospective database analysis

## Requirements
Data managment scripts were implemented in R version 3.6.1 (Action of the Toes) using version 1.12.2 of the _data.table_ package. A few of the scripts contain instances of parallelisation performed using the `mclapply()` function. This function implements parallelisaton via 'thread-forking', which is not possible on Windows systems at the time of writing this. Hence, these will need to be changed to `lapply()` if the script is to be run on a Windows system. The data management on this project was intended to feed into three workstreams of a large programme grant, ergo there may be some files that get created, but appear not to be used. I've done my best to trim these from this repository, but bear in mind that a few of these may still be haning around.

All hidden Markov models were fit using version 1.6.7 of the _msm_ package.

## Directory Structure
This repository contains code for the intermediate data management and analysis of the project. The scripts assume the existence of the following directories:

### Script Directory
* `git` - Directory containing the intermediate data management and analysis scripts, i.e. the scripts present in this repository.

### (Some) Source Data Directories
* `codelists` - Directory containing lists of Read codes.
* `lookup` - Directory containing lookup files, containing the values associated with values present in some of the data# variables of the CPRD-GOLD 'Additional' and 'Test' files.
* `linkage` - Directory containing the project linkages. In this case:
    - Office for National Statistics mortality data.
    - Hospital Episodes Statistics data.
    - Index of Multiple Deprivation data.

### Secondary Data Directories
* `additional` - Directory containing $n$ files (one per risk factor), extracted from information contained within the CPRD-GOLD 'Additional' file.
* `clinical` - Directory containing $n$ files (one per risk factor), extracted from Read codes contained within the CPRD-GOLD 'Clinical' file.
* `therapy` - Directory containing $n$ files (one per risk factor), extracted from prescriptions data contained within the CPRD-GOLD 'Therapy' file.
* `test` - Directory containing $n$ files (one per risk factor), extracted from test results contained within the CPRD-GOLD 'Test' file.

### Tertiary Data Directories
* `base` - Directory containing the CPRD-GOLD 'Patient' and 'Practice' data sets, pre-merged into a single data set.
* `temp` - Scratch space for temporary intermediate data sets.
* `core` - Scratch space for more important intermediate data sets and analysis data sets.
* `output` - Scratch space for writing model outputs and plots (unused in this project).

## Script Order
The command script runs all subsequent scripts in the order required.