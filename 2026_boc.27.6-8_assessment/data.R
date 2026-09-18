#WGWIDE 2026 Boarfish
##Preprocess data, write TAF data tables

library(TAF)

TAF::mkdir("data")

# Get input files
TAF::cp(file.path(getwd(),"boot","data",runName,"BOC.dat"), "data")
TAF::cp(file.path(getwd(),"boot","data",runName,"BOC.ctl"), "data")
TAF::cp(file.path(getwd(),"boot","data",runName,"forecast.ss"), "data")
TAF::cp(file.path(getwd(),"boot","data",runName,"starter.ss"), "data")

# read input files
inputs <- r4ss::SS_read(dir = file.path(getwd(),"data"), verbose = TRUE)
