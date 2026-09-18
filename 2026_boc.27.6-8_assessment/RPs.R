#reference point estimation using EqSim

#remotes::install_github("flr/ss3om")
#remotes::install_github("ices-tools-prod/msy")

library(icesTAF)
library(TAF)
library(ss3om)
library(FLCore)
library(msy)
library(tidyverse)
library(Cairo)

rm(list=ls())
gc()

runName <- "2.3.WGWIDE26_Base_finalconfig"
stockName <- "boc.27.6-8"

mkdir("output")
op.dir <- file.path("output",runName)
mkdir(file.path(op.dir,"RPs"))

#read assessment output into an FLStock object
stk <- ss3om::readFLSss3(dir = file.path("model",runName))

#stock recruit
dfSR <- data.frame(Year = as.integer(dimnames(stk)$year),
                   Rec = as.numeric(rec(stk)),
                   SSB = as.numeric(ssb(stk)))

# Get range info from FLStock object
ages <- range(stk)["min"]:range(stk)["max"]
years <- range(stk)["minyear"]:range(stk)["maxyear"]
meanFages <- c(range(stk)["minfbar"],range(stk)["maxfbar"])

replist <- r4ss::SS_output(dir = "model", verbose = TRUE, printstats = TRUE)

#Uncertainty in final year of assessment
sigmaSSB <- 
  replist$derived_quants$StdDev[replist$derived_quants$Label=="SSB_2026"]/
  replist$derived_quants$Value[replist$derived_quants$Label=="SSB_2026"]

sigmaF <- 
  replist$derived_quants$StdDev[replist$derived_quants$Label=="F_2026"]/
  replist$derived_quants$Value[replist$derived_quants$Label=="F_2026"]

# Create matrix for reference points
# "Fmsy_unconstr" is the Fmsy value without any precautionary considerations (i.e. ignore 5% P(SSB<Blim))
refPts <- matrix(NA,nrow=1,ncol=9, dimnames=list("value",c("MSYBtrigger","5thPerc_SSBmsy","Bpa","Blim",
                                                           "Fpa","Flim", "Fp05","Fmsy_unconstr","Fmsy")))  
#Blim
#mean of (the relatively low) biomasses leading to above average recruitment (2005 2017 2018 2019 2020)
refPts[,"Blim"]  <- mean(as.numeric(ssb(stk)[,c('2005','2017','2018','2019','2020')]))

##~--------------------------------------------------------------------------
## Simulation settings
# Number of sims
# Choose a suitable number, final run should use at least 1000, test runs can be done with less to save time
noSims <- 1000                                

# SR models to use from Segreg, Ricker, Bevholt
appModels <- c("Segreg")   

# Which years (SSB years, not recruitment years) to exclude from the SRR fits 
# exclude recent years (no deviation estimated by SS3)
rmSRRYrs <- c(2024:2026)

# Autocorrelation in recruitment?
rhoRec <- F                                   # default=F

## Weight at age and selectivity
numAvgYrsB <- 3                               # Number of recent years to use for WAA
bioConst   <- TRUE                            # Constant/average WAA (TRUE) or resampling from the years specified (FALSE)
numAvgYrsS <- 3                               # Number of recent years to use for selectivity
selConst   <- TRUE                            # Constant/average selectivity (TRUE) or resampling from the years specified (FALSE)

## Forecast error (see Guidance document for details on calculation of these values)
# F
cvF  <- 0.212                                 # Default = 0.212
phiF <-	0.423                                 # Default = 0.423
# SSB
cvSSB <- 0                                    # Default = 0
phiSSB <- 0                                   # Default = 0

# 5th percentile of SSB in the final year of the assessment
SSB05<-0                                      # used in MSY Btrigger calculation. If set at 0, ignored

# year range
minYear <- range(stk)["minyear"] 
maxYear <- range(stk)["maxyear"]

## SRR years 
# Which years (SSB years) to exclude from the SRR fits
srYears <- setdiff(c(minYear:(maxYear-1)),rmSRRYrs)

#determine segreg model with specified Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= refPts[,"Blim"], ab$a * refPts[,"Blim"], ab$a * ssb))

#check for evidence of autocorrelation
Cairo(file = file.path(op.dir,"RPs","SRAutocor.png"), type="png", width = 6, height = 4, res = 300, units = "in")
ACFrec <- acf(rec(stk)[,ac(srYears)])
dev.off()
acfRecLag1 <- round(ACFrec$acf[,,][2],2)

###-------------------------------------------------------------------------------
## Fit SRRs
FIT_segregBlim <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
Cairo(file = file.path(op.dir,"RPs","SegRegBlim.png"), type="png", width = 6, height = 4, res = 300, units = "in")
eqsr_plot(FIT_segregBlim,ggPlot=TRUE)
dev.off()

FIT_segreg <- eqsr_fit(stk,nsamp=noSims, models = "Segreg", remove.years=rmSRRYrs)
Cairo(file = file.path(op.dir,"RPs","SegReg.png"), type="png", width = 6, height = 4, res = 300, units = "in")
eqsr_plot(FIT_segreg,ggPlot=TRUE)
dev.off()

# save model proportions and parameters:
write.csv(FIT_segregBlim$sr.det, file.path(op.dir,"RPs",paste0(stockName,"_FIT_segregBlim_SRpars.csv")))
write.csv(FIT_segreg$sr.det, file.path(op.dir,"RPs",paste0(stockName,"_FIT_segreg_SRpars.csv")))

#Calculate Bpa based on sigmaSSB
refPts[,"Bpa"]  <- refPts[,"Blim"]*exp(sigmaSSB*1.645) 

## Simuation 1 - get Flim
# Flim is derived from Blim by simulating the stock with segmented regression S-R function with the point of inflection at Blim 
# Flim = the F that, in equilibrium, gives a 50% probability of SSB > Blim. 
# Note this simulation should be conducted with:
#  fixed F (i.e. without inclusion of a Btrigger)
#  without inclusion of assessment/advice errors. 

SIM_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = TRUE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = TRUE,
                            Fcv=0, Fphi=0, SSBcv=0,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,0.5,len=50),verbose=T)

# save MSY and lim values
tmp1 <- t(SIM_segregBlim$Refs2)
write.csv(tmp1, file.path(op.dir,"RPs",paste("EqSim_",stockName,"_SegregBlim_eqRes.csv",sep="")))
refPts[,"Flim"] <- tmp1["F50","catF"]
if (refPts[,"Flim"]<0.2) refPts[,"Flim"] <- round(refPts[,"Flim"],3) else refPts[,"Flim"] <- round(refPts[,"Flim"],2)

###-------------------------------------------------------------------------------
## Simuation 2a - get initial Fmsy
# FMSY should initially be calculated based on:
#     a constant F evaluation 
#     with the inclusion of stochasticity in population and exploitation 
#     as well as assessment/advice error. 
#     Appropriate SRRs should be specified (here using all 3)

#segreg
SIM_All_noTrig <- eqsim_run(FIT_segreg,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                            Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,0.5,len=50),verbose=T)

# save MSY and lim values
tmp2 <- t(SIM_All_noTrig$Refs2)
write.csv(tmp2, file.path(op.dir,"RPs",paste("EqSim_",stockName,"_AllnoTrig_eqRes.csv",sep="")))
Fmsy_tmp <- tmp2["medianMSY","lanF"]

# To ensure consistency between the precautionary and MSY frameworks, FMSY is not allowed to be above Fpa
refPts[,"Fmsy_unconstr"] <- Fmsy_tmp 
refPts[,"Fmsy"] <- Fmsy_tmp

#save summary plot
Cairo(file = file.path(op.dir,"RPs","boc.27.6-8_NoTrig_eqMSYplots.png"), type="png", 
      width = 6, height = 6, res = 600, units = "in")
eqsim_plot(SIM_All_noTrig,catch=TRUE)  
dev.off()

###-------------------------------------------------------------------------------
## MSY Btrigger
data.05<-SIM_segregBlim$rbp
x.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$Ftarget
b.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$p05
plot(b.05~x.05, ylab="SSB", xlab="F", xlim=c(0,0.4))
b.lm <- loess(b.05 ~ x.05)
refPts[,"5thPerc_SSBmsy"] <- predict(b.lm, refPts[,"Fmsy"])
# check if F<Fmsy last 5 years (at least 3 times in last 5 years)
if (sum(as.numeric(fbar(stk)[,ac((maxYear-4):maxYear)])<=refPts[,"Fmsy"])<3) {
  refPts[,"MSYBtrigger"]  <- refPts[,"Bpa"]  
} else {
  # Check if Bmsy_05>Bpa
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"]>refPts[,"Bpa"],refPts[,"5thPerc_SSBmsy"],refPts[,"Bpa"])
  # Check if Bmsy_05 > SSBcur_05
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"] > SSB05,max(refPts[,"Bpa"],SSB05),refPts[,"5thPerc_SSBmsy"])  
}


###-------------------------------------------------------------------------------
## Simuation 2b - get final Fmsy
# MSY Btrigger should be selected to safeguard against an undesirable or unexpected low SSB when fishing at FMSY
# The ICES MSY AR should be evaluated to check that the FMSY and MSY Btrigger combination adheres to precautionary considerations: 
#      in the long term, P(SSB<Blim)<5%
# The evaluation must include:
#      realistic assessment/advice error
#      stochasticity in population biology and fishery exploitation.
#      Appropriate SRRs should be specified (here using all 3)

SIM_All_Trig <- eqsim_run(FIT_segreg,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                          sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                          Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                          rhologRec=rhoRec,
                          Btrigger = refPts[,"MSYBtrigger"], Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                          Nrun=200, Fscan = seq(0,0.5,len=50),verbose=T)

# save MSY and lim values
tmp3 <- t(SIM_All_Trig$Refs2)
write.csv(tmp3, file.path(op.dir,"RPs",paste("EqSim_",stockName,"_AllTrig_eqRes.csv",sep="")))
refPts[,"Fp05"] <- tmp3["F05","catF"]

# save Equilibrium plots
Cairo(file = file.path(op.dir,"RPs","boc.27.6-8_AllTrig_eqMSYplots.png"), type="png", 
      width = 6, height = 6, res = 600, units = "in")
eqsim_plot(SIM_All_Trig,catch=TRUE)  
dev.off()

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
if (refPts[,"Fmsy"] > refPts[,"Fp05"]) {
  print("WHOAAA, Fmsy > Fp05!") # If Fmsy > Fp05, Fmsy = Fp05
  if (refPts[,"Fp05"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fp05"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fp05"],2)
} else {  
  if (refPts[,"Fmsy"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}
if (refPts[,"Fp05"]<0.2) refPts[,"Fp05"] <- round(refPts[,"Fp05"],3) else refPts[,"Fp05"] <- round(refPts[,"Fp05"],2)  

# Fpa = Fp05
refPts[,"Fpa"] <- refPts[,"Fp05"]
if (refPts[,"Fmsy_unconstr"]<0.2) refPts[,"Fmsy_unconstr"] <- round(refPts[,"Fmsy_unconstr"],3) else refPts[,"Fmsy_unconstr"] <- round(refPts[,"Fmsy_unconstr"],2)  

#ICES rounding
refPts[,"Bpa"] <- round(refPts[,"Bpa"],0)
refPts[,"5thPerc_SSBmsy"] <- round(refPts[,"5thPerc_SSBmsy"],0)
refPts[,"MSYBtrigger"] <- round(refPts[,"MSYBtrigger"],0)

###-------------------------------------------------------------------------------
## Save reference points
write.csv(refPts, file.path(op.dir,"RPs",paste(stockName,"_RefPts.csv",sep="")))

###-------------------------------------------------------------------------------
## Save run settings
SRused <- appModels[1]
if (length(appModels)>1) for (i in 2:length(appModels)) SRused <- paste(SRused,appModels[i],sep="_")
SRyears_min <- min(srYears); SRyears_max <- max(srYears)
#setList <- c("stockName", "runName", "SAOAssessment", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
#             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
setList <- c("stockName", "runName", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
runSet <- matrix(NA,ncol=1, nrow=length(setList), dimnames=list(setList,c("Value")))
for (i in setList) runSet[which(setList==i),] <- eval(parse(text = i))

write.csv(runSet, file.path(op.dir,"RPs",paste(stockName,"_RunSettings.csv",sep="")))

###-------------------------------------------------------------------------------
## Save workspace
save.image(file=file.path(op.dir,"RPs",paste(stockName,"_",maxYear,"_EqSim_Workspace.Rdata",sep="")))



