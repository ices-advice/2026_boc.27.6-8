#WGWIDE 2026 Boarfish
#2.0.2.WGWIDE26_Base - sensitivity run (HCR_anchor in forecast.ss)
#new parameter from v.3.30.24
#set to 2 (should mimic old approach)

#2026 catch set to advice + 3yr avg discards + 3yr avg non-quota holders (31336t)
#2026 acoustic survey TSB excluded
#Groundfish index based on VAST fit to updated dataset conducted 10/08/26 (includes additional surveys in NS)

rm(list=ls())
gc()

sink(file="sessionInfo.txt")
sessionInfo()
sink()

library(icesTAF)
library(TAF)
library(r4ss)
library(doParallel)
library(tidyverse)
library(ss3diags)
library(Cairo)
library(officer)
library(flextable)

runName <- "2.0.2.WGWIDE26_Base"

#Reference Points as per 2024 benchmark
lRPs <- list("Blim" = 156762, "Bpa" = 190845, "Flim" = 0.175,
             "Fp05" = 0.042, "Fpa" = 0.042, "Fmsy" = 0.042,
             "MSYBtrigger" = 190845)

#some options
do.STF <- F
do.retro <- T
do.jitter <- F

taf.bootstrap()  #create/populate the data & software folders in boot, based on details in .bib files

TAF::mkdir("model")
TAF::mkdir(paste0("model/",runName))

source("data.R")

# Get input files
TAF::cp("data/BOC.dat", "model")
TAF::cp("data/BOC.ctl", "model")
TAF::cp("data/forecast.ss", "model")
TAF::cp("data/starter.ss", "model")

#copy model executable
#TAF::cp("boot/software/ss3_win_V3.30.23.2.exe", "model/ss3_win.exe")
TAF::cp("boot/software/ss3_win_V3.30.25.exe", "model/ss3_win.exe")

#run assessment
r4ss::run(dir = "model", exe = "SS3_win.exe", show_in_console = TRUE, skipfinished = FALSE)
replist <- r4ss::SS_output(dir = "model", verbose = TRUE, printstats = TRUE)
replist$timeseries$SpawnBio[match(max(replist$timeseries$Yr[replist$timeseries$Era=="TIME"]),replist$timeseries$Yr)]/1e6 #0.838913
replist$maximum_gradient_component #0.000486836

#save the fit, copy outputs
save(replist, file=file.path("model",paste0(runName,"_fit.Rdata")))
TAF::cp(from = "model/*.sso", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.ss_new", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.log", to = paste0("model/",runName), quiet=FALSE)

#retrospectives
if (do.retro) {
  r4ss::retro(dir = "model", exe = "SS_win.exe", oldsubdir = "", 
              newsubdir = file.path(runName,"Retrospective"), years = 0:-5, 
              extras = "-nox", skipfinished = F, verbose = FALSE)
  
  retroModels <- r4ss::SSgetoutput(dirvec=file.path("model",runName,"Retrospective",paste("retro",0:-5, sep="")))
  retroSummary <- r4ss::SSsummarize(retroModels)
  save(retroSummary, retroModels, file=file.path("model",paste0(runName,"_retro_fit.Rdata")))
}

if (do.STF) {
  
  #some settings
  Naver <- 2  # number of average years-1
  Term.yr <- 2026 # terminal assessment year
  Adv_Term.yr <- 29720  #advice for the terminal year
  Adv.yr <- 2027   #advice year
  Adv.yr.next <- Adv.yr + 1 #year following advice year
  
  #create a folder for the runs and outputs
  STF.dir <- file.path(".","model",runName,"STF")
  TAF::mkdir(file.path(STF.dir,"Scenarios"))
  
  #copy the forecast.ss from the assessment to the forecast folder
  TAF::cp("data/forecast.ss", STF.dir)
  
  #read in the forecast file
  fore <- r4ss::SS_readforecast(file = file.path(STF.dir,"forecast.ss"),verbose = FALSE)
  
  #get the F time series from the assessment
  Fdat <- replist$exploitation
  Fdat <- Fdat[-c(3,4,6,7)] #subset out unwanted cols (Seas_dur,F_std,annual_M,Fishery)
  
  #number of forecast years
  Nfor = fore$Nforecastyrs
  #period for recent averages
  #average of the last Nfor+1 years across seasons and fleets - easy here as we only have 1 season and 1 fleet
  startyear = max(Fdat$Yr) - Nfor - Naver
  endyear = max(Fdat$Yr) - Nfor  
  
  Fdata <- subset(Fdat,Fdat$Yr>=startyear & Fdat$Yr<=endyear)
  Fsq <- Fdata$annual_F[Fdata$Yr==endyear]
  Fdata <- Fdata[,-1] # remove year column
  Fdata_int <- aggregate(.~Seas,data=Fdata,mean) 
  
  #FMultipliers around FMSY
  #the multiplier of 1 will not lead to an F equivalent to FMSY as the F provided in the forecast is the apical F
  #it will be necessary to run a number of scenarios to achieve the desired F targets such as FMSY, status quo F, Fpa and Flim
  
  FMult <- c(0)  #F=0 scenario
  #broad scale of multipliers to investigate
  #FMult <- c(0,seq(1,5,by=0.1))
  names(FMult) <- c("F_Adv.yr = 0",rep("Scan",length(FMult)-1))
  
  #homing in...
  #on FMSY
  FMult.msy <- seq(1.1151,1.1153,by=0.0001)
  names(FMult.msy) <- rep("F_Adv.yr = FMSY",length(FMult.msy))
  FMult <- c(FMult,FMult.msy)
  
  #on Flim
  FMult.lim <- seq(4.64,4.66,by=0.01)
  names(FMult.lim) <- rep("F_Adv.yr = Flim",length(FMult.lim))
  FMult <- c(FMult,FMult.lim)
  
  #on Fsq
  FMult.Fsq <- seq(1.41,1.43,by=0.01)
  names(FMult.Fsq) <- rep("F_Adv.yr = F_Term.yr",length(FMult.Fsq))
  FMult <- c(FMult,FMult.Fsq)
  
  #on SSB = Blim in year following advice
  FMult.Blim <- seq(39.303,39.305,by=0.001)
  names(FMult.Blim) <- rep("SSB_Adv.yr.next = Blim",length(FMult.Blim))
  FMult <- c(FMult,FMult.Blim)
  
  #on SSB = MSYBtrigger in year following advice
  FMult.MSYBtrigger <- seq(33.895,33.897,by=0.001)
  names(FMult.MSYBtrigger) <- rep("SSB_Adv.yr.next = MSYBtrigger",length(FMult.MSYBtrigger))
  FMult <- c(FMult,FMult.MSYBtrigger)
  
  l_FMult <- length(FMult)
  
  #catch scenarios
  #terminal year catch
  tCatch <- replist$catch$Obs[replist$catch$Yr==max(replist$catch$Yr)]
  catch.scenarios <- round(c(0.8*tCatch,tCatch,1.25*tCatch))
  names(catch.scenarios) <- c("Catch_Adv.yr = 0.8*Catch_Term.yr",
                              "Catch_Adv.yr = Catch_Term.yr",
                              "Catch_Adv.yr = 1.25*Catch_Term.yr")
  l_Catch <- length(catch.scenarios)
  Catch_names <- paste0("Catch",catch.scenarios)
  
  #generate forecast files for the F based scenarios
  for (i in 1:l_FMult){
    
    fore_dat = data.frame(Year = seq(endyear+1, endyear+Nfor))
    fore_dat$Seas = 1
    fore_dat$Fleet = 1
    fore_dat$F = FMult[i]*lRPs$Fmsy
    
    fore$InputBasis <- 99
    fore$ControlRuleMethod <- 0
    fore$ForeCatch <- fore_dat
    
    ## write all forecast files/scenarios
    r4ss::SS_writeforecast(fore, dir = file.path(STF.dir,"Scenarios"), 
                           file = paste0("forecast",paste0("FMult_",FMult[i]), ".ss"), 
                           overwrite = TRUE, verbose = FALSE)
  }
  
  #generate forecast files for the catch based scenarios
  for (i in 1:l_Catch) {
    
    fore_dat = data.frame(Year = seq(endyear+1, endyear+Nfor))
    fore_dat$Seas = 1
    fore_dat$Fleet = 1
    fore_dat$Catch <- catch.scenarios[i]
    
    fore$InputBasis <- 2
    fore$ControlRuleMethod <- 0
    fore$ForeCatch <- fore_dat
    
    #write all forecast files/scenarios
    r4ss::SS_writeforecast(fore, dir = file.path(STF.dir,"Scenarios"), 
                           file = paste0("forecast",Catch_names[i], ".ss"), 
                           overwrite = TRUE, verbose = FALSE)
    
  }
  
  # create forecast subfolders for F multiplier options & copy in files required for the run
  for (i in 1:l_FMult){
    
    FMult.dir <- file.path(STF.dir,"Scenarios",paste0("FMult",FMult[i]))
    TAF::mkdir(FMult.dir)
    
    #copy the files for the assessment
    TAF::cp("data/starter.ss", FMult.dir)
    TAF::cp("data/BOC.dat", FMult.dir)
    TAF::cp("data/BOC.ctl", FMult.dir)
    
    #copy the scenario forecast file into the scenario specific folder and rename to forecast.ss
    TAF::cp(file.path(STF.dir,"Scenarios",paste0("forecastFmult_",FMult[i],".ss")),
            file.path(FMult.dir, "forecast.ss"))
    
    # #Edit "starter.ss" 
    # starter.file <- readLines(paste(dir.FMult, "/starter.ss", sep=""))
    # linen <- NULL
    # # linen <- grep("# 0=use init values in control file; 1=use ss.par", starter.file)
    # # starter.file[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par") # tells it to use the estimate parameters
    # linen <- grep("#_init_values_src", starter.file) # # 0=use init values in control file; 1=use ss.par
    # starter.file[linen] <- paste0("1 #_init_values_src") # tells it to use the estimate parameters
    # 
    # # linen <- grep("# Turn off estimation for parameters entering after this phase", starter.file)
    # # starter.file[linen] <- paste0("0 # Turn off estimation for parameters entering after this phase")
    # linen <- grep("#_last_estimation_phase", starter.file)
    # starter.file[linen] <- paste0("0 #_last_estimation_phase")
    # write(starter.file, paste(dir.FMult, "/starter.ss", sep=""))
    
  }
  
  # create forecast subfolders for catch scenarios & copy in files for the run
  for (i in 1:l_Catch){
    
    Catch.dir <- file.path(STF.dir,"Scenarios",paste0("Catch",catch.scenarios[i]))
    TAF::mkdir(Catch.dir)
    
    TAF::cp("data/starter.ss", Catch.dir)
    TAF::cp("data/BOC.dat", Catch.dir)
    TAF::cp("data/BOC.ctl", Catch.dir)
    
    #copy the scenario forecast file into the scenario specific folder and rename to forecast.ss
    TAF::cp(file.path(STF.dir,"Scenarios",paste0("forecastCatch",catch.scenarios[i],".ss")),
            file.path(Catch.dir, "forecast.ss"))
    
    # #Edit "starter.ss" 
    # starter.file <- readLines(paste(dir.FMult, "/starter.ss", sep=""))
    # linen <- NULL
    # # linen <- grep("# 0=use init values in control file; 1=use ss.par", starter.file)
    # # starter.file[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par") # tells it to use the estimate parameters
    # linen <- grep("#_init_values_src", starter.file) # # 0=use init values in control file; 1=use ss.par
    # starter.file[linen] <- paste0("1 #_init_values_src") # tells it to use the estimate parameters
    # 
    # # linen <- grep("# Turn off estimation for parameters entering after this phase", starter.file)
    # # starter.file[linen] <- paste0("0 # Turn off estimation for parameters entering after this phase")
    # linen <- grep("#_last_estimation_phase", starter.file)
    # starter.file[linen] <- paste0("0 #_last_estimation_phase")
    # write(starter.file, paste(dir.FMult, "/starter.ss", sep=""))
    
  }
  
  #function definition to run forecast in parallel
  runSS <- function(dir){r4ss::run(dir = dir, exe = "SS3_win.exe", 
                                   show_in_console = FALSE, skipfinished = FALSE)}
  
  #run the forecast models
  cat("Running",length(FMult),"F based forecasts\n")
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores=n.cores/2)
  system.time(foreach(dir = file.path(STF.dir,"Scenarios",paste0("FMult", FMult))) %dopar% runSS(dir))[3]
  foreach::registerDoSEQ()
  
  cat("Running",length(catch.scenarios),"catch based forecasts\n")
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores=n.cores/2)
  system.time(foreach(dir = file.path(STF.dir,"Scenarios", paste0("Catch",catch.scenarios))) %dopar% runSS(dir))[3]
  foreach::registerDoSEQ()
  
  #retrieve and summarise outputs
  forecastModels <- r4ss::SSgetoutput(dirvec = paste0("model/",runName,"/STF/Scenarios/", c(paste0("FMult",FMult),Catch_names)), getcovar = FALSE)
  save(FMult, catch.scenarios, forecastModels, Term.yr, Adv.yr, Adv.yr.next, tCatch,
       file=file.path("model",paste0(runName,"_STF_fit.Rdata")))
  
}

source("output.R")

#rho.SSB$AFSC_Hurtado_SSB 0.08640717 
#rho.RecFBar$AFSC_Hurtado_F -0.1559291 
#rho.RecFBar$AFSC_Hurtado_Rec 0.2816355 

#comparison with base
runs2comp <- c("2.0.WGWIDE26_Base","2.0.2.WGWIDE26_Base")

source("output_comparisons.R")

if (do.jitter) {
  
  # Get input files
  TAF::cp("data/BOC.dat", "model")
  TAF::cp("data/BOC.ctl", "model")
  TAF::cp("data/forecast.ss", "model")
  TAF::cp("data/starter.ss", "model")
  
  #copy model executable
  cp("boot/software/ss3_win_V3.30.23.2.exe", "model")
  
  jit.likes <- r4ss::jitter(dir = "model", exe = "SS3_win.exe",
                            Njitter=25, jitter_fraction = 0.1, init_values_src = 0)
  
  #likesaved
  
  profilemodels <- SSgetoutput(dirvec = "model", keyvec = 1:25, getcovar = FALSE)
  profilesummary <- SSsummarize(profilemodels)
  profilesummary[["likelihoods"]][1, ]
  profilesummary[["pars"]]
  
  table(jit.likes)
  #jit.likes
  #615.883 620.296 625.271  632.65 847.901 877.403 
  #13       1       3       1       2       5
  
  replist$likelihoods_used
  
}


