#WGWIDE 2026 Boarfish
#generate the outputs from stock assessment for boc.27.6-8

# Preamble ----

mkdir("output")
op.dir <- file.path("output",runName)
mkdir(file.path(op.dir,"retros"))
mkdir(file.path(op.dir,"diags"))
mkdir(file.path(op.dir,"STF"))

load(file.path("model",paste0(runName,"_fit.Rdata")))

retros.done <- file.exists(file.path("model",paste0(runName,"_retro_fit.Rdata")))
STF.done <- file.exists(file.path("model",paste0(runName,"_STF_fit.Rdata")))

# Load ----
if (retros.done) load(file.path("model",paste0(runName,"_retro_fit.Rdata")))
if (STF.done) load(file.path("model",paste0(runName,"_STF_fit.Rdata")))

#main run outputs - pdf format & html
r4ss::SS_plots(replist, pdf = TRUE, png = FALSE, dir = paste0("output/",runName))

#uncomment to launch plots in browser
r4ss::SS_plots(replist, dir = paste0("output/",runName), printfolder = "SS_Plots")

# Parameters ----

#table of parameters
table_pars <- r4ss::table_pars(replist)

#filter for those estimate & exclude deviations
dfPars <- data.frame(table_pars[["table"]] %>% 
  filter(Phase>0) %>% 
  filter(!grepl("DEV",toupper(Label))))

#tabulate parameters
ft <- flextable::flextable(dfPars) %>%
  set_table_properties(layout="autofit")

#write to word doc
officer::read_docx() %>%
  officer::body_add_par(paste0(runName," Parameter Estimates"),style = "centered") %>%
  officer::body_add_par(Sys.time(),style = "centered") %>%
  officer::body_add_par("") %>%
  flextable::body_add_flextable(value = ft, split = TRUE) %>%
  officer::body_end_section_landscape() %>% 
  print(target = file.path("output",runName,"ParameterEstimates.docx"))

# Retro ----

#retro outputs
if (retros.done) {
  
  # Set the ending year of each model in the set
  endyrvec <- retroSummary$endyrs + 0:-5
   
  ###Calculate Mohn's Rho values for select quantities
  rho.SSB <- r4ss::SSmohnsrho(retroSummary, endyrvec = endyrvec+1, verbose = TRUE)
  rho.RecFBar <- r4ss::SSmohnsrho(retroSummary, endyrvec = endyrvec, verbose = TRUE)
  
  cat("rho.SSB$AFSC_Hurtado_SSB",rho.SSB$AFSC_Hurtado_SSB,"\n")
  cat("rho.RecFBar$AFSC_Hurtado_F",rho.RecFBar$AFSC_Hurtado_F,"\n") 
  cat("rho.RecFBar$AFSC_Hurtado_Rec",rho.RecFBar$AFSC_Hurtado_Rec,"\n")
   
  #SSB retroplot
  r4ss::SSplotComparisons(retroSummary, subplots = 2, endyrvec = endyrvec+1, plot=F, 
                         legendlabels = paste("Data",0:-5,"years"), 
                         png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T, 
                         pwidth = 4, pheight = 4, filenameprefix = "SSB", legendloc = "topleft",
                         labels = c("Year","SSB (kt)"), lwd=1, pch=20, 
                         xlim = c(retroSummary$startyrs[1],retroSummary$endyrs[1]+1))
   
  retroSummary$FvalueLabels <- "FBar (4-14)"
  
  #F retro
  r4ss::SSplotComparisons(retroSummary, subplots = 8, endyrvec = endyrvec+1, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "FBar", legendloc = "topleft",
                          labels = c("Year"), lwd=1, pch=20, xlim = c(retroSummary$startyrs[1],
                                                                      retroSummary$endyrs[1]))

  r4ss::SSplotComparisons(retroSummary, subplots = 8, plot=T, endyrvec = endyrvec+1,
                          legendlabels = paste("Data",0:-5,"years"),
                          uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "FBar", legendloc = "topleft",
                          labels = c("Year"), lwd=1, pch=20, xlim = c(retroSummary$startyrs[1],
                                                                      retroSummary$endyrs[1]))
  
    
  #recruits
  r4ss::SSplotComparisons(retroSummary, subplots = 10, endyrvec = endyrvec, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "Recr", legendloc = "topleft",
                          labels = c("Year","","","Recruits (billions)"), lwd=1, pch=20, xlim = c(retroSummary$startyrs[1],
                                                                                                  retroSummary$endyrs[1]))
  
  #recent recruits
  r4ss::SSplotComparisons(retroSummary, subplots = 10, endyrvec = endyrvec, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "Recr_recent_", legendloc = "topright",
                          labels = c("Year","","","Recruits (billions)"), lwd=1, pch=20, 
                          xlim = c(retroSummary$endyrs[1]-10,
                                   retroSummary$endyrs[1]))
  
  
  #Reference run
  ss3rep = retroModels[[1]]
  
  # #retro test calculations
  # base <- retroModels[[1]]
  # peel1 <- retroModels[[2]]
  # peel2 <- retroModels[[3]]
  # peel3 <- retroModels[[4]]
  # peel4 <- retroModels[[5]]
  # peel5 <- retroModels[[6]]
  # 
  # #FBar
  # 0.2*(
  # (peel1$derived_quants["F_2024","Value"]-base$derived_quants["F_2024","Value"])/base$derived_quants["F_2024","Value"] +
  # (peel2$derived_quants["F_2023","Value"]-base$derived_quants["F_2023","Value"])/base$derived_quants["F_2023","Value"] +
  # (peel3$derived_quants["F_2022","Value"]-base$derived_quants["F_2022","Value"])/base$derived_quants["F_2022","Value"] +
  # (peel4$derived_quants["F_2021","Value"]-base$derived_quants["F_2021","Value"])/base$derived_quants["F_2021","Value"] +
  # (peel5$derived_quants["F_2020","Value"]-base$derived_quants["F_2020","Value"])/base$derived_quants["F_2020","Value"])
  # 
  # #Rec
  # 0.2*(
  # (peel1$derived_quants["Recr_2024","Value"]-base$derived_quants["Recr_2024","Value"])/base$derived_quants["Recr_2024","Value"] +
  # (peel2$derived_quants["Recr_2023","Value"]-base$derived_quants["Recr_2023","Value"])/base$derived_quants["Recr_2023","Value"] +
  # (peel3$derived_quants["Recr_2022","Value"]-base$derived_quants["Recr_2022","Value"])/base$derived_quants["Recr_2022","Value"] +
  # (peel4$derived_quants["Recr_2021","Value"]-base$derived_quants["Recr_2021","Value"])/base$derived_quants["Recr_2021","Value"] +
  # (peel5$derived_quants["Recr_2020","Value"]-base$derived_quants["Recr_2020","Value"])/base$derived_quants["Recr_2020","Value"])
  # 
  # #SSB (reported on Jan 1 of following year)
  # 0.2*(
  # (peel1$derived_quants["SSB_2025","Value"]-base$derived_quants["SSB_2025","Value"])/base$derived_quants["SSB_2025","Value"] +
  # (peel2$derived_quants["SSB_2024","Value"]-base$derived_quants["SSB_2024","Value"])/base$derived_quants["SSB_2024","Value"] +
  # (peel3$derived_quants["SSB_2023","Value"]-base$derived_quants["SSB_2023","Value"])/base$derived_quants["SSB_2023","Value"] +
  # (peel4$derived_quants["SSB_2022","Value"]-base$derived_quants["SSB_2022","Value"])/base$derived_quants["SSB_2022","Value"] +
  # (peel5$derived_quants["SSB_2021","Value"]-base$derived_quants["SSB_2021","Value"])/base$derived_quants["SSB_2021","Value"])
  

  #data plot
  Cairo(file = file.path(getwd(),"output",runName,"diags","datasetup.png"), 
        type = "png", width = 6, height = 4, res = 300, units = "in")
  r4ss::SSplotData(ss3rep, subplots = 2)
  dev.off()
  
  #residuals
  
  #indices
  
  nIdx <- nrow(replist$index_variance_tuning_check)
  
  for (f in seq(1,nIdx)){
    ss3diags::SSplotRunstest(ss3rep, subplots="cpue", indexselect=f, mixing="two.sided", 
                             print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Idx_")
  }
  
  #multi plot
  sspar(mfrow=c(1,nIdx),plot.cex = 0.8)
  
  ss3diags::SSplotRunstest(ss3rep, subplots="cpue", add=T,
                           legendcex = 0.6, mixing="two.sided",
                           plotdir = paste0("output/",runName,"/diags"),
                           filenameprefix = "Idx_All")
  
  dev.print(jpeg, paste0("output/",runName,"/diags/Idx_residruns.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
  
  #Mean Length
  
  sspar()
  
  nFleets <- length(unique(replist$len_comp_fit_table$Fleet))
  
  for (f in seq(1,nFleets)) {
    ss3diags::SSplotRunstest(ss3rep, subplots="len", indexselect=f, mixing="two.sided", 
                             print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Len_")
  }
  
  
  #multi plot
  sspar(mfrow=c(1,nFleets),plot.cex = 0.8)
  
  SSplotRunstest(ss3rep, subplots="len",add=T, legendcex = 0.6, mixing="two.sided")
  
  dev.print(jpeg, paste0("output/",runName,"/diags/Len_residruns.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
  
  #check for conflicts between indices and mean len
  ss3diags::SSplotJABBAres(ss3rep, subplots="len", col=sscol(3)[c(1,3,2)],
                           print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                           filenameprefix = "Len_")
  
  ss3diags::SSplotJABBAres(ss3rep, subplots="cpue", col=sscol(3)[c(1,3,2)],
                           print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                           filenameprefix = "Idx_")
  
  
  sspar(mfrow=c(1,2),plot.cex = 0.8)
  SSplotJABBAres(ss3rep,subplots="cpue",add=T,col=sscol(3)[c(1,3,2)])
  SSplotJABBAres(ss3rep,subplots="len",add=T,col=sscol(3)[c(1,3,2)])
  
  dev.print(jpeg, paste0("output/",runName,"/diags/JointResids.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
    sspar(mfrow=c(1,2),plot.cex = 0.8)
  #Survey Indices
  
  ss3diags::SSplotRunstest(ss3rep, subplots="cpue", add=T,
                           legendcex = 0.6, mixing="two.sided",
                           plotdir = paste0("output/",runName,"/diags"),
                           filenameprefix = "Idx_All")
  
  #dev.print(jpeg, file.path(diag.dir,paste0("RunsTestResiduals_Idx_",runref,".jpg")), 
  #          width = 8, height = 7, res = 300, units = "in")
  
  
  
  #retro analysis with hindcasting
  
  if (retros.done){
    for (idx in seq(1,nIdx)) {
      ss3diags::SSplotHCxval(retroSummary, subplots="cpue", indexselect=idx, xmin=2006,
                             print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Index_")
    }
    
    hccomps = ss3diags::SSretroComps(retroModels)
    
    for (flt in seq(1,nFleets)){
      ss3diags::SSplotHCxval(hccomps, subplots="len", indexselect=flt, ylim=c(5,16),
                             uncertainty=TRUE, print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Len_")
    }
  }
  
}


# STF ----

if (STF.done) {

  #build a summary of the forecast runs
  forecastSummary <- r4ss::SSsummarize(forecastModels)
  
  #SSB, F, Recr
  SSB <- as.data.frame(forecastSummary[["SpawnBio"]])
  SSB.SD <- as.data.frame(forecastSummary[["SpawnBioSD"]])
  SSB.Lower <- as.data.frame(forecastSummary[["SpawnBioLower"]])
  SSB.Upper <- as.data.frame(forecastSummary[["SpawnBioUpper"]])
  Fvalue <- as.data.frame(forecastSummary[["Fvalue"]])
  Recr <- as.data.frame(forecastSummary[["recruits"]])
  
  #summarise results
  all.scen <- c(FMult,catch.scenarios)
  num.scen <- length(all.scen)
  dfSummary <- 
    data.frame(Scenario.Type = c(rep("FMult",length(FMult)),(rep("Catch",length(catch.scenarios)))),
               Scenario.Name = c(names(FMult),names(catch.scenarios)),
               Val = all.scen,
               SSB_Adv.yr = as.numeric(SSB[SSB$Yr==Adv.yr,paste0('replist',seq(1,num.scen))]),
               SSB_Adv.yr_SD = as.numeric(SSB.SD[SSB$Yr==Adv.yr,paste0('replist',seq(1,num.scen))]),
               SSB_Adv.yr.next = as.numeric(SSB[SSB$Yr==Adv.yr+1,paste0('replist',seq(1,num.scen))]),
               SSB_Adv.yr.next_SD = as.numeric(SSB.SD[SSB$Yr==Adv.yr+1,paste0('replist',seq(1,num.scen))]),
               F_Term.yr = as.numeric(Fvalue[Fvalue$Yr==Term.yr,paste0('replist',seq(1,num.scen))]),
               F_Adv.yr = as.numeric(Fvalue[Fvalue$Yr==Adv.yr,paste0('replist',seq(1,num.scen))]),
               F_Adv.yr.next = as.numeric(Fvalue[Fvalue$Yr==Adv.yr+1,paste0('replist',seq(1,num.scen))]),
               Rec_Adv.yr = as.numeric(Recr[Recr$Yr==Adv.yr,paste0('replist',seq(1,num.scen))]),
               Rec_Adv.yr.next = as.numeric(Recr[Recr$Yr==Adv.yr+1,paste0('replist',seq(1,num.scen))]),
               Catch_Adv.yr = NA, Catch_Adv.yr.next = NA, Fmsy=lRPs$Fmsy, Blim=lRPs$Blim, 
               Adv_Term.yr = Adv_Term.yr, Catch_Term.yr = tCatch)
  
  #probablility of being below Blim in year following Advice Year 
  dfSummary$pBlim_Adv.yr.next <- pnorm(lRPs$Blim, dfSummary$SSB_Adv.yr.next, dfSummary$SSB_Adv.yr.next_SD)
  
  #get catches
  for (i in 1:num.scen){
    output = forecastModels[[i]]
    catch <- output$timeseries %>% filter(Era == "FORE" ) %>% 
      select("Yr", starts_with("dead(B)")) %>% 
      mutate(FMult = FMult[i])
    names(catch) <- c("Year","Catch","FMult")
    catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
    dfSummary$Catch_Adv.yr[i] <- unlist(catch[,paste0("Catch_",Adv.yr)])
    dfSummary$Catch_Adv.yr.next[i] <- unlist(catch[,paste0("Catch_",Adv.yr.next)])
  }
  
  # ggplot(data = dfSummary %>% filter(Scenario.Type=="FMult" & 
  #                                      F_Adv.yr < 2*max(lRPs$Flim,lRPs$Fpa,lRPs$Fmsy)),
  #        mapping = aes(x=F_Adv.yr,y=Catch_Adv.yr,label=Val)) +
  #   xlim(0.999*lRPs$Fmsy,1.001*lRPs$Fmsy) +
  #   geom_label() +
  #   geom_point(col="red") + 
  #   geom_vline(xintercept = lRPs$Fmsy, col="red") +
  #   geom_vline(xintercept = lRPs$Flim, col="red") +
  #   geom_vline(xintercept = lRPs$Fpa, col="red")  
  
  dfSummary$Delta <- NA
  dfSummary$Selected <- FALSE
  
  #best approximation for Fmsy, Fpa in Adv.yr
  cond <- dfSummary$Scenario.Name=="F_Adv.yr = FMSY"
  if (sum(cond)>0) {
    dfSummary[cond,]$Delta <- abs(dfSummary[cond,"F_Adv.yr"] - lRPs$Fmsy)
    dfSummary[cond,]$Selected <- dfSummary[cond,]$Delta == min(dfSummary[cond,]$Delta)
  }
  
  #F=0 in Adv.yr
  dfSummary[dfSummary$Scenario.Name=="F_Adv.yr = 0",]$Selected <- TRUE
  
  #best approximation for Flim in Adv.yr
  cond <- dfSummary$Scenario.Name=="F_Adv.yr = Flim"
  if (sum(cond)>0) {
    dfSummary[cond,]$Delta <- abs(dfSummary[cond,"F_Adv.yr"] - lRPs$Flim)
    dfSummary[cond,]$Selected <- dfSummary[cond,]$Delta == min(dfSummary[cond,]$Delta)
  }
  
  #F in Adv.yr = F in Term.yr (Fsq)
  Fsq <- dfSummary[,"F_Term.yr"][1]
  cond <- dfSummary$Scenario.Name == "F_Adv.yr = F_Term.yr"
  if (sum(cond)>0){
    dfSummary[cond,]$Delta <- abs(dfSummary[cond,"F_Adv.yr"] - Fsq)
    dfSummary[cond,]$Selected <- dfSummary[cond,]$Delta == min(dfSummary[cond,]$Delta)
  }
  
  #SSB in Adv.yr+1 & Blim
  cond <- dfSummary$Scenario.Name=="SSB_Adv.yr.next = Blim"
  if (sum(cond)>0){
    dfSummary[cond,]$Delta <- abs(dfSummary[cond,"SSB_Adv.yr.next"] - lRPs$Blim)
    dfSummary[cond,]$Selected <- dfSummary[cond,]$Delta == min(dfSummary[cond,]$Delta)
  }

  #SSB in Adv.yr+1 & MSYBtrigger
  cond <- dfSummary$Scenario.Name=="SSB_Adv.yr.next = MSYBtrigger"
  if(sum(cond)>0){
    dfSummary[cond,]$Delta <- abs(dfSummary[cond,"SSB_Adv.yr.next"] - lRPs$MSYBtrigger)
    dfSummary[cond,]$Selected <- dfSummary[cond,]$Delta == min(dfSummary[cond,]$Delta)
  }
  
  #exploratory plots
  #Catch vs F in advice year
  # dfSub <- dfSummary %>% filter(Scenario.Type=="FMult") %>% select(paste0(c("F","Catch"),"_",Adv.yr),Val)
  # names(dfSub) <- c("F","Catch","Mult")
  # ggplot(data = dfSub, mapping = aes(x=F,y=Catch,label=Mult)) + 
  #   geom_point() + geom_label() + xlab(paste0("F_",Adv.yr)) + ylab(paste0("Catch_",Adv.yr)) +
  #   geom_vline(xintercept = lRPs$Fsq, col="red") + theme_bw()
  
  #SSB in advice year + 1 vs F in advice year
  # dfSub <- dfSummary %>% filter(Scenario.Type=="FMult") %>% select(paste0("SSB_",Adv.yr.next),paste0("F_",Adv.yr),Val)
  # names(dfSub) <- c("SSB","F","Mult")
  # ggplot(data = dfSub, mapping = aes(x=F,y=SSB,label=Mult)) + 
  #   geom_point() + geom_label(nudge_x = 0.01) + xlab(paste0("F_",Adv.yr)) + ylab(paste0("SSB_",Adv.yr.next)) +
  #   geom_hline(yintercept = Blim, col="red")
  
  #select all catch constraint scenarios
  dfSummary$Selected[grepl("Catch_Adv.yr = ", dfSummary$Scenario.Name)] <- T

  ## Build STF table ----
  #build catch options dataframe  
  dfOptions <- dfSummary %>% 
    filter(Selected) %>%
    mutate(Catch_Adv.yr = round(Catch_Adv.yr),
           F_Adv.yr = round(F_Adv.yr,3),
           SSB_chg = round(100*((SSB_Adv.yr.next/SSB_Adv.yr)-1),3), 
           Catch_chg = round(100*((Catch_Adv.yr/Catch_Term.yr)-1),3), 
           Adv_chg = round(100*((Catch_Adv.yr/Adv_Term.yr)-1),3),
           pBlim_Adv.yr.next = round(pBlim_Adv.yr.next,2)) %>%
    select(Scenario.Name,Catch_Adv.yr,F_Adv.yr,SSB_Adv.yr.next,SSB_chg,Catch_chg,Adv_chg,pBlim_Adv.yr.next)
  
  #select/order
  dfOptions <- dfOptions[match(c("F_Adv.yr = FMSY", "F_Adv.yr = 0", "F_Adv.yr = F_Term.yr",
                                 "SSB_Adv.yr.next = Blim", "SSB_Adv.yr.next = MSYBtrigger", 
                                 "Catch_Adv.yr = 0.8*Catch_Term.yr", "Catch_Adv.yr = Catch_Term.yr", "Catch_Adv.yr = 1.25*Catch_Term.yr"), 
                               dfOptions$Scenario.Name),]
  
  #sub in actual years
  dfOptions$Scenario.Name <- stringr::str_replace_all(dfOptions$Scenario.Name,"Adv.yr.next",as.character(Adv.yr.next))
  dfOptions$Scenario.Name <- stringr::str_replace_all(dfOptions$Scenario.Name,"Adv.yr",as.character(Adv.yr))
  dfOptions$Scenario.Name <- stringr::str_replace_all(dfOptions$Scenario.Name,"Term.yr",as.character(Term.yr))
  names(dfOptions) <- stringr::str_replace_all(names(dfOptions),"Adv.yr.next",as.character(Adv.yr.next))
  names(dfOptions) <- stringr::str_replace_all(names(dfOptions),"Adv.yr",as.character(Adv.yr))
  names(dfOptions) <- stringr::str_replace_all(names(dfOptions),"Term.yr",as.character(Term.yr))

  #create a flextable object
  ftOptions <- flextable(dfOptions)
  ftOptions <- flextable::colformat_num(ftOptions,j=2,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::colformat_num(ftOptions,j=4,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::colformat_num(ftOptions,j=6,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::autofit(ftOptions)

  #output to word doc
  officer::read_docx() %>% 
    officer::body_add_par(paste0(runName," STF"),style = "centered") %>%
    officer::body_add_par(Sys.time(),style = "centered") %>%
    officer::body_add_par("") %>%
    flextable::body_add_flextable(ftOptions) %>% 
    officer::body_end_section_landscape() %>% 
    print(target = file.path(op.dir,"STFOptions.docx"))
  
}


