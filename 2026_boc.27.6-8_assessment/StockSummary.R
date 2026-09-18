#Stock Summary

rm(list=ls())
gc()

library(tidyverse)
library(Cairo)
library(flextable)

## set runName & params ----

runName <- "2.3.WGWIDE26_Base_finalconfig"
label <- "WGWIDE 2026"
#runName <- "1.0.WGWIDE25_Final"
#label <- "WGWIDE 2025"

showForecastPeriod <- TRUE

#Reference Points as per 2024 benchmark
Blim <- 156762
Bpa <- 190845
Flim <- 0.175
Fp05 <- 0.042
Fpa <- 0.042
Fmsy <- 0.042
MSYBtrigger <- 190845

dfRefPts <- data.frame(
  Name = c("Blim","Bpa","Flim","Fp05","Fpa","Fmsy","MSYBtrigger"),
  Val = c(156.762,190.845,0.175,0.042,0.042,0.042,190.845),
  Qty = c("SSB (kt)","SSB (kt)","FBar (4-14)","FBar (4-14)","FBar (4-14)","FBar (4-14)","SSB (kt)")
)

# load ----

if (file.exists(file.path("model",paste0(runName,"_retro_fit.Rdata")))){load(file.path("model",paste0(runName,"_retro_fit.Rdata")))}

if (file.exists(file.path("model",paste0(runName,"_STF_fit.Rdata")))){
  load(file.path("model",paste0(runName,"_STF_fit.Rdata")))
  #need forecast for this
  FMSY.model <- forecastModels[[3]]
}

# Reference run
ss3rep = retroModels[[1]]
#forecast models
#forecastSummary <- SSsummarize(forecastModels)

yrs <- ss3rep$timeseries$Yr[ss3rep$timeseries$Era=="TIME"]
#SSB is given in the first year of the forecast period
yrs.ssb <- c(yrs,max(yrs)+1)


dfStockSummary <- bind_rows(
  data.frame(Year = c(yrs.ssb),
             Qty = "SSB (kt)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             Era = "TIME",
             WG = runName,
             Label = label),
  data.frame(Year = yrs,
             Qty = "FBar (4-14)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             Era = "TIME",
             WG = runName,
             Label = label),
  data.frame(Year = yrs,
             Qty = "Recr (billions)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             Era = "TIME",
             WG = runName,
             Label = label),
  data.frame(Year = yrs,
             Qty = "Catch (kt)",
             Val = ss3rep$catch$Obs[ss3rep$catch$Yr %in% yrs]/1e3,
             SD = 0,
             Era = "TIME",
             WG = runName,
             Label = label)
)
  
#if required, append further assessments for comparison
#runName2 <- "2.0.3.WGWIDE26_Base"
#label2 = "Excl HERAS"
runName2 <- c("1.0.WGWIDE25_Final","1.4.WGWIDE25_Final_incHERAS") 
label2 = c("WGWIDE 2025","WGWIDE 2025 inc HERAS")

for (r in seq(1,length(runName2))){
  
  if (file.exists(file.path("model",paste0(runName2[r],"_fit.Rdata")))){
    
    load(file.path("model",paste0(runName2[r],"_fit.Rdata")))
    
    #load(file.path("model",paste0("1.0.WGWIDE25_Final","_retro_fit.Rdata")))
    ss3rep <- replist
    
    yrs <- ss3rep$timeseries$Yr[ss3rep$timeseries$Era=="TIME"]
    #SSB is given in the first year of the forecast period
    yrs.ssb <- c(yrs,max(yrs)+1)
    
    dfStockSummary <- bind_rows(
      dfStockSummary,
      data.frame(Year = c(yrs.ssb),
                 Qty = "SSB (kt)",
                 Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
                 SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
                 Era = "TIME",
                 WG = runName2[r],
                 Label = label2[r]),
      data.frame(Year = yrs,
                 Qty = "FBar (4-14)",
                 Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
                 SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
                 Era = "TIME",
                 WG = runName2[r],
                 Label = label2[r]),
      data.frame(Year = yrs,
                 Qty = "Recr (billions)",
                 Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
                 SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
                 Era = "TIME",
                 WG = runName2[r],
                 Label = label2[r]),
      data.frame(Year = yrs,
                 Qty = "Catch (kt)",
                 Val = ss3rep$catch$Obs[ss3rep$catch$Yr %in% yrs]/1e3,
                 SD = 0,
                 Era = "TIME",
                 WG = runName2[r],
                 Label = label2[r])
    )
  }
}

dfStockSummary$CV <- dfStockSummary$SD/dfStockSummary$Val
dfStockSummary %>% filter(Qty=="FBar (4-14)" & Year >2023) 

if (showForecastPeriod) {

  #manually add forecast period details for FMSY
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2027, Qty = "Catch (kt)", Val = 33.543, SD=0, Era = "FORE",
                WG = "2.3.WGWIDE26_Base_finalconfig", Label = "WGWIDE 2026")
   )
   
   #FBar
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2027, Qty = "FBar (4-14)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="F_2027"], 
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="F_2027"], Era = "FORE",
                WG = "2.3.WGWIDE26_Base_finalconfig", Label = "WGWIDE 2026")
   )
   
   #Recruits
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2027, Qty = "Recr (billions)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="Recr_2027"]/1e6, 
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="Recr_2027"]/1e6, Era = "FORE",
                WG = "2.3.WGWIDE26_Base_finalconfig", Label = "WGWIDE 2026")
   )
  
   #SSB
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2028, 
                Qty = "SSB (kt)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="SSB_2028"]/1e3,
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="SSB_2028"]/1e3, 
                Era = "FORE",
                WG = "2.3.WGWIDE26_Base_finalconfig", Label = "WGWIDE 2026")
   )
  
}

#summary plot with RPs
p <- ggplot(data = dfStockSummary %>% filter(Label=="WGWIDE 2026"), 
            mapping = aes(x=Year,y=Val,col=Era)) + geom_line() + geom_point(size=1,pch=19) + 
  geom_errorbar(aes(ymin=Val-2*SD, ymax=Val+2*SD), lineend = "square", width=.2, position=position_dodge(0.05)) +
  geom_hline(data = filter(dfRefPts, Name %in% c("Blim","Fmsy")), aes(yintercept = Val), linetype=2) + 
  theme_bw() + facet_wrap(~Qty, scales="free_y",ncol=2) + ylab("") + theme(legend.position = "right")


Cairo(file = file.path(getwd(),"output",runName,"StockSummary_incSTF.png"), 
      type = "png", width = 9, height = 9, res = 300, units = "in")
print(p)
dev.off()

#comparison assessments
p <- ggplot(data = dfStockSummary %>% filter(Era=="TIME"), 
            mapping = aes(x=Year,y=Val,col=Label, group=Label)) + geom_line() + geom_point(size=2) + 
  geom_errorbar(aes(ymin=Val-2*SD, ymax=Val+2*SD), width=.2, position=position_dodge(0.05)) +
  geom_hline(data = filter(dfRefPts, Name %in% c("Blim","Fmsy")), aes(yintercept = Val), linetype=2) + 
  theme_bw() + facet_wrap(~Qty, scales="free_y",ncol=2) + ylab("") +
  theme(legend.position = "bottom") + theme(legend.title = element_blank())

Cairo(file = file.path(getwd(),"output",runName,"StockSummaryWG26_WG25_WG25Heras.png"), 
      type = "png", width = 9, height = 9, res = 300, units = "in")
print(p)
dev.off()


#Generate files containing data for SAG worksheet

#recruitment
write.table(dfSAG.Recr <- dfStockSummary %>%
              filter(Label == "WGWIDE 2026" & Era=="TIME" & Qty=="Recr (billions)") %>%
              mutate(values = Val) %>%
              mutate(Upper = exp(log(Val) + qnorm(1-(1-0.95)/2) * sqrt(log(1 + (SD/Val) * (SD/Val))))) %>%
              mutate(Lower = exp(log(Val) - qnorm(1-(1-0.95)/2) * sqrt(log(1 + (SD/Val) * (SD/Val))))) %>%  
              select(Lower,Val,Upper),
            file = file.path(getwd(),"output",runName,"SAG_Recr.csv"),
            row.names = FALSE, quote = FALSE, sep="\t")

#SSB
write.table(dfStockSummary %>%
              filter(Label == "WGWIDE 2026" & Era=="TIME" & Qty=="SSB (kt)") %>%
              mutate(Val.kt = 1e3*Val, 
                     Lower = 1e3*(Val - qnorm(1-(1-0.95)/2)*SD),
                     Upper = 1e3*(Val + qnorm(1-(1-0.95)/2)*SD)) %>%
              select(Lower,Val.kt,Upper),
          file = file.path(getwd(),"output",runName,"SAG_SSB.csv"),
          row.names = FALSE, quote = FALSE, sep="\t")

#FBar
write.table(dfStockSummary %>%
              filter(Label == "WGWIDE 2026" & Era=="TIME" & Qty=="FBar (4-14)") %>%
              mutate(Lower = Val - qnorm(1-(1-0.95)/2)*SD,
                     Upper = Val + qnorm(1-(1-0.95)/2)*SD) %>%
              select(Lower,Val,Upper),
          file = file.path(getwd(),"output",runName,"SAG_FBar.csv"),
          row.names = FALSE, quote = FALSE,sep="\t")

#stock summary table for report
#(advice version will be generated by SAG)

doc <- officer::read_docx()
ftStockSummary <- flextable(dfStockSummary %>% 
                              filter(Era=="TIME" & Label == "WGWIDE 2026") %>%
                              select(-Label,-CV,-SD,-Era,-WG) %>% 
                              pivot_wider(names_from = Qty, values_from = Val))
ftStockSummary <- flextable::colformat_num(ftStockSummary,j=1,big.mark="")
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(2),digits=3) #SSB
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(3),digits=5) #FBar
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(4),digits=5) #Rec
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(5),digits=3) #Catch
ftStockSummary <- flextable::autofit(ftStockSummary)
doc <- flextable::body_add_flextable(doc,ftStockSummary)
print(doc,target=file.path(getwd(),"output",runName,"StockSummary.docx"))