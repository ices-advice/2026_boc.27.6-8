#WGWIDE 2026 Boarfish
#comparison plots from stock assessment for boc.27.6-8

library(icesTAF)
library(r4ss)
library(ggplot2)

op.dir <- file.path("output","model comparisons",paste(runs2comp,collapse = "_vs_"))
TAF::mkdir(op.dir)

#retrieve outputs
lCompare <- r4ss::SSgetoutput(dirvec=file.path(getwd(),"model",runs2comp))
names(lCompare) <- runs2comp

#SS3 versions
ulCompare <- unlist(lCompare)
ulCompare[names(ulCompare)[grepl("SS_version$",names(ulCompare))]]

#gradients
ulCompare[names(ulCompare)[grepl("maximum_gradient_component$",names(ulCompare))]]

r4ss::SSplotComparisons(r4ss::SSsummarize(lCompare),
                        legendlabels = runs2comp,
                        legendloc = "topleft",
                        print = TRUE,
                        #pdf = TRUE, 
                        plotdir = op.dir,
                        filenameprefix = "")
