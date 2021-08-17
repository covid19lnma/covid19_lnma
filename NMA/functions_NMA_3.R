library(tidyverse)
library(metafor)
library(bayesmeta)
library(readxl)
library(forestplot)
library(magrittr)
library(netmeta)

order.df <- function(pairwise, placebo){
  for (i in 1:nrow(pairwise)){
    if ((pairwise[i,"t1"] == placebo) | (as.numeric(pairwise[i,"tid1"]) > as.numeric(pairwise[i, "tid2"]))){
      auxt = pairwise[i,"t2"]
      pairwise[i,"t2"] = pairwise[i,"t1"]
      pairwise[i,"t1"] = auxt
      
      auxtid = pairwise[i,"tid2"]
      pairwise[i,"tid2"] = pairwise[i,"tid1"]
      pairwise[i,"tid1"] = auxtid
      
      auxn = pairwise[i,"n2"]
      pairwise[i,"n2"] = pairwise[i,"n1"]
      pairwise[i,"n1"] = auxn
      
      auxmean = pairwise[i,"mean2"]
      pairwise[i,"mean2"] = pairwise[i,"mean1"]
      pairwise[i,"mean1"] = auxmean
      
      auxsd = pairwise[i,"sd2"]
      pairwise[i,"sd2"] = pairwise[i,"sd1"]
      pairwise[i,"sd1"] = auxsd
    }
  }
  return(pairwise)
}

get.network.pdf <- function(pairwise, measure, placebo, folder, name){
  
  pathname <- paste0(folder,"/output/", name,".pdf")
  pdf(pathname, width = 8, height = 5, pointsize = 6)
  
  if (measure=="MD" | measure == "ROM") {
    contrast_df=pairwise(list(t1,t2), mean = list(mean1,mean2), n = list(n1,n2),sd=list(sd1,sd2),studlab = study, data = pairwise, sm = measure)
  } else if (measure == "OR"){
    pairwise %<>% filter((e.events != 0 & c.events !=0))
    contrast_df=pairwise(list(t1,t2), event = list(e.events,c.events), n = list(e.total,c.total),studlab = study, data = pairwise, sm = measure) 
  } 
  
  network=netmeta(contrast_df,reference.group = placebo,details.chkmultiarm = T,comb.fixed = F)
  netgraph(network,multiarm = F)
  dev.off()
}