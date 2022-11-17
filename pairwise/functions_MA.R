library(tidyverse)
library(metafor)
library(bayesmeta)
library(readxl)
library(forestplot)

entry <- function(study, treatment, diff, std.err) {
  list(study = study, treatment = treatment, diff = diff, std.err = std.err)
}

as.entry <- function(row) {
  entry(row$study, row$treatment, row$diff, row$std.err)
}

convert <- function(re) {
  options(stringsAsFactors = FALSE)  # Because R
  results <- data.frame()
  studies <- unique(re$study)

  for(studyId in studies) {
    data <- subset(re, study == studyId)
    if(nrow(data) == 1) {
      ## Two-arm study
      base <- entry(studyId, data$base, NA, NA)
      treatment <- as.entry(data[1, ])
      results <- do.call("rbind", list(results, base, treatment))

    } else {
      ## Multi-arm study
      ## These entries are similar, but require standard error of the mean for the baseline
      ## We try to compute this if possible, if not we approximate it

      ## Guess at most common base, which is the one most referred to
      base.table <- table(data$base)
      base.trt <- names(base.table)[[which.max(base.table)]]

      treatments <- subset(data, base == base.trt)

      calc.base.se <- function(d.ab.se, d.ac.se, d.bc.se) {
        ## Let $Var(D_{BC}) = Var(D_{AB}) + Var(D_{AC}) - 2Var(Y_A)$, thus
        ## $se_B = \sqrt{(se^2_{AB} + se^2_{CB} - se^2_{AC}) / 2}$
        sqrt((d.ab.se^2 + d.ac.se^2 - d.bc.se^2) / 2)
      }

      approx.active.comparison <- function(d.ab, d.ac) {
        ## Try to use approximate Var(D_bc) using number of participants, if available (formula 9, Brooks et.al.)
        ## assuming the standard error is proportional to 1/sqrt(n)

        stopifnot(d.ab$base.n == d.ac$base.n) ## A is base should have same number of participants
        n.a <- d.ab$base.n
        n.b <- d.ab$treatment.n
        n.c <- d.ac$treatment.n

        if(all(!is.na(c(n.a, n.b, n.c)))) {
          sqrt(((d.ab$std.err^2 + d.ac$std.err^2) * (1/n.b + 1/n.c)) / (1/n.b + 1/n.c + 2/n.a))
        } else {
          NA
        }
      }

      base.se <- function(b, trt1, trt2) {
        d.ab <- subset(data, base == b & treatment == trt1)
        d.ac <- subset(data, base == b & treatment == trt2)
        d.bc <- subset(data, base == trt2 & treatment == trt1)

        se <- NA
        if(nrow(d.bc) != 0) {
          ##  We have enough data to compute baseline se
          se <- calc.base.se(d.ab$std.err, d.ac$std.err, d.bc$std.err)
        } else {
          ## We need to approximate D_bc
          d.bc.std.err <- approx.active.comparison(d.ab, d.ac)
          if(!is.na(d.bc.std.err)) {
            se <- calc.base.se(d.ab$std.err, d.ac$std.err, d.bc.std.err)
          }
        }
        se
      }

      ## Find the combinations of treatments that we could use (e.g. BC, BD)
      combinations <- t(combn(as.character(treatments$treatment), 2))

      ## Try methods by Brooks et.al. to computer or approximate baseline se,
      ## We take the median of all these values, NAs ommited
      se <- median(apply(combinations, 1, function(row) { base.se(base.trt, row[[1]], row[[2]]) }), na.rm=T)


      ## Append the treatments associated with the base
      if(is.finite(se)) { # not NaN, NA or infinite
        results <- rbind(results, entry(studyId, base.trt, NA, se))
        for(entry in by(treatments, 1:nrow(treatments), as.entry)) {
          results <- rbind(results, entry)
        }
      } else {
        warning(paste("could not calculate baseline std.err from data for study", studyId, "omitting"))
      }
    }
  }

  options(stringsAsFactors = TRUE) # Because R, but lets not break compatibility
  results
}

getestimates <- function(data, TP, TP1, baseline, measure, placebo, name.pdf,folder, digits = 3,folderROM){
  
  data=as.data.frame(data) # tibble doesnt work for subsetting
  #get 1 subdataframes with the treatment columns
  treatments <- select(data, t1, t2)
  
  #all the categories that could be found, even swapped columns
  categories <- distinct(treatments)
  
  #lists of the objects obtained in the loop 
  list.effsize = list()
  list.bm.Turner = list()
  list.bm.hnorm10 = list()
  list.bm.hnorm05 = list()
  list.bm.uniform = list()
  list.bm.jef = list()
  list.estimates = list()
  
  pathname <- paste0(folder,"/output/", gsub(".{4}$", "", name.pdf),".pdf")
  
  # output_dir <- file.path(folder, "output")
  # 
  # if (!dir.exists(output_dir)){
  #   dir.create(output_dir)
  # }
  # 
  pdf(pathname, width = 8, height = 5, pointsize = 6)
  
  for (i in 1:nrow(categories)) {
    p1 <- categories[i,1]
    p2 <- categories[i,2]
    
    if (measure == "OR") {
      
      effsize <- escalc(measure = measure,
                        ai = e.events,  n1i = e.total,
                        ci = c.events, n2i = c.total,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        digits = digits,
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5, 2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Odds ratio", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=digits)
      
    } else if (measure == "RD") {
      
      effsize <- escalc(measure = measure,
                        ai = e.events,  n1i = e.total,
                        ci = c.events, n2i = c.total,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        digits = digits,
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-1, 1),
                     xlim = c(-5,5),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Risk difference", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5, digits=digits)
      
      } else if (measure == "ROM") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        digits = digits,
                        data = data)
      
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5, 2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Ratio of means", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=digits)
      
    } else if (measure == "MD") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        digits = digits,
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-5, 5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Mean difference", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     digits=digits)
    }
    
    to.append <- effsize
    
    #if the new dataframe isn't already in the compiling list, append it
    #this filters out swapped treatments in columns t1 and t2
    if (is.element(list(to.append), list.effsize) == FALSE) {
      
      titlename <- paste(gsub('.{4}$', '', name.pdf), 
                         ": ", 
                         p1,
                         " vs ",
                         p2,
                         sep = "")
      
      title(titlename, line=0)
      
      colvec <- c("green","blue","red","yellow")
      par(mar=c(4,0.5,2,0.5))
      
      list.effsize[[i]] <- to.append
      
      #we use the same loop to obtain the bayesmeta.turner objects and append them in another loops
      if (p1 == placebo | p2 == placebo) {
        bm.Turner <- bayesmeta(effsize, tau.prior=TP$dprior)
        
      } else {
        bm.Turner <- bayesmeta(effsize, tau.prior=TP1$dprior)
        
      }
      #we append all bm.Turner into the list
      turner.to.append <- bm.Turner
      list.bm.Turner[[i]] <- turner.to.append
      
      #bm.hnorm objects, with the argument being 1 and 0.5
      bm.hnorm10 <- bayesmeta(effsize, tau.prior = function(x){dhalfnormal(x,scale=1.0)})
      bm.hnorm05 <- bayesmeta(effsize, tau.prior = function(x){dhalfnormal(x,scale=0.5)})
      
      bm.uniform <- bayesmeta(effsize, tau.prior = "conventional")
      #bm.jef <- bayesmeta(effsize, tau.prior = "Jeffreys")
      
      hnorm10.to.append <- bm.hnorm10
      hnorm05.to.append <- bm.hnorm05
      
      huniform.to.append <- bm.uniform
      #hjef.to.append <- bm.jef
      
      #append in the list and be done with it
      
      list.bm.hnorm10[[i]] <- hnorm10.to.append
      list.bm.hnorm05[[i]] <- hnorm05.to.append
      
      list.bm.uniform[[i]] <- huniform.to.append
      #list.bm.jef[[i]] <- hjef.to.append
      
      rma.fixed <- rma.uni(effsize, method="FE")
      rma.random.DL <- rma.uni(effsize, method="DL")
      
      #concat of the summarys, called estimates, same scheize, calculate and append
      estimates <- rbind("HNorm(0.5)" = c(bm.hnorm05$summary[2,1:2], bm.hnorm05$summary[5:6,2]),
                         #"HNorm(1.0)" = c(bm.hnorm10$summary[2,1:2], bm.hnorm10$summary[5:6,2]),
                         "Conventional" = c(bm.uniform$summary[2,1:2], bm.uniform$summary[5:6,2]),
                         #"Jeffreys" = c(bm.jef$summary[2,1:2], bm.jef$summary[5:6,2]),
                         "Turner Prior"=c(bm.Turner$summary[2,1:2], bm.Turner$summary[5:6,2]),
                         "Frequentist.fixed" = c(sqrt(rma.fixed$tau2.fix), rma.fixed$b, rma.fixed$ci.lb, rma.fixed$ci.ub),
                         "Frequentist.random.DL" = c(sqrt(rma.random.DL$tau2), rma.random.DL$b, rma.random.DL$ci.lb, rma.random.DL$ci.ub))
      
      if (measure == "OR") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp, digits = digits,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        if (p1 == placebo | p2 == placebo){
          
          estimates = estimates %>% 
            as_tibble(rownames="type") %>% 
            mutate(t1=p1,t2=p2,OR=exp(mu),OR_l=exp(`95% lower`),OR_u=exp(`95% upper`),
                   risk=-1*1000*(baseline-((OR*baseline)/(1-baseline+OR*baseline))),
                   risk_l=-1*1000*(baseline-((OR_l*baseline)/(1-baseline+OR_l*baseline))),
                   risk_u=-1*1000*(baseline-((OR_u*baseline)/(1-baseline+OR_u*baseline)))) %>% 
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
        } else {
          
          estimates = estimates %>% as_tibble(rownames="type") %>% 
            mutate(t1=p1,t2=p2,OR=exp(mu),OR_l=exp(`95% lower`),OR_u=exp(`95% upper`)) %>% 
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
          
        }
      } else if (measure == "RD") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4],
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0,
                  digits=digits)}
        
        estimates = estimates %>% 
          as_tibble(rownames="type") %>% 
          mutate(t1=p1,t2=p2,RD=(mu),RD_l=(`95% lower`),RD_u=(`95% upper`)) %>% 
          rename(mu_l=`95% lower`,mu_u=`95% upper`)
          
      } else if (measure == "ROM") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp, digits = digits,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        if (p1 == placebo | p2 == placebo){
          
          estimates = estimates %>% 
            as_tibble(rownames="type") %>% 
            mutate(t1=p1,t2=p2,ROM=exp(mu),ROM_l=exp(`95% lower`),ROM_u=exp(`95% upper`),
                   risk=ROM*baseline-baseline,
                   risk_l=ROM_l*baseline-baseline,
                   risk_u=ROM_u*baseline-baseline) %>% 
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
        } else {
          
          estimates = estimates %>% as_tibble(rownames="type") %>% 
            mutate(t1=p1,t2=p2,ROM=exp(mu),ROM_l=exp(`95% lower`),ROM_u=exp(`95% upper`)) %>% 
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
          
        }
        
      } else if (measure == "MD") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4],  digits = digits,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        estimates = estimates %>% as_tibble(rownames="type") %>% mutate(t1=p1,t2=p2) %>% 
          rename(mu_l=`95% lower`,mu_u=`95% upper`)
        
      }
      estimates.to.append <- estimates
      
      list.estimates[[i]] <- estimates.to.append
      #print(list.estimates)
    }
  }
  
  if(measure == "ROM"){
    bind_rows(list.effsize) %>% select(study,t1,t2,yi,vi,n1,n2) %>% 
      mutate(std.err=sqrt(vi)) %>%
      rename(base=t2,treatment=t1,diff=yi,base.n=n2,treatment.n=n1) %>% 
      mutate(treatment=gsub("^\\d+_(.*$)","\\1",treatment),
             base=gsub("^\\d+_(.*$)","\\1",base)) %>% convert() %>% 
      write_csv(paste0("~/covid19_lnma/NMA/",folderROM,"/",name.pdf))
    
  } else if(measure == "RD"){
    bind_rows(list.effsize) %>% 
      select(study,t1,t2,yi,vi,e.total,c.total) %>% 
      mutate(std.err=sqrt(vi)) %>%
      rename(base=t2,treatment=t1,diff=yi,base.n=c.total,treatment.n=e.total) %>% 
      mutate(treatment=gsub("^\\d+_(.*$)","\\1",treatment), base=gsub("^\\d+_(.*$)","\\1",base)) %>% 
      convert() %>% 
      write_csv(paste0("~/covid19_lnma/NMA/",folderROM,"/",name.pdf)) 
  }
  
  dev.off()
  return(list.estimates)
}

write.estimates.csv <- function(list.estimates ,folder,name) {
  
  rows.estimates <- tibble()
  for (i in 1:length(list.estimates)) {
    est <- as.data.frame(list.estimates[i][1])
    rows.estimates <- bind_rows(rows.estimates, est)
  }
  rows.estimates %>% filter(type=="Conventional") %>% 
    write_csv(paste0(folder,"/output/", name))
}

pairwise_ouput <- function(output, measure, digits, placebo, mainDir, folderROM){
  
  TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
  TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
  baseline =0.0
  
  if (output == "Mortality"){
    data=read.csv("input/Mortality - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "mortality.csv"
    
  } else if (output == "Infection with COVID-19 (laboratory confirmed)"){
    data=read.csv("input/Infection with COVID-19 (laboratory confirmed) - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Infection_COVID-19_(laboratory_confirmed).csv"
    
  } else if (output == "Infection with COVID-19 (confirmed and suspected)"){
    data=read.csv("input/Infection with COVID-19 (confirmed and suspected) - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Infection_COVID-19_(laboratory_confirmed_and_suspected).csv"
    
  } else if (output == "Mechanical ventilation"){
    data=read.csv("input/Mechanical ventilation - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "MV.csv"
    
  } else if (output == "Admission to hospital"){
    data=read.csv("input/Admission to hospital - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "admission_to_hospital.csv"
    
  } else if (output == "Adverse effects leading to discontinuation"){
    data=read.csv("input/Adverse effects leading to discontinuation - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "AE.csv"
    
  } else if (output == "Viral clearance"){
    data=read.csv("input/Viral clearance - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "viral_clearance.csv"
    
  } else if (output == "Venous thromboembolism"){
    data=read.csv("input/Venous thromboembolism - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "VTE.csv"
    
  } else if (output == "TRALI"){
    data=read.csv("input/Transfusion-related acute lung injury - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Transfusion_lung_injury.csv"
    
  } else if (output == "TACO"){
    data=read.csv("input/Transfusion-associated circulatory overload - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Transfusion_circulatory_overload.csv"
    
  } else if (output == "Clinically important bleeding"){
    data=read.csv("input/Clinically important bleeding - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "clinically_important_bleeding.csv"
    
  } else if (output == "Allergic reactions"){
    data=read.csv("input/Allergic reactions - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Allergic_reactions.csv"
    
  } else if (output == "Graft vs. host disease"){
    data=read.csv("input/Graft vs. host disease - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    name <- "Graft.csv"
    
  } else if (output == "Duration of hospitalization"){
    data=read.csv("input/Duration of hospitalization - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "Duration_of_hospitalization.csv"
    
  } else if (output == "ICU length of stay"){
    data=read.csv("input/ICU length of stay - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "ICU_stay.csv"
    
  } else if (output == "Ventilator-free days"){
    data=read.csv("input/Ventilator-free days - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "Ventilator_free_days.csv"
    
  } else if (output == "Duration of ventilation"){
    data=read.csv("input/Duration of ventilation - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "Duration_of_ventilation.csv"
    
  } else if (output == "Time to symptom resolution"){
    data=read.csv("input/Time to symptom resolution - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "Time_to_symptom_resolution.csv"
    
  } else if (output == "Time to to viral clearance"){
    data=read.csv("input/Time to to viral clearance - wide data format.csv")
    data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    baseline=data %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    name <- "Time_to_viral_clearance.csv"
    
  }
  if (baseline ==0){
    baseline = 0.001
  }
  list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir, digits,folderROM)
  
  write.estimates.csv(list.estimates,mainDir, name)
}