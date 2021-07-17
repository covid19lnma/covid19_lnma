library(tidyverse)
library(metafor)
library(bayesmeta)
library(readxl)
library(forestplot)

getestimates <- function(data, TP, TP1, baseline, measure, name.pdf,folder){
  
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
  list.estimates = list()
  
  pathname <- paste0(folder,"/output/", gsub(".{4}$", "", name.pdf),".pdf")
  
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
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5, 2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Odds ratio", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=2)
      
    } else if (measure == "ROM") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5, 2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Ratio of means", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=2)
      
    } else if (measure == "MD") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-5, 5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Mean difference", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     digits=2)
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
      if (p1 == "standard care/placebo" | p2 == "standard care/placebo") {
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
      
      hnorm10.to.append <- bm.hnorm10
      hnorm05.to.append <- bm.hnorm05
      
      #append in the list and be done with it
      
      list.bm.hnorm10[[i]] <- hnorm10.to.append
      list.bm.hnorm05[[i]] <- hnorm05.to.append
      
      rma.fixed <- rma.uni(effsize, method="FE")
      rma.random.DL <- rma.uni(effsize, method="DL")
      
      #concat of the summarys, called estimates, same scheize, calculate and append
      estimates <- rbind("HNorm(0.5)" = c(bm.hnorm05$summary[2,1:2], bm.hnorm05$summary[5:6,2]),
                         "HNorm(1.0)" = c(bm.hnorm10$summary[2,1:2], bm.hnorm10$summary[5:6,2]),
                         "Turner Prior"=c(bm.Turner$summary[2,1:2], bm.Turner$summary[5:6,2]),
                         "Frequentist.fixed" = c(sqrt(rma.fixed$tau2.fix), rma.fixed$b, rma.fixed$ci.lb, rma.fixed$ci.ub),
                         "Frequentist.random.DL" = c(sqrt(rma.random.DL$tau2), rma.random.DL$b, rma.random.DL$ci.lb, rma.random.DL$ci.ub))
      
      if (measure == "OR") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        if (p1 == "standard care/placebo" | p2 == "standard care/placebo" | p1 == "placebo/standard care" | p2 == "placebo/standard care"){
          
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
      } else if (measure == "ROM") {
        
        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        if (p1 == "standard care/placebo" | p2 == "standard care/placebo" | p1 == "placebo/standard care" | p2 == "placebo/standard care"){
          
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
          addpoly(estimates[j,"mu"], ci.lb=estimates[j,3], ci.ub=estimates[j,4], 
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}
        
        estimates = estimates %>% as_tibble(rownames="type") %>% mutate(t1=p1,t2=p2) %>% 
          rename(mu_l=`95% lower`,mu_u=`95% upper`)
        
      }
      estimates.to.append <- estimates
      
      list.estimates[[i]] <- estimates.to.append
      #print(list.estimates)
    }
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
  rows.estimates %>% filter(type=="Turner Prior") %>% 
    write_csv(paste0(folder,"/output/", name))
}
