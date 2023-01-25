library(gemtc)
library(readxl)
library(tidybayes)
library(ggdist)
library(stringr)
library(ggplot2)
library(dplyr)
library(viridis)
library(rjags)
library(magrittr)
library(readr)
library(tidyverse)
library(metafor)
library(bayesmeta)
library(readxl)
library(forestplot)
library(magrittr)
library(netmeta)

#########1#########
model.processing <- function(model){
  #plot(model)
  
  results=mtc.run(model, n.adapt=10000, n.iter=50000) # corre simulaciones
  
  # out_all=gather_draws(results$samples,`d.*`,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% group_by(t1) %>%
  #   mean_qi(.value)
  # 
  # gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(fill=as.factor(.chain),group=.chain,x=.value))+
  #   geom_density(alpha=0.7)+
  #   facet_wrap(~.variable,scales ="free")+
  #   scale_fill_viridis(discrete=T)
  # 
  # gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(x=.iteration,y=.value,color=as.factor(.chain)))+
  #   geom_line(alpha=0.7)+
  #   facet_wrap(~.variable,scale="free_y")+labs(color="chain")+
  #   scale_color_viridis(discrete=T)
  # 
  # gather_draws(results$samples,`d.*`,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(.value, t1)) +
  #   geom_vline(xintercept = 0, size = .25, lty = 2) +
  #   stat_pointinterval(.width = c(.8, .95),point_colour="#FDE725FF",interval_colour="#481567FF",
  #                      alpha=1) + geom_text(
  #                        data = mutate_if(out_all, is.numeric, round, 2),
  #                        aes(label = str_glue("{.value} [{.lower}, {.upper}]"), x = 8.5),
  #                        hjust = "inward"
  #                      )
  results
}

########2######

getestimatesnma <- function(data,
                            pairwise_data,
                            measure,
                            likelihood, 
                            link,
                            linearModel,
                            hy.prior1, 
                            hy.prior2,
                            output_dir,
                            file_name,
                            prob.ref.value,
                            placebo){
  
  if("interventionname" %in% colnames(data)){
    data %<>% 
      rename(as_is_treatment = treatment) %<>%
      rename(treatment = interventionname) %<>% 
      mutate(treatment=if_else(treatment==placebo,"a",treatment))
  } else {
    data %<>% 
      mutate(treatment=if_else(treatment==placebo,"a",treatment))
  }
  
  pairwise_data %<>% mutate(t2=if_else(t2==placebo,"a",t2))
  
  if(measure == "MD"){
    dictionary=data %>% select(study,treatment,mean,std.dev,sampleSize) %>%
      filter(sampleSize!=0) %>% as.data.frame() 
  } else if (measure == "ROM" | measure == "RD"){
    dictionary=data %>% select(study,treatment,diff,std.err) %>% 
      as.data.frame()  
  } else if (measure == "OR"){
    dictionary=data %>% select(study,treatment,responders,sampleSize) %>%
      filter(sampleSize!=0) %>% as.data.frame()
  }
  
  dictionary$treatments.simp <- dictionary$treatment
  dictionary = dictionary %>% select(treatment, treatments.simp) %>%
    mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% distinct()
  
  if(measure == "MD"){
    data=data %>% select(study,treatment,mean,std.dev,sampleSize) %>%
      mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 
  } else if (measure == "ROM" | measure == "RD"){
    data=data %>% select(study,treatment,diff,std.err) %>%
      mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% as.data.frame() 
  } else if (measure == "OR"){
    data=data %>% select(study,treatment,responders,sampleSize) %>%
      mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 
  }
  
  if(measure == "ROM" | measure == "RD"){
    network=mtc.network(data.re = data)
  } else {
    network=mtc.network(data)
  }
  
  # pdf(paste0(output_dir,"/", file_name, ".pdf"))
  # plot(network)
  # dev.off()
  
  model=mtc.model(network,type = "consistency", 
                  likelihood=likelihood,link=link,
                  linearModel=linearModel, n.chain =3,
                  powerAdjust=NA, dic=TRUE,
                  hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
  
  
  # pdf(paste0(output_dir,"/", file_name, ".pdf"))
  # plot(model)
  # dev.off()
  
  results <- model.processing(model)
  
  if (nrow(mtc.nodesplit.comparisons(network)) > 0){
    
    result.node <- mtc.nodesplit(network, likelihood=likelihood,link=link,
                                 linearModel=linearModel, n.chain =4,
                                 n.adapt=10000, n.iter=50000,
                                 hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
    
    indirect=summary(result.node)[[2]]  %>% select(t2,t1,pe,ci.l,ci.u) %>% 
      rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u)
    
    pdf(paste0(output_dir,"/ns_", file_name, ".pdf"),width = 15)
    plot(summary(result.node))
    dev.off()
  }
  
  if (measure == "ROM"){
    
    code <- model$code
    code <- substr(code,1,nchar(code)-2)
    
    prob.ref <- sprintf("prob.ref <- %s\n", prob.ref.value)
    
    extra = readLines("NMA/extra_ROM.txt")
    cat(code,prob.ref,extra,file="NMA/code.txt")
    
    model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
                          n.adapt = 10000,n.chains = model$n.chain)  
    
    monitors=c("cr","RD")
  } else if(measure == "OR"){
    code <- model$code
    code <- substr(code,1,nchar(code)-2)
    
    prob.ref <- sprintf("prob.ref <- %s\n", prob.ref.value)
    
    extra = readLines("NMA/extra.txt")
    cat(code,prob.ref,extra,file="NMA/code.txt")
    
    model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
                          n.adapt = 10000,n.chains = model$n.chain)  
    
    monitors=c("cr","RD")
  }
  
  treatments.names <- model$network$treatments #nombres para tabla
  
  subDir_ref <- "reference"
  
  reference_dir <- file.path(output_dir, subDir_ref)
  
  if (!dir.exists(reference_dir)){
    dir.create(reference_dir)
  }
  
  treatments.names %>% select(description) %>% filter(row_number()==1) %>% 
    write.csv(paste0(output_dir,"/","reference/", file_name, ".csv"),row.names = F)
  
  treatments.names = left_join(treatments.names, dictionary, by=c("id"="treatment")) %>%
    select(id, treatments.simp)
  
  if(measure == "ROM" | measure == "OR"){
    
    samples=coda.samples(model.jags,variable.names = monitors,n.iter = 50000)
    
    absolute.RD=spread_draws(samples,RD[i,j]) %>% group_by(i,j) %>%
      summarise(mean=mean(RD),lower=quantile(RD,.025),upper=quantile(RD,.975)) %>%
      mutate(i = as.character(treatments.names[i,2]), j =as.character(treatments.names[j,2])) %>% 
      ungroup()
    
    
    absolut.cr=spread_draws(samples,cr[i]) %>% group_by(i) %>%
      summarise(mean=mean(cr),lower=quantile(cr,.025),upper=quantile(cr,.975)) %>%
      mutate(i = as.character(treatments.names[i,2]))
    
  }
  
  
  list.estimates=list()
  
  for (i in 1:nrow(relative.effect.table(results))) {
    temp=round((relative.effect.table(results)),5)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
    temp$t2=temp[i,1]
    temp=temp[-c(1:i),]
    list.estimates[[i]] <- temp
  }
  
  aux = bind_rows(list.estimates) %>% select(t1,t2,`50%`,`2.5%`,`50%`,`97.5%`) %>% 
    rename(ci.l.net=`2.5%`,pe.net=`50%`,ci.u.net=`97.5%`) %>%
    filter(!is.na(pe.net))
  
  if (nrow(mtc.nodesplit.comparisons(network)) > 0){
    
    aux=aux %>% left_join(indirect)
    semaf=T
    
  } else {
    
    aux= aux %>% mutate(pe.ind= NA_real_, ci.l.ind= NA_real_, ci.u.ind= NA_real_)
    semaf=F
    
  }
  
  aux = left_join(aux,treatments.names,by=c("t1"="id")) %>% select(treatments.simp,t2,pe.net,ci.l.net,ci.u.net,pe.ind,ci.l.ind,ci.u.ind)
  aux = left_join(aux,treatments.names,by=c("t2"="id")) %>% select(treatments.simp.x,treatments.simp.y,pe.net,ci.l.net,ci.u.net,pe.ind,ci.l.ind,ci.u.ind)
  
  if(measure == "ROM" | measure == "OR"){
    
    absolute.RD = inner_join(aux,absolute.RD,by=c("treatments.simp.x"="i","treatments.simp.y"="j")) %>% 
      rename(t1 = treatments.simp.x, 
             t2 = treatments.simp.y, 
             logrelative_nma = pe.net, 
             logrelative_nma_lower = ci.l.net,
             logrelative_nma_upper = ci.u.net,
             logrelative_indirect = pe.ind, 
             logrelative_indirect_lower = ci.l.ind,
             logrelative_indirect_upper = ci.u.ind,
             absolute_nma = mean,
             absolute_nma_lower = lower,
             absolute_nma_upper = upper)
    
    absolute.RD.inv = absolute.RD %>% rename("t1"="t2", "t2"="t1") %>%
      mutate(logrelative_nma = (-1)*logrelative_nma,
             logrelative_nma_lower_aux = (-1)*logrelative_nma_upper, 
             logrelative_nma_upper = (-1)*logrelative_nma_lower, 
             logrelative_indirect = (-1)*logrelative_indirect,
             logrelative_indirect_lower_aux = (-1)*logrelative_indirect_upper, 
             logrelative_indirect_upper = (-1)*logrelative_indirect_lower, 
             absolute_nma = (-1)*absolute_nma, 
             absolute_nma_lower_aux = (-1)*absolute_nma_upper, 
             absolute_nma_upper = (-1)*absolute_nma_lower) %>%
      select(t1, t2, logrelative_nma, logrelative_nma_lower = logrelative_nma_lower_aux, logrelative_nma_upper, 
             logrelative_indirect, logrelative_indirect_lower = logrelative_indirect_lower_aux, logrelative_indirect_upper,
             absolute_nma, absolute_nma_lower = absolute_nma_lower_aux, absolute_nma_upper)
    
  } else if(measure=="MD" | measure == "RD"){
    absolute.RD = aux %>% rename(t1 = treatments.simp.x, 
                                 t2 = treatments.simp.y, 
                                 logrelative_nma = pe.net, 
                                 logrelative_nma_lower = ci.l.net,
                                 logrelative_nma_upper = ci.u.net,
                                 logrelative_indirect = pe.ind, 
                                 logrelative_indirect_lower = ci.l.ind,
                                 logrelative_indirect_upper = ci.u.ind)
    
    absolute.RD.inv = absolute.RD %>% rename("t1"="t2", "t2"="t1") %>%
      mutate(logrelative_nma = (-1)*logrelative_nma,
             logrelative_nma_lower_aux = (-1)*logrelative_nma_upper, 
             logrelative_nma_upper = (-1)*logrelative_nma_lower, 
             logrelative_indirect = (-1)*logrelative_indirect,
             logrelative_indirect_lower_aux = (-1)*logrelative_indirect_upper, 
             logrelative_indirect_upper = (-1)*logrelative_indirect_lower) %>%
      select(t1, t2, logrelative_nma, logrelative_nma_lower = logrelative_nma_lower_aux, logrelative_nma_upper, 
             logrelative_indirect, logrelative_indirect_lower = logrelative_indirect_lower_aux, logrelative_indirect_upper)
    
  }
  
  if(measure == "MD"){
    
    pairwise=as_tibble(pairwise_data) %>% 
      rename(relative_direct = mu,
             relative_direct_lower = mu_l,
             relative_direct_upper = mu_u) %>% 
      # absolute_direct = risk,
      # absolute_direct_lower = risk_l,
      # absolute_direct_upper = risk_u) %>%
      select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper)
    
  } else if(measure == "ROM"){
    
    pairwise=as_tibble(pairwise_data) %>% 
      rename(relative_direct = ROM,
             relative_direct_lower = ROM_l,
             relative_direct_upper = ROM_u,
             absolute_direct = risk,
             absolute_direct_lower = risk_l,
             absolute_direct_upper = risk_u) %>%
      select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper, 
             absolute_direct, absolute_direct_lower, absolute_direct_upper)
    
  } else if(measure == "OR"){
    
    pairwise=as_tibble(pairwise_data) %>% 
      rename(relative_direct = OR,
             relative_direct_lower = OR_l,
             relative_direct_upper = OR_u,
             absolute_direct = risk,
             absolute_direct_lower = risk_l,
             absolute_direct_upper = risk_u) %>%
      select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper, 
             absolute_direct, absolute_direct_lower, absolute_direct_upper)
    
  } else if(measure == "RD"){
    
    pairwise=as_tibble(pairwise_data) %>% 
      rename(relative_direct = RD,
             relative_direct_lower = RD_l,
             relative_direct_upper = RD_u,) %>%
      select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper)
  }
  
  
  outaux = inner_join(absolute.RD,pairwise,by=c("t1"="t1","t2"="t2"))
  outauxplacebo = left_join(absolute.RD,pairwise,by=c("t1"="t1","t2"="t2"))
  outbase = left_join(absolute.RD.inv,pairwise,by=c("t1"="t1","t2"="t2"))
  
  if (nrow(outaux) !=0){
    for (n in 1:nrow(outbase)) {
      
      for (m in 1:nrow(outaux)){
        
        if ((outbase[n,1]==outaux[m,2]) & (outbase[n,2]==outaux[m,1])){
          
          for (k in 1:ncol(outbase)){
            
            outbase[n,k] = outaux[m,k]
          }
        }
      }
      
      if (outbase[n,1] == "a"){
        
        for (k in 1:ncol(outbase)){
          
          outbase[n,k] = outauxplacebo[n,k]
          
        }
      }
    }
  }
  
  outbase = outbase %>% mutate(t1=if_else(t1=="a",placebo,t1),t2=if_else(t2=="a",placebo,t2))
  
  if(measure == "ROM"){
    
    outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
                                 relative_nma_upper = logrelative_nma_upper,relative_indirect = logrelative_indirect, 
                                 relative_indirect_lower = logrelative_indirect_lower, 
                                 relative_indirect_upper = logrelative_indirect_upper) %>%
      mutate(relative_nma = exp(relative_nma),
             relative_nma_lower = exp(relative_nma_lower),
             relative_nma_upper = exp(relative_nma_upper), 
             relative_indirect = exp(relative_indirect),
             relative_indirect_lower = exp(relative_indirect_lower),
             relative_indirect_upper = exp(relative_indirect_upper),
             absolute_indirect= case_when(
               is.na(relative_indirect) & is.na(relative_direct) ~ absolute_nma,
               !is.na(relative_indirect) & !is.na(relative_direct) & t2==placebo  ~ 
                 relative_indirect*prob.ref.value-prob.ref.value
               
             ),
             absolute_indirect_lower = case_when(
               is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ absolute_nma_lower,
               !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) & t2==placebo  ~ 
                 relative_indirect_lower*prob.ref.value-prob.ref.value
               
             ),
             absolute_indirect_upper = case_when(
               is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ absolute_nma_upper,
               !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) & t2==placebo  ~ 
                 relative_indirect_upper*prob.ref.value-prob.ref.value
               
             )) %>% 
      mutate(
        relative_indirect=case_when(
          is.na(relative_indirect) & is.na(relative_direct) ~ relative_nma,
          !is.na(relative_indirect) & !is.na(relative_direct) ~ relative_indirect),
        
        relative_indirect_lower=case_when(
          is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ relative_nma_lower,
          !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) ~ relative_indirect_lower),
        
        relative_indirect_upper=case_when(
          is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ relative_nma_upper,
          !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) ~ relative_indirect_upper)
      ) %>%
      
      select(t1,t2,relative_nma,relative_nma_lower,relative_nma_upper,absolute_nma,absolute_nma_lower,
             absolute_nma_upper,relative_indirect,relative_indirect_lower,relative_indirect_upper,
             absolute_indirect,absolute_indirect_lower,absolute_indirect_upper,
             relative_direct,relative_direct_lower,relative_direct_upper,
             absolute_direct,absolute_direct_lower,absolute_direct_upper)
    outbase %>% arrange(match(t2, placebo)) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
    
  } else if(measure == "MD"){
    
    outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
                                 relative_nma_upper = logrelative_nma_upper,relative_indirect = logrelative_indirect, 
                                 relative_indirect_lower = logrelative_indirect_lower, 
                                 relative_indirect_upper = logrelative_indirect_upper) %>%
      
      mutate(relative_indirect=case_when(
        is.na(relative_indirect) & is.na(relative_direct) ~ relative_nma,
        !is.na(relative_indirect) & !is.na(relative_direct) ~ relative_indirect),
        
        relative_indirect_lower=case_when(
          is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ relative_nma_lower,
          !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) ~ relative_indirect_lower),
        
        relative_indirect_upper=case_when(
          is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ relative_nma_upper,
          !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) ~ relative_indirect_upper)
      ) %>% 
      select(t1,t2,
             relative_nma,relative_nma_lower,relative_nma_upper,
             #absolute_nma,absolute_nma_lower,absolute_nma_upper,
             relative_indirect,relative_indirect_lower,relative_indirect_upper,
             relative_direct,relative_direct_lower,relative_direct_upper)#,
    #absolute_direct,absolute_direct_lower,absolute_direct_upper)
    outbase %>%  arrange(match(t2, placebo)) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
    
  } else if(measure == "RD"){
    
    outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
                                 relative_nma_upper = logrelative_nma_upper,relative_indirect = logrelative_indirect, 
                                 relative_indirect_lower = logrelative_indirect_lower, 
                                 relative_indirect_upper = logrelative_indirect_upper) %>%
      
      mutate(relative_indirect=case_when(
        is.na(relative_indirect) & is.na(relative_direct) ~ (relative_nma),
        !is.na(relative_indirect) & !is.na(relative_direct) ~ (relative_indirect)),
        
        relative_indirect_lower=case_when(
          is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ (relative_nma_lower),
          !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) ~ (relative_indirect_lower)),
        
        relative_indirect_upper=case_when(
          is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ (relative_nma_upper),
          !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) ~ (relative_indirect_upper))
      ) %>% 
      select(t1,t2,
             relative_nma,relative_nma_lower,relative_nma_upper,
             #absolute_nma,absolute_nma_lower,absolute_nma_upper,
             relative_indirect,relative_indirect_lower,relative_indirect_upper,
             relative_direct,relative_direct_lower,relative_direct_upper)#,
    #absolute_direct,absolute_direct_lower,absolute_direct_upper)
    outbase %>%  arrange(match(t2, placebo)) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
    
  } else if(measure == "OR"){
    
    outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
                                 relative_nma_upper = logrelative_nma_upper,relative_indirect = logrelative_indirect, 
                                 relative_indirect_lower = logrelative_indirect_lower, 
                                 relative_indirect_upper = logrelative_indirect_upper) %>%
      mutate(relative_nma = exp(relative_nma),
             relative_nma_lower = exp(relative_nma_lower),
             relative_nma_upper = exp(relative_nma_upper), 
             relative_indirect = exp(relative_indirect),
             relative_indirect_lower = exp(relative_indirect_lower),
             relative_indirect_upper = exp(relative_indirect_upper),
             absolute_indirect= case_when(
               is.na(relative_indirect) & is.na(relative_direct) ~ absolute_nma,
               !is.na(relative_indirect) & !is.na(relative_direct) & t2==placebo  ~ 
                 -1*1000*(prob.ref.value-((relative_indirect*prob.ref.value)/(1-prob.ref.value+relative_indirect*prob.ref.value)))
               
             ),
             absolute_indirect_lower = case_when(
               is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ absolute_nma_lower,
               !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) & t2==placebo  ~ 
                 -1*1000*(prob.ref.value-((relative_indirect_lower*prob.ref.value)/(1-prob.ref.value+relative_indirect_lower*prob.ref.value)))
               
             ),
             absolute_indirect_upper = case_when(
               is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ absolute_nma_upper,
               !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) & t2==placebo  ~ 
                 -1*1000*(prob.ref.value-((relative_indirect_upper*prob.ref.value)/(1-prob.ref.value+relative_indirect_upper*prob.ref.value)))
               
             )) %>% 
      mutate(relative_indirect=case_when(
        is.na(relative_indirect) & is.na(relative_direct) ~ relative_nma,
        !is.na(relative_indirect) & !is.na(relative_direct) ~ relative_indirect),
        
        relative_indirect_lower=case_when(
          is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ relative_nma_lower,
          !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) ~ relative_indirect_lower),
        
        relative_indirect_upper=case_when(
          is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ relative_nma_upper,
          !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) ~ relative_indirect_upper)
      ) %>%
      
      select(t1,t2,relative_nma,relative_nma_lower,relative_nma_upper,absolute_nma,absolute_nma_lower,
             absolute_nma_upper,relative_indirect,relative_indirect_lower,relative_indirect_upper,
             absolute_indirect,absolute_indirect_lower,absolute_indirect_upper,
             relative_direct,relative_direct_lower,relative_direct_upper,
             absolute_direct,absolute_direct_lower,absolute_direct_upper)
    outbase %>% arrange(match(t2, placebo)) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
  }
  
}

#########3#########

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
  } else {
    #pairwise %<>% filter((e.events != 0 & c.events !=0))
    contrast_df=pairwise(list(t1,t2), 
                         event = list(e.events,c.events), 
                         n = list(e.total,c.total), 
                         studlab = study, 
                         data = pairwise, 
                         sm = measure, 
                         allstudies = T) 
  } 
  
  network=netmeta(contrast_df,
                  reference.group = placebo,
                  details.chkmultiarm = T,
                  comb.fixed = F,
                  tol.multiarm = 0.5,
                  tol.multiarm.se = 0.5)
  netgraph(network,multiarm = T)
  dev.off()
}
#########4######
nma_output <- function(output, 
                      measure,
                      likelihood,
                      link,
                      linearModel,
                      placebo,
                      drugs_or_blood,
                      mainDir){
  
  subDir <- "output"
  output_dir <- file.path(mainDir, subDir)
  
  if (output == "Mortality"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/mortality.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Mortality - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/mortality.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Mortality - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality.csv", stringsAsFactors = F))
    } else {
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/prohylaxis/mortality.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Mortality - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/prohylaxis/output/mortality.csv", stringsAsFactors = F))
    }
    
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "Mortality"
    
    data.baseline=read.csv("input/Mortality - wide data format.csv", stringsAsFactors = F)
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Infection with COVID-19 (laboratory confirmed)"){
    data=read.csv("input/Infection with COVID-19 (laboratory confirmed) - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection_COVID-19_(laboratory_confirmed).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Infection_COVID-19_(laboratory_confirmed).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "prohylaxis"){
      pairwise_data=as_tibble(read.csv("pairwise/prohylaxis/output/Infection_COVID-19_(laboratory_confirmed).csv", stringsAsFactors = F))
    }
    
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "laboratory confirmed"
    
    data.baseline=read.csv("input/Infection with COVID-19 (laboratory confirmed) - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Infection with COVID-19 (confirmed and suspected)"){
    data=read.csv("input/Infection with COVID-19 (confirmed and suspected) - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection_COVID-19_(laboratory_confirmed_and_suspected).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Infection_COVID-19_(laboratory_confirmed_and_suspected).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "prohylaxis"){
      pairwise_data=as_tibble(read.csv("pairwise/prohylaxis/output/Infection_COVID-19_(laboratory_confirmed_and_suspected).csv", stringsAsFactors = F))
    }
    
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "laboratory confirmed and suspected"
    
    data.baseline=read.csv("input/Infection with COVID-19 (confirmed and suspected) - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Mechanical ventilation"){
    data=read.csv("input/Mechanical ventilation - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/MV.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/MV.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "MV"
    
    data.baseline=read.csv("input/Mechanical ventilation - wide data format.csv", stringsAsFactors = F)
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Admission to hospital"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/admission_to_hospital.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Admission to hospital - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/admission_to_hospital.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Admission to hospital - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_to_hospital.csv", stringsAsFactors = F))
    } else {
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/prohylaxis/admission_to_hospital.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Admission to hospital - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/prohylaxis/output/admission_to_hospital.csv", stringsAsFactors = F))
    }
    
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "Admission to hosp"
    
    data.baseline=read.csv("input/Admission to hospital - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Adverse effects leading to discontinuation"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/AE.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Adverse effects leading to discontinuation - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/AE.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Adverse effects leading to discontinuation - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/AE.csv", stringsAsFactors = F))
    } else {
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/prohylaxis/AE.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Adverse effects leading to discontinuation - long data format.csv") %>%
          as.data.frame() %>% rename(study=stauthor,responders=responder)
      }
      pairwise_data=as_tibble(read.csv("pairwise/prohylaxis/output/AE.csv", stringsAsFactors = F))
    }
    
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    
    file_name = "Adverse effects"
    
    data.baseline=read.csv("input/Adverse effects leading to discontinuation - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Viral clearance"){
    data=read.csv("input/Viral clearance - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/viral_clearance.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/viral_clearance.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "Viral clearance"
    
    data.baseline=read.csv("input/Viral clearance - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo| t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Venous thromboembolism"){
    data=read.csv("input/Venous thromboembolism - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/VTE.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/VTE.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "VTE"
    
    data.baseline=read.csv("input/VTE - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "TRALI"){
    data=read.csv("input/Transfusion-related acute lung injury - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Transfusion_lung_injury.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Transfusion_lung_injury.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Transfusion-related acute lung injury"
    
    data.baseline=read.csv("input/Transfusion-related acute lung injury - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "TACO"){
    data=read.csv("input/Transfusion-associated circulatory overload - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Transfusion_circulatory_overload.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Transfusion_circulatory_overload.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Transfusion circulatory overload"
    
    data.baseline=read.csv("input/Transfusion-associated circulatory overload - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Clinically important bleeding"){
    data=read.csv("input/Clinically important bleeding - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/clinically_important_bleeding.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/clinically_important_bleeding.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "clinically important bleeding"
    
    data.baseline=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Allergic reactions"){
    data=read.csv("input/Allergic reactions - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Allergic_reactions.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Allergic_reactions.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Allergic reactions"
    
    data.baseline=read.csv("input/Allergic reactions - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  } else if (output == "Graft vs. host disease"){
    data=read.csv("input/Graft vs. host disease - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Graft.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Graft.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Graft vs. host disease"
    
    data.baseline=read.csv("input/Graft vs. host disease - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  } else if (output == "Duration of hospitalization"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Duration_of_hospitalization.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of hospitalization - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_hospitalization.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Duration_of_hospitalization.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of hospitalization - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Duration_of_hospitalization.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Duration of hospitalization"
    
    data.baseline=read.csv("input/Duration of hospitalization - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(study!="Dorward")
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "ICU length of stay"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/ICU_stay.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/ICU length of stay - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/ICU_stay.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/ICU_stay.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/ICU length of stay - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/ICU_stay.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "ICU length"
    
    data.baseline=read.csv("input/ICU length of stay - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Ventilator-free days"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Ventilator_free_days.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Ventilator-free days - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Ventilator_free_days.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Ventilator_free_days.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Ventilator-free days - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Ventilator_free_days.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Ventilator-free days"
    
    data.baseline=read.csv("input/Ventilator-free days - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Duration of ventilation"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Duration_of_ventilation.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of ventilation - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_ventilation.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Duration_of_ventilation.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of ventilation - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Duration_of_ventilation.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Duration of ventilation"
    
    data.baseline=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Time to symptom resolution"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Time_to_symptom_resolution.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to symptom resolution - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Time_to_symptom_resolution.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to symptom resolution - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Time to symptom"
    
    data.baseline=read.csv("input/Time to symptom resolution - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Time to viral clearance"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Time_to_viral_clearance.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to viral clearance - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_viral_clearance.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Time_to_viral_clearance.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to viral clearance - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Time_to_viral_clearance.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Time to viral clearance"
    
    data.baseline=read.csv("input/Time to viral clearance - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
  }
  if (prob.ref.value ==0){
    prob.ref.value = 0.001
  }
  
  getestimatesnma(data,
                  pairwise_data,
                  measure,
                  likelihood,
                  link,
                  linearModel,
                  hy.prior1,
                  hy.prior2,
                  output_dir,
                  file_name,
                  prob.ref.value,
                  placebo)
  
  get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
  
}