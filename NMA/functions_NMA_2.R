library(readr)

######
getestimatesnma <- function(data,
                            pairwise_data,
                            # study = "study",
                            # treatment = "treatment",
                            # responders = "responders",
                            # sampleSize = "sampleSize",
                            likelihood, 
                            link, 
                            #linearModel, 
                            hy.prior1, 
                            hy.prior2,
                            output_dir,
                            file_name,
                            prob.ref.value){
  
  #data %<>% rename(study=stauthor,responders=responder)
  data %<>% mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))
  pairwise_data %<>% mutate(t2=if_else(t2=="placebo/standard care","a",t2))
  
  dictionary=data %>% select(study,treatment,responders,sampleSize) %>%
    filter(sampleSize!=0) %>% as.data.frame() 
  
  dictionary$treatments.simp <- dictionary$treatment
  dictionary = dictionary %>% select(treatment, treatments.simp) %>%
    mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% distinct()
  
  data=data %>% select(study,treatment,responders,sampleSize) %>%
    mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 
  
  network=mtc.network(data)
  
  model=mtc.model(network,type = "consistency",
                  likelihood=.data[[likelihood]],link=.data[[link]],
                  linearModel="random", n.chain =3,
                  powerAdjust=NA, dic=TRUE,
                  hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
  
  
  pdf(paste0(output_dir,"/", file_name, ".pdf"))
  plot(model)
  dev.off()
  
  results <- model.processing(model)
  
  if (nrow(mtc.nodesplit.comparisons(network)) > 0){
    
    result.node <- mtc.nodesplit(network, likelihood=.data[[likelihood]],link=.data[[link]],
                                 linearModel="random", n.chain =4,
                                 n.adapt=10000, n.iter=50000,
                                 hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
    
    indirect=summary(result.node)[[2]]  %>% select(t2,t1,pe,ci.l,ci.u) %>% 
      rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u)
    
    pdf(paste0(output_dir,"/ns_", file_name, ".pdf"),width = 8,)
    plot(summary(result.node))
    dev.off()
  }
  
  code <- model$code
  code <- substr(code,1,nchar(code)-2)
  
  prob.ref <- sprintf("prob.ref <- %s\n", prob.ref.value)
  
  extra = readLines("NMA/extra.txt")
  cat(code,prob.ref,extra,file="NMA/code.txt")
  
  model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
                        n.adapt = 10000,n.chains = model$n.chain)  
  
  monitors=c("cr","RD")
  
  treatments.names <- model$network$treatments #nombres para tabla
  treatments.names %>% select(description) %>% filter(row_number()==1) %>% 
    write.csv(paste0(output_dir,"/","reference/", file_name, ".csv"),row.names = F)
  
  
  treatments.names = left_join(treatments.names, dictionary, by=c("id"="treatment")) %>%
    select(id, treatments.simp)
  
  samples=coda.samples(model.jags,variable.names = monitors,n.iter = 50000)
  
  absolute.RD=spread_draws(samples,RD[i,j]) %>% group_by(i,j) %>%
    summarise(mean=mean(RD),lower=quantile(RD,.025),upper=quantile(RD,.975)) %>%
    mutate(i = as.character(treatments.names[i,2]), j =as.character(treatments.names[j,2])) %>% 
    ungroup()
  
  
  absolut.cr=spread_draws(samples,cr[i]) %>% group_by(i) %>%
    summarise(mean=mean(cr),lower=quantile(cr,.025),upper=quantile(cr,.975)) %>%
    mutate(i = as.character(treatments.names[i,2]))
  
  #absolut.cr %>% write.csv("absolute_prophy.csv")
  
  list.estimates=list()
  
  for (i in 1:nrow(relative.effect.table(results))) {
    temp=round((relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
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
  
  
  pairwise=as_tibble(pairwise_data) %>% 
    rename(relative_direct = OR,
           relative_direct_lower = OR_l,
           relative_direct_upper = OR_u,
           absolute_direct = risk,
           absolute_direct_lower = risk_l,
           absolute_direct_upper = risk_u) %>%
    select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper, 
           absolute_direct, absolute_direct_lower, absolute_direct_upper)
  
  outaux = inner_join(absolute.RD,pairwise,by=c("t1"="t1","t2"="t2"))
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
    }
  }
  outbase = outbase %>% mutate(t1=if_else(t1=="a","placebo/standard care",t1),t2=if_else(t2=="a","placebo/standard care",t2))
  
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
             !is.na(relative_indirect) & !is.na(relative_direct) & t2=="placebo/standard care"  ~ 
               -1*1000*(prob.ref.value-((relative_indirect*prob.ref.value)/(1-prob.ref.value+relative_indirect*prob.ref.value)))
             
           ),
           absolute_indirect_lower = case_when(
             is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ absolute_nma_lower,
             !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) & t2=="placebo/standard care"  ~ 
               -1*1000*(prob.ref.value-((relative_indirect_lower*prob.ref.value)/(1-prob.ref.value+relative_indirect_lower*prob.ref.value)))
             
           ),
           absolute_indirect_upper = case_when(
             is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ absolute_nma_upper,
             !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) & t2=="placebo/standard care"  ~ 
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
    
    
  #   
  #   absolute_indirect= case_when(
  #     !is.na(relative_indirect) & t2!="placebo/standard care" & semaf==T ~ NA_real_,
  #     !is.na(relative_indirect) & t2=="placebo/standard care" & semaf==T ~ 
  # -1*1000*(prob.ref.value-((relative_indirect*prob.ref.value)/(1-prob.ref.value+relative_indirect*prob.ref.value))),
  #     TRUE                      ~ absolute_nma
  #   ),
  # absolute_indirect_lower = case_when(
  #   !is.na(relative_indirect_lower) & t2!="placebo/standard care" & semaf==T ~ NA_real_,
  #   !is.na(relative_indirect_lower) &t2=="placebo/standard care" & semaf==T ~ 
  # -1*1000*(prob.ref.value-((relative_indirect_lower*prob.ref.value)/(1-prob.ref.value+relative_indirect_lower*prob.ref.value))),
  #   TRUE                      ~ absolute_nma_lower
  # ),
  # absolute_indirect_upper = case_when(
  #   !is.na(relative_indirect_upper) & t2!="placebo/standard care" & semaf==T ~ NA_real_,
  #   !is.na(relative_indirect_upper) & t2=="placebo/standard care" & semaf==T ~ 
  #-1*1000*(prob.ref.value-((relative_indirect_upper*prob.ref.value)/(1-prob.ref.value+relative_indirect_upper*prob.ref.value))),
  #   TRUE                      ~ absolute_nma_upper
  # ) 
  # 
  # ) %>% mutate(relative_indirect=if_else(is.na(relative_indirect),relative_nma,relative_indirect),
  #              relative_indirect_lower=if_else(is.na(relative_indirect_lower),relative_nma_lower,relative_indirect_lower),
  #              relative_indirect_upper=if_else(is.na(relative_indirect_upper),relative_nma_upper,relative_indirect_upper)) %>% 
    
    
    
    
    select(t1,t2,relative_nma,relative_nma_lower,relative_nma_upper,absolute_nma,absolute_nma_lower,
           absolute_nma_upper,relative_indirect,relative_indirect_lower,relative_indirect_upper,
           absolute_indirect,absolute_indirect_lower,absolute_indirect_upper,
           relative_direct,relative_direct_lower,relative_direct_upper,
           absolute_direct,absolute_direct_lower,absolute_direct_upper)
  outbase %>% arrange(match(t2, "placebo/standard care")) %>% 
    write.csv(paste0(output_dir,"/", file_name, ".csv"))
  
}


######
getestimatesnmacontinuous <- function(data,
                            pairwise_data,
                            measure,
                            likelihood, 
                            link, 
                            #linearModel, 
                            hy.prior1, 
                            hy.prior2,
                            output_dir,
                            file_name,
                            prob.ref.value){
  
  data %<>% mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))
  pairwise_data %<>% mutate(t2=if_else(t2=="placebo/standard care","a",t2))
  
  if(measure == "MD"){
    dictionary=data %>% select(study,treatment,mean,std.dev,sampleSize) %>%
      filter(sampleSize!=0) %>% as.data.frame() 
  } else if (measure == "ROM"){
    dictionary=data %>% select(study,treatment,diff,std.err) %>% 
      as.data.frame()  
  }
  
  dictionary$treatments.simp <- dictionary$treatment
  dictionary = dictionary %>% select(treatment, treatments.simp) %>%
    mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% distinct()
  
  if(measure == "MD"){
    data=data %>% select(study,treatment,mean,std.dev,sampleSize) %>%
      mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 
  } else if (measure == "ROM"){
    data=data %>% select(study,treatment,diff,std.err) %>%
      mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% as.data.frame() 
  }
  
  if(measure == "ROM"){
    network=mtc.network(data.re = data)
  } else if(measure == "MD"){
    network=mtc.network(data)
  }
  
  model=mtc.model(network,type = "consistency",
                  likelihood=.data[[likelihood]],link=.data[[link]],
                  linearModel="random", n.chain =3,
                  powerAdjust=NA, dic=TRUE,
                  hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
  
  
  pdf(paste0(output_dir,"/", file_name, ".pdf"))
  plot(model)
  dev.off()
  
  results <- model.processing(model)
  
  if (nrow(mtc.nodesplit.comparisons(network)) > 0){
    
    result.node <- mtc.nodesplit(network, likelihood=.data[[likelihood]],link=.data[[link]],
                                 linearModel="random", n.chain =4,
                                 n.adapt=10000, n.iter=50000,
                                 hy.prior=mtc.hy.prior("var", "dlnorm", hy.prior1, hy.prior2))
    
    indirect=summary(result.node)[[2]]  %>% select(t2,t1,pe,ci.l,ci.u) %>% 
      rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u)
    
    pdf(paste0(output_dir,"/ns_", file_name, ".pdf"),width = 8,)
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
  }
  
  treatments.names <- model$network$treatments #nombres para tabla
  treatments.names %>% select(description) %>% filter(row_number()==1) %>% 
    write.csv(paste0(output_dir,"/","reference/", file_name, ".csv"),row.names = F)
  treatments.names = left_join(treatments.names, dictionary, by=c("id"="treatment")) %>%
    select(id, treatments.simp)
  
  if(measure == "ROM"){
    
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
    temp=round((relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
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
  
  if(measure == "ROM"){
    
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

  } else if(measure=="MD"){
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
    
  }
  
    
    outaux = inner_join(absolute.RD,pairwise,by=c("t1"="t1","t2"="t2"))
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
      }
    }
    
    outbase = outbase %>% mutate(t1=if_else(t1=="a","placebo/standard care",t1),t2=if_else(t2=="a","placebo/standard care",t2))

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
               !is.na(relative_indirect) & !is.na(relative_direct) & t2=="placebo/standard care"  ~ 
                 relative_indirect*prob.ref.value-prob.ref.value
               
             ),
             absolute_indirect_lower = case_when(
               is.na(relative_indirect_lower) & is.na(relative_direct_lower) ~ absolute_nma_lower,
               !is.na(relative_indirect_lower) & !is.na(relative_direct_lower) & t2=="placebo/standard care"  ~ 
                 relative_indirect_lower*prob.ref.value-prob.ref.value
               
             ),
             absolute_indirect_upper = case_when(
               is.na(relative_indirect_upper) & is.na(relative_direct_upper) ~ absolute_nma_upper,
               !is.na(relative_indirect_upper) & !is.na(relative_direct_upper) & t2=="placebo/standard care"  ~ 
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
    outbase %>% arrange(match(t2, "placebo/standard care")) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
    
  } else if(measure == "MD"){
    
    outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
                                 relative_nma_upper = logrelative_nma_upper,relative_indirect = logrelative_indirect, 
                                 relative_indirect_lower = logrelative_indirect_lower, 
                                 relative_indirect_upper = logrelative_indirect_upper) %>%
      mutate(relative_nma = relative_nma,
             relative_nma_lower = (relative_nma_lower),
             relative_nma_upper = (relative_nma_upper), 
             relative_indirect = (relative_indirect),
             relative_indirect_lower = (relative_indirect_lower),
             relative_indirect_upper = (relative_indirect_upper)) %>% 
      select(t1,t2,
             relative_nma,relative_nma_lower,relative_nma_upper,
             #absolute_nma,absolute_nma_lower,absolute_nma_upper,
             relative_indirect,relative_indirect_lower,relative_indirect_upper,
             relative_direct,relative_direct_lower,relative_direct_upper)#,
             #absolute_direct,absolute_direct_lower,absolute_direct_upper)
    outbase %>%  arrange(match(t2, "placebo/standard care")) %>% 
      write.csv(paste0(output_dir,"/", file_name, ".csv"))
    
  }
  
}