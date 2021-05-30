wd <- "/home/antonio/covid19_lnma"
setwd(wd)
source("NMA/functions_NMA.R")

mainDir <- paste0(getwd(),"/NMA/prophylaxis")
subDir <- "output"

data=read.csv("NMA/prophylaxis/AEs - long data format.csv")

#filter(stauthor!="REMAP-CAP_2_NCT02735707") 
dictionary=data %>% select(study,treatment,responders,sampleSize) %>%
  filter(sampleSize!=0) %>% as.data.frame() 

dictionary$treatments.simp <- dictionary$treatment
dictionary = dictionary %>% select(treatment, treatments.simp) %>%
  mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% distinct()

data=data %>% select(study,treatment,responders,sampleSize) %>%
  mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 

network=mtc.network(data)

model=mtc.model(network,type = "consistency",
                likelihood="binom",link="logit",
                linearModel="fixed", n.chain =3,
                powerAdjust=NA, dic=TRUE,
                hy.prior=mtc.hy.prior("var", "dlnorm",-1.87, 0.4328))

#create output directory

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}
pdf(paste0(output_dir,"/profilaxis_aes_network.pdf"))
plot(model)
dev.off()

results <- model.processing(model)

code <- model$code
code <- substr(code,1,nchar(code)-2)

prob.ref.value <- 0.03
prob.ref <- sprintf("prob.ref <- %s\n", prob.ref.value)

extra = readLines("NMA/extra.txt")
cat(code,prob.ref,extra,file = "NMA/code.txt")

model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
                      n.adapt = 10000,n.chains = model$n.chain)  

monitors=c("cr","RD")

treatments.names <- model$network$treatments #nombres para tabla
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

aux = left_join(aux,treatments.names,by=c("t1"="id")) %>% select(treatments.simp,t2,pe.net,ci.l.net,ci.u.net)
aux = left_join(aux,treatments.names,by=c("t2"="id")) %>% select(treatments.simp.x,treatments.simp.y,pe.net,ci.l.net,ci.u.net)

absolute.RD = inner_join(aux,absolute.RD,by=c("treatments.simp.x"="i","treatments.simp.y"="j")) %>% 
  rename(t1 = treatments.simp.x, 
         t2 = treatments.simp.y, 
         logrelative_nma = pe.net, 
         logrelative_nma_lower = ci.l.net,
         logrelative_nma_upper = ci.u.net,
         absolute_nma = mean,
         absolute_nma_lower = lower,
         absolute_nma_upper = upper)


absolute.RD.inv = absolute.RD %>% rename("t1"="t2", "t2"="t1") %>%
  mutate(logrelative_nma = (-1)*logrelative_nma,
         logrelative_nma_lower_aux = (-1)*logrelative_nma_upper, 
         logrelative_nma_upper = (-1)*logrelative_nma_lower, 
         absolute_nma = (-1)*absolute_nma, 
         absolute_nma_lower_aux = (-1)*absolute_nma_upper, 
         absolute_nma_upper = (-1)*absolute_nma_lower) %>%
  select(t1, t2, logrelative_nma, logrelative_nma_lower = logrelative_nma_lower_aux, logrelative_nma_upper, 
         absolute_nma, absolute_nma_lower = absolute_nma_lower_aux, absolute_nma_upper)
  

pairwise=as_tibble(read.csv("pairwise/prophylaxis/output/AE_prophylaxis.csv")) %>% 
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

for (i in 1:nrow(outbase)) {
  for (j in 1:nrow(outaux)){
    if ((outbase[i,1]==outaux[j,2]) & (outbase[i,2]==outaux[j,1])){
      for (k in 1:ncol(outbase)){
        outbase[i,k] = outaux[j,k]
      }
    }
  }
}

outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, relative_nma_upper = logrelative_nma_upper) %>%
  mutate(relative_nma = exp(relative_nma),
         relative_nma_lower = exp(relative_nma_lower),
         relative_nma_upper = exp(relative_nma_upper))

  # mutate(t1 = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), pairwise$t2, pairwise$t1),
  #        t2 = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), pairwise$t1, pairwise$t2),
  #        logrelative_nma = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$logrelative_nma, logrelative_nma),
  #        logrelative_nma_lower = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$logrelative_nma_lower, logrelative_nma_lower),
  #        logrelative_nma_upper = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$logrelative_nma_upper, logrelative_nma_upper),
  #        absolute_nma = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_nma, absolute_nma),
  #        absolute_nma_lower = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_nma_lower, absolute_nma_lower),
  #        absolute_nma_upper = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_nma_upper, absolute_nma_upper),
  #        relative_direct = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$relative_direct, relative_direct),
  #        relative_direct_lower = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$relative_direct_lower, relative_direct_lower),
  #        relative_direct_upper = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$relative_direct_upper, relative_direct_upper),
  #        absolute_direct = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_direct, absolute_direct),
  #        absolute_direct_lower = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_direct_lower, absolute_direct_lower),
  #        absolute_direct_upper = if_else((t1 == pairwise$t2) & (t2 == pairwise$t1), outaux$absolute_direct_upper, absolute_direct_upper)
  #        )
# 
# plus <- function(x) {
#   if(all(is.na(x))){
#     c(x[0],NA)
#   } else {
#     sum(x,na.rm = TRUE)}
# }
# 
# out = rbind(out1,out2) %>% group_by(t1,t2)
# out = aggregate(out[,3:14], out[,1:2], FUN = plus)

# result.node <- mtc.nodesplit(network, likelihood="binom",link="logit",
#                              linearModel="fixed", n.chain =3,
#                              n.adapt=10000, n.iter=50000, thin=10,
#                              hy.prior=mtc.hy.prior("var", "dlnorm", -1.87, 0.4328))

# exp1 <- function(x) (round(exp(x),2))
# 
# summary(result.node)[[2]] %>% mutate_at(c("pe","ci.l","ci.u"),exp1) %>% select(t2,t1,pe,ci.l,ci.u) %>% 
#   rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u) %>% 
#   right_join(NMA,by=c("t1","t2")) %>% as.data.frame() %>% write.csv(file="pairwise/drugs/output/GRADE_mort_fixed_IL6.csv")
