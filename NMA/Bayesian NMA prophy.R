source("NMA/functions_NMA.R")

data=read.csv("NMA/prophylaxis/AEs - long data format.csv")

#filter(stauthor!="REMAP-CAP_2_NCT02735707") 

data=data %>% select(study,treatment,responders,sampleSize) %>%
  mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 

network=mtc.network(data)

model=mtc.model(network,type = "consistency",
                likelihood="binom",link="logit",
                linearModel="fixed", n.chain =3,
                powerAdjust=NA, dic=TRUE,
                hy.prior=mtc.hy.prior("var", "dlnorm",-1.87, 0.4328))

plot(model)
results <- model.processing(model)

extra = readLines("NMA/extra.txt")
cat(model$code,extra,file = "NMA/code.txt")

model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
                      n.adapt = 10000,n.chains = model$n.chain)  

monitors=c("cr","RD")

samples=coda.samples(model.jags,variable.names = monitors,n.iter = 50000)

summary(samples)

model$network$treatments

list.estimates=list()

for (i in 1:nrow(relative.effect.table(results))) {
  temp=round(exp(relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
  temp$t2=temp[i,1]
  temp=temp[-c(1:i),]
  list.estimates[[i]] <- temp
}

NMA=bind_rows(list.estimates) %>% select(t1,t2,`50%`,`2.5%`,`50%`,`97.5%`) %>% rename(ci.l.net=`2.5%`,pe.net=`50%`,ci.u.net=`97.5%`) %>%
  filter(!is.na(pe.net))

# result.node <- mtc.nodesplit(network, likelihood="binom",link="logit",
#                              linearModel="fixed", n.chain =3,
#                              n.adapt=10000, n.iter=50000, thin=10,
#                              hy.prior=mtc.hy.prior("var", "dlnorm", -1.87, 0.4328))

# exp1 <- function(x) (round(exp(x),2))
# 
# summary(result.node)[[2]] %>% mutate_at(c("pe","ci.l","ci.u"),exp1) %>% select(t2,t1,pe,ci.l,ci.u) %>% 
#   rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u) %>% 
#   right_join(NMA,by=c("t1","t2")) %>% as.data.frame() %>% write.csv(file="pairwise/drugs/output/GRADE_mort_fixed_IL6.csv")
