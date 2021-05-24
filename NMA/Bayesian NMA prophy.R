# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")

mainDir <- paste0(getwd(),"/NMA/prophylaxis")
subDir <- "output"

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

#create output directory

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}
pdf(paste0(output_dir,"/profilaxis_aes_network.pdf"))
plot(model)
dev.off()

results <- model.processing(model)

# prob.ref=0.03 checar para poner en extra.txt. Quitar de model$code el ultimo corchete
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

samples=coda.samples(model.jags,variable.names = monitors,n.iter = 50000)

absolute=spread_draws(samples,RD[i,j]) %>% group_by(i,j) %>%
  summarise(mean=mean(RD),lower=quantile(RD,.025),upper=quantile(RD,.975)) %>%
  mutate(i = as.character(treatments.names[i,1]), j =as.character(treatments.names[j,1])) %>% 
  ungroup()


spread_draws(samples,cr[i]) %>% group_by(i) %>%
  summarise(mean=mean(cr),lower=quantile(cr,.025),upper=quantile(cr,.975)) %>%
  mutate(i = as.character(treatments.names[i,1]))


list.estimates=list()

for (i in 1:nrow(relative.effect.table(results))) {
  temp=round(exp(relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
  temp$t2=temp[i,1]
  temp=temp[-c(1:i),]
  list.estimates[[i]] <- temp
}

bind_rows(list.estimates) %>% select(t1,t2,`50%`,`2.5%`,`50%`,`97.5%`) %>% rename(ci.l.net=`2.5%`,pe.net=`50%`,ci.u.net=`97.5%`) %>%
  filter(!is.na(pe.net)) %>% inner_join(absolute,by=c("t1"="i","t2"="j"))


pairwise=read.csv("pairwise/prophylaxis/output/AE_prophylaxis.csv")


# result.node <- mtc.nodesplit(network, likelihood="binom",link="logit",
#                              linearModel="fixed", n.chain =3,
#                              n.adapt=10000, n.iter=50000, thin=10,
#                              hy.prior=mtc.hy.prior("var", "dlnorm", -1.87, 0.4328))

# exp1 <- function(x) (round(exp(x),2))
# 
# summary(result.node)[[2]] %>% mutate_at(c("pe","ci.l","ci.u"),exp1) %>% select(t2,t1,pe,ci.l,ci.u) %>% 
#   rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u) %>% 
#   right_join(NMA,by=c("t1","t2")) %>% as.data.frame() %>% write.csv(file="pairwise/drugs/output/GRADE_mort_fixed_IL6.csv")
