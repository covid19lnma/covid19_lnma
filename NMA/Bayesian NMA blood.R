wd <- "/home/antonio/covid19_lnma"
setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")

mainDir <- paste0(getwd(),"/NMA/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

############## Binary outcomes ######

################ mortality ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "A2:E72") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Mortality"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ mechanical ventilation ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "K2:O40") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mv.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Mechanical Ventilation"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ Admission to hospital ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "U2:Y18") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_hop.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Admission to Hospital"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ adverse events ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AE2:AI22") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data1=as_tibble(read.csv("pairwise/blood/output/AE_disc.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Adverse Events"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ viral cleareance ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AO2:AS21") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/viral_clearance.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Viral clearance"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ transfussion related acute lung injury ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AY2:BC10") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/acute_lung_injury.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "T. acute lung injury"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ transfussion related circulatory overload ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "BI2:BM10") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/circulatory_overload.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "T. circulatory overload"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)


################ Allergic reaction ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "BS2:BW41") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/AE_disc.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Allergic reaction"

prob.ref.value <- 0.23

getestimatesnma(data,
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
                prob.ref.value)

########### Continuous outcomes #######

########### Duration of Hospi ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "A2:F42") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/duration_hospitalization.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "duration hospitalization"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                measure,
                pairwise_data,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value)

########### ICU ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "K2:P6") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/ICU.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "ICU"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

########### Ventilation free days ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "V2:AA10") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/vent_free.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "ventilation free"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

########### TIME SIMPTOM RES ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "AG2:AL21") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/symptom_resolution.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "time for symptom resolution"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)


########### TIME viral clearence ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "AR2:AW7") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/time_viral_clearance.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "time to viral clearance"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

# #
# data %<>% rename(study=stauthor,responders=responder)
# #
# 
# dictionary=data %>% select(study,treatment,responders,sampleSize) %>%
#   filter(sampleSize!=0) %>% as.data.frame() 
# 
# dictionary$treatments.simp <- dictionary$treatment
# dictionary = dictionary %>% select(treatment, treatments.simp) %>%
#   mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% distinct()
# 
# data=data %>% select(study,treatment,responders,sampleSize) %>%
#   mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 
# 
# network=mtc.network(data)
# 
# model=mtc.model(network,type = "consistency",
#                 likelihood="binom",link="logit",
#                 linearModel="random", n.chain =3,
#                 powerAdjust=NA, dic=TRUE,
#                 hy.prior=mtc.hy.prior("var", "dlnorm",-1.87, 0.4328))
# 
# #create output directory
# 
# output_dir <- file.path(mainDir, subDir)
# 
# if (!dir.exists(output_dir)){
#   dir.create(output_dir)
# }
# pdf(paste0(output_dir,"/admission_hosp_blood_network.pdf"))
# plot(model)
# dev.off()
# 
# results <- model.processing(model)
# 
# ## indirect
# 
# mtc.nodesplit.comparisons(network)
# 
# result.node <- mtc.nodesplit(network, likelihood="binom",link="logit",
#                              linearModel="fixed", n.chain =4,
#                              n.adapt=10000, n.iter=50000,
#                              hy.prior=mtc.hy.prior("var", "dlnorm", -1.87, 0.4328))
# 
# indirect=summary(result.node)[[2]]  %>% select(t2,t1,pe,ci.l,ci.u) %>% 
#   rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u)
# 
# if (!dir.exists(output_dir)){
#   dir.create(output_dir)
# }
# pdf(paste0(output_dir,"/ns_admission.pdf"),width = 8,)
# plot(summary(result.node))
# dev.off()
# 
# ## indirect
# 
# # code <- model$code
# # code <- substr(code,1,nchar(code)-2)
# # 
# # prob.ref.value <- 0.23
# # prob.ref <- sprintf("prob.ref <- %s\n", prob.ref.value)
# # 
# # extra = readLines("NMA/extra.txt")
# # cat(code,prob.ref,extra,file="NMA/code.txt")
# # 
# # model.jags=jags.model(file ="NMA/code.txt",data = model$data,inits = model$inits,
# #                       n.adapt = 10000,n.chains = model$n.chain)
# # 
# # monitors=c("cr","RD") nose necesita para MD
# 
# treatments.names <- model$network$treatments #nombres para tabla
# treatments.names = left_join(treatments.names, dictionary, by=c("id"="treatment")) %>%
#   select(id, treatments.simp)
# 
# # samples=coda.samples(model.jags,variable.names = monitors,n.iter = 50000)
# # 
# # absolute.RD=spread_draws(samples,RD[i,j]) %>% group_by(i,j) %>%
# #   summarise(mean=mean(RD),lower=quantile(RD,.025),upper=quantile(RD,.975)) %>%
# #   mutate(i = as.character(treatments.names[i,2]), j =as.character(treatments.names[j,2])) %>% 
# #   ungroup()
# # 
# # 
# # absolut.cr=spread_draws(samples,cr[i]) %>% group_by(i) %>%
# #   summarise(mean=mean(cr),lower=quantile(cr,.025),upper=quantile(cr,.975)) %>%
# #   mutate(i = as.character(treatments.names[i,2]))
# 
# #absolut.cr %>% write.csv("absolute_prophy.csv")
# 
# list.estimates=list()
# 
# for (i in 1:nrow(relative.effect.table(results))) {
#   temp=round((relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
#   temp$t2=temp[i,1]
#   temp=temp[-c(1:i),]
#   list.estimates[[i]] <- temp
# }
# 
# aux = bind_rows(list.estimates) %>% select(t1,t2,`50%`,`2.5%`,`50%`,`97.5%`) %>% 
#   rename(ci.l.net=`2.5%`,pe.net=`50%`,ci.u.net=`97.5%`) %>%
#   filter(!is.na(pe.net))
# 
# aux=aux %>% left_join(indirect) # checar
# 
# aux = left_join(aux,treatments.names,by=c("t1"="id")) %>% select(treatments.simp,t2,pe.net,ci.l.net,ci.u.net)#,pe.ind,ci.l.ind,ci.u.ind)
# aux = left_join(aux,treatments.names,by=c("t2"="id")) %>% select(treatments.simp.x,treatments.simp.y,pe.net,ci.l.net,ci.u.net)#,pe.ind,ci.l.ind,ci.u.ind)
# 
# absolute.RD = inner_join(aux,absolute.RD,by=c("treatments.simp.x"="i","treatments.simp.y"="j")) %>% 
#   rename(t1 = treatments.simp.x, 
#          t2 = treatments.simp.y, 
#          logrelative_nma = pe.net, 
#          logrelative_nma_lower = ci.l.net,
#          logrelative_nma_upper = ci.u.net,
#          # logrelative_indirect = pe.ind, 
#          # logrelative_indirect_lower = ci.l.ind,
#          # logrelative_indirect_upper = ci.u.ind,
#          absolute_nma = mean,
#          absolute_nma_lower = lower,
#          absolute_nma_upper = upper)
# 
# 
# absolute.RD.inv = absolute.RD %>% rename("t1"="t2", "t2"="t1") %>%
#   mutate(logrelative_nma = (-1)*logrelative_nma,
#          logrelative_nma_lower_aux = (-1)*logrelative_nma_upper, 
#          logrelative_nma_upper = (-1)*logrelative_nma_lower, 
#          # logrelative_indirect = (-1)*logrelative_indirect,
#          # logrelative_indirect_lower_aux = (-1)*logrelative_indirect_upper, 
#          # logrelative_indirect_upper = (-1)*logrelative_indirect_lower, 
#          absolute_nma = (-1)*absolute_nma, 
#          absolute_nma_lower_aux = (-1)*absolute_nma_upper, 
#          absolute_nma_upper = (-1)*absolute_nma_lower) %>%
#   select(t1, t2, logrelative_nma, logrelative_nma_lower = logrelative_nma_lower_aux, logrelative_nma_upper, 
#          #logrelative_indirect, logrelative_indirect_lower = logrelative_indirect_lower_aux, logrelative_indirect_upper,
#          absolute_nma, absolute_nma_lower = absolute_nma_lower_aux, absolute_nma_upper)
#   
# 
# pairwise=as_tibble(read.csv("pairwise/blood/output/admission_hop.csv")) %>% 
#   rename(relative_direct = OR,
#          relative_direct_lower = OR_l,
#          relative_direct_upper = OR_u,
#          absolute_direct = risk,
#          absolute_direct_lower = risk_l,
#          absolute_direct_upper = risk_u) %>%
#   select(t1, t2, relative_direct, relative_direct_lower, relative_direct_upper, 
#          absolute_direct, absolute_direct_lower, absolute_direct_upper)
# 
# outaux = inner_join(absolute.RD,pairwise,by=c("t1"="t1","t2"="t2"))
# outbase = left_join(absolute.RD.inv,pairwise,by=c("t1"="t1","t2"="t2"))
# 
# for (i in 1:nrow(outbase)) {
#   for (j in 1:nrow(outaux)){
#     if ((outbase[i,1]==outaux[j,2]) & (outbase[i,2]==outaux[j,1])){
#       for (k in 1:ncol(outbase)){
#         outbase[i,k] = outaux[j,k]
#       }
#     }
#   }
# }
# 
# outbase = outbase %>% rename(relative_nma = logrelative_nma, relative_nma_lower = logrelative_nma_lower, 
#                              relative_nma_upper = logrelative_nma_upper) %>% #,relative_indirect = logrelative_indirect, relative_indirect_lower = logrelative_indirect_lower, 
#                              #relative_indirect_upper = logrelative_indirect_upper) %>%
#   mutate(relative_nma = exp(relative_nma),
#          relative_nma_lower = exp(relative_nma_lower),
#          relative_nma_upper = exp(relative_nma_upper)) %>% 
#          # relative_indirect = exp(relative_indirect),
#          # relative_indirect_lower = exp(relative_indirect_lower),
#          # relative_indirect_upper = exp(relative_indirect_upper)) %>% 
#   select(t1,t2,relative_nma,relative_nma_lower,relative_nma_upper,absolute_nma,absolute_nma_lower,
#          absolute_nma_upper,#relative_indirect,relative_indirect_lower,relative_indirect_upper,
#          relative_direct,relative_direct_lower,relative_direct_upper,
#          absolute_direct,absolute_direct_lower,absolute_direct_upper)
# outbase %>% write.csv("NMA/blood/output/GRADE_admission.csv")
# 