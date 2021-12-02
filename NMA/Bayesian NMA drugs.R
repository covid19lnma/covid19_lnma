# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")

mainDir <- paste0(getwd(),"/NMA/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

##########Dichotomous##########
########### Mortality ####

# data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211122.xlsx", range = "A2:F555") %>%
#   as.data.frame() %>% rename(study=stauthor,responders=responder)
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality.csv", stringsAsFactors = F))
# 
# measure = "OR"
# likelihood = "binom"
# link = "logit"
# linearModel = "random"
# 
# hy.prior1 = -1.87
# hy.prior2 = 0.4328
# 
# placebo = "standard care/placebo"
# 
# file_name = "Mortality"
# 
# data.baseline=read.csv("pairwise/drugs/mortality - wide data format.csv", stringsAsFactors = F)
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()
# 
# # prob.ref.value=data.baseline %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   filter(c.total!=0) %>%
# #   mutate(rate=c.events/c.total) %>%
# #   summarise(median=median(rate)) %>% as.numeric()
# 
# prob.ref.value=.13
# 
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# ########### mechanical ventilation ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211122.xlsx", range = "M2:R291") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/MV.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "mechanical ventilation"

data.baseline=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

prob.ref.value=.1160

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

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

########### Adverse effects leading to discont ####

# data=read_csv("NMA/drugs/AE.csv") %>% filter(study!="Ravichandran") %>% 
#   as.data.frame()
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE.csv", stringsAsFactors = F))
# 
# measure = "RD"
# likelihood = "normal"
# link = "identity"
# linearModel = "fixed"
# 
# hy.prior1 = -2.34
# hy.prior2 = 0.3303
# 
# 
# placebo = "standard care/placebo"
# 
# file_name = "Adverse effects"
# 
# data.baseline=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))  %>% filter(study!="Ravichandran")
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# ########### Admission to hospital #####
# 
# data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211122.xlsx", range = "X2:AC76") %>%
#   as.data.frame() %>% rename(study=stauthor,responders=responder) #%>% 
# #mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))
# 
# measure = "OR"
# likelihood = "binom"
# link = "logit"
# linearModel = "random"
# 
# hy.prior1 = -1.87
# hy.prior2 = 0.4328
# 
# placebo = "standard care/placebo"
# file_name = "Admission to hosp"
# 
# data.baseline=read.csv("pairwise/drugs/admission to hospital - wide data format.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total != 0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# ########### VTE #####
# 
# data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211122.xlsx", range = "AV2:BA25") %>%
#   as.data.frame() %>% rename(study=stauthor,responders=responder) #%>% 
# #mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/VTE.csv", stringsAsFactors = F))
# 
# measure = "OR"
# likelihood = "binom"
# link = "logit"
# linearModel = "random"
# 
# hy.prior1 = -1.87
# hy.prior2 = 0.4328
# 
# placebo = "standard care/placebo"
# file_name = "VTE"
# 
# data.baseline=read.csv("pairwise/drugs/VTE - wide data format.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total != 0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# ########### clinically important bleeding #####
# 
# data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211122.xlsx", range = "BH2:BM29") %>%
#   as.data.frame() %>% rename(study=stauthor,responders=responder) #%>% 
# #mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/clinically_important_bleeding.csv", stringsAsFactors = F))
# 
# measure = "OR"
# likelihood = "binom"
# link = "logit"
# linearModel = "random"
# 
# hy.prior1 = -1.87
# hy.prior2 = 0.4328
# 
# placebo = "standard care/placebo"
# file_name = "clinically important bleeding"
# 
# data.baseline=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total != 0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# ##########Continuous###########
# ########### Duration of hospitalization  ####
# 
# data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211122.xlsx", range = "A2:F238") %>% filter(study!="Dorward") %>% 
#   as.data.frame() #%>% rename(study=stauthor,responders=responder)
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_hospitalization.csv", stringsAsFactors = F))
# 
# measure = "MD"
# likelihood = "normal"
# link = "identity"
# linearModel = "random"
# 
# hy.prior1 = -2.34
# hy.prior2 = 0.3303
# 
# placebo = "standard care/placebo"
# 
# file_name = "Duration of hospitalization"
# 
# data.baseline=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(study!="Dorward")
# 
# # prob.ref.value=data.baseline %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   summarise(median=median(mean2)) %>% as.numeric()
# 
# prob.ref.value=12.8
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# #get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# ########### Time to symptom resolution ####
# 
# data=read_csv("NMA/drugs/Time_to_symptom_resolution.csv") %>% 
#   as.data.frame()#%>% rename(study=stauthor,responders=responder)
# 
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))
# 
# measure = "ROM"
# likelihood = "normal"
# link = "identity"
# linearModel = "random"
# 
# hy.prior1 = -2.34
# hy.prior2 = 0.3303
# 
# placebo = "standard care/placebo"
# file_name = "Time to symptom"
# 
# data.baseline=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   summarise(median=median(mean2)) %>% as.numeric()
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
# ########### Duration of ventilation  ####
# 
# data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211122.xlsx", range = "L2:Q58") %>%
#   as.data.frame() #%>% rename(study=stauthor,responders=responder)
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_ventilation.csv", stringsAsFactors = F))
# 
# measure = "MD"
# likelihood = "normal"
# link = "identity"
# linearModel = "random"
# 
# hy.prior1 = -2.34
# hy.prior2 = 0.3303
# 
# placebo = "standard care/placebo"
# 
# file_name = "Duration of ventilation"
# 
# data.baseline=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# # prob.ref.value=data.baseline %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   summarise(median=median(mean2)) %>% as.numeric()
# 
# prob.ref.value=14.7
# 
# getestimatesnma(data,
#                 pairwise_data,
#                 measure,
#                 likelihood, 
#                 link, 
#                 linearModel, 
#                 hy.prior1, 
#                 hy.prior2,
#                 output_dir,
#                 file_name,
#                 prob.ref.value,
#                 placebo)
# 
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
# 
