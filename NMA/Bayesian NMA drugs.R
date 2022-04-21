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

data=read.csv("input/Mortality - long data format.csv") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Mortality"

data.baseline=read.csv("input/Mortality - wide data format.csv", stringsAsFactors = F)
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()

# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

prob.ref.value=.13

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

# ########### laboratory confirmed ####

data=read.csv("input/Infection with COVID-19 (laboratory confirmed) - long data format.csv") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection with COVID-19 (laboratory confirmed) output.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "laboratory confirmed"

data.baseline=read.csv("input/Infection with COVID-19 (laboratory confirmed) - wide data format.csv")
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

# ########### laboratory confirmed and suspected ####

data=read.csv("input/Infection with COVID-19 (laboratory confirmed and suspected) - long data format.csv") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection with COVID-19 (laboratory confirmed and suspected) output.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "laboratory confirmed and suspected"

data.baseline=read.csv("input/Infection with COVID-19 (laboratory confirmed and suspected) - wide data format.csv")
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

########### Viral clearance ####

# data=read.csv("input/Viral clearance - long data format.csv") %>%
#   as.data.frame() %>% rename(study=stauthor,responders=responder)
# 
# pairwise_data=as_tibble(read.csv("pairwise/drugs/output/viral_clearance.csv", stringsAsFactors = F))
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
# file_name = "Viral clearance"
# 
# data.baseline=read.csv("input/Viral clearance - wide data format.csv")
# data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 
# 
# prob.ref.value=data.baseline %>% 
#   filter(t1==placebo| t2==placebo) %>%
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

########### Adverse effects leading to discont ####

data=read_csv("NMA/drugs/AE.csv") %>%
  as.data.frame()

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE.csv", stringsAsFactors = F))

measure = "RD"
likelihood = "normal"
link = "identity"
linearModel = "fixed"

hy.prior1 = -2.34
hy.prior2 = 0.3303


placebo = "standard care/placebo"

file_name = "Adverse effects"

data.baseline=read.csv("input/Adverse effects leading to discontinuation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

prob.ref.value=data.baseline %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total!=0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

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

########### Admission to hospital #####

data=read.csv("input/Admission to hospital - long data format.csv") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) #%>%
#mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"
file_name = "Admission to hosp"

data.baseline=read.csv("input/Admission to hospital - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

prob.ref.value=data.baseline %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total != 0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

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

########### VTE #####

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

########### clinically important bleeding #####

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

##########Continuous###########
########### Duration of hospitalization  ####

# data=read.csv("input/Duration of hospitalization - long data format.csv") %>%
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
# data.baseline=read.csv("input/Duration of hospitalization - wide data format.csv")
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
# get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Time to symptom resolution ####

data=read_csv("NMA/drugs/Time_to_symptom_resolution.csv") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)


pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"
file_name = "Time to symptom"

data.baseline=read.csv("input/Time to symptom resolution - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

prob.ref.value=data.baseline %>%
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

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

########### Duration of ventilation  ####
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

