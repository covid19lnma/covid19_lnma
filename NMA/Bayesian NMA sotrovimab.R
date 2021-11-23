# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")

mainDir <- paste0(getwd(),"/NMA/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

##### Safety outcomes#####

#######################AE###########################

data=read_csv("NMA/blood/AE_sotro.csv") %>%
  as.data.frame()

pairwise_data=as_tibble(read.csv("pairwise/blood/output/AE_sotro.csv", stringsAsFactors = F))

measure = "RD"
likelihood = "normal"
link = "identity"
linearModel = "fixed"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo <- "placebo/standard care"

file_name = "Adverse effects_sotro"

data.baseline=read.csv("pairwise/blood/adverse effects leading to discontinuation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
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

########### Allergic ####

data=read_csv("NMA/blood/allergic_reac_sotro.csv") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/allergic_reac_sotro.csv", stringsAsFactors = F))

measure = "RD"
likelihood = "normal"
link = "identity"
linearModel = "fixed"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Allergic react_sotro"

data.baseline=read.csv("pairwise/blood/allergic reactions - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
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

##### non severe####
######## Dichotomous #####
#######################Mortality ###########################

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_2021110.xlsx", range = "A2:E39") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality_sotro.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"

file_name = "Mortality_sotro"

data.baseline=read.csv("pairwise/blood/not severe/mortality - wide data format.csv", stringsAsFactors = F)
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()

prob.ref.value=data.baseline %>%
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

#prob.ref.value=.13


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

########### Mechanical ventilation ####

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_2021110.xlsx", range = "K2:O20") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/MV_sotro.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "MV_sotro"

data.baseline=read.csv("pairwise/blood/not severe/mechanical ventilation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
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

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_20211101.xlsx", range = "AF2:AJ25") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) #%>% 
#mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))

pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_to_hospital_sotro.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Admission to hosp_sotro"

data.baseline=read.csv("pairwise/blood/not severe/admission to hospital - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
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

######## Continuous #####

####################### Duration of Hospitalization ###########################

data=read_excel("NMA/blood/Continuous outcomes_non-severe_long data for analysis_20211031.xlsx", range = "A2:F17") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/Duration_of_hospitalization_sotro.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "placebo/standard care"

file_name = "duration hospitalization_sotro"

data.baseline=read.csv("pairwise/blood/not severe/Duration of hospitalization_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

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

########### Time to symptom resolution ####

data=read_csv("NMA/blood/Time_to_symptom_resolution_sotro.csv") %>% 
  as.data.frame()#%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/Time_to_symptom_resolution_sotro.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "placebo/standard care"
file_name = "Time to symptom_sotro"

data.baseline=read.csv("pairwise/blood/not severe/Time to symptom resolution_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

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
