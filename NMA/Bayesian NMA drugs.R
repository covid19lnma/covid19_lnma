# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")

mainDir <- paste0(getwd(),"/NMA/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

########### Mortality ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "A2:F426") %>%
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

data.baseline=read.csv("pairwise/drugs/mortality - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=.13

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)


########### Admission to hospital ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "X2:AC45") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Admission to hospital"

data.baseline=read.csv("pairwise/drugs/admission to hospital - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Adverse effects leading to discont ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "AJ2:AO148") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/adverse_effects.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Adverse effects"

data.baseline=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### clinically important bleeding ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "BT2:BY16") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/clinically_important_bleeding.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "clinically important bleeding"

data.baseline=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### mechanical ventilation ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "M2:R224") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mechanical_ventilation.csv", stringsAsFactors = F))

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

prob.ref.value=.116
  
getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### viral clearance ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "AV2:BA95") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/viral_clearance.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "viral clearance"

data.baseline=read.csv("pairwise/drugs/viral clearance - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### VTE ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20210723.xlsx", range = "BH2:BM16") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/VTE.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "VTE"

data.baseline=read.csv("pairwise/drugs/VTE - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Duration of hospitalization  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20210723.xlsx", range = "A2:F188") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_hospitalization.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Duration of hospitalization"

data.baseline=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=12.8
  
getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Duration of ventilation  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20210723.xlsx", range = "AG2:AL36") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_ventilation.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Duration of ventilation"

data.baseline=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=14.7

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### ICU length of stay  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20210723.xlsx", range = "L2:Q18") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/ICU_length.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "ICU length"

data.baseline=read.csv("pairwise/drugs/ICU length of stay_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=13.3

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

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

file_name = "Time to symptom resolution"

data.baseline=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Time to viral clearance ####

data=read_csv("NMA/drugs/Time_to_viral_clearance.csv") %>%
  as.data.frame() %>% filter(study!="Arabi",study!="Elogary") #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_viral_clearance.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Time to viral clearance"

data.baseline=read.csv("pairwise/drugs/Time to viral clearance_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Ventilator-free days  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20210723.xlsx", range = "V2:AA36") %>%
  as.data.frame() %>% filter(study!="Self") #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Ventilator_free_days.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Ventilator-free days"

data.baseline=read.csv("pairwise/drugs/Ventilator-free days_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)